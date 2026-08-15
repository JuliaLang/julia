# This file is a part of Julia. License is MIT: https://julialang.org/license

# Validation and world-age detachment of field generators for structs with
# computed field types (declared field-type slots holding
# `Core.ComputedFieldType` markers). `_set_fieldtype_generator!` in
# essentials.jl calls `_validate_fieldtype_generator` when it is defined.
#
# Validity: inference must prove the generator `is_foldable` (`:consistent`,
# `:effect_free`, `:noub`, `:terminates`) plus `:nortcall`. `:nothrow` is *not*
# required: a generator that throws for particular parameter values degrades
# those instantiations' field types to `Union{}` (see
# `invoke_fieldtype_generator` in src/jltypes.c).
#
# `:consistent`-cy is world-bounded (see the `Base.@assume_effects`
# documentation), so proven effects alone do not make the generator's meaning
# stable across worlds -- and the definition world no longer exists after a
# precompiled image is reloaded. We therefore duplicate the inferred
# CodeInstance into a *detached* CodeInstance with world validity
# `(1, typemax(UInt))` whose code is closed under the definition world:
#   - every remaining `:call` targets a builtin/intrinsic (world-independent),
#   - every `:invoke` targets another detached CodeInstance (recursively),
#   - every global reference is replaced by its definition-world constant value.
# A detached instance is never registered in any method cache, so method
# redefinition and invalidation cannot reach it; it pins the definition-time
# semantics of the type's field types forever. When the original CodeInstance
# already has a world-independent form (constant return with `:nothrow`), the
# detached copy is just a constant-return instance with no code.

const _fieldgen_detached_owner = :computed_fieldtypes

"""
    fieldtype_generator(T::Type) -> Union{Core.FieldtypeGenerator, Tuple}

For a (possibly partially applied) type `T` whose field types are computed from
the values of its type parameters, return the partially applied field
generator: a plain reflection object — deliberately *not* part of the type
system — that is applied with `apply_type`, like a partially applied
`UnionAll`. Applying some of the remaining free parameters yields another
`Core.FieldtypeGenerator`; applying all of them yields the tuple of field
types, evaluated with the definition-time (world-age-detached) semantics. If
`T` has no remaining free parameters, the tuple is returned directly.

```julia
struct StaticMatrix{R,C,T}
    data::NTuple{(R::Int)*(C::Int), T}
end

g = Base.fieldtype_generator(StaticMatrix{2})
g{3}           # a Core.FieldtypeGenerator
g{3, Float64}  # === (NTuple{6, Float64},)
```

Unlike instantiating the type, errors thrown by the field type expressions
propagate rather than degrading the field types to `Union{}`.
"""
function fieldtype_generator(@nospecialize(ty::Type))
    dt = unwrap_unionall(ty)
    dt isa DataType ||
        throw(ArgumentError("expected a (partially applied) struct type"))
    tn = dt.name
    fg = isdefined(tn, :fieldgen, :monotonic) ? (@atomic :monotonic tn.fieldgen) : nothing
    fg isa SimpleVector ||
        throw(ArgumentError("$(tn.name) does not have computed field types"))
    if !(ty isa UnionAll)
        # fully applied already: the application is complete
        for p in dt.parameters
            if p isa TypeVar || (p isa Type && has_free_typevars(p))
                throw(ArgumentError("type parameters of $(ty) are not fully specified"))
            end
        end
        res = ccall(:jl_call_fieldtype_generator, Any, (Any, Any), tn, dt.parameters)
        nf = length(tn.names)
        (res isa Tuple && nfields(res) == nf) ||
            error("field generator for ", tn.name, " returned an invalid result")
        return res
    end
    return Core.FieldtypeGenerator(ty)
end

function show(io::IO, g::Core.FieldtypeGenerator)
    print(io, "Base.fieldtype_generator(")
    show(io, g.ty)
    print(io, ")")
end

_fieldgen_error(tn::Core.TypeName, msg...) =
    error("invalid computed field types for ", tn.name, ": ", msg...)

# builtins whose runtime behavior consults the method table or binding table of
# the *current* world; these must not remain in detached code
const _fieldgen_forbidden_builtins = Any[
    Core.invoke, Core._apply_iterate, Core._call_latest, Core._call_in_world_total,
    Core.getglobal, Core.setglobal!, Core.isdefinedglobal, Core.replaceglobal!,
    Core.swapglobal!, Core.modifyglobal!, Core.setglobalonce!,
]

function _fieldgen_resolve_const(@nospecialize(x))
    # Return (isconst, value) for IR values that denote compile-time constants.
    if x isa GlobalRef
        if isconst(x.mod, x.name) && isdefinedglobal(x.mod, x.name)
            return true, getglobal(x.mod, x.name)
        end
        return false, nothing
    elseif x isa QuoteNode
        return true, x.value
    elseif x isa Expr || x isa Core.SSAValue || x isa Core.Argument || x isa Core.SlotNumber
        return false, nothing
    end
    return true, x
end

function _detach_ir(@nospecialize(x), seen::IdDict{CodeInstance,Any}, tn::Core.TypeName, world::UInt)
    if x isa Expr
        head = x.head
        nargs = length(x.args)
        if head === :invoke
            nargs >= 2 || _fieldgen_error(tn, "malformed :invoke in field generator code")
            target = x.args[1]
            if target isa MethodInstance
                # not pinned to a CodeInstance; re-infer to obtain one
                target = _fieldgen_infer(target, tn, world)
            end
            target isa CodeInstance ||
                _fieldgen_error(tn, "unsupported :invoke target in field generator code")
            newargs = Vector{Any}(undef, nargs)
            newargs[1] = _detach_codeinst(target, seen, tn, world)
            for i = 2:nargs
                newargs[i] = _detach_ir(x.args[i], seen, tn, world)
            end
            return Expr(:invoke, newargs...)
        elseif head === :call
            nargs >= 1 || _fieldgen_error(tn, "malformed :call in field generator code")
            isc, f = _fieldgen_resolve_const(x.args[1])
            if !isc || !(f isa Core.Builtin)
                _fieldgen_error(tn,
                    "the field generator contains a dynamic call to `", x.args[1], "`; ",
                    "field generators must be fully resolvable by the compiler")
            end
            for forbidden in _fieldgen_forbidden_builtins
                f === forbidden && _fieldgen_error(tn,
                    "the field generator calls `", f, "`, whose behavior depends on the ",
                    "current world age")
            end
            newargs = Vector{Any}(undef, nargs)
            newargs[1] = QuoteNode(f)
            for i = 2:nargs
                newargs[i] = _detach_ir(x.args[i], seen, tn, world)
            end
            return Expr(:call, newargs...)
        elseif head === :foreigncall || head === :cfunction || head === :new_opaque_closure
            _fieldgen_error(tn, "`", head, "` is not supported in field generator code")
        else
            # :new, :splatnew, :boundscheck, :isdefined, :throw_undef_if_not,
            # :gc_preserve_*, :meta, :code_coverage_effect, ...
            for i = 1:nargs
                x.args[i] = _detach_ir(x.args[i], seen, tn, world)
            end
            return x
        end
    elseif x isa GlobalRef
        (isconst(x.mod, x.name) && isdefinedglobal(x.mod, x.name)) ||
            _fieldgen_error(tn, "the field generator reads the non-constant global `",
                            x.mod, ".", x.name, "`")
        return QuoteNode(getglobal(x.mod, x.name))
    elseif x isa Core.GotoIfNot
        return Core.GotoIfNot(_detach_ir(x.cond, seen, tn, world), x.dest)
    elseif x isa Core.ReturnNode
        isdefined(x, :val) || return x
        return Core.ReturnNode(_detach_ir(x.val, seen, tn, world))
    elseif x isa Core.PiNode
        return Core.PiNode(_detach_ir(x.val, seen, tn, world), x.typ)
    elseif x isa Core.PhiNode
        values = x.values
        for i = 1:length(values)
            if isassigned(values, i)
                values[i] = _detach_ir(values[i], seen, tn, world)
            end
        end
        return x
    elseif x isa Core.PhiCNode
        values = x.values
        for i = 1:length(values)
            if isassigned(values, i)
                values[i] = _detach_ir(values[i], seen, tn, world)
            end
        end
        return x
    elseif x isa Core.UpsilonNode
        isdefined(x, :val) || return x
        return Core.UpsilonNode(_detach_ir(x.val, seen, tn, world))
    end
    # SSAValue, Argument, QuoteNode, GotoNode, EnterNode, literals, nothing
    return x
end

function _fieldgen_infer(mi::MethodInstance, tn::Core.TypeName, world::UInt)
    ci = Compiler.typeinf_ext_toplevel(mi, world, Compiler.SOURCE_MODE_GET_SOURCE, Compiler.TRIM_NO)
    ci isa CodeInstance ||
        _fieldgen_error(tn, "inference of the field generator (", mi, ") did not produce code")
    return ci
end

function _fieldgen_source(ci::CodeInstance, tn::Core.TypeName, world::UInt)
    inf = @atomic :monotonic ci.inferred
    if inf isa CodeInfo
        return copy(inf)
    elseif inf isa String
        def = ci.def
        if def isa MethodInstance
            return _uncompressed_ir(ci, inf)
        end
    end
    # source was not retained on the instance (e.g. it only lives in the
    # execution engine); re-run inference locally to recover equivalent code
    def = ci.def
    def isa MethodInstance ||
        _fieldgen_error(tn, "cannot recover source for field generator callee")
    interp = Compiler.NativeInterpreter(world)
    src = Compiler.typeinf_code(interp, def, #=run_optimizer=#true)
    src isa CodeInfo ||
        _fieldgen_error(tn, "cannot recover source for field generator callee ", def)
    return src
end

function _detach_codeinst(ci::CodeInstance, seen::IdDict{CodeInstance,Any}, tn::Core.TypeName, world::UInt)
    ci.owner === _fieldgen_detached_owner && return ci
    prev = get(seen, ci, nothing)
    prev === missing &&
        _fieldgen_error(tn, "recursive field generators are not yet supported")
    prev isa CodeInstance && return prev
    seen[ci] = missing
    debuginfo = isdefined(ci, :debuginfo) ? ci.debuginfo : nothing
    effects = Compiler.decode_effects(ci.ipo_purity_bits)
    local dci
    if isdefined(ci, :rettype_const) && Compiler.is_foldable_nothrow(effects)
        # constant return: already world-age independent, no code needed
        dci = CodeInstance(ci.def, _fieldgen_detached_owner, ci.rettype, ci.exctype,
                           ci.rettype_const, nothing, Int32(0x3), UInt(1), typemax(UInt),
                           ci.ipo_purity_bits, nothing, debuginfo, Core.svec())
    else
        src = _fieldgen_source(ci, tn, world)
        code = src.code
        for i = 1:length(code)
            code[i] = _detach_ir(code[i], seen, tn, world)
        end
        rettype_const = isdefined(ci, :rettype_const) ? ci.rettype_const : nothing
        const_flags = isdefined(ci, :rettype_const) ? Int32(0x2) : Int32(0x0)
        dci = CodeInstance(ci.def, _fieldgen_detached_owner, ci.rettype, ci.exctype,
                           rettype_const, src, const_flags, UInt(1), typemax(UInt),
                           ci.ipo_purity_bits, nothing, debuginfo, Core.svec())
    end
    seen[ci] = dci
    return dci
end

function _validate_fieldtype_generator(@nospecialize(ty), @nospecialize(gen))
    dt = unwrap_unionall(ty)
    dt isa DataType || error("expected a type with computed field types")
    tn = dt.name
    np = length(dt.parameters)
    world = get_world_counter()
    tt = Tuple{typeof(gen), Vararg{Any,np}}
    match = _which(tt; world, raise=false)
    match === nothing && _fieldgen_error(tn, "no matching field generator method")
    mi = Compiler.specialize_method(match)
    ci = _fieldgen_infer(mi, tn, world)
    effects = Compiler.decode_effects(ci.ipo_purity_bits)
    if !Compiler.is_foldable(effects, #=check_rtcall=#true)
        _fieldgen_error(tn,
            "the compiler could not prove the field type expressions consistent and terminating ",
            "(effects: ", effects, "). Computed field type expressions must be pure functions of ",
            "the type parameters; annotating the parameters with their types ",
            "(e.g. `NTuple{(R::Int)*(C::Int), T}`) usually allows the proof to go through, and ",
            "`Base.@assume_effects :foldable` on a helper function called in the expression can ",
            "vouch for effects the compiler cannot prove")
    end
    seen = IdDict{CodeInstance,Any}()
    dci = _detach_codeinst(ci, seen, tn, world)
    return Core.svec(gen, dci)
end
