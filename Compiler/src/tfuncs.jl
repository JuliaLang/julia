# This file is a part of Julia. License is MIT: https://julialang.org/license

#############
# constants #
#############

"""
    @nospecs def

Adds `@nospecialize` annotation to non-annotated arguments of `def`.
```julia
(Core.Compiler) julia> @macroexpand @nospecs function tfunc(𝕃::AbstractLattice, x, y::Bool, zs...)
                           x, ys
                       end
:(function tfunc(\$(Expr(:meta, :specialize, :(𝕃::AbstractLattice))), x, y::Bool, zs...)
      #= REPL[3]:1 =#
      \$(Expr(:meta, :nospecialize, :x, :zs))
      #= REPL[3]:2 =#
      (x, ys)
  end)
```
"""
macro nospecs(ex)
    is_function_def(ex) || throw(ArgumentError("expected function definition"))
    args, body = ex.args
    if isexpr(args, :call)
        args = args.args[2:end] # skip marking `@nospecialize` on the function itself
    else
        @assert isexpr(args, :tuple) # anonymous function
        args = args.args
    end
    names = Symbol[]
    for arg in args
        isexpr(arg, :macrocall) && continue
        if isexpr(arg, :...)
            arg = arg.args[1]
        elseif isexpr(arg, :kw)
            arg = arg.args[1]
        end
        isexpr(arg, :(::)) && continue
        @assert arg isa Symbol
        push!(names, arg)
    end
    @assert isexpr(body, :block)
    isempty(names) && throw(ArgumentError("no arguments for @nospec"))
    lin = first(body.args)::LineNumberNode
    nospec = Expr(:macrocall, GlobalRef(@__MODULE__, :var"@nospecialize"), lin, names...)
    insert!(body.args, 2, nospec)
    return esc(ex)
end

const INT_INF = typemax(Int) # integer infinity

const N_IFUNC = reinterpret(Int32, have_fma) + 1
const T_IFUNC = Vector{Tuple{Int, Int, Any}}(undef, N_IFUNC)
const T_IFUNC_COST = Vector{Int}(undef, N_IFUNC)
const T_FFUNC_KEY = Vector{Any}()
const T_FFUNC_VAL = Vector{Tuple{Int, Int, Any}}()
const T_FFUNC_COST = Vector{Int}()
function find_tfunc(@nospecialize f)
    for i = 1:length(T_FFUNC_KEY)
        if T_FFUNC_KEY[i] === f
            return i
        end
    end
end

const DATATYPE_TYPES_FIELDINDEX = fieldindex(DataType, :types)
const DATATYPE_NAME_FIELDINDEX = fieldindex(DataType, :name)

##########
# tfuncs #
##########

# Note that in most places in the compiler here, we'll assume that T=Type{S} is well-formed,
# and implies that `S <: Type`, not `1::Type{1}`, for example.
# This means that isType(T) implies we can call subtype on T.parameters[1], etc.

function add_tfunc(f::IntrinsicFunction, minarg::Int, maxarg::Int, @nospecialize(tfunc), cost::Int)
    idx = reinterpret(Int32, f) + 1
    T_IFUNC[idx] = (minarg, maxarg, tfunc)
    T_IFUNC_COST[idx] = cost
end
function add_tfunc(@nospecialize(f::Builtin), minarg::Int, maxarg::Int, @nospecialize(tfunc), cost::Int)
    push!(T_FFUNC_KEY, f)
    push!(T_FFUNC_VAL, (minarg, maxarg, tfunc))
    push!(T_FFUNC_COST, cost)
end

add_tfunc(throw, 1, 1, @nospecs((𝕃::AbstractLattice, x)->Bottom), 0)
add_tfunc(Core.throw_methoderror, 1, INT_INF, @nospecs((𝕃::AbstractLattice, x)->Bottom), 0)

# the inverse of typeof_tfunc
# returns (type, isexact, isconcrete, istype)
# if isexact is false, the actual runtime type may (will) be a subtype of t
# if isconcrete is true, the actual runtime type is definitely concrete (unreachable if not valid as a typeof)
# if istype is true, the actual runtime value will definitely be a type (e.g. this is false for Union{Type{Int}, Int})
function instanceof_tfunc(@nospecialize(t), astag::Bool=false, @nospecialize(troot) = t)
    if isa(t, Const)
        if isa(t.val, Type) && valid_as_lattice(t.val, astag)
            return t.val, true, isconcretetype(t.val), true
        end
        return Bottom, true, false, false # runtime throws on non-Type
    end
    t = widenconst(t)
    troot = widenconst(troot)
    if t === Bottom
        return Bottom, true, true, false # runtime unreachable
    elseif t === typeof(Bottom) || !hasintersect(t, Type)
        return Bottom, true, false, false # literal Bottom or non-Type
    elseif isType(t)
        tp = t.parameters[1]
        valid_as_lattice(tp, astag) || return Bottom, true, false, false # runtime unreachable / throws on non-Type
        if troot isa UnionAll
            # Free `TypeVar`s inside `Type` has violated the "diagonal" rule.
            # Widen them before `UnionAll` rewraping to relax concrete constraint.
            tp = widen_diagonal(tp, troot)
        end
        return tp, !has_free_typevars(tp), isconcretetype(tp), true
    elseif isa(t, UnionAll)
        t′ = unwrap_unionall(t)
        t′′, isexact, isconcrete, istype = instanceof_tfunc(t′, astag, rewrap_unionall(t, troot))
        tr = rewrap_unionall(t′′, t)
        if t′′ isa DataType && t′′.name !== Tuple.name && !has_free_typevars(tr)
            # a real instance must be within the declared bounds of the type,
            # so we can intersect with the original wrapper.
            tr = typeintersect(tr, t′′.name.wrapper)
            isconcrete = !isabstracttype(t′′)
            if tr === Union{}
                # runtime unreachable (our inference Type{T} where S is
                # uninhabited with any runtime T that exists)
                isexact = true
            end
        end
        return tr, isexact, isconcrete, istype
    elseif isa(t, Union)
        ta, isexact_a, isconcrete_a, istype_a = instanceof_tfunc(unwraptv(t.a), astag, troot)
        tb, isexact_b, isconcrete_b, istype_b = instanceof_tfunc(unwraptv(t.b), astag, troot)
        isconcrete = isconcrete_a && isconcrete_b
        istype = istype_a && istype_b
        # most users already handle the Union case, so here we assume that
        # `isexact` only cares about the answers where there's actually a Type
        # (and assuming other cases causing runtime errors)
        ta === Union{} && return tb, isexact_b, isconcrete, istype
        tb === Union{} && return ta, isexact_a, isconcrete, istype
        return Union{ta, tb}, false, isconcrete, istype # at runtime, will be exactly one of these
    end
    return Any, false, false, false
end

# IntrinsicFunction
# =================

# conversion
# ----------

@nospecs bitcast_tfunc(𝕃::AbstractLattice, t, x) = bitcast_tfunc(widenlattice(𝕃), t, x)
@nospecs bitcast_tfunc(::JLTypeLattice, t, x) = instanceof_tfunc(t, true)[1]
@nospecs conversion_tfunc(𝕃::AbstractLattice, t, x) = conversion_tfunc(widenlattice(𝕃), t, x)
@nospecs conversion_tfunc(::JLTypeLattice, t, x) = instanceof_tfunc(t, true)[1]

add_tfunc(bitcast, 2, 2, bitcast_tfunc, 0)
add_tfunc(sext_int, 2, 2, conversion_tfunc, 0)
add_tfunc(zext_int, 2, 2, conversion_tfunc, 0)
add_tfunc(trunc_int, 2, 2, conversion_tfunc, 0)
add_tfunc(fptoui, 2, 2, conversion_tfunc, 1)
add_tfunc(fptosi, 2, 2, conversion_tfunc, 1)
add_tfunc(uitofp, 2, 2, conversion_tfunc, 1)
add_tfunc(sitofp, 2, 2, conversion_tfunc, 1)
add_tfunc(fptrunc, 2, 2, conversion_tfunc, 1)
add_tfunc(fpext, 2, 2, conversion_tfunc, 1)

# arithmetic
# ----------

@nospecs math_tfunc(𝕃::AbstractLattice, args...) = math_tfunc(widenlattice(𝕃), args...)
@nospecs math_tfunc(::JLTypeLattice, x, xs...) = widenconst(x)

add_tfunc(neg_int, 1, 1, math_tfunc, 0)
add_tfunc(add_int, 2, 2, math_tfunc, 1)
add_tfunc(sub_int, 2, 2, math_tfunc, 1)
add_tfunc(mul_int, 2, 2, math_tfunc, 3)
add_tfunc(sdiv_int, 2, 2, math_tfunc, 20)
add_tfunc(udiv_int, 2, 2, math_tfunc, 20)
add_tfunc(srem_int, 2, 2, math_tfunc, 20)
add_tfunc(urem_int, 2, 2, math_tfunc, 20)
add_tfunc(neg_float, 1, 1, math_tfunc, 1)
add_tfunc(add_float, 2, 2, math_tfunc, 2)
add_tfunc(sub_float, 2, 2, math_tfunc, 2)
add_tfunc(mul_float, 2, 2, math_tfunc, 8)
add_tfunc(div_float, 2, 2, math_tfunc, 10)
add_tfunc(min_float, 2, 2, math_tfunc, 1)
add_tfunc(max_float, 2, 2, math_tfunc, 1)
add_tfunc(fma_float, 3, 3, math_tfunc, 8)
add_tfunc(muladd_float, 3, 3, math_tfunc, 8)

# fast arithmetic
add_tfunc(neg_float_fast, 1, 1, math_tfunc, 1)
add_tfunc(add_float_fast, 2, 2, math_tfunc, 2)
add_tfunc(sub_float_fast, 2, 2, math_tfunc, 2)
add_tfunc(mul_float_fast, 2, 2, math_tfunc, 8)
add_tfunc(div_float_fast, 2, 2, math_tfunc, 10)
add_tfunc(min_float_fast, 2, 2, math_tfunc, 1)
add_tfunc(max_float_fast, 2, 2, math_tfunc, 1)

# bitwise operators
# -----------------

@nospecs and_int_tfunc(𝕃::AbstractLattice, x, y) = and_int_tfunc(widenlattice(𝕃), x, y)
@nospecs function and_int_tfunc(𝕃::ConstsLattice, x, y)
    if isa(x, Const) && x.val === false && widenconst(y) === Bool
        return Const(false)
    elseif isa(y, Const) && y.val === false && widenconst(x) === Bool
        return Const(false)
    end
    return and_int_tfunc(widenlattice(𝕃), x, y)
end
@nospecs and_int_tfunc(::JLTypeLattice, x, y) = widenconst(x)

@nospecs or_int_tfunc(𝕃::AbstractLattice, x, y) = or_int_tfunc(widenlattice(𝕃), x, y)
@nospecs function or_int_tfunc(𝕃::ConstsLattice, x, y)
    if isa(x, Const) && x.val === true && widenconst(y) === Bool
        return Const(true)
    elseif isa(y, Const) && y.val === true && widenconst(x) === Bool
        return Const(true)
    end
    return or_int_tfunc(widenlattice(𝕃), x, y)
end
@nospecs or_int_tfunc(::JLTypeLattice, x, y) = widenconst(x)

@nospecs shift_tfunc(𝕃::AbstractLattice, x, y) = shift_tfunc(widenlattice(𝕃), x, y)
@nospecs shift_tfunc(::JLTypeLattice, x, y) = widenconst(x)

function not_tfunc(𝕃::AbstractLattice, @nospecialize(b))
    if isa(b, Conditional)
        return Conditional(b.slot, b.elsetype, b.thentype)
    elseif isa(b, Const)
        return Const(not_int(b.val))
    end
    return math_tfunc(𝕃, b)
end

add_tfunc(and_int, 2, 2, and_int_tfunc, 1)
add_tfunc(or_int, 2, 2, or_int_tfunc, 1)
add_tfunc(xor_int, 2, 2, math_tfunc, 1)
add_tfunc(not_int, 1, 1, not_tfunc, 0) # usually used as not_int(::Bool) to negate a condition
add_tfunc(shl_int, 2, 2, shift_tfunc, 1)
add_tfunc(lshr_int, 2, 2, shift_tfunc, 1)
add_tfunc(ashr_int, 2, 2, shift_tfunc, 1)
add_tfunc(bswap_int, 1, 1, math_tfunc, 1)
add_tfunc(ctpop_int, 1, 1, math_tfunc, 1)
add_tfunc(ctlz_int, 1, 1, math_tfunc, 1)
add_tfunc(cttz_int, 1, 1, math_tfunc, 1)
add_tfunc(checked_sdiv_int, 2, 2, math_tfunc, 40)
add_tfunc(checked_udiv_int, 2, 2, math_tfunc, 40)
add_tfunc(checked_srem_int, 2, 2, math_tfunc, 40)
add_tfunc(checked_urem_int, 2, 2, math_tfunc, 40)

# functions
# ---------

add_tfunc(abs_float, 1, 1, math_tfunc, 2)
add_tfunc(copysign_float, 2, 2, math_tfunc, 2)
add_tfunc(flipsign_int, 2, 2, math_tfunc, 1)
add_tfunc(ceil_llvm, 1, 1, math_tfunc, 10)
add_tfunc(floor_llvm, 1, 1, math_tfunc, 10)
add_tfunc(trunc_llvm, 1, 1, math_tfunc, 10)
add_tfunc(rint_llvm, 1, 1, math_tfunc, 10)
add_tfunc(sqrt_llvm, 1, 1, math_tfunc, 20)
add_tfunc(sqrt_llvm_fast, 1, 1, math_tfunc, 20)

# comparisons
# -----------

@nospecs cmp_tfunc(𝕃::AbstractLattice, a, b) = cmp_tfunc(widenlattice(𝕃), a, b)
@nospecs cmp_tfunc(::JLTypeLattice, a, b) = Bool

add_tfunc(eq_int, 2, 2, cmp_tfunc, 1)
add_tfunc(ne_int, 2, 2, cmp_tfunc, 1)
add_tfunc(slt_int, 2, 2, cmp_tfunc, 1)
add_tfunc(ult_int, 2, 2, cmp_tfunc, 1)
add_tfunc(sle_int, 2, 2, cmp_tfunc, 1)
add_tfunc(ule_int, 2, 2, cmp_tfunc, 1)
add_tfunc(eq_float, 2, 2, cmp_tfunc, 2)
add_tfunc(ne_float, 2, 2, cmp_tfunc, 2)
add_tfunc(lt_float, 2, 2, cmp_tfunc, 2)
add_tfunc(le_float, 2, 2, cmp_tfunc, 2)
add_tfunc(fpiseq, 2, 2, cmp_tfunc, 1)
add_tfunc(eq_float_fast, 2, 2, cmp_tfunc, 1)
add_tfunc(ne_float_fast, 2, 2, cmp_tfunc, 1)
add_tfunc(lt_float_fast, 2, 2, cmp_tfunc, 1)
add_tfunc(le_float_fast, 2, 2, cmp_tfunc, 1)

# checked arithmetic
# ------------------

@nospecs chk_tfunc(𝕃::AbstractLattice, x, y) = chk_tfunc(widenlattice(𝕃), x, y)
@nospecs chk_tfunc(::JLTypeLattice, x, y) = Tuple{widenconst(x), Bool}

add_tfunc(checked_sadd_int, 2, 2, chk_tfunc, 2)
add_tfunc(checked_uadd_int, 2, 2, chk_tfunc, 2)
add_tfunc(checked_ssub_int, 2, 2, chk_tfunc, 2)
add_tfunc(checked_usub_int, 2, 2, chk_tfunc, 2)
add_tfunc(checked_smul_int, 2, 2, chk_tfunc, 5)
add_tfunc(checked_umul_int, 2, 2, chk_tfunc, 5)

# other, misc
# -----------

@nospecs function llvmcall_tfunc(𝕃::AbstractLattice, fptr, rt, at, a...)
    return instanceof_tfunc(rt)[1]
end
add_tfunc(Core.Intrinsics.llvmcall, 3, INT_INF, llvmcall_tfunc, 10)

@nospecs cglobal_tfunc(𝕃::AbstractLattice, fptr) = Ptr{Cvoid}
@nospecs function cglobal_tfunc(𝕃::AbstractLattice, fptr, t)
    isa(t, Const) && return isa(t.val, Type) ? Ptr{t.val} : Ptr
    return isType(t) ? Ptr{t.parameters[1]} : Ptr
end
add_tfunc(Core.Intrinsics.cglobal, 1, 2, cglobal_tfunc, 5)

add_tfunc(Core.Intrinsics.have_fma, 1, 1, @nospecs((𝕃::AbstractLattice, x)->Bool), 1)

# builtin functions
# =================

@nospecs function ifelse_tfunc(𝕃::AbstractLattice, cnd, x, y)
    cnd = widenslotwrapper(cnd)
    if isa(cnd, Const)
        if cnd.val === true
            return x
        elseif cnd.val === false
            return y
        else
            return Bottom
        end
    elseif !hasintersect(widenconst(cnd), Bool)
        return Bottom
    end
    return tmerge(𝕃, x, y)
end
add_tfunc(Core.ifelse, 3, 3, ifelse_tfunc, 1)

@nospecs function ifelse_nothrow(𝕃::AbstractLattice, cond, x, y)
    ⊑ = partialorder(𝕃)
    return cond ⊑ Bool
end

@nospecs egal_tfunc(𝕃::AbstractLattice, x, y) = egal_tfunc(widenlattice(𝕃), x, y)
@nospecs function egal_tfunc(𝕃::MustAliasesLattice, x, y)
    return egal_tfunc(widenlattice(𝕃), widenmustalias(x), widenmustalias(y))
end
@nospecs function egal_tfunc(𝕃::ConditionalsLattice, x, y)
    if isa(x, Conditional)
        y = widenconditional(y)
        if isa(y, Const)
            y.val === false && return Conditional(x.slot, x.elsetype, x.thentype)
            y.val === true && return x
            return Const(false)
        end
    elseif isa(y, Conditional)
        x = widenconditional(x)
        if isa(x, Const)
            x.val === false && return Conditional(y.slot, y.elsetype, y.thentype)
            x.val === true && return y
            return Const(false)
        end
    end
    return egal_tfunc(widenlattice(𝕃), x, y)
end
@nospecs function egal_tfunc(𝕃::ConstsLattice, x, y)
    if isa(x, Const) && isa(y, Const)
        return Const(x.val === y.val)
    elseif (isa(x, Const) && y === typeof(x.val) && issingletontype(x)) ||
           (isa(y, Const) && x === typeof(y.val) && issingletontype(y))
        return Const(true)
    end
    return egal_tfunc(widenlattice(𝕃), x, y)
end
@nospecs function egal_tfunc(::JLTypeLattice, x, y)
    hasintersect(widenconst(x), widenconst(y)) || return Const(false)
    return Bool
end
add_tfunc(===, 2, 2, egal_tfunc, 1)

function isdefined_nothrow(𝕃::AbstractLattice, argtypes::Vector{Any})
    if length(argtypes) ≠ 2
        # TODO prove nothrow when ordering is specified
        return false
    end
    return isdefined_nothrow(𝕃, argtypes[1], argtypes[2])
end
@nospecs function isdefined_nothrow(𝕃::AbstractLattice, x, name)
    ⊑ = partialorder(𝕃)
    isvarargtype(x) && return false
    isvarargtype(name) && return false
    if hasintersect(widenconst(x), Module)
        return name ⊑ Symbol
    else
        return name ⊑ Symbol || name ⊑ Int
    end
end

@nospecs function isdefined_tfunc(𝕃::AbstractLattice, arg1, sym, order)
    return isdefined_tfunc(𝕃, arg1, sym)
end
@nospecs function isdefined_tfunc(𝕃::AbstractLattice, arg1, sym)
    arg1t = arg1 isa Const ? typeof(arg1.val) : isconstType(arg1) ? typeof(arg1.parameters[1]) : widenconst(arg1)
    a1 = unwrap_unionall(arg1t)
    if isa(a1, DataType) && !isabstracttype(a1)
        if a1 === Module
            hasintersect(widenconst(sym), Symbol) || return Bottom
            # isa(sym, Const) case intercepted in abstract interpretation
        elseif isa(sym, Const)
            val = sym.val
            if isa(val, Symbol)
                idx = fieldindex(a1, val, false)::Int
            elseif isa(val, Int)
                idx = val
            else
                return Bottom
            end
            if 1 ≤ idx ≤ datatype_min_ninitialized(a1)
                return Const(true)
            elseif a1.name === _NAMEDTUPLE_NAME
                if isconcretetype(a1)
                    return Const(false)
                else
                    ns = a1.parameters[1]
                    if isa(ns, Tuple)
                        return Const(1 ≤ idx ≤ length(ns))
                    end
                end
            elseif idx ≤ 0 || (!isvatuple(a1) && idx > fieldcount(a1))
                return Const(false)
            elseif isa(arg1, Const)
                if !ismutabletype(a1) || isconst(a1, idx)
                    return Const(isdefined(arg1.val, idx))
                end
            elseif isa(arg1, PartialStruct)
                if !isvarargtype(arg1.fields[end])
                    if !is_field_maybe_undef(arg1, idx)
                        return Const(true)
                    end
                end
            elseif !isvatuple(a1)
                fieldT = fieldtype(a1, idx)
                if isa(fieldT, DataType) && isbitstype(fieldT)
                    return Const(true)
                end
            end
        end
    elseif isa(a1, Union)
        # Results can only be `Const` or `Bool`
        return tmerge(𝕃,
                      isdefined_tfunc(𝕃, rewrap_unionall(a1.a, arg1t), sym),
                      isdefined_tfunc(𝕃, rewrap_unionall(a1.b, arg1t), sym))
    end
    return Bool
end

add_tfunc(isdefined, 2, 3, isdefined_tfunc, 1)

function sizeof_nothrow(@nospecialize(x))
    if isa(x, Const)
        if !isa(x.val, Type) || x.val === DataType
            return true
        end
    end
    xu = unwrap_unionall(x)
    if isa(xu, Union)
        return sizeof_nothrow(rewrap_unionall(xu.a, x)) &&
               sizeof_nothrow(rewrap_unionall(xu.b, x))
    end
    t, exact, isconcrete = instanceof_tfunc(x, false)
    if t === Bottom
        # x must be an instance (not a Type) or is the Bottom type object
        x = widenconst(x)
        return !hasintersect(x, Type)
    end
    x = unwrap_unionall(t)
    if isconcrete
        if isa(x, DataType) && x.layout != C_NULL
            # there's just a few concrete types with an opaque layout
            (datatype_nfields(x) == 0 && !datatype_pointerfree(x)) && return false
        end
        return true # these must always have a size of these
    end
    exact || return false # Could always be the type Bottom at runtime, for example, which throws
    t === DataType && return true # DataType itself has a size
    if isa(x, Union)
        isinline = uniontype_layout(x)[1]
        return isinline # even any subset of this union would have a size
    end
    isa(x, DataType) || return false
    x.layout == C_NULL && return false
    (datatype_nfields(x) == 0 && !datatype_pointerfree(x)) && return false # is-layout-opaque
    return true
end

function _const_sizeof(@nospecialize(x))
    # Constant GenericMemory does not have constant size
    isa(x, GenericMemory) && return Int
    size = try
            Core.sizeof(x)
        catch ex
            # Might return
            # "argument is an abstract type; size is indeterminate" or
            # "type does not have a fixed size"
            isa(ex, ErrorException) || rethrow()
            return Int
        end
    return Const(size)
end
@nospecs function sizeof_tfunc(𝕃::AbstractLattice, x)
    x = widenmustalias(x)
    isa(x, Const) && return _const_sizeof(x.val)
    isa(x, Conditional) && return _const_sizeof(Bool)
    isconstType(x) && return _const_sizeof(x.parameters[1])
    xu = unwrap_unionall(x)
    if isa(xu, Union)
        return tmerge(sizeof_tfunc(𝕃, rewrap_unionall(xu.a, x)),
                      sizeof_tfunc(𝕃, rewrap_unionall(xu.b, x)))
    end
    # Core.sizeof operates on either a type or a value. First check which
    # case we're in.
    t, exact = instanceof_tfunc(x, false)
    if t !== Bottom
        # The value corresponding to `x` at runtime could be a type.
        # Normalize the query to ask about that type.
        x = unwrap_unionall(t)
        if exact && isa(x, Union)
            isinline = uniontype_layout(x)[1]
            return isinline ? Const(Int(Core.sizeof(x))) : Bottom
        end
        isa(x, DataType) || return Int
        (isconcretetype(x) || isprimitivetype(x)) && return _const_sizeof(x)
    else
        x = widenconst(x)
        x !== DataType && isconcretetype(x) && return _const_sizeof(x)
        isprimitivetype(x) && return _const_sizeof(x)
    end
    return Int
end
add_tfunc(Core.sizeof, 1, 1, sizeof_tfunc, 1)
@nospecs function nfields_tfunc(𝕃::AbstractLattice, x)
    isa(x, Const) && return Const(nfields(x.val))
    isa(x, Conditional) && return Const(0)
    xt = widenconst(x)
    x = unwrap_unionall(xt)
    isconstType(x) && return Const(nfields(x.parameters[1]))
    if isa(x, DataType) && !isabstracttype(x)
        if x.name === Tuple.name
            isvatuple(x) && return Int
            return Const(length(x.types))
        elseif x.name === _NAMEDTUPLE_NAME
            length(x.parameters) == 2 || return Int
            names = x.parameters[1]
            isa(names, Tuple{Vararg{Symbol}}) || return nfields_tfunc(𝕃, rewrap_unionall(x.parameters[2], xt))
            return Const(length(names))
        else
            return Const(isdefined(x, :types) ? length(x.types) : length(x.name.names))
        end
    end
    if isa(x, Union)
        na = nfields_tfunc(𝕃, unwraptv(x.a))
        na === Int && return Int
        return tmerge(𝕃, na, nfields_tfunc(𝕃, unwraptv(x.b)))
    end
    return Int
end
add_tfunc(nfields, 1, 1, nfields_tfunc, 1)
add_tfunc(Core._expr, 1, INT_INF, @nospecs((𝕃::AbstractLattice, args...)->Expr), 100)
add_tfunc(svec, 0, INT_INF, @nospecs((𝕃::AbstractLattice, args...)->SimpleVector), 20)
@nospecs function typevar_tfunc(𝕃::AbstractLattice, n, lb_arg, ub_arg)
    lb = Union{}
    ub = Any
    ub_certain = lb_certain = true
    if isa(n, Const)
        nval = n.val
        isa(nval, Symbol) || return Union{}
        if isa(lb_arg, Const)
            lb = lb_arg.val
        else
            lb_arg = widenslotwrapper(lb_arg)
            if isType(lb_arg)
                lb = lb_arg.parameters[1]
                lb_certain = false
            else
                return TypeVar
            end
        end
        if isa(ub_arg, Const)
            ub = ub_arg.val
        else
            ub_arg = widenslotwrapper(ub_arg)
            if isType(ub_arg)
                ub = ub_arg.parameters[1]
                ub_certain = false
            else
                return TypeVar
            end
        end
        lb_valid = lb isa Type || lb isa TypeVar
        ub_valid = ub isa Type || ub isa TypeVar
        if lb_valid && ub_valid
            tv = TypeVar(nval, lb, ub)
            return PartialTypeVar(tv, lb_certain, ub_certain)
        elseif !lb_valid && lb_certain
            return Union{}
        elseif !ub_valid && ub_certain
            return Union{}
        end
    end
    return TypeVar
end
@nospecs function typebound_nothrow(𝕃::AbstractLattice, b)
    ⊑ = partialorder(𝕃)
    b = widenconst(b)
    (b ⊑ TypeVar) && return true
    if isType(b)
        return true
    end
    return false
end
@nospecs function typevar_nothrow(𝕃::AbstractLattice, n, lb, ub)
    ⊑ = partialorder(𝕃)
    n ⊑ Symbol || return false
    typebound_nothrow(𝕃, lb) || return false
    typebound_nothrow(𝕃, ub) || return false
    return true
end
add_tfunc(Core._typevar, 3, 3, typevar_tfunc, 100)

struct MemoryOrder x::Cint end
const MEMORY_ORDER_UNSPECIFIED = MemoryOrder(-2)
const MEMORY_ORDER_INVALID     = MemoryOrder(-1)
const MEMORY_ORDER_NOTATOMIC   = MemoryOrder(0)
const MEMORY_ORDER_UNORDERED   = MemoryOrder(1)
const MEMORY_ORDER_MONOTONIC   = MemoryOrder(2)
const MEMORY_ORDER_CONSUME     = MemoryOrder(3)
const MEMORY_ORDER_ACQUIRE     = MemoryOrder(4)
const MEMORY_ORDER_RELEASE     = MemoryOrder(5)
const MEMORY_ORDER_ACQ_REL     = MemoryOrder(6)
const MEMORY_ORDER_SEQ_CST     = MemoryOrder(7)

function get_atomic_order(order::Symbol, loading::Bool, storing::Bool)
    if order === :not_atomic
        return MEMORY_ORDER_NOTATOMIC
    elseif order === :unordered && (loading ⊻ storing)
        return MEMORY_ORDER_UNORDERED
    elseif order === :monotonic && (loading | storing)
        return MEMORY_ORDER_MONOTONIC
    elseif order === :acquire && loading
        return MEMORY_ORDER_ACQUIRE
    elseif order === :release && storing
        return MEMORY_ORDER_RELEASE
    elseif order === :acquire_release && (loading & storing)
        return MEMORY_ORDER_ACQ_REL
    elseif order === :sequentially_consistent
        return MEMORY_ORDER_SEQ_CST
    end
    return MEMORY_ORDER_INVALID
end

function pointer_eltype(@nospecialize(ptr))
    a = widenconst(ptr)
    if !has_free_typevars(a)
        unw = unwrap_unionall(a)
        if isa(unw, DataType) && unw.name === Ptr.body.name
            T = unw.parameters[1]
            valid_as_lattice(T, true) || return Bottom
            return rewrap_unionall(T, a)
        end
    end
    return Any
end

@nospecs function pointerarith_tfunc(𝕃::AbstractLattice, ptr, offset)
    return ptr
end
@nospecs function pointerref_tfunc(𝕃::AbstractLattice, a, i, align)
    return pointer_eltype(a)
end
@nospecs function pointerset_tfunc(𝕃::AbstractLattice, a, v, i, align)
    return a
end
@nospecs function atomic_fence_tfunc(𝕃::AbstractLattice, order)
    return Nothing
end
@nospecs function atomic_pointerref_tfunc(𝕃::AbstractLattice, a, order)
    return pointer_eltype(a)
end
@nospecs function atomic_pointerset_tfunc(𝕃::AbstractLattice, a, v, order)
    return a
end
@nospecs function atomic_pointerswap_tfunc(𝕃::AbstractLattice, a, v, order)
    return pointer_eltype(a)
end
@nospecs function atomic_pointermodify_tfunc(𝕃::AbstractLattice, ptr, op, v, order)
    a = widenconst(ptr)
    if !has_free_typevars(a)
        unw = unwrap_unionall(a)
        if isa(unw, DataType) && unw.name === Ptr.body.name
            T = unw.parameters[1]
            # note: we could sometimes refine this to a PartialStruct if we analyzed `op(T, T)::T`
            valid_as_lattice(T, true) || return Bottom
            return rewrap_unionall(Pair{T, T}, a)
        end
    end
    return Pair
end
@nospecs function atomic_pointerreplace_tfunc(𝕃::AbstractLattice, ptr, x, v, success_order, failure_order)
    a = widenconst(ptr)
    if !has_free_typevars(a)
        unw = unwrap_unionall(a)
        if isa(unw, DataType) && unw.name === Ptr.body.name
            T = unw.parameters[1]
            valid_as_lattice(T) || return Bottom
            return rewrap_unionall(ccall(:jl_apply_cmpswap_type, Any, (Any,), T), a)
        end
    end
    return ccall(:jl_apply_cmpswap_type, Any, (Any,), T) where T
end
add_tfunc(add_ptr, 2, 2, pointerarith_tfunc, 1)
add_tfunc(sub_ptr, 2, 2, pointerarith_tfunc, 1)
add_tfunc(pointerref, 3, 3, pointerref_tfunc, 4)
add_tfunc(pointerset, 4, 4, pointerset_tfunc, 5)
add_tfunc(atomic_fence, 1, 1, atomic_fence_tfunc, 4)
add_tfunc(atomic_pointerref, 2, 2, atomic_pointerref_tfunc, 4)
add_tfunc(atomic_pointerset, 3, 3, atomic_pointerset_tfunc, 5)
add_tfunc(atomic_pointerswap, 3, 3, atomic_pointerswap_tfunc, 5)
add_tfunc(atomic_pointermodify, 4, 4, atomic_pointermodify_tfunc, 5)
add_tfunc(atomic_pointerreplace, 5, 5, atomic_pointerreplace_tfunc, 5)
add_tfunc(donotdelete, 0, INT_INF, @nospecs((𝕃::AbstractLattice, args...)->Nothing), 0)
@nospecs function compilerbarrier_tfunc(𝕃::AbstractLattice, setting, val)
    # strongest barrier if a precise information isn't available at compiler time
    # XXX we may want to have "compile-time" error instead for such case
    isa(setting, Const) || return Any
    setting = setting.val
    isa(setting, Symbol) || return Any
    if setting === :const
        return widenconst(val)
    elseif setting === :conditional
        return widenconditional(val)
    elseif setting === :type
        return Any
    else
        return Bottom
    end
end
add_tfunc(compilerbarrier, 2, 2, compilerbarrier_tfunc, 5)
add_tfunc(Core.finalizer, 2, 4, @nospecs((𝕃::AbstractLattice, args...)->Nothing), 5)

@nospecs function compilerbarrier_nothrow(setting, val)
    return isa(setting, Const) && contains_is((:type, :const, :conditional), setting.val)
end

# more accurate typeof_tfunc for vararg tuples abstract only in length
function typeof_concrete_vararg(t::DataType)
    np = length(t.parameters)
    for i = 1:np
        p = t.parameters[i]
        if i == np && isvarargtype(p)
            if isdefined(p, :T) && isconcretetype(p.T)
                t = Type{Tuple{t.parameters[1:np-1]..., Vararg{p.T, N}}} where N
                if isdefined(p, :N)
                    return t{p.N}
                end
                return t
            end
        elseif !isconcretetype(p)
            break
        end
    end
    return nothing
end

@nospecs function typeof_tfunc(𝕃::AbstractLattice, t)
    isa(t, Const) && return Const(typeof(t.val))
    t = widenconst(t)
    if isType(t)
        tp = t.parameters[1]
        if hasuniquerep(tp)
            return Const(typeof(tp))
        end
    elseif isa(t, DataType)
        if isconcretetype(t)
            return Const(t)
        elseif t === Any
            return DataType
        else
            if t.name === Tuple.name
                tt = typeof_concrete_vararg(t)
                tt === nothing || return tt
            end
            return Type{<:t}
        end
    elseif isa(t, Union)
        a = widenconst(_typeof_tfunc(𝕃, t.a))
        b = widenconst(_typeof_tfunc(𝕃, t.b))
        return Union{a, b}
    elseif isa(t, UnionAll)
        u = unwrap_unionall(t)
        if isa(u, DataType) && !isabstracttype(u)
            if u.name === Tuple.name
                uu = typeof_concrete_vararg(u)
                if uu !== nothing
                    return rewrap_unionall(uu, t)
                end
            else
                return rewrap_unionall(Type{u}, t)
            end
        end
        return rewrap_unionall(widenconst(typeof_tfunc(𝕃, u)), t)
    end
    return DataType # typeof(anything)::DataType
end
# helper function of `typeof_tfunc`, which accepts `TypeVar`
@nospecs function _typeof_tfunc(𝕃::AbstractLattice, t)
    if isa(t, TypeVar)
        return t.ub !== Any ? _typeof_tfunc(𝕃, t.ub) : DataType
    end
    return typeof_tfunc(𝕃, t)
end
add_tfunc(typeof, 1, 1, typeof_tfunc, 1)

@nospecs function typeassert_tfunc(𝕃::AbstractLattice, v, t)
    t = instanceof_tfunc(t, true)[1]
    t === Any && return v
    return tmeet(𝕃, v, t)
end
add_tfunc(typeassert, 2, 2, typeassert_tfunc, 4)

@nospecs function typeassert_nothrow(𝕃::AbstractLattice, v, t)
    ⊑ = partialorder(𝕃)
    # ty, exact = instanceof_tfunc(t, true)
    # return exact && v ⊑ ty
    if (isType(t) && !has_free_typevars(t) && v ⊑ t.parameters[1]) ||
        (isa(t, Const) && isa(t.val, Type) && v ⊑ t.val)
        return true
    end
    return false
end

@nospecs function isa_tfunc(𝕃::AbstractLattice, v, tt)
    t, isexact = instanceof_tfunc(tt, true)
    if t === Bottom
        # check if t could be equivalent to typeof(Bottom), since that's valid in `isa`, but the set of `v` is empty
        # if `t` cannot have instances, it's also invalid on the RHS of isa
        hasintersect(widenconst(tt), Type) || return Union{}
        return Const(false)
    end
    if !has_free_typevars(t)
        if ⊑(𝕃, v, t)
            if isexact && isnotbrokensubtype(v, t)
                return Const(true)
            end
        else
            if isa(v, Const) || isa(v, Conditional)
                # this and the `isdispatchelem` below test for knowledge of a
                # leaftype appearing on the LHS (ensuring the isa is precise)
                return Const(false)
            end
            v = widenconst(v)
            isdispatchelem(v) && return Const(false)
            if !hasintersect(v, t)
                # similar to `isnotbrokensubtype` check above, `typeintersect(v, t)`
                # can't be trusted for kind types so we do an extra check here
                if !iskindtype(v)
                    return Const(false)
                end
            end
        end
    end
    # TODO: handle non-leaftype(t) by testing against lower and upper bounds
    return Bool
end
add_tfunc(isa, 2, 2, isa_tfunc, 1)

@nospecs function isa_nothrow(𝕃::AbstractLattice, obj, typ)
    ⊑ = partialorder(𝕃)
    return typ ⊑ Type
end

@nospecs function subtype_tfunc(𝕃::AbstractLattice, a, b)
    a, isexact_a = instanceof_tfunc(a, false)
    b, isexact_b = instanceof_tfunc(b, false)
    if !has_free_typevars(a) && !has_free_typevars(b)
        if a <: b
            if isexact_b || a === Bottom
                return Const(true)
            end
        else
            if isexact_a || (b !== Bottom && !hasintersect(a, b))
                return Const(false)
            end
        end
    end
    return Bool
end
add_tfunc(<:, 2, 2, subtype_tfunc, 10)

@nospecs function subtype_nothrow(𝕃::AbstractLattice, lty, rty)
    ⊑ = partialorder(𝕃)
    return lty ⊑ Type && rty ⊑ Type
end

function fieldcount_noerror(@nospecialize t)
    if t isa UnionAll || t isa Union
        t = argument_datatype(t)
        if t === nothing
            return nothing
        end
    elseif t === Union{}
        return 0
    end
    t isa DataType || return nothing
    if t.name === _NAMEDTUPLE_NAME
        names, types = t.parameters
        if names isa Tuple
            return length(names)
        end
        if types isa DataType && types <: Tuple
            return fieldcount_noerror(types)
        end
        return nothing
    elseif isabstracttype(t) || (t.name === Tuple.name && isvatuple(t))
        return nothing
    end
    return isdefined(t, :types) ? length(t.types) : length(t.name.names)
end

function try_compute_fieldidx(@nospecialize(typ), @nospecialize(field))
    typ = argument_datatype(typ)
    typ === nothing && return nothing
    if isa(field, Symbol)
        field = fieldindex(typ, field, false)
        field == 0 && return nothing
    elseif isa(field, Int)
        # Numerical field name can only be of type `Int`
        max_fields = fieldcount_noerror(typ)
        max_fields === nothing && return nothing
        (1 <= field <= max_fields) || return nothing
    else
        return nothing
    end
    return field
end

function getfield_boundscheck(argtypes::Vector{Any})
    if length(argtypes) == 2
        isvarargtype(argtypes[2]) && return :unsafe
        return :on
    elseif length(argtypes) == 3
        boundscheck = argtypes[3]
        isvarargtype(boundscheck) && return :unsafe
        if widenconst(boundscheck) === Symbol
            return :on
        end
    elseif length(argtypes) == 4
        boundscheck = argtypes[4]
        isvarargtype(boundscheck) && return :unsafe
    else
        return :unsafe
    end
    boundscheck = widenconditional(boundscheck)
    if widenconst(boundscheck) === Bool
        if isa(boundscheck, Const)
            return boundscheck.val::Bool ? :on : :off
        end
        return :unknown # including a case when specified as `:boundscheck`
    end
    return :unsafe
end

function getfield_nothrow(𝕃::AbstractLattice, argtypes::Vector{Any}, boundscheck::Symbol=getfield_boundscheck(argtypes))
    boundscheck === :unsafe && return false
    ordering = Const(:not_atomic)
    if length(argtypes) == 3
        isvarargtype(argtypes[3]) && return false
        if widenconst(argtypes[3]) !== Bool
            ordering = argtypes[3]
        end
    elseif length(argtypes) == 4
        ordering = argtypes[3]
    elseif length(argtypes) ≠ 2
        return false
    end
    isa(ordering, Const) || return false
    ordering = ordering.val
    isa(ordering, Symbol) || return false
    if ordering !== :not_atomic # TODO: this is assuming not atomic
        return false
    end
    return getfield_nothrow(𝕃, argtypes[1], argtypes[2], !(boundscheck === :off))
end
@nospecs function getfield_nothrow(𝕃::AbstractLattice, s00, name, boundscheck::Bool)
    # If we don't have boundscheck off and don't know the field, don't even bother
    if boundscheck
        isa(name, Const) || return false
    end

    ⊑ = partialorder(𝕃)

    # If we have s00 being a const, we can potentially refine our type-based analysis above
    if isa(s00, Const) || isconstType(s00) || isa(s00, PartialStruct)
        if isa(s00, Const)
            sv = s00.val
            sty = typeof(sv)
            nflds = nfields(sv)
            ismod = sv isa Module
        elseif isa(s00, PartialStruct)
            sty = unwrap_unionall(s00.typ)
            nflds = fieldcount_noerror(sty)
            ismod = false
        else
            sv = (s00::DataType).parameters[1]
            sty = typeof(sv)
            nflds = nfields(sv)
            ismod = sv isa Module
        end
        if isa(name, Const)
            nval = name.val
            if !isa(nval, Symbol)
                ismod && return false
                isa(nval, Int) || return false
            end
            return isdefined_tfunc(𝕃, s00, name) === Const(true)
        end

        # If bounds checking is disabled and all fields are assigned,
        # we may assume that we don't throw
        @assert !boundscheck
        ismod && return false
        name ⊑ Int || name ⊑ Symbol || return false
        sty.name.n_uninitialized == 0 && return true
        nflds === nothing && return false
        for i = (datatype_min_ninitialized(sty)+1):nflds
            isdefined_tfunc(𝕃, s00, Const(i)) === Const(true) || return false
        end
        return true
    end

    s0 = widenconst(s00)
    s = unwrap_unionall(s0)
    if isa(s, Union)
        return getfield_nothrow(𝕃, rewrap_unionall(s.a, s00), name, boundscheck) &&
               getfield_nothrow(𝕃, rewrap_unionall(s.b, s00), name, boundscheck)
    elseif isType(s) && isTypeDataType(s.parameters[1])
        s = s0 = DataType
    end
    if isa(s, DataType)
        # Can't say anything about abstract types
        isabstracttype(s) && return false
        # If all fields are always initialized, and bounds check is disabled,
        # we can assume we don't throw
        if !boundscheck && s.name.n_uninitialized == 0
            name ⊑ Int || name ⊑ Symbol || return false
            return true
        end
        # Else we need to know what the field is
        isa(name, Const) || return false
        field = try_compute_fieldidx(s, name.val)
        field === nothing && return false
        isfieldatomic(s, field) && return false # TODO: currently we're only testing for ordering === :not_atomic
        field <= datatype_min_ninitialized(s) && return true
        # `try_compute_fieldidx` already check for field index bound.
        !isvatuple(s) && isbitstype(fieldtype(s0, field)) && return true
    end

    return false
end

@nospecs function getfield_tfunc(𝕃::AbstractLattice, s00, name, boundscheck_or_order)
    if !isvarargtype(boundscheck_or_order)
        t = widenconst(boundscheck_or_order)
        hasintersect(t, Symbol) || hasintersect(t, Bool) || return Bottom
    end
    return getfield_tfunc(𝕃, s00, name)
end
@nospecs function getfield_tfunc(𝕃::AbstractLattice, s00, name, order, boundscheck)
    hasintersect(widenconst(order), Symbol) || return Bottom
    if !isvarargtype(boundscheck)
        hasintersect(widenconst(boundscheck), Bool) || return Bottom
    end
    return getfield_tfunc(𝕃, s00, name)
end
@nospecs function getfield_tfunc(𝕃::AbstractLattice, s00, name)
    _getfield_tfunc(𝕃, s00, name, false)
end

function _getfield_fieldindex(s::DataType, name::Const)
    nv = name.val
    if isa(nv, Symbol)
        nv = fieldindex(s, nv, false)
    end
    if isa(nv, Int)
        return nv
    end
    return nothing
end

function _getfield_tfunc_const(@nospecialize(sv), name::Const)
    nv = _getfield_fieldindex(typeof(sv), name)
    nv === nothing && return Bottom
    if isa(sv, DataType) && nv == DATATYPE_TYPES_FIELDINDEX && isdefined(sv, nv)
        return Const(getfield(sv, nv))
    end
    if !isa(sv, Module) && isconst(typeof(sv), nv)
        if isdefined(sv, nv)
            return Const(getfield(sv, nv))
        end
        return Bottom
    end
    return nothing
end

@nospecs function _getfield_tfunc(𝕃::InferenceLattice, s00, name, setfield::Bool)
    if isa(s00, LimitedAccuracy)
        # This will error, but it's better than duplicating the error here
        s00 = widenconst(s00)
    end
    return _getfield_tfunc(widenlattice(𝕃), s00, name, setfield)
end

@nospecs function _getfield_tfunc(𝕃::AnyConditionalsLattice, s00, name, setfield::Bool)
    if isa(s00, AnyConditional)
        return Bottom # Bool has no fields
    end
    return _getfield_tfunc(widenlattice(𝕃), s00, name, setfield)
end

@nospecs function _getfield_tfunc(𝕃::AnyMustAliasesLattice, s00, name, setfield::Bool)
    return _getfield_tfunc(widenlattice(𝕃), widenmustalias(s00), widenmustalias(name), setfield)
end

@nospecs function _getfield_tfunc(𝕃::PartialsLattice, s00, name, setfield::Bool)
    if isa(s00, PartialStruct)
        s = widenconst(s00)
        sty = unwrap_unionall(s)::DataType
        if isa(name, Const)
            nv = _getfield_fieldindex(sty, name)
            if isa(nv, Int) && !is_field_maybe_undef(s00, nv)
                return unwrapva(partialstruct_getfield(s00, nv))
            end
        end
        s00 = s
    end
    return _getfield_tfunc(widenlattice(𝕃), s00, name, setfield)
end

@nospecs function _getfield_tfunc(𝕃::ConstsLattice, s00, name, setfield::Bool)
    if isa(s00, Const)
        sv = s00.val
        if isa(name, Const)
            nv = name.val
            if isa(sv, Module)
                setfield && return Bottom
                if isa(nv, Symbol)
                    # In ordinary inference, this case is intercepted early and
                    # re-routed to `getglobal`.
                    return Any
                end
                return Bottom
            end
            r = _getfield_tfunc_const(sv, name)
            r !== nothing && return r
        end
        s00 = widenconst(s00)
    end
    return _getfield_tfunc(widenlattice(𝕃), s00, name, setfield)
end

@nospecs function _getfield_tfunc(𝕃::JLTypeLattice, s00, name, setfield::Bool)
    s = unwrap_unionall(s00)
    if isa(s, Union)
        return tmerge(_getfield_tfunc(𝕃, rewrap_unionall(s.a, s00), name, setfield),
                      _getfield_tfunc(𝕃, rewrap_unionall(s.b, s00), name, setfield))
    end
    if isType(s)
        if isconstType(s)
            sv = (s00::DataType).parameters[1]
            if isa(name, Const)
                r = _getfield_tfunc_const(sv, name)
                r !== nothing && return r
            end
            s = typeof(sv)
        else
            sv = s.parameters[1]
            if isTypeDataType(sv) && isa(name, Const)
                nv = _getfield_fieldindex(DataType, name)::Int
                if nv == DATATYPE_NAME_FIELDINDEX
                    # N.B. This only works for fields that do not depend on type
                    # parameters (which we do not know here).
                    return Const(sv.name)
                end
                s = DataType
            end
        end
    end
    isa(s, DataType) || return Any
    isabstracttype(s) && return Any
    if s <: Tuple && !hasintersect(widenconst(name), Int)
        return Bottom
    end
    if s <: Module
        setfield && return Bottom
        hasintersect(widenconst(name), Symbol) || return Bottom
        return Any
    end
    if s.name === _NAMEDTUPLE_NAME && !isconcretetype(s)
        if isa(name, Const) && isa(name.val, Symbol)
            if isa(s.parameters[1], Tuple)
                name = Const(Int(ccall(:jl_field_index, Cint, (Any, Any, Cint), s, name.val, false)+1))
            else
                name = Int
            end
        elseif Symbol ⊑ name
            name = Int
        end
        _ts = unwraptv(s.parameters[2])
        _ts = rewrap_unionall(_ts, s00)
        if !(_ts <: Tuple)
            return Any
        end
        return _getfield_tfunc(𝕃, _ts, name, setfield)
    end
    ftypes = datatype_fieldtypes(s)
    nf = length(ftypes)
    # If no value has this type, then this statement should be unreachable.
    # Bail quickly now.
    if !has_concrete_subtype(s) || nf == 0
        return Bottom
    end
    if isa(name, Conditional)
        return Bottom # can't index fields with Bool
    end
    if !isa(name, Const)
        name = widenconst(name)
        if !(Int <: name || Symbol <: name)
            return Bottom
        end
        if nf == 1
            fld = 1
        else
            # union together types of all fields
            t = Bottom
            for i in 1:nf
                _ft = unwrapva(ftypes[i])
                valid_as_lattice(_ft, true) || continue
                setfield && isconst(s, i) && continue
                t = tmerge(t, rewrap_unionall(_ft, s00))
                t === Any && break
            end
            return t
        end
    else
        fld = _getfield_fieldindex(s, name)
        fld === nothing && return Bottom
    end
    if s <: Tuple && fld >= nf && isvarargtype(ftypes[nf])
        R = unwrapva(ftypes[nf])
    else
        if fld < 1 || fld > nf
            return Bottom
        elseif setfield && isconst(s, fld)
            return Bottom
        end
        R = ftypes[fld]
        valid_as_lattice(R, true) || return Bottom
        if isempty(s.parameters)
            return R
        end
    end
    return rewrap_unionall(R, s00)
end

# checks if a field of this type is guaranteed to be defined to a value
# and that access to an uninitialized field will cause an `UndefRefError` or return zero
# - is_undefref_fieldtype(String) === true
# - is_undefref_fieldtype(Integer) === true
# - is_undefref_fieldtype(Any) === true
# - is_undefref_fieldtype(Int) === false
# - is_undefref_fieldtype(Union{Int32,Int64}) === false
# - is_undefref_fieldtype(T) === false
function is_undefref_fieldtype(@nospecialize ftyp)
    return !has_free_typevars(ftyp) && !allocatedinline(ftyp)
end

@nospecs function setfield!_tfunc(𝕃::AbstractLattice, o, f, v, order)
    if !isvarargtype(order)
        hasintersect(widenconst(order), Symbol) || return Bottom
    end
    return setfield!_tfunc(𝕃, o, f, v)
end
@nospecs function setfield!_tfunc(𝕃::AbstractLattice, o, f, v)
    mutability_errorcheck(o) || return Bottom
    ft = _getfield_tfunc(𝕃, o, f, true)
    ft === Bottom && return Bottom
    hasintersect(widenconst(v), widenconst(ft)) || return Bottom
    return v
end
mutability_errorcheck(@nospecialize obj) = _mutability_errorcheck(widenconst(obj))
function _mutability_errorcheck(@nospecialize objt0)
    objt = unwrap_unionall(objt0)
    if isa(objt, Union)
        return _mutability_errorcheck(rewrap_unionall(objt.a, objt0)) ||
               _mutability_errorcheck(rewrap_unionall(objt.b, objt0))
    elseif isa(objt, DataType)
        # Can't say anything about abstract types
        isabstracttype(objt) && return true
        return ismutabletype(objt)
    end
    return true
end

@nospecs function setfield!_nothrow(𝕃::AbstractLattice, s00, name, v, order)
    order === Const(:not_atomic) || return false # currently setfield!_nothrow is assuming not atomic
    return setfield!_nothrow(𝕃, s00, name, v)
end
@nospecs function setfield!_nothrow(𝕃::AbstractLattice, s00, name, v)
    s0 = widenconst(s00)
    s = unwrap_unionall(s0)
    if isa(s, Union)
        return setfield!_nothrow(𝕃, rewrap_unionall(s.a, s00), name, v) &&
               setfield!_nothrow(𝕃, rewrap_unionall(s.b, s00), name, v)
    elseif isa(s, DataType)
        # Can't say anything about abstract types
        isabstracttype(s) && return false
        ismutabletype(s) || return false
        isa(name, Const) || return false
        field = try_compute_fieldidx(s, name.val)
        field === nothing && return false
        # `try_compute_fieldidx` already check for field index bound.
        isconst(s, field) && return false
        isfieldatomic(s, field) && return false # TODO: currently we're only testing for ordering === :not_atomic
        v_expected = fieldtype(s0, field)
        ⊑ = partialorder(𝕃)
        return v ⊑ v_expected
    end
    return false
end

@nospecs function swapfield!_tfunc(𝕃::AbstractLattice, o, f, v, order=Symbol)
    setfield!_tfunc(𝕃, o, f, v) === Bottom && return Bottom
    return getfield_tfunc(𝕃, o, f)
end
@nospecs function modifyfield!_tfunc(𝕃::AbstractLattice, o, f, op, v, order=Symbol)
    o′ = widenconst(o)
    T = _fieldtype_tfunc(𝕃, o′, f, isconcretetype(o′))
    T === Bottom && return Bottom
    PT = Const(Pair)
    return instanceof_tfunc(apply_type_tfunc(𝕃, Any[PT, T, T]), true)[1]
end
@nospecs function replacefield!_tfunc(𝕃::AbstractLattice, o, f, x, v, success_order=Symbol, failure_order=Symbol)
    o′ = widenconst(o)
    T = _fieldtype_tfunc(𝕃, o′, f, isconcretetype(o′))
    T === Bottom && return Bottom
    PT = Const(ccall(:jl_apply_cmpswap_type, Any, (Any,), T) where T)
    return instanceof_tfunc(apply_type_tfunc(𝕃, Any[PT, T]), true)[1]
end
@nospecs function setfieldonce!_tfunc(𝕃::AbstractLattice, o, f, v, success_order=Symbol, failure_order=Symbol)
    setfield!_tfunc(𝕃, o, f, v) === Bottom && return Bottom
    isdefined_tfunc(𝕃, o, f) === Const(true) && return Const(false)
    return Bool
end

@nospecs function abstract_modifyop!(interp::AbstractInterpreter, ff, argtypes::Vector{Any}, si::StmtInfo, sv::AbsIntState)
    if ff === modifyfield!
        minargs = 5
        maxargs = 6
        op_argi = 4
        v_argi = 5
    elseif ff === Core.modifyglobal!
        minargs = 5
        maxargs = 6
        op_argi = 4
        v_argi = 5
    elseif ff === Core.memoryrefmodify!
        minargs = 6
        maxargs = 6
        op_argi = 3
        v_argi = 4
    elseif ff === atomic_pointermodify
        minargs = 5
        maxargs = 5
        op_argi = 3
        v_argi = 4
    else
        @assert false "unreachable"
    end

    nargs = length(argtypes)
    if !isempty(argtypes) && isvarargtype(argtypes[nargs])
        nargs - 1 <= maxargs || return Future(CallMeta(Bottom, Any, EFFECTS_THROWS, NoCallInfo()))
        nargs + 1 >= op_argi || return Future(CallMeta(Any, Any, Effects(), NoCallInfo()))
    else
        minargs <= nargs <= maxargs || return Future(CallMeta(Bottom, Any, EFFECTS_THROWS, NoCallInfo()))
    end
    𝕃ᵢ = typeinf_lattice(interp)
    if ff === modifyfield!
        o = unwrapva(argtypes[2])
        f = unwrapva(argtypes[3])
        RT = modifyfield!_tfunc(𝕃ᵢ, o, f, Any, Any, Symbol)
        TF = getfield_tfunc(𝕃ᵢ, o, f)
    elseif ff === Core.modifyglobal!
        o = unwrapva(argtypes[2])
        f = unwrapva(argtypes[3])
        GT = abstract_eval_get_binding_type(interp, sv, o, f).rt
        RT = isa(GT, Const) ? Pair{GT.val, GT.val} : Pair
        TF = isa(GT, Const) ? GT.val : Any
    elseif ff === Core.memoryrefmodify!
        o = unwrapva(argtypes[2])
        RT = memoryrefmodify!_tfunc(𝕃ᵢ, o, Any, Any, Symbol, Bool)
        TF = memoryrefget_tfunc(𝕃ᵢ, o, Symbol, Bool)
    elseif ff === atomic_pointermodify
        o = unwrapva(argtypes[2])
        RT = atomic_pointermodify_tfunc(𝕃ᵢ, o, Any, Any, Symbol)
        TF = atomic_pointerref_tfunc(𝕃ᵢ, o, Symbol)
    else
        @assert false "unreachable"
    end
    info = NoCallInfo()
    if nargs >= v_argi && RT !== Bottom
        # we may be able to refine this to a PartialStruct by analyzing `op(o.f, v)::T`
        # as well as compute the info for the method matches
        op = unwrapva(argtypes[op_argi])
        v = unwrapva(argtypes[v_argi])
        callinfo = abstract_call(interp, ArgInfo(nothing, Any[op, TF, v]), StmtInfo(true, si.saw_latestworld), sv, #=max_methods=#1)
        TF = Core.Box(TF)
        RT = Core.Box(RT)
        return Future{CallMeta}(callinfo, interp, sv) do callinfo, interp, sv
            TF = TF.contents
            RT = RT.contents
            TF2 = tmeet(ipo_lattice(interp), callinfo.rt, widenconst(TF))
            if TF2 === Bottom
                RT = Bottom
            elseif isconcretetype(RT) && has_nontrivial_extended_info(𝕃ᵢ, TF2) # isconcrete condition required to form a PartialStruct
                RT = PartialStruct(fallback_lattice, RT, Any[TF, TF2])
            end
            info = ModifyOpInfo(callinfo.info)
            return CallMeta(RT, Any, Effects(), info)
        end
    end
    return Future(CallMeta(RT, Any, Effects(), info))
end

# we could use tuple_tfunc instead of widenconst, but `o` is mutable, so that is unlikely to be beneficial

add_tfunc(getfield, 2, 4, getfield_tfunc, 1)
add_tfunc(setfield!, 3, 4, setfield!_tfunc, 3)
add_tfunc(swapfield!, 3, 4, swapfield!_tfunc, 3)
add_tfunc(modifyfield!, 4, 5, modifyfield!_tfunc, 3)
add_tfunc(replacefield!, 4, 6, replacefield!_tfunc, 3)
add_tfunc(setfieldonce!, 3, 5, setfieldonce!_tfunc, 3)

@nospecs function fieldtype_nothrow(𝕃::AbstractLattice, s0, name)
    s0 === Bottom && return true # unreachable
    ⊑ = partialorder(𝕃)
    if s0 === Any || s0 === Type || DataType ⊑ s0 || UnionAll ⊑ s0
        # We have no idea
        return false
    end

    if !isa(name, Const) || (!isa(name.val, Symbol) && !isa(name.val, Int))
        # Due to bounds checking, we can't say anything unless we know what
        # the name is.
        return false
    end

    su = unwrap_unionall(s0)
    if isa(su, Union)
        return fieldtype_nothrow(𝕃, rewrap_unionall(su.a, s0), name) &&
               fieldtype_nothrow(𝕃, rewrap_unionall(su.b, s0), name)
    end

    s, exact = instanceof_tfunc(s0, false)
    s === Bottom && return false # always
    return _fieldtype_nothrow(s, exact, name)
end

function _fieldtype_nothrow(@nospecialize(s), exact::Bool, name::Const)
    u = unwrap_unionall(s)
    if isa(u, Union)
        a = _fieldtype_nothrow(u.a, exact, name)
        b = _fieldtype_nothrow(u.b, exact, name)
        return exact ? (a || b) : (a && b)
    end
    u isa DataType || return false
    isabstracttype(u) && return false
    if u.name === _NAMEDTUPLE_NAME && !isconcretetype(u)
        # TODO: better approximate inference
        return false
    end
    fld = name.val
    if isa(fld, Symbol)
        fld = fieldindex(u, fld, false)
    end
    isa(fld, Int) || return false
    ftypes = datatype_fieldtypes(u)
    nf = length(ftypes)
    fld >= 1 || return false
    if u.name === Tuple.name && nf > 0 && isvarargtype(ftypes[nf])
        if !exact && fld >= nf
            # If we don't know the exact type, the length of the tuple will be determined
            # at runtime and we can't say anything.
            return false
        end
    elseif fld > nf
        return false
    end
    return true
end

@nospecs function fieldtype_tfunc(𝕃::AbstractLattice, s0, name, boundscheck)
    return fieldtype_tfunc(𝕃, s0, name)
end
@nospecs function fieldtype_tfunc(𝕃::AbstractLattice, s0, name)
    s0 = widenmustalias(s0)
    if s0 === Bottom
        return Bottom
    end
    if s0 === Any || s0 === Type || DataType ⊑ s0 || UnionAll ⊑ s0
        # For a generic DataType, one of the fields could still be a TypeVar
        # which is not a Type. Tuple{...} can also contain Symbols etc.
        return Any
    end
    # fieldtype only accepts Types
    if isa(s0, Const) && !(isa(s0.val, DataType) || isa(s0.val, UnionAll) || isa(s0.val, Union))
        return Bottom
    end
    if (s0 isa Type && s0 == Type{Union{}}) || isa(s0, Conditional)
        return Bottom
    end

    su = unwrap_unionall(s0)
    if isa(su, Union)
        return tmerge(fieldtype_tfunc(𝕃, rewrap_unionall(su.a, s0), name),
                      fieldtype_tfunc(𝕃, rewrap_unionall(su.b, s0), name))
    end

    s, exact = instanceof_tfunc(s0, false)
    s === Bottom && return Bottom
    return _fieldtype_tfunc(𝕃, s, name, exact)
end

@nospecs function _fieldtype_tfunc(𝕃::AbstractLattice, s, name, exact::Bool)
    exact = exact && !has_free_typevars(s)
    u = unwrap_unionall(s)
    if isa(u, Union)
        ta0 = _fieldtype_tfunc(𝕃, rewrap_unionall(u.a, s), name, exact)
        tb0 = _fieldtype_tfunc(𝕃, rewrap_unionall(u.b, s), name, exact)
        ta0 ⊑ tb0 && return tb0
        tb0 ⊑ ta0 && return ta0
        ta, exacta, _, istypea = instanceof_tfunc(ta0, false)
        tb, exactb, _, istypeb = instanceof_tfunc(tb0, false)
        if exact && exacta && exactb
            return Const(Union{ta, tb})
        end
        if istypea && istypeb
            return Type{<:Union{ta, tb}}
        end
        return Any
    end
    u isa DataType || return Any
    if isabstracttype(u)
        # Abstract types have no fields
        exact && return Bottom
        # Type{...} without free typevars has no subtypes, so it is actually
        # exact, even if `exact` is false.
        isType(u) && !has_free_typevars(u.parameters[1]) && return Bottom
        return Any
    end
    if u.name === _NAMEDTUPLE_NAME && !isconcretetype(u)
        # TODO: better approximate inference
        return Union{Type, TypeVar}
    end
    ftypes = datatype_fieldtypes(u)
    if isempty(ftypes)
        return Bottom
    end

    if !isa(name, Const)
        name = widenconst(name)
        if !(Int <: name || Symbol <: name)
            return Bottom
        end
        t = Bottom
        for i in 1:length(ftypes)
            ft1 = unwrapva(ftypes[i])
            exactft1 = exact || (!has_free_typevars(ft1) && u.name !== Tuple.name)
            ft1 = rewrap_unionall(ft1, s)
            if exactft1
                if hasuniquerep(ft1)
                    ft1 = Const(ft1) # ft unique via type cache
                else
                    ft1 = Type{ft1}
                end
            elseif ft1 isa Type || ft1 isa TypeVar
                if ft1 === Any && u.name === Tuple.name
                    # Tuple{:x} is possible in this case
                    ft1 = Any
                else
                    ft1 = Type{ft} where ft<:ft1
                end
            else
                ft1 = Const(ft1)
            end
            t = tmerge(t, ft1)
            t === Any && break
        end
        return t
    end

    fld = name.val
    if isa(fld, Symbol)
        fld = fieldindex(u, fld, false)
    end
    if !isa(fld, Int)
        return Bottom
    end
    nf = length(ftypes)
    if u.name === Tuple.name && fld >= nf && isvarargtype(ftypes[nf])
        ft = unwrapva(ftypes[nf])
    elseif fld < 1 || fld > nf
        return Bottom
    else
        ft = ftypes[fld]
    end
    if !isa(ft, Type) && !isa(ft, TypeVar)
        return Const(ft)
    end

    exactft = exact || (!has_free_typevars(ft) && u.name !== Tuple.name)
    ft = rewrap_unionall(ft, s)
    if exactft
        if hasuniquerep(ft)
            return Const(ft) # ft unique via type cache
        end
        return Type{ft}
    end
    if u.name === Tuple.name && ft === Any
        # Tuple{:x} is possible
        return Any
    end
    return Type{<:ft}
end
add_tfunc(fieldtype, 2, 3, fieldtype_tfunc, 0)

# Like `valid_tparam`, but in the type domain.
valid_tparam_type(T::DataType) = valid_typeof_tparam(T)
valid_tparam_type(U::Union) = valid_tparam_type(U.a) && valid_tparam_type(U.b)
valid_tparam_type(U::UnionAll) = valid_tparam_type(unwrap_unionall(U))

function apply_type_nothrow(𝕃::AbstractLattice, argtypes::Vector{Any}, @nospecialize(rt))
    rt === Type && return false
    length(argtypes) >= 1 || return false
    headtypetype = argtypes[1]
    if isa(headtypetype, Const)
        headtype = headtypetype.val
    elseif isconstType(headtypetype)
        headtype = headtypetype.parameters[1]
    else
        return false
    end
    # We know the apply_type is well formed. Otherwise our rt would have been
    # Bottom (or Type).
    (headtype === Union) && return true
    isa(rt, Const) && return true
    u = headtype
    # TODO: implement optimization for isvarargtype(u) and istuple occurrences (which are valid but are not UnionAll)
    for i = 2:length(argtypes)
        isa(u, UnionAll) || return false
        ai = widenconditional(argtypes[i])
        if ⊑(𝕃, ai, TypeVar) || ai === DataType
            # We don't know anything about the bounds of this typevar, but as
            # long as the UnionAll is not constrained, that's ok.
            if !(u.var.lb === Union{} && u.var.ub === Any)
                return false
            end
        elseif (isa(ai, Const) && isa(ai.val, Type)) || isconstType(ai)
            ai = isa(ai, Const) ? ai.val : (ai::DataType).parameters[1]
            if has_free_typevars(u.var.lb) || has_free_typevars(u.var.ub)
                return false
            end
            if !(u.var.lb <: ai <: u.var.ub)
                return false
            end
        else
            T, exact, _, istype = instanceof_tfunc(ai, false)
            if T === Bottom
                if !(u.var.lb === Union{} && u.var.ub === Any)
                    return false
                end
                if !valid_tparam_type(widenconst(ai))
                    return false
                end
            else
                istype || return false
                if isa(u.var.ub, TypeVar) || !(T <: u.var.ub)
                    return false
                end
                if exact ? !(u.var.lb <: T) : !(u.var.lb === Bottom)
                    return false
                end
            end
        end
        u = u.body
    end
    return true
end

const _tvarnames = Symbol[:_A, :_B, :_C, :_D, :_E, :_F, :_G, :_H, :_I, :_J, :_K, :_L, :_M,
                          :_N, :_O, :_P, :_Q, :_R, :_S, :_T, :_U, :_V, :_W, :_X, :_Y, :_Z]

function apply_type_tfunc(𝕃::AbstractLattice, argtypes::Vector{Any};
                          max_union_splitting::Int=InferenceParams().max_union_splitting)
    if isempty(argtypes)
        return Bottom
    end
    headtypetype = argtypes[1]
    headtypetype = widenslotwrapper(headtypetype)
    if isa(headtypetype, Const)
        headtype = headtypetype.val
    elseif isconstType(headtypetype)
        headtype = headtypetype.parameters[1]
    else
        return Any
    end
    largs = length(argtypes)
    if largs > 1 && isvarargtype(argtypes[end])
        return isvarargtype(headtype) ? TypeofVararg : Type
    end
    if headtype === Union
        largs == 1 && return Const(Bottom)
        hasnonType = false
        for i = 2:largs
            ai = argtypes[i]
            if isa(ai, Const)
                if !isa(ai.val, Type)
                    if isa(ai.val, TypeVar)
                        hasnonType = true
                    else
                        return Bottom
                    end
                end
            else
                if !isType(ai)
                    if !isa(ai, Type) || hasintersect(ai, Type) || hasintersect(ai, TypeVar)
                        hasnonType = true
                    else
                        return Bottom
                    end
                end
            end
        end
        if largs == 2 # Union{T} --> T
            return tmeet(widenconst(argtypes[2]), Union{Type,TypeVar})
        end
        hasnonType && return Type
        ty = Union{}
        allconst = true
        for i = 2:largs
            ai = argtypes[i]
            if isType(ai)
                aty = ai.parameters[1]
                allconst &= hasuniquerep(aty)
            else
                aty = (ai::Const).val
            end
            ty = Union{ty, aty}
        end
        return allconst ? Const(ty) : Type{ty}
    end
    if 1 < unionsplitcost(𝕃, argtypes) ≤ max_union_splitting
        rt = Bottom
        for split_argtypes = switchtupleunion(𝕃, argtypes)
            this_rt = widenconst(_apply_type_tfunc(𝕃, headtype, split_argtypes))
            rt = Union{rt, this_rt}
        end
        return rt
    end
    return _apply_type_tfunc(𝕃, headtype, argtypes)
end
@nospecs function _apply_type_tfunc(𝕃::AbstractLattice, headtype, argtypes::Vector{Any})
    largs = length(argtypes)
    istuple = headtype === Tuple
    if !istuple && !isa(headtype, UnionAll) && !isvarargtype(headtype)
        return Union{}
    end
    uw = unwrap_unionall(headtype)
    uncertain = false
    canconst = true
    tparams = Any[]
    outervars = TypeVar[]

    # first push the tailing vars from headtype into outervars
    outer_start, ua = 0, headtype
    while isa(ua, UnionAll)
        if (outer_start += 1) > largs - 1
            push!(outervars, ua.var)
        end
        ua = ua.body
    end
    if largs - 1 > outer_start && isa(headtype, UnionAll) # e.g. !isvarargtype(ua) && !istuple
        return Bottom # too many arguments
    end
    outer_start = outer_start - largs + 2

    varnamectr = 1
    ua = headtype
    for i = 2:largs
        ai = widenslotwrapper(argtypes[i])
        if isType(ai)
            aip1 = ai.parameters[1]
            canconst &= !has_free_typevars(aip1)
            push!(tparams, aip1)
        elseif isa(ai, Const) && (isa(ai.val, Type) || isa(ai.val, TypeVar) ||
                                  valid_tparam(ai.val) || (istuple && isvarargtype(ai.val)))
            push!(tparams, ai.val)
        elseif isa(ai, PartialTypeVar)
            canconst = false
            push!(tparams, ai.tv)
        else
            uncertain = true
            unw = unwrap_unionall(ai)
            isT = isType(unw)
            # compute our desired upper bound value
            if isT
                ub = rewrap_unionall(unw.parameters[1], ai)
            else
                ub = Any
            end
            if !istuple && unionall_depth(ai) > 3
                # Heuristic: if we are adding more than N unknown parameters here to the
                # outer type, use the wrapper type, instead of letting it nest more
                # complexity here. This is not monotonic, but seems to work out pretty well.
                if isT
                    ub = unwrap_unionall(unw.parameters[1])
                    if ub isa DataType
                        ub = ub.name.wrapper
                        unw = Type{unwrap_unionall(ub)}
                        ai = rewrap_unionall(unw, ub)
                    else
                        isT = false
                        ai = unw = ub = Any
                    end
                else
                    isT = false
                    ai = unw = ub = Any
                end
            elseif !isT
                # if we didn't have isType to compute ub directly, try to use instanceof_tfunc to refine this guess
                ai_w = widenconst(ai)
                ub = ai_w isa Type && ai_w <: Type ? instanceof_tfunc(ai, false)[1] : Any
            end
            if istuple
                # in the last parameter of a Tuple type, if the upper bound is Any
                # then this could be a Vararg type.
                if i == largs && ub === Any
                    ub = Vararg
                end
                push!(tparams, ub)
            elseif isT
                tai = ai
                while isa(tai, UnionAll)
                    # make sure vars introduced here are unique
                    if contains_is(outervars, tai.var)
                        ai = rename_unionall(ai)
                        unw = unwrap_unionall(ai)::DataType
                        # ub = rewrap_unionall(unw, ai)
                        break
                    end
                    tai = tai.body
                end
                push!(tparams, unw.parameters[1])
                while isa(ai, UnionAll)
                    push!(outervars, ai.var)
                    ai = ai.body
                end
            else
                # Is this the second parameter to a NamedTuple?
                if isa(uw, DataType) && uw.name === _NAMEDTUPLE_NAME && isa(ua, UnionAll) && uw.parameters[2] === ua.var
                    # If the names are known, keep the upper bound, but otherwise widen to Tuple.
                    # This is a widening heuristic to avoid keeping type information
                    # that's unlikely to be useful.
                    if !(uw.parameters[1] isa Tuple || (i == 3 && tparams[1] isa Tuple))
                        ub = Any
                    end
                else
                    ub = Any
                end
                tvname = varnamectr <= length(_tvarnames) ? _tvarnames[varnamectr] : :_Z
                varnamectr += 1
                v = TypeVar(tvname, ub)
                push!(tparams, v)
                push!(outervars, v)
            end
        end
        if ua isa UnionAll
            ua = ua.body
            #otherwise, sometimes ua isa Vararg (Core.TypeofVararg) or Tuple (DataType)
        end
    end
    local appl
    try
        appl = apply_type(headtype, tparams...)
    catch ex
        ex isa InterruptException && rethrow()
        # type instantiation might fail if one of the type parameters doesn't
        # match, which could happen only if a type estimate is too coarse
        # and might guess a concrete value while the actual type for it is Bottom
        if !uncertain
            return Union{}
        end
        canconst = false
        uncertain = true
        empty!(outervars)
        outer_start = 1
        # FIXME: if these vars are substituted with TypeVar here, the result
        # might be wider than the input, so should we use the `.name.wrapper`
        # object here instead, to replace all of these outervars with
        # unconstrained ones? Note that this code is nearly unreachable though,
        # and possibly should simply return Union{} here also, since
        # `apply_type` is already quite conservative about detecting and
        # throwing errors.
        appl = headtype
        if isa(appl, UnionAll)
            for _ = 2:largs
                appl = appl::UnionAll
                push!(outervars, appl.var)
                appl = appl.body
            end
        end
    end
    !uncertain && canconst && return Const(appl)
    if isvarargtype(appl)
        return TypeofVararg
    end
    if istuple
        return Type{<:appl}
    end
    ans = Type{appl}
    for i = length(outervars):-1:outer_start
        ans = UnionAll(outervars[i], ans)
    end
    return ans
end
@nospecs apply_type_tfunc(𝕃::AbstractLattice, headtypetype, args...) =
    apply_type_tfunc(𝕃, Any[i == 0 ? headtypetype : args[i] for i in 0:length(args)])
add_tfunc(apply_type, 1, INT_INF, apply_type_tfunc, 10)

# convert the dispatch tuple type argtype to the real (concrete) type of
# the tuple of those values
function tuple_tfunc(𝕃::AbstractLattice, argtypes::Vector{Any})
    isempty(argtypes) && return Const(())
    argtypes = anymap(widenslotwrapper, argtypes)
    if isvarargtype(argtypes[end]) && unwrapva(argtypes[end]) === Union{}
        # Drop the Vararg in Tuple{...,Vararg{Union{}}} since it must be length 0.
        # If there is a Vararg num also, it must be a TypeVar, and it must be
        # zero, but that generally shouldn't show up here, since it implies a
        # UnionAll context is missing around this.
        pop!(argtypes)
    end
    all_are_const = true
    for i in 1:length(argtypes)
        if !isa(argtypes[i], Const)
            all_are_const = false
            break
        end
    end
    if all_are_const
        return Const(ntuple(i::Int->argtypes[i].val, length(argtypes)))
    end
    params = Vector{Any}(undef, length(argtypes))
    anyinfo = false
    for i in 1:length(argtypes)
        x = argtypes[i]
        if has_nontrivial_extended_info(𝕃, x)
            anyinfo = true
        else
            if !isvarargtype(x)
                x = widenconst(x)
            end
            argtypes[i] = x
        end
        if isa(x, Const)
            params[i] = typeof(x.val)
        else
            x = isvarargtype(x) ? x : widenconst(x)
            # since there don't exist any values whose runtime type are `Tuple{Type{...}}`,
            # here we should turn such `Type{...}`-parameters to valid parameters, e.g.
            # (::Type{Int},) -> Tuple{DataType} (or PartialStruct for more accuracy)
            # (::Union{Type{Int32},Type{Int64}}) -> Tuple{Type}
            if isType(x)
                anyinfo = true
                xparam = x.parameters[1]
                if hasuniquerep(xparam) || xparam === Bottom
                    params[i] = typeof(xparam)
                else
                    params[i] = Type
                end
            elseif iskindtype(x)
                params[i] = x
            elseif !isvarargtype(x) && hasintersect(x, Type)
                params[i] = Union{x, Type}
            elseif x === Union{}
                return Bottom # argtypes is malformed, but try not to crash
            else
                params[i] = x
            end
        end
    end
    typ = Tuple{params...}
    # replace a singleton type with its equivalent Const object
    issingletontype(typ) && return Const(typ.instance)
    return anyinfo ? PartialStruct(𝕃, typ, argtypes) : typ
end

@nospecs function memorynew_tfunc(𝕃::AbstractLattice, memtype, memlen)
    hasintersect(widenconst(memlen), Int) || return Bottom
    memt = tmeet(𝕃, instanceof_tfunc(memtype, true)[1], GenericMemory)
    memt == Union{} && return memt
    # PartialStruct so that loads of Const `length` get inferred
    return PartialStruct(𝕃, memt, Any[memlen, Ptr{Nothing}])
end
add_tfunc(Core.memorynew, 2, 2, memorynew_tfunc, 10)

@nospecs function memoryrefget_tfunc(𝕃::AbstractLattice, mem, order, boundscheck)
    memoryref_builtin_common_errorcheck(mem, order, boundscheck) || return Bottom
    return memoryref_elemtype(mem)
end
@nospecs function memoryrefset!_tfunc(𝕃::AbstractLattice, mem, item, order, boundscheck)
    hasintersect(widenconst(item), memoryrefget_tfunc(𝕃, mem, order, boundscheck)) || return Bottom
    return item
end
@nospecs function memoryrefswap!_tfunc(𝕃::AbstractLattice, mem, v, order, boundscheck)
    memoryrefset!_tfunc(𝕃, mem, v, order, boundscheck) === Bottom && return Bottom
    return memoryrefget_tfunc(𝕃, mem, order, boundscheck)
end
@nospecs function memoryrefmodify!_tfunc(𝕃::AbstractLattice, mem, op, v, order, boundscheck)
    memoryrefget_tfunc(𝕃, mem, order, boundscheck) === Bottom && return Bottom
    T = _memoryref_elemtype(mem)
    T === Bottom && return Bottom
    PT = Const(Pair)
    return instanceof_tfunc(apply_type_tfunc(𝕃, Any[PT, T, T]), true)[1]
end
@nospecs function memoryrefreplace!_tfunc(𝕃::AbstractLattice, mem, x, v, success_order, failure_order, boundscheck)
    memoryrefset!_tfunc(𝕃, mem, v, success_order, boundscheck) === Bottom && return Bottom
    hasintersect(widenconst(failure_order), Symbol) || return Bottom
    T = _memoryref_elemtype(mem)
    T === Bottom && return Bottom
    PT = Const(ccall(:jl_apply_cmpswap_type, Any, (Any,), T) where T)
    return instanceof_tfunc(apply_type_tfunc(𝕃, Any[PT, T]), true)[1]
end
@nospecs function memoryrefsetonce!_tfunc(𝕃::AbstractLattice, mem, v, success_order, failure_order, boundscheck)
    memoryrefset!_tfunc(𝕃, mem, v, success_order, boundscheck) === Bottom && return Bottom
    hasintersect(widenconst(failure_order), Symbol) || return Bottom
    return Bool
end

add_tfunc(Core.memoryrefget, 3, 3, memoryrefget_tfunc, 20)
add_tfunc(Core.memoryrefset!, 4, 4, memoryrefset!_tfunc, 20)
add_tfunc(Core.memoryrefswap!, 4, 4, memoryrefswap!_tfunc, 20)
add_tfunc(Core.memoryrefmodify!, 5, 5, memoryrefmodify!_tfunc, 20)
add_tfunc(Core.memoryrefreplace!, 6, 6, memoryrefreplace!_tfunc, 20)
add_tfunc(Core.memoryrefsetonce!, 5, 5, memoryrefsetonce!_tfunc, 20)

@nospecs function memoryref_isassigned_tfunc(𝕃::AbstractLattice, mem, order, boundscheck)
    return _memoryref_isassigned_tfunc(𝕃, mem, order, boundscheck)
end
@nospecs function _memoryref_isassigned_tfunc(𝕃::AbstractLattice, mem, order, boundscheck)
    memoryref_builtin_common_errorcheck(mem, order, boundscheck) || return Bottom
    return Bool
end
add_tfunc(memoryref_isassigned, 3, 3, memoryref_isassigned_tfunc, 20)

@nospecs function memoryref_tfunc(𝕃::AbstractLattice, mem)
    a = widenconst(unwrapva(mem))
    if !has_free_typevars(a)
        unw = unwrap_unionall(a)
        if isa(unw, DataType) && unw.name === GenericMemory.body.body.body.name
            A = unw.parameters[1]
            T = unw.parameters[2]
            AS = unw.parameters[3]
            T isa Type || T isa TypeVar || return Bottom
            return rewrap_unionall(GenericMemoryRef{A, T, AS}, a)
        end
    end
    return GenericMemoryRef
end
@nospecs function memoryref_tfunc(𝕃::AbstractLattice, ref, idx)
    if isvarargtype(idx)
        idx = unwrapva(idx)
    end
    return memoryref_tfunc(𝕃, ref, idx, Const(true))
end
@nospecs function memoryref_tfunc(𝕃::AbstractLattice, ref, idx, boundscheck)
    memoryref_builtin_common_errorcheck(ref, Const(:not_atomic), boundscheck) || return Bottom
    hasintersect(widenconst(idx), Int) || return Bottom
    return ref
end
add_tfunc(memoryrefnew, 1, 3, memoryref_tfunc, 1)

@nospecs function memoryrefoffset_tfunc(𝕃::AbstractLattice, mem)
    hasintersect(widenconst(mem), GenericMemoryRef) || return Bottom
    return Int
end
add_tfunc(memoryrefoffset, 1, 1, memoryrefoffset_tfunc, 5)

@nospecs function memoryref_builtin_common_errorcheck(mem, order, boundscheck)
    hasintersect(widenconst(mem), GenericMemoryRef) || return false
    hasintersect(widenconst(order), Symbol) || return false
    hasintersect(widenconst(unwrapva(boundscheck)), Bool) || return false
    return true
end

@nospecs function memoryref_elemtype(mem)
    m = widenconst(mem)
    if !has_free_typevars(m) && m <: GenericMemoryRef
        m0 = m
        if isa(m, UnionAll)
            m = unwrap_unionall(m0)
        end
        if isa(m, DataType)
            T = m.parameters[2]
            valid_as_lattice(T, true) || return Bottom
            return rewrap_unionall(T, m0)
        end
    end
    return Any
end

@nospecs function _memoryref_elemtype(mem)
    m = widenconst(mem)
    if !has_free_typevars(m) && m <: GenericMemoryRef
        m0 = m
        if isa(m, UnionAll)
            m = unwrap_unionall(m0)
        end
        if isa(m, DataType)
            T = m.parameters[2]
            valid_as_lattice(T, true) || return Bottom
            has_free_typevars(T) || return Const(T)
            return rewrap_unionall(Type{T}, m0)
        end
    end
    return Type
end

@nospecs function opaque_closure_tfunc(𝕃::AbstractLattice, arg, lb, ub, source, env::Vector{Any}, mi::MethodInstance)
    argt, argt_exact = instanceof_tfunc(arg)
    lbt, lb_exact = instanceof_tfunc(lb)
    if !lb_exact
        lbt = Union{}
    end

    ubt, ub_exact = instanceof_tfunc(ub)

    t = (argt_exact ? Core.OpaqueClosure{argt, T} : Core.OpaqueClosure{<:argt, T}) where T
    t = lbt == ubt ? t{ubt} : (t{T} where lbt <: T <: ubt)

    (isa(source, Const) && isa(source.val, Method)) || return t

    return PartialOpaque(t, tuple_tfunc(𝕃, env), mi, source.val)
end

# whether getindex for the elements can potentially throw UndefRef
@nospecs function array_type_undefable(arytype)
    arytype = unwrap_unionall(arytype)
    if isa(arytype, Union)
        return array_type_undefable(arytype.a) || array_type_undefable(arytype.b)
    elseif arytype isa DataType
        elmtype = memoryref_elemtype(arytype)
        # TODO: use arraytype layout instead to derive this
        return !((elmtype isa DataType && isbitstype(elmtype)) || (elmtype isa Union && isbitsunion(elmtype)))
    end
    return true
end

@nospecs function memoryset_typecheck(𝕃::AbstractLattice, memtype, elemtype)
    # Check that we can determine the element type
    isa(memtype, DataType) || return false
    elemtype_expected = memoryref_elemtype(memtype)
    elemtype_expected === Union{} && return false
    # Check that the element type is compatible with the element we're assigning
    ⊑ = partialorder(𝕃)
    elemtype ⊑ elemtype_expected || return false
    return true
end

function memoryref_builtin_common_nothrow(argtypes::Vector{Any})
    if length(argtypes) == 1
        memtype = widenconst(argtypes[1])
        return memtype ⊑ GenericMemory
    else
        if length(argtypes) == 2
            boundscheck = Const(true)
        elseif length(argtypes) == 3
            boundscheck = argtypes[3]
        else
            return false
        end
        memtype = widenconst(argtypes[1])
        idx = widenconst(argtypes[2])
        idx ⊑ Int || return false
        boundscheck ⊑ Bool || return false
        memtype ⊑ GenericMemoryRef || return false
        # If we have @inbounds (last argument is false), we're allowed to assume
        # we don't throw bounds errors.
        if isa(boundscheck, Const)
            boundscheck.val::Bool || return true
        end
        # Else we can't really say anything here
        # TODO: In the future we may be able to track the minimum length though inference.
        return false
    end
end

function memoryrefop_builtin_common_nothrow(𝕃::AbstractLattice, argtypes::Vector{Any}, @nospecialize f)
    ismemoryset = f === memoryrefset!
    nargs = ismemoryset ? 4 : 3
    length(argtypes) == nargs || return false
    order = argtypes[2 + ismemoryset]
    boundscheck = argtypes[3 + ismemoryset]
    memtype = widenconst(argtypes[1])
    memoryref_builtin_common_typecheck(𝕃, boundscheck, memtype, order) || return false
    if ismemoryset
        # Additionally check element type compatibility
        memoryset_typecheck(𝕃, memtype, argtypes[2]) || return false
    elseif f === memoryrefget
        # If we could potentially throw undef ref errors, bail out now.
        array_type_undefable(memtype) && return false
    end
    # If we have @inbounds (last argument is false), we're allowed to assume
    # we don't throw bounds errors.
    if isa(boundscheck, Const)
        boundscheck.val::Bool || return true
    end
    # Else we can't really say anything here
    # TODO: In the future we may be able to track the minimum length though inference.
    return false
end

@nospecs function memoryref_builtin_common_typecheck(𝕃::AbstractLattice, boundscheck, memtype, order)
    ⊑ = partialorder(𝕃)
    return boundscheck ⊑ Bool && memtype ⊑ GenericMemoryRef && order ⊑ Symbol
end

function memorynew_nothrow(argtypes::Vector{Any})
    if !(argtypes[1] isa Const && argtypes[2] isa Const)
        return false
    end
    MemT = argtypes[1].val
    if !(isconcretetype(MemT) && MemT <: GenericMemory)
        return false
    end
    len = argtypes[2].val
    if !(len isa Int && 0 <= len < typemax(Int))
        return false
    end
    elsz = datatype_layoutsize(MemT)
    overflows = checked_smul_int(len, elsz)[2]
    return !overflows
end

# Query whether the given builtin is guaranteed not to throw given the `argtypes`.
# `argtypes` can be assumed not to contain varargs.
function _builtin_nothrow(𝕃::AbstractLattice, @nospecialize(f::Builtin), argtypes::Vector{Any},
                          @nospecialize(rt))
    ⊑ = partialorder(𝕃)
    na = length(argtypes)
    if f === Core.memorynew
        return memorynew_nothrow(argtypes)
    elseif f === memoryrefnew
        return memoryref_builtin_common_nothrow(argtypes)
    elseif f === memoryrefoffset
        length(argtypes) == 1 || return false
        memtype = widenconst(argtypes[1])
        return memtype ⊑ GenericMemoryRef
    elseif f === memoryrefset!
        return memoryrefop_builtin_common_nothrow(𝕃, argtypes, f)
    elseif f === memoryrefget
        return memoryrefop_builtin_common_nothrow(𝕃, argtypes, f)
    elseif f === memoryref_isassigned
        return memoryrefop_builtin_common_nothrow(𝕃, argtypes, f)
    elseif f === Core._expr
        length(argtypes) >= 1 || return false
        return argtypes[1] ⊑ Symbol
    elseif f === Core._typevar
        na == 3 || return false
        return typevar_nothrow(𝕃, argtypes[1], argtypes[2], argtypes[3])
    elseif f === invoke
        return false
    elseif f === getfield
        return getfield_nothrow(𝕃, argtypes)
    elseif f === setfield!
        if na == 3
            return setfield!_nothrow(𝕃, argtypes[1], argtypes[2], argtypes[3])
        elseif na == 4
            return setfield!_nothrow(𝕃, argtypes[1], argtypes[2], argtypes[3], argtypes[4])
        end
        return false
    elseif f === fieldtype
        na == 2 || return false
        return fieldtype_nothrow(𝕃, argtypes[1], argtypes[2])
    elseif f === apply_type
        return apply_type_nothrow(𝕃, argtypes, rt)
    elseif f === isa
        na == 2 || return false
        return isa_nothrow(𝕃, nothing, argtypes[2])
    elseif f === (<:)
        na == 2 || return false
        return subtype_nothrow(𝕃, argtypes[1], argtypes[2])
    elseif f === isdefined
        return isdefined_nothrow(𝕃, argtypes)
    elseif f === Core.sizeof
        na == 1 || return false
        return sizeof_nothrow(argtypes[1])
    elseif f === Core.ifelse
        na == 3 || return false
        return ifelse_nothrow(𝕃, argtypes[1], nothing, nothing)
    elseif f === typeassert
        na == 2 || return false
        return typeassert_nothrow(𝕃, argtypes[1], argtypes[2])
    elseif f === Core.get_binding_type
        na == 2 || return false
        return get_binding_type_nothrow(𝕃, argtypes[1], argtypes[2])
    elseif f === donotdelete
        return true
    elseif f === Core.finalizer
        2 <= na <= 4 || return false
        # Core.finalizer does no error checking - that's done in Base.finalizer
        return true
    elseif f === Core.compilerbarrier
        na == 2 || return false
        return compilerbarrier_nothrow(argtypes[1], nothing)
    end
    return false
end

# known to be always effect-free (in particular nothrow)
const _PURE_BUILTINS = Any[
    tuple,
    svec,
    ===,
    typeof,
    nfields,
]

const _CONSISTENT_BUILTINS = Any[
    tuple, # Tuple is immutable, thus tuples of egal arguments are egal
    svec,  # SimpleVector is immutable, thus svecs of egal arguments are egal
    ===,
    typeof,
    nfields,
    fieldtype,
    apply_type,
    isa,
    UnionAll,
    Core.sizeof,
    Core.ifelse,
    (<:),
    typeassert,
    throw,
    Core.throw_methoderror,
    setfield!,
    donotdelete
]

# known to be effect-free (but not necessarily nothrow)
const _EFFECT_FREE_BUILTINS = [
    fieldtype,
    apply_type,
    isa,
    UnionAll,
    getfield,
    Core.memorynew,
    memoryrefnew,
    memoryrefoffset,
    memoryrefget,
    memoryref_isassigned,
    isdefined,
    Core.sizeof,
    Core.ifelse,
    Core._typevar,
    (<:),
    typeassert,
    throw,
    Core.throw_methoderror,
    getglobal,
    compilerbarrier,
]

const _INACCESSIBLEMEM_BUILTINS = Any[
    (<:),
    (===),
    apply_type,
    Core.ifelse,
    Core.sizeof,
    svec,
    fieldtype,
    isa,
    nfields,
    throw,
    Core.throw_methoderror,
    tuple,
    typeassert,
    typeof,
    compilerbarrier,
    Core._typevar,
    donotdelete,
    Core.memorynew,
]

const _ARGMEM_BUILTINS = Any[
    memoryrefnew,
    memoryrefoffset,
    memoryrefget,
    memoryref_isassigned,
    memoryrefset!,
    modifyfield!,
    replacefield!,
    setfield!,
    swapfield!,
]

const _INCONSISTENT_INTRINSICS = Any[
    Intrinsics.pointerref,      # this one is volatile
    Intrinsics.sqrt_llvm_fast,  # this one may differ at runtime (by a few ulps)
    Intrinsics.have_fma,        # this one depends on the runtime environment
    Intrinsics.cglobal,         # cglobal lookup answer changes at runtime
    # ... and list fastmath intrinsics:
    # join(string.("Intrinsics.", sort(filter(endswith("_fast")∘string, names(Core.Intrinsics)))), ",\n")
    Intrinsics.add_float_fast,
    Intrinsics.div_float_fast,
    Intrinsics.eq_float_fast,
    Intrinsics.le_float_fast,
    Intrinsics.lt_float_fast,
    Intrinsics.mul_float_fast,
    Intrinsics.ne_float_fast,
    Intrinsics.neg_float_fast,
    Intrinsics.sqrt_llvm_fast,
    Intrinsics.sub_float_fast,
    # TODO needs to revive #31193 to mark this as inconsistent to be accurate
    # while preserving the currently optimizations for many math operations
    # Intrinsics.muladd_float,    # this is not interprocedurally consistent
]

const _SPECIAL_BUILTINS = Any[
    Core._apply_iterate,
]

# Types compatible with fpext/fptrunc
const CORE_FLOAT_TYPES = Union{Core.BFloat16, Float16, Float32, Float64}

function isdefined_effects(𝕃::AbstractLattice, argtypes::Vector{Any})
    # consistent if the first arg is immutable
    na = length(argtypes)
    2 ≤ na ≤ 3 || return EFFECTS_THROWS
    wobj, sym = argtypes
    wobj = unwrapva(wobj)
    sym = unwrapva(sym)
    consistent = CONSISTENT_IF_INACCESSIBLEMEMONLY
    if is_immutable_argtype(wobj)
        consistent = ALWAYS_TRUE
    elseif isdefined_tfunc(𝕃, wobj, sym) isa Const
        # Some bindings/fields are not allowed to transition from defined to undefined or the reverse, so even
        # if the object is not immutable, we can prove `:consistent`-cy of this:
        consistent = ALWAYS_TRUE
    end
    nothrow = isdefined_nothrow(𝕃, argtypes)
    if hasintersect(widenconst(wobj), Module)
        inaccessiblememonly = ALWAYS_FALSE
    elseif is_mutation_free_argtype(wobj)
        inaccessiblememonly = ALWAYS_TRUE
    else
        inaccessiblememonly = INACCESSIBLEMEM_OR_ARGMEMONLY
    end
    return Effects(EFFECTS_TOTAL; consistent, nothrow, inaccessiblememonly)
end

function getfield_effects(𝕃::AbstractLattice, argtypes::Vector{Any}, @nospecialize(rt))
    length(argtypes) < 2 && return EFFECTS_THROWS
    obj = argtypes[1]
    if isvarargtype(obj)
        return Effects(EFFECTS_TOTAL;
            consistent=CONSISTENT_IF_INACCESSIBLEMEMONLY,
            nothrow=false,
            inaccessiblememonly=ALWAYS_FALSE,
            noub=ALWAYS_FALSE)
    end
    # :consistent if the argtype is immutable
    consistent = (is_immutable_argtype(obj) || is_mutation_free_argtype(obj)) ?
        ALWAYS_TRUE : CONSISTENT_IF_INACCESSIBLEMEMONLY
    noub = ALWAYS_TRUE
    bcheck = getfield_boundscheck(argtypes)
    nothrow = getfield_nothrow(𝕃, argtypes, bcheck)
    if !nothrow
        if bcheck !== :on
            # If we cannot independently prove inboundsness, taint `:noub`.
            # The inbounds-ness assertion requires dynamic reachability,
            # while `:noub` needs to be true for all input values.
            # However, as a special exception, we do allow literal `:boundscheck`.
            # `:noub` will be tainted in any caller using `@inbounds`
            # based on the `:noinbounds` effect.
            # N.B. We do not taint for `--check-bounds=no` here.
            # That is handled in concrete evaluation.
            noub = ALWAYS_FALSE
        end
    end
    if hasintersect(widenconst(obj), Module)
        # Modeled more precisely in abstract_eval_getglobal
        inaccessiblememonly = ALWAYS_FALSE
    elseif is_mutation_free_argtype(obj)
        inaccessiblememonly = ALWAYS_TRUE
    else
        inaccessiblememonly = INACCESSIBLEMEM_OR_ARGMEMONLY
    end
    return Effects(EFFECTS_TOTAL; consistent, nothrow, inaccessiblememonly, noub)
end

"""
    builtin_effects(𝕃::AbstractLattice, f::Builtin, argtypes::Vector{Any}, rt) -> Effects

Compute the effects of a builtin function call. `argtypes` should not include `f` itself.
"""
function builtin_effects(𝕃::AbstractLattice, @nospecialize(f::Builtin), argtypes::Vector{Any}, @nospecialize(rt))
    if isa(f, IntrinsicFunction)
        return intrinsic_effects(f, argtypes)
    end

    @assert !contains_is(_SPECIAL_BUILTINS, f)

    if f === getfield
        return getfield_effects(𝕃, argtypes, rt)
    end

    # if this builtin call deterministically throws,
    # don't bother to taint the other effects other than :nothrow:
    # note this is safe only if we accounted for :noub already
    rt === Bottom && return EFFECTS_THROWS

    if f === isdefined
        return isdefined_effects(𝕃, argtypes)
    elseif f === getglobal
        2 ≤ length(argtypes) ≤ 3 || return EFFECTS_THROWS
        # Modeled more precisely in abstract_eval_getglobal
        return generic_getglobal_effects
    elseif f === Core.get_binding_type
        length(argtypes) == 2 || return EFFECTS_THROWS
        # Modeled more precisely in abstract_eval_get_binding_type
        return Effects(EFFECTS_TOTAL; nothrow=get_binding_type_nothrow(𝕃, argtypes[1], argtypes[2]))
    elseif f === compilerbarrier
        length(argtypes) == 2 || return Effects(EFFECTS_THROWS; consistent=ALWAYS_FALSE)
        setting = argtypes[1]
        return Effects(EFFECTS_TOTAL;
            consistent = (isa(setting, Const) && setting.val === :conditional) ? ALWAYS_TRUE : ALWAYS_FALSE,
            nothrow = compilerbarrier_nothrow(setting, nothing))
    elseif f === Core.current_scope
        nothrow = true
        if length(argtypes) != 0
            if length(argtypes) != 1 || !isvarargtype(argtypes[1])
                return EFFECTS_THROWS
            end
            nothrow = false
        end
        return Effects(EFFECTS_TOTAL;
            consistent = ALWAYS_FALSE,
            notaskstate = false,
            nothrow)
    else
        if contains_is(_CONSISTENT_BUILTINS, f)
            consistent = ALWAYS_TRUE
        elseif f === memoryrefnew || f === memoryrefoffset
            consistent = ALWAYS_TRUE
        elseif f === memoryrefget || f === memoryrefset! || f === memoryref_isassigned
            consistent = CONSISTENT_IF_INACCESSIBLEMEMONLY
        elseif f === Core._typevar || f === Core.memorynew
            consistent = CONSISTENT_IF_NOTRETURNED
        else
            consistent = ALWAYS_FALSE
        end
        if f === setfield! || f === memoryrefset!
            effect_free = EFFECT_FREE_IF_INACCESSIBLEMEMONLY
        elseif contains_is(_EFFECT_FREE_BUILTINS, f) || contains_is(_PURE_BUILTINS, f)
            effect_free = ALWAYS_TRUE
        else
            effect_free = ALWAYS_FALSE
        end
        nothrow = builtin_nothrow(𝕃, f, argtypes, rt)
        if contains_is(_INACCESSIBLEMEM_BUILTINS, f)
            inaccessiblememonly = ALWAYS_TRUE
        elseif contains_is(_ARGMEM_BUILTINS, f)
            inaccessiblememonly = INACCESSIBLEMEM_OR_ARGMEMONLY
        else
            inaccessiblememonly = ALWAYS_FALSE
        end
        if f === memoryrefnew || f === memoryrefget || f === memoryrefset! || f === memoryref_isassigned
            noub = memoryop_noub(f, argtypes) ? ALWAYS_TRUE : ALWAYS_FALSE
        else
            noub = ALWAYS_TRUE
        end
        return Effects(EFFECTS_TOTAL; consistent, effect_free, nothrow, inaccessiblememonly, noub)
    end
end

function memoryop_noub(@nospecialize(f), argtypes::Vector{Any})
    nargs = length(argtypes)
    nargs == 0 && return true # must throw and noub
    lastargtype = argtypes[end]
    isva = isvarargtype(lastargtype)
    if f === memoryrefnew
        if nargs == 1 && !isva
            return true
        elseif nargs == 2 && !isva
            return true
        end
        expected_nargs = 3
    elseif f === memoryrefget || f === memoryref_isassigned
        expected_nargs = 3
    else
        @assert f === memoryrefset! "unexpected memoryop is given"
        expected_nargs = 4
    end
    if nargs == expected_nargs && !isva
        boundscheck = widenconditional(lastargtype)
        hasintersect(widenconst(boundscheck), Bool) || return true # must throw and noub
        boundscheck isa Const && boundscheck.val === true && return true
    elseif nargs > expected_nargs + 1
        return true # must throw and noub
    elseif !isva
        return true # must throw and noub
    end
    return false
end

function current_scope_tfunc(interp::AbstractInterpreter, sv::InferenceState)
    pc = sv.currpc
    handler_info = sv.handler_info
    while true
        pchandler = gethandler(sv, pc)
        if pchandler === nothing
            # No local scope available - inherited from the outside
            return Any
        end
        # Remember that we looked at this handler, so we get re-scheduled
        # if the scope information changes
        isdefined(pchandler, :scope_uses) || (pchandler.scope_uses = Int[])
        pcbb = block_for_inst(sv.cfg, pc)
        if findfirst(==(pcbb), pchandler.scope_uses) === nothing
            push!(pchandler.scope_uses, pcbb)
        end
        scope = pchandler.scopet
        if scope !== nothing
            # Found the scope - forward it
            return scope
        end
        pc = pchandler.enter_idx
    end
end
current_scope_tfunc(interp::AbstractInterpreter, sv) = Any

hasvarargtype(argtypes::Vector{Any}) = !isempty(argtypes) && isvarargtype(argtypes[end])

"""
    builtin_nothrow(𝕃::AbstractLattice, f::Builtin, argtypes::Vector{Any}, rt) -> Bool

Compute throw-ness of a builtin function call. `argtypes` should not include `f` itself.
"""
function builtin_nothrow(𝕃::AbstractLattice, @nospecialize(f), argtypes::Vector{Any}, @nospecialize(rt))
    rt === Bottom && return false
    if f === tuple || f === svec
        return true
    elseif hasvarargtype(argtypes)
        return false
    elseif contains_is(_PURE_BUILTINS, f)
        return true
    end
    return _builtin_nothrow(𝕃, f, argtypes, rt)
end

function builtin_tfunction(interp::AbstractInterpreter, @nospecialize(f), argtypes::Vector{Any},
                           sv::Union{AbsIntState, Nothing})
    𝕃ᵢ = typeinf_lattice(interp)
    if isa(f, IntrinsicFunction)
        if is_pure_intrinsic_infer(f) && all(@nospecialize(a) -> isa(a, Const), argtypes)
            argvals = anymap(@nospecialize(a) -> (a::Const).val, argtypes)
            try
                # unroll a few cases which have specialized codegen
                if length(argvals) == 1
                    return Const(f(argvals[1]))
                elseif length(argvals) == 2
                    return Const(f(argvals[1], argvals[2]))
                elseif length(argvals) == 3
                    return Const(f(argvals[1], argvals[2], argvals[3]))
                end
                return Const(f(argvals...))
            catch ex # expected ErrorException, TypeError, ConcurrencyViolationError, DivideError etc.
                ex isa InterruptException && rethrow()
                return Bottom
            end
        end
        iidx = Int(reinterpret(Int32, f)) + 1
        if iidx < 0 || iidx > length(T_IFUNC)
            # unknown intrinsic
            return Any
        end
        tf = T_IFUNC[iidx]
    else
        if f === tuple
            return tuple_tfunc(𝕃ᵢ, argtypes)
        elseif f === Core.current_scope
            if length(argtypes) != 0
                if length(argtypes) != 1 || !isvarargtype(argtypes[1])
                    return Bottom
                end
            end
            return current_scope_tfunc(interp, sv)
        elseif f === Core.apply_type
            return apply_type_tfunc(𝕃ᵢ, argtypes; max_union_splitting=InferenceParams(interp).max_union_splitting)
        end
        fidx = find_tfunc(f)
        if fidx === nothing
            # unknown/unhandled builtin function
            return Any
        end
        tf = T_FFUNC_VAL[fidx]
    end
    if hasvarargtype(argtypes)
        if length(argtypes) - 1 > tf[2]
            # definitely too many arguments
            return Bottom
        end
        if length(argtypes) - 1 == tf[2]
            argtypes = argtypes[1:end-1]
        else
            vatype = argtypes[end]::TypeofVararg
            argtypes = argtypes[1:end-1]
            while length(argtypes) < tf[1]
                push!(argtypes, unwrapva(vatype))
            end
            if length(argtypes) < tf[2]
                push!(argtypes, unconstrain_vararg_length(vatype))
            end
        end
    elseif !(tf[1] <= length(argtypes) <= tf[2])
        # wrong # of args
        return Bottom
    end
    return tf[3](𝕃ᵢ, argtypes...)
end

# Query whether the given intrinsic is nothrow

_iszero(@nospecialize x) = x === Intrinsics.xor_int(x, x)
_isneg1(@nospecialize x) = _iszero(Intrinsics.not_int(x))
_istypemin(@nospecialize x) = !_iszero(x) && Intrinsics.neg_int(x) === x

function builtin_exct(𝕃::AbstractLattice, @nospecialize(f::Builtin), argtypes::Vector{Any}, @nospecialize(rt))
    if isa(f, IntrinsicFunction)
        return intrinsic_exct(𝕃, f, argtypes)
    end
    return Any
end

function div_nothrow(f::IntrinsicFunction, @nospecialize(arg1), @nospecialize(arg2))
    isa(arg2, Const) || return false
    den_val = arg2.val
    _iszero(den_val) && return false
    f !== Intrinsics.checked_sdiv_int && return true
    # Nothrow as long as we additionally don't do typemin(T)/-1
    return !_isneg1(den_val) || (isa(arg1, Const) && !_istypemin(arg1.val))
end

function known_is_valid_intrinsic_elptr(𝕃::AbstractLattice, @nospecialize(ptr))
    ptrT = typeof_tfunc(𝕃, ptr)
    isa(ptrT, Const) || return false
    return is_valid_intrinsic_elptr(ptrT.val)
end

function intrinsic_exct(𝕃::AbstractLattice, f::IntrinsicFunction, argtypes::Vector{Any})
    if hasvarargtype(argtypes)
        return Any
    end

    # First check that we have the correct number of arguments
    iidx = Int(reinterpret(Int32, f)) + 1
    if iidx < 1 || iidx > length(T_IFUNC)
        # invalid intrinsic (system will crash)
        return Any
    end
    tf = T_IFUNC[iidx]
    if !(tf[1] <= length(argtypes) <= tf[2])
        # wrong # of args
        return ArgumentError
    end

    # TODO: We could do better for cglobal
    f === Intrinsics.cglobal && return Any
    # TODO: We can't know for sure, but the user should have a way to assert
    # that it won't
    f === Intrinsics.llvmcall && return Any

    if (f === Intrinsics.checked_udiv_int || f === Intrinsics.checked_urem_int ||
        f === Intrinsics.checked_srem_int || f === Intrinsics.checked_sdiv_int)
        # Nothrow as long as the second argument is guaranteed not to be zero
        arg1 = argtypes[1]
        arg2 = argtypes[2]
        warg1 = widenconst(arg1)
        warg2 = widenconst(arg2)
        if !(warg1 === warg2 && isprimitivetype(warg1))
            return Union{TypeError, DivideError}
        end
        if !div_nothrow(f, arg1, arg2)
            return DivideError
        end
        return Union{}
    end

    if f === Intrinsics.pointerref
        # Nothrow as long as the types are ok. N.B.: dereferencability is not
        # modeled here, but can cause errors (e.g. ReadOnlyMemoryError). We follow LLVM here
        # in that it is legal to remove unused non-volatile loads.
        if !(argtypes[1] ⊑ Ptr && argtypes[2] ⊑ Int && argtypes[3] ⊑ Int)
            return Union{TypeError, ErrorException}
        end
        if !known_is_valid_intrinsic_elptr(𝕃, argtypes[1])
            return ErrorException
        end
        return Union{}
    end

    if f === Intrinsics.pointerset
        eT = pointer_eltype(argtypes[1])
        if !known_is_valid_intrinsic_elptr(𝕃, argtypes[1])
            return Union{TypeError, ErrorException}
        end
        if !(argtypes[2] ⊑ eT && argtypes[3] ⊑ Int && argtypes[4] ⊑ Int)
            return TypeError
        end
        return Union{}
    end

    if f === Intrinsics.bitcast
        ty, isexact, isconcrete = instanceof_tfunc(argtypes[1], true)
        xty = widenconst(argtypes[2])
        if !isconcrete
            return Union{ErrorException, TypeError}
        end
        if !(isprimitivetype(ty) && isprimitivetype(xty) && Core.sizeof(ty) === Core.sizeof(xty))
            return ErrorException
        end
        return Union{}
    end

    if f in (Intrinsics.sext_int, Intrinsics.zext_int, Intrinsics.trunc_int,
             Intrinsics.fptoui, Intrinsics.fptosi, Intrinsics.uitofp,
             Intrinsics.sitofp, Intrinsics.fptrunc, Intrinsics.fpext)
        # If !isconcrete, `ty` may be Union{} at runtime even if we have
        # isprimitivetype(ty).
        ty, isexact, isconcrete = instanceof_tfunc(argtypes[1], true)
        if !isconcrete
            return Union{ErrorException, TypeError}
        end
        xty = widenconst(argtypes[2])
        if !(isprimitivetype(ty) && isprimitivetype(xty))
            return ErrorException
        end

        # fpext and fptrunc have further restrictions on the allowed types.
        if f === Intrinsics.fpext &&
            !(ty <: CORE_FLOAT_TYPES && xty <: CORE_FLOAT_TYPES && Core.sizeof(ty) > Core.sizeof(xty))
            return ErrorException
        end
        if f === Intrinsics.fptrunc &&
            !(ty <: CORE_FLOAT_TYPES && xty <: CORE_FLOAT_TYPES && Core.sizeof(ty) < Core.sizeof(xty))
            return ErrorException
        end

        return Union{}
    end

    if f === Intrinsics.have_fma
        ty, isexact, isconcrete = instanceof_tfunc(argtypes[1], true)
        if !(isconcrete && isprimitivetype(ty))
            return TypeError
        end
        return Union{}
    end

    # The remaining intrinsics are math/bits/comparison intrinsics. They work on all
    # primitive types of the same type.
    isshift = f === shl_int || f === lshr_int || f === ashr_int
    argtype1 = widenconst(argtypes[1])
    isprimitivetype(argtype1) || return ErrorException
    for i = 2:length(argtypes)
        argtype = widenconst(argtypes[i])
        if isshift ? !isprimitivetype(argtype) : argtype !== argtype1
            return ErrorException
        end
    end
    return Union{}
end

function intrinsic_nothrow(f::IntrinsicFunction, argtypes::Vector{Any})
    return intrinsic_exct(SimpleInferenceLattice.instance, f, argtypes) === Union{}
end

# whether `f` is pure for inference
function is_pure_intrinsic_infer(f::IntrinsicFunction)
    return !(f === Intrinsics.pointerref || # this one is volatile
             f === Intrinsics.pointerset || # this one is never effect-free
             f === Intrinsics.llvmcall ||   # this one is never effect-free
             f === Intrinsics.sqrt_llvm_fast ||  # this one may differ at runtime (by a few ulps)
             f === Intrinsics.have_fma ||  # this one depends on the runtime environment
             f === Intrinsics.cglobal)  # cglobal lookup answer changes at runtime
end

# whether `f` is effect free if nothrow
function intrinsic_effect_free_if_nothrow(@nospecialize f)
    return f === Intrinsics.pointerref ||
           f === Intrinsics.have_fma ||
           is_pure_intrinsic_infer(f)
end

function intrinsic_effects(f::IntrinsicFunction, argtypes::Vector{Any})
    if f === Intrinsics.llvmcall
        # llvmcall can do arbitrary things
        return Effects()
    end
    if contains_is(_INCONSISTENT_INTRINSICS, f)
        consistent = ALWAYS_FALSE
    else
        consistent = ALWAYS_TRUE
    end
    effect_free = !(f === Intrinsics.pointerset) ? ALWAYS_TRUE : ALWAYS_FALSE
    nothrow = intrinsic_nothrow(f, argtypes)
    inaccessiblememonly = ALWAYS_TRUE
    return Effects(EFFECTS_TOTAL; consistent, effect_free, nothrow, inaccessiblememonly)
end

# TODO: this function is a very buggy and poor model of the return_type function
# since abstract_call_gf_by_type is a very inaccurate model of _method and of typeinf_type,
# while this assumes that it is an absolutely precise and accurate and exact model of both
function return_type_tfunc(interp::AbstractInterpreter, argtypes::Vector{Any}, si::StmtInfo, sv::AbsIntState)
    UNKNOWN = CallMeta(Type, Any, Effects(EFFECTS_THROWS; nortcall=false), NoCallInfo())
    if !(2 <= length(argtypes) <= 3)
        return Future(UNKNOWN)
    end

    tt = widenslotwrapper(argtypes[end])
    if !isa(tt, Const) && !(isType(tt) && !has_free_typevars(tt))
        return Future(UNKNOWN)
    end

    af_argtype = isa(tt, Const) ? tt.val : (tt::DataType).parameters[1]
    if !isa(af_argtype, DataType) || !(af_argtype <: Tuple)
        return Future(UNKNOWN)
    end

    if length(argtypes) == 3
        aft = widenslotwrapper(argtypes[2])
        argtypes_vec = Any[aft, af_argtype.parameters...]
    else
        argtypes_vec = Any[af_argtype.parameters...]
        isempty(argtypes_vec) && push!(argtypes_vec, Union{})
        aft = argtypes_vec[1]
    end
    if !(isa(aft, Const) || (isType(aft) && !has_free_typevars(aft)) ||
            (isconcretetype(aft) && !(aft <: Builtin) && !iskindtype(aft)))
        return Future(UNKNOWN)
    end

    # effects are not an issue if we know this statement will get removed, but if it does not get removed,
    # then this could be recursively re-entering inference (via concrete-eval), which will not terminate
    RT_CALL_EFFECTS = Effects(EFFECTS_TOTAL; nortcall=false)

    if contains_is(argtypes_vec, Union{})
        return Future(CallMeta(Const(Union{}), Union{}, RT_CALL_EFFECTS, NoCallInfo()))
    end

    # Run the abstract_call without restricting abstract call
    # sites. Otherwise, our behavior model of abstract_call
    # below will be wrong.
    if isa(sv, InferenceState)
        old_restrict = sv.restrict_abstract_call_sites
        sv.restrict_abstract_call_sites = false
    end
    call = abstract_call(interp, ArgInfo(nothing, argtypes_vec), si, sv, #=max_methods=#-1)
    tt = Core.Box(tt)
    return Future{CallMeta}(call, interp, sv) do call, interp, sv
        if isa(sv, InferenceState)
            sv.restrict_abstract_call_sites = old_restrict
        end
        info = MethodResultPure(ReturnTypeCallInfo(call.info))
        rt = widenslotwrapper(call.rt)
        if isa(rt, Const)
            # output was computed to be constant
            return CallMeta(Const(typeof(rt.val)), Union{}, RT_CALL_EFFECTS, info)
        end
        rt = widenconst(rt)
        if rt === Bottom || (isconcretetype(rt) && !iskindtype(rt))
            # output cannot be improved so it is known for certain
            return CallMeta(Const(rt), Union{}, RT_CALL_EFFECTS, info)
        elseif isa(sv, InferenceState) && !isempty(sv.pclimitations)
            # conservatively express uncertainty of this result
            # in two ways: both as being a subtype of this, and
            # because of LimitedAccuracy causes
            return CallMeta(Type{<:rt}, Union{}, RT_CALL_EFFECTS, info)
        elseif isa(tt.contents, Const) || isconstType(tt.contents)
            # input arguments were known for certain
            # XXX: this doesn't imply we know anything about rt
            return CallMeta(Const(rt), Union{}, RT_CALL_EFFECTS, info)
        elseif isType(rt)
            return CallMeta(Type{rt}, Union{}, RT_CALL_EFFECTS, info)
        else
            return CallMeta(Type{<:rt}, Union{}, RT_CALL_EFFECTS, info)
        end
    end
end

# a simplified model of abstract_call_gf_by_type for applicable
function abstract_applicable(interp::AbstractInterpreter, argtypes::Vector{Any},
                             sv::AbsIntState, max_methods::Int)
    length(argtypes) < 2 && return Future(CallMeta(Bottom, ArgumentError, EFFECTS_THROWS, NoCallInfo()))
    isvarargtype(argtypes[2]) && return Future(CallMeta(Bool, ArgumentError, EFFECTS_THROWS, NoCallInfo()))
    argtypes = argtypes[2:end]
    atype = argtypes_to_type(argtypes)
    if atype === Union{}
        rt = Union{} # accidentally unreachable code
    else
        matches = find_method_matches(interp, argtypes, atype; max_methods)
        info = NoCallInfo()
        if isa(matches, FailedMethodMatch)
            rt = Bool # too many matches to analyze
        else
            (; valid_worlds, applicable) = matches
            update_valid_age!(sv, valid_worlds)
            napplicable = length(applicable)
            if napplicable == 0
                rt = Const(false) # never any matches
            elseif !fully_covering(matches) || any_ambig(matches)
                # Account for the fact that we may encounter a MethodError with a non-covered or ambiguous signature.
                rt = Bool
            else
                rt = Const(true) # has applicable matches
            end
            if rt !== Bool
                info = VirtualMethodMatchInfo(matches.info)
            end
        end
    end
    return Future(CallMeta(rt, Union{}, EFFECTS_TOTAL, info))
end
add_tfunc(applicable, 1, INT_INF, @nospecs((𝕃::AbstractLattice, f, args...)->Bool), 40)

# a simplified model of abstract_invoke for Core._hasmethod
function _hasmethod_tfunc(interp::AbstractInterpreter, argtypes::Vector{Any}, sv::AbsIntState)
    if length(argtypes) == 3 && !isvarargtype(argtypes[3])
        ft′ = argtype_by_index(argtypes, 2)
        ft = widenconst(ft′)
        ft === Bottom && return CallMeta(Bool, Any, EFFECTS_THROWS, NoCallInfo())
        typeidx = 3
    elseif length(argtypes) == 2 && !isvarargtype(argtypes[2])
        typeidx = 2
    else
        return CallMeta(Any, Any, Effects(), NoCallInfo())
    end
    (types, isexact, isconcrete, istype) = instanceof_tfunc(argtype_by_index(argtypes, typeidx), false)
    isexact || return CallMeta(Bool, Any, Effects(), NoCallInfo())
    unwrapped = unwrap_unionall(types)
    if types === Bottom || !(unwrapped isa DataType) || unwrapped.name !== Tuple.name
        return CallMeta(Bool, Any, EFFECTS_THROWS, NoCallInfo())
    end
    if typeidx == 3
        isdispatchelem(ft) || return CallMeta(Bool, Any, Effects(), NoCallInfo()) # check that we might not have a subtype of `ft` at runtime, before doing supertype lookup below
        types = rewrap_unionall(Tuple{ft, unwrapped.parameters...}, types)::Type
    end
    mt = ccall(:jl_method_table_for, Any, (Any,), types)
    if !isa(mt, MethodTable)
        return CallMeta(Bool, Any, EFFECTS_THROWS, NoCallInfo())
    end
    match, valid_worlds = findsup(types, method_table(interp))
    update_valid_age!(sv, valid_worlds)
    if match === nothing
        rt = Const(false)
        vresults = MethodLookupResult(Any[], valid_worlds, true)
        vinfo = MethodMatchInfo(vresults, mt, types, false) # XXX: this should actually be an info with invoke-type edge
    else
        rt = Const(true)
        vinfo = InvokeCallInfo(nothing, match, nothing, types)
    end
    info = VirtualMethodMatchInfo(vinfo)
    return CallMeta(rt, Union{}, EFFECTS_TOTAL, info)
end

# N.B.: typename maps type equivalence classes to a single value
function typename_static(@nospecialize(t))
    t isa Const && return _typename(t.val)
    t isa Conditional && return Bool.name
    t = unwrap_unionall(widenconst(t))
    return isType(t) ? _typename(t.parameters[1]) : Core.TypeName
end

function global_order_exct(@nospecialize(o), loading::Bool, storing::Bool)
    if !(o isa Const)
        if o === Symbol
            return ConcurrencyViolationError
        elseif !hasintersect(o, Symbol)
            return TypeError
        else
            return Union{ConcurrencyViolationError, TypeError}
        end
    end
    sym = o.val
    if sym isa Symbol
        order = get_atomic_order(sym, loading, storing)
        if order !== MEMORY_ORDER_INVALID && order !== MEMORY_ORDER_NOTATOMIC
            return Union{}
        else
            return ConcurrencyViolationError
        end
    else
        return TypeError
    end
end

@nospecs function get_binding_type_nothrow(𝕃::AbstractLattice, M, s)
    ⊑ = partialorder(𝕃)
    return M ⊑ Module && s ⊑ Symbol
end

add_tfunc(getglobal, 2, 3, @nospecs((𝕃::AbstractLattice, args...)->Any), 1)
add_tfunc(setglobal!, 3, 4, @nospecs((𝕃::AbstractLattice, args...)->Any), 3)
add_tfunc(swapglobal!, 3, 4, @nospecs((𝕃::AbstractLattice, args...)->Any), 3)
add_tfunc(modifyglobal!, 4, 5, @nospecs((𝕃::AbstractLattice, args...)->Any), 3)
add_tfunc(replaceglobal!, 4, 6, @nospecs((𝕃::AbstractLattice, args...)->Any), 3)
add_tfunc(setglobalonce!, 3, 5, @nospecs((𝕃::AbstractLattice, args...)->Bool), 3)
add_tfunc(Core.get_binding_type, 2, 2, @nospecs((𝕃::AbstractLattice, args...)->Type), 0)

# foreigncall
# ===========

# N.B. the `abstract_eval` callback below allows us to use these queries
# both during abstract interpret and optimization

const FOREIGNCALL_ARG_START = 6

function foreigncall_effects(@nospecialize(abstract_eval), e::Expr)
    # `:foreigncall` can potentially perform all sorts of operations, including calling
    # overlay methods, but the `:foreigncall` itself is not dispatched, and there is no
    # concern that the method calls that potentially occur within the `:foreigncall` will
    # be executed using the wrong method table due to concrete evaluation, so using
    # `EFFECTS_UNKNOWN` here and not tainting with `:nonoverlayed` is fine
    return EFFECTS_UNKNOWN
end

function new_genericmemory_nothrow(@nospecialize(abstract_eval), args::Vector{Any})
    length(args) ≥ 1+FOREIGNCALL_ARG_START || return false
    mtype = instanceof_tfunc(abstract_eval(args[FOREIGNCALL_ARG_START]))[1]
    isa(mtype, DataType) || return false
    isdefined(mtype, :instance) || return false
    elsz = Int(datatype_layoutsize(mtype))
    arrayelem = datatype_arrayelem(mtype)
    dim = abstract_eval(args[1+FOREIGNCALL_ARG_START])
    isa(dim, Const) || return false
    dimval = dim.val
    isa(dimval, Int) || return false
    0 < dimval < typemax(Int) || return false
    tot, ovflw = Intrinsics.checked_smul_int(dimval, elsz)
    ovflw && return false
    isboxed = 1; isunion = 2
    tot, ovflw = Intrinsics.checked_sadd_int(tot, arrayelem == isunion ? 1 + dimval : 1)
    ovflw && return false
    return true
end
