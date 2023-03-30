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
    if !isempty(names)
        lin = first(body.args)::LineNumberNode
        nospec = Expr(:macrocall, Symbol("@nospecialize"), lin, names...)
        insert!(body.args, 2, nospec)
    end
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

# the inverse of typeof_tfunc
# returns (type, isexact, isconcrete, istype)
# if isexact is false, the actual runtime type may (will) be a subtype of t
# if isconcrete is true, the actual runtime type is definitely concrete (unreachable if not valid as a typeof)
# if istype is true, the actual runtime value will definitely be a type (e.g. this is false for Union{Type{Int}, Int})
function instanceof_tfunc(@nospecialize(t))
    if isa(t, Const)
        if isa(t.val, Type) && valid_as_lattice(t.val)
            return t.val, true, isconcretetype(t.val), true
        end
        return Bottom, true, false, false # runtime throws on non-Type
    end
    t = widenconst(t)
    if t === Bottom
        return Bottom, true, true, false # runtime unreachable
    elseif t === typeof(Bottom) || !hasintersect(t, Type)
        return Bottom, true, false, false # literal Bottom or non-Type
    elseif isType(t)
        tp = t.parameters[1]
        valid_as_lattice(tp) || return Bottom, true, false, false # runtime unreachable / throws on non-Type
        return tp, !has_free_typevars(tp), isconcretetype(tp), true
    elseif isa(t, UnionAll)
        t′ = unwrap_unionall(t)
        t′′, isexact, isconcrete, istype = instanceof_tfunc(t′)
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
        ta, isexact_a, isconcrete_a, istype_a = instanceof_tfunc(t.a)
        tb, isexact_b, isconcrete_b, istype_b = instanceof_tfunc(t.b)
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
@nospecs bitcast_tfunc(::JLTypeLattice, t, x) = instanceof_tfunc(t)[1]
@nospecs conversion_tfunc(𝕃::AbstractLattice, t, x) = conversion_tfunc(widenlattice(𝕃), t, x)
@nospecs conversion_tfunc(::JLTypeLattice, t, x) = instanceof_tfunc(t)[1]

add_tfunc(bitcast, 2, 2, bitcast_tfunc, 1)
add_tfunc(sext_int, 2, 2, conversion_tfunc, 1)
add_tfunc(zext_int, 2, 2, conversion_tfunc, 1)
add_tfunc(trunc_int, 2, 2, conversion_tfunc, 1)
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

add_tfunc(neg_int, 1, 1, math_tfunc, 1)
add_tfunc(add_int, 2, 2, math_tfunc, 1)
add_tfunc(sub_int, 2, 2, math_tfunc, 1)
add_tfunc(mul_int, 2, 2, math_tfunc, 4)
add_tfunc(sdiv_int, 2, 2, math_tfunc, 30)
add_tfunc(udiv_int, 2, 2, math_tfunc, 30)
add_tfunc(srem_int, 2, 2, math_tfunc, 30)
add_tfunc(urem_int, 2, 2, math_tfunc, 30)
add_tfunc(add_ptr, 2, 2, math_tfunc, 1)
add_tfunc(sub_ptr, 2, 2, math_tfunc, 1)
add_tfunc(neg_float, 1, 1, math_tfunc, 1)
add_tfunc(add_float, 2, 2, math_tfunc, 1)
add_tfunc(sub_float, 2, 2, math_tfunc, 1)
add_tfunc(mul_float, 2, 2, math_tfunc, 4)
add_tfunc(div_float, 2, 2, math_tfunc, 20)
add_tfunc(fma_float, 3, 3, math_tfunc, 5)
add_tfunc(muladd_float, 3, 3, math_tfunc, 5)

# fast arithmetic
add_tfunc(neg_float_fast, 1, 1, math_tfunc, 1)
add_tfunc(add_float_fast, 2, 2, math_tfunc, 1)
add_tfunc(sub_float_fast, 2, 2, math_tfunc, 1)
add_tfunc(mul_float_fast, 2, 2, math_tfunc, 2)
add_tfunc(div_float_fast, 2, 2, math_tfunc, 10)

# bitwise operators
# -----------------

@nospecs shift_tfunc(𝕃::AbstractLattice, x, y) = shift_tfunc(widenlattice(𝕃), x, y)
@nospecs shift_tfunc(::JLTypeLattice, x, y) = widenconst(x)

add_tfunc(and_int, 2, 2, math_tfunc, 1)
add_tfunc(or_int, 2, 2, math_tfunc, 1)
add_tfunc(xor_int, 2, 2, math_tfunc, 1)
add_tfunc(not_int, 1, 1, math_tfunc, 0) # usually used as not_int(::Bool) to negate a condition
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

add_tfunc(checked_sadd_int, 2, 2, chk_tfunc, 10)
add_tfunc(checked_uadd_int, 2, 2, chk_tfunc, 10)
add_tfunc(checked_ssub_int, 2, 2, chk_tfunc, 10)
add_tfunc(checked_usub_int, 2, 2, chk_tfunc, 10)
add_tfunc(checked_smul_int, 2, 2, chk_tfunc, 10)
add_tfunc(checked_umul_int, 2, 2, chk_tfunc, 10)

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
add_tfunc(Core.Intrinsics.arraylen, 1, 1, @nospecs((𝕃::AbstractLattice, x)->Int), 4)

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
    ⊑ = Core.Compiler.:⊑(𝕃)
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

@nospecs function isdefined_nothrow(𝕃::AbstractLattice, x, name)
    ⊑ = Core.Compiler.:⊑(𝕃)
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
    if isa(arg1, Const)
        arg1t = typeof(arg1.val)
    else
        arg1t = widenconst(arg1)
    end
    if isType(arg1t)
        return Bool
    end
    a1 = unwrap_unionall(arg1t)
    if isa(a1, DataType) && !isabstracttype(a1)
        if a1 === Module
            hasintersect(widenconst(sym), Symbol) || return Bottom
            if isa(sym, Const) && isa(sym.val, Symbol) && isa(arg1, Const) &&
               isdefined(arg1.val::Module, sym.val::Symbol)
                return Const(true)
            end
        elseif isa(sym, Const)
            val = sym.val
            if isa(val, Symbol)
                idx = fieldindex(a1, val, false)::Int
            elseif isa(val, Int)
                idx = val
            else
                return Bottom
            end
            if 1 <= idx <= datatype_min_ninitialized(a1)
                return Const(true)
            elseif a1.name === _NAMEDTUPLE_NAME
                if isconcretetype(a1)
                    return Const(false)
                else
                    ns = a1.parameters[1]
                    if isa(ns, Tuple)
                        return Const(1 <= idx <= length(ns))
                    end
                end
            elseif idx <= 0 || (!isvatuple(a1) && idx > fieldcount(a1))
                return Const(false)
            elseif isa(arg1, Const)
                arg1v = (arg1::Const).val
                if !ismutable(arg1v) || isdefined(arg1v, idx) || isconst(typeof(arg1v), idx)
                    return Const(isdefined(arg1v, idx))
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
    t, exact, isconcrete = instanceof_tfunc(x)
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
    # Constant Vector does not have constant size
    isa(x, Vector) && return Int
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
    t, exact = instanceof_tfunc(x)
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
        na = nfields_tfunc(𝕃, x.a)
        na === Int && return Int
        return tmerge(na, nfields_tfunc(𝕃, x.b))
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
        tv = TypeVar(nval, lb, ub)
        return PartialTypeVar(tv, lb_certain, ub_certain)
    end
    return TypeVar
end
@nospecs function typebound_nothrow(b)
    b = widenconst(b)
    (b ⊑ TypeVar) && return true
    if isType(b)
        return true
    end
    return false
end
@nospecs function typevar_nothrow(𝕃::AbstractLattice, n, lb, ub)
    ⊑ = Core.Compiler.:⊑(𝕃)
    n ⊑ Symbol || return false
    typebound_nothrow(lb) || return false
    typebound_nothrow(ub) || return false
    return true
end
add_tfunc(Core._typevar, 3, 3, typevar_tfunc, 100)

@nospecs function arraysize_tfunc(𝕃::AbstractLattice, ary, dim)
    hasintersect(widenconst(ary), Array) || return Bottom
    hasintersect(widenconst(dim), Int) || return Bottom
    return Int
end
add_tfunc(arraysize, 2, 2, arraysize_tfunc, 4)

@nospecs function arraysize_nothrow(ary, dim)
    ary ⊑ Array || return false
    if isa(dim, Const)
        dimval = dim.val
        return isa(dimval, Int) && dimval > 0
    end
    return false
end

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
            valid_as_lattice(T) || return Bottom
            return rewrap_unionall(T, a)
        end
    end
    return Any
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
            valid_as_lattice(T) || return Bottom
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
            if isdefined(p, :T) && !isdefined(p, :N) && isconcretetype(p.T)
                return Type{Tuple{t.parameters[1:np-1]..., Vararg{p.T, N}}} where N
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
    t = instanceof_tfunc(t)[1]
    t === Any && return v
    return tmeet(𝕃, v, t)
end
add_tfunc(typeassert, 2, 2, typeassert_tfunc, 4)

@nospecs function typeassert_nothrow(𝕃::AbstractLattice, v, t)
    ⊑ = Core.Compiler.:⊑(𝕃)
    # ty, exact = instanceof_tfunc(t)
    # return exact && v ⊑ ty
    if (isType(t) && !has_free_typevars(t) && v ⊑ t.parameters[1]) ||
        (isa(t, Const) && isa(t.val, Type) && v ⊑ t.val)
        return true
    end
    return false
end

@nospecs function isa_tfunc(𝕃::AbstractLattice, v, tt)
    t, isexact = instanceof_tfunc(tt)
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
    ⊑ = Core.Compiler.:⊑(𝕃)
    return typ ⊑ Type
end

@nospecs function subtype_tfunc(𝕃::AbstractLattice, a, b)
    a, isexact_a = instanceof_tfunc(a)
    b, isexact_b = instanceof_tfunc(b)
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
    ⊑ = Core.Compiler.:⊑(𝕃)
    return lty ⊑ Type && rty ⊑ Type
end

function fieldcount_noerror(@nospecialize t)
    if t isa UnionAll || t isa Union
        t = argument_datatype(t)
        if t === nothing
            return nothing
        end
        t = t::DataType
    elseif t === Union{}
        return 0
    end
    if !(t isa DataType)
        return nothing
    end
    if t.name === _NAMEDTUPLE_NAME
        names, types = t.parameters
        if names isa Tuple
            return length(names)
        end
        if types isa DataType && types <: Tuple
            return fieldcount_noerror(types)
        end
        abstr = true
    else
        abstr = isabstracttype(t) || (t.name === Tuple.name && isvatuple(t))
    end
    if abstr
        return nothing
    end
    return isdefined(t, :types) ? length(t.types) : length(t.name.names)
end

function try_compute_fieldidx(typ::DataType, @nospecialize(field))
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

function getfield_boundscheck((; fargs, argtypes)::ArgInfo) # Symbol
    farg = nothing
    if length(argtypes) == 3
        return :on
    elseif length(argtypes) == 4
        fargs !== nothing && (farg = fargs[4])
        boundscheck = argtypes[4]
        isvarargtype(boundscheck) && return :unknown
        if widenconst(boundscheck) === Symbol
            return :on
        end
    elseif length(argtypes) == 5
        fargs !== nothing && (farg = fargs[5])
        boundscheck = argtypes[5]
    else
        return :unknown
    end
    isvarargtype(boundscheck) && return :unknown
    boundscheck = widenconditional(boundscheck)
    if widenconst(boundscheck) === Bool
        if isa(boundscheck, Const)
            return boundscheck.val::Bool ? :on : :off
        elseif farg !== nothing && isexpr(farg, :boundscheck)
            return :boundscheck
        end
    end
    return :unknown
end

function getfield_nothrow(arginfo::ArgInfo, boundscheck::Symbol=getfield_boundscheck(arginfo))
    (;argtypes) = arginfo
    boundscheck === :unknown && return false
    ordering = Const(:not_atomic)
    if length(argtypes) == 4
        isvarargtype(argtypes[4]) && return false
        if widenconst(argtypes[4]) !== Bool
            ordering = argtypes[4]
        end
    elseif length(argtypes) == 5
        ordering = argtypes[5]
    elseif length(argtypes) != 3
        return false
    end
    isvarargtype(ordering) && return false
    widenconst(ordering) === Symbol || return false
    if isa(ordering, Const)
        ordering = ordering.val::Symbol
        if ordering !== :not_atomic # TODO: this is assuming not atomic
            return false
        end
        return getfield_nothrow(argtypes[2], argtypes[3], !(boundscheck === :off))
    else
        return false
    end
end
@nospecs function getfield_nothrow(s00, name, boundscheck::Bool)
    # If we don't have boundscheck off and don't know the field, don't even bother
    if boundscheck
        isa(name, Const) || return false
    end

    # If we have s00 being a const, we can potentially refine our type-based analysis above
    if isa(s00, Const) || isconstType(s00)
        if !isa(s00, Const)
            sv = s00.parameters[1]
        else
            sv = s00.val
        end
        if isa(name, Const)
            nval = name.val
            if !isa(nval, Symbol)
                isa(sv, Module) && return false
                isa(nval, Int) || return false
            end
            return isdefined(sv, nval)
        end
        if !boundscheck && !isa(sv, Module)
            # If bounds checking is disabled and all fields are assigned,
            # we may assume that we don't throw
            for i = 1:fieldcount(typeof(sv))
                isdefined(sv, i) || return false
            end
            return true
        end
        return false
    end

    s0 = widenconst(s00)
    s = unwrap_unionall(s0)
    if isa(s, Union)
        return getfield_nothrow(rewrap_unionall(s.a, s00), name, boundscheck) &&
               getfield_nothrow(rewrap_unionall(s.b, s00), name, boundscheck)
    elseif isType(s) && isTypeDataType(s.parameters[1])
        s = s0 = DataType
    end
    if isa(s, DataType)
        # Can't say anything about abstract types
        isabstracttype(s) && return false
        # If all fields are always initialized, and bounds check is disabled, we can assume
        # we don't throw
        if !boundscheck && s.name.n_uninitialized == 0
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
    t = isvarargtype(boundscheck_or_order) ? unwrapva(boundscheck_or_order) :
        widenconst(boundscheck_or_order)
    hasintersect(t, Symbol) || hasintersect(t, Bool) || return Bottom
    return getfield_tfunc(𝕃, s00, name)
end
@nospecs function getfield_tfunc(𝕃::AbstractLattice, s00, name, order, boundscheck)
    hasintersect(widenconst(order), Symbol) || return Bottom
    if isvarargtype(boundscheck)
        t = unwrapva(boundscheck)
        hasintersect(t, Symbol) || hasintersect(t, Bool) || return Bottom
    else
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
    if isconst(typeof(sv), nv)
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

@nospecs function _getfield_tfunc(𝕃::OptimizerLattice, s00, name, setfield::Bool)
    # If undef, that's a Union, but that doesn't affect the rt when tmerged
    # into the unwrapped result.
    isa(s00, MaybeUndef) && (s00 = s00.typ)
    return _getfield_tfunc(widenlattice(𝕃), s00, name, setfield)
end

@nospecs function _getfield_tfunc(𝕃::AnyConditionalsLattice, s00, name, setfield::Bool)
    if isa(s00, AnyConditional)
        return Bottom # Bool has no fields
    end
    return _getfield_tfunc(widenlattice(𝕃), s00, name, setfield)
end

@nospecs function _getfield_tfunc(𝕃::AnyMustAliasesLattice, s00, name, setfield::Bool)
    return _getfield_tfunc(widenlattice(𝕃), widenmustalias(s00), name, setfield)
end

@nospecs function _getfield_tfunc(𝕃::PartialsLattice, s00, name, setfield::Bool)
    if isa(s00, PartialStruct)
        s = widenconst(s00)
        sty = unwrap_unionall(s)::DataType
        if isa(name, Const)
            nv = _getfield_fieldindex(sty, name)
            if isa(nv, Int) && 1 <= nv <= length(s00.fields)
                return unwrapva(s00.fields[nv])
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
                    return abstract_eval_global(sv, nv)
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
            return rewrap_unionall(unwrapva(ftypes[1]), s00)
        end
        # union together types of all fields
        t = Bottom
        for i in 1:nf
            _ft = ftypes[i]
            setfield && isconst(s, i) && continue
            t = tmerge(t, rewrap_unionall(unwrapva(_ft), s00))
            t === Any && break
        end
        return t
    end
    fld = _getfield_fieldindex(s, name)
    fld === nothing && return Bottom
    if s <: Tuple && fld >= nf && isvarargtype(ftypes[nf])
        return rewrap_unionall(unwrapva(ftypes[nf]), s00)
    end
    if fld < 1 || fld > nf
        return Bottom
    elseif setfield && isconst(s, fld)
        return Bottom
    end
    R = ftypes[fld]
    if isempty(s.parameters)
        return R
    end
    return rewrap_unionall(R, s00)
end

@nospecs function getfield_notundefined(typ0, name)
    if isa(typ0, Const) && isa(name, Const)
        typv = typ0.val
        namev = name.val
        isa(typv, Module) && return true
        if isa(namev, Symbol) || isa(namev, Int)
            # Fields are not allowed to transition from defined to undefined, so
            # even if the field is not const, all we need to check here is that
            # it is defined here.
            return isdefined(typv, namev)
        end
    end
    typ0 = widenconst(typ0)
    typ = unwrap_unionall(typ0)
    if isa(typ, Union)
        return getfield_notundefined(rewrap_unionall(typ.a, typ0), name) &&
               getfield_notundefined(rewrap_unionall(typ.b, typ0), name)
    end
    isa(typ, DataType) || return false
    if typ.name === Tuple.name || typ.name === _NAMEDTUPLE_NAME
        # tuples and named tuples can't be instantiated with undefined fields,
        # so we don't need to be conservative here
        return true
    end
    if !isa(name, Const)
        isvarargtype(name) && return false
        if !hasintersect(widenconst(name), Union{Int,Symbol})
            return true # no undefined behavior if thrown
        end
        # field isn't known precisely, but let's check if all the fields can't be
        # initialized with undefined value so to avoid being too conservative
        fcnt = fieldcount_noerror(typ)
        fcnt === nothing && return false
        all(i::Int->is_undefref_fieldtype(fieldtype(typ,i)), (datatype_min_ninitialized(typ)+1):fcnt) && return true
        return false
    end
    name = name.val
    if isa(name, Symbol)
        fidx = fieldindex(typ, name, false)
        fidx === nothing && return true # no undefined behavior if thrown
    elseif isa(name, Int)
        fidx = name
    else
        return true # no undefined behavior if thrown
    end
    fcnt = fieldcount_noerror(typ)
    fcnt === nothing && return false
    0 < fidx ≤ fcnt || return true # no undefined behavior if thrown
    ftyp = fieldtype(typ, fidx)
    is_undefref_fieldtype(ftyp) && return true
    return fidx ≤ datatype_min_ninitialized(typ)
end
# checks if a field of this type will not be initialized with undefined value
# and the access to that uninitialized field will cause and `UndefRefError`, e.g.,
# - is_undefref_fieldtype(String) === true
# - is_undefref_fieldtype(Integer) === true
# - is_undefref_fieldtype(Any) === true
# - is_undefref_fieldtype(Int) === false
# - is_undefref_fieldtype(Union{Int32,Int64}) === false
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
    ⊑ = Core.Compiler.:⊑(𝕃)
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
        return v ⊑ v_expected
    end
    return false
end

@nospecs function swapfield!_tfunc(𝕃::AbstractLattice, o, f, v, order)
    return getfield_tfunc(𝕃, o, f)
end
@nospecs function swapfield!_tfunc(𝕃::AbstractLattice, o, f, v)
    return getfield_tfunc(𝕃, o, f)
end
@nospecs function modifyfield!_tfunc(𝕃::AbstractLattice, o, f, op, v, order)
    return modifyfield!_tfunc(𝕃, o, f, op, v)
end
@nospecs function modifyfield!_tfunc(𝕃::AbstractLattice, o, f, op, v)
    T = _fieldtype_tfunc(𝕃, o, f, isconcretetype(o))
    T === Bottom && return Bottom
    PT = Const(Pair)
    return instanceof_tfunc(apply_type_tfunc(𝕃, PT, T, T))[1]
end
function abstract_modifyfield!(interp::AbstractInterpreter, argtypes::Vector{Any}, si::StmtInfo, sv::InferenceState)
    nargs = length(argtypes)
    if !isempty(argtypes) && isvarargtype(argtypes[nargs])
        nargs - 1 <= 6 || return CallMeta(Bottom, EFFECTS_THROWS, NoCallInfo())
        nargs > 3 || return CallMeta(Any, EFFECTS_UNKNOWN, NoCallInfo())
    else
        5 <= nargs <= 6 || return CallMeta(Bottom, EFFECTS_THROWS, NoCallInfo())
    end
    𝕃ᵢ = typeinf_lattice(interp)
    o = unwrapva(argtypes[2])
    f = unwrapva(argtypes[3])
    RT = modifyfield!_tfunc(𝕃ᵢ, o, f, Any, Any)
    info = NoCallInfo()
    if nargs >= 5 && RT !== Bottom
        # we may be able to refine this to a PartialStruct by analyzing `op(o.f, v)::T`
        # as well as compute the info for the method matches
        op = unwrapva(argtypes[4])
        v = unwrapva(argtypes[5])
        TF = getfield_tfunc(𝕃ᵢ, o, f)
        callinfo = abstract_call(interp, ArgInfo(nothing, Any[op, TF, v]), StmtInfo(true), sv, #=max_methods=# 1)
        TF2 = tmeet(callinfo.rt, widenconst(TF))
        if TF2 === Bottom
            RT = Bottom
        elseif isconcretetype(RT) && has_nontrivial_extended_info(𝕃ᵢ, TF2) # isconcrete condition required to form a PartialStruct
            RT = PartialStruct(RT, Any[TF, TF2])
        end
        info = ModifyFieldInfo(callinfo.info)
    end
    return CallMeta(RT, Effects(), info)
end
@nospecs function replacefield!_tfunc(𝕃::AbstractLattice, o, f, x, v, success_order, failure_order)
    return replacefield!_tfunc(𝕃, o, f, x, v)
end
@nospecs function replacefield!_tfunc(𝕃::AbstractLattice, o, f, x, v, success_order)
    return replacefield!_tfunc(𝕃, o, f, x, v)
end
@nospecs function replacefield!_tfunc(𝕃::AbstractLattice, o, f, x, v)
    T = _fieldtype_tfunc(𝕃, o, f, isconcretetype(o))
    T === Bottom && return Bottom
    PT = Const(ccall(:jl_apply_cmpswap_type, Any, (Any,), T) where T)
    return instanceof_tfunc(apply_type_tfunc(𝕃, PT, T))[1]
end

# we could use tuple_tfunc instead of widenconst, but `o` is mutable, so that is unlikely to be beneficial

add_tfunc(getfield, 2, 4, getfield_tfunc, 1)
add_tfunc(setfield!, 3, 4, setfield!_tfunc, 3)

add_tfunc(swapfield!, 3, 4, swapfield!_tfunc, 3)
add_tfunc(modifyfield!, 4, 5, modifyfield!_tfunc, 3)
add_tfunc(replacefield!, 4, 6, replacefield!_tfunc, 3)

@nospecs function fieldtype_nothrow(𝕃::AbstractLattice, s0, name)
    s0 === Bottom && return true # unreachable
    ⊑ = Core.Compiler.:⊑(𝕃)
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

    s, exact = instanceof_tfunc(s0)
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

    s, exact = instanceof_tfunc(s0)
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
        ta, exacta, _, istypea = instanceof_tfunc(ta0)
        tb, exactb, _, istypeb = instanceof_tfunc(tb0)
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
    # TODO: implement optimization for isvarargtype(u) and istuple occurences (which are valid but are not UnionAll)
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
            T, exact, _, istype = instanceof_tfunc(ai)
            if T === Bottom
                if !(u.var.lb === Union{} && u.var.ub === Any)
                    return false
                end
                if !valid_tparam_type(widenconst(ai))
                    return false
                end
            else
                istype || return false
                if !(T <: u.var.ub)
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

# TODO: handle e.g. apply_type(T, R::Union{Type{Int32},Type{Float64}})
@nospecs function apply_type_tfunc(𝕃::AbstractLattice, headtypetype, args...)
    headtypetype = widenslotwrapper(headtypetype)
    if isa(headtypetype, Const)
        headtype = headtypetype.val
    elseif isconstType(headtypetype)
        headtype = headtypetype.parameters[1]
    else
        return Any
    end
    if !isempty(args) && isvarargtype(args[end])
        return isvarargtype(headtype) ? TypeofVararg : Type
    end
    largs = length(args)
    if headtype === Union
        largs == 0 && return Const(Bottom)
        hasnonType = false
        for i = 1:largs
            ai = args[i]
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
        if largs == 1 # Union{T} --> T
            u1 = typeintersect(widenconst(args[1]), Union{Type,TypeVar})
            valid_as_lattice(u1) || return Bottom
            return u1
        end
        hasnonType && return Type
        ty = Union{}
        allconst = true
        for i = 1:largs
            ai = args[i]
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
    istuple = isa(headtype, Type) && (headtype == Tuple)
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
        if (outer_start += 1) > largs
            push!(outervars, ua.var)
        end
        ua = ua.body
    end
    if largs > outer_start && isa(headtype, UnionAll) # e.g. !isvarargtype(ua) && !istuple
        return Bottom # too many arguments
    end
    outer_start = outer_start - largs + 1

    varnamectr = 1
    ua = headtype
    for i = 1:largs
        ai = widenslotwrapper(args[i])
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
            if isT
                tai = ai
                while isa(tai, UnionAll)
                    if contains_is(outervars, tai.var)
                        ai = rename_unionall(ai)
                        unw = unwrap_unionall(ai)
                        break
                    end
                    tai = tai.body
                end
            end
            ai_w = widenconst(ai)
            ub = ai_w isa Type && ai_w <: Type ? instanceof_tfunc(ai)[1] : Any
            if istuple
                # in the last parameter of a Tuple type, if the upper bound is Any
                # then this could be a Vararg type.
                if i == largs && ub === Any
                    push!(tparams, Vararg)
                elseif isT
                    push!(tparams, rewrap_unionall(unw.parameters[1], ai))
                else
                    push!(tparams, Any)
                end
            elseif isT
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
                    if !(uw.parameters[1] isa Tuple || (i == 2 && tparams[1] isa Tuple))
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
            for _ = 1:largs
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
add_tfunc(apply_type, 1, INT_INF, apply_type_tfunc, 10)

# convert the dispatch tuple type argtype to the real (concrete) type of
# the tuple of those values
function tuple_tfunc(𝕃::AbstractLattice, argtypes::Vector{Any})
    argtypes = anymap(widenslotwrapper, argtypes)
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
            else
                params[i] = x
            end
        end
    end
    typ = Tuple{params...}
    # replace a singleton type with its equivalent Const object
    issingletontype(typ) && return Const(typ.instance)
    return anyinfo ? PartialStruct(typ, argtypes) : typ
end

@nospecs function arrayref_tfunc(𝕃::AbstractLattice, boundscheck, ary, idxs...)
    return _arrayref_tfunc(𝕃, boundscheck, ary, idxs)
end
@nospecs function _arrayref_tfunc(𝕃::AbstractLattice, boundscheck, ary, @nospecialize idxs::Tuple)
    isempty(idxs) && return Bottom
    array_builtin_common_errorcheck(boundscheck, ary, idxs) || return Bottom
    return array_elmtype(ary)
end
add_tfunc(arrayref, 3, INT_INF, arrayref_tfunc, 20)
add_tfunc(const_arrayref, 3, INT_INF, arrayref_tfunc, 20)

@nospecs function arrayset_tfunc(𝕃::AbstractLattice, boundscheck, ary, item, idxs...)
    hasintersect(widenconst(item), _arrayref_tfunc(𝕃, boundscheck, ary, idxs)) || return Bottom
    return ary
end
add_tfunc(arrayset, 4, INT_INF, arrayset_tfunc, 20)

@nospecs function array_builtin_common_errorcheck(boundscheck, ary, @nospecialize idxs::Tuple)
    hasintersect(widenconst(boundscheck), Bool) || return false
    hasintersect(widenconst(ary), Array) || return false
    for i = 1:length(idxs)
        idx = getfield(idxs, i)
        idx = isvarargtype(idx) ? unwrapva(idx) : widenconst(idx)
        hasintersect(idx, Int) || return false
    end
    return true
end

function array_elmtype(@nospecialize ary)
    a = widenconst(ary)
    if !has_free_typevars(a) && a <: Array
        a0 = a
        if isa(a, UnionAll)
            a = unwrap_unionall(a0)
        end
        if isa(a, DataType)
            T = a.parameters[1]
            valid_as_lattice(T) || return Bottom
            return rewrap_unionall(T, a0)
        end
    end
    return Any
end

@nospecs function _opaque_closure_tfunc(𝕃::AbstractLattice, arg, lb, ub, source, env::Vector{Any}, linfo::MethodInstance)
    argt, argt_exact = instanceof_tfunc(arg)
    lbt, lb_exact = instanceof_tfunc(lb)
    if !lb_exact
        lbt = Union{}
    end

    ubt, ub_exact = instanceof_tfunc(ub)

    t = (argt_exact ? Core.OpaqueClosure{argt, T} : Core.OpaqueClosure{<:argt, T}) where T
    t = lbt == ubt ? t{ubt} : (t{T} where lbt <: T <: ubt)

    (isa(source, Const) && isa(source.val, Method)) || return t

    return PartialOpaque(t, tuple_tfunc(𝕃, env), linfo, source.val)
end

# whether getindex for the elements can potentially throw UndefRef
function array_type_undefable(@nospecialize(arytype))
    if isa(arytype, Union)
        return array_type_undefable(arytype.a) || array_type_undefable(arytype.b)
    elseif isa(arytype, UnionAll)
        return true
    else
        elmtype = (arytype::DataType).parameters[1]
        return !(elmtype isa Type && (isbitstype(elmtype) || isbitsunion(elmtype)))
    end
end

function array_builtin_common_nothrow(argtypes::Vector{Any}, first_idx_idx::Int)
    length(argtypes) >= 4 || return false
    boundscheck = argtypes[1]
    arytype = argtypes[2]
    array_builtin_common_typecheck(boundscheck, arytype, argtypes, first_idx_idx) || return false
    # If we could potentially throw undef ref errors, bail out now.
    arytype = widenconst(arytype)
    array_type_undefable(arytype) && return false
    # If we have @inbounds (first argument is false), we're allowed to assume
    # we don't throw bounds errors.
    if isa(boundscheck, Const)
        !(boundscheck.val::Bool) && return true
    end
    # Else we can't really say anything here
    # TODO: In the future we may be able to track the shapes of arrays though
    # inference.
    return false
end

@nospecs function array_builtin_common_typecheck(boundscheck, arytype,
    argtypes::Vector{Any}, first_idx_idx::Int)
    (boundscheck ⊑ Bool && arytype ⊑ Array) || return false
    for i = first_idx_idx:length(argtypes)
        argtypes[i] ⊑ Int || return false
    end
    return true
end

@nospecs function arrayset_typecheck(arytype, elmtype)
    # Check that we can determine the element type
    arytype = widenconst(arytype)
    isa(arytype, DataType) || return false
    elmtype_expected = arytype.parameters[1]
    isa(elmtype_expected, Type) || return false
    # Check that the element type is compatible with the element we're assigning
    elmtype ⊑ elmtype_expected || return false
    return true
end

# Query whether the given builtin is guaranteed not to throw given the argtypes
@nospecs function _builtin_nothrow(𝕃::AbstractLattice, f, argtypes::Vector{Any}, rt)
    ⊑ = Core.Compiler.:⊑(𝕃)
    if f === arrayset
        array_builtin_common_nothrow(argtypes, 4) || return false
        # Additionally check element type compatibility
        return arrayset_typecheck(argtypes[2], argtypes[3])
    elseif f === arrayref || f === const_arrayref
        return array_builtin_common_nothrow(argtypes, 3)
    elseif f === Core._expr
        length(argtypes) >= 1 || return false
        return argtypes[1] ⊑ Symbol
    end

    # These builtins are not-vararg, so if we have varars, here, we can't guarantee
    # the correct number of arguments.
    na = length(argtypes)
    (na ≠ 0 && isvarargtype(argtypes[end])) && return false
    if f === arraysize
        na == 2 || return false
        return arraysize_nothrow(argtypes[1], argtypes[2])
    elseif f === Core._typevar
        na == 3 || return false
        return typevar_nothrow(𝕃, argtypes[1], argtypes[2], argtypes[3])
    elseif f === invoke
        return false
    elseif f === getfield
        return getfield_nothrow(ArgInfo(nothing, Any[Const(f), argtypes...]))
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
    elseif f === UnionAll
        return na == 2 && (argtypes[1] ⊑ TypeVar && argtypes[2] ⊑ Type)
    elseif f === isdefined
        na == 2 || return false
        return isdefined_nothrow(𝕃, argtypes[1], argtypes[2])
    elseif f === Core.sizeof
        na == 1 || return false
        return sizeof_nothrow(argtypes[1])
    elseif f === Core.ifelse
        na == 3 || return false
        return ifelse_nothrow(𝕃, argtypes[1], nothing, nothing)
    elseif f === typeassert
        na == 2 || return false
        return typeassert_nothrow(𝕃, argtypes[1], argtypes[2])
    elseif f === getglobal
        if na == 2
            return getglobal_nothrow(argtypes[1], argtypes[2])
        elseif na == 3
            return getglobal_nothrow(argtypes[1], argtypes[2], argtypes[3])
        end
        return false
    elseif f === setglobal!
        if na == 3
            return setglobal!_nothrow(argtypes[1], argtypes[2], argtypes[3])
        elseif na == 4
            return setglobal!_nothrow(argtypes[1], argtypes[2], argtypes[3], argtypes[4])
        end
        return false
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
const _PURE_BUILTINS = Any[tuple, svec, ===, typeof, nfields, applicable]

# known to be effect-free (but not necessarily nothrow)
const _EFFECT_FREE_BUILTINS = [
    fieldtype, apply_type, isa, UnionAll,
    getfield, arrayref, const_arrayref, isdefined, Core.sizeof,
    Core.ifelse, Core._typevar, (<:),
    typeassert, throw, arraysize, getglobal, compilerbarrier
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
    setfield!
]

const _INACCESSIBLEMEM_BUILTINS = Any[
    (<:),
    (===),
    apply_type,
    arraysize,
    Core.ifelse,
    Core.sizeof,
    svec,
    fieldtype,
    isa,
    isdefined,
    nfields,
    throw,
    tuple,
    typeassert,
    typeof,
    compilerbarrier,
    Core._typevar
]

const _ARGMEM_BUILTINS = Any[
    arrayref,
    arrayset,
    modifyfield!,
    replacefield!,
    setfield!,
    swapfield!,
]

const _INCONSISTENT_INTRINSICS = Any[
    Intrinsics.pointerref,      # this one is volatile
    Intrinsics.arraylen,        # this one is volatile
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
    Intrinsics.sub_float_fast
    # TODO needs to revive #31193 to mark this as inconsistent to be accurate
    # while preserving the currently optimizations for many math operations
    # Intrinsics.muladd_float,    # this is not interprocedurally consistent
]

const _SPECIAL_BUILTINS = Any[
    Core._apply_iterate,
]

function isdefined_effects(𝕃::AbstractLattice, argtypes::Vector{Any})
    # consistent if the first arg is immutable
    na = length(argtypes)
    na == 0 && return EFFECTS_THROWS
    obj = argtypes[1]
    consistent = is_immutable_argtype(unwrapva(obj)) ? ALWAYS_TRUE : ALWAYS_FALSE
    nothrow = !isvarargtype(argtypes[end]) && na == 2 && isdefined_nothrow(𝕃, obj, argtypes[2])
    return Effects(EFFECTS_TOTAL; consistent, nothrow)
end

function getfield_effects(arginfo::ArgInfo, @nospecialize(rt))
    (;argtypes) = arginfo
    # consistent if the argtype is immutable
    length(argtypes) < 3 && return EFFECTS_THROWS
    obj = argtypes[2]
    isvarargtype(obj) && return Effects(EFFECTS_THROWS; consistent=ALWAYS_FALSE)
    consistent = (is_immutable_argtype(obj) || is_mutation_free_argtype(obj)) ?
        ALWAYS_TRUE : CONSISTENT_IF_INACCESSIBLEMEMONLY
    # access to `isbitstype`-field initialized with undefined value leads to undefined behavior
    # so should taint `:consistent`-cy while access to uninitialized non-`isbitstype` field
    # throws `UndefRefError` so doesn't need to taint it
    # NOTE `getfield_notundefined` conservatively checks if this field is never initialized
    # with undefined value so that we don't taint `:consistent`-cy too aggressively here
    if !(length(argtypes) ≥ 3 && getfield_notundefined(obj, argtypes[3]))
        consistent = ALWAYS_FALSE
    end
    nothrow = getfield_nothrow(arginfo, :on)
    if !nothrow
        bcheck = getfield_boundscheck(arginfo)
        if !(bcheck === :on || bcheck === :boundscheck)
            # If we cannot independently prove inboundsness, taint consistency.
            # The inbounds-ness assertion requires dynamic reachability, while
            # :consistent needs to be true for all input values.
            # However, as a special exception, we do allow literal `:boundscheck`.
            # `:consistent`-cy will be tainted in any caller using `@inbounds` based
            # on the `:noinbounds` effect.
            # N.B. We do not taint for `--check-bounds=no` here. That is handled
            # in concrete evaluation.
            consistent = ALWAYS_FALSE
        end
    end
    if hasintersect(widenconst(obj), Module)
        inaccessiblememonly = getglobal_effects(argtypes[2:end], rt).inaccessiblememonly
    elseif is_mutation_free_argtype(obj)
        inaccessiblememonly = ALWAYS_TRUE
    else
        inaccessiblememonly = INACCESSIBLEMEM_OR_ARGMEMONLY
    end
    return Effects(EFFECTS_TOTAL; consistent, nothrow, inaccessiblememonly)
end

function getglobal_effects(argtypes::Vector{Any}, @nospecialize(rt))
    consistent = inaccessiblememonly = ALWAYS_FALSE
    nothrow = false
    if length(argtypes) ≥ 2
        M, s = argtypes[1], argtypes[2]
        if getglobal_nothrow(M, s)
            nothrow = true
            # typeasserts below are already checked in `getglobal_nothrow`
            Mval, sval = (M::Const).val::Module, (s::Const).val::Symbol
            if isconst(Mval, sval)
                consistent = ALWAYS_TRUE
                if is_mutation_free_argtype(rt)
                    inaccessiblememonly = ALWAYS_TRUE
                end
            end
        end
    end
    return Effects(EFFECTS_TOTAL; consistent, nothrow, inaccessiblememonly)
end

function builtin_effects(𝕃::AbstractLattice, @nospecialize(f::Builtin), arginfo::ArgInfo, @nospecialize(rt))
    if isa(f, IntrinsicFunction)
        return intrinsic_effects(f, arginfo.argtypes[2:end])
    end

    @assert !contains_is(_SPECIAL_BUILTINS, f)

    if f === getfield
        return getfield_effects(arginfo, rt)
    end
    argtypes = arginfo.argtypes[2:end]

    if f === isdefined
        return isdefined_effects(𝕃, argtypes)
    elseif f === getglobal
        return getglobal_effects(argtypes, rt)
    elseif f === Core.get_binding_type
        length(argtypes) == 2 || return EFFECTS_THROWS
        effect_free = get_binding_type_effect_free(argtypes[1], argtypes[2]) ? ALWAYS_TRUE : ALWAYS_FALSE
        return Effects(EFFECTS_TOTAL; effect_free)
    else
        consistent = contains_is(_CONSISTENT_BUILTINS, f) ? ALWAYS_TRUE :
            (f === Core._typevar) ? CONSISTENT_IF_NOTRETURNED : ALWAYS_FALSE
        if f === setfield! || f === arrayset
            effect_free = EFFECT_FREE_IF_INACCESSIBLEMEMONLY
        elseif contains_is(_EFFECT_FREE_BUILTINS, f) || contains_is(_PURE_BUILTINS, f)
            effect_free = ALWAYS_TRUE
        else
            effect_free = ALWAYS_FALSE
        end
        nothrow = (isempty(argtypes) || !isvarargtype(argtypes[end])) && builtin_nothrow(𝕃, f, argtypes, rt)
        if contains_is(_INACCESSIBLEMEM_BUILTINS, f)
            inaccessiblememonly = ALWAYS_TRUE
        elseif contains_is(_ARGMEM_BUILTINS, f)
            inaccessiblememonly = INACCESSIBLEMEM_OR_ARGMEMONLY
        else
            inaccessiblememonly = ALWAYS_FALSE
        end
        return Effects(EFFECTS_TOTAL; consistent, effect_free, nothrow, inaccessiblememonly)
    end
end

function builtin_nothrow(𝕃::AbstractLattice, @nospecialize(f), argtypes::Vector{Any}, @nospecialize(rt))
    rt === Bottom && return false
    contains_is(_PURE_BUILTINS, f) && return true
    return _builtin_nothrow(𝕃, f, argtypes, rt)
end

function builtin_tfunction(interp::AbstractInterpreter, @nospecialize(f), argtypes::Vector{Any},
                           sv::Union{InferenceState,IRCode,Nothing})
    𝕃ᵢ = typeinf_lattice(interp)
    if f === tuple
        return tuple_tfunc(𝕃ᵢ, argtypes)
    end
    if isa(f, IntrinsicFunction)
        if is_pure_intrinsic_infer(f) && all(@nospecialize(a) -> isa(a, Const), argtypes)
            argvals = anymap(@nospecialize(a) -> (a::Const).val, argtypes)
            try
                return Const(f(argvals...))
            catch
            end
        end
        iidx = Int(reinterpret(Int32, f::IntrinsicFunction)) + 1
        if iidx < 0 || iidx > length(T_IFUNC)
            # invalid intrinsic
            return Any
        end
        tf = T_IFUNC[iidx]
    else
        fidx = find_tfunc(f)
        if fidx === nothing
            # unknown/unhandled builtin function
            return Any
        end
        tf = T_FFUNC_VAL[fidx]
    end
    tf = tf::Tuple{Int, Int, Any}
    if !isempty(argtypes) && isvarargtype(argtypes[end])
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

function intrinsic_nothrow(f::IntrinsicFunction, argtypes::Vector{Any})
    # First check that we have the correct number of arguments
    iidx = Int(reinterpret(Int32, f::IntrinsicFunction)) + 1
    if iidx < 1 || iidx > length(T_IFUNC)
        # invalid intrinsic
        return false
    end
    tf = T_IFUNC[iidx]
    tf = tf::Tuple{Int, Int, Any}
    if !(tf[1] <= length(argtypes) <= tf[2])
        # wrong # of args
        return false
    end
    # TODO: We could do better for cglobal
    f === Intrinsics.cglobal && return false
    # TODO: We can't know for sure, but the user should have a way to assert
    # that it won't
    f === Intrinsics.llvmcall && return false
    if f === Intrinsics.checked_udiv_int || f === Intrinsics.checked_urem_int || f === Intrinsics.checked_srem_int || f === Intrinsics.checked_sdiv_int
        # Nothrow as long as the second argument is guaranteed not to be zero
        arg2 = argtypes[2]
        isa(arg2, Const) || return false
        arg1 = argtypes[1]
        warg1 = widenconst(arg1)
        warg2 = widenconst(arg2)
        (warg1 === warg2 && isprimitivetype(warg1)) || return false
        den_val = arg2.val
        _iszero(den_val) && return false
        f !== Intrinsics.checked_sdiv_int && return true
        # Nothrow as long as we additionally don't do typemin(T)/-1
        return !_isneg1(den_val) || (isa(arg1, Const) && !_istypemin(arg1.val))
    end
    if f === Intrinsics.pointerref
        # Nothrow as long as the types are ok. N.B.: dereferencability is not
        # modeled here, but can cause errors (e.g. ReadOnlyMemoryError). We follow LLVM here
        # in that it is legal to remove unused non-volatile loads.
        length(argtypes) == 3 || return false
        return argtypes[1] ⊑ Ptr && argtypes[2] ⊑ Int && argtypes[3] ⊑ Int
    end
    if f === Intrinsics.pointerset
        eT = pointer_eltype(argtypes[1])
        isprimitivetype(eT) || return false
        return argtypes[2] ⊑ eT && argtypes[3] ⊑ Int && argtypes[4] ⊑ Int
    end
    if f === Intrinsics.arraylen
        return argtypes[1] ⊑ Array
    end
    if f === Intrinsics.bitcast
        ty, isexact, isconcrete = instanceof_tfunc(argtypes[1])
        xty = widenconst(argtypes[2])
        return isconcrete && isprimitivetype(ty) && isprimitivetype(xty) && Core.sizeof(ty) === Core.sizeof(xty)
    end
    if f in (Intrinsics.sext_int, Intrinsics.zext_int, Intrinsics.trunc_int,
             Intrinsics.fptoui, Intrinsics.fptosi, Intrinsics.uitofp,
             Intrinsics.sitofp, Intrinsics.fptrunc, Intrinsics.fpext)
        # If !isconcrete, `ty` may be Union{} at runtime even if we have
        # isprimitivetype(ty).
        ty, isexact, isconcrete = instanceof_tfunc(argtypes[1])
        xty = widenconst(argtypes[2])
        return isconcrete && isprimitivetype(ty) && isprimitivetype(xty)
    end
    if f === Intrinsics.have_fma
        ty, isexact, isconcrete = instanceof_tfunc(argtypes[1])
        return isconcrete && isprimitivetype(ty)
    end
    # The remaining intrinsics are math/bits/comparison intrinsics. They work on all
    # primitive types of the same type.
    isshift = f === shl_int || f === lshr_int || f === ashr_int
    argtype1 = widenconst(argtypes[1])
    isprimitivetype(argtype1) || return false
    for i = 2:length(argtypes)
        argtype = widenconst(argtypes[i])
        if isshift ? !isprimitivetype(argtype) : argtype !== argtype1
            return false
        end
    end
    return true
end

# whether `f` is pure for inference
function is_pure_intrinsic_infer(f::IntrinsicFunction)
    return !(f === Intrinsics.pointerref || # this one is volatile
             f === Intrinsics.pointerset || # this one is never effect-free
             f === Intrinsics.llvmcall ||   # this one is never effect-free
             f === Intrinsics.arraylen ||   # this one is volatile
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

    consistent = contains_is(_INCONSISTENT_INTRINSICS, f) ? ALWAYS_FALSE : ALWAYS_TRUE
    effect_free = !(f === Intrinsics.pointerset) ? ALWAYS_TRUE : ALWAYS_FALSE
    nothrow = (isempty(argtypes) || !isvarargtype(argtypes[end])) && intrinsic_nothrow(f, argtypes)

    return Effects(EFFECTS_TOTAL; consistent, effect_free, nothrow)
end

# TODO: this function is a very buggy and poor model of the return_type function
# since abstract_call_gf_by_type is a very inaccurate model of _method and of typeinf_type,
# while this assumes that it is an absolutely precise and accurate and exact model of both
function return_type_tfunc(interp::AbstractInterpreter, argtypes::Vector{Any}, si::StmtInfo, sv::Union{InferenceState, IRCode})
    if length(argtypes) == 3
        tt = widenslotwrapper(argtypes[3])
        if isa(tt, Const) || (isType(tt) && !has_free_typevars(tt))
            aft = widenslotwrapper(argtypes[2])
            if isa(aft, Const) || (isType(aft) && !has_free_typevars(aft)) ||
                   (isconcretetype(aft) && !(aft <: Builtin))
                af_argtype = isa(tt, Const) ? tt.val : (tt::DataType).parameters[1]
                if isa(af_argtype, DataType) && af_argtype <: Tuple
                    argtypes_vec = Any[aft, af_argtype.parameters...]
                    if contains_is(argtypes_vec, Union{})
                        return CallMeta(Const(Union{}), EFFECTS_TOTAL, NoCallInfo())
                    end
                    #
                    # Run the abstract_call without restricting abstract call
                    # sites. Otherwise, our behavior model of abstract_call
                    # below will be wrong.
                    if isa(sv, InferenceState)
                        old_restrict = sv.restrict_abstract_call_sites
                        sv.restrict_abstract_call_sites = false
                        call = abstract_call(interp, ArgInfo(nothing, argtypes_vec), si, sv, -1)
                        sv.restrict_abstract_call_sites = old_restrict
                    else
                        call = abstract_call(interp, ArgInfo(nothing, argtypes_vec), si, sv, -1)
                    end
                    info = verbose_stmt_info(interp) ? MethodResultPure(ReturnTypeCallInfo(call.info)) : MethodResultPure()
                    rt = widenslotwrapper(call.rt)
                    if isa(rt, Const)
                        # output was computed to be constant
                        return CallMeta(Const(typeof(rt.val)), EFFECTS_TOTAL, info)
                    end
                    rt = widenconst(rt)
                    if rt === Bottom || (isconcretetype(rt) && !iskindtype(rt))
                        # output cannot be improved so it is known for certain
                        return CallMeta(Const(rt), EFFECTS_TOTAL, info)
                    elseif isa(sv, InferenceState) && !isempty(sv.pclimitations)
                        # conservatively express uncertainty of this result
                        # in two ways: both as being a subtype of this, and
                        # because of LimitedAccuracy causes
                        return CallMeta(Type{<:rt}, EFFECTS_TOTAL, info)
                    elseif (isa(tt, Const) || isconstType(tt)) &&
                        (isa(aft, Const) || isconstType(aft))
                        # input arguments were known for certain
                        # XXX: this doesn't imply we know anything about rt
                        return CallMeta(Const(rt), EFFECTS_TOTAL, info)
                    elseif isType(rt)
                        return CallMeta(Type{rt}, EFFECTS_TOTAL, info)
                    else
                        return CallMeta(Type{<:rt}, EFFECTS_TOTAL, info)
                    end
                end
            end
        end
    end
    return CallMeta(Type, EFFECTS_THROWS, NoCallInfo())
end

# a simplified model of abstract_call_gf_by_type for applicable
function abstract_applicable(interp::AbstractInterpreter, argtypes::Vector{Any},
                             sv::InferenceState, max_methods::Int)
    length(argtypes) < 2 && return CallMeta(Union{}, EFFECTS_UNKNOWN, NoCallInfo())
    isvarargtype(argtypes[2]) && return CallMeta(Bool, EFFECTS_UNKNOWN, NoCallInfo())
    argtypes = argtypes[2:end]
    atype = argtypes_to_type(argtypes)
    matches = find_matching_methods(typeinf_lattice(interp), argtypes, atype, method_table(interp),
        InferenceParams(interp).max_union_splitting, max_methods)
    if isa(matches, FailedMethodMatch)
        rt = Bool # too many matches to analyze
    else
        (; valid_worlds, applicable) = matches
        update_valid_age!(sv, valid_worlds)

        # also need an edge to the method table in case something gets
        # added that did not intersect with any existing method
        if isa(matches, MethodMatches)
            matches.fullmatch || add_mt_backedge!(sv, matches.mt, atype)
        else
            for (thisfullmatch, mt) in zip(matches.fullmatches, matches.mts)
                thisfullmatch || add_mt_backedge!(sv, mt, atype)
            end
        end

        napplicable = length(applicable)
        if napplicable == 0
            rt = Const(false) # never any matches
        else
            rt = Const(true) # has applicable matches
            for i in 1:napplicable
                match = applicable[i]::MethodMatch
                edge = specialize_method(match)
                add_backedge!(sv, edge)
            end

            if isa(matches, MethodMatches) ? (!matches.fullmatch || any_ambig(matches)) :
                    (!all(matches.fullmatches) || any_ambig(matches))
                # Account for the fact that we may encounter a MethodError with a non-covered or ambiguous signature.
                rt = Bool
            end
        end
    end
    return CallMeta(rt, EFFECTS_TOTAL, NoCallInfo())
end
add_tfunc(applicable, 1, INT_INF, @nospecs((𝕃::AbstractLattice, f, args...)->Bool), 40)

# a simplified model of abstract_invoke for Core._hasmethod
function _hasmethod_tfunc(interp::AbstractInterpreter, argtypes::Vector{Any}, sv::InferenceState)
    if length(argtypes) == 3 && !isvarargtype(argtypes[3])
        ft′ = argtype_by_index(argtypes, 2)
        ft = widenconst(ft′)
        ft === Bottom && return CallMeta(Bool, EFFECTS_THROWS, NoCallInfo())
        typeidx = 3
    elseif length(argtypes) == 2 && !isvarargtype(argtypes[2])
        typeidx = 2
    else
        return CallMeta(Any, Effects(), NoCallInfo())
    end
    (types, isexact, isconcrete, istype) = instanceof_tfunc(argtype_by_index(argtypes, typeidx))
    isexact || return CallMeta(Bool, Effects(), NoCallInfo())
    unwrapped = unwrap_unionall(types)
    if types === Bottom || !(unwrapped isa DataType) || unwrapped.name !== Tuple.name
        return CallMeta(Bool, EFFECTS_THROWS, NoCallInfo())
    end
    if typeidx == 3
        isdispatchelem(ft) || return CallMeta(Bool, Effects(), NoCallInfo()) # check that we might not have a subtype of `ft` at runtime, before doing supertype lookup below
        types = rewrap_unionall(Tuple{ft, unwrapped.parameters...}, types)::Type
    end
    mt = ccall(:jl_method_table_for, Any, (Any,), types)
    if !isa(mt, MethodTable)
        return CallMeta(Bool, EFFECTS_THROWS, NoCallInfo())
    end
    match, valid_worlds, overlayed = findsup(types, method_table(interp))
    update_valid_age!(sv, valid_worlds)
    if match === nothing
        rt = Const(false)
        add_mt_backedge!(sv, mt, types) # this should actually be an invoke-type backedge
    else
        rt = Const(true)
        edge = specialize_method(match)
        add_invoke_backedge!(sv, types, edge)
    end
    return CallMeta(rt, EFFECTS_TOTAL, NoCallInfo())
end


# N.B.: typename maps type equivalence classes to a single value
function typename_static(@nospecialize(t))
    t isa Const && return _typename(t.val)
    t isa Conditional && return Bool.name
    t = unwrap_unionall(widenconst(t))
    return isType(t) ? _typename(t.parameters[1]) : Core.TypeName
end

function global_order_nothrow(@nospecialize(o), loading::Bool, storing::Bool)
    o isa Const || return false
    sym = o.val
    if sym isa Symbol
        order = get_atomic_order(sym, loading, storing)
        return order !== MEMORY_ORDER_INVALID && order !== MEMORY_ORDER_NOTATOMIC
    end
    return false
end
@nospecs function getglobal_nothrow(M, s, o)
    global_order_nothrow(o, #=loading=#true, #=storing=#false) || return false
    return getglobal_nothrow(M, s)
end
@nospecs function getglobal_nothrow(M, s)
    if M isa Const && s isa Const
        M, s = M.val, s.val
        if M isa Module && s isa Symbol
            return isdefined(M, s)
        end
    end
    return false
end
@nospecs function getglobal_tfunc(𝕃::AbstractLattice, M, s, order=Symbol)
    if M isa Const && s isa Const
        M, s = M.val, s.val
        if M isa Module && s isa Symbol
            return abstract_eval_global(M, s)
        end
        return Bottom
    elseif !(hasintersect(widenconst(M), Module) && hasintersect(widenconst(s), Symbol))
        return Bottom
    end
    return Any
end
@nospecs function setglobal!_tfunc(𝕃::AbstractLattice, M, s, v, order=Symbol)
    if !(hasintersect(widenconst(M), Module) && hasintersect(widenconst(s), Symbol))
        return Bottom
    end
    return v
end
add_tfunc(getglobal, 2, 3, getglobal_tfunc, 1)
add_tfunc(setglobal!, 3, 4, setglobal!_tfunc, 3)
@nospecs function setglobal!_nothrow(M, s, newty, o)
    global_order_nothrow(o, #=loading=#false, #=storing=#true) || return false
    return setglobal!_nothrow(M, s, newty)
end
@nospecs function setglobal!_nothrow(M, s, newty)
    if M isa Const && s isa Const
        M, s = M.val, s.val
        if isa(M, Module) && isa(s, Symbol)
            return global_assignment_nothrow(M, s, newty)
        end
    end
    return false
end

function global_assignment_nothrow(M::Module, s::Symbol, @nospecialize(newty))
    if isdefined(M, s) && !isconst(M, s)
        ty = ccall(:jl_get_binding_type, Any, (Any, Any), M, s)
        return ty === nothing || newty ⊑ ty
    end
    return false
end

@nospecs function get_binding_type_effect_free(M, s)
    if M isa Const && s isa Const
        M, s = M.val, s.val
        if M isa Module && s isa Symbol
            return ccall(:jl_get_binding_type, Any, (Any, Any), M, s) !== nothing
        end
    end
    return false
end
@nospecs function get_binding_type_tfunc(𝕃::AbstractLattice, M, s)
    if get_binding_type_effect_free(M, s)
        return Const(Core.get_binding_type((M::Const).val, (s::Const).val))
    end
    return Type
end
add_tfunc(Core.get_binding_type, 2, 2, get_binding_type_tfunc, 0)

@nospecs function get_binding_type_nothrow(𝕃::AbstractLattice, M, s)
    ⊑ = Core.Compiler.:⊑(𝕃)
    return M ⊑ Module && s ⊑ Symbol
end

# foreigncall
# ===========

# N.B. the `abstract_eval` callback below allows us to use these queries
# both during abstract interpret and optimization

const FOREIGNCALL_ARG_START = 6

function foreigncall_effects(@specialize(abstract_eval), e::Expr)
    args = e.args
    name = args[1]
    isa(name, QuoteNode) && (name = name.value)
    isa(name, Symbol) || return EFFECTS_UNKNOWN
    ndims = alloc_array_ndims(name)
    if ndims !== nothing
        if ndims ≠ 0
            return alloc_array_effects(abstract_eval, args, ndims)
        else
            return new_array_effects(abstract_eval, args)
        end
    end
    return EFFECTS_UNKNOWN
end

function alloc_array_ndims(name::Symbol)
    if name === :jl_alloc_array_1d
        return 1
    elseif name === :jl_alloc_array_2d
        return 2
    elseif name === :jl_alloc_array_3d
        return 3
    elseif name === :jl_new_array
        return 0
    end
    return nothing
end

function alloc_array_effects(@specialize(abstract_eval), args::Vector{Any}, ndims::Int)
    nothrow = alloc_array_nothrow(abstract_eval, args, ndims)
    return Effects(EFFECTS_TOTAL; consistent=CONSISTENT_IF_NOTRETURNED, nothrow)
end

function alloc_array_nothrow(@specialize(abstract_eval), args::Vector{Any}, ndims::Int)
    length(args) ≥ ndims+FOREIGNCALL_ARG_START || return false
    atype = instanceof_tfunc(abstract_eval(args[FOREIGNCALL_ARG_START]))[1]
    dims = Csize_t[]
    for i in 1:ndims
        dim = abstract_eval(args[i+FOREIGNCALL_ARG_START])
        isa(dim, Const) || return false
        dimval = dim.val
        isa(dimval, Int) || return false
        push!(dims, reinterpret(Csize_t, dimval))
    end
    return _new_array_nothrow(atype, ndims, dims)
end

function new_array_effects(@specialize(abstract_eval), args::Vector{Any})
    nothrow = new_array_nothrow(abstract_eval, args)
    return Effects(EFFECTS_TOTAL; consistent=CONSISTENT_IF_NOTRETURNED, nothrow)
end

function new_array_nothrow(@specialize(abstract_eval), args::Vector{Any})
    length(args) ≥ FOREIGNCALL_ARG_START+1 || return false
    atype = instanceof_tfunc(abstract_eval(args[FOREIGNCALL_ARG_START]))[1]
    dims = abstract_eval(args[FOREIGNCALL_ARG_START+1])
    isa(dims, Const) || return dims === Tuple{}
    dimsval = dims.val
    isa(dimsval, Tuple{Vararg{Int}}) || return false
    ndims = nfields(dimsval)
    isa(ndims, Int) || return false
    dims = Csize_t[reinterpret(Csize_t, dimval) for dimval in dimsval]
    return _new_array_nothrow(atype, ndims, dims)
end

function _new_array_nothrow(@nospecialize(atype), ndims::Int, dims::Vector{Csize_t})
    isa(atype, DataType) || return false
    eltype = atype.parameters[1]
    iskindtype(typeof(eltype)) || return false
    elsz = aligned_sizeof(eltype)
    return ccall(:jl_array_validate_dims, Cint,
        (Ptr{Csize_t}, Ptr{Csize_t}, UInt32, Ptr{Csize_t}, Csize_t),
        #=nel=#RefValue{Csize_t}(), #=tot=#RefValue{Csize_t}(), ndims, dims, elsz) == 0
end
