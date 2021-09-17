# This file is a part of Julia. License is MIT: https://julialang.org/license

#############
# constants #
#############

@nospecialize

const _NAMEDTUPLE_NAME = NamedTuple.body.body.name

const INT_INF = typemax(Int) # integer infinity

const N_IFUNC = reinterpret(Int32, arraylen) + 1
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

const DATATYPE_NAME_FIELDINDEX = fieldindex(DataType, :name)
const DATATYPE_PARAMETERS_FIELDINDEX = fieldindex(DataType, :parameters)
const DATATYPE_TYPES_FIELDINDEX = fieldindex(DataType, :types)
const DATATYPE_SUPER_FIELDINDEX = fieldindex(DataType, :super)
const DATATYPE_INSTANCE_FIELDINDEX = fieldindex(DataType, :instance)
const DATATYPE_HASH_FIELDINDEX = fieldindex(DataType, :hash)

const TYPENAME_NAME_FIELDINDEX = fieldindex(Core.TypeName, :name)
const TYPENAME_MODULE_FIELDINDEX = fieldindex(Core.TypeName, :module)
const TYPENAME_NAMES_FIELDINDEX = fieldindex(Core.TypeName, :names)
const TYPENAME_WRAPPER_FIELDINDEX = fieldindex(Core.TypeName, :wrapper)
const TYPENAME_HASH_FIELDINDEX = fieldindex(Core.TypeName, :hash)
const TYPENAME_FLAGS_FIELDINDEX = fieldindex(Core.TypeName, :flags)

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
# TODO: add @nospecialize on `f` and declare its type as `Builtin` when that's supported
function add_tfunc(f::Function, minarg::Int, maxarg::Int, @nospecialize(tfunc), cost::Int)
    push!(T_FFUNC_KEY, f)
    push!(T_FFUNC_VAL, (minarg, maxarg, tfunc))
    push!(T_FFUNC_COST, cost)
end

add_tfunc(throw, 1, 1, (@nospecialize(x)) -> Bottom, 0)

# the inverse of typeof_tfunc
# returns (type, isexact, isconcrete, istype)
# if isexact is false, the actual runtime type may (will) be a subtype of t
# if isconcrete is true, the actual runtime type is definitely concrete (unreachable if not valid as a typeof)
# if istype is true, the actual runtime value will definitely be a type (e.g. this is false for Union{Type{Int}, Int})
function instanceof_tfunc(@nospecialize(t))
    if isa(t, Const)
        if isa(t.val, Type)
            return t.val, true, isconcretetype(t.val), true
        end
        return Bottom, true, false, false # runtime throws on non-Type
    end
    t = widenconst(t)
    if t === Bottom
        return Bottom, true, true, false # runtime unreachable
    elseif t === typeof(Bottom) || typeintersect(t, Type) === Bottom
        return Bottom, true, false, false # literal Bottom or non-Type
    elseif isType(t)
        tp = t.parameters[1]
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
bitcast_tfunc(@nospecialize(t), @nospecialize(x)) = instanceof_tfunc(t)[1]
math_tfunc(@nospecialize(x)) = widenconst(x)
math_tfunc(@nospecialize(x), @nospecialize(y)) = widenconst(x)
math_tfunc(@nospecialize(x), @nospecialize(y), @nospecialize(z)) = widenconst(x)
fptoui_tfunc(@nospecialize(t), @nospecialize(x)) = bitcast_tfunc(t, x)
fptosi_tfunc(@nospecialize(t), @nospecialize(x)) = bitcast_tfunc(t, x)

    ## conversion ##
add_tfunc(bitcast, 2, 2, bitcast_tfunc, 1)
add_tfunc(sext_int, 2, 2, bitcast_tfunc, 1)
add_tfunc(zext_int, 2, 2, bitcast_tfunc, 1)
add_tfunc(trunc_int, 2, 2, bitcast_tfunc, 1)
add_tfunc(fptoui, 2, 2, fptoui_tfunc, 1)
add_tfunc(fptosi, 2, 2, fptosi_tfunc, 1)
add_tfunc(uitofp, 2, 2, bitcast_tfunc, 1)
add_tfunc(sitofp, 2, 2, bitcast_tfunc, 1)
add_tfunc(fptrunc, 2, 2, bitcast_tfunc, 1)
add_tfunc(fpext, 2, 2, bitcast_tfunc, 1)
    ## arithmetic ##
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
add_tfunc(rem_float, 2, 2, math_tfunc, 20)
add_tfunc(fma_float, 3, 3, math_tfunc, 5)
add_tfunc(muladd_float, 3, 3, math_tfunc, 5)
    ## fast arithmetic ##
add_tfunc(neg_float_fast, 1, 1, math_tfunc, 1)
add_tfunc(add_float_fast, 2, 2, math_tfunc, 1)
add_tfunc(sub_float_fast, 2, 2, math_tfunc, 1)
add_tfunc(mul_float_fast, 2, 2, math_tfunc, 2)
add_tfunc(div_float_fast, 2, 2, math_tfunc, 10)
add_tfunc(rem_float_fast, 2, 2, math_tfunc, 10)
    ## bitwise operators ##
add_tfunc(and_int, 2, 2, math_tfunc, 1)
add_tfunc(or_int, 2, 2, math_tfunc, 1)
add_tfunc(xor_int, 2, 2, math_tfunc, 1)
add_tfunc(not_int, 1, 1, math_tfunc, 0) # usually used as not_int(::Bool) to negate a condition
add_tfunc(shl_int, 2, 2, math_tfunc, 1)
add_tfunc(lshr_int, 2, 2, math_tfunc, 1)
add_tfunc(ashr_int, 2, 2, math_tfunc, 1)
add_tfunc(bswap_int, 1, 1, math_tfunc, 1)
add_tfunc(ctpop_int, 1, 1, math_tfunc, 1)
add_tfunc(ctlz_int, 1, 1, math_tfunc, 1)
add_tfunc(cttz_int, 1, 1, math_tfunc, 1)
add_tfunc(checked_sdiv_int, 2, 2, math_tfunc, 40)
add_tfunc(checked_udiv_int, 2, 2, math_tfunc, 40)
add_tfunc(checked_srem_int, 2, 2, math_tfunc, 40)
add_tfunc(checked_urem_int, 2, 2, math_tfunc, 40)
    ## functions ##
add_tfunc(abs_float, 1, 1, math_tfunc, 2)
add_tfunc(copysign_float, 2, 2, math_tfunc, 2)
add_tfunc(flipsign_int, 2, 2, math_tfunc, 1)
add_tfunc(ceil_llvm, 1, 1, math_tfunc, 10)
add_tfunc(floor_llvm, 1, 1, math_tfunc, 10)
add_tfunc(trunc_llvm, 1, 1, math_tfunc, 10)
add_tfunc(rint_llvm, 1, 1, math_tfunc, 10)
add_tfunc(sqrt_llvm, 1, 1, math_tfunc, 20)
add_tfunc(sqrt_llvm_fast, 1, 1, math_tfunc, 20)
    ## same-type comparisons ##
cmp_tfunc(@nospecialize(x), @nospecialize(y)) = Bool
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

    ## checked arithmetic ##
chk_tfunc(@nospecialize(x), @nospecialize(y)) = Tuple{widenconst(x), Bool}
add_tfunc(checked_sadd_int, 2, 2, chk_tfunc, 10)
add_tfunc(checked_uadd_int, 2, 2, chk_tfunc, 10)
add_tfunc(checked_ssub_int, 2, 2, chk_tfunc, 10)
add_tfunc(checked_usub_int, 2, 2, chk_tfunc, 10)
add_tfunc(checked_smul_int, 2, 2, chk_tfunc, 10)
add_tfunc(checked_umul_int, 2, 2, chk_tfunc, 10)
    ## other, misc intrinsics ##
add_tfunc(Core.Intrinsics.llvmcall, 3, INT_INF,
          (@nospecialize(fptr), @nospecialize(rt), @nospecialize(at), a...) -> instanceof_tfunc(rt)[1], 10)
cglobal_tfunc(@nospecialize(fptr)) = Ptr{Cvoid}
cglobal_tfunc(@nospecialize(fptr), @nospecialize(t)) = (isType(t) ? Ptr{t.parameters[1]} : Ptr)
cglobal_tfunc(@nospecialize(fptr), t::Const) = (isa(t.val, Type) ? Ptr{t.val} : Ptr)
add_tfunc(Core.Intrinsics.cglobal, 1, 2, cglobal_tfunc, 5)

function ifelse_tfunc(@nospecialize(cnd), @nospecialize(x), @nospecialize(y))
    if isa(cnd, Const)
        if cnd.val === true
            return x
        elseif cnd.val === false
            return y
        else
            return Bottom
        end
    elseif isa(cnd, Conditional)
        # optimized (if applicable) in abstract_call
    elseif !(Bool ⊑ cnd)
        return Bottom
    end
    return tmerge(x, y)
end
add_tfunc(ifelse, 3, 3, ifelse_tfunc, 1)

function egal_tfunc(@nospecialize(x), @nospecialize(y))
    xx = widenconditional(x)
    yy = widenconditional(y)
    if isa(x, Conditional) && isa(yy, Const)
        yy.val === false && return Conditional(x.var, x.elsetype, x.vtype)
        yy.val === true && return x
        return Const(false)
    elseif isa(y, Conditional) && isa(xx, Const)
        xx.val === false && return Conditional(y.var, y.elsetype, y.vtype)
        xx.val === true && return y
        return Const(false)
    elseif isa(xx, Const) && isa(yy, Const)
        return Const(xx.val === yy.val)
    elseif typeintersect(widenconst(xx), widenconst(yy)) === Bottom
        return Const(false)
    elseif (isa(xx, Const) && y === typeof(xx.val) && isdefined(y, :instance)) ||
           (isa(yy, Const) && x === typeof(yy.val) && isdefined(x, :instance))
        return Const(true)
    end
    return Bool
end
add_tfunc(===, 2, 2, egal_tfunc, 1)

function isdefined_nothrow(argtypes::Array{Any, 1})
    length(argtypes) == 2 || return false
    return typeintersect(widenconst(argtypes[1]), Module) === Union{} ?
        (argtypes[2] ⊑ Symbol || argtypes[2] ⊑ Int) :
         argtypes[2] ⊑ Symbol
end
isdefined_tfunc(arg1, sym, order) = (@nospecialize; isdefined_tfunc(arg1, sym))
function isdefined_tfunc(@nospecialize(arg1), @nospecialize(sym))
    if isa(arg1, Const)
        a1 = typeof(arg1.val)
    else
        a1 = widenconst(arg1)
    end
    if isType(a1)
        return Bool
    end
    a1 = unwrap_unionall(a1)
    if isa(a1, DataType) && !isabstracttype(a1)
        if a1 === Module
            Symbol <: widenconst(sym) || return Bottom
            if isa(sym, Const) && isa(sym.val, Symbol) && isa(arg1, Const) && isdefined(arg1.val, sym.val)
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
                if !ismutable(arg1v) || isdefined(arg1v, idx) || (isa(arg1v, DataType) && is_dt_const_field(idx))
                    return Const(isdefined(arg1v, idx))
                end
            elseif !isvatuple(a1)
                fieldT = fieldtype(a1, idx)
                if isa(fieldT, DataType) && isbitstype(fieldT)
                    return Const(true)
                end
            end
        end
    end
    return Bool
end
add_tfunc(isdefined, 2, 3, isdefined_tfunc, 1)

function sizeof_nothrow(@nospecialize(x))
    if isa(x, Const)
        if !isa(x.val, Type) || x.val === DataType
            return true
        end
    elseif isa(x, Conditional)
        return true
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
        return typeintersect(x, Type) === Union{}
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
        isinline, sz, _ = uniontype_layout(x)
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
function sizeof_tfunc(@nospecialize(x),)
    isa(x, Const) && return _const_sizeof(x.val)
    isa(x, Conditional) && return _const_sizeof(Bool)
    isconstType(x) && return _const_sizeof(x.parameters[1])
    xu = unwrap_unionall(x)
    if isa(xu, Union)
        return tmerge(sizeof_tfunc(rewrap_unionall(xu.a, x)),
                      sizeof_tfunc(rewrap_unionall(xu.b, x)))
    end
    # Core.sizeof operates on either a type or a value. First check which
    # case we're in.
    t, exact = instanceof_tfunc(x)
    if t !== Bottom
        # The value corresponding to `x` at runtime could be a type.
        # Normalize the query to ask about that type.
        x = unwrap_unionall(t)
        if exact && isa(x, Union)
            isinline, sz, _ = uniontype_layout(x)
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
function nfields_tfunc(@nospecialize(x))
    isa(x, Const) && return Const(nfields(x.val))
    isa(x, Conditional) && return Const(0)
    x = unwrap_unionall(widenconst(x))
    isconstType(x) && return Const(nfields(x.parameters[1]))
    if isa(x, DataType) && !isabstracttype(x)
        if !(x.name === Tuple.name && isvatuple(x)) &&
           !(x.name === _NAMEDTUPLE_NAME && !isconcretetype(x))
            return Const(isdefined(x, :types) ? length(x.types) : length(x.name.names))
        end
    end
    if isa(x, Union)
        na = nfields_tfunc(x.a)
        na === Int && return Int
        return tmerge(na, nfields_tfunc(x.b))
    end
    return Int
end
add_tfunc(nfields, 1, 1, nfields_tfunc, 1)
add_tfunc(Core._expr, 1, INT_INF, (@nospecialize args...)->Expr, 100)
add_tfunc(svec, 0, INT_INF, (@nospecialize args...)->SimpleVector, 20)
function typevar_tfunc(@nospecialize(n), @nospecialize(lb_arg), @nospecialize(ub_arg))
    lb = Union{}
    ub = Any
    ub_certain = lb_certain = true
    if isa(n, Const)
        isa(n.val, Symbol) || return Union{}
        if isa(lb_arg, Const)
            lb = lb_arg.val
        elseif isType(lb_arg)
            lb = lb_arg.parameters[1]
            lb_certain = false
        else
            return TypeVar
        end
        if isa(ub_arg, Const)
            ub = ub_arg.val
        elseif isType(ub_arg)
            ub = ub_arg.parameters[1]
            ub_certain = false
        else
            return TypeVar
        end
        tv = TypeVar(n.val, lb, ub)
        return PartialTypeVar(tv, lb_certain, ub_certain)
    end
    return TypeVar
end
function typebound_nothrow(b)
    b = widenconst(b)
    (b ⊑ TypeVar) && return true
    if isType(b)
        return true
    end
    return false
end
function typevar_nothrow(n, lb, ub)
    (n ⊑ Symbol) || return false
    typebound_nothrow(lb) || return false
    typebound_nothrow(ub) || return false
    return true
end
add_tfunc(Core._typevar, 3, 3, typevar_tfunc, 100)
add_tfunc(applicable, 1, INT_INF, (@nospecialize(f), args...)->Bool, 100)
add_tfunc(Core.Intrinsics.arraylen, 1, 1, @nospecialize(x)->Int, 4)
add_tfunc(arraysize, 2, 2, (@nospecialize(a), @nospecialize(d))->Int, 4)

function pointer_eltype(@nospecialize(ptr))
    a = widenconst(ptr)
    if !has_free_typevars(a)
        unw = unwrap_unionall(a)
        if isa(unw, DataType) && unw.name === Ptr.body.name
            T = unw.parameters[1]
            T isa Type && return rewrap_unionall(T, a)
        end
    end
    return Any
end
function atomic_pointermodify_tfunc(ptr, op, v, order)
    @nospecialize
    a = widenconst(ptr)
    if !has_free_typevars(a)
        unw = unwrap_unionall(a)
        if isa(unw, DataType) && unw.name === Ptr.body.name
            T = unw.parameters[1]
            # note: we could sometimes refine this to a PartialStruct if we analyzed `op(T, T)::T`
            T isa Type && return rewrap_unionall(Pair{T, T}, a)
        end
    end
    return Pair
end
function atomic_pointerreplace_tfunc(ptr, x, v, success_order, failure_order)
    @nospecialize
    a = widenconst(ptr)
    if !has_free_typevars(a)
        unw = unwrap_unionall(a)
        if isa(unw, DataType) && unw.name === Ptr.body.name
            T = unw.parameters[1]
            T isa Type && return rewrap_unionall(ccall(:jl_apply_cmpswap_type, Any, (Any,), T), a)
        end
    end
    return ccall(:jl_apply_cmpswap_type, Any, (Any,), T) where T
end
add_tfunc(pointerref, 3, 3, (a, i, align) -> (@nospecialize; pointer_eltype(a)), 4)
add_tfunc(pointerset, 4, 4, (a, v, i, align) -> (@nospecialize; a), 5)
add_tfunc(atomic_fence, 1, 1, (order) -> (@nospecialize; Nothing), 4)
add_tfunc(atomic_pointerref, 2, 2, (a, order) -> (@nospecialize; pointer_eltype(a)), 4)
add_tfunc(atomic_pointerset, 3, 3, (a, v, order) -> (@nospecialize; a), 5)
add_tfunc(atomic_pointerswap, 3, 3, (a, v, order) -> (@nospecialize; pointer_eltype(a)), 5)
add_tfunc(atomic_pointermodify, 4, 4, atomic_pointermodify_tfunc, 5)
add_tfunc(atomic_pointerreplace, 5, 5, atomic_pointerreplace_tfunc, 5)

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

function typeof_tfunc(@nospecialize(t))
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
        a = widenconst(typeof_tfunc(t.a))
        b = widenconst(typeof_tfunc(t.b))
        return Union{a, b}
    elseif isa(t, TypeVar) && !(Any === t.ub)
        return typeof_tfunc(t.ub)
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
        return rewrap_unionall(widenconst(typeof_tfunc(u)), t)
    end
    return DataType # typeof(anything)::DataType
end
add_tfunc(typeof, 1, 1, typeof_tfunc, 1)

function typeassert_tfunc(@nospecialize(v), @nospecialize(t))
    t = instanceof_tfunc(t)[1]
    t === Any && return v
    return tmeet(v, t)
end
add_tfunc(typeassert, 2, 2, typeassert_tfunc, 4)

function isa_tfunc(@nospecialize(v), @nospecialize(tt))
    t, isexact = instanceof_tfunc(tt)
    if t === Bottom
        # check if t could be equivalent to typeof(Bottom), since that's valid in `isa`, but the set of `v` is empty
        # if `t` cannot have instances, it's also invalid on the RHS of isa
        if typeintersect(widenconst(tt), Type) === Union{}
            return Union{}
        end
        return Const(false)
    end
    if !has_free_typevars(t)
        if v ⊑ t
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
            if typeintersect(v, t) === Bottom
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

function subtype_tfunc(@nospecialize(a), @nospecialize(b))
    a, isexact_a = instanceof_tfunc(a)
    b, isexact_b = instanceof_tfunc(b)
    if !has_free_typevars(a) && !has_free_typevars(b)
        if a <: b
            if isexact_b || a === Bottom
                return Const(true)
            end
        else
            if isexact_a || (b !== Bottom && typeintersect(a, b) === Union{})
                return Const(false)
            end
        end
    end
    return Bool
end
add_tfunc(<:, 2, 2, subtype_tfunc, 10)

is_dt_const_field(fld::Int) = (
     fld == DATATYPE_NAME_FIELDINDEX ||
     fld == DATATYPE_PARAMETERS_FIELDINDEX ||
     fld == DATATYPE_TYPES_FIELDINDEX ||
     fld == DATATYPE_SUPER_FIELDINDEX ||
     fld == DATATYPE_INSTANCE_FIELDINDEX ||
     fld == DATATYPE_HASH_FIELDINDEX
    )
function const_datatype_getfield_tfunc(@nospecialize(sv), fld::Int)
    if fld == DATATYPE_INSTANCE_FIELDINDEX
        return isdefined(sv, fld) ? Const(getfield(sv, fld)) : Union{}
    elseif is_dt_const_field(fld) && isdefined(sv, fld)
        return Const(getfield(sv, fld))
    end
    return nothing
end

function fieldcount_noerror(@nospecialize t)
    if t isa UnionAll || t isa Union
        t = argument_datatype(t)
        if t === nothing
            return nothing
        end
        t = t::DataType
    elseif t == Union{}
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

function getfield_nothrow(argtypes::Vector{Any})
    if length(argtypes) == 2
        boundscheck = Bool
    elseif length(argtypes) == 3
        boundscheck = argtypes[3]
        if boundscheck === Const(:not_atomic) # TODO: this is assuming not atomic
            boundscheck = Bool
        end
    elseif length(argtypes) == 4
        boundscheck = argtypes[4]
    else
        return false
    end
    widenconst(boundscheck) !== Bool && return false
    bounds_check_disabled = isa(boundscheck, Const) && boundscheck.val === false
    return getfield_nothrow(argtypes[1], argtypes[2], !bounds_check_disabled)
end
function getfield_nothrow(@nospecialize(s00), @nospecialize(name), boundscheck::Bool)
    # If we don't have boundscheck and don't know the field, don't even bother
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
            if !isa(name.val, Symbol)
                isa(sv, Module) && return false
                isa(name.val, Int) || return false
            end
            return isdefined(sv, name.val)
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
        return getfield_nothrow(rewrap(s.a, s00), name, boundscheck) &&
               getfield_nothrow(rewrap(s.b, s00), name, boundscheck)
    elseif isa(s, DataType)
        # Can't say anything about abstract types
        isabstracttype(s) && return false
        s.name.atomicfields == C_NULL || return false # TODO: currently we're only testing for ordering == :not_atomic
        # If all fields are always initialized, and bounds check is disabled, we can assume
        # we don't throw
        if !boundscheck && s.name.n_uninitialized == 0
            return true
        end
        # Else we need to know what the field is
        isa(name, Const) || return false
        field = try_compute_fieldidx(s, name.val)
        field === nothing && return false
        field <= datatype_min_ninitialized(s) && return true
        # `try_compute_fieldidx` already check for field index bound.
        !isvatuple(s) && isbitstype(fieldtype(s0, field)) && return true
    end

    return false
end

getfield_tfunc(s00, name, boundscheck_or_order) = (@nospecialize; getfield_tfunc(s00, name))
getfield_tfunc(s00, name, order, boundscheck) = (@nospecialize; getfield_tfunc(s00, name))
function getfield_tfunc(@nospecialize(s00), @nospecialize(name))
    s = unwrap_unionall(s00)
    if isa(s, Union)
        return tmerge(getfield_tfunc(rewrap(s.a,s00), name),
                      getfield_tfunc(rewrap(s.b,s00), name))
    elseif isa(s, Conditional)
        return Bottom # Bool has no fields
    elseif isa(s, Const) || isconstType(s)
        if !isa(s, Const)
            sv = s.parameters[1]
        else
            sv = s.val
        end
        if isa(name, Const)
            nv = name.val
            if !(isa(nv,Symbol) || isa(nv,Int))
                return Bottom
            end
            if isa(sv, UnionAll)
                if nv === :var || nv === 1
                    return Const(sv.var)
                elseif nv === :body || nv === 2
                    return Const(sv.body)
                end
            elseif isa(sv, DataType)
                idx = nv
                if isa(idx, Symbol)
                    idx = fieldindex(DataType, idx, false)
                end
                if isa(idx, Int)
                    t = const_datatype_getfield_tfunc(sv, idx)
                    t === nothing || return t
                end
            elseif isa(sv, Core.TypeName)
                fld = isa(nv, Symbol) ? fieldindex(Core.TypeName, nv, false) : nv
                if (fld == TYPENAME_NAME_FIELDINDEX ||
                    fld == TYPENAME_MODULE_FIELDINDEX ||
                    fld == TYPENAME_WRAPPER_FIELDINDEX ||
                    fld == TYPENAME_HASH_FIELDINDEX ||
                    fld == TYPENAME_FLAGS_FIELDINDEX ||
                    (fld == TYPENAME_NAMES_FIELDINDEX && isdefined(sv, fld)))
                    return Const(getfield(sv, fld))
                end
            end
            if isa(sv, Module) && isa(nv, Symbol)
                return abstract_eval_global(sv, nv)
            end
            if (isa(sv, SimpleVector) || !ismutable(sv)) && isdefined(sv, nv)
                return Const(getfield(sv, nv))
            end
        end
        s = typeof(sv)
    elseif isa(s00, PartialStruct)
        s = widenconst(s00)
        sty = unwrap_unionall(s)::DataType
        if isa(name, Const)
            nv = name.val
            if isa(nv, Symbol)
                nv = fieldindex(sty, nv, false)
            end
            if isa(nv, Int) && 1 <= nv <= length(s00.fields)
                return unwrapva(s00.fields[nv])
            end
        end
    end
    if isType(s) || !isa(s, DataType) || isabstracttype(s)
        return Any
    end
    s = s::DataType
    if s <: Tuple && name ⊑ Symbol
        return Bottom
    end
    if s <: Module
        if name ⊑ Int
            return Bottom
        end
        return Any
    end
    # If no value has this type, then this statement should be unreachable.
    # Bail quickly now.
    has_concrete_subtype(s) || return Union{}
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
        _ts = s.parameters[2]
        while isa(_ts, TypeVar)
            _ts = _ts.ub
        end
        _ts = rewrap_unionall(_ts, s00)
        if !(_ts <: Tuple)
            return Any
        end
        return getfield_tfunc(_ts, name)
    end
    ftypes = datatype_fieldtypes(s)
    if isempty(ftypes)
        return Bottom
    end
    if isa(name, Conditional)
        return Bottom # can't index fields with Bool
    end
    if !isa(name, Const)
        if !(Int <: name || Symbol <: name)
            return Bottom
        end
        if length(ftypes) == 1
            return rewrap_unionall(unwrapva(ftypes[1]), s00)
        end
        # union together types of all fields
        t = Bottom
        for _ft in ftypes
            t = tmerge(t, rewrap_unionall(unwrapva(_ft), s00))
            t === Any && break
        end
        return t
    end
    fld = name.val
    if isa(fld, Symbol)
        fld = fieldindex(s, fld, false)
    end
    if !isa(fld, Int)
        return Bottom
    end
    nf = length(ftypes)
    if s <: Tuple && fld >= nf && isvarargtype(ftypes[nf])
        return rewrap_unionall(unwrapva(ftypes[nf]), s00)
    end
    if fld < 1 || fld > nf
        return Bottom
    end
    if isconstType(s00)
        sp = s00.parameters[1]
    elseif isa(s00, Const)
        sp = s00.val
    else
        sp = nothing
    end
    if isa(sp, DataType)
        t = const_datatype_getfield_tfunc(sp, fld)
        t !== nothing && return t
    end
    R = ftypes[fld]
    if isempty(s.parameters)
        return R
    end
    return rewrap_unionall(R, s00)
end

setfield!_tfunc(o, f, v, order) = (@nospecialize; v)
setfield!_tfunc(o, f, v) = (@nospecialize; v)

swapfield!_tfunc(o, f, v, order) = (@nospecialize; getfield_tfunc(o, f))
swapfield!_tfunc(o, f, v) = (@nospecialize; getfield_tfunc(o, f))
modifyfield!_tfunc(o, f, op, v, order) = (@nospecialize; modifyfield!_tfunc(o, f, op, v))
function modifyfield!_tfunc(o, f, op, v)
    @nospecialize
    T = _fieldtype_tfunc(o, isconcretetype(o), f)
    T === Bottom && return Bottom
    PT = Const(Pair)
    return instanceof_tfunc(apply_type_tfunc(PT, T, T))[1]
end
function abstract_modifyfield!(interp::AbstractInterpreter, argtypes::Vector{Any}, sv::InferenceState)
    nargs = length(argtypes)
    if !isempty(argtypes) && isvarargtype(argtypes[nargs])
        nargs - 1 <= 6 || return CallMeta(Bottom, false)
        nargs > 3 || return CallMeta(Any, false)
    else
        5 <= nargs <= 6 || return CallMeta(Bottom, false)
    end
    o = unwrapva(argtypes[2])
    f = unwrapva(argtypes[3])
    RT = modifyfield!_tfunc(o, f, Any, Any)
    info = false
    if nargs >= 5 && RT !== Bottom
        # we may be able to refine this to a PartialStruct by analyzing `op(o.f, v)::T`
        # as well as compute the info for the method matches
        op = unwrapva(argtypes[4])
        v = unwrapva(argtypes[5])
        TF = getfield_tfunc(o, f)
        push!(sv.ssavalue_uses[sv.currpc], sv.currpc) # temporarily disable `call_result_unused` check for this call
        callinfo = abstract_call(interp, nothing, Any[op, TF, v], sv, #=max_methods=# 1)
        pop!(sv.ssavalue_uses[sv.currpc], sv.currpc)
        TF2 = tmeet(callinfo.rt, widenconst(TF))
        if TF2 === Bottom
            RT = Bottom
        elseif isconcretetype(RT) && has_nontrivial_const_info(TF2) # isconcrete condition required to form a PartialStruct
            RT = PartialStruct(RT, Any[TF, TF2])
        end
        info = callinfo.info
    end
    return CallMeta(RT, info)
end
replacefield!_tfunc(o, f, x, v, success_order, failure_order) = (@nospecialize; replacefield!_tfunc(o, f, x, v))
replacefield!_tfunc(o, f, x, v, success_order) = (@nospecialize; replacefield!_tfunc(o, f, x, v))
function replacefield!_tfunc(o, f, x, v)
    @nospecialize
    T = _fieldtype_tfunc(o, isconcretetype(o), f)
    T === Bottom && return Bottom
    PT = Const(ccall(:jl_apply_cmpswap_type, Any, (Any,), T) where T)
    return instanceof_tfunc(apply_type_tfunc(PT, T))[1]
end

# we could use tuple_tfunc instead of widenconst, but `o` is mutable, so that is unlikely to be beneficial

add_tfunc(getfield, 2, 4, getfield_tfunc, 1)
add_tfunc(setfield!, 3, 4, setfield!_tfunc, 3)

add_tfunc(swapfield!, 3, 4, swapfield!_tfunc, 3)
add_tfunc(modifyfield!, 4, 5, modifyfield!_tfunc, 3)
add_tfunc(replacefield!, 4, 6, replacefield!_tfunc, 3)

function fieldtype_nothrow(@nospecialize(s0), @nospecialize(name))
    s0 === Bottom && return true # unreachable
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
        return fieldtype_nothrow(rewrap_unionall(su.a, s0), name) &&
               fieldtype_nothrow(rewrap_unionall(su.b, s0), name)
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

fieldtype_tfunc(s0, name, boundscheck) = (@nospecialize; fieldtype_tfunc(s0, name))
function fieldtype_tfunc(@nospecialize(s0), @nospecialize(name))
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
        return tmerge(fieldtype_tfunc(rewrap(su.a, s0), name),
                      fieldtype_tfunc(rewrap(su.b, s0), name))
    end

    s, exact = instanceof_tfunc(s0)
    s === Bottom && return Bottom
    return _fieldtype_tfunc(s, exact, name)
end

function _fieldtype_tfunc(@nospecialize(s), exact::Bool, @nospecialize(name))
    exact = exact && !has_free_typevars(s)
    u = unwrap_unionall(s)
    if isa(u, Union)
        ta0 = _fieldtype_tfunc(rewrap(u.a, s), exact, name)
        tb0 = _fieldtype_tfunc(rewrap(u.b, s), exact, name)
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
function valid_tparam_type(T::DataType)
    T === Symbol && return true
    isbitstype(T) && return true
    if T <: Tuple
        isconcretetype(T) || return false
        for P in T.parameters
            (P === Symbol || isbitstype(P)) || return false
        end
        return true
    end
    return false
end
valid_tparam_type(U::Union) = valid_tparam_type(U.a) && valid_tparam_type(U.b)
valid_tparam_type(U::UnionAll) = valid_tparam_type(unwrap_unionall(U))

function apply_type_nothrow(argtypes::Array{Any, 1}, @nospecialize(rt))
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
    for i = 2:length(argtypes)
        isa(u, UnionAll) || return false
        ai = widenconditional(argtypes[i])
        if ai ⊑ TypeVar || ai === DataType
            # We don't know anything about the bounds of this typevar, but as
            # long as the UnionAll is not constrained, that's ok.
            if !(u.var.lb === Union{} && u.var.ub === Any)
                return false
            end
        elseif (isa(ai, Const) && isa(ai.val, Type)) || isconstType(ai)
            ai = isa(ai, Const) ? ai.val : ai.parameters[1]
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
function apply_type_tfunc(@nospecialize(headtypetype), @nospecialize args...)
    if isa(headtypetype, Const)
        headtype = headtypetype.val
    elseif isconstType(headtypetype)
        headtype = headtypetype.parameters[1]
    else
        return Any
    end
    if !isempty(args) && isvarargtype(args[end])
        return isvarargtype(headtype) ? Core.TypeofVararg : Type
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
                    if !isa(ai, Type) || typeintersect(ai, Type) !== Bottom || typeintersect(ai, TypeVar) !== Bottom
                        hasnonType = true
                    else
                        return Bottom
                    end
                end
            end
        end
        largs == 1 && return isa(args[1], Type) ? typeintersect(args[1], Type) : Type
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
    isnamedtuple = isa(uw, DataType) && uw.name === _NAMEDTUPLE_NAME
    uncertain = false
    canconst = true
    tparams = Any[]
    outervars = Any[]
    varnamectr = 1
    ua = headtype
    for i = 1:largs
        ai = widenconditional(args[i])
        if isType(ai)
            aip1 = ai.parameters[1]
            canconst &= !has_free_typevars(aip1)
            push!(tparams, aip1)
        elseif isa(ai, Const) && (isa(ai.val, Type) || isa(ai.val, TypeVar) ||
                                  valid_tparam(ai.val) || (istuple && isa(ai.val, Core.TypeofVararg)))
            push!(tparams, ai.val)
        elseif isa(ai, PartialTypeVar)
            canconst = false
            push!(tparams, ai.tv)
        else
            uncertain = true
            # These blocks improve type info but make compilation a bit slower.
            # XXX
            #unw = unwrap_unionall(ai)
            #isT = isType(unw)
            #if isT && isa(ai,UnionAll) && contains_is(outervars, ai.var)
            #    ai = rename_unionall(ai)
            #    unw = unwrap_unionall(ai)
            #end
            ai_w = widenconst(ai)
            ub = ai_w isa Type && ai_w <: Type ? instanceof_tfunc(ai)[1] : Any
            if istuple
                # in the last parameter of a Tuple type, if the upper bound is Any
                # then this could be a Vararg type.
                if i == largs && ub === Any
                    push!(tparams, Vararg)
                # XXX
                #elseif isT
                #    push!(tparams, rewrap_unionall(unw.parameters[1], ai))
                else
                    push!(tparams, Any)
                end
            # XXX
            #elseif isT
            #    push!(tparams, unw.parameters[1])
            #    while isa(ai, UnionAll)
            #        push!(outervars, ai.var)
            #        ai = ai.body
            #    end
            else
                # Is this the second parameter to a NamedTuple?
                if isnamedtuple && isa(ua, UnionAll) && uw.parameters[2] === ua.var
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
        if isa(ua, UnionAll)
            ua = ua.body
        else
            ua = nothing
        end
    end
    local appl
    try
        appl = apply_type(headtype, tparams...)
    catch ex
        # type instantiation might fail if one of the type parameters
        # doesn't match, which could happen if a type estimate is too coarse
        return isvarargtype(headtype) ? Core.TypeofVararg : Type{<:headtype}
    end
    !uncertain && canconst && return Const(appl)
    if isvarargtype(appl)
        return Core.TypeofVararg
    end
    if istuple
        return Type{<:appl}
    end
    ans = Type{appl}
    for i = length(outervars):-1:1
        ans = UnionAll(outervars[i], ans)
    end
    return ans
end
add_tfunc(apply_type, 1, INT_INF, apply_type_tfunc, 10)

function has_struct_const_info(x)
    isa(x, PartialTypeVar) && return true
    isa(x, Conditional) && return true
    return has_nontrivial_const_info(x)
end

# convert the dispatch tuple type argtype to the real (concrete) type of
# the tuple of those values
function tuple_tfunc(atypes::Vector{Any})
    atypes = anymap(widenconditional, atypes)
    all_are_const = true
    for i in 1:length(atypes)
        if !isa(atypes[i], Const)
            all_are_const = false
            break
        end
    end
    if all_are_const
        return Const(ntuple(i -> atypes[i].val, length(atypes)))
    end
    params = Vector{Any}(undef, length(atypes))
    anyinfo = false
    for i in 1:length(atypes)
        x = atypes[i]
        if has_struct_const_info(x)
            anyinfo = true
        else
            atypes[i] = x = widenconst(x)
        end
        if isa(x, Const)
            params[i] = typeof(x.val)
        else
            x = widenconst(x)
            if isType(x)
                anyinfo = true
                xparam = x.parameters[1]
                if hasuniquerep(xparam) || xparam === Bottom
                    params[i] = typeof(xparam)
                else
                    params[i] = Type
                end
            else
                params[i] = x
            end
        end
    end
    typ = Tuple{params...}
    # replace a singleton type with its equivalent Const object
    isdefined(typ, :instance) && return Const(typ.instance)
    return anyinfo ? PartialStruct(typ, atypes) : typ
end

function arrayref_tfunc(@nospecialize(boundscheck), @nospecialize(a), @nospecialize i...)
    a = widenconst(a)
    if a <: Array
        if isa(a, DataType) && (isa(a.parameters[1], Type) || isa(a.parameters[1], TypeVar))
            # TODO: the TypeVar case should not be needed here
            a = a.parameters[1]
            return isa(a, TypeVar) ? a.ub : a
        elseif isa(a, UnionAll) && !has_free_typevars(a)
            unw = unwrap_unionall(a)
            if isa(unw, DataType)
                return rewrap_unionall(unw.parameters[1], a)
            end
        end
    end
    return Any
end
add_tfunc(arrayref, 3, INT_INF, arrayref_tfunc, 20)
add_tfunc(const_arrayref, 3, INT_INF, arrayref_tfunc, 20)
add_tfunc(arrayset, 4, INT_INF, (@nospecialize(boundscheck), @nospecialize(a), @nospecialize(v), @nospecialize i...)->a, 20)

function _opaque_closure_tfunc(@nospecialize(arg), @nospecialize(isva),
        @nospecialize(lb), @nospecialize(ub), @nospecialize(source), env::Vector{Any},
        linfo::MethodInstance)

    argt, argt_exact = instanceof_tfunc(arg)
    lbt, lb_exact = instanceof_tfunc(lb)
    if !lb_exact
        lbt = Union{}
    end

    ubt, ub_exact = instanceof_tfunc(ub)

    t = (argt_exact ? Core.OpaqueClosure{argt, T} : Core.OpaqueClosure{<:argt, T}) where T
    t = lbt == ubt ? t{ubt} : (t{T} where lbt <: T <: ubt)

    (isa(source, Const) && isa(source.val, Method)) || return t
    (isa(isva, Const) && isa(isva.val, Bool)) || return t

    return PartialOpaque(t, tuple_tfunc(env), isva.val, linfo, source.val)
end

function array_type_undefable(@nospecialize(a))
    if isa(a, Union)
        return array_type_undefable(a.a) || array_type_undefable(a.b)
    elseif isa(a, UnionAll)
        return true
    else
        etype = (a::DataType).parameters[1]
        return !(etype isa Type && (isbitstype(etype) || isbitsunion(etype)))
    end
end

function array_builtin_common_nothrow(argtypes::Array{Any,1}, first_idx_idx::Int)
    length(argtypes) >= 4 || return false
    atype = argtypes[2]
    (argtypes[1] ⊑ Bool && atype ⊑ Array) || return false
    for i = first_idx_idx:length(argtypes)
        argtypes[i] ⊑ Int || return false
    end
    # If we could potentially throw undef ref errors, bail out now.
    atype = widenconst(atype)
    array_type_undefable(atype) && return false
    # If we have @inbounds (first argument is false), we're allowed to assume
    # we don't throw bounds errors.
    boundcheck = argtypes[1]
    if isa(boundcheck, Const)
        !(boundcheck.val::Bool) && return true
    end
    # Else we can't really say anything here
    # TODO: In the future we may be able to track the shapes of arrays though
    # inference.
    return false
end

# Query whether the given builtin is guaranteed not to throw given the argtypes
function _builtin_nothrow(@nospecialize(f), argtypes::Array{Any,1}, @nospecialize(rt))
    if f === arrayset
        array_builtin_common_nothrow(argtypes, 4) || return true
        # Additionally check element type compatibility
        a = widenconst(argtypes[2])
        # Check that we can determine the element type
        (isa(a, DataType) && isa(a.parameters[1], Type)) || return false
        # Check that the element type is compatible with the element we're assigning
        (argtypes[3] ⊑ a.parameters[1]::Type) || return false
        return true
    elseif f === arrayref || f === const_arrayref
        return array_builtin_common_nothrow(argtypes, 3)
    elseif f === Core._expr
        length(argtypes) >= 1 || return false
        return argtypes[1] ⊑ Symbol
    elseif f === Core._typevar
        length(argtypes) == 3 || return false
        return typevar_nothrow(argtypes[1], argtypes[2], argtypes[3])
    elseif f === invoke
        return false
    elseif f === getfield
        return getfield_nothrow(argtypes)
    elseif f === fieldtype
        length(argtypes) == 2 || return false
        return fieldtype_nothrow(argtypes[1], argtypes[2])
    elseif f === apply_type
        return apply_type_nothrow(argtypes, rt)
    elseif f === isa
        length(argtypes) == 2 || return false
        return argtypes[2] ⊑ Type
    elseif f === (<:)
        length(argtypes) == 2 || return false
        return argtypes[1] ⊑ Type && argtypes[2] ⊑ Type
    elseif f === UnionAll
        return length(argtypes) == 2 &&
            (argtypes[1] ⊑ TypeVar && argtypes[2] ⊑ Type)
    elseif f === isdefined
        return isdefined_nothrow(argtypes)
    elseif f === Core.sizeof
        length(argtypes) == 1 || return false
        return sizeof_nothrow(argtypes[1])
    elseif f === Core.kwfunc
        length(argtypes) == 1 || return false
        return isa(rt, Const)
    elseif f === Core.ifelse
        length(argtypes) == 3 || return false
        return argtypes[1] ⊑ Bool
    end
    return false
end

function builtin_nothrow(@nospecialize(f), argtypes::Array{Any, 1}, @nospecialize(rt))
    rt === Bottom && return false
    contains_is(_PURE_BUILTINS, f) && return true
    return _builtin_nothrow(f, argtypes, rt)
end

function builtin_tfunction(interp::AbstractInterpreter, @nospecialize(f), argtypes::Array{Any,1},
                           sv::Union{InferenceState,Nothing})
    if f === tuple
        return tuple_tfunc(argtypes)
    end
    if isa(f, IntrinsicFunction)
        if is_pure_intrinsic_infer(f) && _all(@nospecialize(a) -> isa(a, Const), argtypes)
            argvals = anymap(a::Const -> a.val, argtypes)
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
            vatype = argtypes[end]::Core.TypeofVararg
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
    return tf[3](argtypes...)
end

# Query whether the given intrinsic is nothrow

_iszero(x) = x === Intrinsics.xor_int(x, x)
_isneg1(x) = _iszero(Intrinsics.not_int(x))
_istypemin(x) = !_iszero(x) && Intrinsics.neg_int(x) === x

function intrinsic_nothrow(f::IntrinsicFunction, argtypes::Array{Any, 1})
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
        isa(argtypes[2], Const) || return false
        if !isprimitivetype(widenconst(argtypes[1])) ||
           (widenconst(argtypes[1]) !== widenconst(argtypes[2]))
            return false
        end
        den_val = argtypes[2].val
        _iszero(den_val) && return false
        f !== Intrinsics.checked_sdiv_int && return true
        # Nothrow as long as we additionally don't do typemin(T)/-1
        return !_isneg1(den_val) || (isa(argtypes[1], Const) && !_istypemin(argtypes[1].val))
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

# TODO: this function is a very buggy and poor model of the return_type function
# since abstract_call_gf_by_type is a very inaccurate model of _method and of typeinf_type,
# while this assumes that it is an absolutely precise and accurate and exact model of both
function return_type_tfunc(interp::AbstractInterpreter, argtypes::Vector{Any}, sv::InferenceState)
    if length(argtypes) == 3
        tt = argtypes[3]
        if isa(tt, Const) || (isType(tt) && !has_free_typevars(tt))
            aft = argtypes[2]
            if isa(aft, Const) || (isType(aft) && !has_free_typevars(aft)) ||
                   (isconcretetype(aft) && !(aft <: Builtin))
                af_argtype = isa(tt, Const) ? tt.val : (tt::DataType).parameters[1]
                if isa(af_argtype, DataType) && af_argtype <: Tuple
                    argtypes_vec = Any[aft, af_argtype.parameters...]
                    if contains_is(argtypes_vec, Union{})
                        return CallMeta(Const(Union{}), false)
                    end
                    call = abstract_call(interp, nothing, argtypes_vec, sv, -1)
                    info = verbose_stmt_info(interp) ? ReturnTypeCallInfo(call.info) : false
                    rt = widenconditional(call.rt)
                    if isa(rt, Const)
                        # output was computed to be constant
                        return CallMeta(Const(typeof(rt.val)), info)
                    end
                    rt = widenconst(rt)
                    if rt === Bottom || (isconcretetype(rt) && !iskindtype(rt))
                        # output cannot be improved so it is known for certain
                        return CallMeta(Const(rt), info)
                    elseif !isempty(sv.pclimitations)
                        # conservatively express uncertainty of this result
                        # in two ways: both as being a subtype of this, and
                        # because of LimitedAccuracy causes
                        return CallMeta(Type{<:rt}, info)
                    elseif (isa(tt, Const) || isconstType(tt)) &&
                        (isa(aft, Const) || isconstType(aft))
                        # input arguments were known for certain
                        # XXX: this doesn't imply we know anything about rt
                        return CallMeta(Const(rt), info)
                    elseif isType(rt)
                        return CallMeta(Type{rt}, info)
                    else
                        return CallMeta(Type{<:rt}, info)
                    end
                end
            end
        end
    end
    return CallMeta(Type, false)
end

# N.B.: typename maps type equivalence classes to a single value
function typename_static(@nospecialize(t))
    t isa Const && return _typename(t.val)
    t isa Conditional && return Bool.name
    t = unwrap_unionall(widenconst(t))
    return isType(t) ? _typename(t.parameters[1]) : Core.TypeName
end

@specialize
