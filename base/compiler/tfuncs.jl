# This file is a part of Julia. License is MIT: https://julialang.org/license

#############
# constants #
#############

@nospecialize

const _NAMEDTUPLE_NAME = NamedTuple.body.body.name

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

add_tfunc(throw, 1, 1, x::LatticeElement -> ⊥, 0)

# the inverse of typeof_tfunc
# returns (type, isexact, isconcrete, istype)
# if isexact is false, the actual runtime type may (will) be a subtype of t
# if isconcrete is true, the actual runtime type is definitely concrete (unreachable if not valid as a typeof)
# if istype is true, the actual runtime value will definitely be a type (e.g. this is false for Union{Type{Int}, Int})
function instanceof_tfunc(t::LatticeElement)
    if isConst(t)
        tval = constant(t)
        if isa(tval, Type) && valid_as_lattice(tval)
            return tval, true, isconcretetype(tval), true
        end
        return Bottom, true, false, false # runtime throws on non-Type
    end
    return _instanceof_tfunc(widenconst(t))
end
function _instanceof_tfunc(@nospecialize(t#=::Type=#))
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
        t′′, isexact, isconcrete, istype = _instanceof_tfunc(t′)
        tr = rewrap_unionall(t′′, t)
        if t′′ isa DataType && t′′.name !== Tuple.name && !has_free_typevars(tr)
            # a real instance must be within the declared bounds of the type,
            # so we can intersect with the original wrapper.
            tr = typeintersect(tr, t′′.name.wrapper)
            isconcrete = !isabstracttype(t′′)
            if tr === Bottom
                # runtime unreachable (our inference Type{T} where S is
                # uninhabited with any runtime T that exists)
                isexact = true
            end
        end
        return tr, isexact, isconcrete, istype
    elseif isa(t, Union)
        ta, isexact_a, isconcrete_a, istype_a = _instanceof_tfunc(t.a)
        tb, isexact_b, isconcrete_b, istype_b = _instanceof_tfunc(t.b)
        isconcrete = isconcrete_a && isconcrete_b
        istype = istype_a && istype_b
        # most users already handle the Union case, so here we assume that
        # `isexact` only cares about the answers where there's actually a Type
        # (and assuming other cases causing runtime errors)
        ta === Bottom && return tb, isexact_b, isconcrete, istype
        tb === Bottom && return ta, isexact_a, isconcrete, istype
        return Union{ta, tb}, false, isconcrete, istype # at runtime, will be exactly one of these
    end
    return Any, false, false, false
end
bitcast_tfunc(t::LatticeElement, x::LatticeElement) = NativeType(instanceof_tfunc(t)[1])
math_tfunc(x::LatticeElement) = NativeType(widenconst(x))
math_tfunc(x::LatticeElement, y::LatticeElement) = math_tfunc(x)
math_tfunc(x::LatticeElement, y::LatticeElement, z::LatticeElement) = math_tfunc(x)
fptoui_tfunc(t::LatticeElement, x::LatticeElement) = bitcast_tfunc(t, x)
fptosi_tfunc(t::LatticeElement, x::LatticeElement) = bitcast_tfunc(t, x)

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
cmp_tfunc(x::LatticeElement, y::LatticeElement) = LBool
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
chk_tfunc(x::LatticeElement, y::LatticeElement) = NativeType(Tuple{widenconst(x), Bool})
add_tfunc(checked_sadd_int, 2, 2, chk_tfunc, 10)
add_tfunc(checked_uadd_int, 2, 2, chk_tfunc, 10)
add_tfunc(checked_ssub_int, 2, 2, chk_tfunc, 10)
add_tfunc(checked_usub_int, 2, 2, chk_tfunc, 10)
add_tfunc(checked_smul_int, 2, 2, chk_tfunc, 10)
add_tfunc(checked_umul_int, 2, 2, chk_tfunc, 10)

    ## other, misc intrinsics ##
function llvmcall_tfunc(fptr::LatticeElement, rt::LatticeElement, at::LatticeElement,
    @nospecialize a::LatticeElement...)
    return NativeType(instanceof_tfunc(rt)[1])
end
add_tfunc(llvmcall, 3, INT_INF, llvmcall_tfunc, 10)
cglobal_tfunc(fptr::LatticeElement) = NativeType(Ptr{Cvoid})
function cglobal_tfunc(fptr::LatticeElement, t::LatticeElement)
    if isConst(t)
        tv = constant(t)
        return NativeType(isa(tv, Type) ? Ptr{tv} : Ptr)
    elseif isVararg(t)
        return NativeType(Ptr)
    end
    t = widenconst(t)
    return NativeType(isType(t) ? Ptr{t.parameters[1]} : Ptr)
end
add_tfunc(cglobal, 1, 2, cglobal_tfunc, 5)
add_tfunc(have_fma, 1, 1, x::LatticeElement->LBool, 1)

function ifelse_tfunc(cnd::LatticeElement, x::LatticeElement, y::LatticeElement)
    if isConst(cnd)
        if constant(cnd) === true
            return x
        elseif constant(cnd) === false
            return y
        else
            return ⊥
        end
    elseif isConditional(cnd)
        # optimized (if applicable) in abstract_call
    elseif !(Bool ⊑ cnd)
        return ⊥
    end
    return tmerge(x, y)
end
add_tfunc(Core.ifelse, 3, 3, ifelse_tfunc, 1)

function egal_tfunc(x::LatticeElement, y::LatticeElement)
    if isConditional(x) && isConst(y)
        cnd = conditional(x)
        constant(y) === false && return Conditional(cnd.slot_id, cnd.elsetype, cnd.vtype)
        constant(y) === true && return x
        return Const(false)
    elseif isConditional(y) && isConst(x)
        cnd = conditional(y)
        constant(x) === false && return Conditional(cnd.slot_id, cnd.elsetype, cnd.vtype)
        constant(x) === true && return y
        return Const(false)
    elseif isConst(x) && isConst(y)
        return Const(constant(x) === constant(y))
    elseif !hasintersect(widenconst(x), widenconst(y))
        return Const(false)
    elseif (isConst(x) && y === typeof(constant(x)) && isdefined(y, :instance)) ||
           (isConst(y) && x === typeof(constant(y)) && isdefined(x, :instance))
        return Const(true)
    end
    return LBool
end
add_tfunc(===, 2, 2, egal_tfunc, 1)

function isdefined_nothrow(argtypes::Vector{LatticeElement})
    length(argtypes) == 2 || return false
    return hasintersect(widenconst(argtypes[1]), Module) ?
           argtypes[2] ⊑ₜ Symbol :
           (argtypes[2] ⊑ₜ Symbol || argtypes[2] ⊑ₜ Int)
end
isdefined_tfunc(arg1::LatticeElement, sym::LatticeElement, order::LatticeElement) = isdefined_tfunc(arg1, sym)
function isdefined_tfunc(arg1::LatticeElement, sym::LatticeElement)
    if isConst(arg1)
        a1 = typeof(constant(arg1))
    else
        a1 = widenconst(arg1)
    end
    if isType(a1)
        return LBool
    end
    a1 = unwrap_unionall(a1)
    if isa(a1, DataType) && !isabstracttype(a1)
        if a1 === Module
            hasintersect(widenconst(sym), Symbol) || return ⊥
            if isConst(sym) && isa(constant(sym), Symbol) && isConst(arg1) && isdefined(constant(arg1), constant(sym)::Symbol)
                return Const(true)
            end
        elseif isConst(sym)
            val = constant(sym)
            if isa(val, Symbol)
                idx = fieldindex(a1, val, false)::Int
            elseif isa(val, Int)
                idx = val
            else
                return ⊥
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
            elseif isConst(arg1)
                arg1v = constant(arg1)
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
        return tmerge(isdefined_tfunc(NativeType(a1.a), sym),
                      isdefined_tfunc(NativeType(a1.b), sym))
    end
    return LBool
end
add_tfunc(isdefined, 2, 3, isdefined_tfunc, 1)

function sizeof_nothrow(x::LatticeElement)
    if isConst(x)
        val = constant(x)
        if !isa(val, Type) || val === DataType
            return true
        end
    elseif isConditional(x)
        return true
    end
    return _sizeof_nothrow(widenconst(x))
end
function _sizeof_nothrow(@nospecialize(x#=::Type=#))
    xu = unwrap_unionall(x)
    if isa(xu, Union)
        return _sizeof_nothrow(rewrap_unionall(xu.a, x)) &&
               _sizeof_nothrow(rewrap_unionall(xu.b, x))
    end
    t, exact, isconcrete = _instanceof_tfunc(x)
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
    isa(x, Vector) && return LInt
    size = try
            Core.sizeof(x)
        catch ex
            # Might return
            # "argument is an abstract type; size is indeterminate" or
            # "type does not have a fixed size"
            isa(ex, ErrorException) || rethrow()
            return LInt
        end
    return Const(size)
end
function sizeof_tfunc(x::LatticeElement)
    isConst(x) && return _const_sizeof(constant(x))
    isConditional(x) && return _const_sizeof(Bool)
    return _sizeof_tfunc(widenconst(x))
end
function _sizeof_tfunc(@nospecialize x#=::Type=#)
    isconstType(x) && return _const_sizeof(x.parameters[1])
    xu = unwrap_unionall(x)
    if isa(xu, Union)
        return tmerge(_sizeof_tfunc(rewrap_unionall(xu.a, x)),
                      _sizeof_tfunc(rewrap_unionall(xu.b, x)))
    end
    # Core.sizeof operates on either a type or a value. First check which
    # case we're in.
    t, exact = _instanceof_tfunc(x)
    if t !== Bottom
        # The value corresponding to `x` at runtime could be a type.
        # Normalize the query to ask about that type.
        x = unwrap_unionall(t)
        if exact && isa(x, Union)
            isinline = uniontype_layout(x)[1]
            return isinline ? Const(Int(Core.sizeof(x))) : ⊥
        end
        isa(x, DataType) || return LInt
        (isconcretetype(x) || isprimitivetype(x)) && return _const_sizeof(x)
    else
        x !== DataType && isconcretetype(x) && return _const_sizeof(x)
        isprimitivetype(x) && return _const_sizeof(x)
    end
    return LInt
end
add_tfunc(Core.sizeof, 1, 1, sizeof_tfunc, 1)
function nfields_tfunc(x::LatticeElement)
    isConst(x) && return Const(nfields(constant(x)))
    isConditional(x) && return Const(0)
    return _nfields_tfunc(widenconst(x))
end
function _nfields_tfunc(@nospecialize(x#=::Type=#))
    x = unwrap_unionall(x)
    isconstType(x) && return Const(nfields(x.parameters[1]))
    if isa(x, DataType) && !isabstracttype(x)
        if !(x.name === Tuple.name && isvatuple(x)) &&
           !(x.name === _NAMEDTUPLE_NAME && !isconcretetype(x))
            return Const(isdefined(x, :types) ? length(x.types) : length(x.name.names))
        end
    end
    if isa(x, Union)
        na = _nfields_tfunc(x.a)
        na === LInt && return na
        nb = _nfields_tfunc(x.b)
        nb === LInt && return nb
        return tmerge(na, nb)
    end
    return LInt
end
add_tfunc(nfields, 1, 1, nfields_tfunc, 1)
add_tfunc(Core._expr, 1, INT_INF, (@nospecialize args::LatticeElement...)->NativeType(Expr), 100)
add_tfunc(svec, 0, INT_INF, (@nospecialize args::LatticeElement...)->NativeType(SimpleVector), 20)
function typevar_tfunc(n::LatticeElement, lb_arg::LatticeElement, ub_arg::LatticeElement)
    lb = Bottom
    ub = Any
    ub_certain = lb_certain = true
    if isConst(n)
        nval = constant(n)
        isa(nval, Symbol) || return ⊥
        if isConst(lb_arg)
            lb = constant(lb_arg)
        else
            lb_arg = widenconst(lb_arg)
            if isType(lb_arg)
                lb = lb_arg.parameters[1]
                lb_certain = false
            else
                return NativeType(TypeVar)
            end
        end
        if isConst(ub_arg)
            ub = constant(ub_arg)
        else
            ub_arg = widenconst(ub_arg)
            if isType(ub_arg)
                ub = ub_arg.parameters[1]
                ub_certain = false
            else
                return NativeType(TypeVar)
            end
        end
        tv = TypeVar(nval, lb, ub)
        return PartialTypeVar(tv, lb_certain, ub_certain)
    end
    return NativeType(TypeVar)
end
function typebound_nothrow(b::LatticeElement)
    b = widenconst(b)
    b <: TypeVar && return true
    if isType(b)
        return true
    end
    return false
end
function typevar_nothrow(n::LatticeElement, lb::LatticeElement, ub::LatticeElement)
    n ⊑ₜ Symbol || return false
    typebound_nothrow(lb) || return false
    typebound_nothrow(ub) || return false
    return true
end
add_tfunc(Core._typevar, 3, 3, typevar_tfunc, 100)
add_tfunc(applicable, 1, INT_INF, (f::LatticeElement, @nospecialize args::LatticeElement...)->LBool, 100)
add_tfunc(arraylen, 1, 1, x::LatticeElement->LInt, 4)

function arraysize_tfunc(ary::LatticeElement, dim::LatticeElement)
    hasintersect(widenconst(ary), Array) || return ⊥
    hasintersect(widenconst(dim), Int) || return ⊥
    return LInt
end
add_tfunc(arraysize, 2, 2, arraysize_tfunc, 4)

function arraysize_nothrow(argtypes::Vector{LatticeElement})
    length(argtypes) == 2 || return false
    ary = argtypes[1]
    dim = argtypes[2]
    ary ⊑ₜ Array || return false
    if isConst(dim)
        dimval = constant(dim)
        return isa(dimval, Int) && dimval > 0
    end
    return false
end

function pointer_eltype(ptr::LatticeElement)
    a = widenconst(ptr)
    if !has_free_typevars(a)
        unw = unwrap_unionall(a)
        if isa(unw, DataType) && unw.name === Ptr.body.name
            T = unw.parameters[1]
            valid_as_lattice(T) || return ⊥
            return NativeType(rewrap_unionall(T, a))
        end
    end
    return ⊤
end
function atomic_pointermodify_tfunc(ptr::LatticeElement,
    op::LatticeElement, v::LatticeElement, order::LatticeElement)
    a = widenconst(ptr)
    if !has_free_typevars(a)
        unw = unwrap_unionall(a)
        if isa(unw, DataType) && unw.name === Ptr.body.name
            T = unw.parameters[1]
            # note: we could sometimes refine this to a PartialStruct if we analyzed `op(T, T)::T`
            valid_as_lattice(T) || return ⊥
            return NativeType(rewrap_unionall(Pair{T, T}, a))
        end
    end
    return NativeType(Pair)
end
function atomic_pointerreplace_tfunc(ptr::LatticeElement,
    x::LatticeElement, v::LatticeElement, success_order::LatticeElement, failure_order::LatticeElement)
    a = widenconst(ptr)
    if !has_free_typevars(a)
        unw = unwrap_unionall(a)
        if isa(unw, DataType) && unw.name === Ptr.body.name
            T = unw.parameters[1]
            valid_as_lattice(T) || return ⊥
            return NativeType(rewrap_unionall(ccall(:jl_apply_cmpswap_type, Any, (Any,), T), a))
        end
    end
    return NativeType(ccall(:jl_apply_cmpswap_type, Any, (Any,), T) where T)
end
add_tfunc(pointerref, 3, 3, (a::LatticeElement, i::LatticeElement, align::LatticeElement) -> pointer_eltype(a), 4)
add_tfunc(pointerset, 4, 4, (a::LatticeElement, v::LatticeElement, i::LatticeElement, align::LatticeElement) -> a, 5)
add_tfunc(atomic_fence, 1, 1, (order::LatticeElement) -> LNothing, 4)
add_tfunc(atomic_pointerref, 2, 2, (a::LatticeElement, order::LatticeElement) -> pointer_eltype(a), 4)
add_tfunc(atomic_pointerset, 3, 3, (a::LatticeElement, v::LatticeElement, order::LatticeElement) -> a, 5)
add_tfunc(atomic_pointerswap, 3, 3, (a::LatticeElement, v::LatticeElement, order::LatticeElement) -> pointer_eltype(a), 5)
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

function typeof_tfunc(t::LatticeElement)
    isConst(t) && return Const(typeof(constant(t)))
    return __typeof_tfunc(widenconst(t))
end
function __typeof_tfunc(@nospecialize(t#=::Type=#))
    if isType(t)
        tp = t.parameters[1]
        if hasuniquerep(tp)
            return Const(typeof(tp))
        end
    elseif isa(t, DataType)
        if isconcretetype(t)
            return Const(t)
        elseif t === Any
            return NativeType(DataType)
        else
            if t.name === Tuple.name
                tt = typeof_concrete_vararg(t)
                tt === nothing || return NativeType(tt)
            end
            return NativeType(Type{<:t})
        end
    elseif isa(t, Union)
        a = widenconst(_typeof_tfunc(t.a))
        b = widenconst(_typeof_tfunc(t.b))
        return NativeType(Union{a, b})
    elseif isa(t, UnionAll)
        u = unwrap_unionall(t)
        if isa(u, DataType) && !isabstracttype(u)
            if u.name === Tuple.name
                uu = typeof_concrete_vararg(u)
                if uu !== nothing
                    return NativeType(rewrap_unionall(uu, t))
                end
            else
                return NativeType(rewrap_unionall(Type{u}, t))
            end
        end
        return NativeType(rewrap_unionall(widenconst(__typeof_tfunc(u)), t))
    end
    return NativeType(DataType) # typeof(anything)::DataType
end
# helper function of `typeof_tfunc`, which accepts `TypeVar`
function _typeof_tfunc(@nospecialize(t#=::Type=#))
    if isa(t, TypeVar)
        return t.ub !== Any ? _typeof_tfunc(t.ub) : NativeType(DataType)
    end
    return __typeof_tfunc(t)
end
add_tfunc(typeof, 1, 1, typeof_tfunc, 1)

function typeassert_tfunc(v::LatticeElement, t::LatticeElement)
    t = instanceof_tfunc(t)[1]
    t === Any && return v
    return v ⊓ t
end
add_tfunc(typeassert, 2, 2, typeassert_tfunc, 4)

function isa_tfunc(v::LatticeElement, tt::LatticeElement)
    t, isexact = instanceof_tfunc(tt)
    if t === Bottom
        # check if t could be equivalent to typeof(Bottom), since that's valid in `isa`, but the set of `v` is empty
        # if `t` cannot have instances, it's also invalid on the RHS of isa
        hasintersect(widenconst(tt), Type) || return ⊥
        return Const(false)
    end
    if !has_free_typevars(t)
        if v ⊑ t
            if isexact && isnotbrokensubtype(widenconst(v), t)
                return Const(true)
            end
        else
            if isConst(v) || isConditional(v)
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
    return LBool
end
add_tfunc(isa, 2, 2, isa_tfunc, 1)

function subtype_tfunc(a::LatticeElement, b::LatticeElement)
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
    return LBool
end
add_tfunc(<:, 2, 2, subtype_tfunc, 10)

function fieldcount_noerror(@nospecialize t)
    if t isa UnionAll || t isa Union
        t = argument_datatype(t)
        if t === nothing
            return nothing
        end
        t = t::DataType
    elseif t === Bottom
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

function getfield_nothrow(argtypes::Vector{LatticeElement})
    if length(argtypes) == 2
        boundscheck = LBool
    elseif length(argtypes) == 3
        boundscheck = argtypes[3]
        if isConst(boundscheck) && constant(boundscheck) === :not_atomic # TODO: this is assuming not atomic
            boundscheck = LBool
        end
    elseif length(argtypes) == 4
        boundscheck = argtypes[4]
    else
        return false
    end
    widenconst(boundscheck) !== Bool && return false
    bounds_check_disabled = isConst(boundscheck) && constant(boundscheck) === false
    return getfield_nothrow(argtypes[1], argtypes[2], !bounds_check_disabled)
end
function getfield_nothrow(s00::LatticeElement, name::LatticeElement, boundscheck::Bool)
    # If we don't have boundscheck and don't know the field, don't even bother
    if boundscheck
        isConst(name) || return false
    end
    # If we have s00 being a const, we can potentially refine our type-based analysis above
    if isConst(s00) || isconstType(widenconst(s00))
        if !isConst(s00)
            sv = (widenconst(s00)::DataType).parameters[1]
        else
            sv = constant(s00)
        end
        if isConst(name)
            nval = constant(name)
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
    return _getfield_nothrow(widenconst(s00), name, boundscheck)
end
function _getfield_nothrow(@nospecialize(s0#=::Type=#), name::LatticeElement, boundscheck::Bool)
    s = unwrap_unionall(s0)
    if isa(s, Union)
        return _getfield_nothrow(rewrap_unionall(s.a, s0), name, boundscheck) &&
               _getfield_nothrow(rewrap_unionall(s.b, s0), name, boundscheck)
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
        isConst(name) || return false
        field = try_compute_fieldidx(s, constant(name))
        field === nothing && return false
        field <= datatype_min_ninitialized(s) && return true
        # `try_compute_fieldidx` already check for field index bound.
        !isvatuple(s) && isbitstype(fieldtype(s0, field)) && return true
    end
    return false
end

function getfield_tfunc(s00::LatticeElement, name::LatticeElement, boundscheck_or_order::LatticeElement)
    t = isVararg(boundscheck_or_order) ? unwrapva(vararg(boundscheck_or_order)) :
        widenconst(boundscheck_or_order)
    hasintersect(t, Symbol) || hasintersect(t, Bool) || return ⊥
    return getfield_tfunc(s00, name)
end
function getfield_tfunc(s00::LatticeElement, name::LatticeElement, order::LatticeElement, boundscheck::LatticeElement)
    hasintersect(widenconst(order), Symbol) || return ⊥
    if isVararg(boundscheck)
        t = unwrapva(vararg(boundscheck))
        hasintersect(t, Symbol) || hasintersect(t, Bool) || return ⊥
    else
        hasintersect(widenconst(boundscheck), Bool) || return ⊥
    end
    return getfield_tfunc(s00, name)
end
getfield_tfunc(s00::LatticeElement, name::LatticeElement) = _getfield_tfunc(s00, name, false)
function _getfield_tfunc(s00::LatticeElement, name::LatticeElement, setfield::Bool)
    s0 = widenconst(s00)
    if isConditional(s00)
        return ⊥ # Bool has no fields
    elseif isConst(s00) || isconstType(s0)
        if !isConst(s00)
            sv = (s0::DataType).parameters[1]
        else
            sv = constant(s00)
        end
        if isConst(name)
            nv = constant(name)
            if isa(sv, Module)
                setfield && return Bottom
                if isa(nv, Symbol)
                    return abstract_eval_global(sv, nv)
                end
                return ⊥
            end
            if isa(nv, Symbol)
                nv = fieldindex(typeof(sv), nv, false)
            end
            if !isa(nv, Int)
                return ⊥
            end
            if isa(sv, DataType) && nv == DATATYPE_TYPES_FIELDINDEX && isdefined(sv, nv)
                return Const(getfield(sv, nv))
            end
            if isconst(typeof(sv), nv)
                if isdefined(sv, nv)
                    return Const(getfield(sv, nv))
                end
                return ⊥
            end
        end
    elseif isPartialStruct(s00)
        if isConst(name)
            nv = constant(name)
            if isa(nv, Symbol)
                nv = fieldindex(s0::DataType, nv, false)
            end
            if isa(nv, Int) && 1 <= nv <= length(partialfields(s00))
                return unwrapva_𝑳(partialfields(s00)[nv])
            end
        end
    end
    return __getfield_tfunc(s0, name, setfield)
end
function __getfield_tfunc(@nospecialize(s0#=::Type=#), name::LatticeElement, setfield::Bool)
    s = unwrap_unionall(s0)
    if isa(s, Union)
        return tmerge(__getfield_tfunc(rewrap_unionall(s.a, s0), name, setfield),
                      __getfield_tfunc(rewrap_unionall(s.b, s0), name, setfield))
    end
    isa(s, DataType) || return ⊤
    isabstracttype(s) && return ⊤
    if s <: Tuple
        hasintersect(widenconst(name), Int) || return ⊥
    elseif s <: Module
        setfield && return ⊥
        hasintersect(widenconst(name), Symbol) || return ⊥
        return ⊤
    end
    if s.name === _NAMEDTUPLE_NAME && !isconcretetype(s)
        if isConst(name) && isa(constant(name), Symbol)
            if isa(s.parameters[1], Tuple)
                name = Const(Int(ccall(:jl_field_index, Cint, (Any, Any, Cint), s, constant(name), false)+1))
            else
                name = LInt
            end
        elseif Symbol ⊑ name
            name = LInt
        end
        _ts = unwraptv(s.parameters[2])
        _ts = rewrap_unionall(_ts, s0)
        if !(_ts <: Tuple)
            return ⊤
        end
        return __getfield_tfunc(_ts, name, setfield)
    end
    ftypes = datatype_fieldtypes(s)
    nf = length(ftypes)
    # If no value has this type, then this statement should be unreachable.
    # Bail quickly now.
    if !has_concrete_subtype(s) || nf == 0
        return ⊥
    end
    if isConditional(name)
        return ⊥ # can't index fields with Bool
    end
    if !isConst(name)
        name = widenconst(name)
        hasintersect(name, Int) || hasintersect(name, Symbol) || return ⊥
        if nf == 1
            return NativeType(rewrap_unionall(unwrapva(ftypes[1]), s0))
        end
        # union together types of all fields
        t = ⊥
        for i in 1:nf
            _ft = ftypes[i]
            setfield && isconst(s, i) && continue
            t = tmerge(t, NativeType(rewrap_unionall(unwrapva(_ft), s0)))
            t === ⊤ && break
        end
        return t
    end
    fld = constant(name)
    if isa(fld, Symbol)
        fld = fieldindex(s, fld, false)
    end
    if !isa(fld, Int)
        return ⊥
    end
    if s <: Tuple && fld >= nf && isvarargtype(ftypes[nf])
        return NativeType(rewrap_unionall(unwrapva(ftypes[nf]), s0))
    end
    if fld < 1 || fld > nf
        return ⊥
    elseif setfield && isconst(s, fld)
        return ⊥
    end
    R = ftypes[fld]
    if isempty(s.parameters)
        return NativeType(R)
    end
    return NativeType(rewrap_unionall(R, s0))
end

function setfield!_tfunc(o::LatticeElement, f::LatticeElement, v::LatticeElement, order::LatticeElement)
    if !isVararg(order)
        hasintersect(widenconst(order), Symbol) || return ⊥
    end
    return setfield!_tfunc(o, f, v)
end
function setfield!_tfunc(o::LatticeElement, f::LatticeElement, v::LatticeElement)
    mutability_errorcheck(o) || return ⊥
    ft = _getfield_tfunc(o, f, true)
    ft === ⊥ && return ⊥
    hasintersect(widenconst(v), widenconst(ft)) || return ⊥
    return v
end
mutability_errorcheck(obj::LatticeElement) = mutability_errorcheck(widenconst(obj))
function mutability_errorcheck(@nospecialize objt0)
    objt = unwrap_unionall(objt0)
    if isa(objt, Union)
        return mutability_errorcheck(rewrap_unionall(objt.a, objt0)) ||
               mutability_errorcheck(rewrap_unionall(objt.b, objt0))
    elseif isa(objt, DataType)
        # Can't say anything about abstract types
        isabstracttype(objt) && return true
        return ismutabletype(objt)
    end
    return true
end

function setfield!_nothrow(argtypes::Vector{LatticeElement})
    if length(argtypes) == 4
        order = argtypes[4]
        order === Const(:not_atomic) || return false # currently setfield!_nothrow is assuming not atomic
    else
        length(argtypes) == 3 || return false
    end
    return setfield!_nothrow(argtypes[1], argtypes[2], argtypes[3])
end
setfield!_nothrow(s00::LatticeElement, name::LatticeElement, v::LatticeElement) =
    _setfield!_nothrow(widenconst(s00), name, v)
function _setfield!_nothrow(@nospecialize(s0), name::LatticeElement, v::LatticeElement)
    s = unwrap_unionall(s0)
    if isa(s, Union)
        return _setfield!_nothrow(rewrap_unionall(s.a, s0), name, v) &&
               _setfield!_nothrow(rewrap_unionall(s.b, s0), name, v)
    elseif isa(s, DataType)
        # Can't say anything about abstract types
        isabstracttype(s) && return false
        ismutabletype(s) || return false
        s.name.atomicfields == C_NULL || return false # TODO: currently we're only testing for ordering == :not_atomic
        isConst(name) || return false
        field = try_compute_fieldidx(s, constant(name))
        field === nothing && return false
        # `try_compute_fieldidx` already check for field index bound.
        isconst(s, field) && return false
        v_expected = fieldtype(s0, field)
        return v ⊑ₜ v_expected
    end
    return false
end

swapfield!_tfunc(o::LatticeElement, f::LatticeElement, v::LatticeElement, order::LatticeElement) = getfield_tfunc(o, f)
swapfield!_tfunc(o::LatticeElement, f::LatticeElement, v::LatticeElement) = getfield_tfunc(o, f)
modifyfield!_tfunc(o::LatticeElement, f::LatticeElement, op::LatticeElement, v::LatticeElement, order::LatticeElement) = modifyfield!_tfunc(o, f, op, v)
function modifyfield!_tfunc(o::LatticeElement, f::LatticeElement, op::LatticeElement, v::LatticeElement)
    T = _fieldtype_tfunc(widenconst(o), isconcretetype(o), f)
    T === ⊥ && return ⊥
    PT = Const(Pair)
    return NativeType(instanceof_tfunc(apply_type_tfunc(PT, T, T))[1])
end
function abstract_modifyfield!(interp::AbstractInterpreter, argtypes::Argtypes, sv::InferenceState)
    nargs = length(argtypes)
    if !isempty(argtypes) && isVararg(argtypes[nargs])
        nargs - 1 <= 6 || return CallMeta(⊥, false)
        nargs > 3 || return CallMeta(⊤, false)
    else
        5 <= nargs <= 6 || return CallMeta(⊥, false)
    end
    o = unwrapva_𝑳(argtypes[2])
    f = unwrapva_𝑳(argtypes[3])
    RT = modifyfield!_tfunc(o, f, ⊤, ⊤)
    info = false
    if nargs >= 5 && RT !== ⊥
        # we may be able to refine this to a PartialStruct by analyzing `op(o.f, v)::T`
        # as well as compute the info for the method matches
        op = unwrapva_𝑳(argtypes[4])
        v = unwrapva_𝑳(argtypes[5])
        TF = getfield_tfunc(o, f)
        push!(sv.ssavalue_uses[sv.currpc], sv.currpc) # temporarily disable `call_result_unused` check for this call
        callinfo = abstract_call(interp, ArgInfo(nothing, LatticeElement[op, TF, v]), sv, #=max_methods=# 1)
        pop!(sv.ssavalue_uses[sv.currpc], sv.currpc)
        TF2 = callinfo.rt ⊓ widenconst(TF)
        if TF2 === ⊥
            RT = ⊥
        elseif isconcretetype(widenconst(RT)) && has_nontrivial_const_info(TF2) # isconcrete condition required to form a PartialStruct
            RT = PartialStruct(widenconst(RT), LatticeElement[TF, TF2])
        end
        info = callinfo.info
    end
    return CallMeta(RT, info)
end
replacefield!_tfunc(o::LatticeElement, f::LatticeElement, x::LatticeElement, v::LatticeElement,
    success_order::LatticeElement, failure_order::LatticeElement) = replacefield!_tfunc(o, f, x, v)
replacefield!_tfunc(o::LatticeElement, f::LatticeElement, x::LatticeElement, v::LatticeElement,
    success_order::LatticeElement) = replacefield!_tfunc(o, f, x, v)
function replacefield!_tfunc(o::LatticeElement, f::LatticeElement, x::LatticeElement, v::LatticeElement)
    T = _fieldtype_tfunc(widenconst(o), isconcretetype(o), f)
    T === ⊥ && return ⊥
    PT = Const(ccall(:jl_apply_cmpswap_type, Any, (Any,), T) where T)
    return NativeType(instanceof_tfunc(apply_type_tfunc(PT, T))[1])
end

# we could use tuple_tfunc instead of widenconst, but `o` is mutable, so that is unlikely to be beneficial

add_tfunc(getfield, 2, 4, getfield_tfunc, 1)
add_tfunc(setfield!, 3, 4, setfield!_tfunc, 3)

add_tfunc(swapfield!, 3, 4, swapfield!_tfunc, 3)
add_tfunc(modifyfield!, 4, 5, modifyfield!_tfunc, 3)
add_tfunc(replacefield!, 4, 6, replacefield!_tfunc, 3)

function fieldtype_nothrow(s00::LatticeElement, name::LatticeElement)
    s0 = widenconst(s00)
    s0 === Bottom && return true # unreachable
    if s0 === Any || s0 === Type || DataType ⊑ s0 || UnionAll ⊑ s0
        # We have no idea
        return false
    end

    # Due to bounds checking, we can't say anything unless we know what
    # the name is.
    isConst(name) || return false
    fld = constant(name)
    (isa(fld, Symbol) || isa(fld, Int)) || return false

    su = unwrap_unionall(s0)
    if isa(su, Union)
        return fieldtype_nothrow(NativeType(rewrap_unionall(su.a, s0)), name) &&
               fieldtype_nothrow(NativeType(rewrap_unionall(su.b, s0)), name)
    end

    s, exact = instanceof_tfunc(s00)
    s === Bottom && return false # always
    return _fieldtype_nothrow(s, exact, fld)
end

function _fieldtype_nothrow(@nospecialize(s), exact::Bool, fld::Union{Symbol,Int})
    u = unwrap_unionall(s)
    if isa(u, Union)
        a = _fieldtype_nothrow(u.a, exact, fld)
        b = _fieldtype_nothrow(u.b, exact, fld)
        return exact ? (a || b) : (a && b)
    end
    u isa DataType || return false
    isabstracttype(u) && return false
    if u.name === _NAMEDTUPLE_NAME && !isconcretetype(u)
        # TODO: better approximate inference
        return false
    end
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

fieldtype_tfunc(s0::LatticeElement, name::LatticeElement, boundscheck::LatticeElement) =
    fieldtype_tfunc(s0, name)
function fieldtype_tfunc(s0::LatticeElement, name::LatticeElement)
    if s0 === ⊥
        return ⊥
    end
    s = widenconst(s0)
    if s0 === ⊤ || s === Type || DataType ⊑ s0 || UnionAll ⊑ s0
        # For a generic DataType, one of the fields could still be a TypeVar
        # which is not a Type. Tuple{...} can also contain Symbols etc.
        return ⊤
    end
    # fieldtype only accepts Types
    if isConst(s0) && let s0val = constant(s0)
           !(isa(s0val, DataType) || isa(s0val, UnionAll) || isa(s0val, Union))
       end
        return ⊥
    elseif (s isa Type && s === Type{Bottom}) || isConditional(s0)
        return ⊥
    end

    su = unwrap_unionall(s)
    if isa(su, Union)
        return tmerge(fieldtype_tfunc(NativeType(rewrap_unionall(su.a, s)), name),
                      fieldtype_tfunc(NativeType(rewrap_unionall(su.b, s)), name))
    end

    s, exact = instanceof_tfunc(s0)
    s === Bottom && return ⊥
    return _fieldtype_tfunc(s, exact, name)
end

function _fieldtype_tfunc(@nospecialize(s#=::Type=#), exact::Bool, name::LatticeElement)
    exact = exact && !has_free_typevars(s)
    u = unwrap_unionall(s)
    if isa(u, Union)
        ta0 = _fieldtype_tfunc(rewrap_unionall(u.a, s), exact, name)
        tb0 = _fieldtype_tfunc(rewrap_unionall(u.b, s), exact, name)
        ta0 ⊑ tb0 && return tb0
        tb0 ⊑ ta0 && return ta0
        ta, exacta, _, istypea = instanceof_tfunc(ta0)
        tb, exactb, _, istypeb = instanceof_tfunc(tb0)
        if exact && exacta && exactb
            return Const(Union{ta, tb})
        end
        if istypea && istypeb
            return NativeType(Type{<:Union{ta, tb}})
        end
        return ⊤
    end
    u isa DataType || return ⊤
    if isabstracttype(u)
        # Abstract types have no fields
        exact && return ⊥
        # Type{...} without free typevars has no subtypes, so it is actually
        # exact, even if `exact` is false.
        isType(u) && !has_free_typevars(u.parameters[1]) && return ⊥
        return ⊤
    end
    if u.name === _NAMEDTUPLE_NAME && !isconcretetype(u)
        # TODO: better approximate inference
        return NativeType(Union{Type, TypeVar})
    end
    ftypes = datatype_fieldtypes(u)
    if isempty(ftypes)
        return ⊥
    end

    if !isConst(name)
        name = widenconst(name)
        if !(Int <: name || Symbol <: name)
            return ⊥
        end
        t = ⊥
        for i in 1:length(ftypes)
            ft1 = unwrapva(ftypes[i])
            exactft1 = exact || (!has_free_typevars(ft1) && u.name !== Tuple.name)
            ft1 = rewrap_unionall(ft1, s)
            if exactft1
                if hasuniquerep(ft1)
                    ft1 = Const(ft1) # ft unique via type cache
                else
                    ft1 = NativeType(Type{ft1})
                end
            elseif ft1 isa Type || ft1 isa TypeVar
                if ft1 === Any && u.name === Tuple.name
                    # Tuple{:x} is possible in this case
                    ft1 = ⊤
                else
                    ft1 = NativeType(Type{ft} where ft<:ft1)
                end
            else
                ft1 = Const(ft1)
            end
            t = tmerge(t, ft1)
            t === ⊤ && break
        end
        return t
    end

    fld = constant(name)
    if isa(fld, Symbol)
        fld = fieldindex(u, fld, false)
    end
    if !isa(fld, Int)
        return ⊥
    end
    nf = length(ftypes)
    if u.name === Tuple.name && fld >= nf && isvarargtype(ftypes[nf])
        ft = unwrapva(ftypes[nf])
    elseif fld < 1 || fld > nf
        return ⊥
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
        return NativeType(Type{ft})
    end
    if u.name === Tuple.name && ft === Any
        # Tuple{:x} is possible
        return ⊤
    end
    return NativeType(Type{<:ft})
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

function apply_type_nothrow(argtypes::Vector{LatticeElement}, rt::LatticeElement)
    widenconst(rt) === Type && return false
    length(argtypes) >= 1 || return false
    headtypetype = argtypes[1]
    if isConst(headtypetype)
        headtype = constant(headtypetype)
    else
        t = widenconst(headtypetype)
        if isconstType(t)
            headtype = t.parameters[1]
        else
            return false
        end
    end
    # We know the apply_type is well formed. Otherwise our rt would have been
    # Bottom (or Type).
    (headtype === Union) && return true
    isConst(rt) && return true
    u = headtype
    for i = 2:length(argtypes)
        isa(u, UnionAll) || return false
        ai = argtypes[i]
        if ai ⊑ₜ TypeVar || unwraptype(ai) === DataType
            # We don't know anything about the bounds of this typevar, but as
            # long as the UnionAll is not constrained, that's ok.
            if !(u.var.lb === Bottom && u.var.ub === Any)
                return false
            end
        elseif (isConst(ai) && isa(constant(ai), Type)) || isconstType(ai)
            ai = isConst(ai) ? constant(ai) : ai.parameters[1]
            if has_free_typevars(u.var.lb) || has_free_typevars(u.var.ub)
                return false
            end
            if !(u.var.lb <: ai <: u.var.ub)
                return false
            end
        else
            T, exact, _, istype = instanceof_tfunc(ai)
            if T === Bottom
                if !(u.var.lb === Bottom && u.var.ub === Any)
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
function apply_type_tfunc(headtypetype::LatticeElement, @nospecialize args::LatticeElement...)
    if isConst(headtypetype)
        headtype = constant(headtypetype)
    else
        t = widenconst(headtypetype)
        if isconstType(t)
            headtype = t.parameters[1]
        else
            return ⊤
        end
    end
    if !isempty(args) && isVararg(args[end])
        return NativeType(isvarargtype(headtype) ? TypeofVararg : Type)
    end
    largs = length(args)
    if headtype === Union
        largs == 0 && return Const(Bottom)
        hasnonType = false
        for i = 1:largs
            ai = args[i]
            if isConst(ai)
                aival = constant(ai)
                if !isa(aival, Type)
                    if isa(aival, TypeVar)
                        hasnonType = true
                    else
                        return ⊥
                    end
                end
            else
                ai = widenconst(ai)
                if !isType(ai)
                    if !isa(ai, Type) || hasintersect(ai, Type) || hasintersect(ai, TypeVar)
                        hasnonType = true
                    else
                        return ⊥
                    end
                end
            end
        end
        if largs == 1 # Union{T} --> T
            u1 = typeintersect(widenconst(args[1]), Type)
            valid_as_lattice(u1) || return ⊥
            return NativeType(u1)
        end
        hasnonType && return NativeType(Type)
        ty = Bottom
        allconst = true
        for i = 1:largs
            ai = args[i]
            if isConst(ai)
                aty = constant(ai)
            else
                aty = (widenconst(ai)::DataType).parameters[1]
                allconst &= hasuniquerep(aty)
            end
            ty = Union{ty, aty}
        end
        return allconst ? Const(ty) : NativeType(Type{ty})
    end
    istuple = isa(headtype, Type) && (headtype == Tuple)
    if !istuple && !isa(headtype, UnionAll) && !isvarargtype(headtype)
        return ⊥
    end
    uw = unwrap_unionall(headtype)
    uncertain = false
    canconst = true
    tparams = Any[]
    outervars = TypeVar[]
    varnamectr = 1
    ua = headtype
    for i = 1:largs
        ai = args[i]
        ai_w = widenconst(ai)
        if isType(ai_w)
            aip1 = ai_w.parameters[1]
            canconst &= !has_free_typevars(aip1)
            push!(tparams, aip1)
        elseif isConst(ai) && begin
                   aival = constant(ai)
                   isa(aival, Type) || isa(aival, TypeVar) || valid_tparam(aival) || (istuple && isvarargtype(aival))
               end
            push!(tparams, aival)
        elseif isPartialTypeVar(ai)
            canconst = false
            push!(tparams, partialtypevar(ai).tv)
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
        return NativeType(isvarargtype(headtype) ? TypeofVararg : Type{<:headtype})
    end
    !uncertain && canconst && return Const(appl)
    if isvarargtype(appl)
        return NativeType(TypeofVararg)
    end
    if istuple
        return NativeType(Type{<:appl})
    end
    ans = Type{appl}
    for i = length(outervars):-1:1
        ans = UnionAll(outervars[i], ans)
    end
    return NativeType(ans)
end
add_tfunc(apply_type, 1, INT_INF, apply_type_tfunc, 10)

# convert the dispatch tuple type argtype to the real (concrete) type of
# the tuple of those values
function tuple_tfunc(argtypes::Vector{LatticeElement})
    all_are_const = true
    for i in 1:length(argtypes)
        if !isConst(argtypes[i])
            all_are_const = false
            break
        end
    end
    if all_are_const
        return Const(ntuple(i -> constant(argtypes[i]), length(argtypes)))
    end
    params = Vector{Any}(undef, length(argtypes))
    anyinfo = false
    for i in 1:length(argtypes)
        argtypes[i] = x = widenconditional(argtypes[i])
        if has_nontrivial_const_info(x) || isPartialTypeVar(x)
            anyinfo = true
        else
            if !isVararg(x)
                x = widenconst(x)
            end
        end
        if isConst(x)
            params[i] = typeof(constant(x))
        else
            t = isVararg(x) ? vararg(x) : widenconst(x)
            if isType(t)
                anyinfo = true
                tparam = t.parameters[1]
                if hasuniquerep(tparam) || tparam === Bottom
                    params[i] = typeof(tparam)
                else
                    params[i] = Type
                end
            else
                params[i] = t
            end
        end
    end
    typ = Tuple{params...}
    # replace a singleton type with its equivalent Const object
    isdefined(typ, :instance) && return Const(typ.instance)
    return anyinfo ? PartialStruct(typ, argtypes) : NativeType(typ)
end

arrayref_tfunc(boundscheck::LatticeElement, ary::LatticeElement, @nospecialize idxs::LatticeElement...) =
    NativeType(_arrayref_tfunc(boundscheck, ary, idxs))
function _arrayref_tfunc(boundscheck::LatticeElement, ary::LatticeElement,
    @nospecialize idxs::Tuple)
    isempty(idxs) && return ⊥
    array_builtin_common_errorcheck(boundscheck, ary, idxs) || return ⊥
    return array_elmtype(ary)
end
add_tfunc(arrayref, 3, INT_INF, arrayref_tfunc, 20)
add_tfunc(const_arrayref, 3, INT_INF, arrayref_tfunc, 20)

function arrayset_tfunc(boundscheck::LatticeElement, ary::LatticeElement, item::LatticeElement,
    @nospecialize idxs::LatticeElement...)
    hasintersect(widenconst(item), _arrayref_tfunc(boundscheck, ary, idxs)) || return ⊥
    return ary
end
add_tfunc(arrayset, 4, INT_INF, arrayset_tfunc, 20)

function array_builtin_common_errorcheck(boundscheck::LatticeElement, ary::LatticeElement,
    @nospecialize idxs::Tuple)
    hasintersect(widenconst(boundscheck), Bool) || return false
    hasintersect(widenconst(ary), Array) || return false
    for i = 1:length(idxs)
        idx = getfield(idxs, i)
        idx = isVararg(idx) ? unwrapva(vararg(idx)) : widenconst(idx)
        hasintersect(idx, Int) || return false
    end
    return true
end

function array_elmtype(ary::LatticeElement)
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

function _opaque_closure_tfunc(arg::LatticeElement, isva::LatticeElement,
        lb::LatticeElement, ub::LatticeElement, source::LatticeElement, env::Vector{LatticeElement},
        linfo::MethodInstance)

    argt, argt_exact = instanceof_tfunc(arg)
    lbt, lb_exact = instanceof_tfunc(lb)
    if !lb_exact
        lbt = Bottom
    end

    ubt, ub_exact = instanceof_tfunc(ub)

    t = (argt_exact ? Core.OpaqueClosure{argt, T} : Core.OpaqueClosure{<:argt, T}) where T
    t = lbt == ubt ? t{ubt} : (t{T} where lbt <: T <: ubt)

    isConst(source) || return NativeType(t)
    sourceval = constant(source)
    isa(sourceval, Method) || return NativeType(t)
    isConst(isva) || return NativeType(t)
    isvaval = constant(isva)
    isa(isvaval, Bool) || return NativeType(t)

    return mkPartialOpaque(t, tuple_tfunc(env), isvaval, linfo, sourceval)
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

function array_builtin_common_nothrow(argtypes::Vector{LatticeElement}, first_idx_idx::Int)
    length(argtypes) >= 4 || return false
    boundscheck = argtypes[1]
    arytype = argtypes[2]
    array_builtin_common_typecheck(boundscheck, arytype, argtypes, first_idx_idx) || return false
    # If we could potentially throw undef ref errors, bail out now.
    arytype = widenconst(arytype)
    array_type_undefable(arytype) && return false
    # If we have @inbounds (first argument is false), we're allowed to assume
    # we don't throw bounds errors.
    if isConst(boundscheck)
        !(constant(boundscheck)::Bool) && return true
    end
    # Else we can't really say anything here
    # TODO: In the future we may be able to track the shapes of arrays though
    # inference.
    return false
end

function array_builtin_common_typecheck(
    boundscheck::LatticeElement, arytype::LatticeElement,
    argtypes::Vector{LatticeElement}, first_idx_idx::Int)
    (boundscheck ⊑ₜ Bool && arytype ⊑ₜ Array) || return false
    for i = first_idx_idx:length(argtypes)
        argtypes[i] ⊑ₜ Int || return false
    end
    return true
end

function arrayset_typecheck(arytype::LatticeElement, elmtype::LatticeElement)
    # Check that we can determine the element type
    arytype = widenconst(arytype)
    isa(arytype, DataType) || return false
    elmtype_expected = arytype.parameters[1]
    isa(elmtype_expected, Type) || return false
    # Check that the element type is compatible with the element we're assigning
    elmtype ⊑ NativeType(lmtype_expected) || return false
    return true
end

# Query whether the given builtin is guaranteed not to throw given the argtypes
function _builtin_nothrow(@nospecialize(f), argtypes::Vector{LatticeElement}, rt::LatticeElement)
    if f === arrayset
        array_builtin_common_nothrow(argtypes, 4) || return true
        # Additionally check element type compatibility
        return arrayset_typecheck(argtypes[2], argtypes[3])
    elseif f === arrayref || f === const_arrayref
        return array_builtin_common_nothrow(argtypes, 3)
    elseif f === arraysize
        return arraysize_nothrow(argtypes)
    elseif f === Core._expr
        length(argtypes) >= 1 || return false
        return argtypes[1] ⊑ₜ Symbol
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
        return argtypes[2] ⊑ₜ Type
    elseif f === (<:)
        length(argtypes) == 2 || return false
        return argtypes[1] ⊑ₜ Type && argtypes[2] ⊑ₜ Type
    elseif f === UnionAll
        return length(argtypes) == 2 &&
            (argtypes[1] ⊑ₜ TypeVar && argtypes[2] ⊑ₜ Type)
    elseif f === isdefined
        return isdefined_nothrow(argtypes)
    elseif f === Core.sizeof
        length(argtypes) == 1 || return false
        return sizeof_nothrow(argtypes[1])
    elseif f === Core.kwfunc
        length(argtypes) == 1 || return false
        return isConst(rt)
    elseif f === Core.ifelse
        length(argtypes) == 3 || return false
        return argtypes[1] ⊑ₜ Bool
    elseif f === typeassert
        length(argtypes) == 2 || return false
        a3 = argtypes[2]
        a3w = widenconst(a3)
        if (isType(a3w) && !has_free_typevars(a3w) && argtypes[1] ⊑ₜ a3w.parameters[1]) ||
            (isConst(a3) && isa(constant(a3), Type) && argtypes[1] ⊑ₜ constant(a3))
            return true
        end
        return false
    end
    return false
end

function builtin_nothrow(@nospecialize(f), argtypes::Array{Any, 1}, @nospecialize(rt))
    rt === Bottom && return false
    contains_is(_PURE_BUILTINS, f) && return true
    return _builtin_nothrow(f, argtypes, rt)
end

function builtin_tfunction(interp::AbstractInterpreter, @nospecialize(f), argtypes::Argtypes,
                           sv::Union{InferenceState,Nothing})
    if f === tuple
        return tuple_tfunc(argtypes)
    end
    if isa(f, IntrinsicFunction)
        if is_pure_intrinsic_infer(f) && _all(a::LatticeElement -> isConst(a), argtypes)
            argvals = anymap(a::LatticeElement -> constant(a), argtypes)
            try
                return Const(f(argvals...))
            catch
            end
        end
        iidx = Int(reinterpret(Int32, f::IntrinsicFunction)) + 1
        if iidx < 0 || iidx > length(T_IFUNC)
            # invalid intrinsic
            return ⊤
        end
        tf = T_IFUNC[iidx]
    else
        fidx = find_tfunc(f)
        if fidx === nothing
            # unknown/unhandled builtin function
            return ⊤
        end
        tf = T_FFUNC_VAL[fidx]
    end
    tf = tf::Tuple{Int, Int, Any}
    if !isempty(argtypes) && isVararg(argtypes[end])
        if length(argtypes) - 1 > tf[2]
            # definitely too many arguments
            return ⊥
        end
        if length(argtypes) - 1 == tf[2]
            argtypes = argtypes[1:end-1]
        else
            vatype = vararg(argtypes[end])
            argtypes = argtypes[1:end-1]
            while length(argtypes) < tf[1]
                push!(argtypes, NativeType(unwrapva(vatype)))
            end
            if length(argtypes) < tf[2]
                push!(argtypes, NativeType(unconstrain_vararg_length(vatype)))
            end
        end
    elseif !(tf[1] <= length(argtypes) <= tf[2])
        # wrong # of args
        return ⊥
    end
    return tf[3](argtypes...)::LatticeElement
end

# Query whether the given intrinsic is nothrow

_iszero(x) = x === Intrinsics.xor_int(x, x)
_isneg1(x) = _iszero(Intrinsics.not_int(x))
_istypemin(x) = !_iszero(x) && Intrinsics.neg_int(x) === x

function intrinsic_nothrow(f::IntrinsicFunction, argtypes::Vector{LatticeElement})
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
        isConst(argtypes[2]) || return false
        if !isprimitivetype(widenconst(argtypes[1])) ||
           (widenconst(argtypes[1]) !== widenconst(argtypes[2]))
            return false
        end
        den_val = constant(argtypes[2])
        _iszero(den_val) && return false
        f !== Intrinsics.checked_sdiv_int && return true
        # Nothrow as long as we additionally don't do typemin(T)/-1
        return !_isneg1(den_val) || (isConst(argtypes[1]) && !_istypemin(constant(argtypes[1])))
    end
    if f === Intrinsics.pointerref
        # Nothrow as long as the types are ok. N.B.: dereferencability is not
        # modeled here, but can cause errors (e.g. ReadOnlyMemoryError). We follow LLVM here
        # in that it is legal to remove unused non-volatile loads.
        length(argtypes) == 3 || return false
        return argtypes[1] ⊑ₜ Ptr && argtypes[2] ⊑ₜ Int && argtypes[3] ⊑ₜ Int
    end
    if f === Intrinsics.pointerset
        eT = widenconst(pointer_eltype(argtypes[1]))
        isprimitivetype(eT) || return false
        return argtypes[2] ⊑ₜ eT && argtypes[3] ⊑ₜ Int && argtypes[4] ⊑ₜ Int
    end
    if f === Intrinsics.arraylen
        return argtypes[1] ⊑ₜ Array
    end
    if f === Intrinsics.bitcast
        ty, isexact, isconcrete = instanceof_tfunc(argtypes[1])
        xty = widenconst(argtypes[2])
        return isconcrete && isprimitivetype(ty) && isprimitivetype(xty) && Core.sizeof(ty) === Core.sizeof(xty)
    end
    if f in (Intrinsics.sext_int, Intrinsics.zext_int, Intrinsics.trunc_int,
             Intrinsics.fptoui, Intrinsics.fptosi, Intrinsics.uitofp,
             Intrinsics.sitofp, Intrinsics.fptrunc, Intrinsics.fpext)
        # If !isconcrete, `ty` may be Bottom at runtime even if we have
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

# TODO: this function is a very buggy and poor model of the return_type function
# since abstract_call_gf_by_type is a very inaccurate model of _method and of typeinf_type,
# while this assumes that it is an absolutely precise and accurate and exact model of both
function return_type_tfunc(interp::AbstractInterpreter, argtypes::Argtypes, sv::InferenceState)
    if length(argtypes) == 3
        tt = unwraptype(argtypes[3])
        if isConst(tt) || (isType(tt) && !has_free_typevars(tt))
            aft = unwraptype(argtypes[2])
            if isConst(aft) || (isType(aft) && !has_free_typevars(aft)) ||
                   (isconcretetype(aft) && !(aft <: Builtin))
                af_argtype = isConst(tt) ? constant(tt) : (tt::DataType).parameters[1]
                if isa(af_argtype, DataType) && af_argtype <: Tuple
                    argtypes = LatticeElement[LatticeElement(aft)]
                    for ty in af_argtype.parameters
                        push!(argtypes, NativeType(ty))
                    end
                    if contains_is(argtypes, ⊥)
                        return CallMeta(Const(Bottom), false)
                    end
                    call = abstract_call(interp, ArgInfo(nothing, argtypes), sv, -1)
                    info = verbose_stmt_info(interp) ? ReturnTypeCallInfo(call.info) : false
                    rt = call.rt
                    if isConst(rt)
                        # output was computed to be constant
                        return CallMeta(Const(typeof(constant(rt))), info)
                    end
                    rt = widenconst(rt)
                    if rt === Bottom || (isconcretetype(rt) && !iskindtype(rt))
                        # output cannot be improved so it is known for certain
                        return CallMeta(Const(rt), info)
                    elseif !isempty(sv.pclimitations)
                        # conservatively express uncertainty of this result
                        # in two ways: both as being a subtype of this, and
                        # because of LimitedAccuracy causes
                        return CallMeta(NativeType(Type{<:rt}), info)
                    elseif (isConst(tt) || isconstType(tt)) &&
                        (isConst(aft) || isconstType(aft))
                        # input arguments were known for certain
                        # XXX: this doesn't imply we know anything about rt
                        return CallMeta(Const(rt), info)
                    elseif isType(rt)
                        return CallMeta(NativeType(Type{rt}), info)
                    else
                        return CallMeta(NativeType(Type{<:rt}), info)
                    end
                end
            end
        end
    end
    return CallMeta(NativeType(Type), false)
end

@specialize
