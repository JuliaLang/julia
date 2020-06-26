# This file is a part of Julia. License is MIT: https://julialang.org/license

#############
# constants #
#############

@nospecialize

const AbstractEvalConstant = Const

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
const DATATYPE_MUTABLE_FIELDINDEX = fieldindex(DataType, :mutable)
const DATATYPE_INSTANCE_FIELDINDEX = fieldindex(DataType, :instance)

const TYPENAME_NAME_FIELDINDEX = fieldindex(Core.TypeName, :name)
const TYPENAME_MODULE_FIELDINDEX = fieldindex(Core.TypeName, :module)
const TYPENAME_WRAPPER_FIELDINDEX = fieldindex(Core.TypeName, :wrapper)

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
# returns (type, isexact)
# if isexact is false, the actual runtime type may (will) be a subtype of t
function instanceof_tfunc(@nospecialize(t))
    if isa(t, Const)
        if isa(t.val, Type)
            return t.val, true
        end
        return Bottom, true
    end
    t = widenconst(t)
    if t === Bottom || t === typeof(Bottom) || typeintersect(t, Type) === Bottom
        return Bottom, true
    elseif isType(t)
        tp = t.parameters[1]
        return tp, !has_free_typevars(tp)
    elseif isa(t, UnionAll)
        t′ = unwrap_unionall(t)
        t′′, isexact = instanceof_tfunc(t′)
        tr = rewrap_unionall(t′′, t)
        if t′′ isa DataType && !has_free_typevars(tr)
            # a real instance must be within the declared bounds of the type,
            # so we can intersect with the original wrapper.
            tr = typeintersect(tr, t′′.name.wrapper)
        end
        return tr, isexact
    elseif isa(t, Union)
        ta, isexact_a = instanceof_tfunc(t.a)
        tb, isexact_b = instanceof_tfunc(t.b)
        ta === Union{} && return tb, isexact_b
        tb === Union{} && return ta, isexact_a
        ta == tb && return ta, isexact_a && isexact_b
        return Union{ta, tb}, false # at runtime, will be exactly one of these
    end
    return Any, false
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
add_tfunc(fpislt, 2, 2, cmp_tfunc, 1)
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
function isdefined_tfunc(@nospecialize(args...))
    arg1 = args[1]
    if isa(arg1, Const)
        a1 = typeof(arg1.val)
    else
        a1 = widenconst(arg1)
    end
    if isType(a1)
        return Bool
    end
    a1 = unwrap_unionall(a1)
    if isa(a1, DataType) && !a1.abstract
        if a1 === Module
            length(args) == 2 || return Bottom
            sym = args[2]
            Symbol <: widenconst(sym) || return Bottom
            if isa(sym, Const) && isa(sym.val, Symbol) && isa(arg1, Const) && isdefined(arg1.val, sym.val)
                return Const(true)
            end
        elseif length(args) == 2 && isa(args[2], Const)
            val = args[2].val
            idx::Int = 0
            if isa(val, Symbol)
                idx = fieldindex(a1, val, false)
            elseif isa(val, Int)
                idx = val
            else
                return Bottom
            end
            if 1 <= idx <= a1.ninitialized
                return Const(true)
            elseif a1.name === _NAMEDTUPLE_NAME
                if isconcretetype(a1)
                    return Const(false)
                end
            elseif idx <= 0 || (!isvatuple(a1) && idx > fieldcount(a1))
                return Const(false)
            elseif !isvatuple(a1) && isbitstype(fieldtype(a1, idx))
                return Const(true)
            elseif isa(arg1, Const)
                arg1v = (arg1::Const).val
                if !ismutable(arg1v) || isdefined(arg1v, idx) || (isa(arg1v, DataType) && is_dt_const_field(idx))
                    return Const(isdefined(arg1v, idx))
                end
            end
        end
    end
    Bool
end

add_tfunc(isdefined, 1, 2, isdefined_tfunc, 1)
function sizeof_nothrow(@nospecialize(x))
    if isa(x, Const)
        x = x.val
        if !isa(x, Type) || x === DataType
            return true
        end
    elseif isa(x, Conditional)
        return true
    else
        x = widenconst(x)
    end
    if isa(x, Union)
        return sizeof_nothrow(x.a) && sizeof_nothrow(x.b)
    end
    isconstType(x) && (x = x.parameters[1]) # since sizeof(typeof(x)) == sizeof(x)
    x === DataType && return false
    return isconcretetype(x) || isprimitivetype(x)
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
    x = widenconst(x)
    if isa(x, Union)
        return tmerge(sizeof_tfunc(x.a), sizeof_tfunc(x.b))
    end
    x !== DataType && isconcretetype(x) && return _const_sizeof(x)
    isprimitivetype(x) && return _const_sizeof(x)
    return Int
end
add_tfunc(Core.sizeof, 1, 1, sizeof_tfunc, 0)
function nfields_tfunc(@nospecialize(x))
    isa(x, Const) && return Const(nfields(x.val))
    isa(x, Conditional) && return Const(0)
    x = widenconst(x)
    if isa(x, DataType) && !x.abstract && !(x.name === Tuple.name && isvatuple(x))
        if !(x.name === _NAMEDTUPLE_NAME && !isconcretetype(x))
            return Const(isdefined(x, :types) ? length(x.types) : length(x.name.names))
        end
    end
    return Int
end
add_tfunc(nfields, 1, 1, nfields_tfunc, 0)
add_tfunc(Core._expr, 1, INT_INF, (@nospecialize args...)->Expr, 100)
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
        b = unwrap_unionall(b.parameters[1])
        b === Union{} && return true
        return !isa(b, DataType) || b.name != _va_typename
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
    if a <: Ptr
        if isa(a,DataType) && isa(a.parameters[1],Type)
            return a.parameters[1]
        elseif isa(a,UnionAll) && !has_free_typevars(a)
            unw = unwrap_unionall(a)
            if isa(unw,DataType)
                return rewrap_unionall(unw.parameters[1], a)
            end
        end
    end
    return Any
end
add_tfunc(pointerref, 3, 3,
          function (@nospecialize(a), @nospecialize(i), @nospecialize(align))
            return pointer_eltype(a)
          end, 4)
add_tfunc(pointerset, 4, 4, (@nospecialize(a), @nospecialize(v), @nospecialize(i), @nospecialize(align)) -> a, 5)

# more accurate typeof_tfunc for vararg tuples abstract only in length
function typeof_concrete_vararg(t::DataType)
    np = length(t.parameters)
    for i = 1:np
        p = t.parameters[i]
        if i == np && isvarargtype(p)
            pp = unwrap_unionall(p)
            if isconcretetype(pp.parameters[1]) && pp.parameters[2] isa TypeVar
                return rewrap_unionall(Type{Tuple{t.parameters[1:np-1]..., pp}}, p)
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
        if isa(u, DataType) && !u.abstract
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
add_tfunc(typeof, 1, 1, typeof_tfunc, 0)

function typeassert_tfunc(@nospecialize(v), @nospecialize(t))
    t = instanceof_tfunc(t)[1]
    t === Any && return v
    if isa(v, Const)
        if !has_free_typevars(t) && !isa(v.val, t)
            return Bottom
        end
        return v
    elseif isa(v, Conditional)
        if !(Bool <: t)
            return Bottom
        end
        return v
    end
    return typeintersect(widenconst(v), t)
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
add_tfunc(isa, 2, 2, isa_tfunc, 0)

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
add_tfunc(<:, 2, 2, subtype_tfunc, 0)

is_dt_const_field(fld::Int) = (
     fld == DATATYPE_NAME_FIELDINDEX ||
     fld == DATATYPE_PARAMETERS_FIELDINDEX ||
     fld == DATATYPE_TYPES_FIELDINDEX ||
     fld == DATATYPE_SUPER_FIELDINDEX ||
     fld == DATATYPE_MUTABLE_FIELDINDEX ||
     fld == DATATYPE_INSTANCE_FIELDINDEX
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
    if t.name === NamedTuple.body.body.name
        names, types = t.parameters
        if names isa Tuple
            return length(names)
        end
        if types isa DataType && types <: Tuple
            return fieldcount_noerror(types)
        end
        abstr = true
    else
        abstr = t.abstract || (t.name === Tuple.name && isvatuple(t))
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
    elseif isa(field, Integer)
        max_fields = fieldcount_noerror(typ)
        max_fields === nothing && return nothing
        (1 <= field <= max_fields) || return nothing
    else
        return nothing
    end
    return field
end

function getfield_nothrow(argtypes::Vector{Any})
    2 <= length(argtypes) <= 3 || return false
    length(argtypes) == 2 && return getfield_nothrow(argtypes[1], argtypes[2], Const(true))
    return getfield_nothrow(argtypes[1], argtypes[2], argtypes[3])
end
function getfield_nothrow(@nospecialize(s00), @nospecialize(name), @nospecialize(inbounds))
    bounds_check_disabled = isa(inbounds, Const) && inbounds.val === false
    # If we don't have invounds and don't know the field, don't even bother
    if !bounds_check_disabled
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
        if bounds_check_disabled && !isa(sv, Module)
            # If bounds checking is disabled and all fields are assigned,
            # we may assume that we don't throw
            for i = 1:fieldcount(typeof(sv))
                isdefined(sv, i) || return false
            end
            return true
        end
        return false
    end

    s = unwrap_unionall(widenconst(s00))
    if isa(s, Union)
        return getfield_nothrow(rewrap(s.a, s00), name, inbounds) &&
            getfield_nothrow(rewrap(s.b, s00), name, inbounds)
    elseif isa(s, DataType)
        # Can't say anything about abstract types
        s.abstract && return false
        # If all fields are always initialized, and bounds check is disabled, we can assume
        # we don't throw
        if bounds_check_disabled && !isvatuple(s) && s.name !== NamedTuple.body.body.name && fieldcount(s) == s.ninitialized
            return true
        end
        # Else we need to know what the field is
        isa(name, Const) || return false
        field = try_compute_fieldidx(s, name.val)
        field === nothing && return false
        field <= s.ninitialized && return true
    end

    return false
end

getfield_tfunc(@nospecialize(s00), @nospecialize(name), @nospecialize(inbounds)) =
    getfield_tfunc(s00, name)
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
                    fld == TYPENAME_WRAPPER_FIELDINDEX)
                    return AbstractEvalConstant(getfield(sv, fld))
                end
            end
            if isa(sv, Module) && isa(nv, Symbol)
                return abstract_eval_global(sv, nv)
            end
            if !(isa(nv,Symbol) || isa(nv,Int))
                return Bottom
            end
            if (isa(sv, SimpleVector) || !ismutable(sv)) && isdefined(sv, nv)
                return AbstractEvalConstant(getfield(sv, nv))
            end
        end
        s = typeof(sv)
    elseif isa(s, PartialStruct)
        if isa(name, Const)
            nv = name.val
            if isa(nv, Symbol)
                nv = fieldindex(widenconst(s), nv, false)
            end
            if isa(nv, Int) && 1 <= nv <= length(s.fields)
                return s.fields[nv]
            end
        end
        s = widenconst(s)
    end
    if isType(s) || !isa(s, DataType) || s.abstract
        return Any
    end
    if s <: Tuple && name ⊑ Symbol
        return Bottom
    end
    if s <: Module
        if name ⊑ Int
            return Bottom
        end
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
add_tfunc(getfield, 2, 3, getfield_tfunc, 1)
add_tfunc(setfield!, 3, 3, (@nospecialize(o), @nospecialize(f), @nospecialize(v)) -> v, 3)
fieldtype_tfunc(@nospecialize(s0), @nospecialize(name), @nospecialize(inbounds)) =
    fieldtype_tfunc(s0, name)

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
    u.abstract && return false
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
    if u.name === Tuple.name && fld >= nf && isvarargtype(ftypes[nf])
        # If we don't know the exact type, the length of the tuple will be determined
        # at runtime and we can't say anything.
        return exact
    end
    (fld >= 1 && fld <= nf) || return false
    return true
end

function fieldtype_tfunc(@nospecialize(s0), @nospecialize(name))
    if s0 === Bottom
        return Bottom
    end
    if s0 === Any || s0 === Type || DataType ⊑ s0 || UnionAll ⊑ s0
        return Type
    end
    # fieldtype only accepts Types, errors on `Module`
    if isa(s0, Const) && (!(isa(s0.val, DataType) || isa(s0.val, UnionAll) || isa(s0.val, Union)) || s0.val === Module)
        return Bottom
    end
    if s0 == Type{Module} || s0 == Type{Union{}} || isa(s0, Conditional)
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
        return tmerge(_fieldtype_tfunc(rewrap(u.a, s), exact, name),
                      _fieldtype_tfunc(rewrap(u.b, s), exact, name))
    end
    u isa DataType || return Type
    u.abstract && return Type
    if u.name === _NAMEDTUPLE_NAME && !isconcretetype(u)
        # TODO: better approximate inference
        return Type
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
            else
                ft1 = Type{ft} where ft<:ft1
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

    exactft = exact || (!has_free_typevars(ft) && u.name !== Tuple.name)
    ft = rewrap_unionall(ft, s)
    if exactft
        if hasuniquerep(ft)
            return Const(ft) # ft unique via type cache
        end
        return Type{ft}
    end
    return Type{<:ft}
end
add_tfunc(fieldtype, 2, 3, fieldtype_tfunc, 0)

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
    # We know the apply_type is well formed. Oherwise our rt would have been
    # Bottom (or Type).
    (headtype === Union) && return true
    isa(rt, Const) && return true
    u = headtype
    for i = 2:length(argtypes)
        isa(u, UnionAll) || return false
        ai = widenconditional(argtypes[i])
        if ai ⊑ TypeVar
            # We don't know anything about the bounds of this typevar, but as
            # long as the UnionAll is not constrained, that's ok.
            if !(u.var.lb === Union{} && u.var.ub === Any)
                return false
            end
        elseif isa(ai, Const) && isa(ai.val, Type)
            ai = ai.val
            if has_free_typevars(u.var.lb) || has_free_typevars(u.var.ub)
                return false
            end
            if !(u.var.lb <: ai <: u.var.ub)
                return false
            end
        else
            return false
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
        return Type
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
    istuple = (headtype == Tuple)
    if !istuple && !isa(headtype, UnionAll)
        return Union{}
    end
    uncertain = false
    canconst = true
    tparams = Any[]
    outervars = Any[]
    varnamectr = 1
    for i = 1:largs
        ai = widenconditional(args[i])
        if isType(ai)
            aip1 = ai.parameters[1]
            canconst &= !has_free_typevars(aip1)
            push!(tparams, aip1)
        elseif isa(ai, Const) && (isa(ai.val, Type) || isa(ai.val, TypeVar) || valid_tparam(ai.val))
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
            if istuple
                if i == largs
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
                tvname = varnamectr <= length(_tvarnames) ? _tvarnames[varnamectr] : :_Z
                varnamectr += 1
                v = TypeVar(tvname)
                push!(tparams, v)
                push!(outervars, v)
            end
        end
    end
    local appl
    try
        appl = apply_type(headtype, tparams...)
    catch ex
        # type instantiation might fail if one of the type parameters
        # doesn't match, which could happen if a type estimate is too coarse
        return Type{<:headtype}
    end
    !uncertain && canconst && return Const(appl)
    if isvarargtype(headtype)
        return Type
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

function invoke_tfunc(interp::AbstractInterpreter, @nospecialize(ft), @nospecialize(types), @nospecialize(argtype), sv::InferenceState)
    argtype = typeintersect(types, argtype)
    argtype === Bottom && return Bottom
    argtype isa DataType || return Any # other cases are not implemented below
    isdispatchelem(ft) || return Any # check that we might not have a subtype of `ft` at runtime, before doing supertype lookup below
    types = rewrap_unionall(Tuple{ft, unwrap_unionall(types).parameters...}, types)
    argtype = Tuple{ft, argtype.parameters...}
    entry = ccall(:jl_gf_invoke_lookup, Any, (Any, UInt), types, get_world_counter(interp))
    if entry === nothing
        return Any
    end
    # XXX: update_valid_age!(min_valid[1], max_valid[1], sv)
    meth = entry.func
    (ti, env) = ccall(:jl_type_intersection_with_env, Any, (Any, Any), argtype, meth.sig)::SimpleVector
    rt, edge = typeinf_edge(interp, meth::Method, ti, env, sv)
    edge !== nothing && add_backedge!(edge::MethodInstance, sv)
    return rt
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
        return Const(tuple(Any[atypes[i].val for i in 1:length(atypes)]...))
    end
    params = Vector{Any}(undef, length(atypes))
    anyinfo = false
    for i in 1:length(atypes)
        x = atypes[i]
        # TODO ignore singleton Const (don't forget to update cache logic if you implement this)
        if !anyinfo
            anyinfo = !isa(x, Type) || isType(x)
        end
        if isa(x, Const)
            params[i] = typeof(x.val)
        else
            x = widenconst(x)
            if isType(x)
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
    (isa(argtypes[1], Const) && !argtypes[1].val) && return true
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
    isva = !isempty(argtypes) && isvarargtype(argtypes[end])
    if f === tuple
        return tuple_tfunc(argtypes)
    elseif f === svec
        return SimpleVector
    elseif f === arrayset
        if length(argtypes) < 4
            isva && return Any
            return Bottom
        end
        return argtypes[2]
    elseif f === arrayref || f === const_arrayref
        if length(argtypes) < 3
            isva && return Any
            return Bottom
        end
        a = widenconst(argtypes[2])
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
    elseif f === Expr
        if length(argtypes) < 1 && !isva
            return Bottom
        end
        return Expr
    elseif f === invoke
        if length(argtypes) > 1 && sv !== nothing && (isa(argtypes[1], Const) || isa(argtypes[1], Type))
            if isa(argtypes[1], Const)
                ft = Core.Typeof(argtypes[1].val)
            else
                ft = argtypes[1]
            end
            sig = argtypes[2]
            if isa(sig, Const)
                sigty = sig.val
            elseif isType(sig)
                sigty = sig.parameters[1]
            else
                sigty = nothing
            end
            if isa(sigty, Type) && !has_free_typevars(sigty) && sigty <: Tuple
                return invoke_tfunc(interp, ft, sigty, argtypes_to_type(argtypes[3:end]), sv)
            end
        end
        return Any
    end
    if isva
        return Any
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
    if !(tf[1] <= length(argtypes) <= tf[2])
        # wrong # of args
        return Bottom
    end
    return tf[3](argtypes...)
end

# Query whether the given intrinsic is nothrow

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
        den_val !== zero(typeof(den_val)) || return false
        f !== Intrinsics.checked_sdiv_int && return true
        # Nothrow as long as we additionally don't do typemin(T)/-1
        return den_val !== -1 || (isa(argtypes[1], Const) &&
            argtypes[1].val !== typemin(typeof(den_val)))
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
        ty = instanceof_tfunc(argtypes[1])[1]
        xty = widenconst(argtypes[2])
        return isprimitivetype(ty) && isprimitivetype(xty) && ty.size === xty.size
    end
    if f in (Intrinsics.sext_int, Intrinsics.zext_int, Intrinsics.trunc_int,
             Intrinsics.fptoui, Intrinsics.fptosi, Intrinsics.uitofp,
             Intrinsics.sitofp, Intrinsics.fptrunc, Intrinsics.fpext)
        # If !isexact, `ty` may be Union{} at runtime even if we have
        # isprimitivetype(ty).
        ty, isexact = instanceof_tfunc(argtypes[1])
        xty = widenconst(argtypes[2])
        return isexact && isprimitivetype(ty) && isprimitivetype(xty)
    end
    # The remaining intrinsics are math/bits/comparison intrinsics. They work on all
    # primitive types of the same type.
    isshift = f == shl_int || f == lshr_int || f == ashr_int
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
function return_type_tfunc(interp::AbstractInterpreter, argtypes::Vector{Any}, vtypes::VarTable, sv::InferenceState)
    if length(argtypes) == 3
        tt = argtypes[3]
        if isa(tt, Const) || (isType(tt) && !has_free_typevars(tt))
            aft = argtypes[2]
            if isa(aft, Const) || (isType(aft) && !has_free_typevars(aft)) ||
                   (isconcretetype(aft) && !(aft <: Builtin))
                af_argtype = isa(tt, Const) ? tt.val : tt.parameters[1]
                if isa(af_argtype, DataType) && af_argtype <: Tuple
                    argtypes_vec = Any[aft, af_argtype.parameters...]
                    if contains_is(argtypes_vec, Union{})
                        return Const(Union{})
                    end
                    rt = abstract_call(interp, nothing, argtypes_vec, vtypes, sv, -1)
                    if isa(rt, Const)
                        # output was computed to be constant
                        return Const(typeof(rt.val))
                    elseif hasuniquerep(rt) || rt === Bottom
                        # output type was known for certain
                        return Const(rt)
                    elseif (isa(tt, Const) || isconstType(tt)) &&
                           (isa(aft, Const) || isconstType(aft))
                        # input arguments were known for certain
                        # XXX: this doesn't imply we know anything about rt
                        return Const(rt)
                    elseif isType(rt)
                        return Type{rt}
                    else
                        return Type{<:widenconst(rt)}
                    end
                end
            end
        end
    end
    return nothing
end

# N.B.: typename maps type equivalence classes to a single value
function typename_static(@nospecialize(t))
    t isa Const && return _typename(t.val)
    t isa Conditional && return Bool.name
    t = unwrap_unionall(widenconst(t))
    return isType(t) ? _typename(t.parameters[1]) : Core.TypeName
end

@specialize
