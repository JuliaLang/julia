# This file is a part of Julia. License is MIT: https://julialang.org/license

#############
# constants #
#############

const AbstractEvalConstant = Const

const _NAMEDTUPLE_NAME = NamedTuple.body.body.name

const INT_INF = typemax(Int) # integer infinity

const N_IFUNC = reinterpret(Int32, arraylen) + 1
const T_IFUNC = Vector{Tuple{Int, Int, Any}}(undef, N_IFUNC)
const T_IFUNC_COST = Vector{Int}(undef, N_IFUNC)
const T_FFUNC_KEY = Vector{Any}()
const T_FFUNC_VAL = Vector{Tuple{Int, Int, Any}}()
const T_FFUNC_COST = Vector{Int}()

const DATATYPE_NAME_FIELDINDEX = fieldindex(DataType, :name)
const DATATYPE_PARAMETERS_FIELDINDEX = fieldindex(DataType, :parameters)
const DATATYPE_TYPES_FIELDINDEX = fieldindex(DataType, :types)
const DATATYPE_SUPER_FIELDINDEX = fieldindex(DataType, :super)
const DATATYPE_MUTABLE_FIELDINDEX = fieldindex(DataType, :mutable)

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
        return rewrap_unionall(t′′, t), isexact
    elseif isa(t, Union)
        ta, isexact_a = instanceof_tfunc(t.a)
        tb, isexact_b = instanceof_tfunc(t.b)
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
function fptoui_tfunc(@nospecialize(x))
    T = widenconst(x)
    T === Float64 && return UInt64
    T === Float32 && return UInt32
    T === Float16 && return UInt16
    return Any
end
function fptosi_tfunc(@nospecialize(x))
    T = widenconst(x)
    T === Float64 && return Int64
    T === Float32 && return Int32
    T === Float16 && return Int16
    return Any
end

    ## conversion ##
add_tfunc(bitcast, 2, 2, bitcast_tfunc, 1)
add_tfunc(sext_int, 2, 2, bitcast_tfunc, 1)
add_tfunc(zext_int, 2, 2, bitcast_tfunc, 1)
add_tfunc(trunc_int, 2, 2, bitcast_tfunc, 1)
add_tfunc(fptoui, 1, 2, fptoui_tfunc, 1)
add_tfunc(fptosi, 1, 2, fptosi_tfunc, 1)
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
add_tfunc(not_int, 1, 1, math_tfunc, 1)
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
add_tfunc(ifelse, 3, 3,
    function (@nospecialize(cnd), @nospecialize(x), @nospecialize(y))
        if isa(cnd, Const)
            if cnd.val === true
                return x
            elseif cnd.val === false
                return y
            else
                return Bottom
            end
        elseif isa(cnd, Conditional)
            # handled in abstract_call
        elseif !(Bool ⊑ cnd)
            return Bottom
        end
        return tmerge(x, y)
    end, 1)
function egal_tfunc(@nospecialize(x), @nospecialize(y))
    if isa(x, Const) && isa(y, Const)
        return Const(x.val === y.val)
    elseif typeintersect(widenconst(x), widenconst(y)) === Bottom
        return Const(false)
    elseif (isa(x, Const) && y === typeof(x.val) && isdefined(y, :instance)) ||
           (isa(y, Const) && x === typeof(y.val) && isdefined(x, :instance))
        return Const(true)
    elseif isa(x, Conditional) && isa(y, Const)
        y.val === false && return Conditional(x.var, x.elsetype, x.vtype)
        y.val === true && return x
        return x
    elseif isa(y, Conditional) && isa(x, Const)
        x.val === false && return Conditional(y.var, y.elsetype, y.vtype)
        x.val === true && return y
    end
    return Bool
end
add_tfunc(===, 2, 2, egal_tfunc, 1)
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
        if a1 <: Array # TODO update when deprecation is removed
        elseif a1 === Module
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
            elseif isa(arg1, Const) && isimmutable((arg1::Const).val)
                return Const(isdefined((arg1::Const).val, idx))
            end
        end
    end
    Bool
end
# TODO change INT_INF to 2 when deprecation is removed
add_tfunc(isdefined, 1, INT_INF, isdefined_tfunc, 1)
function _const_sizeof(@nospecialize(x))
    # Constant Vector does not have constant size
    isa(x, Vector) && return Int
    size = try
        Core.sizeof(x)
    catch ex
        # Might return
        # "argument is an abstract type; size is indeterminate" or
        # "type does not have a fixed size"
        isa(ex, ErrorException) || rethrow(ex)
        return Int
    end
    return Const(size)
end
add_tfunc(Core.sizeof, 1, 1,
          function (@nospecialize(x),)
              isa(x, Const) && return _const_sizeof(x.val)
              isa(x, Conditional) && return _const_sizeof(Bool)
              isconstType(x) && return _const_sizeof(x.parameters[1])
              x !== DataType && isconcretetype(x) && return _const_sizeof(x)
              return Int
          end, 0)
old_nfields(@nospecialize x) = length((isa(x, DataType) ? x : typeof(x)).types)
add_tfunc(nfields, 1, 1,
    function (@nospecialize(x),)
        isa(x, Const) && return Const(old_nfields(x.val))
        isa(x, Conditional) && return Const(old_nfields(Bool))
        if isType(x)
            # TODO: remove with deprecation in builtins.c for nfields(::Type)
            p = x.parameters[1]
            issingletontype(p) && return Const(old_nfields(p))
        elseif isa(x, DataType) && !x.abstract && !(x.name === Tuple.name && isvatuple(x)) && x !== DataType
            if !(x.name === _NAMEDTUPLE_NAME && !isconcretetype(x))
                return Const(length(x.types))
            end
        end
        return Int
    end, 0)
add_tfunc(Core._expr, 1, INT_INF, (args...)->Expr, 100)
add_tfunc(applicable, 1, INT_INF, (@nospecialize(f), args...)->Bool, 100)
add_tfunc(Core.Intrinsics.arraylen, 1, 1, x->Int, 4)
add_tfunc(arraysize, 2, 2, (@nospecialize(a), @nospecialize(d))->Int, 4)
add_tfunc(pointerref, 3, 3,
          function (@nospecialize(a), @nospecialize(i), @nospecialize(align))
              a = widenconst(a)
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
          end, 4)
add_tfunc(pointerset, 4, 4, (@nospecialize(a), @nospecialize(v), @nospecialize(i), @nospecialize(align)) -> a, 5)

function typeof_tfunc(@nospecialize(t))
    if isa(t, Const)
        return Const(typeof(t.val))
    elseif isa(t, Conditional)
        return Const(Bool)
    elseif isType(t)
        tp = t.parameters[1]
        if issingletontype(tp)
            return Const(typeof(tp))
        else
            return Type
        end
    elseif isa(t, DataType)
        if isconcretetype(t) || isvarargtype(t)
            return Const(t)
        elseif t === Any
            return DataType
        else
            return Type{<:t}
        end
    elseif isa(t, Union)
        a = widenconst(typeof_tfunc(t.a))
        b = widenconst(typeof_tfunc(t.b))
        return Union{a, b}
    elseif isa(t, TypeVar) && !(Any <: t.ub)
        return typeof_tfunc(t.ub)
    elseif isa(t, UnionAll)
        return rewrap_unionall(widenconst(typeof_tfunc(unwrap_unionall(t))), t)
    else
        return DataType # typeof(anything)::DataType
    end
end
add_tfunc(typeof, 1, 1, typeof_tfunc, 0)
add_tfunc(typeassert, 2, 2,
          function (@nospecialize(v), @nospecialize(t))
              t, isexact = instanceof_tfunc(t)
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
              return typeintersect(v, t)
          end, 4)
add_tfunc(isa, 2, 2,
          function (@nospecialize(v), @nospecialize(t))
              t, isexact = instanceof_tfunc(t)
              if !has_free_typevars(t)
                  if t === Bottom
                      return Const(false)
                  elseif v ⊑ t
                      if isexact && isnotbrokensubtype(v, t)
                          return Const(true)
                      end
                  elseif isa(v, Const) || isa(v, Conditional) || isdispatchelem(v)
                      # this tests for knowledge of a leaftype appearing on the LHS
                      # (ensuring the isa is precise)
                      return Const(false)
                  elseif isexact && typeintersect(v, t) === Bottom
                      # similar to `isnotbrokensubtype` check above, `typeintersect(v, t)`
                      # can't be trusted for kind types so we do an extra check here
                      if !iskindtype(v)
                          return Const(false)
                      end
                  end
              end
              # TODO: handle non-leaftype(t) by testing against lower and upper bounds
              return Bool
          end, 0)
add_tfunc(<:, 2, 2,
          function (@nospecialize(a), @nospecialize(b))
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
          end, 0)

function const_datatype_getfield_tfunc(sv, fld)
    if (fld == DATATYPE_NAME_FIELDINDEX ||
            fld == DATATYPE_PARAMETERS_FIELDINDEX ||
            fld == DATATYPE_TYPES_FIELDINDEX ||
            fld == DATATYPE_SUPER_FIELDINDEX ||
            fld == DATATYPE_MUTABLE_FIELDINDEX)
        return AbstractEvalConstant(getfield(sv, fld))
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
    return length(t.types)
end


function try_compute_fieldidx(@nospecialize(typ), @nospecialize(field))
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
            (isa(sv, Module) && isa(name.val, Symbol)) || return false
            (isa(name.val, Symbol) || isa(name.val, Int)) || return false
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
                t = const_datatype_getfield_tfunc(sv, isa(nv, Symbol) ?
                      fieldindex(DataType, nv, false) : nv)
                t !== nothing && return t
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
            if (isa(sv, SimpleVector) || isimmutable(sv)) && isdefined(sv, nv)
                return AbstractEvalConstant(getfield(sv, nv))
            end
        end
        s = typeof(sv)
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
        # TODO: better approximate inference
        return Any
    end
    if isempty(s.types)
        return Bottom
    end
    if isa(name, Conditional)
        return Bottom # can't index fields with Bool
    end
    if !isa(name, Const)
        if !(Int <: name || Symbol <: name)
            return Bottom
        end
        if length(s.types) == 1
            return rewrap_unionall(unwrapva(s.types[1]), s00)
        end
        # union together types of all fields
        return tmerge_all(map(@nospecialize(t) -> rewrap_unionall(unwrapva(t), s00), s.types))
    end
    fld = name.val
    if isa(fld,Symbol)
        fld = fieldindex(s, fld, false)
    end
    if !isa(fld,Int)
        return Bottom
    end
    nf = length(s.types)
    if s <: Tuple && fld >= nf && isvarargtype(s.types[nf])
        return rewrap_unionall(unwrapva(s.types[nf]), s00)
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
    R = s.types[fld]
    if isempty(s.parameters)
        return R
    end
    return rewrap_unionall(R, s00)
end
add_tfunc(getfield, 2, 3, getfield_tfunc, 1)
add_tfunc(setfield!, 3, 3, (@nospecialize(o), @nospecialize(f), @nospecialize(v)) -> v, 3)
fieldtype_tfunc(@nospecialize(s0), @nospecialize(name), @nospecialize(inbounds)) =
    fieldtype_tfunc(s0, name)
function fieldtype_tfunc(@nospecialize(s0), @nospecialize(name))
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

    s = instanceof_tfunc(s0)[1]
    u = unwrap_unionall(s)

    if isa(u, Union)
        return tmerge(rewrap(fieldtype_tfunc(u.a, name), s),
                      rewrap(fieldtype_tfunc(u.b, name), s))
    end

    if !isa(u, DataType) || u.abstract
        return Type
    end
    if u.name === _NAMEDTUPLE_NAME && !isconcretetype(u)
        return Type
    end
    ftypes = u.types
    if isempty(ftypes)
        return Bottom
    end

    if !isa(name, Const)
        if !(Int <: name || Symbol <: name)
            return Bottom
        end
        return tmerge_all(Any[ fieldtype_tfunc(s0, Const(i)) for i = 1:length(ftypes) ])
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

    exact = (isa(s0, Const) || isType(s0)) && !has_free_typevars(s)
    ft = rewrap_unionall(ft, s)
    if exact
        return Const(ft)
    end
    return Type{<:ft}
end
add_tfunc(fieldtype, 2, 3, fieldtype_tfunc, 0)

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
                        return Union{}
                    end
                end
            else
                if !isType(ai)
                    if !isa(ai, Type) || typeintersect(ai, Type) != Union{}
                        hasnonType = true
                    else
                        return Union{}
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
                allconst &= issingletontype(aty)
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
    for i = 1:largs
        ai = args[i]
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
                v = TypeVar(:_)
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

function invoke_tfunc(@nospecialize(ft), @nospecialize(types), @nospecialize(argtype), sv::InferenceState)
    argument_mt(ft) === nothing && return Any
    argtype = typeintersect(types, argtype)
    if argtype === Bottom
        return Bottom
    end
    types = rewrap_unionall(Tuple{ft, unwrap_unionall(types).parameters...}, types)
    argtype = Tuple{ft, argtype.parameters...}
    entry = ccall(:jl_gf_invoke_lookup, Any, (Any, UInt), types, sv.params.world)
    if entry === nothing
        return Any
    end
    meth = entry.func
    (ti, env) = ccall(:jl_type_intersection_with_env, Any, (Any, Any), argtype, meth.sig)::SimpleVector
    rt, edge = typeinf_edge(meth::Method, ti, env, sv)
    edge !== nothing && add_backedge!(edge::MethodInstance, sv)
    return rt
end

# convert the dispatch tuple type argtype to the real (concrete) type of
# the tuple of those values
function tuple_tfunc(@nospecialize(argtype))
    if isa(argtype, DataType) && argtype.name === Tuple.name
        p = Vector{Any}()
        for x in argtype.parameters
            if isType(x)
                xparam = x.parameters[1]
                if issingletontype(xparam) || xparam === Bottom
                    push!(p, typeof(xparam))
                else
                    push!(p, Type)
                end
            else
                push!(p, x)
            end
        end
        t = Tuple{p...}
        # replace a singleton type with its equivalent Const object
        isdefined(t, :instance) && return Const(t.instance)
        return t
    end
    return argtype
end

function array_builtin_common_nothrow(argtypes, first_idx_idx)
    length(argtypes) >= 4 || return false
    (argtypes[0] ⊑ Bool && argtypes[1] ⊑ Array) || return false
    for i = first_idx_idx:length(argtypes)
        argtypes[i] ⊑ Int || return false
    end
    # If we have @inbounds (first argument is false), we're allowed to assume we don't throw
    (isa(argtypes[0], Const) && !argtypes[0].val) && return true
    # Else we can't really say anything here
    # TODO: In the future we may be able to track the shapes of arrays though
    # inference.
    return false
end

# Query whether the given builtin is guaranteed not to throw given the argtypes
function builtin_nothrow(@nospecialize(f), argtypes::Array{Any,1})
    (f === tuple || f === svec) && return true
    if f === arrayset
        array_builtin_common_nothrow(argtypes, 4) || return true
        # Additionally check element type compatibility
        a = widenconst(argtypes[2])
        # Check that we can determine the element type
        (isa(a, DataType) && isa(a.parameters[1], Type)) || return false
        # Check that the element type is compatible with the element we're assigning
        (argtypes[3] ⊑ a.parameters[1]::Type) || return false
        return true
    elseif f === arrayref
        return array_builtin_common_nothrow(argtypes, 3)
    elseif f === Core._expr
        return length(argtypes) >= 1 && argtypes[1] ⊑ Symbol
    elseif f === invoke
        return false
    elseif f === getfield
        return getfield_nothrow(argtypes)
    end
    return false
end

function builtin_tfunction(@nospecialize(f), argtypes::Array{Any,1},
                           sv::Union{InferenceState,Nothing}, params::Params = sv.params)
    isva = !isempty(argtypes) && isvarargtype(argtypes[end])
    if f === tuple
        for a in argtypes
            if !isa(a, Const)
                return tuple_tfunc(argtypes_to_type(argtypes))
            end
        end
        return Const(tuple(anymap(a->a.val, argtypes)...))
    elseif f === svec
        return SimpleVector
    elseif f === arrayset
        if length(argtypes) < 4
            isva && return Any
            return Bottom
        end
        return argtypes[2]
    elseif f === arrayref
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
                return invoke_tfunc(ft, sigty, argtypes_to_type(argtypes[3:end]), sv)
            end
        end
        return Any
    end
    if isva
        return Any
    end
    if isa(f, IntrinsicFunction)
        if is_pure_intrinsic_infer(f) && _all(@nospecialize(a) -> isa(a, Const), argtypes)
            argvals = anymap(a -> a.val, argtypes)
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
        fidx = findfirst(x->x===f, T_FFUNC_KEY)
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

# TODO: this function is a very buggy and poor model of the return_type function
# since abstract_call_gf_by_type is a very inaccurate model of _method and of typeinf_type,
# while this assumes that it is an absolutely precise and accurate and exact model of both
function return_type_tfunc(argtypes::Vector{Any}, vtypes::VarTable, sv::InferenceState)
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
                    astype = argtypes_to_type(argtypes_vec)
                    if isa(aft, Const)
                        rt = abstract_call(aft.val, (), argtypes_vec, vtypes, sv)
                    elseif isconstType(aft)
                        rt = abstract_call(aft.parameters[1], (), argtypes_vec, vtypes, sv)
                    else
                        rt = abstract_call_gf_by_type(nothing, argtypes_vec, astype, sv)
                    end
                    if isa(rt, Const)
                        # output was computed to be constant
                        return Const(typeof(rt.val))
                    elseif issingletontype(rt) || rt === Bottom
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
    return NOT_FOUND
end

# N.B.: typename maps type equivalence classes to a single value
function typename_static(@nospecialize(t))
    t = unwrap_unionall(t)
    return isType(t) ? _typename(t.parameters[1]) : Core.TypeName
end
typename_static(t::Const) = _typename(t.val)
