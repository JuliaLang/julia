# This file is a part of Julia. License is MIT: http://julialang.org/license

import Core: _apply, svec, apply_type, Builtin, IntrinsicFunction, MethodInstance

#### parameters limiting potentially-infinite types ####
const MAX_TYPEUNION_LEN = 3
const MAX_TYPE_DEPTH = 8

immutable InferenceParams
    world::UInt

    # optimization
    inlining::Bool

    # parameters limiting potentially-infinite types (configurable)
    MAX_TUPLETYPE_LEN::Int
    MAX_TUPLE_DEPTH::Int
    MAX_TUPLE_SPLAT::Int
    MAX_UNION_SPLITTING::Int

    # reasonable defaults
    function InferenceParams(world::UInt;
                    inlining::Bool = inlining_enabled(),
                    tupletype_len::Int = 15,
                    tuple_depth::Int = 4,
                    tuple_splat::Int = 16,
                    union_splitting::Int = 4)
        return new(world, inlining, tupletype_len,
            tuple_depth, tuple_splat, union_splitting)
    end
end

const UNION_SPLIT_MISMATCH_ERROR = false

# alloc_elim_pass! relies on `Slot_AssignedOnce | Slot_UsedUndef` being
# SSA. This should be true now but can break if we start to track conditional
# constants. e.g.
#
#     cond && (a = 1)
#     other_code()
#     cond && use(a)

# slot property bit flags
const Slot_Assigned     = 2
const Slot_AssignedOnce = 16
const Slot_UsedUndef    = 32

#### inference state types ####

immutable NotFound end
const NF = NotFound()
typealias LineNum Int
typealias VarTable Array{Any,1}

type VarState
    typ
    undef::Bool
    VarState(typ::ANY, undef::Bool) = new(typ, undef)
end

immutable Const
    val
    Const(v::ANY) = new(v)
end

function rewrap(t::ANY, u::ANY)
    isa(t, Const) && return t
    rewrap_unionall(t, u)
end

type InferenceState
    sp::SimpleVector     # static parameters
    label_counter::Int   # index of the current highest label for this function
    mod::Module
    currpc::LineNum

    # info on the state of inference and the linfo
    params::InferenceParams
    linfo::MethodInstance # used here for the tuple (specTypes, env, Method)
    src::CodeInfo
    min_valid::UInt
    max_valid::UInt
    nargs::Int
    stmt_types::Vector{Any}
    stmt_edges::Vector{Any}
    # return type
    bestguess #::Type
    # current active instruction pointers
    ip::IntSet
    pc´´::Int
    nstmts::Int
    # current exception handler info
    cur_hand #::Tuple{LineNum, Tuple{LineNum, ...}}
    handler_at::Vector{Any}
    n_handlers::Int
    # ssavalue sparsity and restart info
    ssavalue_uses::Vector{IntSet}
    ssavalue_init::Vector{Any}
    # call-graph edges connecting from a caller to a callee (and back)
    # we shouldn't need to iterate edges very often, so we use it to optimize the lookup from edge -> linenum
    # whereas backedges is optimized for iteration
    edges::ObjectIdDict # a Dict{InferenceState, Vector{LineNum}}
    backedges::Vector{Tuple{InferenceState, Vector{LineNum}}}
    # iteration fixed-point detection
    fixedpoint::Bool
    inworkq::Bool
    const_api::Bool
    const_ret::Bool

    # TODO: put these in InferenceParams (depends on proper multi-methodcache support)
    optimize::Bool
    cached::Bool

    inferred::Bool

    # src is assumed to be a newly-allocated CodeInfo, that can be modified in-place to contain intermediate results
    function InferenceState(linfo::MethodInstance, src::CodeInfo,
                            optimize::Bool, cached::Bool, params::InferenceParams)
        code = src.code::Array{Any,1}
        nl = label_counter(code) + 1
        toplevel = !isdefined(linfo, :def)

        if !toplevel && isempty(linfo.sparam_vals) && !isempty(linfo.def.sparam_syms)
            # linfo is unspecialized
            sp = Any[]
            sig = linfo.def.sig
            while isa(sig,UnionAll)
                push!(sp, sig.var)
                sig = sig.body
            end
            sp = svec(sp...)
        else
            sp = linfo.sparam_vals
        end

        nslots = length(src.slotnames)
        src.slottypes = Any[ Any for i = 1:nslots ]
        src.ssavaluetypes = Any[ NF for i = 1:(src.ssavaluetypes::Int) ]

        n = length(code)
        s_types = Any[ () for i = 1:n ]
        # initial types
        s_types[1] = Any[ VarState(Bottom, true) for i = 1:nslots ]
        s_edges = Any[ () for i = 1:n ]

        atypes = unwrap_unionall(linfo.specTypes)
        nargs = toplevel ? 0 : linfo.def.nargs
        la = nargs
        if la > 0
            if linfo.def.isva
                if atypes == Tuple
                    if la > 1
                        atypes = Tuple{Any[Any for i = 1:(la - 1)]..., Tuple.parameters[1]}
                    end
                    s_types[1][la] = VarState(Tuple, false)
                else
                    s_types[1][la] = VarState(rewrap(tuple_tfunc(limit_tuple_depth(params,
                                                                                   tupletype_tail(atypes, la))),
                                                     linfo.specTypes),
                                              false)
                end
                la -= 1
            end
        end

        laty = length(atypes.parameters)
        if laty > 0
            if laty > la
                laty = la
            end
            local lastatype
            atail = laty
            for i = 1:laty
                atyp = atypes.parameters[i]
                if i == laty && isvarargtype(atyp)
                    atyp = unwrap_unionall(atyp).parameters[1]
                    atail -= 1
                end
                if isa(atyp, TypeVar)
                    atyp = atyp.ub
                end
                if isa(atyp, DataType) && isdefined(atyp, :instance)
                    # replace singleton types with their equivalent Const object
                    atyp = Const(atyp.instance)
                else
                    atyp = rewrap_unionall(atyp, linfo.specTypes)
                end
                i == laty && (lastatype = atyp)
                s_types[1][i] = VarState(atyp, false)
            end
            for i = (atail + 1):la
                s_types[1][i] = VarState(lastatype, false)
            end
        else
            @assert la == 0 # wrong number of arguments
        end

        ssavalue_uses = find_ssavalue_uses(code)
        ssavalue_init = copy(src.ssavaluetypes::Vector{Any})

        # exception handlers
        cur_hand = ()
        handler_at = Any[ () for i=1:n ]
        n_handlers = 0

        W = IntSet()
        push!(W, 1) #initial pc to visit

        if !toplevel
            meth = linfo.def
            inmodule = meth.module
        else
            inmodule = current_module() # toplevel thunks are inferred in the current module
        end

        if cached && !toplevel
            min_valid = min_world(linfo.def)
            max_valid = max_world(linfo.def)
        else
            min_valid = typemax(UInt)
            max_valid = typemin(UInt)
        end
        frame = new(
            sp, nl, inmodule, 0, params,
            linfo, src, min_valid, max_valid,
            nargs, s_types, s_edges, Union{}, W, 1, n,
            cur_hand, handler_at, n_handlers,
            ssavalue_uses, ssavalue_init,
            ObjectIdDict(), # Dict{InferenceState, Vector{LineNum}}(),
            Vector{Tuple{InferenceState, Vector{LineNum}}}(),
            false, false, false, false, optimize, cached, false)
        push!(active, frame)
        nactive[] += 1
        return frame
    end
end

# create copies of the CodeInfo definition, and any fields that type-inference might modify
# TODO: post-inference see if we can swap back to the original arrays
function get_source(li::MethodInstance)
    src = ccall(:jl_copy_code_info, Ref{CodeInfo}, (Any,), li.def.source)
    if isa(src.code, Array{UInt8,1})
        src.code = ccall(:jl_uncompress_ast, Any, (Any, Any), li.def, src.code)
    else
        src.code = copy_exprargs(src.code)
    end
    src.slotnames = copy(src.slotnames)
    src.slotflags = copy(src.slotflags)
    return src
end

function get_staged(li::MethodInstance)
    src = ccall(:jl_code_for_staged, Any, (Any,), li)::CodeInfo
    if isa(src.code, Array{UInt8,1})
        src.code = ccall(:jl_uncompress_ast, Any, (Any, Any), li.def, src.code)
    end
    return src
end


#### current global inference state ####

const active = Vector{Any}() # set of all InferenceState objects being processed
const nactive = Array{Int}(())
nactive[] = 0
const workq = Vector{InferenceState}() # set of InferenceState objects that can make immediate progress

#### helper functions ####

@inline slot_id(s) = isa(s, SlotNumber) ? (s::SlotNumber).id : (s::TypedSlot).id # using a function to ensure we can infer this

# avoid cycle due to over-specializing `any` when used by inference
function _any(f::ANY, a)
    for x in a
        f(x) && return true
    end
    return false
end

function contains_is(itr, x::ANY)
    for y in itr
        if y === x
            return true
        end
    end
    return false
end

_topmod(sv::InferenceState) = _topmod(sv.mod)
_topmod(m::Module) = ccall(:jl_base_relative_to, Any, (Any,), m)::Module

function istopfunction(topmod, f::ANY, sym)
    if isdefined(Main, :Base) && isdefined(Main.Base, sym) && f === getfield(Main.Base, sym)
        return true
    elseif isdefined(topmod, sym) && f === getfield(topmod, sym)
        return true
    end
    return false
end

isknownlength(t::DataType) = !isvatuple(t) ||
    (length(t.parameters) > 0 && isa(unwrap_unionall(t.parameters[end]).parameters[2],Int))

# t[n:end]
tupletype_tail(t::ANY, n) = Tuple{t.parameters[n:end]...}


#### type-functions for builtins / intrinsics ####

const _Type_name = Type.body.name
isType(t::ANY) = isa(t, DataType) && (t::DataType).name === _Type_name

# true if Type is inlineable as constant (is a singleton)
isconstType(t::ANY) = isType(t) && (isleaftype(t.parameters[1]) || t.parameters[1] === Union{})

const IInf = typemax(Int) # integer infinity
const n_ifunc = reinterpret(Int32,arraylen)+1
const t_ifunc = Array{Tuple{Int,Int,Any},1}(n_ifunc)
const t_ffunc_key = Array{Function,1}(0)
const t_ffunc_val = Array{Tuple{Int,Int,Any},1}(0)
function add_tfunc(f::IntrinsicFunction, minarg::Int, maxarg::Int, tfunc::ANY)
    t_ifunc[reinterpret(Int32,f)+1] = (minarg, maxarg, tfunc)
end
function add_tfunc(f::Function, minarg::Int, maxarg::Int, tfunc::ANY)
    push!(t_ffunc_key, f)
    push!(t_ffunc_val, (minarg, maxarg, tfunc))
end

add_tfunc(throw, 1, 1, (x::ANY) -> Bottom)

# the inverse of typeof_tfunc
function instanceof_tfunc(t::ANY)
    # TODO improve
    if t === Bottom
        return t
    elseif isa(t, Const)
        if isa(t.val, Type)
            return t.val
        end
    elseif isType(t)
        return t.parameters[1]
    end
    return Any
end
box_tfunc(t::ANY, x::ANY) = instanceof_tfunc(t)
math_tfunc(x::ANY) = widenconst(x)
math_tfunc(x::ANY, y::ANY) = widenconst(x)
math_tfunc(x::ANY, y::ANY, z::ANY) = widenconst(x)
fptoui_tfunc(t::ANY, x::ANY) = box_tfunc(t, x)
fptosi_tfunc(t::ANY, x::ANY) = box_tfunc(t, x)
function fptoui_tfunc(x::ANY)
    T = widenconst(x)
    T === Float64 && return UInt64
    T === Float32 && return UInt32
    T === Float16 && return UInt16
    return Any
end
function fptosi_tfunc(x::ANY)
    T = widenconst(x)
    T === Float64 && return Int64
    T === Float32 && return Int32
    T === Float16 && return Int16
    return Any
end

    ## conversion ##
add_tfunc(box, 2, 2, box_tfunc)
add_tfunc(sext_int, 2, 2, box_tfunc)
add_tfunc(zext_int, 2, 2, box_tfunc)
add_tfunc(trunc_int, 2, 2, box_tfunc)
add_tfunc(fptoui, 1, 2, fptoui_tfunc)
add_tfunc(fptosi, 1, 2, fptosi_tfunc)
add_tfunc(uitofp, 2, 2, box_tfunc)
add_tfunc(sitofp, 2, 2, box_tfunc)
add_tfunc(fptrunc, 2, 2, box_tfunc)
add_tfunc(fpext, 2, 2, box_tfunc)
    ## checked conversion ##
add_tfunc(checked_trunc_sint, 2, 2, box_tfunc)
add_tfunc(checked_trunc_uint, 2, 2, box_tfunc)
add_tfunc(check_top_bit, 1, 1, math_tfunc)
    ## arithmetic ##
add_tfunc(neg_int, 1, 1, math_tfunc)
add_tfunc(add_int, 2, 2, math_tfunc)
add_tfunc(sub_int, 2, 2, math_tfunc)
add_tfunc(mul_int, 2, 2, math_tfunc)
add_tfunc(sdiv_int, 2, 2, math_tfunc)
add_tfunc(udiv_int, 2, 2, math_tfunc)
add_tfunc(srem_int, 2, 2, math_tfunc)
add_tfunc(urem_int, 2, 2, math_tfunc)
add_tfunc(neg_float, 1, 1, math_tfunc)
add_tfunc(add_float, 2, 2, math_tfunc)
add_tfunc(sub_float, 2, 2, math_tfunc)
add_tfunc(mul_float, 2, 2, math_tfunc)
add_tfunc(div_float, 2, 2, math_tfunc)
add_tfunc(rem_float, 2, 2, math_tfunc)
add_tfunc(fma_float, 3, 3, math_tfunc)
add_tfunc(muladd_float, 3, 3, math_tfunc)
    ## fast arithmetic ##
add_tfunc(neg_float_fast, 1, 1, math_tfunc)
add_tfunc(add_float_fast, 2, 2, math_tfunc)
add_tfunc(sub_float_fast, 2, 2, math_tfunc)
add_tfunc(mul_float_fast, 2, 2, math_tfunc)
add_tfunc(div_float_fast, 2, 2, math_tfunc)
add_tfunc(rem_float_fast, 2, 2, math_tfunc)
    ## bitwise operators ##
add_tfunc(and_int, 2, 2, math_tfunc)
add_tfunc(or_int, 2, 2, math_tfunc)
add_tfunc(xor_int, 2, 2, math_tfunc)
add_tfunc(not_int, 1, 1, math_tfunc)
add_tfunc(shl_int, 2, 2, math_tfunc)
add_tfunc(lshr_int, 2, 2, math_tfunc)
add_tfunc(ashr_int, 2, 2, math_tfunc)
add_tfunc(bswap_int, 1, 1, math_tfunc)
add_tfunc(ctpop_int, 1, 1, math_tfunc)
add_tfunc(ctlz_int, 1, 1, math_tfunc)
add_tfunc(cttz_int, 1, 1, math_tfunc)
add_tfunc(checked_sdiv_int, 2, 2, math_tfunc)
add_tfunc(checked_udiv_int, 2, 2, math_tfunc)
add_tfunc(checked_srem_int, 2, 2, math_tfunc)
add_tfunc(checked_urem_int, 2, 2, math_tfunc)
    ## functions ##
add_tfunc(abs_float, 1, 1, math_tfunc)
add_tfunc(copysign_float, 2, 2, math_tfunc)
add_tfunc(flipsign_int, 2, 2, math_tfunc)
add_tfunc(ceil_llvm, 1, 1, math_tfunc)
add_tfunc(floor_llvm, 1, 1, math_tfunc)
add_tfunc(trunc_llvm, 1, 1, math_tfunc)
add_tfunc(rint_llvm, 1, 1, math_tfunc)
add_tfunc(sqrt_llvm, 1, 1, math_tfunc)
add_tfunc(powi_llvm, 2, 2, math_tfunc)
add_tfunc(sqrt_llvm_fast, 1, 1, math_tfunc)
    ## same-type comparisons ##
cmp_tfunc(x::ANY, y::ANY) = Bool
add_tfunc(eq_int, 2, 2, cmp_tfunc)
add_tfunc(ne_int, 2, 2, cmp_tfunc)
add_tfunc(slt_int, 2, 2, cmp_tfunc)
add_tfunc(ult_int, 2, 2, cmp_tfunc)
add_tfunc(sle_int, 2, 2, cmp_tfunc)
add_tfunc(ule_int, 2, 2, cmp_tfunc)
add_tfunc(eq_float, 2, 2, cmp_tfunc)
add_tfunc(ne_float, 2, 2, cmp_tfunc)
add_tfunc(lt_float, 2, 2, cmp_tfunc)
add_tfunc(le_float, 2, 2, cmp_tfunc)
add_tfunc(fpiseq, 2, 2, cmp_tfunc)
add_tfunc(fpislt, 2, 2, cmp_tfunc)
add_tfunc(eq_float_fast, 2, 2, cmp_tfunc)
add_tfunc(ne_float_fast, 2, 2, cmp_tfunc)
add_tfunc(lt_float_fast, 2, 2, cmp_tfunc)
add_tfunc(le_float_fast, 2, 2, cmp_tfunc)

    ## checked arithmetic ##
chk_tfunc(x::ANY, y::ANY) = Tuple{widenconst(x), Bool}
add_tfunc(checked_sadd_int, 2, 2, chk_tfunc)
add_tfunc(checked_uadd_int, 2, 2, chk_tfunc)
add_tfunc(checked_ssub_int, 2, 2, chk_tfunc)
add_tfunc(checked_usub_int, 2, 2, chk_tfunc)
add_tfunc(checked_smul_int, 2, 2, chk_tfunc)
add_tfunc(checked_umul_int, 2, 2, chk_tfunc)
    ## other, misc intrinsics ##
add_tfunc(Core.Intrinsics.llvmcall, 3, IInf,
    (fptr::ANY, rt::ANY, at::ANY, a...) -> instanceof_tfunc(rt))
cglobal_tfunc(fptr::ANY) = Ptr{Void}
cglobal_tfunc(fptr::ANY, t::ANY) = (isType(t) ? Ptr{t.parameters[1]} : Ptr)
cglobal_tfunc(fptr::ANY, t::Const) = (isa(t.val, Type) ? Ptr{t.val} : Ptr)
add_tfunc(Core.Intrinsics.cglobal, 1, 2, cglobal_tfunc)
add_tfunc(Core.Intrinsics.select_value, 3, 3,
    function (cnd::ANY, x::ANY, y::ANY)
        if isa(cnd, Const)
            if cnd.val === true
                return x
            elseif cnd.val === false
                return y
            else
                return Bottom
            end
        end
        (Bool ⊑ cnd) || return Bottom
        return tmerge(x, y)
    end)
add_tfunc(===, 2, 2,
    function (x::ANY, y::ANY)
        if isa(x, Const) && isa(y, Const)
            return Const(x.val === y.val)
        elseif typeintersect(widenconst(x), widenconst(y)) === Bottom
            return Const(false)
        elseif (isa(x, Const) && y === typeof(x.val) && isdefined(y, :instance)) ||
               (isa(y, Const) && x === typeof(y.val) && isdefined(x, :instance))
            return Const(true)
        else
            return Bool
        end
    end)
add_tfunc(isdefined, 1, IInf, (args...)->Bool)
add_tfunc(Core.sizeof, 1, 1, x->Int)
add_tfunc(nfields, 1, 1,
    function (x::ANY)
        isa(x,Const) && return Const(nfields(x.val))
        if isType(x)
            isleaftype(x.parameters[1]) && return Const(nfields(x.parameters[1]))
        elseif isa(x,DataType) && !x.abstract && !(x.name === Tuple.name && isvatuple(x))
            return Const(length(x.types))
        end
        return Int
    end)
add_tfunc(Core._expr, 1, IInf, (args...)->Expr)
add_tfunc(applicable, 1, IInf, (f::ANY, args...)->Bool)
add_tfunc(Core.Intrinsics.arraylen, 1, 1, x->Int)
add_tfunc(arraysize, 2, 2, (a::ANY, d::ANY)->Int)
add_tfunc(pointerref, 3, 3,
          function (a::ANY, i::ANY, align::ANY)
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
          end)
add_tfunc(pointerset, 4, 4, (a::ANY, v::ANY, i::ANY, align::ANY) -> a)

function typeof_tfunc(t::ANY)
    if isa(t, Const)
        return Const(typeof(t.val))
    elseif isType(t)
        tp = t.parameters[1]
        if !isleaftype(tp)
            return DataType # typeof(Kind::Type)::DataType
        else
            return Const(typeof(tp)) # XXX: this is not necessarily true
        end
    elseif isa(t, DataType)
        if isleaftype(t) || isvarargtype(t)
            return Const(t)
        elseif t === Any
            return DataType
        else
            return Type{_} where _<:t
        end
    elseif isa(t, Union)
        a = widenconst(typeof_tfunc(t.a))
        b = widenconst(typeof_tfunc(t.b))
        return Union{a, b}
    elseif isa(t, TypeVar) && !(Any <: t.ub)
        return typeof_tfunc(t.ub)
    else
        return DataType # typeof(anything)::DataType
    end
end
add_tfunc(typeof, 1, 1, typeof_tfunc)
add_tfunc(typeassert, 2, 2,
          function (v::ANY, t::ANY)
              t = instanceof_tfunc(t)
              t === Any && return v
              if isa(v, Const)
                  if !has_free_typevars(t) && !isa(v.val, t)
                      return Bottom
                  end
                  return v
              end
              return typeintersect(v, t)
          end)
add_tfunc(isa, 2, 2,
          function (v::ANY, t::ANY)
              t = instanceof_tfunc(t)
              if t !== Any && !has_free_typevars(t)
                  if v ⊑ t
                      return Const(true)
                  elseif isa(v, Const) || isleaftype(v)
                      return Const(false)
                  end
              end
              return Bool
          end)
add_tfunc(issubtype, 2, 2,
          function (a::ANY, b::ANY)
              a = instanceof_tfunc(a)
              b = instanceof_tfunc(b)
              if a !== Any && b !== Any
                  if !has_free_typevars(a) && !has_free_typevars(b)
                      return Const(issubtype(a, b))
                  end
              end
              return Bool
          end)

function type_depth(t::ANY)
    if t === Bottom
        return 0
    elseif isa(t, Union)
        return max(type_depth(t.a), type_depth(t.b)) + 1
    elseif isa(t, DataType)
        return (t::DataType).depth
    elseif isa(t, UnionAll)
        if t.var.ub === Any
            return type_depth(t.body)
        end
        return max(type_depth(t.var.ub)+1, type_depth(t.body))
    end
    return 0
end

function limit_type_depth(t::ANY, d::Int, cov::Bool=true, var::Union{Void,TypeVar}=nothing)
    if isa(t,Union)
        if d > MAX_TYPE_DEPTH
            return Any
        end
        return Union{map(x->limit_type_depth(x, d+1, cov, var), (t.a,t.b))...}
    elseif isa(t,UnionAll)
        v = t.var
        if v.ub === Any
            return UnionAll(t.var, limit_type_depth(t.body, d, cov, var))
        end
        ub = limit_type_depth(v.ub, d+1, true, nothing)
        v2 = TypeVar(v.name, v.lb, ub)
        return UnionAll(v2, limit_type_depth(t{v2}, d, cov, var))
    elseif !isa(t,DataType)
        return t
    end
    P = t.parameters
    isempty(P) && return t
    if d > MAX_TYPE_DEPTH
        cov && return t.name.wrapper
        # TODO mutating a TypeVar is not great style
        var.ub = t.name.wrapper
        return var
    end
    stillcov = cov && (t.name === Tuple.name)
    if cov && !stillcov
        var = TypeVar(:_)
    end
    Q = map(x->limit_type_depth(x, d+1, stillcov, var), P)
    R = t.name.wrapper{Q...}
    return (cov && !stillcov) ? UnionAll(var, R) : R
end

const DataType_parameters_fieldindex = fieldindex(DataType, :parameters)
const DataType_types_fieldindex = fieldindex(DataType, :types)
const DataType_super_fieldindex = fieldindex(DataType, :super)

# returns (type, isexact)
function getfield_tfunc(s00::ANY, name)
    if isa(s00, TypeVar)
        s00 = s00.ub
    end
    s = unwrap_unionall(s00)
    if isType(s)
        p1 = s.parameters[1]
        if !isleaftype(p1)
            return Any
        end
        s = DataType # typeof(p1)
    elseif isa(s,Union)
        return rewrap(tmerge(getfield_tfunc(s.a, name), getfield_tfunc(s.b, name)),
                      s00)
    elseif isa(s,Const)
        sv = s.val
        if isa(name, Const)
            nv = name.val
            if isa(sv, UnionAll)
                if nv === :var || nv === 1
                    return Const(sv.var)
                elseif nv === :body || nv === 2
                    return Const(sv.body)
                end
            end
            if isa(sv, Module) && isa(nv, Symbol)
                return abstract_eval_global(sv, nv)
            end
            if (isa(sv, DataType) || isimmutable(sv)) && isdefined(sv, nv)
                return abstract_eval_constant(getfield(sv, nv))
            end
        end
        s = typeof(sv)
    end
    if !isa(s,DataType) || s.abstract
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
    if isempty(s.types)
        return Bottom
    end
    if !isa(name,Const)
        if !(Int <: name || Symbol <: name)
            return Bottom
        end
        if length(s.types) == 1
            return rewrap_unionall(unwrapva(s.types[1]), s00)
        end
        # union together types of all fields
        R = reduce(tmerge, Bottom, map(t->rewrap_unionall(unwrapva(t),s00), s.types))
        # do the same limiting as the known-symbol case to preserve type-monotonicity
        if isempty(s.parameters)
            return R
        end
        return limit_type_depth(R, 0)
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
    if isType(s00) && isleaftype(s00.parameters[1])
        sp = s00.parameters[1]
    elseif isa(s00, Const) && isa(s00.val, DataType)
        sp = s00.val
    else
        sp = nothing
    end
    if (sp !== nothing &&
        (fld == DataType_parameters_fieldindex ||
         fld == DataType_types_fieldindex ||
         fld == DataType_super_fieldindex))
        return Const(getfield(sp, fld))
    end
    R = s.types[fld]
    if isempty(s.parameters)
        return R
    end
    # TODO jb/subtype is this still necessary?
    # conservatively limit the type depth here,
    # since the UnionAll type bound is otherwise incorrect
    # in the current type system
    return rewrap_unionall(limit_type_depth(R, 0), s00)
end
add_tfunc(getfield, 2, 2, (s::ANY, name::ANY) -> getfield_tfunc(s, name))
add_tfunc(setfield!, 3, 3, (o::ANY, f::ANY, v::ANY) -> v)
function fieldtype_tfunc(s::ANY, name::ANY)
    if isa(s, Const)
        s = s.val
    elseif isType(s)
        s = s.parameters[1]
    else
        return Type
    end
    t = getfield_tfunc(s, name)
    if t === Bottom
        return t
    elseif isa(t, Const)
        return Const(typeof(t.val))
    elseif isleaftype(t)
        return Const(t)
    elseif isvarargtype(t)
        return Type{t}
    end
    return Type{_} where _<:t
end
add_tfunc(fieldtype, 2, 2, fieldtype_tfunc)

function valid_tparam(x::ANY)
    if isa(x,Tuple)
        for t in x
            !valid_tparam(t) && return false
        end
        return true
    end
    return isa(x,Int) || isa(x,Symbol) || isa(x,Bool) || (!isa(x,Type) && isbits(x))
end

has_free_typevars(t::ANY) = ccall(:jl_has_free_typevars, Cint, (Any,), t)!=0

# TODO: handle e.g. apply_type(T, R::Union{Type{Int32},Type{Float64}})
function apply_type_tfunc(headtypetype::ANY, args::ANY...)
    if isa(headtypetype, Const)
        headtype = headtypetype.val
    elseif isType(headtypetype) && isleaftype(headtypetype.parameters[1])
        headtype = headtypetype.parameters[1]
    else
        return Any
    end
    largs = length(args)
    if headtype === Union
        largs == 0 && return Const(Bottom)
        largs == 1 && return args[1]
        for i = 1:largs
            ai = args[i]
            if !isa(ai, Const) || !isa(ai.val, Type)
                if !isType(ai)
                    return Any
                end
            end
        end
        ty = Union{}
        allconst = true
        for i = 1:largs
            ai = args[i]
            if isType(ai)
                aty = ai.parameters[1]
                isleaftype(aty) || (allconst = false)
            else
                aty = (ai::Const).val
            end
            ty = Union{ty, aty}
        end
        return allconst ? Const(ty) : Type{ty}
    end
    istuple = (headtype == Tuple)
    if !istuple && !isa(headtype, UnionAll)
        # TODO: return `Bottom` for trying to apply a non-UnionAll
        return Any
    end
    uncertain = false
    tparams = Any[]
    for i = 1:largs
        ai = args[i]
        if isType(ai)
            aip1 = ai.parameters[1]
            uncertain |= has_free_typevars(aip1)
            push!(tparams, aip1)
        elseif isa(ai, Const) && (isa(ai.val, Type) || valid_tparam(ai.val))
            push!(tparams, ai.val)
        else
            # TODO: return `Bottom` for trying to apply a non-UnionAll
            #if !istuple && i-1 > length(headtype.parameters)
            #    # too many parameters for type
            #    return Bottom
            #end
            uncertain = true
            if istuple
                push!(tparams, Any)
            else
                # TODO: use rewrap_unionall to skip only the unknown parameters
                #push!(tparams, headtype.parameters[i-1])
                break
            end
        end
    end
    local appl
    try
        appl = apply_type(headtype, tparams...)
    catch ex
        # type instantiation might fail if one of the type parameters
        # doesn't match, which could happen if a type estimate is too coarse
        appl = headtype
        uncertain = true
    end
    !uncertain && return Const(appl)
    if type_too_complex(appl,0)
        return Type{_} where _<:headtype
    end
    !isa(appl,TypeVar) ? (Type{_} where _<:appl) : Type{appl}
end
add_tfunc(apply_type, 1, IInf, apply_type_tfunc)

@pure function type_typeof(v::ANY)
    if isa(v, Type)
        return Type{v}
    end
    return typeof(v)
end

function invoke_tfunc(f::ANY, types::ANY, argtype::ANY, sv::InferenceState)
    if !isleaftype(Type{types})
        return Any
    end
    argtype = typeintersect(types,limit_tuple_type(argtype, sv.params))
    if argtype === Bottom
        return Bottom
    end
    ft = type_typeof(f)
    types = Tuple{ft, types.parameters...}
    argtype = Tuple{ft, argtype.parameters...}
    entry = ccall(:jl_gf_invoke_lookup, Any, (Any, UInt), types, sv.params.world)
    if entry === nothing
        return Any
    end
    meth = entry.func
    (ti, env) = ccall(:jl_match_method, Ref{SimpleVector}, (Any, Any),
                      argtype, meth.sig)
    return typeinf_edge(meth::Method, ti, env, sv)
end

function tuple_tfunc(argtype::ANY)
    if isa(argtype, DataType) && argtype.name === Tuple.name
        p = Any[ isType(x) && !isa(x.parameters[1], TypeVar) ? typeof(x.parameters[1]) : x
                 for x in argtype.parameters ]
        t = Tuple{p...}
        # replace a singleton type with its equivalent Const object
        isdefined(t, :instance) && return Const(t.instance)
        return t
    end
    return argtype
end

function builtin_tfunction(f::ANY, argtypes::Array{Any,1}, sv::InferenceState)
    isva = !isempty(argtypes) && isvarargtype(argtypes[end])
    if f === tuple
        for a in argtypes
            if !isa(a, Const)
                return tuple_tfunc(limit_tuple_depth(sv.params, argtypes_to_type(argtypes)))
            end
        end
        return Const(tuple(map(a->a.val, argtypes)...))
    elseif f === svec
        return SimpleVector
    elseif f === arrayset
        if length(argtypes) < 3 && !isva
            return Bottom
        end
        a1 = argtypes[1]
        if isvarargtype(a1)
            return unwrap_unionall(a1).parameters[1]
        end
        return a1
    elseif f === arrayref
        if length(argtypes) < 2 && !isva
            return Bottom
        end
        a = widenconst(argtypes[1])
        if a <: Array
            if isa(a,DataType) && (isa(a.parameters[1],Type) || isa(a.parameters[1],TypeVar))
                # TODO: the TypeVar case should not be needed here
                a = a.parameters[1]
                return isa(a,TypeVar) ? a.ub : a
            elseif isa(a,UnionAll) && !has_free_typevars(a)
                unw = unwrap_unionall(a)
                if isa(unw,DataType)
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
        if length(argtypes)>1 && isa(argtypes[1], Const)
            af = argtypes[1].val
            sig = argtypes[2]
            if isa(sig, Const)
                sigty = sig.val
            elseif isType(sig)
                sigty = sig.parameters[1]
            else
                sigty = nothing
            end
            if isa(sigty, Type) && sigty <: Tuple
                return invoke_tfunc(af, sigty, argtypes_to_type(argtypes[3:end]), sv)
            end
        end
        return Any
    end
    if isva
        return Any
    end
    if isa(f, IntrinsicFunction)
        iidx = Int(reinterpret(Int32, f::IntrinsicFunction))+1
        if !isassigned(t_ifunc, iidx)
            # unknown/unhandled intrinsic (most fall in this category since most return an unboxed value)
            return Any
        end
        tf = t_ifunc[iidx]
    else
        fidx = findfirst(t_ffunc_key, f::Function)
        if fidx == 0
            # unknown/unhandled builtin or anonymous function
            return Any
        end
        tf = t_ffunc_val[fidx]
    end
    tf = tf::Tuple{Real, Real, Any}
    if !(tf[1] <= length(argtypes) <= tf[2])
        # wrong # of args
        return Bottom
    end
    return tf[3](argtypes...)
end

limit_tuple_depth(params::InferenceParams, t::ANY) = limit_tuple_depth_(params,t,0)

function limit_tuple_depth_(params::InferenceParams, t::ANY, d::Int)
    if isa(t,Union)
        # also limit within Union types.
        # may have to recur into other stuff in the future too.
        return Union{limit_tuple_depth_(params, t.a, d+1),
                     limit_tuple_depth_(params, t.b, d+1)}
    elseif isa(t,UnionAll)
        ub = limit_tuple_depth_(params, t.var.ub, d)
        if ub !== t.var.ub
            var = TypeVar(t.var.name, t.var.lb, ub)
            body = t{var}
        else
            var = t.var
            body = t.body
        end
        body = limit_tuple_depth_(params, body, d)
        return UnionAll(var, body)
    elseif !(isa(t,DataType) && t.name === Tuple.name)
        return t
    elseif d > params.MAX_TUPLE_DEPTH
        return Tuple
    end
    p = map(x->limit_tuple_depth_(params,x,d+1), t.parameters)
    Tuple{p...}
end

limit_tuple_type = (t::ANY, params::InferenceParams) -> limit_tuple_type_n(t, params.MAX_TUPLETYPE_LEN)

function limit_tuple_type_n(t::ANY, lim::Int)
    if isa(t,UnionAll)
        return UnionAll(t.var, limit_tuple_type_n(t.body, lim))
    end
    p = t.parameters
    n = length(p)
    if n > lim
        tail = reduce(typejoin, Bottom, Any[p[lim:(n-1)]..., unwrapva(p[n])])
        return Tuple{p[1:(lim-1)]..., Vararg{tail}}
    end
    return t
end


#### recursing into expression ####

function abstract_call_gf_by_type(f::ANY, atype::ANY, sv::InferenceState)
    tm = _topmod(sv)
    # don't consider more than N methods. this trades off between
    # compiler performance and generated code performance.
    # typically, considering many methods means spending lots of time
    # obtaining poor type information.
    # It is important for N to be >= the number of methods in the error()
    # function, so we can still know that error() is always Bottom.
    # here I picked 4.
    argtype = limit_tuple_type(atype, sv.params)
    argtypes = unwrap_unionall(argtype).parameters
    ft = argtypes[1] # TODO: ccall jl_first_argument_datatype here
    isa(ft, DataType) || return Any # the function being called is unknown. can't properly handle this backedge right now
    isdefined(ft.name, :mt) || return Any # not callable. should be Bottom, but can't track this backedge right now
    if ft.name === _Type_name
        tname = ft.parameters[1]
        if isa(tname, TypeVar)
            tname = tname.ub
        end
        tname = unwrap_unionall(tname)
        if !isa(tname, DataType)
            # can't track the backedge to the ctor right now
            # for things like Union
            return Any
        end
    end
    min_valid = UInt[typemin(UInt)]
    max_valid = UInt[typemax(UInt)]
    applicable = _methods_by_ftype(argtype, 4, sv.params.world, min_valid, max_valid)
    rettype = Bottom
    if applicable === false
        # this means too many methods matched
        return Any
    end
    x::Array{Any,1} = applicable
    fullmatch = false
    for (m::SimpleVector) in x
        sig = m[1]
        sigtuple = unwrap_unionall(sig)::DataType
        method = m[3]::Method
        sparams = m[2]::SimpleVector
        recomputesvec = false
        if !fullmatch && typeseq(sig, argtype)
            fullmatch = true
        end

        # limit argument type tuple growth
        msig = unwrap_unionall(m[3].sig)
        lsig = length(msig.parameters)
        ls = length(sigtuple.parameters)
        td = type_depth(sig)
        # look at the existing edges to detect growing argument lists
        mightlimitlength = ls > lsig + 1
        mightlimitdepth = td > 2

        limitlength = false
        if mightlimitlength
            for (callee, _) in sv.edges
                callee = callee::InferenceState
                if method === callee.linfo.def && ls > length(unwrap_unionall(callee.linfo.specTypes).parameters)
                    limitlength = true
                    break
                end
            end
        end

        # limit argument type size growth
        if mightlimitlength || mightlimitdepth
            # TODO: FIXME: this heuristic depends on non-local state making type-inference unpredictable
            for infstate in active
                infstate === nothing && continue
                infstate = infstate::InferenceState
                if isdefined(infstate.linfo, :def) && method === infstate.linfo.def
                    if mightlimitlength && ls > length(unwrap_unionall(infstate.linfo.specTypes).parameters)
                        limitlength = true
                    end
                    if mightlimitdepth && td > type_depth(infstate.linfo.specTypes)
                        # impose limit if we recur and the argument types grow beyond MAX_TYPE_DEPTH
                        if td > MAX_TYPE_DEPTH
                            sig = limit_type_depth(sig, 0)
                            sigtuple = unwrap_unionall(sig)
                            recomputesvec = true
                            break
                        else
                            p1, p2 = sigtuple.parameters, unwrap_unionall(infstate.linfo.specTypes).parameters
                            if length(p2) == ls
                                limitdepth = false
                                newsig = Array{Any}(ls)
                                for i = 1:ls
                                    if p1[i] <: Function && type_depth(p1[i]) > type_depth(p2[i]) &&
                                        isa(p1[i],DataType)
                                        # if a Function argument is growing (e.g. nested closures)
                                        # then widen to the outermost function type. without this
                                        # inference fails to terminate on do_quadgk.
                                        newsig[i] = p1[i].name.wrapper
                                        limitdepth  = true
                                    else
                                        newsig[i] = limit_type_depth(p1[i], 1)
                                    end
                                end
                                if limitdepth
                                    sigtuple = Tuple{newsig...}
                                    sig = rewrap_unionall(sigtuple, sig)
                                    recomputesvec = true
                                    break
                                end
                            end
                        end
                    end
                end
            end
        end

        # limit length based on size of definition signature.
        # for example, given function f(T, Any...), limit to 3 arguments
        # instead of the default (MAX_TUPLETYPE_LEN)
        if limitlength
            if !istopfunction(tm, f, :promote_typeof)
                fst = sigtuple.parameters[lsig + 1]
                allsame = true
                # allow specializing on longer arglists if all the trailing
                # arguments are the same, since there is no exponential
                # blowup in this case.
                for i = (lsig + 2):ls
                    if sigtuple.parameters[i] != fst
                        allsame = false
                        break
                    end
                end
                if !allsame
                    sigtuple = limit_tuple_type_n(sigtuple, lsig + 1)
                    sig = rewrap_unionall(sigtuple, sig)
                    recomputesvec = true
                end
            end
        end

        # if sig changed, may need to recompute the sparams environment
        if recomputesvec && !isempty(sparams)
            recomputed = ccall(:jl_type_intersection_env, Ref{SimpleVector}, (Any, Any), sig, method.sig)
            sig = recomputed[1]
            if !isa(unwrap_unionall(sig), DataType) # probably Union{}
                rettype = Any
                break
            end
            sparams = recomputed[2]::SimpleVector
        end
        rt = typeinf_edge(method, sig, sparams, sv)
        rettype = tmerge(rettype, rt)
        if rettype === Any
            break
        end
    end
    if !(fullmatch || rettype === Any)
        # also need an edge to the method table in case something gets
        # added that did not intersect with any existing method
        add_mt_backedge(ft.name.mt, argtype, sv)
        update_valid_age!(min_valid[1], max_valid[1], sv)
    end
    if isempty(x)
        # TODO: this is needed because type intersection is wrong in some cases
        return Any
    end
    #print("=> ", rettype, "\n")
    return rettype
end

# determine whether `ex` abstractly evals to constant `c`
function abstract_evals_to_constant(ex, c::ANY, vtypes, sv)
    av = abstract_eval(ex, vtypes, sv)
    return isa(av,Const) && av.val === c
end

# `types` is an array of inferred types for expressions in `args`.
# if an expression constructs a container (e.g. `svec(x,y,z)`),
# refine its type to an array of element types. returns an array of
# arrays of types, or `nothing`.
function precise_container_types(args, types, vtypes::VarTable, sv)
    n = length(args)
    assert(n == length(types))
    result = Vector{Any}(n)
    for i = 1:n
        ai = args[i]
        ti = types[i]
        tti = widenconst(ti)
        tti = unwrap_unionall(tti)
        if isa(ti, Const) && (isa(ti.val, SimpleVector) || isa(ti.val, Tuple))
            result[i] = Any[ abstract_eval_constant(x) for x in ti.val ]
        elseif isa(ai, Expr) && ai.head === :call && (abstract_evals_to_constant(ai.args[1], svec, vtypes, sv) ||
                                                      abstract_evals_to_constant(ai.args[1], tuple, vtypes, sv))
            aa = ai.args
            result[i] = Any[ (isa(aa[j],Expr) ? aa[j].typ : abstract_eval(aa[j],vtypes,sv)) for j=2:length(aa) ]
            if _any(isvarargtype, result[i])
                return nothing
            end
        elseif isa(tti, Union)
            return nothing
        elseif isa(tti,DataType) && tti <: Tuple
            if i == n
                if isvatuple(tti) && length(tti.parameters) == 1
                    result[i] = Any[Vararg{unwrapva(tti.parameters[1])}]
                else
                    result[i] = tti.parameters
                end
            elseif isknownlength(tti)
                result[i] = tti.parameters
            else
                return nothing
            end
        elseif tti <: AbstractArray && i == n
            result[i] = Any[Vararg{eltype(tti)}]
        else
            return nothing
        end
    end
    return result
end

# do apply(af, fargs...), where af is a function value
function abstract_apply(af::ANY, fargs, aargtypes::Vector{Any}, vtypes::VarTable, sv)
    ctypes = precise_container_types(fargs, aargtypes, vtypes, sv)
    if ctypes !== nothing
        # apply with known func with known tuple types
        # can be collapsed to a call to the applied func
        at = append_any(Any[Const(af)], ctypes...)
        n = length(at)
        if n-1 > sv.params.MAX_TUPLETYPE_LEN
            tail = foldl((a,b)->tmerge(a,unwrapva(b)), Bottom, at[sv.params.MAX_TUPLETYPE_LEN+1:n])
            at = vcat(at[1:sv.params.MAX_TUPLETYPE_LEN], Any[Vararg{widenconst(tail)}])
        end
        return abstract_call(af, (), at, vtypes, sv)
    end
    # apply known function with unknown args => f(Any...)
    return abstract_call(af, (), Any[Const(af), Vararg{Any}], vtypes, sv)
end

function return_type_tfunc(argtypes::ANY, vtypes::VarTable, sv::InferenceState)
    if length(argtypes) == 3
        tt = argtypes[3]
        if isa(tt, Const) || (isType(tt) && !has_free_typevars(tt))
            aft = argtypes[2]
            if isa(aft, Const) || (isType(aft) && !has_free_typevars(aft)) ||
                   (isleaftype(aft) && !(aft <: Builtin) && !(aft <: IntrinsicFunction))
                af_argtype = isa(tt, Const) ? tt.val : tt.parameters[1]
                if isa(af_argtype, DataType) && af_argtype <: Tuple
                    argtypes_vec = Any[aft, af_argtype.parameters...]
                    if isa(aft, Const)
                        rt = abstract_call(aft.val, (), argtypes_vec, vtypes, sv)
                    elseif isconstType(aft)
                        rt = abstract_call(aft.parameters[1], (), argtypes_vec, vtypes, sv)
                    else
                        rt = abstract_call_gf_by_type(nothing, argtypes_to_type(argtypes_vec), sv)
                    end
                    if isa(rt, Const)
                        # output was computed to be constant
                        return Const(typeof(rt.val))
                    elseif isleaftype(rt)
                        # output type was known for certain
                        return Const(rt)
                    elseif (isa(tt, Const) || isconstType(tt)) &&
                           (isa(aft, Const) || isconstType(aft))
                        # input arguments were known for certain
                        return Const(rt)
                    else
                        return Type{R} where R <: rt
                    end
                end
            end
        end
    end
    return NF
end

function pure_eval_call(f::ANY, argtypes::ANY, atype::ANY, vtypes::VarTable, sv::InferenceState)
    for i = 2:length(argtypes)
        a = argtypes[i]
        if !(isa(a,Const) || isconstType(a))
            return false
        end
    end

    min_valid = UInt[typemin(UInt)]
    max_valid = UInt[typemax(UInt)]
    meth = _methods_by_ftype(atype, 1, sv.params.world, min_valid, max_valid)
    if meth === false || length(meth) != 1
        return false
    end
    meth = meth[1]::SimpleVector
    method = meth[3]::Method
    # TODO: check pure on the inferred thunk
    if method.isstaged || !method.source.pure
        return false
    end

    args = Any[ (a=argtypes[i]; isa(a,Const) ? a.val : a.parameters[1]) for i in 2:length(argtypes) ]
    try
        value = Core._apply_pure(f, args)
        # TODO: add some sort of edge(s)
        return abstract_eval_constant(value)
    catch
        return false
    end
end

argtypes_to_type(argtypes::Array{Any,1}) = Tuple{map(widenconst, argtypes)...}

_Pair_name = nothing
function Pair_name()
    global _Pair_name
    if _Pair_name === nothing
        if isdefined(Main, :Base) && isdefined(Main.Base, :Pair)
            _Pair_name = Main.Base.Pair.body.body.name
        end
    end
    return _Pair_name
end

function abstract_call(f::ANY, fargs, argtypes::Vector{Any}, vtypes::VarTable, sv::InferenceState)
    if f === _apply
        length(fargs) > 1 || return Any
        aft = argtypes[2]
        if isa(aft, Const)
            af = aft.val
        else
            if isType(aft) && isleaftype(aft.parameters[1])
                af = aft.parameters[1]
            elseif isleaftype(aft) && isdefined(aft, :instance)
                af = aft.instance
            else
                # TODO jb/functions: take advantage of case where non-constant `af`'s type is known
                return Any
            end
        end
        return abstract_apply(af, fargs[3:end], argtypes[3:end], vtypes, sv)
    end
    for i=2:(length(argtypes)-1)
        if isvarargtype(argtypes[i])
            return Any
        end
    end
    if isa(f,Builtin) || isa(f,IntrinsicFunction)
        rt = builtin_tfunction(f, argtypes[2:end], sv)
        return isa(rt, TypeVar) ? rt.ub : rt
    elseif f === Core.kwfunc
        if length(fargs) == 2
            ft = widenconst(argtypes[2])
            if isa(ft,DataType) && isdefined(ft.name, :mt) && isdefined(ft.name.mt, :kwsorter)
                return Const(ft.name.mt.kwsorter)
            end
        end
        return Any
    elseif f === UnionAll
        if length(fargs) == 3 && isa(argtypes[2], Const)
            tv = argtypes[2].val
            if isa(tv, TypeVar)
                if isa(argtypes[3], Const)
                    body = argtypes[3].val
                elseif isType(argtypes[3])
                    body = argtypes[3].parameters[1]
                else
                    return Any
                end
                return abstract_eval_constant(UnionAll(tv, body))
            end
        end
        return Any
    elseif f === return_type
        rt_rt = return_type_tfunc(argtypes, vtypes, sv)
        if rt_rt !== NF
            return rt_rt
        end
    end

    tm = _topmod(sv)
    if length(argtypes)>2 && argtypes[3] ⊑ Int
        at2 = widenconst(argtypes[2])
        if (at2 <: Tuple ||
            (isa(at2, DataType) && (at2::DataType).name === Pair_name()))
            # allow tuple indexing functions to take advantage of constant
            # index arguments.
            if istopfunction(tm, f, :getindex)
                return getfield_tfunc(argtypes[2], argtypes[3])
            elseif istopfunction(tm, f, :next)
                t1 = widenconst(getfield_tfunc(argtypes[2], argtypes[3]))
                return t1===Bottom ? Bottom : Tuple{t1, Int}
            elseif istopfunction(tm, f, :indexed_next)
                t1 = widenconst(getfield_tfunc(argtypes[2], argtypes[3]))
                return t1===Bottom ? Bottom : Tuple{t1, Int}
            end
        end
    end

    atype = argtypes_to_type(argtypes)
    t = pure_eval_call(f, argtypes, atype, vtypes, sv)
    t !== false && return t

    if istopfunction(tm, f, :promote_type) || istopfunction(tm, f, :typejoin)
        return Type
    end

    if sv.params.inlining
        # need to model the special inliner for ^
        # to ensure we have added the same edge
        if isdefined(Main, :Base) &&
            ((isdefined(Main.Base, :^) && f === Main.Base.:^) ||
             (isdefined(Main.Base, :.^) && f === Main.Base.:.^)) &&
            length(argtypes) == 3 && (argtypes[3] ⊑ Int32 || argtypes[3] ⊑ Int64)

            a1 = argtypes[2]
            basenumtype = Union{corenumtype, Main.Base.Complex64, Main.Base.Complex128, Main.Base.Rational}
            if a1 ⊑ basenumtype
                ftimes = Main.Base.:*
                ta1 = widenconst(a1)
                abstract_call_gf_by_type(ftimes, Tuple{typeof(ftimes), ta1, ta1}, sv)
            end
        end
    end
    return abstract_call_gf_by_type(f, atype, sv)
end

function abstract_eval_call(e::Expr, vtypes::VarTable, sv::InferenceState)
    argtypes = Any[abstract_eval(a, vtypes, sv) for a in e.args]
    #print("call ", e.args[1], argtypes, "\n\n")
    for x in argtypes
        x === Bottom && return Bottom
    end
    ft = argtypes[1]
    if isa(ft, Const)
        f = ft.val
    else
        if isType(ft) && isleaftype(ft.parameters[1])
            f = ft.parameters[1]
        elseif isleaftype(ft) && isdefined(ft, :instance)
            f = ft.instance
        else
            for i = 2:(length(argtypes)-1)
                if isvarargtype(argtypes[i])
                    return Any
                end
            end
            # non-constant function, but type is known
            if (isleaftype(ft) || ft <: Type) && !(ft <: Builtin) && !(ft <: IntrinsicFunction)
                return abstract_call_gf_by_type(nothing, argtypes_to_type(argtypes), sv)
            end
            return Any
        end
    end
    return abstract_call(f, e.args, argtypes, vtypes, sv)
end

const _Ref_name = Ref.body.name

function abstract_eval(e::ANY, vtypes::VarTable, sv::InferenceState)
    if isa(e, QuoteNode)
        return abstract_eval_constant((e::QuoteNode).value)
    elseif isa(e, SSAValue)
        return abstract_eval_ssavalue(e::SSAValue, sv.src)
    elseif isa(e, Slot)
        return vtypes[slot_id(e)].typ
    elseif isa(e, Symbol)
        return abstract_eval_global(sv.mod, e)
    elseif isa(e,GlobalRef)
        return abstract_eval_global(e.mod, e.name)
    end

    if !isa(e, Expr)
        return abstract_eval_constant(e)
    end
    e = e::Expr
    if e.head === :call
        t = abstract_eval_call(e, vtypes, sv)
    elseif e.head === :null
        t = Void
    elseif e.head === :new
        t = instanceof_tfunc(abstract_eval(e.args[1], vtypes, sv))
        for i = 2:length(e.args)
            if abstract_eval(e.args[i], vtypes, sv) === Bottom
                rt = Bottom
            end
        end
    elseif e.head === :&
        abstract_eval(e.args[1], vtypes, sv)
        t = Any
    elseif e.head === :foreigncall
        rt = e.args[2]
        if isdefined(sv.linfo, :def)
            spsig = sv.linfo.def.sig
            if isa(spsig, UnionAll)
                env = data_pointer_from_objref(sv.linfo.sparam_vals) + sizeof(Ptr{Void})
                rt = ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), e.args[2], spsig, env)
            end
        end
        abstract_eval(e.args[1], vtypes, sv)
        for i = 3:length(e.args)
            if abstract_eval(e.args[i], vtypes, sv) === Bottom
                t = Bottom
            end
        end
        if rt === Bottom
            t = Bottom
        elseif isa(rt, Type)
            t = rt
            if isa(t, DataType) && (t::DataType).name === _Ref_name
                t = t.parameters[1]
                if t === Any
                    t = Bottom # a return type of Box{Any} is invalid
                end
            end
        else
            t = Any
        end
    elseif e.head === :static_parameter
        n = e.args[1]
        t = Any
        if n <= length(sv.sp)
            val = sv.sp[n]
            if isa(val, TypeVar)
                # static param bound to typevar
                # if the tvar does not refer to anything more specific than Any,
                # the static param might actually be an integer, symbol, etc.
                if !(Any <: val.ub)
                    t = UnionAll(val, Type{val})
                end
            else
                t = abstract_eval_constant(val)
            end
        end
    elseif e.head === :method
        t = (length(e.args) == 1) ? Any : Void
    elseif e.head === :copyast
        t = abstract_eval(e.args[1], vtypes, sv)
    elseif e.head === :inert
        return abstract_eval_constant(e.args[1])
    elseif e.head === :invoke
        error("type inference data-flow error: tried to double infer a function")
    else
        t = Any
    end
    if isa(t,TypeVar)
        # no need to use a typevar as the type of an expression
        t = t.ub
    end
    if isa(t, DataType) && isdefined(t, :instance)
        # replace singleton types with their equivalent Const object
        t = Const(t.instance)
    end
    e.typ = t
    return t
end

const Type_Array = Const(Array)

function abstract_eval_constant(x::ANY)
    if x === Array
        return Type_Array
    end
    return Const(x)
end

function abstract_eval_global(M::Module, s::Symbol)
    if isdefined(M,s) && isconst(M,s)
        return abstract_eval_constant(getfield(M,s))
    end
    return Any
end

function abstract_eval_ssavalue(s::SSAValue, src::CodeInfo)
    typ = src.ssavaluetypes[s.id + 1]
    if typ === NF
        return Bottom
    end
    return typ
end


#### handling for statement-position expressions ####

type StateUpdate
    var::Union{Slot,SSAValue}
    vtype
    state::VarTable
end

function abstract_interpret(e::ANY, vtypes::VarTable, sv::InferenceState)
    !isa(e, Expr) && return vtypes
    # handle assignment
    if e.head === :(=)
        t = abstract_eval(e.args[2], vtypes, sv)
        t === Bottom && return ()
        lhs = e.args[1]
        if isa(lhs, Slot) || isa(lhs, SSAValue)
            # don't bother for GlobalRef
            return StateUpdate(lhs, VarState(t, false), vtypes)
        end
    elseif e.head === :call || e.head === :foreigncall
        t = abstract_eval(e, vtypes, sv)
        t === Bottom && return ()
    elseif e.head === :gotoifnot
        t = abstract_eval(e.args[1], vtypes, sv)
        t === Bottom && return ()
    elseif e.head === :method
        fname = e.args[1]
        if isa(fname, Slot)
            return StateUpdate(fname, VarState(Any, false), vtypes)
        end
    end
    return vtypes
end

function type_too_complex(t::ANY, d)
    if d > MAX_TYPE_DEPTH
        return true
    elseif isa(t,Union)
        return type_too_complex(t.a, d+1) || type_too_complex(t.b, d+1)
    elseif isa(t,TypeVar)
        return type_too_complex(t.lb,d+1) || type_too_complex(t.ub,d+1)
    elseif isa(t,UnionAll)
        return type_too_complex(t.var, d) || type_too_complex(t.body, d)
    elseif isa(t,DataType)
        for x in (t.parameters)::SimpleVector
            if type_too_complex(x, d+1)
                return true
            end
        end
    end
    return false
end

## lattice operators

function ⊑(a::ANY, b::ANY)
    a === NF && return true
    b === NF && return false
    if isa(a,Const)
        if isa(b,Const)
            return a.val === b.val
        end
        return isa(a.val, b)
    elseif isa(b,Const)
        return a === Bottom
    elseif !(isa(a,Type) || isa(a,TypeVar)) || !(isa(b,Type) || isa(b,TypeVar))
        return a === b
    else
        return a <: b
    end
end

widenconst(c::Const) = isa(c.val, Type) ? Type{c.val} : typeof(c.val)
widenconst(t::ANY) = t

issubstate(a::VarState, b::VarState) = (a.typ ⊑ b.typ && a.undef <= b.undef)

# Meta expression head, these generally can't be deleted even when they are
# in a dead branch but can be ignored when analyzing uses/liveness.
is_meta_expr_head(head::Symbol) =
    (head === :inbounds || head === :boundscheck || head === :meta ||
     head === :line)
is_meta_expr(ex::Expr) = is_meta_expr_head(ex.head)

function tmerge(typea::ANY, typeb::ANY)
    typea ⊑ typeb && return typeb
    typeb ⊑ typea && return typea
    typea, typeb = widenconst(typea), widenconst(typeb)
    typea === typeb && return typea
    if !(isa(typea,Type) || isa(typea,TypeVar)) || !(isa(typeb,Type) || isa(typeb,TypeVar))
        return Any
    end
    if (typea <: Tuple) && (typeb <: Tuple)
        if isa(typea, DataType) && isa(typeb, DataType) && length(typea.parameters) == length(typeb.parameters) && !isvatuple(typea) && !isvatuple(typeb)
            return typejoin(typea, typeb)
        end
        if isa(typea, Union) || isa(typeb, Union) || (isa(typea,DataType) && length(typea.parameters)>3) ||
            (isa(typeb,DataType) && length(typeb.parameters)>3)
            # widen tuples faster (see #6704), but not too much, to make sure we can infer
            # e.g. (t::Union{Tuple{Bool},Tuple{Bool,Int}})[1]
            return Tuple
        end
    end
    u = Union{typea, typeb}
    if unionlen(u) > MAX_TYPEUNION_LEN || type_too_complex(u, 0)
        # don't let type unions get too big
        # TODO: something smarter, like a common supertype
        return Any
    end
    return u
end

function smerge(sa::Union{NotFound,VarState}, sb::Union{NotFound,VarState})
    sa === sb && return sa
    sa === NF && return sb
    sb === NF && return sa
    issubstate(sa, sb) && return sb
    issubstate(sb, sa) && return sa
    return VarState(tmerge(sa.typ, sb.typ), sa.undef | sb.undef)
end

@inline tchanged(n::ANY, o::ANY) = o === NF || (n !== NF && !(n ⊑ o))
@inline schanged(n::ANY, o::ANY) = (n !== o) && (o === NF || (n !== NF && !issubstate(n, o)))

function stupdate!(state::Tuple{}, changes::StateUpdate)
    newst = copy(changes.state)
    if isa(changes.var, Slot)
        newst[slot_id(changes.var::Slot)] = changes.vtype
    end
    return newst
end

function stupdate!(state::VarTable, change::StateUpdate)
    if !isa(change.var, Slot)
        return stupdate!(state, change.state)
    end
    newstate = false
    changeid = slot_id(change.var::Slot)
    for i = 1:length(state)
        if i == changeid
            newtype = change.vtype
        else
            newtype = change.state[i]
        end
        oldtype = state[i]
        if schanged(newtype, oldtype)
            newstate = state
            state[i] = smerge(oldtype, newtype)
        end
    end
    return newstate
end

function stupdate!(state::VarTable, changes::VarTable)
    newstate = false
    for i = 1:length(state)
        newtype = changes[i]
        oldtype = state[i]
        if schanged(newtype, oldtype)
            newstate = state
            state[i] = smerge(oldtype, newtype)
        end
    end
    return newstate
end

stupdate!(state::Tuple{}, changes::VarTable) = copy(changes)

stupdate!(state::Tuple{}, changes::Tuple{}) = false

function stupdate1!(state::VarTable, change::StateUpdate)
    if !isa(change.var, Slot)
        return false
    end
    i = slot_id(change.var::Slot)
    newtype = change.vtype
    oldtype = state[i]
    if schanged(newtype, oldtype)
        state[i] = smerge(oldtype, newtype)
        return true
    end
    return false
end


#### helper functions for typeinf initialization and looping ####

function label_counter(body)
    l = -1
    for b in body
        if isa(b,LabelNode) && (b::LabelNode).label > l
            l = (b::LabelNode).label
        end
    end
    return l
end
genlabel(sv) = LabelNode(sv.label_counter += 1)

function find_ssavalue_uses(body)
    uses = IntSet[]
    for line = 1:length(body)
        find_ssavalue_uses(body[line], uses, line)
    end
    return uses
end
function find_ssavalue_uses(e::ANY, uses, line)
    if isa(e,SSAValue)
        id = (e::SSAValue).id + 1
        while length(uses) < id
            push!(uses, IntSet())
        end
        push!(uses[id], line)
    elseif isa(e,Expr)
        b = e::Expr
        head = b.head
        is_meta_expr_head(head) && return
        if head === :(=)
            if isa(b.args[1],SSAValue)
                id = (b.args[1]::SSAValue).id + 1
                while length(uses) < id
                    push!(uses, IntSet())
                end
            end
            find_ssavalue_uses(b.args[2], uses, line)
            return
        end
        for a in b.args
            find_ssavalue_uses(a, uses, line)
        end
    end
end

function newvar!(sv::InferenceState, typ::ANY)
    id = length(sv.src.ssavaluetypes)
    push!(sv.src.ssavaluetypes, typ)
    return SSAValue(id)
end

inlining_enabled() = (JLOptions().can_inline == 1)
coverage_enabled() = (JLOptions().code_coverage != 0)

# TODO: track the worlds for which this InferenceState
# is being used, and split it if the WIP requires it?
function converge_valid_age!(sv::InferenceState)
    # push the validity range of sv into its fixedpoint callers
    # recursing as needed to cover the graph
    for (i, _) in sv.backedges
        if i.fixedpoint
            updated = false
            if i.min_valid < sv.min_valid
                i.min_valid = sv.min_valid
                updated = true
            end
            if i.max_valid > sv.max_valid
                i.max_valid = sv.max_valid
                updated = true
            end
            @assert !isdefined(i.linfo, :def) || !i.cached || i.min_valid <= i.params.world <= i.max_valid "invalid age range update"
            if updated
                converge_valid_age!(i)
            end
        end
    end
    nothing
end

# work towards converging the valid age range for sv
function update_valid_age!(min_valid::UInt, max_valid::UInt, sv::InferenceState)
    sv.min_valid = max(sv.min_valid, min_valid)
    sv.max_valid = min(sv.max_valid, max_valid)
    @assert !isdefined(sv.linfo, :def) || !sv.cached || sv.min_valid <= sv.params.world <= sv.max_valid "invalid age range update"
    nothing
end
update_valid_age!(edge::InferenceState, sv::InferenceState) = update_valid_age!(edge.min_valid, edge.max_valid, sv)
update_valid_age!(li::MethodInstance, sv::InferenceState) = update_valid_age!(min_world(li), max_world(li), sv)

# temporarily accumulate our edges to later add as backedges in the callee
function add_backedge(li::MethodInstance, caller::InferenceState)
    isdefined(caller.linfo, :def) || return # don't add backedges to toplevel exprs
    if caller.stmt_edges[caller.currpc] === ()
        caller.stmt_edges[caller.currpc] = []
    end
    push!(caller.stmt_edges[caller.currpc], li)
    update_valid_age!(li, caller)
    nothing
end

# temporarily accumulate our no method errors to later add as backedges in the callee method table
function add_mt_backedge(mt::MethodTable, typ::ANY, caller::InferenceState)
    isdefined(caller.linfo, :def) || return # don't add backedges to toplevel exprs
    if caller.stmt_edges[caller.currpc] === ()
        caller.stmt_edges[caller.currpc] = []
    end
    push!(caller.stmt_edges[caller.currpc], mt)
    push!(caller.stmt_edges[caller.currpc], typ)
    nothing
end

# add the real backedges now
function finalize_backedges(frame::InferenceState)
    toplevel = !isdefined(frame.linfo, :def)
    if !toplevel && frame.cached && frame.max_valid == typemax(UInt)
        caller = frame.linfo
        for edges in frame.stmt_edges
            i = 1
            while i <= length(edges)
                to = edges[i]
                if isa(to, MethodInstance)
                    ccall(:jl_method_instance_add_backedge, Void, (Any, Any), to, caller)
                    i += 1
                else
                    typeassert(to, MethodTable)
                    typ = edges[i + 1]
                    ccall(:jl_method_table_add_backedge, Void, (Any, Any, Any), to, typ, caller)
                    i += 2
                end
            end
        end
    end
end

function code_for_method(method::Method, atypes::ANY, sparams::SimpleVector, world::UInt, preexisting::Bool=false)
    if world < min_world(method) || world > max_world(method)
        return nothing
    end
    if method.isstaged && !isleaftype(atypes)
        # don't call staged functions on abstract types.
        # (see issues #8504, #10230)
        # we can't guarantee that their type behavior is monotonic.
        # XXX: this test is wrong if Types (such as DataType or Bottom) are present
        return nothing
    end
    if preexisting
        if method.specializations !== nothing
            # check cached specializations
            # for an existing result stored there
            return ccall(:jl_specializations_lookup, Any, (Any, Any, UInt), method, atypes, world)
        end
        return nothing
    end
    return ccall(:jl_specializations_get_linfo, Ref{MethodInstance}, (Any, Any, Any, UInt), method, atypes, sparams, world)
end

function typeinf_active(linfo::MethodInstance)
    for infstate in active
        infstate === nothing && continue
        infstate = infstate::InferenceState
        if linfo === infstate.linfo
            return infstate
        end
    end
    return nothing
end

function add_backedge(frame::InferenceState, caller::InferenceState, currpc::Int)
    update_valid_age!(frame, caller)
    if haskey(caller.edges, frame)
        Ws = caller.edges[frame]::Vector{Int}
        if !(currpc in Ws)
            push!(Ws, currpc)
        end
    else
        Ws = Int[currpc]
        caller.edges[frame] = Ws
        push!(frame.backedges, (caller, Ws))
    end
end

# build (and start inferring) the inference frame for the linfo
function typeinf_frame(linfo::MethodInstance, caller, optimize::Bool, cached::Bool,
                       params::InferenceParams)
    # println(params.world, ' ', linfo)
    if cached && linfo.inInference
        # inference on this signature may be in progress,
        # find the corresponding frame in the active list
        frame = typeinf_active(linfo)
        # TODO: this assertion seems iffy
        assert(frame !== nothing)
    else
        # inference not started yet, make a new frame for a new lambda
        if linfo.def.isstaged
            try
                # user code might throw errors – ignore them
                src = get_staged(linfo)
            catch
                return nothing
            end
        else
            src = get_source(linfo)
        end
        cached && (linfo.inInference = true)
        frame = InferenceState(linfo, src, optimize, cached, params)
    end
    frame = frame::InferenceState

    if isa(caller, InferenceState)
        # if we were called from inside inference, the caller will be the InferenceState object
        # for which the edge was required
        @assert caller.currpc > 0
        add_backedge(frame, caller, caller.currpc)
    end
    typeinf_loop(frame)
    return frame
end

# compute (and cache) an inferred AST and return the current best estimate of the result type
function typeinf_edge(method::Method, atypes::ANY, sparams::SimpleVector, caller::InferenceState)
    code = code_for_method(method, atypes, sparams, caller.params.world)
    code === nothing && return Any
    code = code::MethodInstance
    if isdefined(code, :inferred)
        # return rettype if the code is already inferred
        # staged functions make this hard since they have two "inferred" conditions,
        # so need to check whether the code itself is also inferred
        inf = code.inferred
        if !isa(inf, CodeInfo) || (inf::CodeInfo).inferred
            add_backedge(code, caller)
            if isdefined(code, :inferred_const)
                return abstract_eval_constant(code.inferred_const)
            else
                return code.rettype
            end
        end
    end
    frame = typeinf_frame(code, caller, true, true, caller.params)
    frame === nothing && return Any
    frame = frame::InferenceState
    return frame.bestguess
end

#### entry points for inferring a MethodInstance given a type signature ####

# compute an inferred AST and return type
function typeinf_code(method::Method, atypes::ANY, sparams::SimpleVector,
                      optimize::Bool, cached::Bool, params::InferenceParams)
    code = code_for_method(method, atypes, sparams, params.world)
    code === nothing && return (nothing, Any)
    return typeinf_code(code::MethodInstance, optimize, cached, params)
end
function typeinf_code(linfo::MethodInstance, optimize::Bool, cached::Bool,
                      params::InferenceParams)
    for i = 1:2 # test-and-lock-and-test
        i == 2 && ccall(:jl_typeinf_begin, Void, ())
        if cached && isdefined(linfo, :inferred)
            # see if this code already exists in the cache
            # staged functions make this hard since they have two "inferred" conditions,
            # so need to check whether the code itself is also inferred
            inf = linfo.inferred
            if min_world(linfo) <= params.world <= max_world(linfo)
                if linfo.jlcall_api == 2
                    method = linfo.def
                    tree = ccall(:jl_new_code_info_uninit, Ref{CodeInfo}, ())
                    tree.code = Any[ Expr(:return, QuoteNode(inf)) ]
                    tree.slotnames = Any[ compiler_temp_sym for i = 1:method.nargs ]
                    tree.slotflags = UInt8[ 0 for i = 1:method.nargs ]
                    tree.slottypes = nothing
                    tree.ssavaluetypes = 0
                    tree.inferred = true
                    tree.pure = true
                    tree.inlineable = true
                    i == 2 && ccall(:jl_typeinf_end, Void, ())
                    return svec(linfo, tree, linfo.rettype)
                elseif isa(inf, CodeInfo)
                    if (inf::CodeInfo).inferred
                        i == 2 && ccall(:jl_typeinf_end, Void, ())
                        return svec(linfo, inf, linfo.rettype)
                    end
                end
            end
        end
    end
    frame = typeinf_frame(linfo, nothing, optimize, cached, params)
    ccall(:jl_typeinf_end, Void, ())
    frame === nothing && return svec(nothing, nothing, Any)
    frame = frame::InferenceState
    frame.inferred || return svec(nothing, nothing, Any)
    frame.cached || return svec(nothing, frame.src, widenconst(frame.bestguess))
    return svec(frame.linfo, frame.src, widenconst(frame.bestguess))
end

# compute (and cache) an inferred AST and return the inferred return type
function typeinf_type(method::Method, atypes::ANY, sparams::SimpleVector,
                      cached::Bool, params::InferenceParams)
    code = code_for_method(method, atypes, sparams, params.world)
    code === nothing && return nothing
    code = code::MethodInstance
    for i = 1:2 # test-and-lock-and-test
        i == 2 && ccall(:jl_typeinf_begin, Void, ())
        if cached && isdefined(code, :inferred)
            # see if this rettype already exists in the cache
            # staged functions make this hard since they have two "inferred" conditions,
            # so need to check whether the code itself is also inferred
            inf = code.inferred
            if !isa(inf, CodeInfo) || (inf::CodeInfo).inferred
                i == 2 && ccall(:jl_typeinf_end, Void, ())
                return code.rettype
            end
        end
    end
    frame = typeinf_frame(code, nothing, cached, cached, params)
    ccall(:jl_typeinf_end, Void, ())
    frame === nothing && return nothing
    frame = frame::InferenceState
    frame.inferred || return nothing
    return widenconst(frame.bestguess)
end

function typeinf_ext(linfo::MethodInstance, world::UInt)
    if isdefined(linfo, :def)
        # method lambda - infer this specialization via the method cache
        return typeinf_code(linfo, true, true, InferenceParams(world))
    else
        # toplevel lambda - infer directly
        linfo.inInference = true
        ccall(:jl_typeinf_begin, Void, ())
        frame = InferenceState(linfo, linfo.inferred::CodeInfo,
                               true, true, InferenceParams(world))
        typeinf_loop(frame)
        ccall(:jl_typeinf_end, Void, ())
        @assert frame.inferred # TODO: deal with this better
        @assert frame.linfo === linfo
        return svec(linfo, frame.src, linfo.rettype)
    end
end

#### do the work of inference ####

in_typeinf_loop = false
function typeinf_loop(frame)
    global in_typeinf_loop
    if in_typeinf_loop
        frame.inworkq || typeinf_frame(frame)
        return
    end
    try
        in_typeinf_loop = true
        # the core type-inference algorithm
        # processes everything in workq,
        # and returns when there is nothing left
        while nactive[] > 0
            while active[end] === nothing
                pop!(active)
            end
            if isempty(workq)
                frame = active[end]::InferenceState
            else
                frame = pop!(workq)
            end
            typeinf_frame(frame)
            if isempty(workq) && nactive[] > 0
                # nothing in active has an edge that hasn't reached a fixed-point
                # so all of them can be considered finished now
                fplist = Any[]
                for i in active
                    i === nothing && continue
                    i = i::InferenceState
                    if i.fixedpoint
                        push!(fplist, i)
                        i.inworkq = true
                    end
                end
                for i in length(fplist):-1:1
                    # optimize and record the results
                    # the reverse order makes it more likely to inline a callee into its caller
                    optimize(fplist[i]::InferenceState) # this may add incomplete work to active
                end
                for i in fplist
                    # push valid ages from each node across the graph cycle
                    converge_valid_age!(i::InferenceState)
                end
                for i in fplist
                    # record the results
                    finish(i::InferenceState)
                end
                for i in fplist
                    # update and record all of the back edges for the finished world
                    finalize_backedges(i::InferenceState)
                end
            end
        end
        # cleanup the active queue
        empty!(active)
    #    while active[end] === nothing
    #        # this pops everything, but with exaggerated care just in case
    #        # something managed to add something to the queue at the same time
    #        # (or someone decides to use an alternative termination condition)
    #        pop!(active)
    #    end
        in_typeinf_loop = false
    catch ex
        println("WARNING: An error occurred during inference. Type inference is now partially disabled.")
        println(ex)
        ccall(:jlbacktrace, Void, ())
    end
    nothing
end

global_sv = nothing
function typeinf_frame(frame)
    global global_sv # TODO: actually pass this to all functions that need it
    last_global_sv = global_sv
    global_sv = frame
    @assert !frame.inferred
    frame.inworkq = true
    W = frame.ip
    s = frame.stmt_types
    n = frame.nstmts
    while frame.pc´´ <= n
        # make progress on the active ip set
        local pc::Int = frame.pc´´ # current program-counter
        while true # inner loop optimizes the common case where it can run straight from pc to pc + 1
            #print(pc,": ",s[pc],"\n")
            local pc´::Int = pc + 1 # next program-counter (after executing instruction)
            if pc == frame.pc´´
                # need to update pc´´ to point at the new lowest instruction in W
                min_pc = next(W, Int64(pc) + 1)[1]
                if min_pc >= W.limit
                    frame.pc´´ = max(min_pc, n + 1)
                else
                    frame.pc´´ = min_pc
                end
            end
            delete!(W, pc)
            frame.currpc = pc
            frame.cur_hand = frame.handler_at[pc]
            frame.stmt_edges[pc] === () || empty!(frame.stmt_edges[pc])
            stmt = frame.src.code[pc]
            changes = abstract_interpret(stmt, s[pc]::Array{Any,1}, frame)
            if changes === ()
                break # this line threw an error and so there is no need to continue
                # changes = s[pc]
            end
            if frame.cur_hand !== () && isa(changes, StateUpdate)
                # propagate new type info to exception handler
                # the handling for Expr(:enter) propagates all changes from before the try/catch
                # so this only needs to propagate any changes
                l = frame.cur_hand[1]
                if stupdate1!(s[l]::VarTable, changes::StateUpdate) !== false
                    if l < frame.pc´´
                        frame.pc´´ = l
                    end
                    push!(W, l)
                end
            end
            if isa(changes, StateUpdate) && isa((changes::StateUpdate).var, SSAValue)
                # directly forward changes to an SSAValue to the applicable line
                changes = changes::StateUpdate
                id = (changes.var::SSAValue).id + 1
                new = changes.vtype.typ
                old = frame.src.ssavaluetypes[id]
                if old === NF || !(new ⊑ old)
                    frame.src.ssavaluetypes[id] = tmerge(old, new)
                    for r in frame.ssavalue_uses[id]
                        if s[r] !== () # s[r] === () => unreached statement
                            if r < frame.pc´´
                                frame.pc´´ = r
                            end
                            push!(W, r)
                        end
                    end
                end
            elseif isa(stmt, GotoNode)
                pc´ = (stmt::GotoNode).label
            elseif isa(stmt, Expr)
                stmt = stmt::Expr
                hd = stmt.head
                if hd === :gotoifnot
                    condt = abstract_eval(stmt.args[1], s[pc], frame)
                    condval = isa(condt, Const) ? condt.val : nothing
                    l = stmt.args[2]::Int
                    # constant conditions
                    if condval === true
                    elseif condval === false
                        pc´ = l
                    else
                        # general case
                        frame.handler_at[l] = frame.cur_hand
                        newstate = stupdate!(s[l], changes)
                        if newstate !== false
                            # add else branch to active IP list
                            if l < frame.pc´´
                                frame.pc´´ = l
                            end
                            push!(W, l)
                            s[l] = newstate
                        end
                    end
                elseif hd === :return
                    pc´ = n + 1
                    rt = abstract_eval(stmt.args[1], s[pc], frame)
                    if tchanged(rt, frame.bestguess)
                        # new (wider) return type for frame
                        frame.bestguess = tmerge(frame.bestguess, rt)
                        for (caller, callerW) in frame.backedges
                            # notify backedges of updated type information
                            for caller_pc in callerW
                                if caller.stmt_types[caller_pc] !== ()
                                    if caller_pc < caller.pc´´
                                        caller.pc´´ = caller_pc
                                    end
                                    push!(caller.ip, caller_pc)
                                end
                            end
                        end
                        unmark_fixedpoint(frame)
                    end
                elseif hd === :enter
                    l = stmt.args[1]::Int
                    frame.cur_hand = (l, frame.cur_hand)
                    # propagate type info to exception handler
                    l = frame.cur_hand[1]
                    old = s[l]
                    new = s[pc]::Array{Any,1}
                    newstate = stupdate!(old, new)
                    if newstate !== false
                        if l < frame.pc´´
                            frame.pc´´ = l
                        end
                        push!(W, l)
                        s[l] = newstate
                    end
                    typeassert(s[l], VarTable)
                    frame.handler_at[l] = frame.cur_hand
                elseif hd === :leave
                    for i = 1:((stmt.args[1])::Int)
                        frame.cur_hand = frame.cur_hand[2]
                    end
                end
            end
            pc´ > n && break # can't proceed with the fast-path fall-through
            frame.handler_at[pc´] = frame.cur_hand
            newstate = stupdate!(s[pc´], changes)
            if isa(stmt, GotoNode) && frame.pc´´ < pc´
                # if we are processing a goto node anyways,
                # (such as a terminator for a loop, if-else, or try block),
                # consider whether we should jump to an older backedge first,
                # to try to traverse the statements in approximate dominator order
                if newstate !== false
                    s[pc´] = newstate
                end
                push!(W, pc´)
                pc = frame.pc´´
            elseif newstate !== false
                s[pc´] = newstate
                pc = pc´
            elseif pc´ in W
                pc = pc´
            else
                break
            end
        end
    end

    # with no active ip's, type inference on frame is done if there are no outstanding (unfinished) edges
    #@assert isempty(W)
    @assert !frame.inferred
    finished = isempty(frame.edges)
    if isempty(workq)
        # oops, there's a cycle somewhere in the `edges` graph
        # so we've run out off the tree and will need to start work on the loop
        frame.fixedpoint = true
    end

    if finished || frame.fixedpoint
        if finished
            optimize(frame)
            finish(frame)
            finalize_backedges(frame)
        else # fixedpoint propagation
            for (i, _) in frame.edges
                i = i::InferenceState
                if !i.fixedpoint
                    update_valid_age!(i, frame) # work towards converging age at the same time
                    if !i.inworkq
                        push!(workq, i)
                        i.inworkq = true
                    end
                    i.fixedpoint = true
                end
            end
        end
    end
    frame.inworkq = false
    global_sv = last_global_sv
    nothing
end

function unmark_fixedpoint(frame::InferenceState)
    # type information changed for frame, so its edges are no longer stuck
    # recursively unmark any nodes that had previously been thought to be at a fixedpoint
    # based upon (recursively) assuming that frame was stuck
    if frame.fixedpoint
        frame.fixedpoint = false
        for (i, _) in frame.backedges
            unmark_fixedpoint(i)
        end
    end
end


#### finalize and record the result of running type inference ####

function isinlineable(m::Method, src::CodeInfo)
    inlineable = false
    cost = 1000
    if m.module === _topmod(m.module)
        name = m.name
        sig = m.sig
        if ((name === :+ || name === :* || name === :min || name === :max) &&
            isa(sig,DataType) &&
            sig == Tuple{sig.parameters[1],Any,Any,Any,Vararg{Any}})
            inlineable = true
        elseif (name === :next || name === :done || name === :unsafe_convert ||
                name === :cconvert)
            cost ÷= 4
        end
    end
    if !inlineable
        body = Expr(:block)
        body.args = src.code
        inlineable = inline_worthy(body, cost)
    end
    return inlineable
end

# inference completed on `me`
# now converge the optimization work
function optimize(me::InferenceState)
    for (i, _) in me.edges
        i = i::InferenceState
        @assert i.fixedpoint
    end
    # below may call back into inference and
    # see this InferenceState is in an incomplete state
    # set `inworkq` to prevent it from trying to look
    # at the object in any detail
    @assert me.inworkq

    # annotate fulltree with type information
    gt = me.src.ssavaluetypes
    for i = 1:length(gt)
        if gt[i] === NF
            gt[i] = Union{}
        end
    end
    type_annotate!(me)

    # run optimization passes on fulltree
    force_noinline = false
    if me.optimize
        # This pass is required for the AST to be valid in codegen
        # if any `SSAValue` is created by type inference. Ref issue #6068
        # This (and `reindex_labels!`) needs to be run for `!me.optimize`
        # if we start to create `SSAValue` in type inference when not
        # optimizing and use unoptimized IR in codegen.
        gotoifnot_elim_pass!(me)
        inlining_pass!(me)
        void_use_elim_pass!(me)
        alloc_elim_pass!(me)
        getfield_elim_pass!(me)
        # Clean up for `alloc_elim_pass!` and `getfield_elim_pass!`
        void_use_elim_pass!(me)
        do_coverage = coverage_enabled()
        meta_elim_pass!(me.src.code::Array{Any,1}, me.src.propagate_inbounds, do_coverage)
        # Pop metadata before label reindexing
        force_noinline = popmeta!(me.src.code::Array{Any,1}, :noinline)[1]
        reindex_labels!(me)
    end
    widen_all_consts!(me.src)

    if isa(me.bestguess, Const) || isconstType(me.bestguess)
        me.const_ret = true
        ispure = me.src.pure
        if !ispure && length(me.src.code) < 10
            ispure = true
            for stmt in me.src.code
                if !statement_effect_free(stmt, me.src, me.mod)
                    ispure = false
                    break
                end
            end
            if ispure
                for fl in me.src.slotflags
                    if (fl & Slot_UsedUndef) != 0
                        ispure = false
                        break
                    end
                end
            end
        end
        me.src.pure = ispure

        do_coverage = coverage_enabled()
        if ispure && !do_coverage
            # use constant calling convention
            # Do not emit `jlcall_api == 2` if coverage is enabled
            # so that we don't need to add coverage support
            # to the `jl_call_method_internal` fast path
            # Still set pure flag to make sure `inference` tests pass
            # and to possibly enable more optimization in the future
            me.const_api = true
            force_noinline || (me.src.inlineable = true)
        end
    end

    # determine and cache inlineability
    if !me.src.inlineable && !force_noinline && isdefined(me.linfo, :def)
        me.src.inlineable = isinlineable(me.linfo.def, me.src)
    end
    me.src.inferred = true
    nothing
end

# inference completed on `me`
# update the MethodInstance and notify the edges
function finish(me::InferenceState)
    me.currpc = 1 # used by add_backedge
    if me.cached
        toplevel = !isdefined(me.linfo, :def)
        if !toplevel
            min_valid = me.min_valid
            max_valid = me.max_valid
        else
            min_valid = UInt(0)
            max_valid = UInt(0)
        end

        # check if the existing me.linfo metadata is also sufficient to describe the current inference result
        # to decide if it is worth caching it again (which would also clear any generated code)
        already_inferred = false
        if isdefined(me.linfo, :inferred)
            inf = me.linfo.inferred
            if !isa(inf, CodeInfo) || (inf::CodeInfo).inferred
                if min_world(me.linfo) == min_valid && max_world(me.linfo) == max_valid
                    already_inferred = true
                end
            end
        end

        if !already_inferred
            const_flags = (me.const_ret) << 1 | me.const_api
            if me.const_ret
                if isa(me.bestguess, Const)
                    inferred_const = (me.bestguess::Const).val
                else
                    @assert isconstType(me.bestguess)
                    inferred_const = me.bestguess.parameters[1]
                end
            else
                inferred_const = nothing
            end
            if me.const_api
                # use constant calling convention
                inferred_result = inferred_const
            else
                inferred_result = me.src
            end

            if !toplevel
                if !me.const_api
                    keeptree = me.src.inlineable || ccall(:jl_is_cacheable_sig, Int32, (Any, Any, Any),
                        me.linfo.specTypes, me.linfo.def.sig, me.linfo.def) != 0
                    if !keeptree
                        inferred_result = nothing
                    else
                        # compress code for non-toplevel thunks
                        inferred_result.code = ccall(:jl_compress_ast, Any, (Any, Any), me.linfo.def, inferred_result.code)
                    end
                end
            end
            cache = ccall(:jl_set_method_inferred, Ref{MethodInstance}, (Any, Any, Any, Any, Int32, UInt, UInt),
                me.linfo, widenconst(me.bestguess), inferred_const, inferred_result,
                const_flags, min_valid, max_valid)
            if cache !== me.linfo
                me.linfo.inInference = false
                me.linfo = cache
            end
        end
    end

    # lazy-delete the item from active for several reasons:
    # efficiency, correctness, and recursion-safety
    nactive[] -= 1
    active[findlast(active, me)] = nothing

    # update all of the callers by traversing the backedges
    for (i, _) in me.backedges
        if !me.fixedpoint || !i.fixedpoint
            # wake up each backedge, unless both me and it already reached a fixed-point (cycle resolution stage)
            delete!(i.edges, me)
            i.inworkq || push!(workq, i)
            i.inworkq = true
        end
        add_backedge(me.linfo, i)
    end

    # finalize and record the linfo result
    me.cached && (me.linfo.inInference = false)
    me.inferred = true
    nothing
end

function record_slot_type!(id, vt::ANY, slottypes)
    if vt !== Bottom
        otherTy = slottypes[id]
        if otherTy === Bottom
            slottypes[id] = vt
        elseif otherTy !== Any && !typeseq(otherTy, vt)
            slottypes[id] = Any
        end
    end
end

function eval_annotate(e::ANY, vtypes::ANY, sv::InferenceState, undefs::Array{Bool,1}, pass::Int)
    if isa(e, Slot)
        id = slot_id(e)
        s = vtypes[id]
        vt = widenconst(s.typ)
        if pass == 1
            # first pass: find used-undef variables and type-constant variables
            if s.undef
                undefs[id] = true
            end
            record_slot_type!(id, vt, sv.src.slottypes)
            return e
        end
        # second pass: add type annotations where needed
        return vt === sv.src.slottypes[id] ? e : TypedSlot(id, vt)
    end

    if !isa(e,Expr)
        return e
    end

    e = e::Expr
    head = e.head
    if is_meta_expr_head(head) || head === :const
        return e
    elseif head === :(=)
        e.args[2] = eval_annotate(e.args[2], vtypes, sv, undefs, pass)
        return e
    end
    i0 = head === :method ? 2 : 1
    for i=i0:length(e.args)
        subex = e.args[i]
        if !(isa(subex,Number) || isa(subex,AbstractString))
            e.args[i] = eval_annotate(subex, vtypes, sv, undefs, pass)
        end
    end
    return e
end

# annotate types of all symbols in AST
function type_annotate!(sv::InferenceState)
    src = sv.src
    states = sv.stmt_types
    nargs = sv.nargs
    nslots = length(states[1])
    for i = 1:nargs
        src.slottypes[i] = widenconst(states[1][i].typ)
    end
    for i = nargs+1:nslots
        src.slottypes[i] = Bottom
    end
    undefs = fill(false, nslots)
    body = src.code::Array{Any,1}
    nexpr = length(body)
    i = 1
    while i <= nexpr
        st_i = states[i]
        expr = body[i]
        if st_i !== ()
            # st_i === ()  =>  unreached statement  (see issue #7836)
            eval_annotate(expr, st_i, sv, undefs, 1)
            if isa(expr, Expr) && expr.head == :(=) && i < nexpr && isa(expr.args[1], Slot) && states[i + 1] !== ()
                # record type of assigned slot by looking at the next statement.
                # this is needed in case the slot is never used (which makes eval_annotate miss it).
                id = slot_id(expr.args[1])
                record_slot_type!(id, widenconst(states[i + 1][id].typ), src.slottypes)
            end
        elseif sv.optimize
            if ((isa(expr, Expr) && is_meta_expr(expr::Expr)) ||
                isa(expr, LineNumberNode))
                i += 1
                continue
            end
            # This can create `Expr(:gotoifnot)` with dangling label, which we
            # clean up in `reindex_labels!`
            deleteat!(body, i)
            deleteat!(states, i)
            nexpr -= 1
            continue
        end
        i += 1
    end
    for i = 1:nexpr
        st_i = states[i]
        if st_i !== ()
            body[i] = eval_annotate(body[i], st_i, sv, undefs, 2)
        end
    end

    # mark used-undef variables
    for i = 1:nslots
        if undefs[i]
            src.slotflags[i] |= Slot_UsedUndef
        end
    end
    nothing
end

# widen all Const elements in type annotations
_widen_all_consts(x::ANY) = x
_widen_all_consts(x::TypedSlot) = TypedSlot(x.id, widenconst(x.typ))
function _widen_all_consts(x::Expr)
    x.typ = widenconst(x.typ)
    for i = 1:length(x.args)
        x.args[i] = _widen_all_consts(x.args[i])
    end
    return x
end
function widen_all_consts!(src::CodeInfo)
    for i = 1:length(src.ssavaluetypes)
        src.ssavaluetypes[i] = widenconst(src.ssavaluetypes[i])
    end
    for i = 1:length(src.code)
        src.code[i] = _widen_all_consts(src.code[i])
    end
    return src
end

# replace slots 1:na with argexprs, static params with spvals, and increment
# other slots by offset.
function substitute!(e::ANY, na::Int, argexprs::Vector{Any}, spsig::ANY, spvals::Vector{Any}, offset::Int)
    if isa(e, Slot)
        id = slot_id(e)
        if 1 <= id <= na
            ae = argexprs[id]
            if isa(e, TypedSlot) && isa(ae, Slot)
                return TypedSlot(ae.id, e.typ)
            end
            return ae
        end
        if isa(e, SlotNumber)
            return SlotNumber(id + offset)
        else
            return TypedSlot(id + offset, e.typ)
        end
    end
    if isa(e, NewvarNode)
        return NewvarNode(substitute!(e.slot, na, argexprs, spsig, spvals, offset))
    end
    if isa(e, Expr)
        e = e::Expr
        head = e.head
        if head === :static_parameter
            return spvals[e.args[1]]
        elseif head === :foreigncall
            for i = 1:length(e.args)
                if i == 2
                    e.args[2] = ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), e.args[2], spsig, spvals)
                elseif i == 3
                    argtuple = Any[
                        ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), argt, spsig, spvals)
                        for argt
                        in e.args[3] ]
                    e.args[3] = svec(argtuple...)
                else
                    e.args[i] = substitute!(e.args[i], na, argexprs, spsig, spvals, offset)
                end
            end
        elseif !is_meta_expr_head(head)
            for i = 1:length(e.args)
                e.args[i] = substitute!(e.args[i], na, argexprs, spsig, spvals, offset)
            end
        end
    end
    return e
end

# count occurrences up to n+1
function occurs_more(e::ANY, pred, n)
    if isa(e,Expr)
        e = e::Expr
        head = e.head
        is_meta_expr_head(head) && return 0
        c = 0
        for a = e.args
            c += occurs_more(a, pred, n)
            if c>n
                return c
            end
        end
        return c
    end
    if pred(e)
        return 1
    end
    return 0
end

function exprtype(x::ANY, src::CodeInfo, mod::Module)
    if isa(x, Expr)
        return (x::Expr).typ
    elseif isa(x, SlotNumber)
        return src.slottypes[(x::SlotNumber).id]
    elseif isa(x, TypedSlot)
        return (x::TypedSlot).typ
    elseif isa(x, SSAValue)
        return abstract_eval_ssavalue(x::SSAValue, src)
    elseif isa(x, Symbol)
        return abstract_eval_global(mod, x::Symbol)
    elseif isa(x, QuoteNode)
        return abstract_eval_constant((x::QuoteNode).value)
    elseif isa(x, GlobalRef)
        return abstract_eval_global(x.mod, (x::GlobalRef).name)
    else
        return abstract_eval_constant(x)
    end
end

# known affect-free calls (also effect-free)
const _pure_builtins = Any[tuple, svec, fieldtype, apply_type, ===, isa, typeof, UnionAll]

# known effect-free calls (might not be affect-free)
const _pure_builtins_volatile = Any[getfield, arrayref]

function is_pure_builtin(f::ANY)
    if contains_is(_pure_builtins, f)
        return true
    end
    if contains_is(_pure_builtins_volatile, f)
        return true
    end
    if isa(f,IntrinsicFunction)
        if !(f === Intrinsics.pointerref || # this one is volatile
             f === Intrinsics.pointerset || # this one is never effect-free
             f === Intrinsics.llvmcall ||   # this one is never effect-free
             f === Intrinsics.checked_trunc_sint ||
             f === Intrinsics.checked_trunc_uint ||
             f === Intrinsics.checked_sdiv_int ||
             f === Intrinsics.checked_udiv_int ||
             f === Intrinsics.checked_srem_int ||
             f === Intrinsics.checked_urem_int ||
             f === Intrinsics.check_top_bit ||
             f === Intrinsics.sqrt_llvm)
            return true
        end
    end
    return false
end

function statement_effect_free(e::ANY, src::CodeInfo, mod::Module)
    if isa(e, Expr)
        if e.head === :(=)
            return !isa(e.args[1], GlobalRef) && effect_free(e.args[2], src, mod, false)
        elseif e.head === :gotoifnot
            return effect_free(e.args[1], src, mod, false)
        end
    elseif isa(e, LabelNode) || isa(e, GotoNode)
        return true
    end
    return effect_free(e, src, mod, false)
end

# detect some important side-effect-free calls (allow_volatile=true)
# and some affect-free calls (allow_volatile=false) -- affect_free means the call
# cannot be affected by previous calls, except assignment nodes
function effect_free(e::ANY, src::CodeInfo, mod::Module, allow_volatile::Bool)
    if isa(e, GlobalRef)
        return (isdefined(e.mod, e.name) && (allow_volatile || isconst(e.mod, e.name)))
    elseif isa(e, Symbol)
        return allow_volatile
    elseif isa(e, Expr)
        e = e::Expr
        head = e.head
        if head === :static_parameter || is_meta_expr_head(head)
            return true
        end
        ea = e.args
        if head === :call && !isa(e.args[1], SSAValue) && !isa(e.args[1], Slot)
            if is_known_call_p(e, is_pure_builtin, src, mod)
                if !allow_volatile
                    if is_known_call(e, arrayref, src, mod) || is_known_call(e, arraylen, src, mod)
                        return false
                    elseif is_known_call(e, getfield, src, mod)
                        et = exprtype(e, src, mod)
                        if !isa(et,Const) && !(isType(et) && isleaftype(et))
                            # first argument must be immutable to ensure e is affect_free
                            a = ea[2]
                            typ = widenconst(exprtype(a, src, mod))
                            if !isa(typ, DataType) || typ.mutable || typ.abstract
                                return false
                            end
                        end
                    end
                end
                # fall-through
            else
                return false
            end
        elseif head === :new
            if !allow_volatile
                a = ea[1]
                typ = widenconst(exprtype(a, src, mod))
                if !isType(typ) || !isa((typ::Type).parameters[1],DataType) || ((typ::Type).parameters[1]::DataType).mutable
                    return false
                end
            end
            # fall-through
        elseif head === :return
            # fall-through
        elseif head === :the_exception
            return allow_volatile
        else
            return false
        end
        for a in ea
            if !effect_free(a, src, mod, allow_volatile)
                return false
            end
        end
    elseif isa(e, LabelNode) || isa(e, GotoNode)
        return false
    end
    return true
end


#### post-inference optimizations ####

immutable InvokeData
    mt::MethodTable
    entry::TypeMapEntry
    types0
    fexpr
    texpr
end

function inline_as_constant(val::ANY, argexprs, sv::InferenceState,
                            invoke_data::ANY)
    if invoke_data === nothing
        invoke_fexpr = nothing
        invoke_texpr = nothing
    else
        invoke_data = invoke_data::InvokeData
        invoke_fexpr = invoke_data.fexpr
        invoke_texpr = invoke_data.texpr
    end
    # check if any arguments aren't effect_free and need to be kept around
    stmts = invoke_fexpr === nothing ? [] : Any[invoke_fexpr]
    for i = 1:length(argexprs)
        arg = argexprs[i]
        if !effect_free(arg, sv.src, sv.mod, false)
            push!(stmts, arg)
        end
        if i == 1 && !(invoke_texpr === nothing)
            push!(stmts, invoke_texpr)
        end
    end
    return (QuoteNode(val), stmts)
end

function countunionsplit(atypes::Vector{Any})
    nu = 1
    for ti in atypes
        if isa(ti, Union)
            nu *= unionlen(ti::Union)
        end
    end
    return nu
end

function get_spec_lambda(atypes::ANY, sv, invoke_data::ANY)
    if invoke_data === nothing
        return ccall(:jl_get_spec_lambda, Any, (Any, UInt), atypes, sv.params.world)
    else
        invoke_data = invoke_data::InvokeData
        atypes <: invoke_data.types0 || return nothing
        return ccall(:jl_get_invoke_lambda, Any, (Any, Any, Any, UInt),
                     invoke_data.mt, invoke_data.entry, atypes, sv.params.world)
    end
end

function invoke_NF(argexprs, etype::ANY, atypes, sv, atype_unlimited::ANY,
                   invoke_data::ANY)
    # converts a :call to :invoke
    nu = countunionsplit(atypes)
    nu > sv.params.MAX_UNION_SPLITTING && return NF
    if invoke_data === nothing
        invoke_fexpr = nothing
        invoke_texpr = nothing
    else
        invoke_data = invoke_data::InvokeData
        invoke_fexpr = invoke_data.fexpr
        invoke_texpr = invoke_data.texpr
    end

    if nu > 1
        spec_hit = nothing
        spec_miss = nothing
        error_label = nothing
        linfo_var = add_slot!(sv.src, MethodInstance, false)
        ex = Expr(:call)
        ex.args = copy(argexprs)
        ex.typ = etype
        stmts = []
        arg_hoisted = false
        for i = length(atypes):-1:1
            if i == 1 && !(invoke_texpr === nothing)
                unshift!(stmts, invoke_texpr)
                arg_hoisted = true
            end
            ti = atypes[i]
            if arg_hoisted || isa(ti, Union)
                aei = ex.args[i]
                if !effect_free(aei, sv.src, sv.mod, false)
                    arg_hoisted = true
                    newvar = newvar!(sv, ti)
                    unshift!(stmts, :($newvar = $aei))
                    ex.args[i] = newvar
                end
            end
        end
        invoke_fexpr === nothing || unshift!(stmts, invoke_fexpr)
        function splitunion(atypes::Vector{Any}, i::Int)
            if i == 0
                local sig = argtypes_to_type(atypes)
                local li = get_spec_lambda(sig, sv, invoke_data)
                li === nothing && return false
                add_backedge(li, sv)
                local stmt = []
                push!(stmt, Expr(:(=), linfo_var, li))
                spec_hit === nothing && (spec_hit = genlabel(sv))
                push!(stmt, GotoNode(spec_hit.label))
                return stmt
            else
                local ti = atypes[i]
                if isa(ti, Union)
                    local all = true
                    local stmts = []
                    local aei = ex.args[i]
                    for ty in uniontypes(ti::Union)
                        local ty
                        atypes[i] = ty
                        local match = splitunion(atypes, i - 1)
                        if match !== false
                            after = genlabel(sv)
                            unshift!(match, Expr(:gotoifnot, Expr(:call, GlobalRef(Core, :isa), aei, ty), after.label))
                            append!(stmts, match)
                            push!(stmts, after)
                        else
                            all = false
                        end
                    end
                    if UNION_SPLIT_MISMATCH_ERROR && all
                        error_label === nothing && (error_label = genlabel(sv))
                        push!(stmts, GotoNode(error_label.label))
                    else
                        spec_miss === nothing && (spec_miss = genlabel(sv))
                        push!(stmts, GotoNode(spec_miss.label))
                    end
                    atypes[i] = ti
                    return isempty(stmts) ? false : stmts
                else
                    return splitunion(atypes, i - 1)
                end
            end
        end
        local match = splitunion(atypes, length(atypes))
        if match !== false && spec_hit !== nothing
            append!(stmts, match)
            if error_label !== nothing
                push!(stmts, error_label)
                push!(stmts, Expr(:call, GlobalRef(_topmod(sv.mod), :error), "fatal error in type inference (type bound)"))
            end
            local ret_var, merge
            if spec_miss !== nothing
                ret_var = add_slot!(sv.src, widenconst(ex.typ), false)
                merge = genlabel(sv)
                push!(stmts, spec_miss)
                push!(stmts, Expr(:(=), ret_var, ex))
                push!(stmts, GotoNode(merge.label))
            else
                ret_var = newvar!(sv, ex.typ)
            end
            push!(stmts, spec_hit)
            ex = copy(ex)
            ex.head = :invoke
            unshift!(ex.args, linfo_var)
            push!(stmts, Expr(:(=), ret_var, ex))
            if spec_miss !== nothing
                push!(stmts, merge)
            end
            return (ret_var, stmts)
        end
    else
        local cache_linfo = get_spec_lambda(atype_unlimited, sv, invoke_data)
        cache_linfo === nothing && return NF
        add_backedge(cache_linfo, sv)
        unshift!(argexprs, cache_linfo)
        ex = Expr(:invoke)
        ex.args = argexprs
        ex.typ = etype
        if invoke_texpr === nothing
            if invoke_fexpr === nothing
                return ex
            else
                return ex, Any[invoke_fexpr]
            end
        end
        newvar = newvar!(sv, atypes[1])
        stmts = Any[invoke_fexpr, :($newvar = $(argexprs[1])),
                    invoke_texpr]
        argexprs[1] = newvar
        return ex, stmts
    end
    return NF
end

# inline functions whose bodies are "inline_worthy"
# where the function body doesn't contain any argument more than once.
# static parameters are ok if all the static parameter values are leaf types,
# meaning they are fully known.
# `ft` is the type of the function. `f` is the exact function if known, or else `nothing`.
function inlineable(f::ANY, ft::ANY, e::Expr, atypes::Vector{Any}, sv::InferenceState)
    argexprs = e.args

    if (f === typeassert || ft ⊑ typeof(typeassert)) && length(atypes)==3
        # typeassert(x::S, T) => x, when S<:T
        if isType(atypes[3]) && isleaftype(atypes[3]) &&
            atypes[2] ⊑ atypes[3].parameters[1]
            return (argexprs[2],())
        end
    end
    topmod = _topmod(sv)
    # special-case inliners for known pure functions that compute types
    if sv.params.inlining
        if isa(e.typ, Const) # || isconstType(e.typ)
            # XXX: this is needlessly buggy and should just call `inline_as_constant`
            if (f === apply_type || f === fieldtype || f === typeof ||
                istopfunction(topmod, f, :typejoin) ||
                istopfunction(topmod, f, :promote_type))
                # XXX: compute effect_free for the actual arguments
                if length(argexprs) < 2 || effect_free(argexprs[2], sv.src, sv.mod, true)
                    return (e.typ.val, ())
                else
                    return (e.typ.val, Any[argexprs[2]])
                end
            end
        end
        if istopfunction(topmod, f, :isbits) && length(atypes)==2 && isType(atypes[2]) &&
            effect_free(argexprs[2], sv.src, sv.mod, true) && isleaftype(atypes[2].parameters[1])
            # TODO: this is needlessly complicated and should just call `inline_as_constant`
            return (isbits(atypes[2].parameters[1]),())
        end
        if f === Core.kwfunc && length(argexprs) == 2 && isa(e.typ, Const)
            # TODO: replace with a call to `inline_as_constant`
            if effect_free(argexprs[2], sv.src, sv.mod, true)
                return (e.typ.val, ())
            else
                return (e.typ.val, Any[argexprs[2]])
            end
        end
    end
    invoke_data = nothing
    invoke_fexpr = nothing
    invoke_texpr = nothing
    if f === Core.invoke && length(atypes) >= 3
        ft = widenconst(atypes[2])
        invoke_tt = widenconst(atypes[3])
        if !isleaftype(ft) || !isleaftype(invoke_tt) || !isType(invoke_tt)
            return NF
        end
        if !(isa(invoke_tt.parameters[1], Type) &&
             invoke_tt.parameters[1] <: Tuple)
            return NF
        end
        invoke_tt_params = invoke_tt.parameters[1].parameters
        invoke_types = Tuple{ft, invoke_tt_params...}
        invoke_entry = ccall(:jl_gf_invoke_lookup, Any, (Any, UInt),
                             invoke_types, sv.params.world)
        invoke_entry === nothing && return NF
        invoke_fexpr = argexprs[1]
        invoke_texpr = argexprs[3]
        if effect_free(invoke_fexpr, sv.src, sv.mod, false)
            invoke_fexpr = nothing
        end
        if effect_free(invoke_texpr, sv.src, sv.mod, false)
            invoke_fexpr = nothing
        end
        invoke_data = InvokeData(ft.name.mt, invoke_entry,
                                 invoke_types, invoke_fexpr, invoke_texpr)
        atype0 = atypes[2]
        argexpr0 = argexprs[2]
        atypes = atypes[4:end]
        argexprs = argexprs[4:end]
        unshift!(atypes, atype0)
        unshift!(argexprs, argexpr0)
        f = isdefined(ft, :instance) ? ft.instance : nothing
    elseif isa(f, IntrinsicFunction) || ft ⊑ IntrinsicFunction ||
            isa(f, Builtin) || ft ⊑ Builtin
        return NF
    end

    atype_unlimited = argtypes_to_type(atypes)
    if !(invoke_data === nothing)
        invoke_data = invoke_data::InvokeData
        # TODO emit a type check and proceed for this case
        atype_unlimited <: invoke_data.types0 || return NF
    end
    if !sv.params.inlining
        return invoke_NF(argexprs, e.typ, atypes, sv, atype_unlimited,
                         invoke_data)
    end

    if length(atype_unlimited.parameters) - 1 > sv.params.MAX_TUPLETYPE_LEN
        atype = limit_tuple_type(atype_unlimited, sv.params)
    else
        atype = atype_unlimited
    end
    if invoke_data === nothing
        min_valid = UInt[typemin(UInt)]
        max_valid = UInt[typemax(UInt)]
        meth = _methods_by_ftype(atype, 1, sv.params.world, min_valid, max_valid)
        if meth === false || length(meth) != 1
            return invoke_NF(argexprs, e.typ, atypes, sv,
                             atype_unlimited, invoke_data)
        end
        meth = meth[1]::SimpleVector
        metharg = meth[1]::Type
        methsp = meth[2]::SimpleVector
        method = meth[3]::Method
    else
        invoke_data = invoke_data::InvokeData
        method = invoke_data.entry.func
        (metharg, methsp) = ccall(:jl_match_method, Ref{SimpleVector}, (Any, Any),
                                  atype_unlimited, method.sig)
        methsp = methsp::SimpleVector
    end
    # check whether call can be inlined to just a quoted constant value
    if isa(f, widenconst(ft)) && !method.isstaged && (method.source.pure || f === return_type)
        if isconstType(e.typ)
            return inline_as_constant(e.typ.parameters[1], argexprs, sv,
                                      invoke_data)
        elseif isa(e.typ,Const)
            return inline_as_constant(e.typ.val, argexprs, sv,
                                      invoke_data)
        end
    end

    methsig = method.sig
    if !(atype <: metharg)
        return invoke_NF(argexprs, e.typ, atypes, sv, atype_unlimited,
                         invoke_data)
    end

    argexprs0 = argexprs
    na = Int(method.nargs)
    # check for vararg function
    isva = false
    if na > 0 && method.isva
        @assert length(argexprs) >= na - 1
        # construct tuple-forming expression for argument tail
        vararg = mk_tuplecall(argexprs[na:end], sv)
        argexprs = Any[argexprs[1:(na - 1)]..., vararg]
        isva = true
    elseif na != length(argexprs)
        # we have a method match only because an earlier
        # inference step shortened our call args list, even
        # though we have too many arguments to actually
        # call this function
        return NF
    end

    @assert na == length(argexprs)

    for i = 1:length(methsp)
        si = methsp[i]
        isa(si, TypeVar) && return NF
    end

    # some gf have special tfunc, meaning they wouldn't have been inferred yet
    # check the same conditions from abstract_call to detect this case
    force_infer = false
    if !method.isstaged
        if method.module == _topmod(method.module) || (isdefined(Main, :Base) && method.module == Main.Base)
            if method.name == :getindex || method.name == :next || method.name == :indexed_next
                if length(atypes) > 2 && atypes[3] ⊑ Int
                    at2 = widenconst(atypes[2])
                    if (at2 <: Tuple ||
                        (isa(at2, DataType) && (at2::DataType).name === Pair_name()))
                        force_infer = true
                    end
                end
            end
        end
    end

    # see if the method has been previously inferred (and cached)
    linfo = code_for_method(method, metharg, methsp, sv.params.world, !force_infer) # Union{Void, MethodInstance}
    isa(linfo, MethodInstance) || return invoke_NF(argexprs0, e.typ, atypes, sv,
                                                   atype_unlimited, invoke_data)
    linfo = linfo::MethodInstance
    if linfo.jlcall_api == 2
        # in this case function can be inlined to a constant
        add_backedge(linfo, sv)
        return inline_as_constant(linfo.inferred, argexprs, sv, invoke_data)
    end

    # see if the method has a current InferenceState frame
    # or existing inferred code info
    frame = nothing # Union{Void, InferenceState}
    src = nothing # Union{Void, CodeInfo}
    if force_infer && isa(atypes[3], Const)
        # Since we inferred this with the information that atypes[3]::Const,
        # must inline with that same information.
        # We do that by overriding the argument type,
        # while ensuring we don't cache that information
        # This isn't particularly important for `getindex`,
        # as we'll be able to fix that up at the end of inlinable when we verify the return type.
        # But `next` and `indexed_next` make tuples which would end up burying some of that information in the AST
        # where we can't easily correct it afterwards.
        frame = InferenceState(linfo, get_source(linfo), #=optimize=#true, #=cache=#false, sv.params)
        frame.stmt_types[1][3] = VarState(atypes[3], false)
        typeinf_loop(frame)
    else
        if isdefined(linfo, :inferred) && isa(linfo.inferred, CodeInfo) && (linfo.inferred::CodeInfo).inferred
            # use cache
            src = linfo.inferred
        elseif linfo.inInference
            # use WIP
            frame = typeinf_active(linfo)
        elseif force_infer
            # create inferred code on-demand
            # but if we decided in the past not to try to infer this particular signature
            # (due to signature coarsening in abstract_call_gf_by_type)
            # don't infer it now, as attempting to force it now would be a bad idea (non terminating)
            frame = typeinf_frame(linfo, nothing, #=optimize=#true, #=cache=#true, sv.params)
        end
    end

    # compute the return value
    if isa(frame, InferenceState)
        frame = frame::InferenceState
        linfo = frame.linfo
        src = frame.src
        if frame.const_api # handle like jlcall_api == 2
            if frame.inferred || !frame.cached
                add_backedge(frame.linfo, sv)
            else
                add_backedge(frame, sv, 0)
            end
            if isa(frame.bestguess, Const)
                inferred_const = (frame.bestguess::Const).val
            else
                @assert isconstType(frame.bestguess)
                inferred_const = frame.bestguess.parameters[1]
            end
            return inline_as_constant(inferred_const, argexprs, sv, invoke_data)
        end
        rettype = widenconst(frame.bestguess)
    else
        rettype = linfo.rettype
    end

    # check that the code is inlineable
    isa(src, CodeInfo) || return invoke_NF(argexprs0, e.typ, atypes, sv,
                                           atype_unlimited, invoke_data)
    src = src::CodeInfo
    ast = src.code
    if !src.inferred || !src.inlineable
        return invoke_NF(argexprs0, e.typ, atypes, sv, atype_unlimited,
                         invoke_data)
    end

    # create the backedge
    if isa(frame, InferenceState) && !frame.inferred && frame.cached
        # in this case, the actual backedge linfo hasn't been computed
        # yet, but will be when inference on the frame finishes
        add_backedge(frame, sv, 0)
    else
        add_backedge(linfo, sv)
    end

    spvals = Any[]
    for i = 1:length(methsp)
        push!(spvals, methsp[i])
    end
    for i = 1:length(spvals)
        si = spvals[i]
        if isa(si, Symbol) || isa(si, SSAValue) || isa(si, Slot)
            spvals[i] = QuoteNode(si)
        end
    end

    nm = length(unwrap_unionall(metharg).parameters)

    if !isa(ast, Array{Any,1})
        ast = ccall(:jl_uncompress_ast, Any, (Any, Any), method, ast)
    else
        ast = copy_exprargs(ast)
    end
    ast = ast::Array{Any,1}

    body = Expr(:block)
    body.args = ast
    propagate_inbounds = src.propagate_inbounds

    # see if each argument occurs only once in the body expression
    stmts = []
    prelude_stmts = []
    stmts_free = true # true = all entries of stmts are effect_free

    for i = na:-1:1 # stmts_free needs to be calculated in reverse-argument order
        #args_i = args[i]
        aei = argexprs[i]
        aeitype = argtype = widenconst(exprtype(aei, sv.src, sv.mod))
        if i == 1 && !(invoke_texpr === nothing)
            unshift!(prelude_stmts, invoke_texpr)
        end

        # ok for argument to occur more than once if the actual argument
        # is a symbol or constant, or is not affected by previous statements
        # that will exist after the inlining pass finishes
        affect_free = stmts_free  # false = previous statements might affect the result of evaluating argument
        occ = 0
        for j = length(body.args):-1:1
            b = body.args[j]
            if occ < 6
                occ += occurs_more(b, x->(isa(x, Slot) && slot_id(x) == i), 6)
            end
            if occ > 0 && affect_free && !effect_free(b, src, method.module, true)
                #TODO: we might be able to short-circuit this test better by memoizing effect_free(b) in the for loop over i
                affect_free = false
            end
            if occ > 5 && !affect_free
                break
            end
        end
        free = effect_free(aei, sv.src, sv.mod, true)
        if ((occ==0 && aeitype===Bottom) || (occ > 1 && !inline_worthy(aei, occ*2000)) ||
                (affect_free && !free) || (!affect_free && !effect_free(aei, sv.src, sv.mod, false)))
            if occ != 0
                vnew = newvar!(sv, aeitype)
                argexprs[i] = vnew
                unshift!(prelude_stmts, Expr(:(=), vnew, aei))
                stmts_free &= free
            elseif !free && !isType(aeitype)
                unshift!(prelude_stmts, aei)
                stmts_free = false
            end
        end
    end
    invoke_fexpr === nothing || unshift!(prelude_stmts, invoke_fexpr)

    # re-number the SSAValues and copy their type-info to the new ast
    ssavalue_types = src.ssavaluetypes
    if !isempty(ssavalue_types)
        incr = length(sv.src.ssavaluetypes)
        if incr != 0
            body = ssavalue_increment(body, incr)
        end
        append!(sv.src.ssavaluetypes, ssavalue_types)
    end

    # ok, substitute argument expressions for argument names in the body
    body = substitute!(body, na, argexprs, method.sig, spvals, length(sv.src.slotnames) - na)
    append!(sv.src.slotnames, src.slotnames[(na + 1):end])
    append!(sv.src.slottypes, src.slottypes[(na + 1):end])
    append!(sv.src.slotflags, src.slotflags[(na + 1):end])

    # make labels / goto statements unique
    # relocate inlining information
    newlabels = zeros(Int,label_counter(body.args)+1)
    for i = 1:length(body.args)
        a = body.args[i]
        if isa(a,LabelNode)
            a = a::LabelNode
            newlabel = genlabel(sv)
            newlabels[a.label+1] = newlabel.label
            body.args[i] = newlabel
        end
    end
    for i = 1:length(body.args)
        a = body.args[i]
        if isa(a,GotoNode)
            a = a::GotoNode
            body.args[i] = GotoNode(newlabels[a.label+1])
        elseif isa(a,Expr)
            a = a::Expr
            if a.head === :enter
                a.args[1] = newlabels[a.args[1]+1]
            elseif a.head === :gotoifnot
                a.args[2] = newlabels[a.args[2]+1]
            end
        end
    end

    # convert return statements into a series of goto's
    retstmt = genlabel(sv)
    local retval
    multiret = false
    lastexpr = pop!(body.args)
    if isa(lastexpr,LabelNode)
        push!(body.args, lastexpr)
        push!(body.args, Expr(:call, GlobalRef(_topmod(sv.mod), :error), "fatal error in type inference (lowering)"))
        lastexpr = nothing
    elseif !(isa(lastexpr,Expr) && lastexpr.head === :return)
        # code sometimes ends with a meta node, e.g. inbounds pop
        push!(body.args, lastexpr)
        lastexpr = nothing
    end
    for a in body.args
        push!(stmts, a)
        if isa(a,Expr)
            a = a::Expr
            if a.head === :return
                if !multiret
                    # create slot first time
                    retval = add_slot!(sv.src, rettype, false)
                end
                multiret = true
                unshift!(a.args, retval)
                a.head = :(=)
                push!(stmts, GotoNode(retstmt.label))
            end
        end
    end

    if multiret
        if lastexpr !== nothing
            unshift!(lastexpr.args, retval)
            lastexpr.head = :(=)
            push!(stmts, lastexpr)
        end
        push!(stmts, retstmt)
        expr = retval
    else
        # Dead code elimination can leave a non-return statement at the end
        if lastexpr === nothing
            expr = nothing
        else
            expr = lastexpr.args[1]
        end
    end

    inlining_ignore = function (stmt::ANY)
        isa(stmt, Expr) && return is_meta_expr(stmt::Expr)
        isa(stmt, LineNumberNode) && return true
        stmt === nothing && return true
        return false
    end

    do_coverage = coverage_enabled()
    if do_coverage
        line = method.line
        if !isempty(stmts) && isa(stmts[1], LineNumberNode)
            line = (shift!(stmts)::LineNumberNode).line
        end
        # Check if we are switching module, which is necessary to catch user
        # code inlined into `Base` with `--code-coverage=user`.
        # Assume we are inlining directly into `enclosing` instead of another
        # function inlined in it
        mod = method.module
        if mod === sv.mod
            unshift!(stmts, Expr(:meta, :push_loc, method.file,
                                 method.name, line))
        else
            unshift!(stmts, Expr(:meta, :push_loc, method.file,
                                 method.name, line, mod))
        end
        push!(stmts, Expr(:meta, :pop_loc))
    elseif !isempty(stmts)
        if all(inlining_ignore, stmts)
            empty!(stmts)
        else
            line::Int = method.line
            if isa(stmts[1], LineNumberNode)
                line = (shift!(stmts)::LineNumberNode).line
            end
            unshift!(stmts, Expr(:meta, :push_loc, method.file,
                                 method.name, line))
            if isa(stmts[end], LineNumberNode)
                stmts[end] = Expr(:meta, :pop_loc)
            else
                push!(stmts, Expr(:meta, :pop_loc))
            end
        end
    end
    if !isempty(stmts) && !propagate_inbounds
        # avoid redundant inbounds annotations
        s_1, s_end = stmts[1], stmts[end]
        i = 2
        while length(stmts) > i && ((isa(s_1,Expr)&&s_1.head===:line) || isa(s_1,LineNumberNode))
            s_1 = stmts[i]
            i += 1
        end
        if isa(s_1, Expr) && s_1.head === :inbounds && s_1.args[1] === false &&
            isa(s_end, Expr) && s_end.head === :inbounds && s_end.args[1] === :pop
        else
            # inlined statements are out-of-bounds by default
            unshift!(stmts, Expr(:inbounds, false))
            push!(stmts, Expr(:inbounds, :pop))
        end
    end

    if isa(expr,Expr)
        old_t = e.typ
        if old_t ⊑ expr.typ
            # if we had better type information than the content being inlined,
            # change the return type now to use the better type
            expr.typ = old_t
        end
    end
    if !isempty(prelude_stmts)
        stmts = append!(prelude_stmts, stmts)
    end
    return (expr, stmts)
end

inline_worthy(body::ANY, cost::Integer) = true

# should the expression be part of the inline cost model
function inline_ignore(ex::ANY)
    if isa(ex, LineNumberNode) || ex === nothing
        return true
    end
    return isa(ex, Expr) && is_meta_expr(ex::Expr)
end

function inline_worthy(body::Expr, cost::Integer=1000) # precondition: 0 < cost; nominal cost = 1000
    if popmeta!(body, :noinline)[1]
        return false
    end
    symlim = 1000 + 5_000_000 ÷ cost
    nstmt = 0
    for stmt in body.args
        if !(isa(stmt, SSAValue) || inline_ignore(stmt))
            nstmt += 1
        end
    end
    if nstmt < (symlim + 500) ÷ 1000
        symlim *= 16
        symlim ÷= 1000
        if occurs_more(body, e->!inline_ignore(e), symlim) < symlim
            return true
        end
    end
    return false
end

ssavalue_increment(body::ANY, incr) = body
ssavalue_increment(body::SSAValue, incr) = SSAValue(body.id + incr)
function ssavalue_increment(body::Expr, incr)
    if is_meta_expr(body)
        return body
    end
    for i in 1:length(body.args)
        body.args[i] = ssavalue_increment(body.args[i], incr)
    end
    return body
end

const top_getfield = GlobalRef(Core, :getfield)
const top_tuple = GlobalRef(Core, :tuple)

function mk_getfield(texpr, i, T)
    e = Expr(:call, top_getfield, texpr, i)
    e.typ = T
    return e
end

function mk_tuplecall(args, sv::InferenceState)
    e = Expr(:call, top_tuple, args...)
    e.typ = tuple_tfunc(Tuple{Any[widenconst(exprtype(x, sv.src, sv.mod)) for x in args]...})
    return e
end

function inlining_pass!(sv::InferenceState)
    eargs = sv.src.code
    i = 1
    while i <= length(eargs)
        ei = eargs[i]
        if isa(ei, Expr)
            res = inlining_pass(ei, sv)
            eargs[i] = res[1]
            if isa(res[2], Array)
                sts = res[2]::Array{Any,1}
                for j = 1:length(sts)
                    insert!(eargs, i, sts[j])
                    i += 1
                end
            end
        end
        i += 1
    end
end

const corenumtype = Union{Int32, Int64, Float32, Float64}

function inlining_pass(e::Expr, sv::InferenceState)
    if e.head === :method
        # avoid running the inlining pass on function definitions
        return (e, ())
    end
    eargs = e.args
    if length(eargs) < 1
        return (e, ())
    end
    stmts = []
    arg1 = eargs[1]
    isccall = false
    i0 = 1
    # don't inline first (global) arguments of ccall, as this needs to be evaluated
    # by the interpreter and inlining might put in something it can't handle,
    # like another ccall (or try to move the variables out into the function)
    if e.head === :foreigncall
        # 3 is rewritten to 1 below to handle the callee.
        i0 = 3
        isccall = true
    elseif is_known_call(e, Core.Intrinsics.llvmcall, sv.src, sv.mod)
        i0 = 5
    end
    has_stmts = false # needed to preserve order-of-execution
    for _i = length(eargs):-1:i0
        if isccall && _i == 3
            i = 1
            isccallee = true
        else
            i = _i
            isccallee = false
        end
        ei = eargs[i]
        if isa(ei,Expr)
            ei = ei::Expr
            if ei.head === :&
                argloc = ei.args
                i = 1
                ei = argloc[1]
                if !isa(ei,Expr)
                    continue
                end
                ei = ei::Expr
            else
                argloc = eargs
            end
            res = inlining_pass(ei, sv)
            res1 = res[1]
            res2 = res[2]
            has_new_stmts = isa(res2, Array) && !isempty(res2::Array{Any,1})
            if isccallee
                restype = exprtype(res1, sv.src, sv.mod)
                if isa(restype, Const)
                    argloc[i] = restype.val
                    if !effect_free(res1, sv.src, sv.mod, false)
                        insert!(stmts, 1, res1)
                    end
                    if has_new_stmts
                        prepend!(stmts, res2::Array{Any,1})
                    end
                    # Assume this is the last argument to process
                    break
                end
            end
            if has_stmts && !effect_free(res1, sv.src, sv.mod, false)
                restype = exprtype(res1, sv.src, sv.mod)
                vnew = newvar!(sv, restype)
                argloc[i] = vnew
                unshift!(stmts, Expr(:(=), vnew, res1))
            else
                argloc[i] = res1
            end
            if has_new_stmts
                res2 = res2::Array{Any,1}
                prepend!(stmts, res2)
                if !has_stmts && !(_i == i0)
                    for stmt in res2
                        if !effect_free(stmt, sv.src, sv.mod, true)
                            has_stmts = true
                        end
                    end
                end
            end
        end
    end
    if isccall
        le = length(eargs)
        for i = 4:2:(le - 1)
            if eargs[i] === eargs[i + 1]
                eargs[i + 1] = 0
            end
        end
    end
    if e.head !== :call
        return (e, stmts)
    end

    ft = exprtype(arg1, sv.src, sv.mod)
    if isa(ft, Const)
        f = ft.val
    else
        f = nothing
        if !( isleaftype(ft) || ft<:Type )
            return (e, stmts)
        end
    end

    if sv.params.inlining
        if isdefined(Main, :Base) &&
            ((isdefined(Main.Base, :^) && f === Main.Base.:^) ||
             (isdefined(Main.Base, :.^) && f === Main.Base.:.^)) &&
            length(e.args) == 3

            a2 = e.args[3]
            if isa(a2, Symbol) || isa(a2, Slot) || isa(a2, SSAValue)
                ta2 = exprtype(a2, sv.src, sv.mod)
                if isa(ta2, Const)
                    a2 = ta2.val
                end
            end

            square = (a2 === Int32(2) || a2 === Int64(2))
            triple = (a2 === Int32(3) || a2 === Int64(3))
            if square || triple
                a1 = e.args[2]
                basenumtype = Union{corenumtype, Main.Base.Complex64, Main.Base.Complex128, Main.Base.Rational}
                if isa(a1, basenumtype) || ((isa(a1, Symbol) || isa(a1, Slot) || isa(a1, SSAValue)) &&
                                           exprtype(a1, sv.src, sv.mod) ⊑ basenumtype)
                    if square
                        e.args = Any[GlobalRef(Main.Base,:*), a1, a1]
                        res = inlining_pass(e, sv)
                    else
                        e.args = Any[GlobalRef(Main.Base,:*), Expr(:call, GlobalRef(Main.Base,:*), a1, a1), a1]
                        res = inlining_pass(e, sv)
                    end
                    if isa(res, Tuple)
                        if isa(res[2], Array) && !isempty(res[2])
                            append!(stmts, res[2])
                        end
                        res = res[1]
                    end
                    return (res, stmts)
                end
            end
        end
    end

    for ninline = 1:100
        ata = Vector{Any}(length(e.args))
        ata[1] = ft
        for i = 2:length(e.args)
            a = exprtype(e.args[i], sv.src, sv.mod)
            (a === Bottom || isvarargtype(a)) && return (e, stmts)
            ata[i] = a
        end
        res = inlineable(f, ft, e, ata, sv)
        if isa(res,Tuple)
            if isa(res[2],Array) && !isempty(res[2])
                append!(stmts,res[2])
            end
            res = res[1]
        end

        if res !== NF
            # iteratively inline apply(f, tuple(...), tuple(...), ...) in order
            # to simplify long vararg lists as in multi-arg +
            if isa(res,Expr) && is_known_call(res, _apply, sv.src, sv.mod)
                e = res::Expr
                f = _apply; ft = abstract_eval_constant(f)
            else
                return (res,stmts)
            end
        end

        if f === _apply
            na = length(e.args)
            newargs = Vector{Any}(na-2)
            for i = 3:na
                aarg = e.args[i]
                t = widenconst(exprtype(aarg, sv.src, sv.mod))
                if isa(aarg,Expr) && (is_known_call(aarg, tuple, sv.src, sv.mod) || is_known_call(aarg, svec, sv.src, sv.mod))
                    # apply(f,tuple(x,y,...)) => f(x,y,...)
                    newargs[i-2] = aarg.args[2:end]
                elseif isa(aarg, Tuple)
                    newargs[i-2] = Any[ QuoteNode(x) for x in aarg ]
                elseif isa(t, DataType) && t.name === Tuple.name && !isvatuple(t) &&
                        effect_free(aarg, sv.src, sv.mod, true) && length(t.parameters) <= sv.params.MAX_TUPLE_SPLAT
                    # apply(f,t::(x,y)) => f(t[1],t[2])
                    tp = t.parameters
                    newargs[i-2] = Any[ mk_getfield(aarg,j,tp[j]) for j=1:length(tp) ]
                else
                    # not all args expandable
                    return (e,stmts)
                end
            end
            e.args = [Any[e.args[2]]; newargs...]

            # now try to inline the simplified call
            ft = exprtype(e.args[1], sv.src, sv.mod)
            if isa(ft,Const)
                f = ft.val
            else
                f = nothing
                if !( isleaftype(ft) || ft<:Type )
                    return (e,stmts)
                end
            end
        else
            return (e,stmts)
        end
    end
    return (e,stmts)
end

const compiler_temp_sym = Symbol("#temp#")

function add_slot!(src::CodeInfo, typ::ANY, is_sa::Bool, name::Symbol=compiler_temp_sym)
    id = length(src.slotnames) + 1
    push!(src.slotnames, name)
    push!(src.slottypes, typ); @assert !isa(typ, Const)
    push!(src.slotflags, Slot_Assigned + is_sa * Slot_AssignedOnce)
    return SlotNumber(id)
end

function is_known_call(e::Expr, func::ANY, src::CodeInfo, mod::Module)
    if e.head !== :call
        return false
    end
    f = exprtype(e.args[1], src, mod)
    return isa(f, Const) && f.val === func
end

function is_known_call_p(e::Expr, pred::ANY, src::CodeInfo, mod::Module)
    if e.head !== :call
        return false
    end
    f = exprtype(e.args[1], src, mod)
    return (isa(f, Const) && pred(f.val)) || (isType(f) && pred(f.parameters[1]))
end

function delete_var!(src::CodeInfo, id, T)
    filter!(x->!(isa(x,Expr) && (x.head === :(=) || x.head === :const) &&
                 isa(x.args[1],T) && x.args[1].id == id),
            src.code)
    return src
end

function slot_replace!(src::CodeInfo, id::Int, rhs::ANY, T::ANY)
    for i = 1:length(src.code)
        src.code[i] = _slot_replace!(src.code[i], id, rhs, T)
    end
    return src
end

function _slot_replace!(e, id::Int, rhs::ANY, T::ANY)
    if isa(e,T) && e.id == id
        return rhs
    end
    if isa(e,Expr)
        for i = 1:length(e.args)
            e.args[i] = _slot_replace!(e.args[i], id, rhs, T)
        end
    end
    return e
end

occurs_undef(var::Int, expr, flags) =
    flags[var] & Slot_UsedUndef != 0 && occurs_more(expr, e -> (isa(e, Slot) && slot_id(e) == var), 0) > 0

is_argument(nargs::Int, v::Slot) = slot_id(v) <= nargs

# remove all single-assigned vars v in "v = x" where x is an argument.
# "sa" is the result of find_sa_vars
# T: Slot or SSAValue
function remove_redundant_temp_vars(src::CodeInfo, nargs::Int, sa, T)
    flags = src.slotflags
    ssavalue_types = src.ssavaluetypes
    bexpr = Expr(:block)
    bexpr.args = src.code
    for (v, init) in sa
        if isa(init, Slot) && is_argument(nargs, init::Slot)
            # this transformation is not valid for vars used before def.
            # we need to preserve the point of assignment to know where to
            # throw errors (issue #4645).
            if T === SSAValue || !occurs_undef(v, bexpr, flags)
                # the transformation is not ideal if the assignment
                # is present for the auto-unbox functionality
                # (from inlining improved type inference information)
                # and this transformation would worsen the type information
                # everywhere later in the function
                ityp = isa(init, TypedSlot) ? init.typ : src.slottypes[(init::SlotNumber).id]
                if ityp ⊑ (T === SSAValue ? ssavalue_types[v + 1] : src.slottypes[v])
                    delete_var!(src, v, T)
                    slot_replace!(src, v, init, T)
                end
            end
        end
    end
    return src
end

# compute set of slots assigned once
function find_sa_vars(src::CodeInfo, nargs::Int)
    body = src.code
    av = ObjectIdDict()
    av2 = ObjectIdDict()
    gss = ObjectIdDict()
    for i = 1:length(body)
        e = body[i]
        if isa(e,Expr) && e.head === :(=)
            lhs = e.args[1]
            if isa(lhs, SSAValue)
                gss[lhs.id] = e.args[2]
            elseif isa(lhs, Slot)
                id = slot_id(lhs)
                if id > nargs  # exclude args
                    if !haskey(av, id)
                        av[id] = e.args[2]
                    else
                        av2[id] = true
                    end
                end
            end
        end
    end
    filter!((id, _) -> !haskey(av2, id), av)
    return (av, gss)
end

symequal(x::SSAValue, y::SSAValue) = x.id === y.id
symequal(x::Slot    , y::Slot)     = x.id === y.id
symequal(x::ANY     , y::ANY)      = x === y

function occurs_outside_getfield(e::ANY, sym::ANY,
                                 sv::InferenceState, field_count::Int, field_names::ANY)
    if e === sym || (isa(e, Slot) && isa(sym, Slot) && slot_id(e) == slot_id(sym))
        return true
    end
    if isa(e,Expr)
        e = e::Expr
        head = e.head
        is_meta_expr_head(head) && return false
        if is_known_call(e, getfield, sv.src, sv.mod) && symequal(e.args[2],sym)
            idx = e.args[3]
            if isa(idx,QuoteNode) && (idx.value in field_names)
                return false
            end
            if isa(idx,Int) && (1 <= idx <= field_count)
                return false
            end
            return true
        end
        if head === :(=)
            return occurs_outside_getfield(e.args[2], sym, sv,
                                           field_count, field_names)
        else
            if (head === :block && isa(sym, Slot) &&
                sv.src.slotflags[slot_id(sym)] & Slot_UsedUndef == 0)
                ignore_void = true
            else
                ignore_void = false
            end
            for a in e.args
                if ignore_void && isa(a, Slot) && slot_id(a) == slot_id(sym)
                    continue
                end
                if occurs_outside_getfield(a, sym, sv, field_count, field_names)
                    return true
                end
            end
        end
    end
    return false
end

function void_use_elim_pass!(sv::InferenceState)
    # Remove top level SSAValue and slots that is `!usedUndef`.
    # Also remove some `nothing` while we are at it....
    not_void_use = function (ex::ANY)
        if isa(ex, SSAValue)
            # Explicitly listed here for clarity
            return false
        elseif isa(ex, Slot)
            return sv.src.slotflags[slot_id(ex)] & Slot_UsedUndef != 0
        elseif isa(ex, GlobalRef)
            ex = ex::GlobalRef
            return !isdefined(ex.mod, ex.name)
        elseif (isa(ex, Expr) || isa(ex, GotoNode) || isa(ex, LineNumberNode) ||
                isa(ex, NewvarNode) || isa(ex, Symbol) || isa(ex, LabelNode))
            # This is a list of special type handled by the compiler
            return true
        end
        return false
    end
    filter!(not_void_use, sv.src.code::Array{Any,1})
    nothing
end

function meta_elim_pass!(code::Array{Any,1}, propagate_inbounds::Bool, do_coverage::Bool)
    # 1. Remove place holders
    #
    # 2. If coverage is off, remove line number nodes that don't mark any
    #    real expressions.
    #
    # 3. Remove top level SSAValue
    #
    # 4. Handle bounds check elision
    #
    #    4.1. If check_bounds is always on, delete all `Expr(:boundscheck)`
    #    4.2. If check_bounds is always off, delete all boundscheck blocks.
    #    4.3. If check_bounds is default, figure out whether each checkbounds
    #         blocks needs to be eliminated or could be eliminated when inlined
    #         into another function. Delete the blocks that should be eliminated
    #         and delete the `Expr(:boundscheck)` for blocks that will never be
    #         deleted. (i.e. the ones that are not eliminated with
    #         `length(inbounds_stack) >= 2`)
    #
    #    When deleting IR with boundscheck, keep the label node in order to not
    #    confuse later passes or codegen. (we could also track if  any SSAValue
    #    is deleted while still having uses that are not but that's a little
    #    expensive).
    #
    # 5. Clean up `Expr(:inbounds)`
    #
    #    Delete all `Expr(:inbounds)` that is unnecessary, which is all of them
    #    for non-default check_bounds. For default check_bounds this includes
    #
    #    * `Expr(:inbounds, true)` in `Expr(:inbounds, true)`
    #    * `Expr(:inbounds, false)` when
    #      `!is_inbounds && length(inbounds_stack) >= 2`
    #
    #    Functions without `propagate_inbounds` have an implicit `false` on the
    #    `inbounds_stack`
    #
    #    There are other cases in which we can eliminate `Expr(:inbounds)` or
    #    `Expr(:boundscheck)` (e.g. when they don't enclose any non-meta
    #    expressions). Those are a little harder to detect and are hopefully
    #    not too common.
    check_bounds = JLOptions().check_bounds

    inbounds_stack = propagate_inbounds ? Bool[] : Bool[false]
    # Whether the push is deleted (therefore if the pop has to be too)
    # Shared for `Expr(:boundscheck)` and `Expr(:inbounds)`
    bounds_elim_stack = Bool[]
    # The expression index of the push, set to `0` when encountering a
    # non-meta expression that might be affect by the push.
    # The clearing needs to be propagated up during pop
    # This is not pushed to if the push is already eliminated
    # Also shared for `Expr(:boundscheck)` and `Expr(:inbounds)`
    bounds_push_pos_stack = Int[0] # always non-empty
    # Number of boundscheck pushes in a eliminated boundscheck block
    void_boundscheck_depth = 0
    is_inbounds = check_bounds == 2
    enabled = true

    # Position of the last line number node without any non-meta expressions
    # in between.
    prev_dbg_stack = Int[0] # always non-empty
    # Whether there's any non-meta exprs after the enclosing `push_loc`
    push_loc_pos_stack = Int[0] # always non-empty

    for i in 1:length(code)
        ex = code[i]
        if ex === nothing
            continue
        elseif isa(ex, SSAValue)
            code[i] = nothing
            continue
        elseif isa(ex, LabelNode)
            prev_dbg_stack[end] = 0
            push_loc_pos_stack[end] = 0
            continue
        elseif !do_coverage && (isa(ex, LineNumberNode) ||
                                (isa(ex, Expr) && (ex::Expr).head === :line))
            prev_label = prev_dbg_stack[end]
            if prev_label != 0
                code[prev_label] = nothing
            end
            prev_dbg_stack[end] = i
            continue
        elseif !isa(ex, Expr)
            if enabled
                prev_dbg_stack[end] = 0
                push_loc_pos_stack[end] = 0
                bounds_push_pos_stack[end] = 0
            else
                code[i] = nothing
            end
            continue
        end
        ex = ex::Expr
        args = ex.args
        head = ex.head
        if head === :boundscheck
            if !enabled
                # we are in an eliminated boundscheck, simply record the number
                # of push/pop
                if !(args[1] === :pop)
                    void_boundscheck_depth += 1
                elseif void_boundscheck_depth == 0
                    # There must have been a push
                    pop!(bounds_elim_stack)
                    enabled = true
                else
                    void_boundscheck_depth -= 1
                end
                code[i] = nothing
            elseif args[1] === :pop
                # This will also delete pops that don't match
                if (isempty(bounds_elim_stack) ? true :
                    pop!(bounds_elim_stack))
                    code[i] = nothing
                    continue
                end
                push_idx = bounds_push_pos_stack[end]
                if length(bounds_push_pos_stack) > 1
                    pop!(bounds_push_pos_stack)
                end
                if push_idx > 0
                    code[push_idx] = nothing
                    code[i] = nothing
                else
                    bounds_push_pos_stack[end] = 0
                end
            elseif is_inbounds
                code[i] = nothing
                push!(bounds_elim_stack, true)
                enabled = false
            elseif check_bounds == 1 || length(inbounds_stack) >= 2
                # Not inbounds and at least two levels deep, this will never
                # be eliminated when inlined to another function.
                code[i] = nothing
                push!(bounds_elim_stack, true)
            else
                push!(bounds_elim_stack, false)
                push!(bounds_push_pos_stack, i)
            end
            continue
        end
        if !enabled && !(do_coverage && head === :meta)
            code[i] = nothing
            continue
        end
        if head === :inbounds
            if check_bounds != 0
                code[i] = nothing
                continue
            end
            arg1 = args[1]
            if arg1 === true
                if !isempty(inbounds_stack) && inbounds_stack[end]
                    code[i] = nothing
                    push!(bounds_elim_stack, true)
                else
                    is_inbounds = true
                    push!(bounds_elim_stack, false)
                    push!(bounds_push_pos_stack, i)
                end
                push!(inbounds_stack, true)
            elseif arg1 === false
                if is_inbounds
                    # There must have been a `true` on the stack so
                    # `inbounds_stack` must not be empty
                    if !inbounds_stack[end]
                        is_inbounds = false
                    end
                    push!(bounds_elim_stack, false)
                    push!(bounds_push_pos_stack, i)
                elseif length(inbounds_stack) >= 2
                    code[i] = nothing
                    push!(bounds_elim_stack, true)
                else
                    push!(bounds_elim_stack, false)
                    push!(bounds_push_pos_stack, i)
                end
                push!(inbounds_stack, false)
            else
                # pop
                inbounds_len = length(inbounds_stack)
                if inbounds_len != 0
                    pop!(inbounds_stack)
                    inbounds_len -= 1
                end
                # This will also delete pops that don't match
                if (isempty(bounds_elim_stack) ? true :
                    pop!(bounds_elim_stack))
                    # No need to update `is_inbounds` since the push was a no-op
                    code[i] = nothing
                    continue
                end
                if inbounds_len >= 2
                    is_inbounds = (inbounds_stack[inbounds_len] ||
                                   inbounds_stack[inbounds_len - 1])
                elseif inbounds_len == 1
                    is_inbounds = inbounds_stack[inbounds_len]
                else
                    is_inbounds = false
                end
                push_idx = bounds_push_pos_stack[end]
                if length(bounds_push_pos_stack) > 1
                    pop!(bounds_push_pos_stack)
                end
                if push_idx > 0
                    code[push_idx] = nothing
                    code[i] = nothing
                else
                    bounds_push_pos_stack[end] = 0
                end
            end
            continue
        end
        if head !== :meta
            prev_dbg_stack[end] = 0
            push_loc_pos_stack[end] = 0
            bounds_push_pos_stack[end] = 0
            continue
        end
        nargs = length(args)
        if do_coverage || nargs == 0
            continue
        end
        arg1 = args[1]
        if arg1 === :push_loc
            push!(prev_dbg_stack, 0)
            push!(push_loc_pos_stack, i)
        elseif arg1 === :pop_loc
            prev_dbg = if length(prev_dbg_stack) > 1
                pop!(prev_dbg_stack)
            else
                prev_dbg_stack[end]
            end
            if prev_dbg > 0
                code[prev_dbg] = nothing
            end
            push_loc = if length(push_loc_pos_stack) > 1
                pop!(push_loc_pos_stack)
            else
                push_loc_pos_stack[end]
            end
            if push_loc > 0
                code[push_loc] = nothing
                code[i] = nothing
            else
                push_loc_pos_stack[end] = 0
            end
        else
            continue
        end
    end
    return filter!(x -> x !== nothing, code)
end

# does the same job as alloc_elim_pass for allocations inline in getfields
# TODO can probably be removed when we switch to a linear IR
function getfield_elim_pass!(sv::InferenceState)
    body = sv.src.code
    for i = 1:length(body)
        body[i] = _getfield_elim_pass!(body[i], sv)
    end
end

function _getfield_elim_pass!(e::Expr, sv::InferenceState)
    for i = 1:length(e.args)
        e.args[i] = _getfield_elim_pass!(e.args[i], sv)
    end
    if is_known_call(e, getfield, sv.src, sv.mod) && length(e.args)==3 &&
        (isa(e.args[3],Int) || isa(e.args[3],QuoteNode))
        e1 = e.args[2]
        j = e.args[3]
        if isa(e1,Expr)
            alloc = is_allocation(e1, sv)
            if alloc !== false
                flen, fnames = alloc
                if isa(j,QuoteNode)
                    j = findfirst(fnames, j.value)
                end
                if 1 <= j <= flen
                    ok = true
                    for k = 2:length(e1.args)
                        k == j+1 && continue
                        if !effect_free(e1.args[k], sv.src, sv.mod, true)
                            ok = false; break
                        end
                    end
                    if ok
                        return e1.args[j+1]
                    end
                end
            end
        elseif isa(e1,Tuple) && isa(j,Int) && (1 <= j <= length(e1))
            e1j = e1[j]
            if !(isa(e1j,Number) || isa(e1j,AbstractString) || isa(e1j,Tuple) ||
                 isa(e1j,Type))
                e1j = QuoteNode(e1j)
            end
            return e1j
        elseif isa(e1,QuoteNode) && isa(e1.value,Tuple) && isa(j,Int) && (1 <= j <= length(e1.value))
            return QuoteNode(e1.value[j])
        end
    end
    return e
end

_getfield_elim_pass!(e::ANY, sv) = e

# check if e is a successful allocation of an struct
# if it is, returns (n,f) such that it is always valid to call
# getfield(..., 1 <= x <= n) or getfield(..., x in f) on the result
function is_allocation(e::ANY, sv::InferenceState)
    isa(e, Expr) || return false
    if is_known_call(e, tuple, sv.src, sv.mod)
        return (length(e.args)-1,())
    elseif e.head === :new
        typ = widenconst(exprtype(e, sv.src, sv.mod))
        if isa(typ, DataType) && isleaftype(typ)
            nf = length(e.args) - 1
            names = fieldnames(typ)
            @assert(nf <= nfields(typ))
            if nf < nfields(typ)
                # some fields were left undef
                # we could potentially propagate Bottom
                # for pointer fields
                names = names[1:nf]
            end
            return (nf, names)
        end
    end
    false
end

# Replace branches with constant conditions with unconditional branches
function gotoifnot_elim_pass!(sv::InferenceState)
    body = sv.src.code
    i = 1
    while i < length(body)
        expr = body[i]
        i += 1
        isa(expr, Expr) || continue
        expr = expr::Expr
        expr.head === :gotoifnot || continue
        cond = expr.args[1]
        condt = exprtype(cond, sv.src, sv.mod)
        isa(condt, Const) || continue
        val = (condt::Const).val
        # Codegen should emit an unreachable if val is not a Bool so
        # we don't need to do anything (also, type inference currently
        # doesn't recognize the error for strictly non-Bool condition)
        if isa(val, Bool)
            # in case there's side effects... (like raising `UndefVarError`)
            body[i - 1] = cond
            if val === false
                insert!(body, i, GotoNode(expr.args[2]))
                i += 1
            end
        end
    end
end

# eliminate allocation of unnecessary objects
# that are only used as arguments to safe getfield calls
function alloc_elim_pass!(sv::InferenceState)
    body = sv.src.code
    bexpr = Expr(:block)
    bexpr.args = body
    vs, gs = find_sa_vars(sv.src, sv.nargs)
    remove_redundant_temp_vars(sv.src, sv.nargs, vs, Slot)
    remove_redundant_temp_vars(sv.src, sv.nargs, gs, SSAValue)
    i = 1
    while i < length(body)
        e = body[i]
        if !isa(e, Expr)
            i += 1
            continue
        end
        e = e::Expr
        if e.head === :(=) && (isa(e.args[1], SSAValue) ||
                               (isa(e.args[1], Slot) && haskey(vs, slot_id(e.args[1]))))
            var = e.args[1]
            rhs = e.args[2]
            # Need to make sure LLVM can recognize this as LLVM ssa value too
            is_ssa = (isa(var, SSAValue) ||
                      sv.src.slotflags[slot_id(var)] & Slot_UsedUndef == 0)
        else
            var = nothing
            rhs = e
            is_ssa = false # doesn't matter as long as it's a Bool...
        end
        alloc = is_allocation(rhs, sv)
        if alloc !== false
            nv, field_names = alloc
            tup = rhs.args
            # This makes sure the value doesn't escape so we can elide
            # allocation of mutable types too
            if (var !== nothing &&
                occurs_outside_getfield(bexpr, var, sv, nv, field_names))
                i += 1
                continue
            end

            deleteat!(body, i)  # remove tuple allocation
            # convert tuple allocation to a series of local var assignments
            n_ins = 0
            if var === nothing
                for j=1:nv
                    tupelt = tup[j+1]
                    if !(isa(tupelt,Number) || isa(tupelt,AbstractString) ||
                         isa(tupelt,QuoteNode) || isa(tupelt, SSAValue))
                        insert!(body, i+n_ins, tupelt)
                        n_ins += 1
                    end
                end
            else
                vals = Vector{Any}(nv)
                local new_slots::Vector{Int}
                if !is_ssa
                    new_slots = Vector{Int}(nv)
                end
                for j=1:nv
                    tupelt = tup[j+1]
                    # If `!is_ssa` we have to create new variables for each
                    # (used) fields in order to preserve the undef check.
                    if is_ssa && (isa(tupelt,Number) ||
                                  isa(tupelt,AbstractString) ||
                                  isa(tupelt,QuoteNode) || isa(tupelt, SSAValue))
                        vals[j] = tupelt
                    else
                        elty = exprtype(tupelt, sv.src, sv.mod)
                        if is_ssa
                            tmpv = newvar!(sv, elty)
                        else
                            tmpv = add_slot!(sv.src, widenconst(elty), false,
                                             sv.src.slotnames[slot_id(var)])
                            tmpv_id = slot_id(tmpv)
                            new_slots[j] = tmpv_id
                            sv.src.slotflags[tmpv_id] |= Slot_UsedUndef
                        end
                        tmp = Expr(:(=), tmpv, tupelt)
                        insert!(body, i+n_ins, tmp)
                        vals[j] = tmpv
                        n_ins += 1
                    end
                end
                replace_getfield!(bexpr, var, vals, field_names, sv)
                if !is_ssa
                    i += replace_newvar_node!(body, slot_id(var),
                                              new_slots, i)
                elseif isa(var, Slot)
                    # occurs_outside_getfield might have allowed
                    # void use of the slot, we need to delete them too
                    i -= delete_void_use!(body, var, i)
                end
            end
            # Do not increment counter and do the optimization recursively
            # on the allocation of fields too.
            # This line can probably be added back for linear IR
            # i += n_ins
        else
            i += 1
        end
    end
end

# Return the number of expressions added before `i0`
function replace_newvar_node!(body, orig, new_slots, i0)
    nvars = length(new_slots)
    nvars == 0 && return 0
    narg = length(body)
    i = 1
    nins = 0
    newvars = [ NewvarNode(SlotNumber(id)) for id in new_slots ]
    while i <= narg
        a = body[i]
        if isa(a, NewvarNode) && slot_id((a::NewvarNode).slot) == orig
            splice!(body, i, newvars)
            if i - nins < i0
                nins += nvars - 1
            end
            narg += nvars - 1
            i += nvars
        else
            i += 1
        end
    end
    return nins
end

# Return the number of expressions deleted before `i0`
function delete_void_use!(body, var::Slot, i0)
    narg = length(body)
    i = 1
    ndel = 0
    while i <= narg
        a = body[i]
        if isa(a, Slot) && slot_id(a) == slot_id(var)
            deleteat!(body, i)
            if i + ndel < i0
                ndel += 1
            end
            narg -= 1
        else
            i += 1
        end
    end
    return ndel
end

function replace_getfield!(e::Expr, tupname, vals, field_names, sv::InferenceState)
    for i = 1:length(e.args)
        a = e.args[i]
        if isa(a,Expr) && is_known_call(a, getfield, sv.src, sv.mod) &&
            symequal(a.args[2],tupname)
            idx = if isa(a.args[3], Int)
                a.args[3]
            else
                @assert isa(a.args[3], QuoteNode)
                findfirst(field_names, a.args[3].value)
            end
            @assert(idx > 0) # clients should check that all getfields are valid
            val = vals[idx]
            # original expression might have better type info than
            # the tuple element expression that's replacing it.
            if isa(val, Slot)
                val = val::Slot
                id = slot_id(val)
                valtyp = isa(val, TypedSlot) ? val.typ : sv.src.slottypes[id]
                if a.typ ⊑ valtyp && !(valtyp ⊑ a.typ)
                    if isa(val, TypedSlot)
                        val = TypedSlot(id, a.typ)
                    end
                    sv.src.slottypes[id] = widenconst(a.typ)
                end
            elseif isa(val,SSAValue)
                val = val::SSAValue
                typ = exprtype(val, sv.src, sv.mod)
                if a.typ ⊑ typ && !(typ ⊑ a.typ)
                    sv.src.ssavaluetypes[val.id + 1] = a.typ
                end
            end
            e.args[i] = val
        elseif isa(a, Expr)
            replace_getfield!(a::Expr, tupname, vals, field_names, sv)
        end
    end
end

# fix label numbers to always equal the statement index of the label
function reindex_labels!(sv::InferenceState)
    body = sv.src.code
    mapping = zeros(Int, sv.label_counter)
    for i = 1:length(body)
        el = body[i]
        if isa(el,LabelNode)
            mapping[el.label] = i
            body[i] = LabelNode(i)
        end
    end
    for i = 1:length(body)
        el = body[i]
        # For goto and enter, the statement and the target has to be
        # both reachable or both not. For gotoifnot, the dead code
        # elimination in type_annotate! can delete the target
        # of a reachable (but never taken) node. In which case we can
        # just replace the node with the branch condition.
        if isa(el,GotoNode)
            labelnum = mapping[el.label]
            @assert labelnum !== 0
            body[i] = GotoNode(labelnum)
        elseif isa(el,Expr)
            el = el::Expr
            if el.head === :gotoifnot
                labelnum = mapping[el.args[2]]
                if labelnum !== 0
                    el.args[2] = mapping[el.args[2]]
                else
                    # Might have side effects
                    body[i] = el.args[1]
                end
            elseif el.head === :enter
                labelnum = mapping[el.args[1]]
                @assert labelnum !== 0
                el.args[1] = labelnum
            end
        end
    end
end

function return_type(f::ANY, t::ANY)
    params = InferenceParams(ccall(:jl_get_tls_world_age, UInt, ()))
    rt = Union{}
    for m in _methods(f, t, -1, params.world)
        ty = typeinf_type(m[3], m[1], m[2], true, params)
        ty === nothing && return Any
        rt = tmerge(rt, ty)
        rt === Any && break
    end
    return rt
end

#### bootstrapping ####

# make sure that typeinf is executed before turning on typeinf_ext
# this ensures that typeinf_ext doesn't recurse before it can add the item to the workq
# especially try to make sure any recursive and leaf functions have concrete signatures,
# since we won't be able to specialize & infer them at runtime

let fs = Any[typeinf_ext, typeinf_loop, typeinf_edge, occurs_outside_getfield, eval_annotate, pure_eval_call]
    for x in t_ffunc_val
        push!(fs, x[3])
    end
    for i = 1:length(t_ifunc)
        if isassigned(t_ifunc, i)
            x = t_ifunc[i]
            push!(fs, x[3])
        end
    end
    for f in fs
        for m in _methods_by_ftype(Tuple{typeof(f), Vararg{Any}}, 10, typemax(UInt))
            # remove any TypeVars from the intersection
            typ = Any[m[1].parameters...]
            for i = 1:length(typ)
                if isa(typ[i], TypeVar)
                    typ[i] = typ[i].ub
                end
            end
            typeinf_type(m[3], Tuple{typ...}, m[2], true, InferenceParams(typemax(UInt)))
        end
    end
end
