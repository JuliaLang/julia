# This file is a part of Julia. License is MIT: https://julialang.org/license

import Core: _apply, svec, apply_type, Builtin, IntrinsicFunction, MethodInstance

#### parameters limiting potentially-infinite types ####
const MAX_TYPEUNION_LEN = 3
const MAX_TYPE_DEPTH = 8
const TUPLE_COMPLEXITY_LIMIT_DEPTH = 3

const MAX_INLINE_CONST_SIZE = 256

struct InferenceParams
    world::UInt

    # optimization
    inlining::Bool
    inline_cost_threshold::Int  # number of CPU cycles beyond which it's not worth inlining
    inline_nonleaf_penalty::Int # penalty for dynamic dispatch
    inline_tupleret_bonus::Int  # extra willingness for non-isbits tuple return types

    # parameters limiting potentially-infinite types (configurable)
    MAX_METHODS::Int
    MAX_TUPLETYPE_LEN::Int
    MAX_TUPLE_DEPTH::Int
    MAX_TUPLE_SPLAT::Int
    MAX_UNION_SPLITTING::Int
    MAX_APPLY_UNION_ENUM::Int

    # reasonable defaults
    function InferenceParams(world::UInt;
                    inlining::Bool = inlining_enabled(),
                    inline_cost_threshold::Int = 100,
                    inline_nonleaf_penalty::Int = 1000,
                    inline_tupleret_bonus::Int = 400,
                    max_methods::Int = 4,
                    tupletype_len::Int = 15,
                    tuple_depth::Int = 4,
                    tuple_splat::Int = 16,
                    union_splitting::Int = 4,
                    apply_union_enum::Int = 8)
        return new(world, inlining, inline_cost_threshold, inline_nonleaf_penalty,
                   inline_tupleret_bonus, max_methods, tupletype_len,
                   tuple_depth, tuple_splat, union_splitting, apply_union_enum)
    end
end

# alloc_elim_pass! relies on `Slot_AssignedOnce | Slot_UsedUndef` being
# SSA. This should be true now but can break if we start to track conditional
# constants. e.g.
#
#     cond && (a = 1)
#     other_code()
#     cond && use(a)

# slot property bit flags
const Slot_AssignedOnce = 16
const Slot_UsedUndef    = 32

#### inference state types ####

struct NotFound end
const NF = NotFound()
const LineNum = Int
const VarTable = Array{Any,1}

const isleaftype = _isleaftype

# The type of a variable load is either a value or an UndefVarError
mutable struct VarState
    typ
    undef::Bool
    VarState(@nospecialize(typ), undef::Bool) = new(typ, undef)
end

# The type of a value might be constant
struct Const
    val
    actual::Bool  # if true, we obtained `val` by actually calling a @pure function
    Const(@nospecialize(v)) = new(v, false)
    Const(@nospecialize(v), a::Bool) = new(v, a)
end

# The type of a value might be Bool,
# but where the value of the boolean can be used in back-propagation to
# limit the type of some other variable
# The Conditional type tracks the set of branches on variable type info
# that was used to create the boolean condition
mutable struct Conditional
    var::Union{Slot,SSAValue}
    vtype
    elsetype
    function Conditional(
                @nospecialize(var),
                @nospecialize(vtype),
                @nospecialize(nottype))
        return new(var, vtype, nottype)
    end
end

struct PartialTypeVar
    tv::TypeVar
    # N.B.: Currently unused, but would allow turning something back
    # into Const, if the bounds are pulled out of this TypeVar
    lb_certain::Bool
    ub_certain::Bool
    PartialTypeVar(tv::TypeVar, lb_certain::Bool, ub_certain::Bool) = new(tv, lb_certain, ub_certain)
end

function rewrap(@nospecialize(t), @nospecialize(u))
    isa(t, Const) && return t
    isa(t, Conditional) && return t
    return rewrap_unionall(t, u)
end

mutable struct InferenceState
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
    pc´´::LineNum
    nstmts::Int
    # current exception handler info
    cur_hand #::Tuple{LineNum, Tuple{LineNum, ...}}
    handler_at::Vector{Any}
    n_handlers::Int
    # ssavalue sparsity and restart info
    ssavalue_uses::Vector{IntSet}
    ssavalue_defs::Vector{LineNum}
    vararg_type_container #::Type

    backedges::Vector{Tuple{InferenceState, LineNum}} # call-graph backedges connecting from callee to caller
    callers_in_cycle::Vector{InferenceState}
    parent::Union{Void, InferenceState}

    const_api::Bool
    const_ret::Bool

    # TODO: put these in InferenceParams (depends on proper multi-methodcache support)
    optimize::Bool
    cached::Bool

    inferred::Bool

    dont_work_on_me::Bool

    # src is assumed to be a newly-allocated CodeInfo, that can be modified in-place to contain intermediate results
    function InferenceState(linfo::MethodInstance, src::CodeInfo,
                            optimize::Bool, cached::Bool, params::InferenceParams)
        code = src.code::Array{Any,1}
        nl = label_counter(code) + 1
        toplevel = !isa(linfo.def, Method)

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

        nssavalues = src.ssavaluetypes::Int
        src.ssavaluetypes = Any[ NF for i = 1:nssavalues ]

        n = length(code)
        s_edges = Any[ () for i = 1:n ]
        s_types = Any[ () for i = 1:n ]

        # initial types
        nslots = length(src.slotnames)
        s_types[1] = Any[ VarState(Bottom, true) for i = 1:nslots ]
        src.slottypes = Any[ Bottom for i = 1:nslots ]

        atypes = unwrap_unionall(linfo.specTypes)
        nargs::Int = toplevel ? 0 : linfo.def.nargs
        la = nargs
        vararg_type_container = nothing
        if la > 0
            if linfo.def.isva
                if atypes == Tuple
                    if la > 1
                        atypes = Tuple{Any[Any for i = 1:(la - 1)]..., Tuple.parameters[1]}
                    end
                    vararg_type = Tuple
                else
                    vararg_type_container = limit_tuple_depth(params, tupletype_tail(atypes, la))
                    vararg_type = tuple_tfunc(vararg_type_container) # returns a Const object, if applicable
                    vararg_type = rewrap(vararg_type, linfo.specTypes)
                end
                s_types[1][la] = VarState(vararg_type, false)
                src.slottypes[la] = vararg_type
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
                elseif isconstType(atyp)
                    atype = Const(atyp.parameters[1])
                else
                    atyp = rewrap_unionall(atyp, linfo.specTypes)
                end
                i == laty && (lastatype = atyp)
                s_types[1][i] = VarState(atyp, false)
                src.slottypes[i] = atyp
            end
            for i = (atail + 1):la
                s_types[1][i] = VarState(lastatype, false)
                src.slottypes[i] = lastatype
            end
        else
            @assert la == 0 # wrong number of arguments
        end

        ssavalue_uses = find_ssavalue_uses(code, nssavalues)
        ssavalue_defs = find_ssavalue_defs(code, nssavalues)

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
            inmodule = linfo.def::Module
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
            nargs, s_types, s_edges,
            Union{}, W, 1, n,
            cur_hand, handler_at, n_handlers,
            ssavalue_uses, ssavalue_defs, vararg_type_container,
            Vector{Tuple{InferenceState,LineNum}}(), # backedges
            Vector{InferenceState}(), # callers_in_cycle
            #=parent=#nothing,
            false, false, optimize, cached, false, false)
        return frame
    end
end

function InferenceState(linfo::MethodInstance,
                        optimize::Bool, cached::Bool, params::InferenceParams)
    # prepare an InferenceState object for inferring lambda
    src = retrieve_code_info(linfo)
    src === nothing && return nothing
    if JLOptions().debug_level == 2
        # this is a debug build of julia, so let's validate linfo
        errors = validate_code(linfo, src)
        if !isempty(errors)
            for e in errors
                println(STDERR, "WARNING: Encountered invalid lowered code for method ",
                        linfo.def, ": ", e)
            end
        end
    end
    return InferenceState(linfo, src, optimize, cached, params)
end

function get_staged(li::MethodInstance)
    return ccall(:jl_code_for_staged, Any, (Any,), li)::CodeInfo
end


#### debugging utilities ####

function print_callstack(sv::InferenceState)
    while sv !== nothing
        println(sv.linfo)
        for cycle in sv.callers_in_cycle
            println(' ', cycle.linfo)
        end
        sv = sv.parent
    end
end


#### helper functions ####

# create copies of the CodeInfo definition, and any fields that type-inference might modify
function copy_code_info(c::CodeInfo)
    cnew = ccall(:jl_copy_code_info, Ref{CodeInfo}, (Any,), c)
    cnew.code = copy_exprargs(cnew.code)
    cnew.slotnames = copy(cnew.slotnames)
    cnew.slotflags = copy(cnew.slotflags)
    return cnew
end

function retrieve_code_info(linfo::MethodInstance)
    m = linfo.def::Method
    if isdefined(m, :generator)
        try
            # user code might throw errors – ignore them
            c = get_staged(linfo)
        catch
            return nothing
        end
    else
        # TODO: post-inference see if we can swap back to the original arrays?
        if isa(m.source, Array{UInt8,1})
            c = ccall(:jl_uncompress_ast, Any, (Any, Any), m, m.source)
        else
            c = copy_code_info(m.source)
        end
    end
    return c
end

@inline slot_id(s) = isa(s, SlotNumber) ? (s::SlotNumber).id : (s::TypedSlot).id # using a function to ensure we can infer this

# avoid cycle due to over-specializing `any` when used by inference
function _any(@nospecialize(f), a)
    for x in a
        f(x) && return true
    end
    return false
end

function contains_is(itr, @nospecialize(x))
    for y in itr
        if y === x
            return true
        end
    end
    return false
end

anymap(f::Function, a::Array{Any,1}) = Any[ f(a[i]) for i=1:length(a) ]

_topmod(sv::InferenceState) = _topmod(sv.mod)
_topmod(m::Module) = ccall(:jl_base_relative_to, Any, (Any,), m)::Module

function istopfunction(topmod, @nospecialize(f), sym)
    if isdefined(Main, :Base) && isdefined(Main.Base, sym) && isconst(Main.Base, sym) && f === getfield(Main.Base, sym)
        return true
    elseif isdefined(topmod, sym) && isconst(topmod, sym) && f === getfield(topmod, sym)
        return true
    end
    return false
end

isknownlength(t::DataType) = !isvatuple(t) ||
    (length(t.parameters) > 0 && isa(unwrap_unionall(t.parameters[end]).parameters[2],Int))

# t[n:end]
tupletype_tail(@nospecialize(t), n) = Tuple{t.parameters[n:end]...}

function is_specializable_vararg_slot(arg, sv::InferenceState)
    return (isa(arg, Slot) && slot_id(arg) == sv.nargs &&
            isa(sv.vararg_type_container, DataType))
end


#### type-functions for builtins / intrinsics ####

const _Type_name = Type.body.name
isType(@nospecialize t) = isa(t, DataType) && (t::DataType).name === _Type_name

# true if Type is inlineable as constant (is a singleton)
function isconstType(@nospecialize t)
    isType(t) || return false
    p1 = t.parameters[1]
    # typeof(Bottom) is special since even though it is as leaftype,
    # at runtime, it might be Type{Union{}} instead, so don't attempt inference of it
    p1 === typeof(Union{}) && return false
    p1 === Union{} && return true
    isleaftype(p1) && return true
    return false
end

iskindtype(@nospecialize t) = (t === DataType || t === UnionAll || t === Union || t === typeof(Bottom))

const IInf = typemax(Int) # integer infinity
const n_ifunc = reinterpret(Int32, arraylen) + 1
const t_ifunc = Array{Tuple{Int, Int, Any}, 1}(n_ifunc)
const t_ifunc_cost = Array{Int, 1}(n_ifunc)
const t_ffunc_key = Array{Any, 1}(0)
const t_ffunc_val = Array{Tuple{Int, Int, Any}, 1}(0)
const t_ffunc_cost = Array{Int, 1}(0)
function add_tfunc(f::IntrinsicFunction, minarg::Int, maxarg::Int, @nospecialize(tfunc), cost::Int)
    idx = reinterpret(Int32, f) + 1
    t_ifunc[idx] = (minarg, maxarg, tfunc)
    t_ifunc_cost[idx] = cost
end
# TODO: add @nospecialize on `f` and declare its type as `Builtin` when that's supported
function add_tfunc(f::Function, minarg::Int, maxarg::Int, @nospecialize(tfunc), cost::Int)
    push!(t_ffunc_key, f)
    push!(t_ffunc_val, (minarg, maxarg, tfunc))
    push!(t_ffunc_cost, cost)
end

add_tfunc(throw, 1, 1, (@nospecialize(x)) -> Bottom, 0)

# the inverse of typeof_tfunc
# returns (type, isexact)
# if isexact is false, the actual runtime type may (will) be a subtype of t
function instanceof_tfunc(@nospecialize(t))
    if t === Bottom || t === typeof(Bottom)
        return Bottom, true
    elseif isa(t, Const)
        if isa(t.val, Type)
            return t.val, true
        end
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
add_tfunc(Core.Intrinsics.llvmcall, 3, IInf,
          (@nospecialize(fptr), @nospecialize(rt), @nospecialize(at), a...) -> instanceof_tfunc(rt)[1], 10)
cglobal_tfunc(@nospecialize(fptr)) = Ptr{Void}
cglobal_tfunc(@nospecialize(fptr), @nospecialize(t)) = (isType(t) ? Ptr{t.parameters[1]} : Ptr)
cglobal_tfunc(@nospecialize(fptr), t::Const) = (isa(t.val, Type) ? Ptr{t.val} : Ptr)
add_tfunc(Core.Intrinsics.cglobal, 1, 2, cglobal_tfunc, 5)
add_tfunc(Core.Intrinsics.select_value, 3, 3,
    function (@nospecialize(cnd), @nospecialize(x), @nospecialize(y))
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
    end, 1)
add_tfunc(===, 2, 2,
    function (@nospecialize(x), @nospecialize(y))
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
    end, 1)
function isdefined_tfunc(args...)
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
            elseif idx <= 0 || (!isvatuple(a1) && idx > fieldcount(a1))
                return Const(false)
            elseif !isvatuple(a1) && isbits(fieldtype(a1, idx))
                return Const(true)
            elseif isa(arg1, Const) && isimmutable((arg1::Const).val)
                return Const(isdefined((arg1::Const).val, idx))
            end
        end
    end
    Bool
end
# TODO change IInf to 2 when deprecation is removed
add_tfunc(isdefined, 1, IInf, isdefined_tfunc, 1)
_const_sizeof(@nospecialize(x)) = try
    # Constant Vector does not have constant size
    isa(x, Vector) && return Int
    return Const(Core.sizeof(x))
catch
    return Int
end
add_tfunc(Core.sizeof, 1, 1,
          function (@nospecialize(x),)
              isa(x, Const) && return _const_sizeof(x.val)
              isa(x, Conditional) && return _const_sizeof(Bool)
              isconstType(x) && return _const_sizeof(x.parameters[1])
              x !== DataType && isleaftype(x) && return _const_sizeof(x)
              return Int
          end, 0)
old_nfields(@nospecialize x) = length((isa(x,DataType) ? x : typeof(x)).types)
add_tfunc(nfields, 1, 1,
    function (@nospecialize(x),)
        isa(x,Const) && return Const(old_nfields(x.val))
        isa(x,Conditional) && return Const(old_nfields(Bool))
        if isType(x)
            # TODO: remove with deprecation in builtins.c for nfields(::Type)
            isleaftype(x.parameters[1]) && return Const(old_nfields(x.parameters[1]))
        elseif isa(x,DataType) && !x.abstract && !(x.name === Tuple.name && isvatuple(x)) && x !== DataType
            return Const(length(x.types))
        end
        return Int
    end, 0)
add_tfunc(Core._expr, 1, IInf, (args...)->Expr, 100)
add_tfunc(applicable, 1, IInf, (@nospecialize(f), args...)->Bool, 100)
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
                      if isexact
                          return Const(true)
                      end
                  elseif isa(v, Const) || isa(v, Conditional) || (isleaftype(v) && !iskindtype(v))
                      return Const(false)
                  elseif isexact && typeintersect(v, t) === Bottom
                      if !iskindtype(v) #= subtyping currently intentionally answers this query incorrectly for kinds =#
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

function type_depth(@nospecialize(t))
    if t === Bottom
        return 0
    elseif isa(t, Union)
        return max(type_depth(t.a), type_depth(t.b)) + 1
    elseif isa(t, DataType)
        return (t::DataType).depth
    elseif isa(t, UnionAll)
        if t.var.ub === Any && t.var.lb === Bottom
            return type_depth(t.body)
        end
        return max(type_depth(t.var.ub) + 1, type_depth(t.var.lb) + 1, type_depth(t.body))
    end
    return 0
end

function limit_type_depth(@nospecialize(t), d::Int)
    r = limit_type_depth(t, d, true, TypeVar[])
    @assert !isa(t, Type) || t <: r
    return r
end

function limit_type_depth(@nospecialize(t), d::Int, cov::Bool, vars::Vector{TypeVar}=TypeVar[])
    if isa(t, Union)
        if d < 0
            if cov
                return Any
            else
                var = TypeVar(:_)
                push!(vars, var)
                return var
            end
        end
        return Union{limit_type_depth(t.a, d - 1, cov, vars),
                     limit_type_depth(t.b, d - 1, cov, vars)}
    elseif isa(t, UnionAll)
        v = t.var
        if v.ub === Any
            if v.lb === Bottom
                return UnionAll(t.var, limit_type_depth(t.body, d, cov, vars))
            end
            ub = Any
        else
            ub = limit_type_depth(v.ub, d - 1, cov, vars)
        end
        if v.lb === Bottom || type_depth(v.lb) > d
            # note: lower bounds need to be widened by making them lower
            lb = Bottom
        else
            lb = v.lb
        end
        v2 = TypeVar(v.name, lb, ub)
        return UnionAll(v2, limit_type_depth(t{v2}, d, cov, vars))
    elseif !isa(t,DataType)
        return t
    end
    P = t.parameters
    isempty(P) && return t
    if d < 0
        if isvarargtype(t)
            # never replace Vararg with non-Vararg
            # passing depth=0 avoids putting a bare typevar here, for the diagonal rule
            return Vararg{limit_type_depth(P[1], 0, cov, vars), P[2]}
        end
        widert = t.name.wrapper
        if !(t <: widert)
            # This can happen when a typevar has bounds too wide for its context, e.g.
            # `Complex{T} where T` is not a subtype of `Complex`. In that case widen even
            # faster to something safe to ensure the result is a supertype of the input.
            widert = Any
        end
        cov && return widert
        var = TypeVar(:_, widert)
        push!(vars, var)
        return var
    end
    stillcov = cov && (t.name === Tuple.name)
    newdepth = d - 1
    if isvarargtype(t)
        newdepth = max(newdepth, 0)
    end
    Q = map(x -> limit_type_depth(x, newdepth, stillcov, vars), P)
    R = t.name.wrapper{Q...}
    if cov && !stillcov
        for var in vars
            R = UnionAll(var, R)
        end
    end
    return R
end

# limit the complexity of type `t` to be simpler than the comparison type `compare`
# no new values may be introduced, so the parameter `source` encodes the set of all values already present
function limit_type_size(@nospecialize(t), @nospecialize(compare), @nospecialize(source))
    source = svec(unwrap_unionall(compare), unwrap_unionall(source))
    source[1] === source[2] && (source = svec(source[1]))
    type_more_complex(t, compare, source, TUPLE_COMPLEXITY_LIMIT_DEPTH) || return t
    r = _limit_type_size(t, compare, source)
    @assert t <: r
    #@assert r === _limit_type_size(r, t, source) # this monotonicity constraint is slightly stronger than actually required,
      # since we only actually need to demonstrate that repeated application would reaches a fixed point,
      #not that it is already at the fixed point
    return r
end

sym_isless(a::Symbol, b::Symbol) = ccall(:strcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}), a, b) < 0

function type_more_complex(@nospecialize(t), @nospecialize(c), sources::SimpleVector, tupledepth::Int)
    # detect cases where the comparison is trivial
    if t === c
        return false
    elseif t === Union{}
        return false # Bottom is as simple as they come
    elseif isa(t, DataType) && isempty(t.parameters)
        return false # fastpath: unparameterized types are always finite
    elseif tupledepth > 0 && isa(unwrap_unionall(t), DataType) && isa(c, Type) && c !== Union{} && c <: t
        return false # t is already wider than the comparison in the type lattice
    elseif tupledepth > 0 && is_derived_type_from_any(unwrap_unionall(t), sources)
        return false # t isn't something new
    end
    # peel off wrappers
    if isa(c, UnionAll)
        # allow wrapping type with fewer UnionAlls than comparison if in a covariant context
        if !isa(t, UnionAll) && tupledepth == 0
            return true
        end
        t = unwrap_unionall(t)
        c = unwrap_unionall(c)
    end
    # rules for various comparison types
    if isa(c, TypeVar)
        if isa(t, TypeVar)
            return !(t.lb === Union{} || t.lb === c.lb) || # simplify lb towards Union{}
                   type_more_complex(t.ub, c.ub, sources, tupledepth)
        end
        c.lb === Union{} || return true
        return type_more_complex(t, c.ub, sources, max(tupledepth, 1)) # allow replacing a TypeVar with a concrete value
    elseif isa(c, Union)
        if isa(t, Union)
            return type_more_complex(t.a, c.a, sources, tupledepth) ||
                   type_more_complex(t.b, c.b, sources, tupledepth)
        end
        return type_more_complex(t, c.a, sources, tupledepth) &&
               type_more_complex(t, c.b, sources, tupledepth)
    elseif isa(t, Int) && isa(c, Int)
        return t !== 1 # alternatively, could use !(0 <= t < c)
    end
    # base case for data types
    if isa(t, DataType)
        tP = t.parameters
        if isa(c, DataType) && t.name === c.name
            cP = c.parameters
            length(cP) < length(tP) && return true
            ntail = length(cP) - length(tP) # assume parameters were dropped from the tuple head
            # allow creating variation within a nested tuple, but only so deep
            if t.name === Tuple.name && tupledepth > 0
                tupledepth -= 1
            elseif !isvarargtype(t)
                tupledepth = 0
            end
            isgenerator = (t.name.name === :Generator && t.name.module === _topmod(t.name.module))
            for i = 1:length(tP)
                tPi = tP[i]
                cPi = cP[i + ntail]
                if isgenerator
                    let tPi = unwrap_unionall(tPi),
                        cPi = unwrap_unionall(cPi)
                        if isa(tPi, DataType) && isa(cPi, DataType) &&
                                !tPi.abstract && !cPi.abstract &&
                                sym_isless(cPi.name.name, tPi.name.name)
                            # allow collect on (anonymous) Generators to nest, provided that their functions are appropriately ordered
                            # TODO: is there a better way?
                            continue
                        end
                    end
                end
                type_more_complex(tPi, cPi, sources, tupledepth) && return true
            end
            return false
        end
        if isType(t) # allow taking typeof any source type anywhere as Type{...}, as long as it isn't nesting Type{Type{...}}
            tt = unwrap_unionall(t.parameters[1])
            if isa(tt, DataType) && !isType(tt)
                is_derived_type_from_any(tt, sources) || return true
                return false
            end
        end
    end
    return true
end

function is_derived_type(@nospecialize(t), @nospecialize(c)) # try to find `type` somewhere in `comparison` type
    t === c && return true
    if isa(c, TypeVar)
        # see if it is replacing a TypeVar upper bound with something simpler
        return is_derived_type(t, c.ub)
    elseif isa(c, Union)
        # see if it is one of the elements of the union
        return is_derived_type(t, c.a) || is_derived_type(t, c.b)
    elseif isa(c, UnionAll)
        # see if it is derived from the body
        return is_derived_type(t, c.body)
    elseif isa(c, DataType)
        if isa(t, DataType)
            # see if it is one of the supertypes of a parameter
            super = supertype(c)
            while super !== Any
                t === super && return true
                super = supertype(super)
            end
        end
        # see if it was extracted from a type parameter
        cP = c.parameters
        for p in cP
            is_derived_type(t, p) && return true
        end
        if isleaftype(c) && isbits(c)
            # see if it was extracted from a fieldtype
            # however, only look through types that can be inlined
            # to ensure monotonicity of derivation
            # since we know that for immutable types,
            # the field types must have been constructed prior to the type,
            # it cannot have a reference cycle in the type graph
            cF = c.types
            for f in cF
                is_derived_type(t, f) && return true
            end
        end
    end
    return false
end

function is_derived_type_from_any(@nospecialize(t), sources::SimpleVector)
    for s in sources
        is_derived_type(t, s) && return true
    end
    return false
end

function _limit_type_size(@nospecialize(t), @nospecialize(c), sources::SimpleVector) # type vs. comparison which was derived from source
    if t === c
        return t # quick egal test
    elseif t === Union{}
        return t # easy case
    elseif isa(t, DataType) && isempty(t.parameters)
        return t # fast path: unparameterized are always simple
    elseif isa(unwrap_unionall(t), DataType) && isa(c, Type) && c !== Union{} && c <: t
        return t # t is already wider than the comparison in the type lattice
    elseif is_derived_type_from_any(unwrap_unionall(t), sources)
        return t # t isn't something new
    end
    if isa(t, TypeVar)
        if isa(c, TypeVar)
            if t.ub === c.ub && t.lb === c.lb
                return t
            end
        end
    elseif isa(t, Union)
        if isa(c, Union)
            a = _limit_type_size(t.a, c.a, sources)
            b = _limit_type_size(t.b, c.b, sources)
            return Union{a, b}
        end
    elseif isa(t, UnionAll)
        if isa(c, UnionAll)
            tv = t.var
            cv = c.var
            if tv.ub === cv.ub
                if tv.lb === cv.lb
                    return UnionAll(tv, _limit_type_size(t.body, c.body, sources))
                end
                ub = tv.ub
            else
                ub = _limit_type_size(tv.ub, cv.ub, sources)
            end
            if tv.lb === cv.lb
                lb = tv.lb
            else
                # note: lower bounds need to be widened by making them lower
                lb = Bottom
            end
            v2 = TypeVar(tv.name, lb, ub)
            return UnionAll(v2, _limit_type_size(t{v2}, c{v2}, sources))
        end
        tbody = _limit_type_size(t.body, c, sources)
        tbody === t.body && return t
        return UnionAll(t.var, tbody)
    elseif isa(t, DataType)
        if isa(c, DataType)
            tP = t.parameters
            cP = c.parameters
            if t.name === c.name && !isempty(cP)
                if isvarargtype(t)
                    VaT = _limit_type_size(tP[1], cP[1], sources)
                    N = tP[2]
                    if isa(N, TypeVar) || N === cP[2]
                        return Vararg{VaT, N}
                    end
                    return Vararg{VaT}
                elseif t.name === Tuple.name
                    # for covariant datatypes (aka Tuple),
                    # apply type-size limit elementwise
                    np = min(length(tP), length(cP))
                    Q = Any[ tP[i] for i in 1:np ]
                    if length(tP) > np # implies Tuple
                        # combine tp[np:end] into tP[np] using Vararg
                        Q[np] = tuple_tail_elem(Bottom, Any[ tP[i] for i in np:length(tP) ])
                    end
                    for i = 1:np
                        Q[i] = _limit_type_size(Q[i], cP[i], sources)
                    end
                    return Tuple{Q...}
                end
            end
        end
        if isType(t) # allow taking typeof as Type{...}, but ensure it doesn't start nesting
            tt = unwrap_unionall(t.parameters[1])
            if isa(tt, DataType) && !isType(tt)
                is_derived_type_from_any(tt, sources) && return t
            end
        end
        if isvarargtype(t)
            # never replace Vararg with non-Vararg
            return Vararg
        end
        widert = t.name.wrapper
        if !(t <: widert)
            # This can happen when a typevar has bounds too wide for its context, e.g.
            # `Complex{T} where T` is not a subtype of `Complex`. In that case widen even
            # faster to something safe to ensure the result is a supertype of the input.
            return Any
        end
        return widert
    end
    return Any
end


const DataType_name_fieldindex = fieldindex(DataType, :name)
const DataType_parameters_fieldindex = fieldindex(DataType, :parameters)
const DataType_types_fieldindex = fieldindex(DataType, :types)
const DataType_super_fieldindex = fieldindex(DataType, :super)

const TypeName_name_fieldindex = fieldindex(TypeName, :name)
const TypeName_module_fieldindex = fieldindex(TypeName, :module)
const TypeName_wrapper_fieldindex = fieldindex(TypeName, :wrapper)

function const_datatype_getfield_tfunc(sv, fld)
    if (fld == DataType_name_fieldindex ||
            fld == DataType_parameters_fieldindex ||
            fld == DataType_types_fieldindex ||
            fld == DataType_super_fieldindex)
        return abstract_eval_constant(getfield(sv, fld))
    end
    return nothing
end

getfield_tfunc(@nospecialize(s00), @nospecialize(name), @nospecialize(inbounds)) =
    getfield_tfunc(s00, name)
function getfield_tfunc(@nospecialize(s00), @nospecialize(name))
    if isa(s00, TypeVar)
        s00 = s00.ub
    end
    s = unwrap_unionall(s00)
    if isa(s, Union)
        return tmerge(rewrap(getfield_tfunc(s.a, name),s00),
                      rewrap(getfield_tfunc(s.b, name),s00))
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
            elseif isa(sv, TypeName)
                fld = isa(nv, Symbol) ? fieldindex(TypeName, nv, false) : nv
                if (fld == TypeName_name_fieldindex ||
                    fld == TypeName_module_fieldindex ||
                    fld == TypeName_wrapper_fieldindex)
                    return abstract_eval_constant(getfield(sv, fld))
                end
            end
            if isa(sv, Module) && isa(nv, Symbol)
                return abstract_eval_global(sv, nv)
            end
            if !(isa(nv,Symbol) || isa(nv,Int))
                return Bottom
            end
            if (isa(sv, SimpleVector) || isimmutable(sv)) && isdefined(sv, nv)
                return abstract_eval_constant(getfield(sv, nv))
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
        R = reduce(tmerge, Bottom, map(t -> rewrap_unionall(unwrapva(t), s00), s.types))
        # do the same limiting as the known-symbol case to preserve type-monotonicity
        if isempty(s.parameters)
            return R
        end
        return limit_type_depth(R, MAX_TYPE_DEPTH)
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
    if sp !== nothing
        t = const_datatype_getfield_tfunc(sp, fld)
        t !== nothing && return t
    end
    R = s.types[fld]
    if isempty(s.parameters)
        return R
    end
    # TODO jb/subtype is this still necessary?
    # conservatively limit the type depth here,
    # since the UnionAll type bound is otherwise incorrect
    # in the current type system
    return rewrap_unionall(limit_type_depth(R, MAX_TYPE_DEPTH), s00)
end
add_tfunc(getfield, 2, 3, getfield_tfunc, 1)
add_tfunc(setfield!, 3, 3, (@nospecialize(o), @nospecialize(f), @nospecialize(v)) -> v, 3)
fieldtype_tfunc(@nospecialize(s0), @nospecialize(name), @nospecialize(inbounds)) =
    fieldtype_tfunc(s0, name)
function fieldtype_tfunc(@nospecialize(s0), @nospecialize(name))
    if s0 === Any || s0 === Type || DataType ⊑ s0 || UnionAll ⊑ s0
        return Type
    end
    # fieldtype only accepts DataType and UnionAll, errors on `Module`
    if isa(s0,Const) && (!(isa(s0.val,DataType) || isa(s0.val,UnionAll)) || s0.val === Module)
        return Bottom
    end
    if s0 == Type{Module} || s0 == Type{Union{}} || isa(s0, Conditional)
        return Bottom
    end

    s = instanceof_tfunc(s0)[1]
    u = unwrap_unionall(s)

    if isa(u,Union)
        return tmerge(rewrap(fieldtype_tfunc(u.a, name),s),
                      rewrap(fieldtype_tfunc(u.b, name),s))
    end

    if !isa(u,DataType) || u.abstract
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
        return reduce(tmerge, Bottom,
                      Any[ fieldtype_tfunc(s0, Const(i)) for i = 1:length(ftypes) ])
    end

    fld = name.val
    if isa(fld,Symbol)
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
    ft = rewrap_unionall(ft,s)
    if exact
        return Const(ft)
    end
    return Type{<:ft}
end
add_tfunc(fieldtype, 2, 3, fieldtype_tfunc, 0)

function valid_tparam(@nospecialize(x))
    if isa(x,Tuple)
        for t in x
            !valid_tparam(t) && return false
        end
        return true
    end
    return isa(x,Int) || isa(x,Symbol) || isa(x,Bool) || (!isa(x,Type) && isbits(x))
end

has_free_typevars(@nospecialize(t)) = ccall(:jl_has_free_typevars, Cint, (Any,), t)!=0

# TODO: handle e.g. apply_type(T, R::Union{Type{Int32},Type{Float64}})
function apply_type_tfunc(@nospecialize(headtypetype), @nospecialize args...)
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
            # TODO: return `Bottom` for trying to apply a non-UnionAll
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
    if uncertain && type_too_complex(appl, MAX_TYPE_DEPTH)
        return Type{<:headtype}
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
add_tfunc(apply_type, 1, IInf, apply_type_tfunc, 10)

@pure function type_typeof(@nospecialize(v))
    if isa(v, Type)
        return Type{v}
    end
    return typeof(v)
end

function invoke_tfunc(@nospecialize(f), @nospecialize(types), @nospecialize(argtype), sv::InferenceState)
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
    (ti, env) = ccall(:jl_type_intersection_with_env, Any, (Any, Any), argtype, meth.sig)::SimpleVector
    rt, edge = typeinf_edge(meth::Method, ti, env, sv)
    edge !== nothing && add_backedge!(edge::MethodInstance, sv)
    return rt
end

function tuple_tfunc(@nospecialize(argtype))
    if isa(argtype, DataType) && argtype.name === Tuple.name
        p = Vector{Any}()
        for x in argtype.parameters
            if isType(x) && !isa(x.parameters[1], TypeVar)
                xparam = x.parameters[1]
                if isleaftype(xparam) || xparam === Bottom
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

function builtin_tfunction(@nospecialize(f), argtypes::Array{Any,1},
                           sv::Union{InferenceState,Void}, params::InferenceParams = sv.params)
    isva = !isempty(argtypes) && isvarargtype(argtypes[end])
    if f === tuple
        for a in argtypes
            if !isa(a, Const)
                return tuple_tfunc(limit_tuple_depth(params, argtypes_to_type(argtypes)))
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
            if isa(sigty, Type) && sigty <: Tuple && sv !== nothing
                return invoke_tfunc(af, sigty, argtypes_to_type(argtypes[3:end]), sv)
            end
        end
        return Any
    end
    if isva
        return Any
    end
    if isa(f, IntrinsicFunction)
        iidx = Int(reinterpret(Int32, f::IntrinsicFunction)) + 1
        if iidx < 0 || iidx > length(t_ifunc)
            # invalid intrinsic
            return Any
        end
        tf = t_ifunc[iidx]
    else
        fidx = findfirst(x->x===f, t_ffunc_key)
        if fidx == 0
            # unknown/unhandled builtin function
            return Any
        end
        tf = t_ffunc_val[fidx]
    end
    tf = tf::Tuple{Int, Int, Any}
    if !(tf[1] <= length(argtypes) <= tf[2])
        # wrong # of args
        return Bottom
    end
    return tf[3](argtypes...)
end

limit_tuple_depth(params::InferenceParams, @nospecialize(t)) = limit_tuple_depth_(params,t,0)

function limit_tuple_depth_(params::InferenceParams, @nospecialize(t), d::Int)
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

limit_tuple_type = (@nospecialize(t), params::InferenceParams) -> limit_tuple_type_n(t, params.MAX_TUPLETYPE_LEN)

function limit_tuple_type_n(@nospecialize(t), lim::Int)
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

# return an upper-bound on type `a` with type `b` removed
# such that `return <: a` && `Union{return, b} == Union{a, b}`
function typesubtract(@nospecialize(a), @nospecialize(b))
    if a <: b
        return Bottom
    end
    if isa(a, Union)
        return Union{typesubtract(a.a, b),
                     typesubtract(a.b, b)}
    end
    return a # TODO: improve this bound?
end

#### recursing into expression ####

# take a Tuple where one or more parameters are Unions
# and return an array such that those Unions are removed
# and `Union{return...} == ty`
function switchtupleunion(@nospecialize(ty))
    tparams = (unwrap_unionall(ty)::DataType).parameters
    return _switchtupleunion(Any[tparams...], length(tparams), [], ty)
end

function _switchtupleunion(t::Vector{Any}, i::Int, tunion::Vector{Any}, @nospecialize(origt))
    if i == 0
        tpl = rewrap_unionall(Tuple{t...}, origt)
        push!(tunion, tpl)
    else
        ti = t[i]
        if isa(ti, Union)
            for ty in uniontypes(ti::Union)
                t[i] = ty
                _switchtupleunion(t, i - 1, tunion, origt)
            end
            t[i] = ti
        else
            _switchtupleunion(t, i - 1, tunion, origt)
        end
    end
    return tunion
end

function abstract_call_gf_by_type(@nospecialize(f), @nospecialize(atype), sv::InferenceState)
    # don't consider more than N methods. this trades off between
    # compiler performance and generated code performance.
    # typically, considering many methods means spending lots of time
    # obtaining poor type information.
    # It is important for N to be >= the number of methods in the error()
    # function, so we can still know that error() is always Bottom.
    # here I picked 4.
    argtype = limit_tuple_type(atype, sv.params)
    argtypes = unwrap_unionall(argtype).parameters
    ft = unwrap_unionall(argtypes[1]) # TODO: ccall jl_first_argument_datatype here
    isa(ft, DataType) || return Any # the function being called is unknown. can't properly handle this backedge right now
    ftname = ft.name
    isdefined(ftname, :mt) || return Any # not callable. should be Bottom, but can't track this backedge right now
    if ftname === _Type_name
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
    splitunions = 1 < countunionsplit(argtypes) <= sv.params.MAX_UNION_SPLITTING
    if splitunions
        splitsigs = switchtupleunion(argtype)
        applicable = Any[]
        for sig_n in splitsigs
            xapplicable = _methods_by_ftype(sig_n, sv.params.MAX_METHODS, sv.params.world, min_valid, max_valid)
            xapplicable === false && return Any
            append!(applicable, xapplicable)
        end
    else
        applicable = _methods_by_ftype(argtype, sv.params.MAX_METHODS, sv.params.world, min_valid, max_valid)
        if applicable === false
            # this means too many methods matched
            return Any
        end
    end
    applicable = applicable::Array{Any,1}
    napplicable = length(applicable)
    fullmatch = false
    rettype = Bottom
    for i in 1:napplicable
        match = applicable[i]::SimpleVector
        method = match[3]::Method
        if !fullmatch && (argtype <: method.sig)
            fullmatch = true
        end
        sig = match[1]
        sigtuple = unwrap_unionall(sig)::DataType
        splitunions = false
        # TODO: splitunions = 1 < countunionsplit(sigtuple.parameters) * napplicable <= sv.params.MAX_UNION_SPLITTING
        # currently this triggers a bug in inference recursion detection
        if splitunions
            splitsigs = switchtupleunion(sig)
            for sig_n in splitsigs
                rt = abstract_call_method(method, f, sig_n, svec(), sv)
                rettype = tmerge(rettype, rt)
                rettype === Any && break
            end
            rettype === Any && break
        else
            rt = abstract_call_method(method, f, sig, match[2]::SimpleVector, sv)
            rettype = tmerge(rettype, rt)
            rettype === Any && break
        end
    end
    if !(fullmatch || rettype === Any)
        # also need an edge to the method table in case something gets
        # added that did not intersect with any existing method
        add_mt_backedge(ftname.mt, argtype, sv)
        update_valid_age!(min_valid[1], max_valid[1], sv)
    end
    #print("=> ", rettype, "\n")
    return rettype
end

function abstract_call_method(method::Method, @nospecialize(f), @nospecialize(sig), sparams::SimpleVector, sv::InferenceState)
    sigtuple = unwrap_unionall(sig)::DataType

    tm = _topmod(sv)
    if (# promote_typeof signature may be used with many arguments
          !istopfunction(tm, f, :promote_typeof)
        # assume getindex methods aren't directly recursive, since wrappers like ReshapedArrays won't look like it here
        # should still manage to detect recursive growth either via other intermediate methods or actual type-equal signature recursion
       && !istopfunction(tm, f, :getindex)
       && !istopfunction(tm, f, :setindex!)
        # the construct-to-convert method is a bottleneck in inference,
        # so just assume that recursion will get prevented at some other point
       && !(method.sig == Tuple{Type, Any}))
        # otherwise: limit argument type tuple growth of all other functions
        msig = unwrap_unionall(method.sig)
        lsig = length(msig.parameters)
        ls = length(sigtuple.parameters)
        # look through the parents list to find the topmost
        # function call to the same method
        cyclei = 0
        infstate = sv
        topmost = nothing
        while infstate !== nothing
            infstate = infstate::InferenceState
            if method === infstate.linfo.def
                if infstate.linfo.specTypes == sig
                    # avoid widening when detecting self-recursion
                    # TODO: merge call cycle and return right away
                    topmost = nothing
                    break
                end
                topmost === nothing && (topmost = infstate)
            end
            # iterate through the cycle before walking to the parent
            if cyclei < length(infstate.callers_in_cycle)
                cyclei += 1
                infstate = infstate.callers_in_cycle[cyclei]
            else
                cyclei = 0
                infstate = infstate.parent
            end
        end

        # TODO: FIXME: this heuristic depends on non-local state making type-inference unpredictable
        # it also should be integrated into the cycle resolution and iterated to convergence
        if topmost !== nothing
            # impose limit if we recur on the same method and the argument type complexity is growing or is beyond MAX_TYPE_DEPTH
            newsig = sig
            if !isempty(topmost.callers_in_cycle)
                # already discovered this method causes dependent self-recursion
                # widen fully to avoid making the cycle any larger
                newsig = method.sig
            else
                comparison = topmost.linfo.specTypes
                if ls > lsig + 1 && ls > length(unwrap_unionall(comparison).parameters)
                    # limit length based on size of definition signature.
                    # for example, given function f(T, Any...), limit to 3 arguments
                    # instead of the default (MAX_TUPLETYPE_LEN)
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
                        newsig = rewrap_unionall(sigtuple, newsig)
                    end
                end
                td = type_depth(newsig)
                max_type_depth = min(MAX_TYPE_DEPTH, type_depth(comparison))
                if td > max_type_depth
                    # limit growth in type depth
                    newsig = limit_type_depth(newsig, max_type_depth)
                end
                # see if the type is still too big, and limit it further if required
                newsig = limit_type_size(newsig, comparison, sv.linfo.specTypes)
            end
            if newsig !== sig
                sig = newsig
                sigtuple = unwrap_unionall(sig)
                sparams = svec()
            end
        end
    end

    # if sig changed, may need to recompute the sparams environment
    if isa(method.sig, UnionAll) && isempty(sparams)
        recomputed = ccall(:jl_type_intersection_with_env, Any, (Any, Any), sig, method.sig)::SimpleVector
        sig = recomputed[1]
        if !isa(unwrap_unionall(sig), DataType) # probably Union{}
            return Any
        end
        sparams = recomputed[2]::SimpleVector
    end
    rt, edge = typeinf_edge(method, sig, sparams, sv)
    edge !== nothing && add_backedge!(edge::MethodInstance, sv)
    return rt
end

# determine whether `ex` abstractly evals to constant `c`
function abstract_evals_to_constant(@nospecialize(ex), @nospecialize(c), vtypes::VarTable, sv::InferenceState)
    av = abstract_eval(ex, vtypes, sv)
    return isa(av,Const) && av.val === c
end

# `typ` is the inferred type for expression `arg`.
# if the expression constructs a container (e.g. `svec(x,y,z)`),
# refine its type to an array of element types.
# Union of Tuples of the same length is converted to Tuple of Unions.
# returns an array of types
function precise_container_type(@nospecialize(arg), @nospecialize(typ), vtypes::VarTable, sv::InferenceState)
    if isa(typ, Const)
        val = typ.val
        if isa(val, SimpleVector) || isa(val, Tuple)
            return Any[ Const(val[i]) for i in 1:length(val) ] # avoid making a tuple Generator here!
        end
    end

    while isa(arg, SSAValue)
        def = sv.ssavalue_defs[arg.id + 1]
        stmt = sv.src.code[def]::Expr
        arg = stmt.args[2]
    end

    if is_specializable_vararg_slot(arg, sv)
        return Any[rewrap_unionall(p, sv.linfo.specTypes) for p in sv.vararg_type_container.parameters]
    end

    tti0 = widenconst(typ)
    tti = unwrap_unionall(tti0)
    if isa(arg, Expr) && arg.head === :call && (abstract_evals_to_constant(arg.args[1], svec, vtypes, sv) ||
                                                abstract_evals_to_constant(arg.args[1], tuple, vtypes, sv))
        aa = arg.args
        result = Any[ (isa(aa[j],Expr) ? aa[j].typ : abstract_eval(aa[j],vtypes,sv)) for j=2:length(aa) ]
        if _any(isvarargtype, result)
            return Any[Vararg{Any}]
        end
        return result
    elseif isa(tti, Union)
        utis = uniontypes(tti)
        if _any(t -> !isa(t,DataType) || !(t <: Tuple) || !isknownlength(t), utis)
            return Any[Vararg{Any}]
        end
        result = Any[rewrap_unionall(p, tti0) for p in utis[1].parameters]
        for t in utis[2:end]
            if length(t.parameters) != length(result)
                return Any[Vararg{Any}]
            end
            for j in 1:length(t.parameters)
                result[j] = tmerge(result[j], rewrap_unionall(t.parameters[j], tti0))
            end
        end
        return result
    elseif isa(tti0,DataType) && tti0 <: Tuple
        if isvatuple(tti0) && length(tti0.parameters) == 1
            return Any[Vararg{unwrapva(tti0.parameters[1])}]
        else
            return Any[ p for p in tti0.parameters ]
        end
    elseif tti0 <: Array
        return Any[Vararg{eltype(tti0)}]
    else
        return Any[abstract_iteration(typ, vtypes, sv)]
    end
end

# simulate iteration protocol on container type up to fixpoint
function abstract_iteration(@nospecialize(itertype), vtypes::VarTable, sv::InferenceState)
    tm = _topmod(sv)
    if !isdefined(tm, :start) || !isdefined(tm, :next) || !isconst(tm, :start) || !isconst(tm, :next)
        return Vararg{Any}
    end
    startf = getfield(tm, :start)
    nextf = getfield(tm, :next)
    statetype = abstract_call(startf, (), Any[Const(startf), itertype], vtypes, sv)
    statetype === Bottom && return Bottom
    valtype = Bottom
    while valtype !== Any
        nt = abstract_call(nextf, (), Any[Const(nextf), itertype, statetype], vtypes, sv)
        nt = widenconst(nt)
        if !isa(nt, DataType) || !(nt <: Tuple) || isvatuple(nt) || length(nt.parameters) != 2
            return Vararg{Any}
        end
        if nt.parameters[1] <: valtype && nt.parameters[2] <: statetype
            break
        end
        valtype = tmerge(valtype, nt.parameters[1])
        statetype = tmerge(statetype, nt.parameters[2])
    end
    return Vararg{valtype}
end

function tuple_tail_elem(@nospecialize(init), ct)
    return Vararg{widenconst(foldl((a, b) -> tmerge(a, unwrapva(b)), init, ct))}
end

# do apply(af, fargs...), where af is a function value
function abstract_apply(@nospecialize(aft), fargs::Vector{Any}, aargtypes::Vector{Any}, vtypes::VarTable, sv::InferenceState)
    if !isa(aft, Const) && !isconstType(aft)
        if !(isleaftype(aft) || aft <: Type) || (aft <: Builtin) || (aft <: IntrinsicFunction)
            return Any
        end
        # non-constant function, but type is known
    end
    res = Union{}
    nargs = length(fargs)
    assert(nargs == length(aargtypes))
    splitunions = 1 < countunionsplit(aargtypes) <= sv.params.MAX_APPLY_UNION_ENUM
    ctypes = Any[Any[aft]]
    for i = 1:nargs
        ctypes´ = []
        for ti in (splitunions ? uniontypes(aargtypes[i]) : Any[aargtypes[i]])
            cti = precise_container_type(fargs[i], ti, vtypes, sv)
            for ct in ctypes
                if !isempty(ct) && isvarargtype(ct[end])
                    tail = tuple_tail_elem(unwrapva(ct[end]), cti)
                    push!(ctypes´, push!(ct[1:(end - 1)], tail))
                else
                    push!(ctypes´, append_any(ct, cti))
                end
            end
        end
        ctypes = ctypes´
    end
    for ct in ctypes
        if length(ct) > sv.params.MAX_TUPLETYPE_LEN
            tail = tuple_tail_elem(Bottom, ct[sv.params.MAX_TUPLETYPE_LEN:end])
            resize!(ct, sv.params.MAX_TUPLETYPE_LEN)
            ct[end] = tail
        end
        if isa(aft, Const)
            rt = abstract_call(aft.val, (), ct, vtypes, sv)
        elseif isconstType(aft)
            rt = abstract_call(aft.parameters[1], (), ct, vtypes, sv)
        else
            astype = argtypes_to_type(ct)
            rt = abstract_call_gf_by_type(nothing, astype, sv)
        end
        res = tmerge(res, rt)
        if res === Any
            break
        end
    end
    return res
end

# TODO: this function is a very buggy and poor model of the return_type function
# since abstract_call_gf_by_type is a very inaccurate model of _method and of typeinf_type,
# while this assumes that it is a precisely accurate and exact model of both
function return_type_tfunc(@nospecialize(argtypes), vtypes::VarTable, sv::InferenceState)
    if length(argtypes) == 3
        tt = argtypes[3]
        if isa(tt, Const) || (isType(tt) && !has_free_typevars(tt))
            aft = argtypes[2]
            if isa(aft, Const) || (isType(aft) && !has_free_typevars(aft)) ||
                   (isleaftype(aft) && !(aft <: Builtin))
                af_argtype = isa(tt, Const) ? tt.val : tt.parameters[1]
                if isa(af_argtype, DataType) && af_argtype <: Tuple
                    argtypes_vec = Any[aft, af_argtype.parameters...]
                    astype = argtypes_to_type(argtypes_vec)
                    if isa(aft, Const)
                        rt = abstract_call(aft.val, (), argtypes_vec, vtypes, sv)
                    elseif isconstType(aft)
                        rt = abstract_call(aft.parameters[1], (), argtypes_vec, vtypes, sv)
                    else
                        rt = abstract_call_gf_by_type(nothing, astype, sv)
                    end
                    if isa(rt, Const)
                        # output was computed to be constant
                        return Const(typeof(rt.val))
                    elseif isleaftype(rt) || rt === Bottom
                        # output type was known for certain
                        return Const(rt)
                    elseif (isa(tt, Const) || isconstType(tt)) &&
                           (isa(aft, Const) || isconstType(aft))
                        # input arguments were known for certain
                        return Const(rt)
                    else
                        return Type{<:rt}
                    end
                end
            end
        end
    end
    return NF
end

function pure_eval_call(@nospecialize(f), @nospecialize(argtypes), @nospecialize(atype), sv::InferenceState)
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
    if isdefined(method, :generator) || !method.pure
        return false
    end

    args = Any[ (a=argtypes[i]; isa(a,Const) ? a.val : a.parameters[1]) for i in 2:length(argtypes) ]
    try
        value = Core._apply_pure(f, args)
        # TODO: add some sort of edge(s)
        return Const(value, true)
    catch
        return false
    end
end

argtypes_to_type(argtypes::Array{Any,1}) = Tuple{anymap(widenconst, argtypes)...}

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

_typename(a) = Union{}
_typename(a::Vararg) = Any
_typename(a::TypeVar) = Any
_typename(a::DataType) = Const(a.name)
function _typename(a::Union)
    ta = _typename(a.a)
    tb = _typename(a.b)
    ta === tb ? tb : (ta === Any || tb === Any) ? Any : Union{}
end
_typename(union::UnionAll) = _typename(union.body)

# N.B.: typename maps type equivalence classes to a single value
typename_static(t::Const) = _typename(t.val)
typename_static(@nospecialize(t)) = isType(t) ? _typename(t.parameters[1]) : Any

function abstract_call(@nospecialize(f), fargs::Union{Tuple{},Vector{Any}}, argtypes::Vector{Any}, vtypes::VarTable, sv::InferenceState)
    if f === _apply
        length(fargs) > 1 || return Any
        return abstract_apply(argtypes[2], fargs[3:end], argtypes[3:end], vtypes, sv)
    end

    la = length(argtypes)
    for i = 2:(la - 1)
        if isvarargtype(argtypes[i])
            return Any
        end
    end

    tm = _topmod(sv)
    if isa(f, Builtin) || isa(f, IntrinsicFunction)
        rt = builtin_tfunction(f, argtypes[2:end], sv)
        if rt === Bool && isa(fargs, Vector{Any})
            # perform very limited back-propagation of type information for `is` and `isa`
            if f === isa
                a = fargs[2]
                if isa(a, fieldtype(Conditional, :var))
                    aty = widenconst(argtypes[2])
                    tty_ub, isexact_tty = instanceof_tfunc(argtypes[3])
                    if isexact_tty && !isa(tty_ub, TypeVar)
                        tty_lb = tty_ub # TODO: this would be wrong if !isexact_tty, but instanceof_tfunc doesn't preserve this info
                        if !has_free_typevars(tty_lb) && !has_free_typevars(tty_ub)
                            ifty = typeintersect(aty, tty_ub)
                            elsety = typesubtract(aty, tty_lb)
                            if ifty != elsety
                                return Conditional(a, ifty, elsety)
                            end
                        end
                    end
                    return Bool
                end
            elseif f === (===)
                a = fargs[2]
                b = fargs[3]
                aty = argtypes[2]
                bty = argtypes[3]
                # if doing a comparison to a singleton, consider returning a `Conditional` instead
                if isa(aty, Const) && isa(b, fieldtype(Conditional, :var))
                    if isdefined(typeof(aty.val), :instance) # can only widen a if it is a singleton
                        return Conditional(b, aty, typesubtract(widenconst(bty), typeof(aty.val)))
                    end
                    return Conditional(b, aty, bty)
                end
                if isa(bty, Const) && isa(a, fieldtype(Conditional, :var))
                    if isdefined(typeof(bty.val), :instance) # same for b
                        return Conditional(a, bty, typesubtract(widenconst(aty), typeof(bty.val)))
                    end
                    return Conditional(a, bty, aty)
                end
            end
        end
        return isa(rt, TypeVar) ? rt.ub : rt
    elseif f === Core.kwfunc
        if length(fargs) == 2
            ft = widenconst(argtypes[2])
            if isa(ft, DataType) && isdefined(ft.name, :mt) && isdefined(ft.name.mt, :kwsorter)
                return Const(ft.name.mt.kwsorter)
            end
        end
        return Any
    elseif f === TypeVar
        lb = Union{}
        ub = Any
        ub_certain = lb_certain = true
        if length(fargs) >= 2 && isa(argtypes[2], Const)
            nv = argtypes[2].val
            ubidx = 3
            if length(fargs) >= 4
                ubidx = 4
                if isa(argtypes[3], Const)
                    lb = argtypes[3].val
                elseif isType(argtypes[3])
                    lb = argtypes[3].parameters[1]
                    lb_certain = false
                else
                    return TypeVar
                end
            end
            if length(fargs) >= ubidx
                if isa(argtypes[ubidx], Const)
                    ub = argtypes[ubidx].val
                elseif isType(argtypes[ubidx])
                    ub = argtypes[ubidx].parameters[1]
                    ub_certain = false
                else
                    return TypeVar
                end
            end
            tv = TypeVar(nv, lb, ub)
            return PartialTypeVar(tv, lb_certain, ub_certain)
        end
        return TypeVar
    elseif f === UnionAll
        if length(fargs) == 3
            canconst = true
            if isa(argtypes[3], Const)
                body = argtypes[3].val
            elseif isType(argtypes[3])
                body = argtypes[3].parameters[1]
                canconst = false
            else
                return Any
            end
            if !isa(body, Type) && !isa(body, TypeVar)
                return Any
            end
            has_free_typevars(body) || return body
            if isa(argtypes[2], Const)
                tv = argtypes[2].val
            elseif isa(argtypes[2], PartialTypeVar)
                ptv = argtypes[2]
                tv = ptv.tv
                canconst = false
            else
                return Any
            end
            !isa(tv, TypeVar) && return Any
            theunion = UnionAll(tv, body)
            ret = canconst ? abstract_eval_constant(theunion) : Type{theunion}
            return ret
        end
        return Any
    elseif f === return_type
        rt_rt = return_type_tfunc(argtypes, vtypes, sv)
        if rt_rt !== NF
            return rt_rt
        end
    elseif length(fargs) == 2 && istopfunction(tm, f, :!)
        aty = argtypes[2]
        if isa(aty, Conditional)
            abstract_call_gf_by_type(f, Tuple{typeof(f), Bool}, sv) # make sure we've inferred `!(::Bool)`
            return Conditional(aty.var, aty.elsetype, aty.vtype)
        end
    elseif length(fargs) == 3 && istopfunction(tm, f, :!==)
        rty = abstract_call((===), fargs, argtypes, vtypes, sv)
        if isa(rty, Conditional)
            return Conditional(rty.var, rty.elsetype, rty.vtype) # swap if-else
        elseif isa(rty, Const)
            return Const(rty.val === false)
        end
        return rty
    elseif length(fargs) == 3 && istopfunction(tm, f, :(>:))
        # swap T1 and T2 arguments and call <:
        fargs = Any[<:, fargs[3], fargs[2]]
        argtypes = Any[typeof(<:), argtypes[3], argtypes[2]]
        rty = abstract_call(<:, fargs, argtypes, vtypes, sv)
        return rty
    end

    if la>2 && argtypes[3] ⊑ Int
        at2 = widenconst(argtypes[2])
        if la==3 && at2 <: SimpleVector && istopfunction(tm, f, :getindex)
            if isa(argtypes[2], Const) && isa(argtypes[3], Const)
                svecval = argtypes[2].val
                idx = argtypes[3].val
                if isa(idx, Int) && 1 <= idx <= length(svecval) &&
                      isassigned(svecval, idx)
                    return Const(getindex(svecval, idx))
                end
            end
        elseif (at2 <: Tuple ||
                (isa(at2, DataType) && (at2::DataType).name === Pair_name()))
            # allow tuple indexing functions to take advantage of constant
            # index arguments.
            if istopfunction(tm, f, :getindex) && la==3
                return getfield_tfunc(argtypes[2], argtypes[3])
            elseif istopfunction(tm, f, :next) && la==3
                t1 = widenconst(getfield_tfunc(argtypes[2], argtypes[3]))
                return t1===Bottom ? Bottom : Tuple{t1, Int}
            elseif istopfunction(tm, f, :indexed_next) && la==4
                t1 = widenconst(getfield_tfunc(argtypes[2], argtypes[3]))
                return t1===Bottom ? Bottom : Tuple{t1, Int}
            end
        end
    elseif la==2 && argtypes[2] ⊑ SimpleVector && istopfunction(tm, f, :length)
        if isa(argtypes[2], Const)
            return Const(length(argtypes[2].val))
        end
    end

    atype = argtypes_to_type(argtypes)
    t = pure_eval_call(f, argtypes, atype, sv)
    t !== false && return t

    if istopfunction(tm, f, :typejoin) || f === return_type
        return Type # don't try to infer these function edges directly -- it won't actually come up with anything useful
    elseif length(argtypes) == 2 && istopfunction(tm, f, :typename)
        return typename_static(argtypes[2])
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

function abstract_eval(@nospecialize(e), vtypes::VarTable, sv::InferenceState)
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
        t = instanceof_tfunc(abstract_eval(e.args[1], vtypes, sv))[1]
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
        if isa(sv.linfo.def, Method)
            spsig = sv.linfo.def.sig
            if isa(spsig, UnionAll)
                if !isempty(sv.linfo.sparam_vals)
                    env = data_pointer_from_objref(sv.linfo.sparam_vals) + sizeof(Ptr{Void})
                    rt = ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), e.args[2], spsig, env)
                else
                    rt = rewrap_unionall(e.args[2], spsig)
                end
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
            for v in sv.linfo.sparam_vals
                if isa(v,TypeVar)
                    t = UnionAll(v, t)
                end
            end
        else
            t = Any
        end
    elseif e.head === :static_parameter
        n = e.args[1]
        t = Any
        if 1 <= n <= length(sv.sp)
            val = sv.sp[n]
            if isa(val, TypeVar)
                if Any <: val.ub
                    # static param bound to typevar
                    # if the tvar is not known to refer to anything more specific than Any,
                    # the static param might actually be an integer, symbol, etc.
                else
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
    elseif e.head === :boundscheck
        return Bool
    elseif e.head === :isdefined
        sym = e.args[1]
        t = Bool
        if isa(sym, Slot)
            vtyp = vtypes[slot_id(sym)]
            if vtyp.typ === Bottom
                t = Const(false) # never assigned previously
            elseif !vtyp.undef
                t = Const(true) # definitely assigned previously
            end
        elseif isa(sym, Symbol)
            if isdefined(sv.mod, sym.name)
                t = Const(true)
            end
        elseif isa(sym, GlobalRef)
            if isdefined(sym.mod, sym.name)
                t = Const(true)
            end
        elseif isa(sym, Expr) && sym.head === :static_parameter
            n = sym.args[1]
            if 1 <= n <= length(sv.sp)
                val = sv.sp[n]
                if !isa(val, TypeVar)
                    t = Const(true)
                end
            end
        end
    else
        t = Any
    end
    if isa(t, TypeVar)
        # no need to use a typevar as the type of an expression
        t = t.ub
    end
    if isa(t, DataType) && isdefined(t, :instance)
        # replace singleton types with their equivalent Const object
        t = Const(t.instance)
    end
    if isa(t, Conditional)
        e.typ = Bool
    else
        e.typ = t
    end
    return t
end

const abstract_eval_constant = Const

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

mutable struct StateUpdate
    var::Union{Slot,SSAValue}
    vtype::VarState
    state::VarTable
end

function abstract_interpret(@nospecialize(e), vtypes::VarTable, sv::InferenceState)
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

function type_too_complex(@nospecialize(t), d::Int)
    if d < 0
        return true
    elseif isa(t, Union)
        return type_too_complex(t.a, d - 1) || type_too_complex(t.b, d - 1)
    elseif isa(t, TypeVar)
        return type_too_complex(t.lb, d - 1) || type_too_complex(t.ub, d - 1)
    elseif isa(t, UnionAll)
        return type_too_complex(t.var, d) || type_too_complex(t.body, d)
    elseif isa(t, DataType)
        for x in (t.parameters)::SimpleVector
            if type_too_complex(x, d - 1)
                return true
            end
        end
    end
    return false
end

## lattice operators

function issubconditional(a::Conditional, b::Conditional)
    avar = a.var
    bvar = b.var
    if (isa(avar, Slot) && isa(bvar, Slot) && slot_id(avar) === slot_id(bvar)) ||
       (isa(avar, SSAValue) && isa(bvar, SSAValue) && avar === bvar)
        if a.vtype ⊑ b.vtype
            if a.elsetype ⊑ b.elsetype
                return true
            end
        end
    end
    return false
end

function ⊑(@nospecialize(a), @nospecialize(b))
    (a === NF || b === Any) && return true
    (a === Any || b === NF) && return false
    a === Union{} && return true
    b === Union{} && return false
    if isa(a, Conditional)
        if isa(b, Conditional)
            return issubconditional(a, b)
        end
        a = Bool
    elseif isa(b, Conditional)
        return a === Bottom
    end
    if isa(a, Const)
        if isa(b, Const)
            return a.val === b.val
        end
        return isa(a.val, widenconst(b))
    elseif isa(b, Const)
        return a === Bottom
    elseif !(isa(a, Type) || isa(a, TypeVar)) ||
           !(isa(b, Type) || isa(b, TypeVar))
        return a === b
    else
        return a <: b
    end
end

widenconst(c::Conditional) = Bool
function widenconst(c::Const)
    if isa(c.val, Type)
        if isvarargtype(c.val)
            return Type
        end
        return Type{c.val}
    else
        return typeof(c.val)
    end
end
widenconst(c::PartialTypeVar) = TypeVar
widenconst(@nospecialize(t)) = t

issubstate(a::VarState, b::VarState) = (a.typ ⊑ b.typ && a.undef <= b.undef)

# Meta expression head, these generally can't be deleted even when they are
# in a dead branch but can be ignored when analyzing uses/liveness.
is_meta_expr_head(head::Symbol) =
    (head === :inbounds || head === :boundscheck || head === :meta ||
     head === :line || head === :simdloop)
is_meta_expr(ex::Expr) = is_meta_expr_head(ex.head)

function tmerge(@nospecialize(typea), @nospecialize(typeb))
    typea ⊑ typeb && return typeb
    typeb ⊑ typea && return typea
    if isa(typea, Conditional) && isa(typeb, Conditional)
        if typea.var === typeb.var
            vtype = tmerge(typea.vtype, typeb.vtype)
            elsetype = tmerge(typea.elsetype, typeb.elsetype)
            if vtype != elsetype
                return Conditional(typea.var, vtype, elsetype)
            end
        end
        return Bool
    end
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
    if unionlen(u) > MAX_TYPEUNION_LEN || type_too_complex(u, MAX_TYPE_DEPTH)
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

@inline tchanged(@nospecialize(n), @nospecialize(o)) = o === NF || (n !== NF && !(n ⊑ o))
@inline schanged(@nospecialize(n), @nospecialize(o)) = (n !== o) && (o === NF || (n !== NF && !issubstate(n, o)))

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

function label_counter(body::Vector{Any})
    l = -1
    for b in body
        if isa(b, LabelNode) && b.label > l
            l = b.label
        end
    end
    return l
end
genlabel(sv) = LabelNode(sv.label_counter += 1)

function get_label_map(body::Vector{Any}, sv::InferenceState)
    labelmap = zeros(Int, sv.label_counter)
    for i = 1:length(body)
        el = body[i]
        if isa(el, LabelNode)
            labelmap[el.label] = i
        end
    end
    return labelmap
end


function find_ssavalue_uses(body::Vector{Any}, nvals::Int)
    uses = IntSet[ IntSet() for i = 1:nvals ]
    for line in 1:length(body)
        e = body[line]
        isa(e, Expr) && find_ssavalue_uses(e, uses, line)
    end
    return uses
end

function find_ssavalue_uses(e::Expr, uses::Vector{IntSet}, line::Int)
    head = e.head
    is_meta_expr_head(head) && return
    skiparg = (head === :(=))
    for a in e.args
        if skiparg
            skiparg = false
        elseif isa(a, SSAValue)
            push!(uses[a.id + 1], line)
        elseif isa(a, Expr)
            find_ssavalue_uses(a, uses, line)
        end
    end
end

function find_ssavalue_defs(body::Vector{Any}, nvals::Int)
    defs = zeros(Int, nvals)
    for line in 1:length(body)
        e = body[line]
        if isa(e, Expr) && e.head === :(=)
            lhs = e.args[1]
            if isa(lhs, SSAValue)
                defs[lhs.id + 1] = line
            end
        end
    end
    return defs
end

function newvar!(sv::InferenceState, @nospecialize(typ))
    id = length(sv.src.ssavaluetypes)
    push!(sv.src.ssavaluetypes, typ)
    return SSAValue(id)
end

inlining_enabled() = (JLOptions().can_inline == 1)
coverage_enabled() = (JLOptions().code_coverage != 0)

# work towards converging the valid age range for sv
function update_valid_age!(min_valid::UInt, max_valid::UInt, sv::InferenceState)
    sv.min_valid = max(sv.min_valid, min_valid)
    sv.max_valid = min(sv.max_valid, max_valid)
    @assert !isa(sv.linfo.def, Method) || !sv.cached || sv.min_valid <= sv.params.world <= sv.max_valid "invalid age range update"
    nothing
end
update_valid_age!(edge::InferenceState, sv::InferenceState) = update_valid_age!(edge.min_valid, edge.max_valid, sv)
update_valid_age!(li::MethodInstance, sv::InferenceState) = update_valid_age!(min_world(li), max_world(li), sv)

# temporarily accumulate our edges to later add as backedges in the callee
function add_backedge!(li::MethodInstance, caller::InferenceState)
    isa(caller.linfo.def, Method) || return # don't add backedges to toplevel exprs
    if caller.stmt_edges[caller.currpc] === ()
        caller.stmt_edges[caller.currpc] = []
    end
    push!(caller.stmt_edges[caller.currpc], li)
    update_valid_age!(li, caller)
    nothing
end

# temporarily accumulate our no method errors to later add as backedges in the callee method table
function add_mt_backedge(mt::MethodTable, @nospecialize(typ), caller::InferenceState)
    isa(caller.linfo.def, Method) || return # don't add backedges to toplevel exprs
    if caller.stmt_edges[caller.currpc] === ()
        caller.stmt_edges[caller.currpc] = []
    end
    push!(caller.stmt_edges[caller.currpc], mt)
    push!(caller.stmt_edges[caller.currpc], typ)
    nothing
end

# add the real backedges now
function finalize_backedges(frame::InferenceState)
    toplevel = !isa(frame.linfo.def, Method)
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

function code_for_method(method::Method, @nospecialize(atypes), sparams::SimpleVector, world::UInt, preexisting::Bool=false)
    if world < min_world(method)
        return nothing
    end
    if isdefined(method, :generator) && !isleaftype(atypes)
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

function typeinf_active(linfo::MethodInstance, sv::InferenceState)
    for infstate in sv.callers_in_cycle
        linfo === infstate.linfo && return infstate
    end
    return nothing
end

function add_backedge!(frame::InferenceState, caller::InferenceState, currpc::Int)
    update_valid_age!(frame, caller)
    backedge = (caller, currpc)
    contains_is(frame.backedges, backedge) || push!(frame.backedges, backedge)
    return frame
end

# at the end, all items in b's cycle
# will now be added to a's cycle
function union_caller_cycle!(a::InferenceState, b::InferenceState)
    callers_in_cycle = b.callers_in_cycle
    b.parent = a.parent
    b.callers_in_cycle = a.callers_in_cycle
    contains_is(a.callers_in_cycle, b) || push!(a.callers_in_cycle, b)
    if callers_in_cycle !== a.callers_in_cycle
        for caller in callers_in_cycle
            if caller !== b
                caller.parent = a.parent
                caller.callers_in_cycle = a.callers_in_cycle
                push!(a.callers_in_cycle, caller)
            end
        end
    end
    return
end

function merge_call_chain!(parent::InferenceState, ancestor::InferenceState, child::InferenceState)
    # add backedge of parent <- child
    # then add all backedges of parent <- parent.parent
    # and merge all of the callers into ancestor.callers_in_cycle
    # and ensure that walking the parent list will get the same result (DAG) from everywhere
    while true
        add_backedge!(child, parent, parent.currpc)
        union_caller_cycle!(ancestor, child)
        child = parent
        parent = child.parent
        child === ancestor && break
    end
end

# Walk through `linfo`'s upstream call chain, starting at `parent`. If a parent
# frame matching `linfo` is encountered, then there is a cycle in the call graph
# (i.e. `linfo` is a descendant callee of itself). Upon encountering this cycle,
# we "resolve" it by merging the call chain, which entails unioning each intermediary
# frame's `callers_in_cycle` field and adding the appropriate backedges. Finally,
# we return `linfo`'s pre-existing frame. If no cycles are found, `nothing` is
# returned instead.
function resolve_call_cycle!(linfo::MethodInstance, parent::InferenceState)
    frame = parent
    while isa(frame, InferenceState)
        if frame.linfo === linfo
            merge_call_chain!(parent, frame, frame)
            return frame
        end
        for caller in frame.callers_in_cycle
            if caller.linfo === linfo
                merge_call_chain!(parent, frame, caller)
                return caller
            end
        end
        frame = frame.parent
    end
    return nothing
end

# build (and start inferring) the inference frame for the linfo
function typeinf_frame(linfo::MethodInstance,
                       optimize::Bool, cached::Bool, params::InferenceParams)
    frame = InferenceState(linfo, optimize, cached, params)
    frame === nothing && return nothing
    cached && (linfo.inInference = true)
    typeinf(frame)
    return frame
end

# compute (and cache) an inferred AST and return the current best estimate of the result type
function typeinf_edge(method::Method, @nospecialize(atypes), sparams::SimpleVector, caller::InferenceState)
    code = code_for_method(method, atypes, sparams, caller.params.world)
    code === nothing && return Any, nothing
    code = code::MethodInstance
    if isdefined(code, :inferred)
        # return rettype if the code is already inferred
        # staged functions make this hard since they have two "inferred" conditions,
        # so need to check whether the code itself is also inferred
        inf = code.inferred
        if !isa(inf, CodeInfo) || (inf::CodeInfo).inferred
            if isdefined(code, :inferred_const)
                return abstract_eval_constant(code.inferred_const), code
            else
                return code.rettype, code
            end
        end
    end
    frame = resolve_call_cycle!(code, caller)
    if frame === nothing
        code.inInference = true
        frame = InferenceState(code, #=optimize=#true, #=cached=#true, caller.params) # always optimize and cache edge targets
        if frame === nothing
            code.inInference = false
            return Any, nothing
        end
        if caller.cached # don't involve uncached functions in cycle resolution
            frame.parent = caller
        end
        typeinf(frame)
        return frame.bestguess, frame.inferred ? frame.linfo : nothing
    end
    frame = frame::InferenceState
    return frame.bestguess, nothing
end


#### entry points for inferring a MethodInstance given a type signature ####

# compute an inferred AST and return type
function typeinf_code(method::Method, @nospecialize(atypes), sparams::SimpleVector,
                      optimize::Bool, cached::Bool, params::InferenceParams)
    code = code_for_method(method, atypes, sparams, params.world)
    code === nothing && return (nothing, nothing, Any)
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
            if min_world(linfo) <= params.world <= max_world(linfo)
                inf = linfo.inferred
                if linfo.jlcall_api == 2
                    method = linfo.def::Method
                    tree = ccall(:jl_new_code_info_uninit, Ref{CodeInfo}, ())
                    tree.code = Any[ Expr(:return, QuoteNode(linfo.inferred_const)) ]
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
                    if inf.inferred
                        i == 2 && ccall(:jl_typeinf_end, Void, ())
                        return svec(linfo, inf, linfo.rettype)
                    end
                end
            end
        end
    end
    frame = typeinf_frame(linfo, optimize, cached, params)
    ccall(:jl_typeinf_end, Void, ())
    frame === nothing && return svec(nothing, nothing, Any)
    frame = frame::InferenceState
    frame.inferred || return svec(nothing, nothing, Any)
    frame.cached || return svec(nothing, frame.src, widenconst(frame.bestguess))
    return svec(frame.linfo, frame.src, widenconst(frame.bestguess))
end

# compute (and cache) an inferred AST and return the inferred return type
function typeinf_type(method::Method, @nospecialize(atypes), sparams::SimpleVector,
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
    frame = typeinf_frame(code, cached, cached, params)
    ccall(:jl_typeinf_end, Void, ())
    frame === nothing && return nothing
    frame = frame::InferenceState
    frame.inferred || return nothing
    return widenconst(frame.bestguess)
end

function typeinf_ext(linfo::MethodInstance, world::UInt)
    if isa(linfo.def, Method)
        # method lambda - infer this specialization via the method cache
        return typeinf_code(linfo, true, true, InferenceParams(world))
    else
        # toplevel lambda - infer directly
        ccall(:jl_typeinf_begin, Void, ())
        frame = InferenceState(linfo, linfo.inferred::CodeInfo,
                               true, true, InferenceParams(world))
        typeinf(frame)
        ccall(:jl_typeinf_end, Void, ())
        @assert frame.inferred # TODO: deal with this better
        @assert frame.linfo === linfo
        return svec(linfo, frame.src, linfo.rettype)
    end
end

#### do the work of inference ####

function typeinf_work(frame::InferenceState)
    @assert !frame.inferred
    frame.dont_work_on_me = true # mark that this function is currently on the stack
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
                min_pc = next(W, pc)[2]
                if done(W, min_pc)
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
            changes = abstract_interpret(stmt, s[pc]::VarTable, frame)
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
            if isa(changes, StateUpdate)
                changes_var = changes.var
                if isa(changes_var, SSAValue)
                    # directly forward changes to an SSAValue to the applicable line
                    record_ssa_assign(changes_var.id + 1, changes.vtype.typ, frame)
                end
            elseif isa(stmt, NewvarNode)
                sn = slot_id(stmt.slot)
                changes = changes::VarTable
                changes[sn] = VarState(Bottom, true)
            elseif isa(stmt, GotoNode)
                pc´ = (stmt::GotoNode).label
            elseif isa(stmt, Expr)
                stmt = stmt::Expr
                hd = stmt.head
                if hd === :gotoifnot
                    condt = abstract_eval(stmt.args[1], s[pc], frame)
                    condval = isa(condt, Const) ? condt.val : nothing
                    l = stmt.args[2]::Int
                    changes = changes::VarTable
                    # constant conditions
                    if condval === true
                    elseif condval === false
                        pc´ = l
                    else
                        # general case
                        frame.handler_at[l] = frame.cur_hand
                        if isa(condt, Conditional)
                            changes_else = StateUpdate(condt.var, VarState(condt.elsetype, false), changes)
                            changes = StateUpdate(condt.var, VarState(condt.vtype, false), changes)
                        else
                            changes_else = changes
                        end
                        newstate_else = stupdate!(s[l], changes_else)
                        if newstate_else !== false
                            # add else branch to active IP list
                            if l < frame.pc´´
                                frame.pc´´ = l
                            end
                            push!(W, l)
                            s[l] = newstate_else
                        end
                    end
                elseif hd === :return
                    pc´ = n + 1
                    rt = abstract_eval(stmt.args[1], s[pc], frame)
                    if tchanged(rt, frame.bestguess)
                        # new (wider) return type for frame
                        frame.bestguess = tmerge(frame.bestguess, rt)
                        for (caller, caller_pc) in frame.backedges
                            # notify backedges of updated type information
                            if caller.stmt_types[caller_pc] !== ()
                                if caller_pc < caller.pc´´
                                    caller.pc´´ = caller_pc
                                end
                                push!(caller.ip, caller_pc)
                            end
                        end
                    end
                elseif hd === :enter
                    l = stmt.args[1]::Int
                    frame.cur_hand = (l, frame.cur_hand)
                    # propagate type info to exception handler
                    l = frame.cur_hand[1]
                    old = s[l]
                    new = s[pc]::Array{Any,1}
                    newstate_catch = stupdate!(old, new)
                    if newstate_catch !== false
                        if l < frame.pc´´
                            frame.pc´´ = l
                        end
                        push!(W, l)
                        s[l] = newstate_catch
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
    frame.dont_work_on_me = false
end

function typeinf(frame::InferenceState)

    typeinf_work(frame)

    # If the current frame is part of a cycle, solve the cycle before finishing
    no_active_ips_in_callers = false
    while !no_active_ips_in_callers
        no_active_ips_in_callers = true
        for caller in frame.callers_in_cycle
            caller.dont_work_on_me && return
            if caller.pc´´ <= caller.nstmts # equivalent to `isempty(caller.ip)`
                # Note that `typeinf_work(caller)` can potentially modify the other frames
                # `frame.callers_in_cycle`, which is why making incremental progress requires the
                # outer while loop.
                typeinf_work(caller)
                no_active_ips_in_callers = false
            end
            if caller.min_valid < frame.min_valid
                caller.min_valid = frame.min_valid
            end
            if caller.max_valid > frame.max_valid
                caller.max_valid = frame.max_valid
            end
        end
    end

    # with no active ip's, type inference on frame is done

    if isempty(frame.callers_in_cycle)
        @assert !(frame.dont_work_on_me)
        frame.dont_work_on_me = true
        optimize(frame)
        finish(frame)
        finalize_backedges(frame)
    else # frame is in frame.callers_in_cycle
        for caller in frame.callers_in_cycle
            @assert !(caller.dont_work_on_me)
            caller.dont_work_on_me = true
        end
        for caller in frame.callers_in_cycle
            optimize(caller)
            if frame.min_valid < caller.min_valid
                frame.min_valid = caller.min_valid
            end
            if frame.max_valid > caller.max_valid
                frame.max_valid = caller.max_valid
            end
        end
        for caller in frame.callers_in_cycle
            caller.min_valid = frame.min_valid
        end
        for caller in frame.callers_in_cycle
            finish(caller)
        end
        for caller in frame.callers_in_cycle
            finalize_backedges(caller)
        end
    end

    nothing
end


function record_ssa_assign(ssa_id::Int, @nospecialize(new), frame::InferenceState)
    old = frame.src.ssavaluetypes[ssa_id]
    if old === NF || !(new ⊑ old)
        frame.src.ssavaluetypes[ssa_id] = tmerge(old, new)
        W = frame.ip
        s = frame.stmt_types
        for r in frame.ssavalue_uses[ssa_id]
            if s[r] !== () # s[r] === () => unreached statement
                if r < frame.pc´´
                    frame.pc´´ = r
                end
                push!(W, r)
            end
        end
    end
    nothing
end


#### finalize and record the result of running type inference ####

function isinlineable(m::Method, src::CodeInfo, mod::Module, params::InferenceParams, bonus::Int=0)
    # compute the cost (size) of inlining this code
    inlineable = false
    cost_threshold = params.inline_cost_threshold
    if m.module === _topmod(m.module)
        # a few functions get special treatment
        name = m.name
        sig = m.sig
        if ((name === :+ || name === :* || name === :min || name === :max) &&
            isa(sig,DataType) &&
            sig == Tuple{sig.parameters[1],Any,Any,Any,Vararg{Any}})
            inlineable = true
        elseif (name === :next || name === :done || name === :unsafe_convert ||
                name === :cconvert)
            cost_threshold *= 4
        end
    end
    if !inlineable
        inlineable = inline_worthy(src.code, src, mod, params, cost_threshold + bonus)
    end
    return inlineable
end

# inference completed on `me`
# now converge the optimization work
function optimize(me::InferenceState)
    # annotate fulltree with type information
    type_annotate!(me)

    # run optimization passes on fulltree
    force_noinline = true
    if me.optimize
        # This pass is required for the AST to be valid in codegen
        # if any `SSAValue` is created by type inference. Ref issue #6068
        # This (and `reindex_labels!`) needs to be run for `!me.optimize`
        # if we start to create `SSAValue` in type inference when not
        # optimizing and use unoptimized IR in codegen.
        gotoifnot_elim_pass!(me)
        inlining_pass!(me, me.src.propagate_inbounds)
        # Clean up after inlining
        gotoifnot_elim_pass!(me)
        basic_dce_pass!(me)
        void_use_elim_pass!(me)
        # Compute escape information
        # and elide unnecessary allocations
        alloc_elim_pass!(me)
        getfield_elim_pass!(me)
        # Clean up for `alloc_elim_pass!` and `getfield_elim_pass!`
        void_use_elim_pass!(me)
        # Pop metadata before label reindexing
        let code = me.src.code::Array{Any,1}
            meta_elim_pass!(code, coverage_enabled())
            filter!(x -> x !== nothing, code)
            force_noinline = popmeta!(code, :noinline)[1]
        end
        reindex_labels!(me)
    end

    # convert all type information into the form consumed by the code-generator
    widen_all_consts!(me.src)

    if isa(me.bestguess, Const) || isconstType(me.bestguess)
        me.const_ret = true
        proven_pure = false
        # must be proven pure to use const_api; otherwise we might skip throwing errors
        # (issue #20704)
        # TODO: Improve this analysis; if a function is marked @pure we should really
        # only care about certain errors (e.g. method errors and type errors).
        if length(me.src.code) < 10
            proven_pure = true
            for stmt in me.src.code
                if !statement_effect_free(stmt, me.src, me.mod)
                    proven_pure = false
                    break
                end
            end
            if proven_pure
                for fl in me.src.slotflags
                    if (fl & Slot_UsedUndef) != 0
                        proven_pure = false
                        break
                    end
                end
            end
        end
        if proven_pure
            me.src.pure = true
        end

        if proven_pure && !coverage_enabled()
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
    if !force_noinline
        # don't keep ASTs for functions specialized on a Union argument
        # TODO: this helps avoid a type-system bug mis-computing sparams during intersection
        sig = unwrap_unionall(me.linfo.specTypes)
        if isa(sig, DataType) && sig.name === Tuple.name
            for P in sig.parameters
                P = unwrap_unionall(P)
                if isa(P, Union)
                    force_noinline = true
                    break
                end
            end
        else
            force_noinline = true
        end
    end
    def = me.linfo.def
    if force_noinline
        me.src.inlineable = false
    elseif !me.src.inlineable && isa(def, Method)
        bonus = 0
        if me.bestguess ⊑ Tuple && !isbits(widenconst(me.bestguess))
            bonus = me.params.inline_tupleret_bonus
        end
        me.src.inlineable = isinlineable(def, me.src, me.mod, me.params, bonus)
    end
    me.src.inferred = true
    nothing
end

# inference completed on `me`
# update the MethodInstance and notify the edges
function finish(me::InferenceState)
    me.currpc = 1 # used by add_backedge
    if me.cached
        toplevel = !isa(me.linfo.def, Method)
        if !toplevel
            min_valid = me.min_valid
            max_valid = me.max_valid
        else
            min_valid = UInt(0)
            max_valid = UInt(0)
        end

        # check if the existing me.linfo metadata is also sufficient to describe the current inference result
        # to decide if it is worth caching it again (which would also clear any generated code)
        already_inferred = !me.linfo.inInference
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
                inferred_result = nothing
            else
                inferred_result = me.src
            end

            if !toplevel
                if !me.const_api
                    def = me.linfo.def::Method
                    keeptree = me.src.inlineable || ccall(:jl_is_cacheable_sig, Int32, (Any, Any, Any),
                        me.linfo.specTypes, def.sig, def) != 0
                    if !keeptree
                        inferred_result = nothing
                    else
                        # compress code for non-toplevel thunks
                        inferred_result = ccall(:jl_compress_ast, Any, (Any, Any), def, inferred_result)
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

    # update all of the callers with real backedges by traversing the temporary list of backedges
    for (i, _) in me.backedges
        add_backedge!(me.linfo, i)
    end

    # finalize and record the linfo result
    me.cached && (me.linfo.inInference = false)
    me.inferred = true
    nothing
end

function annotate_slot_load!(e::Expr, vtypes::VarTable, sv::InferenceState, undefs::Array{Bool,1})
    head = e.head
    i0 = 1
    if is_meta_expr_head(head) || head === :const
        return
    end
    if head === :(=) || head === :method
        i0 = 2
    end
    for i = i0:length(e.args)
        subex = e.args[i]
        if isa(subex, Expr)
            annotate_slot_load!(subex, vtypes, sv, undefs)
        elseif isa(subex, Slot)
            id = slot_id(subex)
            s = vtypes[id]
            vt = s.typ
            if s.undef
                # find used-undef variables
                undefs[id] = true
            end
            #  add type annotations where needed
            if !(sv.src.slottypes[id] ⊑ vt)
                e.args[i] = TypedSlot(id, vt)
            end
        end
    end
end

function record_slot_assign!(sv::InferenceState)
    # look at all assignments to slots
    # and union the set of types stored there
    # to compute a lower bound on the storage required
    states = sv.stmt_types
    body = sv.src.code::Vector{Any}
    slottypes = sv.src.slottypes::Vector{Any}
    for i = 1:length(body)
        expr = body[i]
        st_i = states[i]
        # find all reachable assignments to locals
        if isa(st_i, VarTable) && isa(expr, Expr) && expr.head === :(=)
            lhs = expr.args[1]
            rhs = expr.args[2]
            if isa(lhs, Slot)
                id = slot_id(lhs)
                if isa(rhs, Slot)
                    # exprtype isn't yet computed for slots
                    vt = st_i[slot_id(rhs)].typ
                else
                    vt = exprtype(rhs, sv.src, sv.mod)
                end
                vt = widenconst(vt)
                if vt !== Bottom
                    otherTy = slottypes[id]
                    if otherTy === Bottom
                        slottypes[id] = vt
                    elseif otherTy === Any
                        slottypes[id] = Any
                    else
                        slottypes[id] = tmerge(otherTy, vt)
                    end
                end
            end
        end
    end
end

# annotate types of all symbols in AST
function type_annotate!(sv::InferenceState)
    # remove all unused ssa values
    gt = sv.src.ssavaluetypes
    for j = 1:length(gt)
        if gt[j] === NF
            gt[j] = Union{}
        end
    end

    # compute the required type for each slot
    # to hold all of the items assigned into it
    record_slot_assign!(sv)

    # annotate variables load types
    # remove dead code
    # and compute which variables may be used undef
    src = sv.src
    states = sv.stmt_types
    nargs = sv.nargs
    nslots = length(states[1])
    undefs = fill(false, nslots)
    body = src.code::Array{Any,1}
    nexpr = length(body)
    i = 1
    while i <= nexpr
        st_i = states[i]
        expr = body[i]
        if isa(st_i, VarTable)
            # st_i === ()  =>  unreached statement  (see issue #7836)
            if isa(expr, Expr)
                annotate_slot_load!(expr, st_i, sv, undefs)
            elseif isa(expr, Slot)
                id = slot_id(expr)
                if st_i[slot_id(expr)].undef
                    # find used-undef variables in statement position
                    undefs[id] = true
                end
            end
        elseif sv.optimize
            if ((isa(expr, Expr) && is_meta_expr(expr)) ||
                 isa(expr, LineNumberNode))
                # keep any lexically scoped expressions
                i += 1
                continue
            end
            # This can create `Expr(:gotoifnot)` with dangling label, which we
            # will clean up in `reindex_labels!`
            deleteat!(body, i)
            deleteat!(states, i)
            nexpr -= 1
            continue
        end
        i += 1
    end

    # finish marking used-undef variables
    for j = 1:nslots
        if undefs[j]
            src.slotflags[j] |= Slot_UsedUndef
        end
    end
    nothing
end

# widen all Const elements in type annotations
function _widen_all_consts!(e::Expr, untypedload::Vector{Bool}, slottypes::Vector{Any})
    e.typ = widenconst(e.typ)
    for i = 1:length(e.args)
        x = e.args[i]
        if isa(x, Expr)
            _widen_all_consts!(x, untypedload, slottypes)
        elseif isa(x, TypedSlot)
            vt = widenconst(x.typ)
            if !(vt === x.typ)
                if slottypes[x.id] <: vt
                    x = SlotNumber(x.id)
                    untypedload[x.id] = true
                else
                    x = TypedSlot(x.id, vt)
                end
                e.args[i] = x
            end
        elseif isa(x, SlotNumber) && (i != 1 || e.head !== :(=))
            untypedload[x.id] = true
        end
    end
    nothing
end

function widen_all_consts!(src::CodeInfo)
    for i = 1:length(src.ssavaluetypes)
        src.ssavaluetypes[i] = widenconst(src.ssavaluetypes[i])
    end
    nslots = length(src.slottypes)
    untypedload = fill(false, nslots)
    e = Expr(:body)
    e.args = src.code
    _widen_all_consts!(e, untypedload, src.slottypes)
    for i = 1:nslots
        src.slottypes[i] = widen_slot_type(src.slottypes[i], untypedload[i])
    end
    return src
end

# widen all slots to their optimal storage layout
# we also need to preserve the type for any untyped load of a DataType
# since codegen optimizations of functions like `is` will depend on knowing it
function widen_slot_type(@nospecialize(ty), untypedload::Bool)
    ty = widenconst(ty)
    if isa(ty, DataType)
        if untypedload || isbits(ty) || isdefined(ty, :instance)
            return ty
        end
    elseif isa(ty, Union)
        ty_a = widen_slot_type(ty.a, false)
        ty_b = widen_slot_type(ty.b, false)
        if ty_a !== Any || ty_b !== Any
            # TODO: better optimized codegen for unions?
            return ty
        end
    elseif isa(ty, UnionAll)
        if untypedload
            return ty
        end
    end
    return Any
end

# replace slots 1:na with argexprs, static params with spvals, and increment
# other slots by offset.
function substitute!(
        @nospecialize(e), na::Int, argexprs::Vector{Any},
        @nospecialize(spsig), spvals::Vector{Any},
        offset::Int, boundscheck::Symbol)
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
        return NewvarNode(substitute!(e.slot, na, argexprs, spsig, spvals, offset, boundscheck))
    end
    if isa(e, Expr)
        e = e::Expr
        head = e.head
        if head === :static_parameter
            sp = spvals[e.args[1]]
            is_self_quoting(sp) && return sp
            return QuoteNode(sp)
        elseif head === :foreigncall
            @assert !isa(spsig, UnionAll) || !isempty(spvals)
            for i = 1:length(e.args)
                if i == 2
                    e.args[2] = ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), e.args[2], spsig, spvals)
                elseif i == 3
                    argtuple = Any[
                        ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), argt, spsig, spvals)
                        for argt
                        in e.args[3] ]
                    e.args[3] = svec(argtuple...)
                elseif i == 4
                    @assert isa((e.args[4]::QuoteNode).value, Symbol)
                elseif i == 5
                    @assert isa(e.args[5], Int)
                else
                    e.args[i] = substitute!(e.args[i], na, argexprs, spsig, spvals, offset, boundscheck)
                end
            end
        elseif head === :boundscheck
            if boundscheck === :propagate
                return e
            elseif boundscheck === :off
                return false
            else
                return true
            end
        elseif !is_meta_expr_head(head)
            for i = 1:length(e.args)
                e.args[i] = substitute!(e.args[i], na, argexprs, spsig, spvals, offset, boundscheck)
            end
        end
    end
    return e
end

# count occurrences up to n+1
function occurs_more(@nospecialize(e), pred, n)
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

function exprtype(@nospecialize(x), src::CodeInfo, mod::Module)
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
const _pure_builtins = Any[tuple, svec, fieldtype, apply_type, ===, isa, typeof, UnionAll, nfields]

# known effect-free calls (might not be affect-free)
const _pure_builtins_volatile = Any[getfield, arrayref, isdefined, Core.sizeof]

function is_pure_intrinsic(f::IntrinsicFunction)
    return !(f === Intrinsics.pointerref || # this one is volatile
             f === Intrinsics.pointerset || # this one is never effect-free
             f === Intrinsics.llvmcall ||   # this one is never effect-free
             f === Intrinsics.checked_sdiv_int ||
             f === Intrinsics.checked_udiv_int ||
             f === Intrinsics.checked_srem_int ||
             f === Intrinsics.checked_urem_int ||
             f === Intrinsics.sqrt_llvm ||
             f === Intrinsics.cglobal)  # cglobal throws an error for symbol-not-found
end

function is_pure_builtin(@nospecialize(f))
    return (contains_is(_pure_builtins, f) ||
            contains_is(_pure_builtins_volatile, f) ||
            (isa(f,IntrinsicFunction) && is_pure_intrinsic(f)) ||
            f === return_type)
end

function statement_effect_free(@nospecialize(e), src::CodeInfo, mod::Module)
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
function effect_free(@nospecialize(e), src::CodeInfo, mod::Module, allow_volatile::Bool)
    if isa(e, GlobalRef)
        return (isdefined(e.mod, e.name) && (allow_volatile || isconst(e.mod, e.name)))
    elseif isa(e, Symbol)
        return allow_volatile
    elseif isa(e, Slot)
        return src.slotflags[slot_id(e)] & Slot_UsedUndef == 0
    elseif isa(e, Expr)
        e = e::Expr
        head = e.head
        if is_meta_expr_head(head)
            return true
        end
        if head === :static_parameter
            # if we aren't certain about the type, it might be an UndefVarError at runtime
            return isa(e.typ, DataType) && isleaftype(e.typ)
        end
        if e.typ === Bottom
            return false
        end
        ea = e.args
        if head === :call
            if is_known_call_p(e, is_pure_builtin, src, mod)
                if !allow_volatile
                    if is_known_call(e, arrayref, src, mod) || is_known_call(e, arraylen, src, mod)
                        return false
                    elseif is_known_call(e, getfield, src, mod)
                        nargs = length(ea)
                        (nargs == 3 || nargs == 4) || return false
                        et = exprtype(e, src, mod)
                        if !isa(et, Const) && !(isType(et) && isleaftype(et))
                            # first argument must be immutable to ensure e is affect_free
                            a = ea[2]
                            typ = widenconst(exprtype(a, src, mod))
                            if isconstType(typ)
                                if Const(:uid) ⊑ exprtype(ea[3], src, mod)
                                    return false    # DataType uid field can change
                                end
                            elseif typ !== SimpleVector && (!isa(typ, DataType) || typ.mutable || typ.abstract)
                                return false
                            end
                        end
                    end
                end
                # fall-through
            elseif is_known_call(e, _apply, src, mod) && length(ea) > 1
                ft = exprtype(ea[2], src, mod)
                if !isa(ft, Const) || (!contains_is(_pure_builtins, ft.val) &&
                                       ft.val !== Core.sizeof)
                    return false
                end
                # fall-through
            else
                return false
            end
        elseif head === :new
            a = ea[1]
            typ = exprtype(a, src, mod)
            # `Expr(:new)` of unknown type could raise arbitrary TypeError.
            typ, isexact = instanceof_tfunc(typ)
            isexact || return false
            (isleaftype(typ) && !iskindtype(typ)) || return false
            typ = typ::DataType
            if !allow_volatile && typ.mutable
                return false
            end
            fieldcount(typ) >= length(ea) - 1 || return false
            for fld_idx in 1:(length(ea) - 1)
                exprtype(ea[fld_idx + 1], src, mod) ⊑ fieldtype(typ, fld_idx) || return false
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

struct InvokeData
    mt::MethodTable
    entry::TypeMapEntry
    types0
    fexpr
    texpr
end

function inline_as_constant(@nospecialize(val), argexprs, sv::InferenceState, @nospecialize(invoke_data))
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
    if !is_self_quoting(val)
        val = QuoteNode(val)
    end
    return (val, stmts)
end

function is_self_quoting(@nospecialize(x))
    return isa(x,Number) || isa(x,AbstractString) || isa(x,Tuple) || isa(x,Type)
end

function countunionsplit(atypes)
    nu = 1
    for ti in atypes
        if isa(ti, Union)
            nu *= unionlen(ti::Union)
        end
    end
    return nu
end

function get_spec_lambda(@nospecialize(atypes), sv, @nospecialize(invoke_data))
    if invoke_data === nothing
        return ccall(:jl_get_spec_lambda, Any, (Any, UInt), atypes, sv.params.world)
    else
        invoke_data = invoke_data::InvokeData
        atypes <: invoke_data.types0 || return nothing
        return ccall(:jl_get_invoke_lambda, Any, (Any, Any, Any, UInt),
                     invoke_data.mt, invoke_data.entry, atypes, sv.params.world)
    end
end

function linearize_args!(args::Vector{Any}, atypes::Vector{Any}, stmts::Vector{Any}, sv::InferenceState)
    # linearize the IR by moving the arguments to SSA position
    na = length(args)
    @assert length(atypes) == na
    newargs = Vector{Any}(na)
    for i = na:-1:1
        aei = args[i]
        ti = atypes[i]
        if isa(aei, Expr) || isa(aei, GlobalRef)
            newvar = newvar!(sv, ti)
            unshift!(stmts, Expr(:(=), newvar, aei))
        else
            newvar = aei
        end
        newargs[i] = newvar
    end
    return newargs
end

function invoke_NF(argexprs, @nospecialize(etype), atypes::Vector{Any}, sv::InferenceState,
                   @nospecialize(atype_unlimited), @nospecialize(invoke_data))
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
        # linearize the IR by moving the arguments to SSA position
        stmts = []

        spec_miss = nothing
        error_label = nothing
        ex = Expr(:call)
        ex.typ = etype
        ex.args = linearize_args!(argexprs, atypes, stmts, sv)
        invoke_texpr === nothing || insert!(stmts, 2, invoke_texpr)
        invoke_fexpr === nothing || unshift!(stmts, invoke_fexpr)

        local ret_var, merge, invoke_ex, spec_hit
        ret_var = add_slot!(sv.src, widenconst(etype), false)
        merge = genlabel(sv)
        invoke_ex = copy(ex)
        invoke_ex.head = :invoke
        unshift!(invoke_ex.args, nothing)
        spec_hit = false

        function splitunion(atypes::Vector{Any}, i::Int)
            if i == 0
                local sig = argtypes_to_type(atypes)
                local li = get_spec_lambda(sig, sv, invoke_data)
                li === nothing && return false
                add_backedge!(li, sv)
                local stmt = []
                invoke_ex = copy(invoke_ex)
                invoke_ex.args[1] = li
                push!(stmt, Expr(:(=), ret_var, invoke_ex))
                push!(stmt, GotoNode(merge.label))
                spec_hit = true
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
                            isa_ty = Expr(:call, GlobalRef(Core, :isa), aei, ty)
                            isa_ty.typ = Bool
                            unshift!(match, Expr(:gotoifnot, isa_ty, after.label))
                            append!(stmts, match)
                            push!(stmts, after)
                        else
                            all = false
                        end
                    end
                    if all
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
        if match !== false && spec_hit
            append!(stmts, match)
            if error_label !== nothing
                push!(stmts, error_label)
                push!(stmts, Expr(:call, GlobalRef(_topmod(sv.mod), :error), "fatal error in type inference (type bound)"))
            end
            if spec_miss !== nothing
                push!(stmts, spec_miss)
                push!(stmts, Expr(:(=), ret_var, ex))
                push!(stmts, GotoNode(merge.label))
            end
            push!(stmts, merge)
            return (ret_var, stmts)
        end
    else
        local cache_linfo = get_spec_lambda(atype_unlimited, sv, invoke_data)
        cache_linfo === nothing && return NF
        add_backedge!(cache_linfo, sv)
        argexprs = copy(argexprs)
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
        stmts = Any[invoke_fexpr,
                    :($newvar = $(argexprs[2])),
                    invoke_texpr]
        argexprs[2] = newvar
        return ex, stmts
    end
    return NF
end

# inline functions whose bodies are "inline_worthy"
# where the function body doesn't contain any argument more than once.
# static parameters are ok if all the static parameter values are leaf types,
# meaning they are fully known.
# `ft` is the type of the function. `f` is the exact function if known, or else `nothing`.
# `pending_stmts` is an array of statements from functions inlined so far, so
# we can estimate the total size of the enclosing function after inlining.
function inlineable(@nospecialize(f), @nospecialize(ft), e::Expr, atypes::Vector{Any},
                    pending_stmt::Vector{Any}, boundscheck::Symbol,
                    sv::InferenceState)
    argexprs = e.args

    if (f === typeassert || ft ⊑ typeof(typeassert)) && length(atypes)==3
        # typeassert(x::S, T) => x, when S<:T
        a3 = atypes[3]
        if (isType(a3) && isleaftype(a3) && atypes[2] ⊑ a3.parameters[1]) ||
            (isa(a3,Const) && isa(a3.val,Type) && atypes[2] ⊑ a3.val)
            return (argexprs[2], ())
        end
    end
    topmod = _topmod(sv)
    # special-case inliners for known pure functions that compute types
    if sv.params.inlining
        if isa(e.typ, Const) # || isconstType(e.typ)
            val = e.typ.val
            if (f === apply_type || f === fieldtype || f === typeof || f === (===) ||
                f === Core.sizeof || f === isdefined ||
                istopfunction(topmod, f, :typejoin) ||
                istopfunction(topmod, f, :isbits) ||
                istopfunction(topmod, f, :promote_type) ||
                (f === Core.kwfunc && length(argexprs) == 2) ||
                (isbits(val) && Core.sizeof(val) <= MAX_INLINE_CONST_SIZE &&
                 (contains_is(_pure_builtins, f) ||
                  (f === getfield && effect_free(e, sv.src, sv.mod, false)) ||
                  (isa(f,IntrinsicFunction) && is_pure_intrinsic(f)))))
                return inline_as_constant(val, argexprs, sv, nothing)
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

    if length(atypes) == 3 && istopfunction(topmod, f, :!==)
        # special-case inliner for !== that precedes _methods_by_ftype union splitting
        # and that works, even though inference generally avoids inferring the `!==` Method
        if isa(e.typ, Const)
            return inline_as_constant(e.typ.val, argexprs, sv, nothing)
        end
        not_is = Expr(:call, GlobalRef(Core.Intrinsics, :not_int),
                             Expr(:call, GlobalRef(Core, :(===)), argexprs[2], argexprs[3]))
        not_is.typ = Bool
        not_is.args[2].typ = Bool
        return (not_is, ())
    elseif length(atypes) == 3 && istopfunction(topmod, f, :(>:))
        # special-case inliner for issupertype
        # that works, even though inference generally avoids inferring the `>:` Method
        if isa(e.typ, Const)
            return inline_as_constant(e.typ.val, argexprs, sv, nothing)
        end
        arg_T1 = argexprs[2]
        arg_T2 = argexprs[3]
        issubtype_stmts = ()
        if !effect_free(arg_T2, sv.src, sv.mod, false)
            # spill first argument to preserve order-of-execution
            issubtype_vnew = newvar!(sv, widenconst(exprtype(arg_T1, sv.src, sv.mod)))
            issubtype_stmts = Any[ Expr(:(=), issubtype_vnew, arg_T1) ]
            arg_T1 = issubtype_vnew
        end
        issubtype_expr = Expr(:call, GlobalRef(Core, :(<:)), arg_T2, arg_T1)
        issubtype_expr.typ = Bool
        return (issubtype_expr, issubtype_stmts)
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
        (metharg, methsp) = ccall(:jl_type_intersection_with_env, Any, (Any, Any),
                                  atype_unlimited, method.sig)::SimpleVector
        methsp = methsp::SimpleVector
    end

    methsig = method.sig
    if !(atype <: metharg)
        return invoke_NF(argexprs, e.typ, atypes, sv, atype_unlimited,
                         invoke_data)
    end

    # check whether call can be inlined to just a quoted constant value
    if isa(f, widenconst(ft)) && !isdefined(method, :generator)
        if f === return_type
            if isconstType(e.typ)
                return inline_as_constant(e.typ.parameters[1], argexprs, sv, invoke_data)
            elseif isa(e.typ, Const)
                return inline_as_constant(e.typ.val, argexprs, sv, invoke_data)
            end
        elseif method.pure && isa(e.typ, Const) && e.typ.actual
            return inline_as_constant(e.typ.val, argexprs, sv, invoke_data)
        end
    end

    argexprs0 = argexprs
    atypes0 = atypes
    na = Int(method.nargs)
    # check for vararg function
    isva = false
    if na > 0 && method.isva
        @assert length(argexprs) >= na - 1
        # construct tuple-forming expression for argument tail
        vararg = mk_tuplecall(argexprs[na:end], sv)
        argexprs = Any[argexprs[1:(na - 1)]..., vararg]
        atypes = Any[atypes[1:(na - 1)]..., vararg.typ]
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
        isa(methsp[i], TypeVar) && return NF
    end

    # some gf have special tfunc, meaning they wouldn't have been inferred yet
    # check the same conditions from abstract_call to detect this case
    force_infer = false
    if !isdefined(method, :generator)
        if method.module == _topmod(method.module) || (isdefined(Main, :Base) && method.module == Main.Base)
            la = length(atypes)
            if (la==3 && (method.name == :getindex || method.name == :next)) ||
                (la==4 && method.name == :indexed_next)
                if atypes[3] ⊑ Int
                    at2 = widenconst(atypes[2])
                    if (at2 <: Tuple || at2 <: SimpleVector ||
                        (isa(at2, DataType) && (at2::DataType).name === Pair_name()))
                        force_infer = true
                    end
                end
            elseif la == 2 && method.name == :length && atypes[2] ⊑ SimpleVector
                force_infer = true
            end
        end
    end

    # see if the method has been previously inferred (and cached)
    linfo = code_for_method(method, metharg, methsp, sv.params.world, !force_infer) # Union{Void, MethodInstance}
    isa(linfo, MethodInstance) || return invoke_NF(argexprs0, e.typ, atypes0, sv,
                                                   atype_unlimited, invoke_data)
    linfo = linfo::MethodInstance
    if linfo.jlcall_api == 2
        # in this case function can be inlined to a constant
        add_backedge!(linfo, sv)
        return inline_as_constant(linfo.inferred_const, argexprs, sv, invoke_data)
    end

    # see if the method has a current InferenceState frame
    # or existing inferred code info
    frame = nothing # Union{Void, InferenceState}
    inferred = nothing # Union{Void, CodeInfo}
    if force_infer && la > 2 && isa(atypes[3], Const)
        # Since we inferred this with the information that atypes[3]::Const,
        # must inline with that same information.
        # We do that by overriding the argument type,
        # while ensuring we don't cache that information
        # This isn't particularly important for `getindex`,
        # as we'll be able to fix that up at the end of inlinable when we verify the return type.
        # But `next` and `indexed_next` make tuples which would end up burying some of that information in the AST
        # where we can't easily correct it afterwards.
        frame = InferenceState(linfo, #=optimize=#true, #=cache=#false, sv.params)
        frame.stmt_types[1][3] = VarState(atypes[3], false)
        typeinf(frame)
    else
        if isdefined(linfo, :inferred) && linfo.inferred !== nothing
            # use cache
            inferred = linfo.inferred
        elseif force_infer
            # create inferred code on-demand
            # but if we decided in the past not to try to infer this particular signature
            # (due to signature coarsening in abstract_call_gf_by_type)
            # don't infer it now, as attempting to force it now would be a bad idea (non terminating)
            frame = typeinf_frame(linfo, #=optimize=#true, #=cache=#true, sv.params)
        end
    end

    # compute the return value
    if isa(frame, InferenceState)
        frame = frame::InferenceState
        linfo = frame.linfo
        inferred = frame.src
        if frame.const_api # handle like jlcall_api == 2
            if frame.inferred || !frame.cached
                add_backedge!(frame.linfo, sv)
            else
                add_backedge!(frame, sv, 0)
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
    if inferred === nothing
        src_inferred = src_inlineable = false
    else
        src_inferred = ccall(:jl_ast_flag_inferred, Bool, (Any,), inferred)
        src_inlineable = ccall(:jl_ast_flag_inlineable, Bool, (Any,), inferred)
    end
    if !src_inferred || !src_inlineable
        return invoke_NF(argexprs0, e.typ, atypes0, sv, atype_unlimited,
                         invoke_data)
    end

    if isa(inferred, CodeInfo)
        src = inferred
        ast = copy_exprargs(inferred.code)
    else
        src = ccall(:jl_uncompress_ast, Any, (Any, Any), method, inferred)::CodeInfo
        ast = src.code
    end
    ast = ast::Array{Any,1}

    # create the backedge
    if isa(frame, InferenceState) && !frame.inferred && frame.cached
        # in this case, the actual backedge linfo hasn't been computed
        # yet, but will be when inference on the frame finishes
        add_backedge!(frame, sv, 0)
    else
        add_backedge!(linfo, sv)
    end

    nm = length(unwrap_unionall(metharg).parameters)

    body = Expr(:block)
    body.args = ast

    # see if each argument occurs only once in the body expression
    stmts = []
    prelude_stmts = []
    stmts_free = true # true = all entries of stmts are effect_free

    argexprs = copy(argexprs)
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
        if ((occ==0 && aeitype===Bottom) || (occ > 1 && !inline_worthy(aei, sv.src, sv.mod, sv.params)) ||
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
    body = substitute!(body, na, argexprs, method.sig, Any[methsp...], length(sv.src.slotnames) - na, boundscheck)
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
    if isa(lastexpr, LabelNode)
        push!(body.args, lastexpr)
        push!(body.args, Expr(:call, GlobalRef(topmod, :error), "fatal error in type inference (lowering)"))
        lastexpr = nothing
    elseif !(isa(lastexpr, Expr) && lastexpr.head === :return)
        # code sometimes ends with a meta node, e.g. inbounds pop
        push!(body.args, lastexpr)
        lastexpr = nothing
    end
    for a in body.args
        push!(stmts, a)
        if isa(a, Expr)
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

    inlining_ignore = function (@nospecialize(stmt),)
        isa(stmt, Expr) && return is_meta_expr(stmt::Expr)
        isa(stmt, LineNumberNode) && return true
        stmt === nothing && return true
        return false
    end

    do_coverage = coverage_enabled()
    line::Int = method.line
    file = method.file
    if !isempty(stmts)
        if !do_coverage && all(inlining_ignore, stmts)
            empty!(stmts)
        elseif isa(stmts[1], LineNumberNode)
            linenode = shift!(stmts)::LineNumberNode
            line = linenode.line
            isa(linenode.file, Symbol) && (file = linenode.file)
        end
    end
    if do_coverage
        # Check if we are switching module, which is necessary to catch user
        # code inlined into `Base` with `--code-coverage=user`.
        # Assume we are inlining directly into `enclosing` instead of another
        # function inlined in it
        mod = method.module
        if mod === sv.mod
            unshift!(stmts, Expr(:meta, :push_loc, file,
                                 method.name, line))
        else
            unshift!(stmts, Expr(:meta, :push_loc, file,
                                 method.name, line, mod))
        end
        push!(stmts, Expr(:meta, :pop_loc))
    elseif !isempty(stmts)
        unshift!(stmts, Expr(:meta, :push_loc, file,
                             method.name, line))
        if isa(stmts[end], LineNumberNode)
            stmts[end] = Expr(:meta, :pop_loc)
        else
            push!(stmts, Expr(:meta, :pop_loc))
        end
    end

    if isa(expr, Expr)
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

## Computing the cost of a function body

# saturating sum (inputs are nonnegative), prevents overflow with typemax(Int) below
plus_saturate(x, y) = max(x, y, x+y)
# known return type
isknowntype(T) = (T == Union{}) || isleaftype(T)

function statement_cost(ex::Expr, line::Int, src::CodeInfo, mod::Module, params::InferenceParams)
    head = ex.head
    if is_meta_expr(ex) || head == :copyast # not sure if copyast is right
        return 0
    end
    argcost = 0
    for a in ex.args
        if a isa Expr
            argcost = plus_saturate(argcost, statement_cost(a, line, src, mod, params))
        end
    end
    if head == :return || head == :(=)
        return argcost
    end
    if head == :call
        extyp = exprtype(ex.args[1], src, mod)
        if isa(extyp, Type)
            return argcost
        end
        if isa(extyp, Const)
            f = (extyp::Const).val
            if isa(f, IntrinsicFunction)
                iidx = Int(reinterpret(Int32, f::IntrinsicFunction)) + 1
                if !isassigned(t_ifunc_cost, iidx)
                    # unknown/unhandled intrinsic
                    return plus_saturate(argcost, params.inline_nonleaf_penalty)
                end
                return plus_saturate(argcost, t_ifunc_cost[iidx])
            end
            if isa(f, Builtin)
                # The efficiency of operations like a[i] and s.b
                # depend strongly on whether the result can be
                # inferred, so check ex.typ
                if f == Main.Core.getfield || f == Main.Core.tuple
                    # we might like to penalize non-inferrability, but
                    # tuple iteration/destructuring makes that
                    # impossible
                    # return plus_saturate(argcost, isknowntype(ex.typ) ? 1 : params.inline_nonleaf_penalty)
                    return argcost
                elseif f == Main.Core.arrayref
                    return plus_saturate(argcost, isknowntype(ex.typ) ? 4 : params.inline_nonleaf_penalty)
                end
                fidx = findfirst(x->x===f, t_ffunc_key)
                if fidx == 0
                    # unknown/unhandled builtin or anonymous function
                    # Use the generic cost of a direct function call
                    return plus_saturate(argcost, 20)
                end
                return plus_saturate(argcost, t_ffunc_cost[fidx])
            end
        end
        return plus_saturate(argcost, params.inline_nonleaf_penalty)
    elseif head == :foreigncall || head == :invoke
        # Calls whose "return type" is Union{} do not actually return:
        # they are errors. Since these are not part of the typical
        # run-time of the function, we omit them from
        # consideration. This way, non-inlined error branches do not
        # prevent inlining.
        return ex.typ == Union{} ? 0 : plus_saturate(20, argcost)
    elseif head == :llvmcall
        return plus_saturate(10, argcost) # a wild guess at typical cost
    elseif head == :enter
        # try/catch is a couple function calls,
        # but don't inline functions with try/catch
        # since these aren't usually performance-sensitive functions,
        # and llvm is more likely to miscompile them when these functions get large
        return typemax(Int)
    elseif head == :gotoifnot
        target = ex.args[2]::Int
        # loops are generally always expensive
        # but assume that forward jumps are already counted for from
        # summing the cost of the not-taken branch
        return target < line ? plus_saturate(40, argcost) : argcost
    end
    return argcost
end

function inline_worthy(body::Array{Any,1}, src::CodeInfo, mod::Module,
                       params::InferenceParams,
                       cost_threshold::Integer=params.inline_cost_threshold)
    bodycost = 0
    for line = 1:length(body)
        stmt = body[line]
        if stmt isa Expr
            thiscost = statement_cost(stmt, line, src, mod, params)::Int
        elseif stmt isa GotoNode
            # loops are generally always expensive
            # but assume that forward jumps are already counted for from
            # summing the cost of the not-taken branch
            thiscost = stmt.label < line ? 40 : 0
        else
            continue
        end
        bodycost = plus_saturate(bodycost, thiscost)
        bodycost == typemax(Int) && return false
    end
    return bodycost <= cost_threshold
end

function inline_worthy(body::Expr, src::CodeInfo, mod::Module, params::InferenceParams,
                       cost_threshold::Integer=params.inline_cost_threshold)
    bodycost = statement_cost(body, typemax(Int), src, mod, params)
    return bodycost <= cost_threshold
end

function inline_worthy(@nospecialize(body), src::CodeInfo, mod::Module, params::InferenceParams,
                       cost_threshold::Integer=params.inline_cost_threshold)
    newbody = exprtype(body, src, mod)
    !isa(newbody, Expr) && return true
    return inline_worthy(newbody, src, mod, params, cost_threshold)
end

ssavalue_increment(@nospecialize(body), incr) = body
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

function inlining_pass!(sv::InferenceState, propagate_inbounds::Bool)
    # Also handles bounds check elision:
    #
    #    1. If check_bounds is always on, set `Expr(:boundscheck)` true
    #    2. If check_bounds is always off, set `Expr(:boundscheck)` false
    #    3. If check_bounds is default, figure out whether each boundscheck
    #         is true, false, or propagate based on the enclosing inbounds directives
    _opt_check_bounds = JLOptions().check_bounds
    opt_check_bounds = (_opt_check_bounds == 0 ? :default :
                        _opt_check_bounds == 1 ? :on :
                        :off)
    # Number of stacked inbounds
    inbounds_depth = 0

    eargs = sv.src.code
    i = 1
    stmtbuf = []
    while i <= length(eargs)
        ei = eargs[i]
        if isa(ei, Expr)
            if ei.head === :inbounds
                eargs[i] = nothing
                arg1 = ei.args[1]
                if arg1 === true # push
                    inbounds_depth += 1
                elseif arg1 === false # clear
                    inbounds_depth = 0
                elseif inbounds_depth > 0 # pop
                    inbounds_depth -= 1
                end
            else
                if opt_check_bounds === :off
                     boundscheck = :off
                elseif opt_check_bounds === :on
                     boundscheck = :on
                elseif inbounds_depth > 0
                     boundscheck = :off
                elseif propagate_inbounds
                     boundscheck = :propagate
                else
                     boundscheck = :on
                end
                eargs[i] = inlining_pass(ei, sv, stmtbuf, 1, boundscheck)
                if !isempty(stmtbuf)
                    splice!(eargs, i:(i - 1), stmtbuf)
                    i += length(stmtbuf)
                    empty!(stmtbuf)
                end
            end
        end
        i += 1
    end
end

const corenumtype = Union{Int32, Int64, Float32, Float64}

# return inlined replacement for `e`, inserting new needed statements
# at index `ins` in `stmts`.
function inlining_pass(e::Expr, sv::InferenceState, stmts::Vector{Any}, ins, boundscheck::Symbol)
    if e.head === :meta
        # ignore meta nodes
        return e
    end
    if e.head === :method
        # avoid running the inlining pass on function definitions
        return e
    end
    # inliners for special expressions
    if e.head === :boundscheck
        return e
    end
    if e.head === :isdefined
        isa(e.typ, Const) && return e.typ.val
        return e
    end

    eargs = e.args
    if length(eargs) < 1
        return e
    end
    arg1 = eargs[1]
    isccall = false
    i0 = 1
    # don't inline first (global) arguments of ccall, as this needs to be evaluated
    # by the interpreter and inlining might put in something it can't handle,
    # like another ccall (or try to move the variables out into the function)
    if e.head === :foreigncall
        # 5 is rewritten to 1 below to handle the callee.
        i0 = 5
        isccall = true
    elseif is_known_call(e, Core.Intrinsics.llvmcall, sv.src, sv.mod)
        i0 = 5
    end
    has_stmts = false # needed to preserve order-of-execution
    prev_stmts_length = length(stmts)
    for _i = length(eargs):-1:i0
        if isccall && _i == 5
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
            sl0 = length(stmts)
            res = inlining_pass(ei, sv, stmts, ins, boundscheck)
            ns = length(stmts) - sl0  # number of new statements just added
            if isccallee
                restype = exprtype(res, sv.src, sv.mod)
                if isa(restype, Const)
                    argloc[i] = restype.val
                    if !effect_free(res, sv.src, sv.mod, false)
                        insert!(stmts, ins+ns, res)
                    end
                    # Assume this is the last argument to process
                    break
                end
            end
            if has_stmts && !effect_free(res, sv.src, sv.mod, false)
                restype = exprtype(res, sv.src, sv.mod)
                vnew = newvar!(sv, restype)
                argloc[i] = vnew
                insert!(stmts, ins+ns, Expr(:(=), vnew, res))
            else
                argloc[i] = res
            end
            if !has_stmts && ns > 0 && !(_i == i0)
                for s = ins:ins+ns-1
                    stmt = stmts[s]
                    if !effect_free(stmt, sv.src, sv.mod, true)
                        has_stmts = true; break
                    end
                end
            end
        end
    end
    if isccall
        le = length(eargs)
        nccallargs = eargs[5]::Int
        ccallargs = ObjectIdDict()
        for i in 6:(5 + nccallargs)
            ccallargs[eargs[i]] = nothing
        end
        i = 6 + nccallargs
        while i <= le
            rootarg = eargs[i]
            if haskey(ccallargs, rootarg)
                deleteat!(eargs, i)
                le -= 1
            elseif i < le
                ccallargs[rootarg] = nothing
            end
            i += 1
        end
    end
    if e.head !== :call
        return e
    end

    ft = exprtype(arg1, sv.src, sv.mod)
    if isa(ft, Const)
        f = ft.val
    elseif isa(ft, Conditional)
        f = nothing
        ft = Bool
    else
        f = nothing
        if !( isleaftype(ft) || ft<:Type )
            return e
        end
    end

    ins += (length(stmts) - prev_stmts_length)

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
                        res = inlining_pass(e, sv, stmts, ins, boundscheck)
                    else
                        e.args = Any[GlobalRef(Main.Base,:*), Expr(:call, GlobalRef(Main.Base,:*), a1, a1), a1]
                        e.args[2].typ = e.typ
                        res = inlining_pass(e, sv, stmts, ins, boundscheck)
                    end
                    return res
                end
            end
        end
    end

    for ninline = 1:100
        ata = Vector{Any}(length(e.args))
        ata[1] = ft
        for i = 2:length(e.args)
            a = exprtype(e.args[i], sv.src, sv.mod)
            (a === Bottom || isvarargtype(a)) && return e
            ata[i] = a
        end
        res = inlineable(f, ft, e, ata, stmts, boundscheck, sv)
        if isa(res,Tuple)
            if isa(res[2],Array) && !isempty(res[2])
                splice!(stmts, ins:ins-1, res[2])
                ins += length(res[2])
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
                return res
            end
        end

        if f === _apply
            na = length(e.args)
            newargs = Vector{Any}(na-2)
            newstmts = Any[]
            effect_free_upto = 0
            for i = 3:na
                aarg = e.args[i]
                argt = exprtype(aarg, sv.src, sv.mod)
                t = widenconst(argt)
                if isa(aarg, Expr) && (is_known_call(aarg, tuple, sv.src, sv.mod) || is_known_call(aarg, svec, sv.src, sv.mod))
                    # apply(f, tuple(x, y, ...)) => f(x, y, ...)
                    newargs[i - 2] = aarg.args[2:end]
                elseif isa(argt, Const) && (isa(argt.val, Tuple) || isa(argt.val, SimpleVector)) &&
                        effect_free(aarg, sv.src, sv.mod, true)
                    val = argt.val
                    newargs[i - 2] = Any[ QuoteNode(val[i]) for i in 1:(length(val)::Int) ] # avoid making a tuple Generator here!
                elseif isa(aarg, Tuple) || (isa(aarg, QuoteNode) && (isa(aarg.value, Tuple) || isa(aarg.value, SimpleVector)))
                    if isa(aarg, QuoteNode)
                        aarg = aarg.value
                    end
                    newargs[i - 2] = Any[ QuoteNode(aarg[i]) for i in 1:(length(aarg)::Int) ] # avoid making a tuple Generator here!
                elseif isa(t, DataType) && t.name === Tuple.name && !isvatuple(t) &&
                         length(t.parameters) <= sv.params.MAX_TUPLE_SPLAT
                    for k = (effect_free_upto + 1):(i - 3)
                        as = newargs[k]
                        for kk = 1:length(as)
                            ak = as[kk]
                            if !effect_free(ak, sv.src, sv.mod, true)
                                tmpv = newvar!(sv, widenconst(exprtype(ak, sv.src, sv.mod)))
                                push!(newstmts, Expr(:(=), tmpv, ak))
                                as[kk] = tmpv
                            end
                        end
                    end
                    effect_free_upto = i - 3
                    if effect_free(aarg, sv.src, sv.mod, true)
                        # apply(f,t::(x,y)) => f(t[1],t[2])
                        tmpv = aarg
                    else
                        # apply(f,t::(x,y)) => tmp=t; f(tmp[1],tmp[2])
                        tmpv = newvar!(sv, t)
                        push!(newstmts, Expr(:(=), tmpv, aarg))
                    end
                    if is_specializable_vararg_slot(aarg, sv)
                        tp = sv.vararg_type_container.parameters
                    else
                        tp = t.parameters
                    end
                    newargs[i - 2] = Any[ mk_getfield(tmpv, j, tp[j]) for j in 1:(length(tp)::Int) ]
                else
                    # not all args expandable
                    return e
                end
            end
            splice!(stmts, ins:(ins - 1), newstmts)
            ins += length(newstmts)
            e.args = [Any[e.args[2]]; newargs...]

            # now try to inline the simplified call
            ft = exprtype(e.args[1], sv.src, sv.mod)
            if isa(ft, Const)
                f = ft.val
            elseif isa(ft, Conditional)
                f = nothing
                ft = Bool
            else
                f = nothing
                if !( isleaftype(ft) || ft<:Type )
                    return e
                end
            end
        else
            return e
        end
    end
    return e
end

const compiler_temp_sym = Symbol("#temp#")

function add_slot!(src::CodeInfo, @nospecialize(typ), is_sa::Bool, name::Symbol=compiler_temp_sym)
    @assert !isa(typ, Const) && !isa(typ, Conditional)
    id = length(src.slotnames) + 1
    push!(src.slotnames, name)
    push!(src.slottypes, typ)
    push!(src.slotflags, is_sa * Slot_AssignedOnce)
    return SlotNumber(id)
end

function is_known_call(e::Expr, @nospecialize(func), src::CodeInfo, mod::Module)
    if e.head !== :call
        return false
    end
    f = exprtype(e.args[1], src, mod)
    return isa(f, Const) && f.val === func
end

function is_known_call_p(e::Expr, @nospecialize(pred), src::CodeInfo, mod::Module)
    if e.head !== :call
        return false
    end
    f = exprtype(e.args[1], src, mod)
    return (isa(f, Const) && pred(f.val)) || (isType(f) && pred(f.parameters[1]))
end

function record_used(@nospecialize(e), @nospecialize(T), used::Vector{Bool})
    if isa(e,T)
        used[e.id+1] = true
    elseif isa(e,Expr)
        i0 = e.head === :(=) ? 2 : 1
        for i = i0:length(e.args)
            record_used(e.args[i], T, used)
        end
    end
end

function remove_unused_vars!(src::CodeInfo)
    used = fill(false, length(src.slotnames)+1)
    used_ssa = fill(false, length(src.ssavaluetypes)+1)
    for i = 1:length(src.code)
        record_used(src.code[i], Slot, used)
        record_used(src.code[i], SSAValue, used_ssa)
    end
    for i = 1:length(src.code)
        e = src.code[i]
        if isa(e,NewvarNode) && !used[e.slot.id+1]
            src.code[i] = nothing
        elseif isa(e,Expr) && e.head === :(=)
            if (isa(e.args[1],Slot) && !used[e.args[1].id+1]) ||
                (isa(e.args[1],SSAValue) && !used_ssa[e.args[1].id+1])
                src.code[i] = e.args[2]
            end
        end
    end
end

function delete_vars!(src::CodeInfo, r::ObjectIdDict)
    filter!(x->!(isa(x,Expr) && (x.head === :(=) || x.head === :const) &&
                 haskey(r, normvar(x.args[1]))),
            src.code)
    return src
end

function replace_vars!(src::CodeInfo, r::ObjectIdDict)
    for i = 1:length(src.code)
        src.code[i] = _replace_vars!(src.code[i], r)
    end
    return src
end

function _replace_vars!(@nospecialize(e), r::ObjectIdDict)
    if isa(e, SSAValue) || isa(e, Slot)
        v = normvar(e)
        if haskey(r, v)
            return r[v]
        end
    end
    if isa(e, Expr)
        for i = 1:length(e.args)
            a = e.args[i]
            if e.head === :isdefined
                if e.args[i] !== _replace_vars!(a, r)
                    return true
                end
            else
                e.args[i] = _replace_vars!(a, r)
            end
        end
    end
    return e
end

is_argument(nargs::Int, v::Slot) = slot_id(v) <= nargs

normslot(s::SlotNumber) = s
normslot(s::TypedSlot) = SlotNumber(slot_id(s))
normvar(s::Slot) = normslot(s)
normvar(s::SSAValue) = s
normvar(@nospecialize(s)) = s

# given a single-assigned var and its initializer `init`, return what we can
# replace `var` with, or `var` itself if we shouldn't replace it
function get_replacement(table::ObjectIdDict, var::Union{SlotNumber, SSAValue}, @nospecialize(init),
                         nargs::Int, slottypes::Vector{Any}, ssavaluetypes::Vector{Any})
    #if isa(init, QuoteNode)  # this can cause slight code size increases
    #    return init
    if isa(init, Expr) && init.head === :static_parameter
        # if we aren't certain about the type, it might be an UndefVarError at runtime (!effect_free)
        # so we need to preserve the original point of assignment
        if isa(init.typ, DataType) && isleaftype(init.typ)
            return init
        end
    elseif isa(init, corenumtype) || init === () || init === nothing
        return init
    elseif isa(init, Slot) && is_argument(nargs, init::Slot)
        # the transformation is not ideal if the assignment
        # is present for the auto-unbox functionality
        # (from inlining improved type inference information)
        # and this transformation would worsen the type information
        # everywhere later in the function
        ityp = isa(init, TypedSlot) ? init.typ : slottypes[(init::SlotNumber).id]
        if ityp ⊑ (isa(var,SSAValue) ? ssavaluetypes[var.id + 1] : slottypes[var.id])
            return init
        end
    elseif isa(init, SSAValue)
        if isa(var, SlotNumber) && slottypes[var.id] ⊑ Tuple
            # Here we avoid replacing a Slot with an SSAValue when the type is an
            # aggregate. That can cause LLVM to generate a bunch of extra memcpys
            # if the data ever needs to be stack allocated later.
            return var
        end
        if haskey(table, init)
            return get_replacement(table, init, table[init], nargs, slottypes, ssavaluetypes)
        end
        return init
    elseif isa(init, SlotNumber) && haskey(table, init)
        return get_replacement(table, init, table[init], nargs, slottypes, ssavaluetypes)
    elseif isa(init, TypedSlot)
        sl = normslot(init)
        if haskey(table, sl)
            rep = get_replacement(table, sl, table[sl], nargs, slottypes, ssavaluetypes)
            if isa(rep, SlotNumber)
                rep = TypedSlot(rep.id, init.typ)
            end
            return rep
        end
    end
    return var
end

# remove all single-assigned vars v in "v = x" where x is an argument.
# "sa" is the result of find_sa_vars
function remove_redundant_temp_vars!(src::CodeInfo, nargs::Int, sa::ObjectIdDict)
    flags = src.slotflags
    slottypes = src.slottypes
    ssavaluetypes = src.ssavaluetypes
    repls = ObjectIdDict()
    for (v, init) in sa
        repl = get_replacement(sa, v, init, nargs, slottypes, ssavaluetypes)
        compare = isa(repl, TypedSlot) ? normslot(repl) : repl
        if compare !== v
            repls[v] = repl
        end
    end
    if !isempty(repls)
        delete_vars!(src, repls)
        replace_vars!(src, repls)
    end
    return src
end

# compute set of slots assigned once
function find_sa_vars(src::CodeInfo, nargs::Int)
    body = src.code
    av = ObjectIdDict()
    av2 = ObjectIdDict()
    for i = 1:length(body)
        e = body[i]
        if isa(e,Expr) && e.head === :(=)
            lhs = e.args[1]
            if isa(lhs, SSAValue)
                av[lhs] = e.args[2]
            elseif isa(lhs, Slot)
                lhs = normslot(lhs)
                id = lhs.id
                # exclude args and used undef vars
                # this transformation is not valid for vars used before def.
                # we need to preserve the point of assignment to know where to
                # throw errors (issue #4645).
                if id > nargs && (src.slotflags[id] & Slot_UsedUndef == 0)
                    if !haskey(av, lhs)
                        av[lhs] = e.args[2]
                    else
                        av2[lhs] = true
                    end
                end
            end
        end
    end
    filter!(p -> !haskey(av2, p.first), av)
    return av
end

symequal(x::SSAValue, y::SSAValue) = x.id === y.id
symequal(x::Slot    , y::Slot)     = x.id === y.id
symequal(@nospecialize(x)     , @nospecialize(y))      = x === y

function occurs_outside_getfield(@nospecialize(e), @nospecialize(sym),
                                 sv::InferenceState, field_count::Int, @nospecialize(field_names))
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
        elseif head === :foreigncall
            args = e.args
            nccallargs = args[5]::Int
            # Only arguments escape the structure/layout of the object,
            # GC root arguments do not.
            # Also note that only being used in the root slot for this ccall itself
            # does **not** mean that the object is not needed during the ccall.
            # However, if its address is never taken
            # and the object is never used in a way that escapes its layout, we can be sure
            # that there's no way the user code can rely on the heap allocation of this object.
            for i in 1:length(args)
                a = args[i]
                if i > 5 + nccallargs && symequal(a, sym)
                    # No need to verify indices, uninitialized members can be
                    # ignored in root slot.
                    continue
                end
                if occurs_outside_getfield(a, sym, sv, field_count, field_names)
                    return true
                end
            end
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
    not_void_use = function (@nospecialize(ex),)
        if isa(ex, SSAValue)
            # Explicitly listed here for clarity
            return false
        elseif isa(ex, Slot)
            return sv.src.slotflags[slot_id(ex)] & Slot_UsedUndef != 0
        elseif isa(ex, GlobalRef)
            ex = ex::GlobalRef
            return !isdefined(ex.mod, ex.name)
        elseif isa(ex, Expr)
            h = ex.head
            if h === :return || h === :(=) || h === :gotoifnot || is_meta_expr_head(h)
                return true
            end
            return !effect_free(ex, sv.src, sv.mod, false)
        elseif (isa(ex, GotoNode) || isa(ex, LineNumberNode) ||
                isa(ex, NewvarNode) || isa(ex, Symbol) || isa(ex, LabelNode))
            # This is a list of special types handled by the compiler
            return true
        end
        return false
    end
    filter!(not_void_use, sv.src.code::Array{Any,1})
    nothing
end

function meta_elim_pass!(code::Array{Any,1}, do_coverage::Bool)
    # 1. Remove place holders
    #
    # 2. If coverage is off, remove line number nodes that don't mark any
    #    real expressions.
    #
    # 3. Remove top level SSAValue
    #

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
            prev_dbg_stack[end] = 0
            push_loc_pos_stack[end] = 0
            continue
        end
        ex = ex::Expr
        args = ex.args
        head = ex.head
        if head !== :meta
            prev_dbg_stack[end] = 0
            push_loc_pos_stack[end] = 0
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
                prev_dbg_stack[end] = 0
                push_loc_pos_stack[end] = 0
            end
        else
            continue
        end
    end
end

# does the same job as alloc_elim_pass for allocations inline in getfields
# TODO can probably be removed when we switch to a linear IR
function getfield_elim_pass!(sv::InferenceState)
    body = sv.src.code
    nssavalues = length(sv.src.ssavaluetypes)
    sv.ssavalue_defs = find_ssavalue_defs(body, nssavalues)
    sv.ssavalue_uses = find_ssavalue_uses(body, nssavalues)
    for i = 1:length(body)
        body[i] = _getfield_elim_pass!(body[i], sv)
    end
end

function _getfield_elim_pass!(e::Expr, sv::InferenceState)
    nargs = length(e.args)
    for i = 1:nargs
        e.args[i] = _getfield_elim_pass!(e.args[i], sv)
    end
    if is_known_call(e, getfield, sv.src, sv.mod) &&
            (nargs == 3 || nargs == 4) &&
            (isa(e.args[3], Int) || isa(e.args[3], QuoteNode)) &&
            (nargs == 3 || isa(e.args[4], Bool))
        e1 = e.args[2]
        j = e.args[3]
        single_use = true
        while isa(e1, SSAValue)
            if single_use
                if length(sv.ssavalue_uses[e1.id + 1]) > 1
                    single_use = false
                end
            end
            def = sv.ssavalue_defs[e1.id + 1]
            stmt = sv.src.code[def]::Expr
            e1 = stmt.args[2]
        end
        if isa(e1, Expr)
            alloc = single_use && is_allocation(e1, sv)
            if alloc !== false
                flen, fnames = alloc
                if isa(j, QuoteNode)
                    j = findfirst(equalto(j.value), fnames)
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
        elseif isa(e1, GlobalRef) || isa(e1, Symbol) || isa(e1, Slot)
            # non-self-quoting value
        else
            if isa(e1, QuoteNode)
                e1 = e1.value
            end
            if isimmutable(e1) || isa(e1, SimpleVector)
                # SimpleVector length field is immutable
                if isa(j, QuoteNode)
                    j = j.value
                    if !(isa(j, Int) || isa(j, Symbol))
                        return e
                    end
                end
                if isdefined(e1, j)
                    e1j = getfield(e1, j)
                    if !is_self_quoting(e1j)
                        e1j = QuoteNode(e1j)
                    end
                    return e1j
                end
            end
        end
    end
    return e
end

_getfield_elim_pass!(@nospecialize(e), sv) = e

# check if e is a successful allocation of an struct
# if it is, returns (n,f) such that it is always valid to call
# getfield(..., 1 <= x <= n) or getfield(..., x in f) on the result
function is_allocation(@nospecialize(e), sv::InferenceState)
    isa(e, Expr) || return false
    if is_known_call(e, tuple, sv.src, sv.mod)
        return (length(e.args)-1,())
    elseif e.head === :new
        typ = widenconst(exprtype(e, sv.src, sv.mod))
        if isa(typ, DataType) && isleaftype(typ)
            nf = length(e.args) - 1
            names = fieldnames(typ)
            @assert(nf <= length(names))
            if nf < length(names)
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

# basic dead-code-elimination of unreachable statements
function basic_dce_pass!(sv::InferenceState)
    body = sv.src.code
    labelmap = get_label_map(body, sv)
    reachable = IntSet()
    W = IntSet()
    push!(W, 1)
    while !isempty(W)
        pc = pop!(W)
        pc in reachable && continue
        push!(reachable, pc)
        expr = body[pc]
        pc += 1
        if isa(expr, GotoNode)
            pc = labelmap[expr.label]
        elseif isa(expr, Expr)
            label = 0
            if expr.head === :gotoifnot
                label = labelmap[expr.args[2]::Int]
                label === 0 || push!(W, label) # inference must have computed that this condition is always true
            elseif expr.head === :enter
                push!(W, labelmap[expr.args[1]::Int])
            elseif expr.head === :return
                continue
            end
        end
        pc <= length(body) && push!(W, pc)
    end
    for i in 1:length(body)
        expr = body[i]
        if !(i in reachable ||
             (isa(expr, Expr) && is_meta_expr(expr)) ||
             isa(expr, LineNumberNode))
            body[i] = nothing
        end
    end
end


# eliminate allocation of unnecessary objects
# that are only used as arguments to safe getfield calls
function alloc_elim_pass!(sv::InferenceState)
    body = sv.src.code
    bexpr = Expr(:block)
    bexpr.args = body
    vs = find_sa_vars(sv.src, sv.nargs)
    remove_redundant_temp_vars!(sv.src, sv.nargs, vs)
    remove_unused_vars!(sv.src)
    i = 1
    while i < length(body)
        e = body[i]
        if !isa(e, Expr)
            i += 1
            continue
        end
        e = e::Expr
        if e.head === :(=) && (isa(e.args[1], SSAValue) ||
                               (isa(e.args[1], Slot) && haskey(vs, normslot(e.args[1]))))
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
            # allocation of mutable structs too
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
        if !isa(a, Expr)
            continue
        end
        a = a::Expr
        if is_known_call(a, getfield, sv.src, sv.mod) && symequal(a.args[2], tupname)
            idx = if isa(a.args[3], Int)
                a.args[3]
            else
                @assert isa(a.args[3], QuoteNode)
                findfirst(equalto(a.args[3].value), field_names)
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
        else
            if a.head === :foreigncall
                args = a.args
                nccallargs = args[5]::Int
                le = length(args)
                next_i = 6 + nccallargs
                while next_i <= le
                    i = next_i
                    next_i += 1

                    symequal(args[i], tupname) || continue
                    # Replace the gc root argument with its fields
                    splice!(args, i, vals)
                    next_i += length(vals) - 1
                end
            end
            replace_getfield!(a, tupname, vals, field_names, sv)
        end
    end
end

# fix label numbers to always equal the statement index of the label
function reindex_labels!(sv::InferenceState)
    body = sv.src.code
    mapping = get_label_map(body, sv)
    for i = 1:length(body)
        el = body[i]
        # For goto and enter, the statement and the target has to be
        # both reachable or both not. For gotoifnot, the dead code
        # elimination in type_annotate! can delete the target
        # of a reachable (but never taken) node. In which case we can
        # just replace the node with the branch condition.
        if isa(el, LabelNode)
            labelnum = mapping[el.label]
            @assert labelnum !== 0
            body[i] = LabelNode(labelnum)
        elseif isa(el, GotoNode)
            labelnum = mapping[el.label]
            @assert labelnum !== 0
            body[i] = GotoNode(labelnum)
        elseif isa(el, Expr)
            if el.head === :gotoifnot
                labelnum = mapping[el.args[2]::Int]
                if labelnum === 0
                    # Might still have side effects
                    body[i] = el.args[1]
                else
                    el.args[2] = labelnum
                end
            elseif el.head === :enter
                labelnum = mapping[el.args[1]::Int]
                @assert labelnum !== 0
                el.args[1] = labelnum
            end
        end
    end
end

function return_type(@nospecialize(f), @nospecialize(t))
    params = InferenceParams(ccall(:jl_get_tls_world_age, UInt, ()))
    rt = Union{}
    if isa(f, Builtin)
        rt = builtin_tfunction(f, Any[t.parameters...], nothing, params)
        if isa(rt, TypeVar)
            rt = rt.ub
        else
            rt = widenconst(rt)
        end
    else
        for m in _methods(f, t, -1, params.world)
            ty = typeinf_type(m[3], m[1], m[2], true, params)
            ty === nothing && return Any
            rt = tmerge(rt, ty)
            rt === Any && break
        end
    end
    return rt
end

#### bootstrapping ####

# make sure that typeinf is executed before turning on typeinf_ext
# this ensures that typeinf_ext doesn't recurse before it can add the item to the workq
# especially try to make sure any recursive and leaf functions have concrete signatures,
# since we won't be able to specialize & infer them at runtime

let fs = Any[typeinf_ext, typeinf, typeinf_edge, occurs_outside_getfield, pure_eval_call],
    world = ccall(:jl_get_world_counter, UInt, ())
    for x in t_ffunc_val
        push!(fs, x[3])
    end
    for i = 1:length(t_ifunc)
        if isassigned(t_ifunc, i)
            x = t_ifunc[i]
            push!(fs, x[3])
        else
            println(STDERR, "WARNING: tfunc missing for ", reinterpret(IntrinsicFunction, Int32(i)))
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
            typeinf_type(m[3], Tuple{typ...}, m[2], true, InferenceParams(world))
        end
    end
end
