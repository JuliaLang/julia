# This file is a part of Julia. License is MIT: https://julialang.org/license

import Core: _apply, svec, apply_type, Builtin, IntrinsicFunction, MethodInstance

#### parameters limiting potentially-infinite types ####
const MAX_TYPEUNION_LEN = 3
const MAX_TYPE_DEPTH = 8
const TUPLE_COMPLEXITY_LIMIT_DEPTH = 3

const MAX_INLINE_CONST_SIZE = 256

const empty_vector = Vector{Any}()

mutable struct InferenceResult
    linfo::MethodInstance
    args::Vector{Any}
    result # ::Type, or InferenceState if WIP
    src::Union{CodeInfo, Nothing} # if inferred copy is available
    function InferenceResult(linfo::MethodInstance)
        if isdefined(linfo, :inferred_const)
            result = Const(linfo.inferred_const)
        else
            result = linfo.rettype
        end
        return new(linfo, empty_vector, result, nothing)
    end
end

function get_argtypes(result::InferenceResult)
    result.args === empty_vector || return result.args # already cached
    linfo = result.linfo
    toplevel = !isa(linfo.def, Method)
    atypes::SimpleVector = unwrap_unionall(linfo.specTypes).parameters
    nargs::Int = toplevel ? 0 : linfo.def.nargs
    args = Vector{Any}(uninitialized, nargs)
    if !toplevel && linfo.def.isva
        if linfo.specTypes == Tuple
            if nargs > 1
                atypes = svec(Any[ Any for i = 1:(nargs - 1) ]..., Tuple.parameters[1])
            end
            vararg_type = Tuple
        else
            vararg_type = rewrap(tupleparam_tail(atypes, nargs), linfo.specTypes)
        end
        args[nargs] = vararg_type
        nargs -= 1
    end
    laty = length(atypes)
    if laty > 0
        if laty > nargs
            laty = nargs
        end
        local lastatype
        atail = laty
        for i = 1:laty
            atyp = atypes[i]
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
                atyp = Const(atyp.parameters[1])
            else
                atyp = rewrap_unionall(atyp, linfo.specTypes)
            end
            i == laty && (lastatype = atyp)
            args[i] = atyp
        end
        for i = (atail + 1):nargs
            args[i] = lastatype
        end
    else
        @assert nargs == 0 "invalid specialization of method" # wrong number of arguments
    end
    result.args = args
    return args
end

struct InferenceParams
    cache::Vector{InferenceResult}
    world::UInt

    # optimization
    inlining::Bool
    ipo_constant_propagation::Bool
    aggressive_constant_propagation::Bool
    inline_cost_threshold::Int  # number of CPU cycles beyond which it's not worth inlining
    inline_nonleaf_penalty::Int # penalty for dynamic dispatch
    inline_tupleret_bonus::Int  # extra willingness for non-isbits tuple return types

    # don't consider more than N methods. this trades off between
    # compiler performance and generated code performance.
    # typically, considering many methods means spending lots of time
    # obtaining poor type information.
    # It is important for N to be >= the number of methods in the error()
    # function, so we can still know that error() is always Bottom.
    MAX_METHODS::Int
    # the maximum number of union-tuples to swap / expand
    # before computing the set of matching methods
    MAX_UNION_SPLITTING::Int
    # the maximum number of union-tuples to swap / expand
    # when inferring a call to _apply
    MAX_APPLY_UNION_ENUM::Int

    # parameters limiting large types
    MAX_TUPLETYPE_LEN::Int
    MAX_TUPLE_DEPTH::Int

    # when attempting to inlining _apply, abort the optimization if the tuple
    # contains more than this many elements
    MAX_TUPLE_SPLAT::Int

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
        return new(Vector{InferenceResult}(),
                   world, inlining, true, false, inline_cost_threshold, inline_nonleaf_penalty,
                   inline_tupleret_bonus, max_methods, union_splitting, apply_union_enum,
                   tupletype_len, tuple_depth, tuple_splat)
    end
end

# slot property bit flags
# The slot is has uses that are not statically dominated by any assignment
# This is implied by `Slot_UsedUndef`.
# If this is not set, all the uses are (statically) dominated by the defs.
# In particular, if a slot has `AssignedOnce && !StaticUndef`, it is an SSA.
const Slot_StaticUndef  = 1
# The slot is assigned to only once
const Slot_AssignedOnce = 16
# The slot has uses that might raise UndefVarError
const Slot_UsedUndef    = 32
# const Slot_Called       = 64

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
    params::InferenceParams # describes how to compute the result
    result::InferenceResult # remember where to put the result
    linfo::MethodInstance # used here for the tuple (specTypes, env, Method) and world-age validity
    sp::SimpleVector     # static parameters
    mod::Module
    currpc::LineNum

    # info on the state of inference and the linfo
    src::CodeInfo
    min_valid::UInt
    max_valid::UInt
    nargs::Int
    stmt_types::Vector{Any}
    stmt_edges::Vector{Any}
    # return type
    bestguess #::Type
    # current active instruction pointers
    ip::BitSet
    pc´´::LineNum
    nstmts::Int
    # current exception handler info
    cur_hand #::Tuple{LineNum, Tuple{LineNum, ...}}
    handler_at::Vector{Any}
    n_handlers::Int
    # ssavalue sparsity and restart info
    ssavalue_uses::Vector{BitSet}
    ssavalue_defs::Vector{LineNum}
    vararg_type_container #::Type

    backedges::Vector{Tuple{InferenceState, LineNum}} # call-graph backedges connecting from callee to caller
    callers_in_cycle::Vector{InferenceState}
    parent::Union{Nothing, InferenceState}

    const_api::Bool
    const_ret::Bool

    # TODO: move these to InferenceResult / InferenceParams?
    optimize::Bool
    cached::Bool
    limited::Bool
    inferred::Bool
    dont_work_on_me::Bool

    # src is assumed to be a newly-allocated CodeInfo, that can be modified in-place to contain intermediate results
    function InferenceState(result::InferenceResult, src::CodeInfo,
                            optimize::Bool, cached::Bool, params::InferenceParams)
        linfo = result.linfo
        code = src.code::Array{Any,1}
        toplevel = !isa(linfo.def, Method)

        if !toplevel && isempty(linfo.sparam_vals) && !isempty(linfo.def.sparam_syms)
            # linfo is unspecialized
            sp = Any[]
            sig = linfo.def.sig
            while isa(sig, UnionAll)
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
        argtypes = get_argtypes(result)
        vararg_type_container = nothing
        nargs = length(argtypes)
        s_argtypes = VarTable(uninitialized, nslots)
        src.slottypes = Vector{Any}(uninitialized, nslots)
        for i in 1:nslots
            at = (i > nargs) ? Bottom : argtypes[i]
            if !toplevel && linfo.def.isva && i == nargs
                if !(at == Tuple) # would just be a no-op
                    vararg_type_container = limit_tuple_depth(params, unwrap_unionall(at)) # TODO: should be limiting tuple depth much earlier than here
                    vararg_type = tuple_tfunc(vararg_type_container) # returns a Const object, if applicable
                    at = rewrap(vararg_type, linfo.specTypes)
                end
            end
            s_argtypes[i] = VarState(at, i > nargs)
            src.slottypes[i] = at
        end
        s_types[1] = s_argtypes

        ssavalue_uses = find_ssavalue_uses(code, nssavalues)
        ssavalue_defs = find_ssavalue_defs(code, nssavalues)

        # exception handlers
        cur_hand = ()
        handler_at = Any[ () for i=1:n ]
        n_handlers = 0

        W = BitSet()
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
            params, result, linfo,
            sp, inmodule, 0,
            src, min_valid, max_valid,
            nargs, s_types, s_edges,
            Union{}, W, 1, n,
            cur_hand, handler_at, n_handlers,
            ssavalue_uses, ssavalue_defs, vararg_type_container,
            Vector{Tuple{InferenceState,LineNum}}(), # backedges
            Vector{InferenceState}(), # callers_in_cycle
            #=parent=#nothing,
            false, false, optimize, cached, false, false, false)
        result.result = frame
        cached && push!(params.cache, result)
        return frame
    end
end


function InferenceState(linfo::MethodInstance,
                        optimize::Bool, cached::Bool, params::InferenceParams)
    return InferenceState(InferenceResult(linfo), optimize, cached, params)
end
function InferenceState(result::InferenceResult,
                        optimize::Bool, cached::Bool, params::InferenceParams)
    # prepare an InferenceState object for inferring lambda
    src = retrieve_code_info(result.linfo)
    src === nothing && return nothing
    _validate(result.linfo, src, "lowered")
    return InferenceState(result, src, optimize, cached, params)
end

function _validate(linfo::MethodInstance, src::CodeInfo, kind::String)
    if JLOptions().debug_level == 2
        # this is a debug build of julia, so let's validate linfo
        errors = validate_code(linfo, src)
        if !isempty(errors)
            for e in errors
                if linfo.def isa Method
                    println(STDERR, "WARNING: Encountered invalid ", kind, " code for method ",
                            linfo.def, ": ", e)
                else
                    println(STDERR, "WARNING: Encountered invalid ", kind, " code for top level expression in ",
                            linfo.def, ": ", e)
                end
            end
        end
    end
end

function get_staged(li::MethodInstance)
    try
        # user code might throw errors – ignore them
        return ccall(:jl_code_for_staged, Any, (Any,), li)::CodeInfo
    catch
        return nothing
    end
end

mutable struct OptimizationState
    linfo::MethodInstance
    vararg_type_container #::Type
    backedges::Vector{Any}
    src::CodeInfo
    mod::Module
    nargs::Int
    next_label::Int # index of the current highest label for this function
    min_valid::UInt
    max_valid::UInt
    params::InferenceParams
    function OptimizationState(frame::InferenceState)
        s_edges = frame.stmt_edges[1]
        if s_edges === ()
            s_edges = []
            frame.stmt_edges[1] = s_edges
        end
        next_label = label_counter(frame.src.code) + 1
        return new(frame.linfo, frame.vararg_type_container,
                   s_edges::Vector{Any},
                   frame.src, frame.mod, frame.nargs,
                   next_label, frame.min_valid, frame.max_valid,
                   frame.params)
    end
    function OptimizationState(linfo::MethodInstance, src::CodeInfo,
                               params::InferenceParams)
        # prepare src for running optimization passes
        # if it isn't already
        nssavalues = src.ssavaluetypes
        if nssavalues isa Int
            src.ssavaluetypes = Any[ Any for i = 1:nssavalues ]
        end
        if src.slottypes === nothing
            nslots = length(src.slotnames)
            src.slottypes = Any[ Any for i = 1:nslots ]
        end
        s_edges = []
        # cache some useful state computations
        toplevel = !isa(linfo.def, Method)
        if !toplevel
            meth = linfo.def
            inmodule = meth.module
            nargs = meth.nargs
        else
            inmodule = linfo.def::Module
            nargs = 0
        end
        next_label = label_counter(src.code) + 1
        vararg_type_container = nothing # if you want something more accurate, set it yourself :P
        return new(linfo, vararg_type_container,
                   s_edges::Vector{Any},
                   src, inmodule, nargs,
                   next_label,
                   min_world(linfo), max_world(linfo),
                   params)
    end
end

function OptimizationState(linfo::MethodInstance, params::InferenceParams)
    src = retrieve_code_info(linfo)
    src === nothing && return nothing
    return OptimizationState(linfo, src, params)
end


#### debugging utilities ####

function print_callstack(sv::InferenceState)
    while sv !== nothing
        print(sv.linfo)
        sv.limited && print("  [limited]")
        !sv.cached && print("  [uncached]")
        println()
        for cycle in sv.callers_in_cycle
            print(' ', cycle.linfo)
            cycle.limited && print("  [limited]")
            println()
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
        return get_staged(linfo)
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

# TODO: Use these functions instead of directly manipulating
# the "actual" method for appropriate places in inference (see #24676)
function method_for_inference_heuristics(cinfo, default)
    if isa(cinfo, CodeInfo)
        # appropriate format for `sig` is svec(ftype, argtypes, world)
        sig = cinfo.signature_for_inference_heuristics
        if isa(sig, SimpleVector) && length(sig) == 3
            methods = _methods(sig[1], sig[2], -1, sig[3])
            if length(methods) == 1
                _, _, m = methods[]
                if isa(m, Method)
                    return m
                end
            end
        end
    end
    return default
end

function method_for_inference_heuristics(method::Method, @nospecialize(sig), sparams, world)
    if isdefined(method, :generator) && method.generator.expand_early
        method_instance = code_for_method(method, sig, sparams, world, false)
        if isa(method_instance, MethodInstance)
            return method_for_inference_heuristics(get_staged(method_instance), method)
        end
    end
    return method
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

anymap(f::Function, a::Array{Any,1}) = Any[ f(a[i]) for i in 1:length(a) ]

_topmod(sv::OptimizationState) = _topmod(sv.mod)
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
function tupleparam_tail(t::SimpleVector, n)
    lt = length(t)
    if n > lt
        va = t[lt]
        if isvarargtype(va)
            # assumes that we should never see Vararg{T, x}, where x is a constant (should be guaranteed by construction)
            return Tuple{va}
        end
        return Tuple{}
    end
    return Tuple{t[n:lt]...}
end

function is_specializable_vararg_slot(@nospecialize(arg), sv::Union{InferenceState, OptimizationState})
    return (isa(arg, Slot) && slot_id(arg) == sv.nargs &&
            isa(sv.vararg_type_container, DataType))
end

function is_self_quoting(@nospecialize(x))
    return isa(x,Number) || isa(x,AbstractString) || isa(x,Tuple) || isa(x,Type) ||
        isa(x,Char) || x === nothing || isa(x,Function)
end

function quoted(@nospecialize(x))
    return is_self_quoting(x) ? x : QuoteNode(x)
end


#### type-functions for builtins / intrinsics ####

const _Type_name = Type.body.name
isType(@nospecialize t) = isa(t, DataType) && (t::DataType).name === _Type_name

const _NamedTuple_name = NamedTuple.body.body.name

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
const t_ifunc = Vector{Tuple{Int, Int, Any}}(uninitialized, n_ifunc)
const t_ifunc_cost = Vector{Int}(uninitialized, n_ifunc)
const t_ffunc_key = Vector{Any}()
const t_ffunc_val = Vector{Tuple{Int, Int, Any}}()
const t_ffunc_cost = Vector{Int}()
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
add_tfunc(Core.Intrinsics.llvmcall, 3, IInf,
          (@nospecialize(fptr), @nospecialize(rt), @nospecialize(at), a...) -> instanceof_tfunc(rt)[1], 10)
cglobal_tfunc(@nospecialize(fptr)) = Ptr{Cvoid}
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
            elseif a1.name === _NamedTuple_name
                if isleaftype(a1)
                    return Const(false)
                end
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
            if !(x.name === _NamedTuple_name && !isleaftype(x))
                return Const(length(x.types))
            end
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
# the outermost tuple type is permitted to have up to `allowed_tuplelen` parameters
function limit_type_size(@nospecialize(t), @nospecialize(compare), @nospecialize(source), allowed_tuplelen::Int)
    source = svec(unwrap_unionall(compare), unwrap_unionall(source))
    source[1] === source[2] && (source = svec(source[1]))
    type_more_complex(t, compare, source, 1, TUPLE_COMPLEXITY_LIMIT_DEPTH, allowed_tuplelen) || return t
    r = _limit_type_size(t, compare, source, 1, allowed_tuplelen)
    @assert t <: r
    #@assert r === _limit_type_size(r, t, source) # this monotonicity constraint is slightly stronger than actually required,
      # since we only actually need to demonstrate that repeated application would reaches a fixed point,
      #not that it is already at the fixed point
    return r
end

sym_isless(a::Symbol, b::Symbol) = ccall(:strcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}), a, b) < 0

function type_more_complex(@nospecialize(t), @nospecialize(c), sources::SimpleVector, depth::Int, tupledepth::Int, allowed_tuplelen::Int)
    # detect cases where the comparison is trivial
    if t === c
        return false
    elseif t === Union{}
        return false # Bottom is as simple as they come
    elseif isa(t, DataType) && isempty(t.parameters)
        return false # fastpath: unparameterized types are always finite
    elseif tupledepth > 0 && isa(unwrap_unionall(t), DataType) && isa(c, Type) && c !== Union{} && c <: t
        return false # t is already wider than the comparison in the type lattice
    elseif tupledepth > 0 && is_derived_type_from_any(unwrap_unionall(t), sources, depth)
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
        tupledepth = 1 # allow replacing a TypeVar with a concrete value (since we know the UnionAll must be in covariant position)
        if isa(t, TypeVar)
            return !(t.lb === Union{} || t.lb === c.lb) || # simplify lb towards Union{}
                   type_more_complex(t.ub, c.ub, sources, depth + 1, tupledepth, 0)
        end
        c.lb === Union{} || return true
        return type_more_complex(t, c.ub, sources, depth, tupledepth, 0)
    elseif isa(c, Union)
        if isa(t, Union)
            return type_more_complex(t.a, c.a, sources, depth, tupledepth, allowed_tuplelen) ||
                   type_more_complex(t.b, c.b, sources, depth, tupledepth, allowed_tuplelen)
        end
        return type_more_complex(t, c.a, sources, depth, tupledepth, allowed_tuplelen) &&
               type_more_complex(t, c.b, sources, depth, tupledepth, allowed_tuplelen)
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
                type_more_complex(tPi, cPi, sources, depth + 1, tupledepth, 0) && return true
            end
            return false
        elseif isvarargtype(c)
            return type_more_complex(t, unwrapva(c), sources, depth, tupledepth, 0)
        end
        if isType(t) # allow taking typeof any source type anywhere as Type{...}, as long as it isn't nesting Type{Type{...}}
            tt = unwrap_unionall(t.parameters[1])
            if isa(tt, DataType) && !isType(tt)
                is_derived_type_from_any(tt, sources, depth) || return true
                return false
            end
        end
    end
    return true
end

# try to find `type` somewhere in `comparison` type
# at a minimum nesting depth of `mindepth`
function is_derived_type(@nospecialize(t), @nospecialize(c), mindepth::Int)
    if mindepth > 0
        mindepth -= 1
    end
    if t === c
        return mindepth == 0
    end
    if isa(c, TypeVar)
        # see if it is replacing a TypeVar upper bound with something simpler
        return is_derived_type(t, c.ub, mindepth)
    elseif isa(c, Union)
        # see if it is one of the elements of the union
        return is_derived_type(t, c.a, mindepth + 1) || is_derived_type(t, c.b, mindepth + 1)
    elseif isa(c, UnionAll)
        # see if it is derived from the body
        return is_derived_type(t, c.body, mindepth)
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
            is_derived_type(t, p, mindepth) && return true
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
                is_derived_type(t, f, mindepth) && return true
            end
        end
    end
    return false
end

function is_derived_type_from_any(@nospecialize(t), sources::SimpleVector, mindepth::Int)
    for s in sources
        is_derived_type(t, s, mindepth) && return true
    end
    return false
end

# type vs. comparison or which was derived from source
function _limit_type_size(@nospecialize(t), @nospecialize(c), sources::SimpleVector, depth::Int, allowed_tuplelen::Int)
    if t === c
        return t # quick egal test
    elseif t === Union{}
        return t # easy case
    elseif isa(t, DataType) && isempty(t.parameters)
        return t # fast path: unparameterized are always simple
    elseif isa(unwrap_unionall(t), DataType) && isa(c, Type) && c !== Union{} && c <: t
        return t # t is already wider than the comparison in the type lattice
    elseif is_derived_type_from_any(unwrap_unionall(t), sources, depth)
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
            a = _limit_type_size(t.a, c.a, sources, depth, allowed_tuplelen)
            b = _limit_type_size(t.b, c.b, sources, depth, allowed_tuplelen)
            return Union{a, b}
        end
    elseif isa(t, UnionAll)
        if isa(c, UnionAll)
            tv = t.var
            cv = c.var
            if tv.ub === cv.ub
                if tv.lb === cv.lb
                    return UnionAll(tv, _limit_type_size(t.body, c.body, sources, depth + 1, allowed_tuplelen))
                end
                ub = tv.ub
            else
                ub = _limit_type_size(tv.ub, cv.ub, sources, depth + 1, 0)
            end
            if tv.lb === cv.lb
                lb = tv.lb
            else
                # note: lower bounds need to be widened by making them lower
                lb = Bottom
            end
            v2 = TypeVar(tv.name, lb, ub)
            return UnionAll(v2, _limit_type_size(t{v2}, c{v2}, sources, depth + 1, allowed_tuplelen))
        end
        tbody = _limit_type_size(t.body, c, sources, depth + 1, allowed_tuplelen)
        tbody === t.body && return t
        return UnionAll(t.var, tbody)
    elseif isa(c, UnionAll)
        # peel off non-matching wrapper of comparison
        return _limit_type_size(t, c.body, sources, depth, allowed_tuplelen)
    elseif isa(t, DataType)
        if isa(c, DataType)
            tP = t.parameters
            cP = c.parameters
            if t.name === c.name && !isempty(cP)
                if isvarargtype(t)
                    VaT = _limit_type_size(tP[1], cP[1], sources, depth + 1, 0)
                    N = tP[2]
                    if isa(N, TypeVar) || N === cP[2]
                        return Vararg{VaT, N}
                    end
                    return Vararg{VaT}
                elseif t.name === Tuple.name
                    # for covariant datatypes (Tuple),
                    # apply type-size limit element-wise
                    ltP = length(tP)
                    lcP = length(cP)
                    np = min(ltP, max(lcP, allowed_tuplelen))
                    Q = Any[ tP[i] for i in 1:np ]
                    if ltP > np
                        # combine tp[np:end] into tP[np] using Vararg
                        Q[np] = tuple_tail_elem(Bottom, Any[ tP[i] for i in np:ltP ])
                    end
                    for i = 1:np
                        # now apply limit element-wise to Q
                        # padding out the comparison as needed to allowed_tuplelen elements
                        if i <= lcP
                            cPi = cP[i]
                        elseif isvarargtype(cP[lcP])
                            cPi = cP[lcP]
                        else
                            cPi = Any
                        end
                        Q[i] = _limit_type_size(Q[i], cPi, sources, depth + 1, 0)
                    end
                    return Tuple{Q...}
                end
            elseif isvarargtype(c)
                # Tuple{Vararg{T}} --> Tuple{T} is OK
                return _limit_type_size(t, cP[1], sources, depth, 0)
            end
        end
        if isType(t) # allow taking typeof as Type{...}, but ensure it doesn't start nesting
            tt = unwrap_unionall(t.parameters[1])
            if isa(tt, DataType) && !isType(tt)
                is_derived_type_from_any(tt, sources, depth) && return t
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
const DataType_mutable_fieldindex = fieldindex(DataType, :mutable)

const TypeName_name_fieldindex = fieldindex(TypeName, :name)
const TypeName_module_fieldindex = fieldindex(TypeName, :module)
const TypeName_wrapper_fieldindex = fieldindex(TypeName, :wrapper)

function const_datatype_getfield_tfunc(sv, fld)
    if (fld == DataType_name_fieldindex ||
            fld == DataType_parameters_fieldindex ||
            fld == DataType_types_fieldindex ||
            fld == DataType_super_fieldindex ||
            fld == DataType_mutable_fieldindex)
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
    if s.name === _NamedTuple_name && !isleaftype(s)
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
    if u.name === _NamedTuple_name && !isleaftype(u)
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
                           sv::Union{InferenceState,Nothing}, params::InferenceParams = sv.params)
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
        if is_pure_intrinsic_infer(f) && all(a -> isa(a, Const), argtypes)
            argvals = anymap(a -> a.val, argtypes)
            try
                return Const(f(argvals...))
            end
        end
        iidx = Int(reinterpret(Int32, f::IntrinsicFunction)) + 1
        if iidx < 0 || iidx > length(t_ifunc)
            # invalid intrinsic
            return Any
        end
        tf = t_ifunc[iidx]
    else
        fidx = findfirst(x->x===f, t_ffunc_key)
        if fidx === nothing
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

function abstract_call_gf_by_type(@nospecialize(f), argtypes::Vector{Any}, @nospecialize(atype), sv::InferenceState)
    atype = limit_tuple_type(atype, sv.params)
    atype_params = unwrap_unionall(atype).parameters
    ft = unwrap_unionall(atype_params[1]) # TODO: ccall jl_first_argument_datatype here
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
    splitunions = 1 < countunionsplit(atype_params) <= sv.params.MAX_UNION_SPLITTING
    if splitunions
        splitsigs = switchtupleunion(atype)
        applicable = Any[]
        for sig_n in splitsigs
            xapplicable = _methods_by_ftype(sig_n, sv.params.MAX_METHODS, sv.params.world, min_valid, max_valid)
            xapplicable === false && return Any
            append!(applicable, xapplicable)
        end
    else
        applicable = _methods_by_ftype(atype, sv.params.MAX_METHODS, sv.params.world, min_valid, max_valid)
        if applicable === false
            # this means too many methods matched
            # (assume this will always be true, so we don't compute / update valid age in this case)
            return Any
        end
    end
    update_valid_age!(min_valid[1], max_valid[1], sv)
    applicable = applicable::Array{Any,1}
    napplicable = length(applicable)
    rettype = Bottom
    edgecycle = false
    for i in 1:napplicable
        match = applicable[i]::SimpleVector
        method = match[3]::Method
        sig = match[1]
        sigtuple = unwrap_unionall(sig)::DataType
        splitunions = false
        # TODO: splitunions = 1 < countunionsplit(sigtuple.parameters) * napplicable <= sv.params.MAX_UNION_SPLITTING
        # currently this triggers a bug in inference recursion detection
        if splitunions
            splitsigs = switchtupleunion(sig)
            for sig_n in splitsigs
                rt, edgecycle1 = abstract_call_method(method, sig_n, svec(), sv)
                edgecycle |= edgecycle1::Bool
                rettype = tmerge(rettype, rt)
                rettype === Any && break
            end
            rettype === Any && break
        else
            rt, edgecycle = abstract_call_method(method, sig, match[2]::SimpleVector, sv)
            rettype = tmerge(rettype, rt)
            rettype === Any && break
        end
    end
    if napplicable == 1 && !edgecycle && isa(rettype, Type) && sv.params.ipo_constant_propagation
        # if there's a possibility we could constant-propagate a better result
        # (hopefully without doing too much work), try to do that now
        # TODO: it feels like this could be better integrated into abstract_call_method / typeinf_edge
        const_rettype = abstract_call_method_with_const_args(f, argtypes, applicable[1]::SimpleVector, sv)
        if const_rettype ⊑ rettype
            # use the better result, if it's a refinement of rettype
            rettype = const_rettype
        end
    end
    if !(rettype === Any) # adding a new method couldn't refine (widen) this type
        fullmatch = false
        for i in napplicable:-1:1
            match = applicable[i]::SimpleVector
            method = match[3]::Method
            if atype <: method.sig
                fullmatch = true
                break
            end
        end
        if !fullmatch
            # also need an edge to the method table in case something gets
            # added that did not intersect with any existing method
            add_mt_backedge(ftname.mt, atype, sv)
        end
    end
    #print("=> ", rettype, "\n")
    return rettype
end

function cache_lookup(code::MethodInstance, argtypes::Vector{Any}, cache::Vector{InferenceResult})
    method = code.def::Method
    nargs::Int = method.nargs
    method.isva && (nargs -= 1)
    for cache_code in cache
        # try to search cache first
        cache_args = cache_code.args
        if cache_code.linfo === code && length(cache_args) >= nargs
            cache_match = true
            # verify that the trailing args (va) aren't Const
            for i in (nargs + 1):length(cache_args)
                if isa(cache_args[i], Const)
                    cache_match = false
                    break
                end
            end
            cache_match || continue
            for i in 1:nargs
                a = argtypes[i]
                ca = cache_args[i]
                # verify that all Const argument types match between the call and cache
                if (isa(a, Const) || isa(ca, Const)) && !(a === ca)
                    cache_match = false
                    break
                end
            end
            cache_match || continue
            return cache_code
        end
    end
    return nothing
end

function abstract_call_method_with_const_args(@nospecialize(f), argtypes::Vector{Any}, match::SimpleVector, sv::InferenceState)
    method = match[3]::Method
    nargs::Int = method.nargs
    method.isva && (nargs -= 1)
    length(argtypes) >= nargs || return Any # probably limit_tuple_type made this non-matching method apparently match
    haveconst = false
    for i in 1:nargs
        a = argtypes[i]
        if isa(a, Const) && !isdefined(typeof(a.val), :instance)
            if !isleaftype(a.val) # alternately: !isa(a.val, DataType) || !isconstType(Type{a.val})
                # have new information from argtypes that wasn't available from the signature
                haveconst = true
                break
            end
        end
    end
    haveconst || return Any
    sig = match[1]
    sparams = match[2]::SimpleVector
    code = code_for_method(method, sig, sparams, sv.params.world)
    code === nothing && return Any
    code = code::MethodInstance
    # decide if it's likely to be worthwhile
    cache_inlineable = false
    if isdefined(code, :inferred)
        cache_inf = code.inferred
        if !(cache_inf === nothing)
            cache_src_inferred = ccall(:jl_ast_flag_inferred, Bool, (Any,), cache_inf)
            cache_src_inlineable = ccall(:jl_ast_flag_inlineable, Bool, (Any,), cache_inf)
            cache_inlineable = cache_src_inferred && cache_src_inlineable
        end
    end
    if !cache_inlineable && !sv.params.aggressive_constant_propagation
        tm = _topmod(sv)
        if !istopfunction(tm, f, :getproperty) && !istopfunction(tm, f, :setproperty!)
            # in this case, see if all of the arguments are constants
            for i in 1:nargs
                a = argtypes[i]
                if !isa(a, Const) && !isconstType(a)
                    return Any
                end
            end
        end
    end
    inf_result = cache_lookup(code, argtypes, sv.params.cache)
    if inf_result === nothing
        inf_result = InferenceResult(code)
        atypes = get_argtypes(inf_result)
        for i in 1:nargs
            a = argtypes[i]
            if a isa Const
                atypes[i] = a # inject Const argtypes into inference
            end
        end
        frame = InferenceState(inf_result, #=optimize=#true, #=cache=#false, sv.params)
        frame.limited = true
        frame.parent = sv
        push!(sv.params.cache, inf_result)
        typeinf(frame)
    end
    result = inf_result.result
    isa(result, InferenceState) && return Any # TODO: is this recursive constant inference?
    add_backedge!(inf_result.linfo, sv)
    return result
end

const deprecated_sym = Symbol("deprecated.jl")

function abstract_call_method(method::Method, @nospecialize(sig), sparams::SimpleVector, sv::InferenceState)
    # TODO: remove with 0.7 deprecations
    if method.file === deprecated_sym && method.sig == (Tuple{Type{T},Any} where T)
        return Any, false
    end
    topmost = nothing
    # Limit argument type tuple growth of functions:
    # look through the parents list to see if there's a call to the same method
    # and from the same method.
    # Returns the topmost occurrence of that repeated edge.
    cyclei = 0
    infstate = sv
    edgecycle = false
    while !(infstate === nothing)
        infstate = infstate::InferenceState
        if method === infstate.linfo.def
            if infstate.linfo.specTypes == sig
                # avoid widening when detecting self-recursion
                # TODO: merge call cycle and return right away
                topmost = nothing
                edgecycle = true
                break
            end
            if topmost === nothing
                # inspect the parent of this edge,
                # to see if they are the same Method as sv
                # in which case we'll need to ensure it is convergent
                # otherwise, we don't
                for parent in infstate.callers_in_cycle
                    # check in the cycle list first
                    # all items in here are mutual parents of all others
                    if parent.linfo.def === sv.linfo.def
                        topmost = infstate
                        edgecycle = true
                        break
                    end
                end
                let parent = infstate.parent
                    # then check the parent link
                    if topmost === nothing && parent !== nothing
                        parent = parent::InferenceState
                        if parent.cached && parent.linfo.def === sv.linfo.def
                            topmost = infstate
                            edgecycle = true
                        end
                    end
                end
            end
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

    if !(topmost === nothing)
        topmost = topmost::InferenceState
        sigtuple = unwrap_unionall(sig)::DataType
        msig = unwrap_unionall(method.sig)::DataType
        spec_len = length(msig.parameters) + 1
        ls = length(sigtuple.parameters)
        if method === sv.linfo.def
            # Under direct self-recursion, permit much greater use of reducers.
            # here we assume that complexity(specTypes) :>= complexity(sig)
            comparison = sv.linfo.specTypes
            l_comparison = length(unwrap_unionall(comparison).parameters)
            spec_len = max(spec_len, l_comparison)
        else
            comparison = method.sig
        end
        # see if the type is actually too big (relative to the caller), and limit it if required
        newsig = limit_type_size(sig, comparison, sv.linfo.specTypes, spec_len)

        if newsig !== sig
            # continue inference, but note that we've limited parameter complexity
            # on this call (to ensure convergence), so that we don't cache this result
            infstate = sv
            topmost = topmost::InferenceState
            while !(infstate.parent === topmost.parent)
                infstate.limited = true
                for infstate_cycle in infstate.callers_in_cycle
                    infstate_cycle.limited = true
                end
                infstate = infstate.parent
            end
            sig = newsig
            sparams = svec()
        end
    end

    # if sig changed, may need to recompute the sparams environment
    if isa(method.sig, UnionAll) && isempty(sparams)
        recomputed = ccall(:jl_type_intersection_with_env, Any, (Any, Any), sig, method.sig)::SimpleVector
        sig = recomputed[1]
        if !isa(unwrap_unionall(sig), DataType) # probably Union{}
            return Any, false
        end
        sparams = recomputed[2]::SimpleVector
    end

    rt, edge = typeinf_edge(method, sig, sparams, sv)
    if edge === nothing
        edgecycle = true
    else
        add_backedge!(edge::MethodInstance, sv)
    end
    return rt, edgecycle
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
            rt = abstract_call_gf_by_type(nothing, ct, astype, sv)
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
function return_type_tfunc(argtypes::Vector{Any}, vtypes::VarTable, sv::InferenceState)
    if length(argtypes) == 3
        tt = argtypes[3]
        if isa(tt, Const) || (isType(tt) && !has_free_typevars(tt))
            aft = argtypes[2]
            if isa(aft, Const) || (isType(aft) && !has_free_typevars(aft)) ||
                   (isleaftype(aft) && !(aft <: Builtin))
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

function pure_eval_call(@nospecialize(f), argtypes::Vector{Any}, @nospecialize(atype), sv::InferenceState)
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
            elseif f === Core.Inference.not_int
                aty = argtypes[2]
                if isa(aty, Conditional)
                    return Conditional(aty.var, aty.elsetype, aty.vtype)
                end
            end
        end
        return isa(rt, TypeVar) ? rt.ub : rt
    elseif f === Core.kwfunc
        if length(argtypes) == 2
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
        if length(argtypes) >= 2 && isa(argtypes[2], Const)
            nv = argtypes[2].val
            ubidx = 3
            if length(argtypes) >= 4
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
            if length(argtypes) >= ubidx
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
        if length(argtypes) == 3
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
    elseif length(argtypes) == 2 && istopfunction(tm, f, :!)
        # handle Conditional propagation through !Bool
        aty = argtypes[2]
        if isa(aty, Conditional)
            abstract_call_gf_by_type(f, Any[Const(f), Bool], Tuple{typeof(f), Bool}, sv) # make sure we've inferred `!(::Bool)`
            return Conditional(aty.var, aty.elsetype, aty.vtype)
        end
    elseif length(argtypes) == 3 && istopfunction(tm, f, :!==)
        # mark !== as exactly a negated call to ===
        rty = abstract_call((===), fargs, argtypes, vtypes, sv)
        if isa(rty, Conditional)
            return Conditional(rty.var, rty.elsetype, rty.vtype) # swap if-else
        elseif isa(rty, Const)
            return Const(rty.val === false)
        end
        return rty
    elseif length(argtypes) == 3 && istopfunction(tm, f, :(>:))
        # mark issupertype as a exact alias for issubtype
        # swap T1 and T2 arguments and call <:
        if length(fargs) == 3
            fargs = Any[<:, fargs[3], fargs[2]]
        else
            fargs = ()
        end
        argtypes = Any[typeof(<:), argtypes[3], argtypes[2]]
        rty = abstract_call(<:, fargs, argtypes, vtypes, sv)
        return rty
    elseif length(argtypes) == 2 && isa(argtypes[2], Const) && isa(argtypes[2].val, SimpleVector) && istopfunction(tm, f, :length)
        # mark length(::SimpleVector) as @pure
        return Const(length(argtypes[2].val))
    elseif length(argtypes) == 3 && isa(argtypes[2], Const) && isa(argtypes[3], Const) &&
            isa(argtypes[2].val, SimpleVector) && isa(argtypes[3].val, Int) && istopfunction(tm, f, :getindex)
        # mark getindex(::SimpleVector, i::Int) as @pure
        svecval = argtypes[2].val::SimpleVector
        idx = argtypes[3].val::Int
        if 1 <= idx <= length(svecval) && isassigned(svecval, idx)
            return Const(getindex(svecval, idx))
        end
    elseif length(argtypes) == 2 && istopfunction(tm, f, :typename)
        return typename_static(argtypes[2])
    end

    atype = argtypes_to_type(argtypes)
    t = pure_eval_call(f, argtypes, atype, sv)
    t !== false && return t

    if istopfunction(tm, f, :typejoin) || f === return_type
        return Type # don't try to infer these function edges directly -- it won't actually come up with anything useful
    end

    if sv.params.inlining
        # need to model the special inliner for ^
        # to ensure we have added the same edge
        if isdefined(Main, :Base) &&
            ((isdefined(Main.Base, :^) && f === Main.Base.:^) ||
             (isdefined(Main.Base, :.^) && f === Main.Base.:.^)) &&
            length(argtypes) == 3 && (argtypes[3] ⊑ Int32 || argtypes[3] ⊑ Int64)

            a1 = argtypes[2]
            basenumtype = Union{corenumtype, Main.Base.ComplexF32, Main.Base.ComplexF64, Main.Base.Rational}
            if a1 ⊑ basenumtype
                ftimes = Main.Base.:*
                ta1 = widenconst(a1)
                abstract_call_gf_by_type(ftimes, Any[ftimes, a1, a1], Tuple{typeof(ftimes), ta1, ta1}, sv)
            end
        end
    end
    return abstract_call_gf_by_type(f, argtypes, atype, sv)
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
                return abstract_call_gf_by_type(nothing, argtypes, argtypes_to_type(argtypes), sv)
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
                    env = pointer_from_objref(sv.linfo.sparam_vals) + sizeof(Ptr{Cvoid})
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
        t = (length(e.args) == 1) ? Any : Nothing
    elseif e.head === :copyast
        t = abstract_eval(e.args[1], vtypes, sv)
        if t isa Const && t.val isa Expr
            # `copyast` makes copies of Exprs
            t = Expr
        end
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
    e.typ = t
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
    (head === :inbounds || head === :boundscheck || head === :meta || head === :simdloop)
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

# scan body for the value of the largest referenced label
function label_counter(body::Vector{Any})
    l = 0
    for b in body
        label = 0
        if isa(b, GotoNode)
            label = b.label::Int
        elseif isa(b, LabelNode)
            label = b.label
        elseif isa(b, Expr) && b.head == :gotoifnot
            label = b.args[2]::Int
        elseif isa(b, Expr) && b.head == :enter
            label = b.args[1]::Int
        end
        if label > l
            l = label
        end
    end
    return l
end
genlabel(sv::OptimizationState) = LabelNode(sv.next_label += 1)

function get_label_map(body::Vector{Any})
    nlabels = label_counter(body)
    labelmap = zeros(Int, nlabels)
    for i = 1:length(body)
        el = body[i]
        if isa(el, LabelNode)
            labelmap[el.label] = i
        end
    end
    return labelmap
end


function find_ssavalue_uses(body::Vector{Any}, nvals::Int)
    uses = BitSet[ BitSet() for i = 1:nvals ]
    for line in 1:length(body)
        e = body[line]
        isa(e, Expr) && find_ssavalue_uses(e, uses, line)
    end
    return uses
end

function find_ssavalue_uses(e::Expr, uses::Vector{BitSet}, line::Int)
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

function newvar!(sv::OptimizationState, @nospecialize(typ))
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
    @assert(!isa(sv.linfo.def, Method) ||
            !sv.cached ||
            sv.min_valid <= sv.params.world <= sv.max_valid,
            "invalid age range update")
    nothing
end
function update_valid_age!(min_valid::UInt, max_valid::UInt, sv::OptimizationState)
    sv.min_valid = max(sv.min_valid, min_valid)
    sv.max_valid = min(sv.max_valid, max_valid)
    @assert(!isa(sv.linfo.def, Method) ||
            (sv.min_valid == typemax(UInt) && sv.max_valid == typemin(UInt)) ||
            sv.min_valid <= sv.params.world <= sv.max_valid,
            "invalid age range update")
    nothing
end
update_valid_age!(edge::InferenceState, sv::InferenceState) = update_valid_age!(edge.min_valid, edge.max_valid, sv)
update_valid_age!(li::MethodInstance, sv::InferenceState) = update_valid_age!(min_world(li), max_world(li), sv)
update_valid_age!(li::MethodInstance, sv::OptimizationState) = update_valid_age!(min_world(li), max_world(li), sv)

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

function add_backedge!(li::MethodInstance, caller::OptimizationState)
    isa(caller.linfo.def, Method) || return # don't add backedges to toplevel exprs
    push!(caller.backedges, li)
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
                    ccall(:jl_method_instance_add_backedge, Cvoid, (Any, Any), to, caller)
                    i += 1
                else
                    typeassert(to, MethodTable)
                    typ = edges[i + 1]
                    ccall(:jl_method_table_add_backedge, Cvoid, (Any, Any, Any), to, typ, caller)
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
    uncached = false
    while isa(frame, InferenceState)
        uncached |= !frame.cached # ensure we never add an uncached frame to a cycle
        if frame.linfo === linfo
            uncached && return true
            merge_call_chain!(parent, frame, frame)
            return frame
        end
        for caller in frame.callers_in_cycle
            if caller.linfo === linfo
                uncached && return true
                merge_call_chain!(parent, frame, caller)
                return caller
            end
        end
        frame = frame.parent
    end
    return false
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
    if !caller.cached && caller.parent === nothing
        # this caller exists to return to the user
        # (if we asked resolve_call_cyle, it might instead detect that there is a cycle that it can't merge)
        frame = false
    else
        frame = resolve_call_cycle!(code, caller)
    end
    if frame === false
        # completely new
        code.inInference = true
        frame = InferenceState(code, #=optimize=#true, #=cached=#true, caller.params) # always optimize and cache edge targets
        if frame === nothing
            # can't get the source for this, so we know nothing
            code.inInference = false
            return Any, nothing
        end
        if caller.cached # don't involve uncached functions in cycle resolution
            frame.parent = caller
        end
        typeinf(frame)
        return frame.bestguess, frame.inferred ? frame.linfo : nothing
    elseif frame === true
        # unresolvable cycle
        return Any, nothing
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
        i == 2 && ccall(:jl_typeinf_begin, Cvoid, ())
        if cached && isdefined(linfo, :inferred)
            # see if this code already exists in the cache
            # staged functions make this hard since they have two "inferred" conditions,
            # so need to check whether the code itself is also inferred
            if min_world(linfo) <= params.world <= max_world(linfo)
                inf = linfo.inferred
                if linfo.jlcall_api == 2
                    method = linfo.def::Method
                    tree = ccall(:jl_new_code_info_uninit, Ref{CodeInfo}, ())
                    tree.code = Any[ Expr(:return, quoted(linfo.inferred_const)) ]
                    tree.signature_for_inference_heuristics = nothing
                    tree.slotnames = Any[ compiler_temp_sym for i = 1:method.nargs ]
                    tree.slotflags = UInt8[ 0 for i = 1:method.nargs ]
                    tree.slottypes = nothing
                    tree.ssavaluetypes = 0
                    tree.inferred = true
                    tree.pure = true
                    tree.inlineable = true
                    i == 2 && ccall(:jl_typeinf_end, Cvoid, ())
                    return svec(linfo, tree, linfo.rettype)
                elseif isa(inf, CodeInfo)
                    if inf.inferred
                        i == 2 && ccall(:jl_typeinf_end, Cvoid, ())
                        return svec(linfo, inf, linfo.rettype)
                    end
                end
            end
        end
    end
    frame = typeinf_frame(linfo, optimize, cached, params)
    ccall(:jl_typeinf_end, Cvoid, ())
    frame === nothing && return svec(nothing, nothing, Any)
    frame = frame::InferenceState
    frame.inferred || return svec(nothing, nothing, Any)
    frame.cached || return svec(nothing, frame.src, widenconst(frame.bestguess))
    return svec(frame.linfo, frame.src, widenconst(frame.bestguess))
end

# compute (and cache) an inferred AST and return the inferred return type
function typeinf_type(method::Method, @nospecialize(atypes), sparams::SimpleVector,
                      cached::Bool, params::InferenceParams)
    if contains_is(unwrap_unionall(atypes).parameters, Union{})
        return Union{}
    end
    code = code_for_method(method, atypes, sparams, params.world)
    code === nothing && return nothing
    code = code::MethodInstance
    for i = 1:2 # test-and-lock-and-test
        i == 2 && ccall(:jl_typeinf_begin, Cvoid, ())
        if cached && isdefined(code, :inferred)
            # see if this rettype already exists in the cache
            # staged functions make this hard since they have two "inferred" conditions,
            # so need to check whether the code itself is also inferred
            inf = code.inferred
            if !isa(inf, CodeInfo) || (inf::CodeInfo).inferred
                i == 2 && ccall(:jl_typeinf_end, Cvoid, ())
                return code.rettype
            end
        end
    end
    frame = typeinf_frame(code, cached, cached, params)
    ccall(:jl_typeinf_end, Cvoid, ())
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
        ccall(:jl_typeinf_begin, Cvoid, ())
        result = InferenceResult(linfo)
        frame = InferenceState(result, linfo.inferred::CodeInfo,
                               true, true, InferenceParams(world))
        typeinf(frame)
        ccall(:jl_typeinf_end, Cvoid, ())
        @assert frame.inferred # TODO: deal with this better
        @assert frame.linfo === linfo
        linfo.rettype = widenconst(frame.bestguess)
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
                    if !isa(rt, Const) && !isa(rt, Type)
                        # only propagate information we know we can store
                        # and is valid inter-procedurally
                        rt = widenconst(rt)
                    end
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
        # complete the computation of the src optimizations
        for caller in frame.callers_in_cycle
            optimize(caller)
            if frame.min_valid < caller.min_valid
                frame.min_valid = caller.min_valid
            end
            if frame.max_valid > caller.max_valid
                frame.max_valid = caller.max_valid
            end
        end
        # update and store in the global cache
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
    if me.limited && me.cached && me.parent !== nothing
        # a top parent will be cached still, but not this intermediate work
        me.cached = false
        me.linfo.inInference = false
    elseif me.optimize
        opt = OptimizationState(me)
        # This pass is required for the AST to be valid in codegen
        # if any `SSAValue` is created by type inference. Ref issue #6068
        # This (and `reindex_labels!`) needs to be run for `!me.optimize`
        # if we start to create `SSAValue` in type inference when not
        # optimizing and use unoptimized IR in codegen.
        gotoifnot_elim_pass!(opt)
        inlining_pass!(opt, opt.src.propagate_inbounds)
        # Clean up after inlining
        gotoifnot_elim_pass!(opt)
        basic_dce_pass!(opt)
        void_use_elim_pass!(opt)
        copy_duplicated_expr_pass!(opt)
        split_undef_flag_pass!(opt)
        fold_constant_getfield_pass!(opt)
        # Compute escape information
        # and elide unnecessary allocations
        alloc_elim_pass!(opt)
        # Clean up for `alloc_elim_pass!`
        gotoifnot_elim_pass!(opt)
        basic_dce_pass!(opt)
        void_use_elim_pass!(opt)
        # Pop metadata before label reindexing
        let code = opt.src.code::Array{Any,1}
            meta_elim_pass!(code, coverage_enabled())
            filter!(x -> x !== nothing, code)
            force_noinline = popmeta!(code, :noinline)[1]
        end
        reindex_labels!(opt)
        me.min_valid = opt.min_valid
        me.max_valid = opt.max_valid
    end

    # convert all type information into the form consumed by the code-generator
    widen_all_consts!(me.src)

    # compute inlining and other related properties
    me.const_ret = (isa(me.bestguess, Const) || isconstType(me.bestguess))
    if me.const_ret
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
    if me.optimize && !(me.limited && me.parent !== nothing)
        _validate(me.linfo, me.src, "optimized")
    end
    nothing
end

# inference completed on `me`
# update the MethodInstance and notify the edges
function finish(me::InferenceState)
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

        # don't store inferred code if we've decided to interpret this function
        if !already_inferred && me.linfo.jlcall_api != 4
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
                    keeptree = me.optimize &&
                        (me.src.inlineable ||
                         ccall(:jl_is_cacheable_sig, Int32, (Any, Any, Any), me.linfo.specTypes, def.sig, def) != 0)
                    if keeptree
                        # compress code for non-toplevel thunks
                        inferred_result = ccall(:jl_compress_ast, Any, (Any, Any), def, inferred_result)
                    else
                        inferred_result = nothing
                    end
                end
            end
            cache = ccall(:jl_set_method_inferred, Ref{MethodInstance}, (Any, Any, Any, Any, Int32, UInt, UInt),
                me.linfo, widenconst(me.bestguess), inferred_const, inferred_result,
                const_flags, min_valid, max_valid)
            if cache !== me.linfo
                me.linfo.inInference = false
                me.linfo = cache
                me.result.linfo = cache
            end
        end
        me.linfo.inInference = false
    end

    # finish updating the result struct
    if me.src.inlineable
        me.result.src = me.src # stash a copy of the code (for inlining)
    end
    me.result.result = me.bestguess # record type, and that wip is done and me.linfo can be used as a backedge

    # update all of the callers with real backedges by traversing the temporary list of backedges
    for (i, _) in me.backedges
        add_backedge!(me.linfo, i)
    end

    # finalize and record the linfo result
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
                if st_i[id].undef
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
            # will clean up by replacing them with the conditions later.
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
            src.slotflags[j] |= Slot_UsedUndef | Slot_StaticUndef
        end
    end

    # The dead code elimination can delete the target of a reachable node. This
    # must mean that the target is unreachable. Later optimization passes will
    # assume that all branches lead to labels that exist, so we must replace
    # the node with the branch condition (which may have side effects).
    labelmap = get_label_map(body)
    for i in 1:length(body)
        expr = body[i]
        if isa(expr, Expr) && expr.head === :gotoifnot
            labelnum = labelmap[expr.args[2]::Int]
            if labelnum === 0
                body[i] = expr.args[1]
            end
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
                if slottypes[x.id] ⊑ vt
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
    for i = 1:length(src.slottypes)
        src.slottypes[i] = widenconst(src.slottypes[i])
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
            return quoted(spvals[e.args[1]])
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

# whether `f` is pure for Inference
function is_pure_intrinsic_infer(f::IntrinsicFunction)
    return !(f === Intrinsics.pointerref || # this one is volatile
             f === Intrinsics.pointerset || # this one is never effect-free
             f === Intrinsics.llvmcall ||   # this one is never effect-free
             f === Intrinsics.arraylen ||   # this one is volatile
             f === Intrinsics.sqrt_llvm ||  # this one may differ at runtime (by a few ulps)
             f === Intrinsics.cglobal)  # cglobal lookup answer changes at runtime
end

# whether `f` is pure for Optimizations
function is_pure_intrinsic_optim(f::IntrinsicFunction)
    return !(f === Intrinsics.pointerref || # this one is volatile
             f === Intrinsics.pointerset || # this one is never effect-free
             f === Intrinsics.llvmcall ||   # this one is never effect-free
             f === Intrinsics.arraylen ||   # this one is volatile
             f === Intrinsics.checked_sdiv_int ||  # these may throw errors
             f === Intrinsics.checked_udiv_int ||
             f === Intrinsics.checked_srem_int ||
             f === Intrinsics.checked_urem_int ||
             f === Intrinsics.cglobal)  # cglobal throws an error for symbol-not-found
end

function is_pure_builtin(@nospecialize(f))
    if isa(f, IntrinsicFunction)
        return is_pure_intrinsic_optim(f)
    elseif isa(f, Builtin)
        return (contains_is(_pure_builtins, f) ||
                contains_is(_pure_builtins_volatile, f))
    else
        return f === return_type
    end
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
            return (isa(e.typ, DataType) && isleaftype(e.typ)) || isa(e.typ, Const)
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
                        (2 < nargs < 5) || return false
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
        elseif head === :isdefined
            return allow_volatile
        elseif head === :the_exception
            return allow_volatile
        elseif head === :copyast
            return true
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

function inline_as_constant(@nospecialize(val), argexprs::Vector{Any}, sv::OptimizationState, @nospecialize(invoke_data))
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
    return (quoted(val), stmts)
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

function get_spec_lambda(@nospecialize(atypes), sv::OptimizationState, @nospecialize(invoke_data))
    if invoke_data === nothing
        return ccall(:jl_get_spec_lambda, Any, (Any, UInt), atypes, sv.params.world)
    else
        invoke_data = invoke_data::InvokeData
        atypes <: invoke_data.types0 || return nothing
        return ccall(:jl_get_invoke_lambda, Any, (Any, Any, Any, UInt),
                     invoke_data.mt, invoke_data.entry, atypes, sv.params.world)
    end
end

function invoke_NF(argexprs, @nospecialize(etype), atypes::Vector{Any}, sv::OptimizationState,
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
        stmts = []

        spec_miss = nothing
        error_label = nothing
        ex = Expr(:call)
        ex.typ = etype
        ex.args = copy(argexprs)
        invoke_texpr === nothing || insert!(stmts, 2, invoke_texpr)
        invoke_fexpr === nothing || pushfirst!(stmts, invoke_fexpr)

        local ret_var, merge, invoke_ex, spec_hit
        ret_var = add_slot!(sv.src, widenconst(etype), false)
        merge = genlabel(sv)
        invoke_ex = copy(ex)
        invoke_ex.head = :invoke
        pushfirst!(invoke_ex.args, nothing)
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
                            isa_var = newvar!(sv, Bool)
                            isa_ty = Expr(:call, GlobalRef(Core, :isa), aei, ty)
                            isa_ty.typ = Bool
                            pushfirst!(match, Expr(:gotoifnot, isa_var, after.label))
                            pushfirst!(match, Expr(:(=), isa_var, isa_ty))
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
        pushfirst!(argexprs, cache_linfo)
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
                    sv::OptimizationState)
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
                  (isa(f, IntrinsicFunction) && is_pure_intrinsic_optim(f)))))
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
        invoke_tt = invoke_tt.parameters[1]
        invoke_types = rewrap_unionall(Tuple{ft, unwrap_unionall(invoke_tt).parameters...}, invoke_tt)
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
        pushfirst!(atypes, atype0)
        pushfirst!(argexprs, argexpr0)
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
        is_var = newvar!(sv, Bool)
        stmts = Any[ Expr(:(=), is_var, Expr(:call, GlobalRef(Core, :(===)), argexprs[2], argexprs[3])) ]
        stmts[1].args[2].typ = Bool
        not_is = Expr(:call, GlobalRef(Core.Intrinsics, :not_int), is_var)
        not_is.typ = Bool
        return (not_is, stmts)
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

    # see if the method has been previously inferred (and cached)
    linfo = code_for_method(method, metharg, methsp, sv.params.world, true) # Union{Nothing, MethodInstance}
    isa(linfo, MethodInstance) || return invoke_NF(argexprs0, e.typ, atypes0, sv,
                                                   atype_unlimited, invoke_data)
    linfo = linfo::MethodInstance
    if linfo.jlcall_api == 2
        # in this case function can be inlined to a constant
        add_backedge!(linfo, sv)
        return inline_as_constant(linfo.inferred_const, argexprs, sv, invoke_data)
    end

    # see if the method has a InferenceResult in the current cache
    # or an existing inferred code info store in `.inferred`
    haveconst = false
    for i in 1:length(atypes)
        a = atypes[i]
        if isa(a, Const) && !isdefined(typeof(a.val), :instance)
            if !isleaftype(a.val) # alternately: !isa(a.val, DataType) || !isconstType(Type{a.val})
                # have new information from argtypes that wasn't available from the signature
                haveconst = true
                break
            end
        end
    end
    if haveconst
        inf_result = cache_lookup(linfo, atypes, sv.params.cache) # Union{Nothing, InferenceResult}
    else
        inf_result = nothing
    end
    if isa(inf_result, InferenceResult) && isa(inf_result.src, CodeInfo)
        linfo = inf_result.linfo
        result = inf_result.result
        if (inf_result.src::CodeInfo).pure
            if isa(result, Const)
                inferred_const = result.val
            elseif isconstType(result)
                inferred_const = result.parameters[1]
            end
            if @isdefined inferred_const
                add_backedge!(linfo, sv)
                return inline_as_constant(inferred_const, argexprs, sv, invoke_data)
            end
        end
        inferred = inf_result.src
        rettype = widenconst(result)
    elseif isdefined(linfo, :inferred)
        inferred = linfo.inferred
        rettype = linfo.rettype
    else
        rettype = Any
        inferred = nothing
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

    # create the backedge
    add_backedge!(linfo, sv)

    # prepare the code object for mutation
    if isa(inferred, CodeInfo)
        src = inferred
        ast = copy_exprargs(inferred.code)
    else
        src = ccall(:jl_uncompress_ast, Any, (Any, Any), method, inferred::Vector{UInt8})::CodeInfo
        ast = src.code
    end
    ast = ast::Array{Any,1}
    nm = length(unwrap_unionall(metharg).parameters)
    body = Expr(:block)
    body.args = ast

    # see if each argument occurs only once in the body expression
    stmts = []
    prelude_stmts = []
    stmts_free = true # true = all entries of stmts are effect_free

    argexprs = copy(argexprs)
    if isva
        # move constructed vararg tuple to an ssavalue
        varargvar = newvar!(sv, atypes[na])
        push!(prelude_stmts, Expr(:(=), varargvar, argexprs[na]))
        argexprs[na] = varargvar
    end
    for i = na:-1:1 # stmts_free needs to be calculated in reverse-argument order
        #args_i = args[i]
        aei = argexprs[i]
        aeitype = argtype = widenconst(exprtype(aei, sv.src, sv.mod))
        if i == 1 && !(invoke_texpr === nothing)
            pushfirst!(prelude_stmts, invoke_texpr)
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
                pushfirst!(prelude_stmts, Expr(:(=), vnew, aei))
                stmts_free &= free
            elseif !free && !isType(aeitype)
                pushfirst!(prelude_stmts, aei)
                stmts_free = false
            end
        end
    end
    invoke_fexpr === nothing || pushfirst!(prelude_stmts, invoke_fexpr)

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
    newlabels = zeros(Int, label_counter(body.args))
    for i = 1:length(body.args)
        a = body.args[i]
        if isa(a, LabelNode)
            newlabel = genlabel(sv)
            newlabels[a.label] = newlabel.label
            body.args[i] = newlabel
        end
    end
    for i = 1:length(body.args)
        a = body.args[i]
        if isa(a, GotoNode)
            body.args[i] = GotoNode(newlabels[a.label])
        elseif isa(a, Expr)
            if a.head === :enter
                a.args[1] = newlabels[a.args[1]::Int]
            elseif a.head === :gotoifnot
                a.args[2] = newlabels[a.args[2]::Int]
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
                pushfirst!(a.args, retval)
                a.head = :(=)
                push!(stmts, GotoNode(retstmt.label))
            end
        end
    end

    if multiret
        if lastexpr !== nothing
            pushfirst!(lastexpr.args, retval)
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
            linenode = popfirst!(stmts)::LineNumberNode
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
            pushfirst!(stmts, Expr(:meta, :push_loc, file,
                                 method.name, line))
        else
            pushfirst!(stmts, Expr(:meta, :push_loc, file,
                                 method.name, line, mod))
        end
        push!(stmts, Expr(:meta, :pop_loc))
    elseif !isempty(stmts)
        pushfirst!(stmts, Expr(:meta, :push_loc, file,
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
                if fidx === nothing
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

function mk_tuplecall(args, sv::OptimizationState)
    e = Expr(:call, top_tuple, args...)
    e.typ = tuple_tfunc(Tuple{Any[widenconst(exprtype(x, sv.src, sv.mod)) for x in args]...})
    return e
end

function inlining_pass!(sv::OptimizationState, propagate_inbounds::Bool)
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
                eargs[i] = inline_expr(ei, sv, stmtbuf, boundscheck)
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

function inline_expr(e::Expr, sv::OptimizationState, stmts::Vector{Any}, boundscheck::Symbol)
    if e.head === :call
        return inline_call(e, sv, stmts, boundscheck)
    elseif e.head === :isdefined
        isa(e.typ, Const) && return e.typ.val
    elseif e.head === :(=) && isa(e.args[2], Expr)
        e.args[2] = inline_expr(e.args[2], sv, stmts, boundscheck)
    elseif e.head === :return && isa(e.args[1], Expr)
        e.args[1] = inline_expr(e.args[1], sv, stmts, boundscheck)
    end
    return e
end

function finddef(v::SSAValue, stmts::Vector{Any})
    for s in stmts
        if isa(s,Expr) && s.head === :(=) && s.args[1] === v
            return s
        end
    end
    return nothing
end

# return inlined replacement for call `e`, inserting new needed statements in `stmts`.
function inline_call(e::Expr, sv::OptimizationState, stmts::Vector{Any}, boundscheck::Symbol)
    eargs = e.args
    if length(eargs) < 1
        return e
    end
    arg1 = eargs[1]

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

    ins = 1

    # TODO: determine whether this is really necessary
#=
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
                basenumtype = Union{corenumtype, Main.Base.ComplexF32, Main.Base.ComplexF64, Main.Base.Rational}
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
=#
    for ninline = 1:100
        ata = Vector{Any}(uninitialized, length(e.args))
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
            newargs = Vector{Any}(uninitialized, na-2)
            newstmts = Any[]
            effect_free_upto = 0
            for i = 3:na
                aarg = e.args[i]
                argt = exprtype(aarg, sv.src, sv.mod)
                t = widenconst(argt)
                if isa(argt, Const) && (isa(argt.val, Tuple) || isa(argt.val, SimpleVector)) &&
                        effect_free(aarg, sv.src, sv.mod, true)
                    val = argt.val
                    newargs[i - 2] = Any[ quoted(val[i]) for i in 1:(length(val)::Int) ] # avoid making a tuple Generator here!
                elseif isa(aarg, Tuple) || (isa(aarg, QuoteNode) && (isa(aarg.value, Tuple) || isa(aarg.value, SimpleVector)))
                    if isa(aarg, QuoteNode)
                        aarg = aarg.value
                    end
                    newargs[i - 2] = Any[ quoted(aarg[i]) for i in 1:(length(aarg)::Int) ] # avoid making a tuple Generator here!
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
                    ntp = length(tp)::Int
                    if isa(aarg,SSAValue) && any(p->(p === DataType || p === UnionAll || p === Union || p === Type), tp)
                        # replace element type from Tuple{DataType} with more specific type if possible
                        def = finddef(aarg, sv.src.code)
                        if def !== nothing
                            defex = def.args[2]
                            if isa(defex, Expr) && is_known_call(defex, tuple, sv.src, sv.mod)
                                tp = collect(Any, tp)
                                for j = 1:ntp
                                    specific_type = exprtype(defex.args[j+1], sv.src, sv.mod)
                                    if (tp[j] <: Type) && specific_type ⊑ tp[j]
                                        tp[j] = specific_type
                                    end
                                end
                            end
                        end
                    end
                    fldvars = Any[ newvar!(sv, tp[j]) for j in 1:ntp ]
                    for j = 1:ntp
                        push!(newstmts, Expr(:(=), fldvars[j], mk_getfield(tmpv, j, tp[j])))
                    end
                    newargs[i - 2] = fldvars
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

symequal(x::SSAValue, y::SSAValue) = x.id === y.id
symequal(x::Slot    , y::Slot)     = x.id === y.id
symequal(@nospecialize(x)     , @nospecialize(y))      = x === y

function void_use_elim_pass!(sv::OptimizationState)
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
            elseif h === :isdefined || h === :copyast || h === :the_exception
                return false
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
        elseif !do_coverage && isa(ex, LineNumberNode)
            prev_label = prev_dbg_stack[end]
            if prev_label != 0 && code[prev_label].file === ex.file
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
            npops = (nargs > 1 ? args[2]::Int : 1)
            for pop in 1:npops
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
                    npops -= 1
                else
                    prev_dbg_stack[end] = 0
                    push_loc_pos_stack[end] = 0
                end
            end
            if npops > 1
                code[i] = Expr(:meta, :pop_loc, npops)
            elseif npops == 1
                code[i] = Expr(:meta, :pop_loc)
            else
                code[i] = nothing
            end
        else
            continue
        end
    end

    # combine consecutive :pop_loc instructions
    lastpop = nothing
    npops = 0
    for i in 1:length(code)
        ex = code[i]
        if isa(ex, Expr) && ex.head === :meta && length(ex.args) > 0 && ex.args[1] == :pop_loc
            npops += (length(ex.args) > 1 ? ex.args[2]::Int : 1)
            if lastpop === nothing
                lastpop = i
            else
                code[i] = nothing
            end
        elseif ex !== nothing && lastpop !== nothing
            if npops > 1
                popex = code[lastpop]
                if length(popex.args) == 1
                    code[lastpop] = Expr(:meta, :pop_loc, npops)
                else
                    popex.args[2] = npops
                end
            end
            lastpop = nothing
            npops = 0
        end
    end
end

# Replace branches with constant conditions with unconditional branches
function gotoifnot_elim_pass!(sv::OptimizationState)
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
function basic_dce_pass!(sv::OptimizationState)
    body = sv.src.code
    labelmap = get_label_map(body)
    reachable = BitSet()
    W = BitSet()
    push!(W, 1)
    while !isempty(W)
        pc = pop!(W)
        pc in reachable && continue
        push!(reachable, pc)
        expr = body[pc]
        pc´ = pc + 1 # next program-counter (after executing instruction)
        if isa(expr, GotoNode)
            pc´ = labelmap[expr.label]
        elseif isa(expr, Expr)
            if expr.head === :gotoifnot
                push!(W, labelmap[expr.args[2]::Int])
            elseif expr.head === :enter
                push!(W, labelmap[expr.args[1]::Int])
            elseif expr.head === :return
                continue
            end
        end
        pc´ <= length(body) && push!(W, pc´)
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

# Run on linear IR before the `alloc_elim_pass!` to fold away
# getfield on constant immutable objects. Any chained getfield will then be optimized
# away by the `alloc_elim_pass!`.
# Also expect undefined variable access check to be lifted into flags.
function fold_constant_getfield_pass!(sv::OptimizationState)
    body = sv.src.code
    nexpr = length(body)
    for i in 1:nexpr
        ex = body[i]
        isa(ex, Expr) || continue
        head = ex.head
        parent = body
        pidx = i
        if head === :(=)
            parent = ex.args
            pidx = 2
            ex = ex.args[2]
            isa(ex, Expr) || continue
            head = ex.head
        end
        (head === :call && 2 < length(ex.args) < 5) || continue
        is_known_call(ex, getfield, sv.src, sv.mod) || continue
        # No side effect can happen for linear IR
        obj = exprtype(ex.args[2], sv.src, sv.mod)
        isa(obj, Const) || continue
        obj = obj.val
        !typeof(obj).mutable || isa(obj, DataType) || continue
        fld = ex.args[3]
        if isa(fld, Int)
            fld = fld
        elseif isa(fld, QuoteNode)
            fld = fld.value
        else
            continue
        end
        isa(fld, Symbol) || isa(fld, Int) || continue
        if isdefined(obj, fld)
            parent[pidx] = quoted(getfield(obj, fld))
        end
    end
end

function get_undef_flag_slot(src::CodeInfo, flagslots, id)
    flag_id = flagslots[id]
    flag_id != 0 && return SlotNumber(flag_id)
    slot = add_slot!(src, Nothing, src.slotflags[id] & Slot_AssignedOnce != 0, src.slotnames[id])
    flag_id = slot_id(slot)
    src.slotflags[flag_id] |= Slot_StaticUndef | Slot_UsedUndef
    flagslots[id] = flag_id
    return slot
end

# Run on linear IR before the `alloc_elim_pass!` to make sure all expression arguments are
# effect-free while maintaining the original def-use chain so that assignments of allocation
# to a UsedUndef slot can still be optimized.
# After this pass, we are sure that no `Expr` arguments have side-effects.
function split_undef_flag_pass!(sv::OptimizationState)
    body = sv.src.code
    len = length(body)
    next_i = 1
    norigslots = length(sv.src.slotflags)
    flagslots = zeros(Int, norigslots)
    while next_i <= len
        i = next_i
        next_i += 1
        ex = body[i]
        if isa(ex, Slot)
            id = slot_id(ex)
            var_has_undef(sv.src, id) || continue
            body[i] = get_undef_flag_slot(sv.src, flagslots, id)
            continue
        elseif isa(ex, NewvarNode)
            id = slot_id(ex.slot)
            var_has_undef(sv.src, id) || continue
            body[i] = NewvarNode(get_undef_flag_slot(sv.src, flagslots, id))
            continue
        elseif !isa(ex, Expr)
            continue
        end
        head = ex.head
        if head === :(=)
            rhs = ex.args[2]
            lhs = ex.args[1]
            if isa(lhs, Slot)
                id = slot_id(lhs)
                if var_has_undef(sv.src, id)
                    flagslot = get_undef_flag_slot(sv.src, flagslots, id)
                    insert!(body, i + 1, :($flagslot = $nothing))
                    next_i += 1
                    len += 1
                end
            end
            if isa(rhs, Expr)
                ex = rhs
                head = ex.head
            else
                if isa(rhs, Slot)
                    id = slot_id(rhs)
                    if var_has_undef(sv.src, id)
                        insert!(body, i, get_undef_flag_slot(sv.src, flagslots, id))
                        i += 1
                        next_i += 1
                        len += 1
                    end
                end
                continue
            end
        end
        eargs = ex.args
        if head === :const || is_meta_expr_head(head)
            continue
        elseif head === :isdefined
            @assert length(eargs) == 1
            ea1 = eargs[1]
            if isa(ea1, Slot)
                id = slot_id(ea1)
                if var_has_undef(sv.src, id)
                    eargs[1] = get_undef_flag_slot(sv.src, flagslots, id)
                end
            end
            continue
        end
        isccall = head === :foreigncall
        for j in 1:length(eargs)
            ea = eargs[j]
            if isccall && isa(ea, Expr) && ea.head === :&
                ea = ea.args[1]
            end
            if isa(ea, Slot)
                id = slot_id(ea)
                if j == 1 && head === :method
                    flagslot = get_undef_flag_slot(sv.src, flagslots, id)
                    insert!(body, i + 1, :($flagslot = $nothing))
                    next_i += 1
                    len += 1
                    continue
                end
                if var_has_undef(sv.src, id)
                    insert!(body, i, get_undef_flag_slot(sv.src, flagslots, id))
                    i += 1
                    next_i += 1
                    len += 1
                end
            end
        end
    end
    for i in 1:norigslots
        if flagslots[i] != 0
            sv.src.slotflags[i] = (sv.src.slotflags[i] | Slot_StaticUndef) & ~UInt8(Slot_UsedUndef)
        end
    end
end

# This struct contains information about a use of certain value (`SSAValue` or `Slot`)
# This might be a toplevel use for `Slot` in which case the `expr` field is `#undef`,
# and it only happens if the slot might be used before it's defined.
struct ValueUse
    # The statement array where `expr` (or its parent assignment) appears in
    stmts::Vector{Any}
    # The position of the `expr` in the `stmts` array
    stmtidx::Int

    # The position the value appears in `expr`
    exidx::Int
    # The expression the value is used in.
    # If `expr` is undef, the use is a statement level use.
    # This must be one of the following:
    # 1. A statement level `Expr(:(=), ...)`.
    # 2. The RHS of a statement level `Expr(:(=), ...)`.
    # 3. A `&` ccall argument.
    expr::Expr

    ValueUse(stmts, stmtidx, expr, exidx) = new(stmts, stmtidx, exidx, expr)
    ValueUse(stmts, stmtidx) = new(stmts, stmtidx, 0)
end
# Check if the use is still valid.
# The code that invalidate this use is responsible for adding new use(s) if any.
function check_valid(use::ValueUse, changes::ObjectIdDict)
    haskey(changes, use.stmts=>use.stmtidx) && return false
    isdefined(use, :expr) && haskey(changes, use.expr) && return false
    return true
end

# This struct contains information about a def of certain value (`SSAValue` or `Slot`)
# The `assign` field is usually an assignment but it might be a `NewvarNode` for `Slot`,
# which can happen if the slot might be used before it's defined.
struct ValueDef
    assign::Union{Expr,NewvarNode}

    # The statement array where `expr` (or its parent assignment) appears in
    stmts::Vector{Any}
    # The position of the `expr` in the `stmts` array
    stmtidx::Int
end
# Check if the use is still valid.
# The code that invalidate this use is responsible for adding new def(s) if any.
check_valid(def::ValueDef, changes::ObjectIdDict) = !haskey(changes, def.stmts=>def.stmtidx)

# Allocation optimization, must not be mutated.
const empty_uses = ValueUse[]
const empty_defs = ValueDef[]
# Uses and defs of a value.
mutable struct ValueInfo
    uses::Vector{ValueUse}
    defs::Vector{ValueDef}
    has_method::Bool
    ValueInfo() = new(empty_uses, empty_defs, false)
end
function remove_invalid!(info::ValueInfo, changes::ObjectIdDict)
    if isempty(changes)
        return
    end
    cb = x->check_valid(x, changes)
    filter!(cb, info.uses)
    filter!(cb, info.defs)
    return
end

function add_def(info::ValueInfo, def::ValueDef)
    if info.defs === empty_defs
        info.defs = [def]
    else
        push!(info.defs, def)
    end
    return
end
function add_use(info::ValueInfo, use::ValueUse)
    if info.uses === empty_uses
        info.uses = [use]
    else
        push!(info.uses, use)
    end
    return
end

# The def and use information of all variables in a function.
# `slots` is indexed by slot id and `ssas` is indexed by ssa id + 1.
# This structure is indexable by either a `Slot`, a `SSAValue` or a `id=>is_ssa` pair.
struct ValueInfoMap
    slots::Vector{ValueInfo}
    ssas::Vector{ValueInfo}
    ValueInfoMap() = new(ValueInfo[], ValueInfo[])
end
@inline get_info_entry(infomap::ValueInfoMap, slot::Slot) = (infomap.slots, slot_id(slot))
@inline get_info_entry(infomap::ValueInfoMap, ssa::SSAValue) = (infomap.ssas, ssa.id + 1)
@inline get_info_entry(infomap::ValueInfoMap, pair::Pair{Int,Bool}) =
    (pair.second ? infomap.ssas : infomap.slots, pair.first)
isassigned(infomap::ValueInfoMap, var) = isassigned(get_info_entry(infomap, var)...)
function delete!(infomap::ValueInfoMap, var)
    infos, idx = get_info_entry(infomap, var)
    ccall(:jl_arrayunset, Cvoid, (Any, Csize_t), infos, (idx - 1) % UInt)
end
function getindex(infomap::ValueInfoMap, var)
    infos, idx = get_info_entry(infomap, var)
    if isassigned(infos, idx)
        return infos[idx]
    else
        idx > length(infos) && resize!(infos, idx)
        info = ValueInfo()
        infos[idx] = info
        return info
    end
end
function setindex!(infomap::ValueInfoMap, info::ValueInfo, var)
    ary, idx = get_info_entry(infomap, var)
    idx > length(ary) && resize!(ary, idx)
    ary[idx] = info
end
add_def(infomap::ValueInfoMap, var, def::ValueDef) = add_def(infomap[var], def)
add_use(infomap::ValueInfoMap, var, use::ValueUse) = add_use(infomap[var], use)

@inline function var_has_static_undef(src, id, ssa=false)
    ssa && return false
    flags = src.slotflags[id]
    # The check for `UsedUndef` shouldn't be necessary but doesn't hurt
    return flags & Slot_UsedUndef != 0 || flags & Slot_StaticUndef != 0
end

@inline function var_has_undef(src, id, ssa=false)
    ssa && return false
    flags = src.slotflags[id]
    return flags & Slot_UsedUndef != 0
end

@inline function var_is_ssa(src, id, ssa=false)
    ssa && return true
    flags = src.slotflags[id]
    # The check for `UsedUndef` shouldn't be necessary but doesn't hurt
    return flags & (Slot_UsedUndef | Slot_StaticUndef) == 0 && flags & Slot_AssignedOnce != 0
end

function scan_expr_use!(infomap, body, i, ex, src)
    if isa(ex, Vector{Any})
        # Shouldn't happen but just to be safe since we treat this as nested blocks
        # in the `alloc_elim_pass!`
        body[i] = nothing
        return
    elseif isa(ex, SSAValue)
        body[i] = nothing
        return
    elseif isa(ex, Slot)
        if var_has_undef(src, slot_id(ex))
            add_use(infomap, ex, ValueUse(body, i))
        else
            body[i] = nothing
        end
        return
    elseif isa(ex, NewvarNode)
        # Clear newvarnode if the variable is never used undefined
        if var_has_undef(src, slot_id(ex.slot))
            add_def(infomap, ex.slot, ValueDef(ex, body, i))
        else
            body[i] = nothing
        end
        return
    elseif !isa(ex, Expr)
        return
    end
    head = ex.head
    is_meta_expr_head(head) && return
    if head === :(=)
        rhs = ex.args[2]
        lhs = ex.args[1]
        lhs_slot = isa(lhs, Slot)
        rhs_slot = isa(rhs, Slot)
        if lhs_slot && rhs_slot && symequal(lhs, rhs)
            # Self assignment
            if var_has_undef(src, slot_id(lhs))
                body[i] = rhs
                add_use(infomap, rhs, ValueUse(body, i))
            else
                body[i] = nothing
            end
            return
        end
        if lhs_slot || isa(lhs, SSAValue)
            add_def(infomap, lhs, ValueDef(ex, body, i))
        end
        if isa(rhs, Expr)
            ex = rhs
            head = ex.head
        else
            if rhs_slot || isa(rhs, SSAValue)
                add_use(infomap, rhs, ValueUse(body, i, ex, 2))
            end
            return
        end
    end
    eargs = ex.args
    isccall = head === :foreigncall
    for j in 1:length(eargs)
        ea = eargs[j]
        if j == 1 && head === :method
            if isa(ea, Slot)
                infomap[ea].has_method = true
                continue
            end
        end
        if isccall && isa(ea, Expr) && ea.head === :&
            ea2 = ea.args[1]
            if isa(ea2, Slot) || isa(ea2, SSAValue)
                add_use(infomap, ea2, ValueUse(body, i, ea, 1))
            end
        elseif isa(ea, Slot) || isa(ea, SSAValue)
            add_use(infomap, ea, ValueUse(body, i, ex, j))
        end
    end
end

# Collect infomation about all the defs and uses of variables in the function
# Also do simple cleanups as we do a linear scan over the code
function collect_value_infos(body::Vector{Any}, src::CodeInfo, nargs::Int)
    infomap = ValueInfoMap()
    nexpr = length(body)
    for i in 1:nexpr
        ex = body[i]
        scan_expr_use!(infomap, body, i, ex, src)
    end

    # Remove arguments from slot uses since they are useless
    slotsinfo = infomap.slots
    for i in 1:length(slotsinfo)
        if isassigned(slotsinfo, i)
            info = slotsinfo[i]
            if i > nargs && length(info.defs) == 1 && isa(info.defs[1].assign, Expr)
                src.slotflags[i] |= Slot_AssignedOnce
            end
        end
    end

    return infomap
end

struct StructInfo
    defs::Vector{Any}
    names::Vector{Symbol}
    typ::DataType
    mutable::Bool
    isnew::Bool
end

struct AllocOptContext
    infomap::ValueInfoMap
    sv::OptimizationState
    todo::ObjectIdDict
    changes::ObjectIdDict
    sym_count::ObjectIdDict
    all_fld::ObjectIdDict
    setfield_typ::ObjectIdDict
    undef_fld::ObjectIdDict
    structinfos::Vector{StructInfo}
    function AllocOptContext(infomap::ValueInfoMap, sv::OptimizationState)
        todo = ObjectIdDict()
        for i in 1:length(infomap.ssas)
            isassigned(infomap.ssas, i) || continue
            todo[i=>true] = nothing
        end
        for i in 1:length(infomap.slots)
            isassigned(infomap.slots, i) || continue
            i > sv.nargs || continue
            todo[i=>false] = nothing
        end
        return new(infomap, sv, todo, ObjectIdDict(), ObjectIdDict(),
                   ObjectIdDict(), ObjectIdDict(), ObjectIdDict(), StructInfo[])
    end
end

function delete_valueinfo!(ctx::AllocOptContext, key)
    # Slot
    if !key.second
        ctx.sv.src.slotflags[key.first] = 0
    end
    delete!(ctx.infomap, key)
    return
end

function add_allocopt_todo(ctx::AllocOptContext, id, is_ssa)
    ctx.todo[id=>is_ssa] = nothing
end

@inline function add_allocopt_todo(ctx::AllocOptContext, @nospecialize(var))
    if isa(var, SSAValue)
        ctx.todo[(var.id + 1)=>true] = nothing
    elseif isa(var, Pair{Int,Bool})
        ctx.todo[var] = nothing
    else
        ctx.todo[slot_id(var)=>false] = nothing
    end
end

@inline function maybe_add_allocopt_todo(ctx::AllocOptContext, @nospecialize(var))
    if isa(var, SSAValue)
        ctx.todo[(var.id + 1)=>true] = nothing
    elseif isa(var, Slot)
        ctx.todo[slot_id(var)=>false] = nothing
    end
end

function delete_value!(ctx::AllocOptContext, info, key)
    # No use is left for this value, delete the assignment
    for def in info.defs
        ctx.changes[def.assign] = nothing
        assign = def.assign
        if isa(assign, NewvarNode)
            def.stmts[def.stmtidx] = nothing
            continue
        end
        defex = assign.args[2]
        if isa(defex, SSAValue)
            add_allocopt_todo(ctx, defex)
            def.stmts[def.stmtidx] = nothing
        elseif isa(defex, Slot)
            if var_has_undef(ctx.sv.src, slot_id(defex))
                add_use(ctx.infomap, defex, ValueUse(def.stmts, def.stmtidx))
                def.stmts[def.stmtidx] = defex
            else
                add_allocopt_todo(ctx, defex)
                def.stmts[def.stmtidx] = nothing
            end
        elseif isa(defex, Vector{Any})
            def.stmts[def.stmtidx] = nothing
        else
            # Update this to add todo
            # when adding cases where this might enable further optimizations.
            def.stmts[def.stmtidx] = defex
        end
    end
    delete_valueinfo!(ctx, key)
end

function merge_value_ssa!(ctx::AllocOptContext, info, key)
    # There are other cases that we can merge
    # but those require control flow analysis.
    var_has_static_undef(ctx.sv.src, key.first, key.second) && return false
    local defkey
    for def in info.defs
        # No NewvarNode def for variables that aren't used undefined
        defex = (def.assign::Expr).args[2]
        if isa(defex, SSAValue)
            new_key = (defex.id + 1)=>true
            if @isdefined(defkey) && defkey != new_key
                return true
            else
                defkey = new_key
            end
        elseif isa(defex, Slot)
            id = slot_id(defex)
            new_key = id=>false
            defslot_is_ssa = id <= ctx.sv.nargs || var_is_ssa(ctx.sv.src, id)
            if !defslot_is_ssa || (@isdefined(defkey) && defkey != new_key)
                return true
            else
                defkey = new_key
            end
        else
            return @isdefined(defkey)
        end
    end

    if defkey.second || defkey.first > ctx.sv.nargs
        add_allocopt_todo(ctx, defkey)
    end

    # don't replace TypedSlots with SSAValues
    # TODO: introduce extra SSAValue for each differently-typed use.
    if defkey.second
        for use in info.uses
            if isdefined(use, :expr) && use.expr.args[use.exidx] isa TypedSlot
                return false
            end
        end
    end

    definfo = ctx.infomap[defkey]
    for def in info.defs
        ctx.changes[def.assign] = nothing
        def.stmts[def.stmtidx] = nothing
    end
    if defkey.second
        # SSAValue def
        replace_v = SSAValue(defkey.first - 1)
    else
        replace_v = SlotNumber(defkey.first)
    end
    for use in info.uses
        if isdefined(use, :expr)
            this_use = use.expr.args[use.exidx]
            if !defkey.second && this_use isa TypedSlot
                use.expr.args[use.exidx] = TypedSlot(replace_v.id, this_use.typ)
            else
                use.expr.args[use.exidx] = replace_v
            end
            add_use(definfo, use)
        else
            # This variable is never used undef, ignore statement level use
            use.stmts[use.stmtidx] = nothing
        end
    end
    delete_valueinfo!(ctx, key)
    return true
end

function replace_use_expr_with!(ctx::AllocOptContext, use::ValueUse, expr,
                                delete_old=true, update_var_use=false)
    # Not supported on ccall & expression
    oldexpr = use.expr
    stmt = use.stmts[use.stmtidx]::Expr
    if stmt === oldexpr
        use.stmts[use.stmtidx] = expr
        if update_var_use && (isa(expr, Slot) || isa(expr, SSAValue))
            add_use(ctx.infomap, expr, ValueUse(use.stmts, use.stmtidx))
        end
    else
        @assert stmt.head === :(=) && stmt.args[2] === oldexpr
        stmt.args[2] = expr
        if update_var_use && (isa(expr, Slot) || isa(expr, SSAValue))
            add_use(ctx.infomap, expr, ValueUse(use.stmts, use.stmtidx, stmt, 2))
        end
        maybe_add_allocopt_todo(ctx, stmt.args[1])
    end
    if delete_old
        ctx.changes[oldexpr] = nothing
    end
end

function propagate_const_def!(ctx::AllocOptContext, info, key)
    local constv
    for def in info.defs
        # No NewvarNode def for variables that aren't used undefined
        defex = (def.assign::Expr).args[2]
        # We don't want to probagate this constant since this is a token.
        isa(defex, Expr) && defex.head === :gc_preserve_begin && return false
        v = exprtype(defex, ctx.sv.src, ctx.sv.mod)
        isa(v, Const) || return false
        v = v.val
        @isdefined(constv) && constv !== v && return false
        constv = v
    end
    for def in info.defs
        defex = (def.assign::Expr).args[2]
        if isa(defex, Expr)
            # Expr def can have side effects
            def.stmts[def.stmtidx] = defex
        else
            def.stmts[def.stmtidx] = nothing
        end
        ctx.changes[def.assign] = nothing
    end
    replace_ex = quoted(constv)
    is_immutable = !typeof(constv).mutable || isa(constv, DataType)
    for use in info.uses
        if !isdefined(use, :expr)
            use.stmts[use.stmtidx] = nothing
            continue
        elseif is_immutable
            if use.exidx == 2 && is_known_call(use.expr, getfield, ctx.sv.src, ctx.sv.mod) &&
                2 < length(use.expr.args) < 5

                fld = use.expr.args[3]
                if isa(fld, Int)
                    fld = fld
                elseif isa(fld, QuoteNode)
                    fld = fld.value
                else
                    fld = nothing
                end
                if isa(fld, Symbol) || isa(fld, Int)
                    if isdefined(constv, fld)
                        fieldv = getfield(constv, fld)
                        replace_use_expr_with!(ctx, use, quoted(fieldv))
                        continue
                    end
                end
            end
        end
        use.expr.args[use.exidx] = replace_ex
        if use.expr.head === :(=)
            maybe_add_allocopt_todo(ctx, use.expr.args[1])
        end
    end
    delete_valueinfo!(ctx, key)
    return true
end

function split_disjoint_assign!(ctx::AllocOptContext, info, key)
    key.second && return false
    isleaftype(ctx.sv.src.slottypes[key.first]) && return false
    alltypes = ObjectIdDict()
    ndefs = length(info.defs)
    deftypes = Vector{Any}(uninitialized, ndefs)
    for i in 1:ndefs
        def = info.defs[i]
        defex = (def.assign::Expr).args[2]
        rhstyp = widenconst(exprtype(defex, ctx.sv.src, ctx.sv.mod))
        isleaftype(rhstyp) || return false
        alltypes[rhstyp] = nothing
        deftypes[i] = rhstyp
    end
    need_tag = false
    for use in info.uses
        usex = use.expr
        slot = usex.args[use.exidx]
        if isa(slot, TypedSlot)
            usetyp = widenconst(slot.typ)
            if isleaftype(usetyp)
                alltypes[usetyp] = nothing
                continue
            end
        end
        if usex.head === :(=) || usex.head === :&
            return false
        else
            ty = exprtype(usex, ctx.sv.src, ctx.sv.mod)
            if isa(ty, Const)
            elseif isa(ty, Conditional) && isa(ty.var, Slot) && slot_id(ty.var) == key.first
                if isa(ty.vtype, Const) && !isdefined(typeof(ty.vtype.val), :instance)
                    return false
                end
                need_tag = true
            else
                return false
            end
            effect_free(usex, ctx.sv.src, ctx.sv.mod, false) || return false
        end
    end
    name = ctx.sv.src.slotnames[key.first]
    flags = ctx.sv.src.slotflags[key.first]
    if need_tag
        tag_var = add_slot!(ctx.sv.src, Int, false, name)
        ctx.sv.src.slotflags[tag_var.id] = flags
    end
    for i in 1:ndefs
        def = info.defs[i]
        assign = def.assign::Expr
        defex = assign.args[2]
        rhstyp = deftypes[i]
        new_slot = alltypes[rhstyp]
        if !isa(new_slot, SlotNumber)
            new_slot = add_slot!(ctx.sv.src, rhstyp, false, name)
            ctx.sv.src.slotflags[new_slot.id] = flags | Slot_StaticUndef
            alltypes[rhstyp] = new_slot
            add_allocopt_todo(ctx, new_slot)
        end
        assign.args[1] = new_slot
        if need_tag
            stmts = Any[:($tag_var = $(new_slot.id)), assign]
            add_def(ctx.infomap, tag_var, ValueDef(stmts[1], stmts, 1))
            scan_expr_use!(ctx.infomap, stmts, 2, assign, ctx.sv.src)
            ctx.changes[def.stmts=>def.stmtidx] = nothing
            def.stmts[def.stmtidx] = stmts
        else
            add_def(ctx.infomap, new_slot, def)
        end
    end
    for use in info.uses
        usex = use.expr
        slot = usex.args[use.exidx]
        if isa(slot, TypedSlot)
            usetyp = widenconst(slot.typ)
            if isleaftype(usetyp)
                usetyp = widenconst(slot.typ)
                new_slot = alltypes[usetyp]
                if !isa(new_slot, SlotNumber)
                    new_slot = add_slot!(ctx.sv.src, usetyp, false, name)
                    ctx.sv.src.slotflags[new_slot.id] = flags | Slot_StaticUndef
                    alltypes[usetyp] = new_slot
                end
                new_slot = new_slot::SlotNumber
                use.expr.args[use.exidx] = new_slot
                add_use(ctx.infomap, new_slot, use)
                continue
            end
        end
        ty = exprtype(usex, ctx.sv.src, ctx.sv.mod)
        if isa(ty, Const)
            replace_use_expr_with!(ctx, use, quoted(ty.val))
        elseif isa(ty, Conditional)
            exprs = []
            vars = []
            for (t, v) in alltypes
                isa(v, SlotNumber) || continue
                if t ⊑ widenconst(ty.vtype)
                    new_var = newvar!(ctx.sv, Bool)
                    new_val = :($(GlobalRef(Core, :(===)))($tag_var, $(v.id)))
                    new_val.typ = Bool
                    new_ex = :($new_var = $new_val)
                    push!(exprs, new_ex)
                    push!(vars, new_var)
                    add_def(ctx.infomap, new_var, ValueDef(new_ex, exprs, length(exprs)))
                    add_use(ctx.infomap, tag_var, ValueUse(exprs, length(exprs), new_val, 2))
                end
            end
            if isempty(vars)
                replace_use_expr_with!(ctx, use, quoted(false))
            else
                var = vars[1]
                for var_i in 2:length(vars)
                    new_var = newvar!(ctx.sv, Bool)
                    new_val = :($(GlobalRef(Core.Intrinsics, :and_int))($var, $(vars[var_i])))
                    new_val.typ = Bool
                    new_ex = :($new_var = $new_val)
                    push!(exprs, new_ex)
                    add_def(ctx.infomap, new_var, ValueDef(new_ex, exprs, length(exprs)))
                    add_use(ctx.infomap, var, ValueUse(exprs, length(exprs), new_val, 2))
                    add_use(ctx.infomap, vars[var_i], ValueUse(exprs, length(exprs), new_val, 3))
                    var = new_var
                end
                replace_use_expr_with!(ctx, use, var, false)
                old_expr = use.stmts[use.stmtidx]
                push!(exprs, old_expr)
                use.stmts[use.stmtidx] = exprs
                scan_expr_use!(ctx.infomap, exprs, length(exprs), old_expr, ctx.sv.src)
                ctx.changes[use.stmts=>use.stmtidx] = nothing
            end
        end
    end
    delete_valueinfo!(ctx, key)
    return true
end

function show_info(io::IO, use::ValueUse, ctx)
    if isdefined(use, :expr)
        print(io, "ValueUse(expr=`", use.expr, "`, exidx=", use.exidx,
              ", stmts[", use.stmtidx, "]=`", use.stmts[use.stmtidx], "`)")
    else
        print(io, "ValueUse(stmts[", use.stmtidx, "]=", use.stmts[use.stmtidx], ")")
    end
    if isa(ctx, AllocOptContext) && !check_valid(use, ctx.changes)
        print(io, " Invalidated")
    end
end
function show_info(io::IO, def::ValueDef, ctx)
    print(io, "ValueDef(assign=`", def.assign, "`, stmts=..., stmtidx=", def.stmtidx, ")")
    if isa(ctx, AllocOptContext) && !check_valid(def, ctx.changes)
        print(io, " Invalidated")
    end
end
function show_info(io::IO, info::ValueInfo, ctx, prefix="")
    println(io, prefix, "ValueInfo:")
    if !isempty(info.defs)
        println(io, prefix, "  Defs:")
        for def in info.defs
            print(io, prefix, "    ")
            show_info(io, def, ctx)
            println(io)
        end
    end
    if !isempty(info.uses)
        println(io, prefix, "  Uses:")
        for use in info.uses
            print(io, prefix, "    ")
            show_info(io, use, ctx)
            println(io)
        end
    end
end
function show_info(io::IO, info::ValueInfoMap, ctx)
    for i in 1:length(info.ssas)
        isassigned(info.ssas, i) || continue
        println(io, "SSAValue(", i - 1, "):")
        show_info(io, info.ssas[i], ctx, "  ")
    end
    for i in 1:length(info.slots)
        isassigned(info.slots, i) || continue
        println(io, "Slot(", i, "):")
        show_info(io, info.slots[i], ctx, "  ")
    end
end

function structinfo_constant(ctx::AllocOptContext, @nospecialize(v), vt::DataType)
    nf = fieldcount(vt)
    if vt <: Tuple
        si = StructInfo(Vector{Any}(uninitialized, nf), Symbol[], vt, false, false)
    else
        si = StructInfo(Vector{Any}(uninitialized, nf), fieldnames(vt), vt, false, false)
    end
    for i in 1:nf
        if isdefined(v, i)
            si.defs[i] = quoted(getfield(v, i))
        else
            ctx.undef_fld[i] = nothing
            ctx.undef_fld[si.names[i]] = nothing
        end
    end
    return si
end

structinfo_tuple(ex::Expr) = StructInfo(ex.args[2:end], Symbol[], Tuple, false, false)
function structinfo_new(ctx::AllocOptContext, ex::Expr, vt::DataType)
    nf = fieldcount(vt)
    si = StructInfo(Vector{Any}(uninitialized, nf), fieldnames(vt), vt, vt.mutable, true)
    ninit = length(ex.args) - 1
    for i in 1:nf
        if i <= ninit
            si.defs[i] = ex.args[i + 1]
        else
            ft = fieldtype(vt, i)
            if isbits(ft)
                ex = Expr(:new, ft)
                ex.typ = ft
                si.defs[i] = ex
            else
                ctx.undef_fld[i] = nothing
                ctx.undef_fld[si.names[i]] = nothing
            end
        end
    end
    return si
end

function split_struct_alloc!(ctx::AllocOptContext, info, key)
    # Collect information about each struct/tuple allocation
    min_nf = typemax(Int)
    max_nf = 0
    n_immut = 0
    ndef = length(info.defs)
    empty!(ctx.sym_count)
    empty!(ctx.undef_fld)
    empty!(ctx.structinfos)
    for def in info.defs
        local defex, si, def_nf
        defex = (def.assign::Expr).args[2]
        if isa(defex, Expr)
            defex = defex::Expr
            if is_known_call(defex, tuple, ctx.sv.src, ctx.sv.mod)
                si = structinfo_tuple(defex)
            elseif defex.head === :new
                typ = widenconst(exprtype(defex, ctx.sv.src, ctx.sv.mod))
                # typ <: Tuple shouldn't happen but just in case someone generated invalid AST
                if !isa(typ, DataType) || !isleaftype(typ) || typ <: Tuple
                    return false
                end
                si = structinfo_new(ctx, defex, typ)
            else
                return false
            end
        else
            v = exprtype(defex, ctx.sv.src, ctx.sv.mod)
            isa(v, Const) || return false
            v = v.val
            vt = typeof(v)
            if (vt.mutable && !isa(v, DataType)) || fieldcount(vt) == 0
                # allowing DataType as immutable can change the behavior if the user calls
                # `setfield!` on it but that's invalid anyway and throwing an error is actually
                # better than letting it work.
                return false
            end
            si = structinfo_constant(ctx, v, vt)
        end
        if !si.mutable
            n_immut += 1
        end
        def_nf = length(si.defs)
        if def_nf < min_nf
            min_nf = def_nf
        end
        if def_nf > max_nf
            max_nf = def_nf
        end
        for fld_idx in 1:length(si.names)
            sym = si.names[fld_idx]
            prev_count = get(ctx.sym_count, sym, 0=>0)
            if isa(prev_count, Pair{Int,Int})
                if first(prev_count) == 0
                    ctx.sym_count[sym] = 1=>fld_idx
                elseif last(prev_count) == fld_idx
                    ctx.sym_count[sym] = (first(prev_count) + 1)=>fld_idx
                else
                    ctx.sym_count[sym] = (first(prev_count) + 1)=>[last(prev_count), fld_idx]
                end
            else
                prev_count = prev_count::Pair{Int,Vector{Int}}
                ctx.sym_count[sym] = (first(prev_count) + 1)=>push!(last(prev_count), fld_idx)
            end
        end
        push!(ctx.structinfos, si)
    end
    if ndef > 1
        allowed_idx = trues(min_nf)
        idx_count = zeros(Int, min_nf)
        for v in values(ctx.sym_count)
            # Ignore field names that will not be allowed to be optimized
            if isa(v, Pair{Int,Int})
                first(v) == ndef || continue
                if last(v) <= min_nf
                    idx_count[last(v)] += 1
                end
            else
                v = v::Pair{Int,Vector{Int}}
                first(v) == ndef || continue
                for fld_idx in last(v)
                    allowed_idx[fld_idx] = false
                end
            end
        end
        for fld_idx in 1:min_nf
            if idx_count[fld_idx] > 1
                allowed_idx[fld_idx] = false
            end
        end
    end

    # Find the set of valid operations for each kind of use
    #
    # * `isdefined`
    #
    #     Field name or index must be constant and unambiguous.
    #     Field that are known or unknown for all defs are allowed.
    #     Unambiguous means that each field must be only accessed by index or only by name
    #     that maps to the same index for all defs.
    #     Currently this requirement is raised to be only based on the def and not the use
    #
    # * `getfield`
    #
    #     Requires all conditions for `isdefined`.
    #     Field index must be within the intersect of all the defs.
    #     Field name must be known for all defs.
    #
    # * `setfield!`
    #
    #     Requires all conditions for `getfield`.
    #     Additionally require all defs to be mutable.
    #
    # * preserved objects (`@gc_preserve` and `ccall` roots)
    #
    #     No `setfield!` should be called for mutable defs on the NULL sites.
    #     This is because it's currently unclear how conditional undefined root slots
    #     can be represented. It's possible that we can just change the requirement in codegen
    #     for preserved objects.
    #     Currently also require single assignment.
    #     Lifting this requirement is certainly possible but harder to implement....

    has_preserve = false
    has_setfield_undef = false
    empty!(ctx.all_fld)
    empty!(ctx.setfield_typ)
    for use in info.uses
        isdefined(use, :expr) || continue
        local fld
        local expr = use.expr
        local head = expr.head
        if head === :isdefined
            # May or may not be needed but trivial to handle
            continue
        elseif head === :gc_preserve_begin
            if ndef > 1
                # This is certainly overkill, but too hard for initial version ;-p
                return false
            end
            has_setfield_undef && return false
            has_preserve = true
            continue
        elseif head === :foreigncall
            if ndef > 1
                # This is certainly overkill, but too hard for initial version ;-p
                return false
            end
            nccallargs = expr.args[5]::Int
            if use.exidx <= 5 + nccallargs
                return false
            end
            has_setfield_undef && return false
            has_preserve = true
            continue
        elseif head === :call
            if use.exidx != 2 || length(expr.args) < 3
                return false
            end
            fld = expr.args[3]
            if is_known_call(expr, isdefined, ctx.sv.src, ctx.sv.mod)
                use_kind = 0
            elseif is_known_call(expr, getfield, ctx.sv.src, ctx.sv.mod)
                use_kind = 1
            elseif is_known_call(expr, setfield!, ctx.sv.src, ctx.sv.mod)
                if n_immut != 0 || length(expr.args) < 4
                    return false
                end
                use_kind = 2
            else
                return false
            end
        else
            return false
        end
        if isa(fld, Int)
            fld = fld
        elseif isa(fld, QuoteNode)
            fld = fld.value
        else
            return false
        end
        if isa(fld, Int)
            if 0 < fld <= min_nf
                ndef == 1 || allowed_idx[fld] || return false
            elseif min_nf < fld <= max_nf
                return false
            elseif use_kind == 0
                # We can handle this but don't record in `all_fld`
                continue
            else
                return false
            end
        elseif isa(fld, Symbol)
            v = get(ctx.sym_count, fld, 0=>0)
            fld_count = isa(v, Pair{Int,Int}) ? first(v) : first(v::Pair{Int,Vector})
            if use_kind == 0 && fld_count == 0
                # We can handle this but don't record in `all_fld`
                continue
            elseif fld_count != ndef
                return false
            end
        else
            return false
        end
        if use_kind == 2
            if haskey(ctx.undef_fld, fld)
                has_preserve && return false
                has_setfield_undef = true
            end
            fld_typ = widenconst(exprtype(expr.args[4], ctx.sv.src, ctx.sv.mod))
            ctx.setfield_typ[fld] = Union{fld_typ, get(ctx.setfield_typ, fld, Union{})}
        end
        ctx.all_fld[fld] = nothing
    end
    if ndef == 1
        split_struct_alloc_single!(ctx, info, key, min_nf, has_preserve, has_setfield_undef)
    else
        split_struct_alloc_multi!(ctx, info, key)
    end
    delete_valueinfo!(ctx, key)
    return true
end

function split_struct_alloc_multi!(ctx::AllocOptContext, info, key)
    # If there are multiple assignments, we have to create slots for each values
    # We currently only allow ccall root with a single assignment so we only need to handle
    # isdefined, setfield! and getfield here.

    # First, assign a slot to each variable.
    # The slot types at this point is determined only by the setfield that are applied.
    # We'll include the initialization type as we go through the defs
    vars = ObjectIdDict()
    create_struct_field_slots!(ctx, key, vars)

    # Now, for each def. Assign all the slots.
    # Also `Union` the RHS of the assignments into the slot type as we scan through the defs.
    replace_struct_defs!(ctx, info, vars)

    # Finally replace all uses
    replace_struct_uses!(ctx, info, vars)
end

function replace_struct_uses!(ctx, info, vars)
    for use in info.uses
        if !isdefined(use, :expr)
            # Top level dummy use, remove
            use.stmts[use.stmtidx] = nothing
            continue
        end
        local fld
        local use_kind
        expr = use.expr
        head = expr.head
        if head === :isdefined
            replace_use_expr_with!(ctx, use, quoted(true))
            continue
        elseif head === :call
            fld = expr.args[3]
            if is_known_call(expr, isdefined, ctx.sv.src, ctx.sv.mod)
                use_kind = 0
            elseif is_known_call(expr, getfield, ctx.sv.src, ctx.sv.mod)
                use_kind = 1
            elseif is_known_call(expr, setfield!, ctx.sv.src, ctx.sv.mod)
                use_kind = 2
            end
        end
        if isa(fld, QuoteNode)
            fld = fld.value
        end
        slot_id = get(vars, fld, 0)
        if slot_id == 0
            @assert use_kind == 0
            replace_use_expr_with!(ctx, use, quoted(false))
            continue
        end

        if use_kind == 0
            # isdefined
            if haskey(ctx.undef_fld, fld)
                replace_use_expr_with!(ctx, use, SlotNumber(slot_id + 1), true, true)
            else
                replace_use_expr_with!(ctx, use, quoted(true))
            end
        elseif use_kind == 1
            # getfield
            if !haskey(ctx.undef_fld, fld)
                replace_use_expr_with!(ctx, use, SlotNumber(slot_id), true, true)
            else
                replace_var = SlotNumber(slot_id)
                flag_slot = SlotNumber(slot_id + 1)
                check_expr = Expr(:call, throw_undefreferror_ifnot, flag_slot)
                stmts = Any[check_expr]
                add_use(ctx.infomap, flag_slot, ValueUse(stmts, 1, check_expr, 2))
                stmt = use.stmts[use.stmtidx]::Expr
                if stmt !== expr
                    @assert stmt.head === :(=) && stmt.args[2] === expr
                    stmt.args[2] = replace_var
                    push!(stmts, stmt)
                    scan_expr_use!(ctx.infomap, stmts, length(stmts), stmt, ctx.sv.src)
                end
                ctx.changes[use.stmts=>use.stmtidx] = nothing
                use.stmts[use.stmtidx] = stmts
            end
        else
            @assert use_kind == 2
            # setfield!
            replace_var = SlotNumber(slot_id)
            val = expr.args[4]
            stmts = Any[]
            if haskey(ctx.undef_fld, fld)
                flag_slot = SlotNumber(slot_id + 1)
                assign_flag_ex = :($flag_slot = true)
                push!(stmts, assign_flag_ex)
                add_def(ctx.infomap, flag_slot, ValueDef(assign_flag_ex, stmts, length(stmts)))
            end
            assign_ex = :($replace_var = $val)
            push!(stmts, assign_ex)
            scan_expr_use!(ctx.infomap, stmts, length(stmts), assign_ex, ctx.sv.src)
            stmt = use.stmts[use.stmtidx]::Expr
            if stmt !== expr
                @assert stmt.head === :(=) && stmt.args[2] === expr
                extra_assign_ex = :($(stmt.args[1]) = $val)
                push!(stmts, extra_assign_ex)
                scan_expr_use!(ctx.infomap, stmts, length(stmts), extra_assign_ex, ctx.sv.src)
            end
            use.stmts[use.stmtidx] = stmts
            ctx.changes[use.stmts=>use.stmtidx] = nothing
        end
    end
end

function check_new_field_type(@nospecialize(val), @nospecialize(typ))
    if !isa(val, typ)
        ccall(:jl_type_error_new_expr, Union{}, (Any, Any), typ, val)
    end
end

function replace_struct_defs!(ctx, info, vars)
    for i in 1:length(info.defs)
        si = ctx.structinfos[i]
        has_name = !isempty(si.names)
        exprs = []
        for fld_idx in 1:length(si.defs)
            local slot_id
            local fld_def
            need_flag = false
            if isassigned(si.defs, fld_idx)
                fld_def = si.defs[fld_idx]
            end
            has_def = @isdefined(fld_def)
            # First check field name
            if has_name
                fname = si.names[fld_idx]
                if haskey(vars, fname)
                    slot_id = vars[fname]
                    need_flag = haskey(ctx.undef_fld, fname)
                end
            end
            if !@isdefined(slot_id)
                # Then check field index
                if haskey(vars, fld_idx)
                    slot_id = vars[fld_idx]
                    need_flag = haskey(ctx.undef_fld, fld_idx)
                else
                    # The field is not used and we don't need to deal with any side effects
                    if has_def
                        if si.isnew && !(exprtype(fld_def, ctx.sv.src,
                                                  ctx.sv.mod) ⊑ fieldtype(si.typ, fld_idx))
                            check_ex = Expr(:call, check_new_field_type, fld_def,
                                            quoted(fieldtype(si.typ, fld_idx)))
                            push!(exprs, check_ex)
                            scan_expr_use!(ctx.infomap, exprs, length(exprs), check_ex, ctx.sv.src)
                        else
                            maybe_add_allocopt_todo(ctx, fld_def)
                        end
                    end
                    continue
                end
            end

            if need_flag
                flag_slot = SlotNumber(slot_id + 1)
                flag_ex = :($flag_slot = $has_def)
                push!(exprs, flag_ex)
                add_def(ctx.infomap, flag_slot, ValueDef(flag_ex, exprs, length(exprs)))
            end
            if has_def
                fld_ty = widenconst(exprtype(fld_def, ctx.sv.src, ctx.sv.mod))
                if si.isnew && !(fld_ty ⊑ fieldtype(si.typ, fld_idx))
                    struct_fld_ty = fieldtype(si.typ, fld_idx)
                    check_ex = Expr(:call, check_new_field_type, fld_def,
                                    quoted(struct_fld_ty))
                    push!(exprs, check_ex)
                    scan_expr_use!(ctx.infomap, exprs, length(exprs), check_ex, ctx.sv.src)
                    fld_ty = typeintersect(struct_fld_ty, fld_ty)
                else
                    maybe_add_allocopt_todo(ctx, fld_def)
                end
                assign_ex = :($(SlotNumber(slot_id)) = $fld_def)
                push!(exprs, assign_ex)
                scan_expr_use!(ctx.infomap, exprs, length(exprs), assign_ex, ctx.sv.src)
                slot_type = ctx.sv.src.slottypes[slot_id]
                ctx.sv.src.slottypes[slot_id] = Union{slot_type,fld_ty}
            end
        end
        def = info.defs[i]
        ctx.changes[def.stmts=>def.stmtidx] = nothing
        def.stmts[def.stmtidx] = exprs
    end
end

function create_struct_field_slots!(ctx, key, vars)
    slot_flag = var_has_static_undef(ctx.sv.src, key.first, key.second) * Slot_StaticUndef
    for fld in keys(ctx.all_fld)
        haskey(vars, fld) && continue
        local fldidx
        local slot_id
        slot_type = get(ctx.setfield_typ, fld, Union{})
        # We shouldn't need to check both field index and field name for undef
        # since the two should agree for unambiguous fields.
        if isa(fld, Symbol)
            slot_name = fld
            v = get(ctx.sym_count, fld, 0=>0)
            if isa(v, Pair{Int,Int}) && first(v) != 0
                # single field index
                fldidx = last(v)
                slot_type = Union{slot_type,get(ctx.setfield_typ, fldidx, Union{})}
                if haskey(vars, fldidx)
                    slot_id = first(vars[fldidx]::Int)
                    ctx.sv.src.slottypes[slot_id] = slot_type
                    ctx.sv.src.slotnames[slot_id] = slot_name
                end
            end
        else
            slot_name = :field_slot
        end

        if !@isdefined(slot_id)
            slot_id = add_slot!(ctx.sv.src, slot_type, false, slot_name).id
            add_allocopt_todo(ctx, slot_id, false)
            if haskey(ctx.undef_fld, fld)
                ctx.sv.src.slotflags[end] = Slot_StaticUndef
                undef_slot = add_slot!(ctx.sv.src, Bool, false, :field_flag)
                ctx.sv.src.slotflags[end] = slot_flag
                add_allocopt_todo(ctx, undef_slot)
                @assert undef_slot.id == slot_id + 1
            else
                ctx.sv.src.slotflags[end] = slot_flag
            end
            if @isdefined(fldidx)
                vars[fldidx] = slot_id
            end
        end
        vars[fld] = slot_id
    end
end

function throw_undefreferror_ifnot(cond::Bool)
    if !cond
        throw(UndefRefError())
    end
    return
end

function split_struct_alloc_single!(ctx::AllocOptContext, info, key, nf, has_preserve,
                                    has_setfield_undef)
    # If there's only a single def, there's a lot more tricks we can do that's impossible
    # when there are multiple ones.
    #
    # 1. Using SSAValue instead of Slot if the variable itself is SSA and there isn't
    #    any setfield on the value
    # 2. Forward SSAValue or constant field value to use directly when there isn't setfield
    # 3. (For now) Splitting preserved objects
    def = info.defs[1]
    si = ctx.structinfos[1]
    if !isempty(ctx.undef_fld) && has_setfield_undef
        flag_vars = Vector{SlotNumber}(uninitialized, nf)
    end
    vars = Vector{Any}(uninitialized, nf)
    is_ssa = !var_has_static_undef(ctx.sv.src, key.first, key.second)
    def_exprs = Any[]
    if has_preserve
        preserved_vars = Any[]
    end
    # First go through each field and decide the variable name and create assignments
    # expressions.
    for i in 1:nf
        local fld_name, orig_def
        if !isempty(si.names)
            fld_name = si.names[i]
        end
        has_fld_use = haskey(ctx.all_fld, i) || (@isdefined(fld_name) &&
                                                 haskey(ctx.all_fld, fld_name))
        has_def = isassigned(si.defs, i)
        has_setfld_use = false
        if has_def
            orig_def = si.defs[i]
            field_typ = widenconst(exprtype(orig_def, ctx.sv.src, ctx.sv.mod))
            if si.isnew && !(field_typ ⊑ fieldtype(si.typ, i))
                struct_fld_ty = fieldtype(si.typ, i)
                check_ex = Expr(:call, check_new_field_type, orig_def,
                                quoted(struct_fld_ty))
                push!(def_exprs, check_ex)
                scan_expr_use!(ctx.infomap, def_exprs, length(def_exprs), check_ex, ctx.sv.src)
                field_typ = typeintersect(struct_fld_ty, field_typ)
            else
                maybe_add_allocopt_todo(ctx, orig_def)
            end
        else
            field_typ = Union{}
        end
        if has_fld_use && !isempty(ctx.setfield_typ)
            if haskey(ctx.setfield_typ, i)
                has_setfld_use = true
                field_typ = Union{field_typ,ctx.setfield_typ[i]}
            end
            if @isdefined(fld_name) && haskey(ctx.setfield_typ, fld_name)
                has_setfld_use = true
                field_typ = Union{field_typ,ctx.setfield_typ[fld_name]}
            end
        end
        if !@isdefined(fld_name)
            fld_name = :struct_field
        end
        need_preserved_root = has_preserve && !isbits(field_typ)
        local var_slot
        if !has_def
            # If there's no direct use of the field
            # (and we know that there's no use in preserved root) we can ignore this field
            # Similarly, unless someone set the field, it is always #undef and we can
            # replace all uses with that
            has_setfld_use || continue
            # OK, so someone calls setfield! on this =(
            # We need to allocate the variable **AND** the undef tag variable
            flag_slot = add_slot!(ctx.sv.src, Bool, false, fld_name)
            ctx.sv.src.slotflags[end] = is_ssa ? 0 : Slot_StaticUndef
            flag_vars[i] = flag_slot
            add_allocopt_todo(ctx, flag_slot)

            var_slot = add_slot!(ctx.sv.src, field_typ, false, fld_name)
            ctx.sv.src.slotflags[end] = Slot_StaticUndef
            vars[i] = var_slot
            add_allocopt_todo(ctx, var_slot)

            newvar_flag = :($flag_slot = false)
            push!(def_exprs, newvar_flag)
            add_def(ctx.infomap, flag_slot, ValueDef(newvar_flag, def_exprs, length(def_exprs)))
            continue
        elseif has_setfld_use
            var_slot = add_slot!(ctx.sv.src, field_typ, false, fld_name)
            ctx.sv.src.slotflags[end] = is_ssa ? 0 : Slot_StaticUndef
            def_ex = :($var_slot = $orig_def)
            push!(def_exprs, def_ex)
            scan_expr_use!(ctx.infomap, def_exprs, length(def_exprs), def_ex, ctx.sv.src)
            vars[i] = var_slot
            add_allocopt_todo(ctx, var_slot)
        else
            # OK so no setfield
            # First check if the field was a constant
            cv = exprtype(orig_def, ctx.sv.src, ctx.sv.mod)
            if isa(cv, Const)
                vars[i] = quoted(cv.val)
                # Constants don't need to be preserved
                continue
            end
            if has_fld_use || need_preserved_root
                need_assign = true
                if is_ssa
                    if isa(orig_def, SSAValue)
                        var_slot = orig_def
                        need_assign = false
                    else
                        var_slot = newvar!(ctx.sv, field_typ)
                    end
                else
                    var_slot = add_slot!(ctx.sv.src, field_typ, false, fld_name)
                    ctx.sv.src.slotflags[end] = Slot_StaticUndef
                end
                if need_assign
                    def_ex = :($var_slot = $orig_def)
                    push!(def_exprs, def_ex)
                    scan_expr_use!(ctx.infomap, def_exprs, length(def_exprs), def_ex, ctx.sv.src)
                end
                vars[i] = var_slot
                add_allocopt_todo(ctx, var_slot)
            end
            # No side effect allowed in `Expr` arguments at this point
        end
        if need_preserved_root
            push!(preserved_vars, var_slot)
        end
    end
    # OK, so now we've created all the variables and assignments needed, replace the def.
    ctx.changes[def.stmts=>def.stmtidx] = nothing
    def.stmts[def.stmtidx] = def_exprs
    # Now fix up uses =)
    for use in info.uses
        local fld
        local use_kind
        if !isdefined(use, :expr)
            # Top level dummy use, remove
            use.stmts[use.stmtidx] = nothing
            continue
        end
        expr = use.expr
        head = expr.head
        if head === :isdefined
            replace_use_expr_with!(ctx, use, quoted(true))
            continue
        elseif head === :foreigncall || head === :gc_preserve_begin
            # Splicing in the new ccall roots without moving existing other roots
            npreserved_vars = length(preserved_vars)
            if npreserved_vars == 0
                use.expr.args[use.exidx] = nothing
            else
                cvar = preserved_vars[1]
                expr.args[use.exidx] = cvar
                add_use(ctx.infomap, cvar, use)
                if npreserved_vars > 1
                    old_nargs = length(expr.args)
                    resize!(expr.args, old_nargs + npreserved_vars - 1)
                    for i in 2:npreserved_vars
                        cvar = preserved_vars[i]
                        expr.args[old_nargs + i - 1] = cvar
                        add_use(ctx.infomap, cvar, ValueUse(use.stmts, use.stmtidx,
                                                            expr, old_nargs + i - 1))
                    end
                end
            end
            continue
        elseif head === :call
            fld = expr.args[3]
            if is_known_call(expr, isdefined, ctx.sv.src, ctx.sv.mod)
                use_kind = 0
            elseif is_known_call(expr, getfield, ctx.sv.src, ctx.sv.mod)
                use_kind = 1
            elseif is_known_call(expr, setfield!, ctx.sv.src, ctx.sv.mod)
                use_kind = 2
            end
        end
        if isa(fld, QuoteNode)
            fld = fld.value
        end
        if !isa(fld, Int)
            v = get(ctx.sym_count, fld, 0=>0)::Pair{Int,Int}
            if first(v) == 0
                @assert use_kind == 0
                replace_use_expr_with!(ctx, use, quoted(false))
                continue
            end
            fld = last(v)
        end
        if fld > nf || fld <= 0
            @assert use_kind == 0
            replace_use_expr_with!(ctx, use, quoted(false))
            continue
        end
        if use_kind == 0
            # isdefined
            if isassigned(si.defs, fld)
                replace_use_expr_with!(ctx, use, quoted(true))
                continue
            end
            flag_slot = flag_vars[fld]
            replace_use_expr_with!(ctx, use, flag_slot, true, true)
        elseif use_kind == 1
            # getfield
            if isassigned(si.defs, fld)
                replace_var = vars[fld]
                replace_use_expr_with!(ctx, use, replace_var, true, true)
            elseif haskey(ctx.setfield_typ, fld) || (!isempty(si.names) &&
                                                     haskey(ctx.setfield_typ, si.names[fld]))
                replace_var = vars[fld]
                flag_slot = flag_vars[fld]
                check_expr = Expr(:call, throw_undefreferror_ifnot, flag_slot)
                stmts = Any[check_expr]
                add_use(ctx.infomap, flag_slot, ValueUse(stmts, 1, check_expr, 2))
                stmt = use.stmts[use.stmtidx]::Expr
                if stmt !== expr
                    @assert stmt.head === :(=) && stmt.args[2] === expr
                    stmt.args[2] = replace_var
                    push!(stmts, stmt)
                    scan_expr_use!(ctx.infomap, stmts, length(stmts), stmt, ctx.sv.src)
                end
                ctx.changes[use.stmts=>use.stmtidx] = nothing
                use.stmts[use.stmtidx] = stmts
            else
                throw_err = Expr(:call, GlobalRef(Core, :throw), UndefRefError())
                throw_err.typ = Union{}
                # We can't simply delete the assignment since it can break assumptions downstream
                # If we are assigning to anything, just assign the throw instead.
                stmt = use.stmts[use.stmtidx]::Expr
                if stmt === expr
                    use.stmts[use.stmtidx] = Any[throw_err]
                else
                    @assert stmt.head === :(=) && stmt.args[2] === expr
                    stmt.args[2] = throw_err
                    stmts = Any[stmt]
                    use.stmts[use.stmtidx] = stmts
                    lhs = stmt.args[1]
                    if isa(lhs, SSAValue) || isa(lhs, Slot)
                        add_def(ctx.infomap, lhs, ValueDef(stmt, stmts, 1))
                    end
                end
                ctx.changes[use.stmts=>use.stmtidx] = nothing
            end
        else
            @assert use_kind == 2
            # setfield!
            replace_var = vars[fld]
            val = expr.args[4]
            stmts = Any[]
            if !isassigned(si.defs, fld)
                flag_slot = flag_vars[fld]
                assign_flag_ex = :($flag_slot = true)
                push!(stmts, assign_flag_ex)
                add_def(ctx.infomap, flag_slot, ValueDef(assign_flag_ex, stmts, length(stmts)))
            end
            assign_ex = :($replace_var = $val)
            push!(stmts, assign_ex)
            scan_expr_use!(ctx.infomap, stmts, length(stmts), assign_ex, ctx.sv.src)
            stmt = use.stmts[use.stmtidx]::Expr
            if stmt !== expr
                @assert stmt.head === :(=) && stmt.args[2] === expr
                extra_assign_ex = :($(stmt.args[1]) = $val)
                push!(stmts, extra_assign_ex)
                scan_expr_use!(ctx.infomap, stmts, length(stmts), extra_assign_ex, ctx.sv.src)
            end
            use.stmts[use.stmtidx] = stmts
            ctx.changes[use.stmts=>use.stmtidx] = nothing
        end
    end
end

macro check_ast(ctx, ex)
    eex = esc(ex)
    ectx = esc(ctx)
    qex = QuoteNode(ex)
    quote
        ctx = $ectx
        if !$eex
            println("Check failed: ", $qex)
            println("Code:")
            println(ctx.sv.src)
            println("Value Info Map:")
            show_info(STDOUT, ctx.infomap, ctx)
            ccall(:abort, Union{}, ())
        end
    end
end

function verify_value_infomap(ctx::AllocOptContext)
    seen = ObjectIdDict()
    in_methods = ObjectIdDict()
    infomap = ctx.infomap
    all_stmts = ObjectIdDict()
    for i in 1:length(infomap.ssas)
        isassigned(infomap.ssas, i) || continue
        info = infomap.ssas[i]
        ssav = SSAValue(i - 1)
        @check_ast(ctx, !info.has_method)
        ndef = 0
        for def in info.defs
            check_valid(def, ctx.changes) || continue
            ndef += 1
            @check_ast(ctx, !haskey(seen, def))
            seen[def] = nothing
            all_stmts[def.stmts] = nothing

            assign = def.assign
            @check_ast(ctx, isa(assign, Expr))
            @check_ast(ctx, assign.head === :(=))
            @check_ast(ctx, def.stmts[def.stmtidx] === assign)
            @check_ast(ctx, assign.args[1] === ssav)
        end
        @check_ast(ctx, ndef <= 1)
        nuse = 0
        for use in info.uses
            check_valid(use, ctx.changes) || continue
            nuse += 1
            @check_ast(ctx, !haskey(seen, use))
            seen[use] = nothing
            all_stmts[use.stmts] = nothing
            stmt = use.stmts[use.stmtidx]
            if !isdefined(use, :expr)
                @check_ast(ctx, stmt === ssav)
            else
                expr = use.expr
                @check_ast(ctx, expr.args[use.exidx] === ssav)
                if stmt === expr
                    if expr.head === :(=)
                        @check_ast(ctx, use.exidx != 1)
                    end
                elseif expr.head === :&
                    @check_ast(ctx, use.exidx === 1)
                    ccall_expr = stmt::Expr
                    if ccall_expr.head === :(=)
                        ccall_expr = ccall_expr.args[2]::Expr
                    end
                    @check_ast(ctx, ccall_expr.head === :foreigncall)
                    nccallargs = ccall_expr.args[5]::Int
                    found_ccallarg = false
                    for argi in 6:(5 + nccallargs)
                        if ccall_expr.args[argi] === expr
                            found_ccallarg = true
                            break
                        end
                    end
                    @check_ast(ctx, found_ccallarg)
                else
                    @check_ast(ctx, stmt.head === :(=) && stmt.args[2] === expr)
                end
            end
        end
        if ndef == 0
            @check_ast(ctx, nuse == 0)
        end
    end
    for i in 1:length(infomap.slots)
        isassigned(infomap.slots, i) || continue
        info = infomap.slots[i]
        slotv = SlotNumber(i)
        if info.has_method
            in_methods[slotv] = nothing
        end
        ndef = 0
        for def in info.defs
            check_valid(def, ctx.changes) || continue
            @check_ast(ctx, !haskey(seen, def))
            seen[def] = nothing
            all_stmts[def.stmts] = nothing

            assign = def.assign
            @check_ast(ctx, def.stmts[def.stmtidx] === assign)
            if isa(assign, NewvarNode)
                @check_ast(ctx, var_has_undef(ctx.sv.src, i))
                @check_ast(ctx, assign.slot === slotv)
            else
                ndef += 1
                @check_ast(ctx, assign.head === :(=))
                @check_ast(ctx, assign.args[1] === slotv)
            end
        end
        if ctx.sv.src.slotflags[i] & Slot_AssignedOnce != 0
            @check_ast(ctx, ndef <= 1)
        end
        for use in info.uses
            check_valid(use, ctx.changes) || continue
            @check_ast(ctx, !haskey(seen, use))
            seen[use] = nothing
            all_stmts[use.stmts] = nothing
            stmt = use.stmts[use.stmtidx]

            if !isdefined(use, :expr)
                @check_ast(ctx, symequal(stmt, slotv))
            else
                expr = use.expr
                @check_ast(ctx, symequal(expr.args[use.exidx], slotv))
                if stmt === expr
                    if expr.head === :(=)
                        @check_ast(ctx, use.exidx != 1)
                    end
                elseif expr.head === :&
                    @check_ast(ctx, use.exidx === 1)
                    ccall_expr = stmt::Expr
                    if ccall_expr.head === :(=)
                        ccall_expr = ccall_expr.args[2]::Expr
                    end
                    @check_ast(ctx, ccall_expr.head === :foreigncall)
                    nccallargs = ccall_expr.args[5]::Int
                    found_ccallarg = false
                    for argi in 6:(5 + nccallargs)
                        if ccall_expr.args[argi] === expr
                            found_ccallarg = true
                            break
                        end
                    end
                    @check_ast(ctx, found_ccallarg)
                else
                    @check_ast(ctx, stmt.head === :(=) && stmt.args[2] === expr)
                end
            end
        end
    end
    verify_value_infomap_rescan(ctx, ctx.sv.src.code, seen, in_methods, all_stmts)
    @check_ast(ctx, isempty(all_stmts))
    @check_ast(ctx, isempty(seen))
end

function verify_seen_info(ctx::AllocOptContext, seen, def_or_use)
    @check_ast(ctx, haskey(seen, def_or_use))
    delete!(seen, def_or_use)
end

function verify_value_infomap_rescan(ctx::AllocOptContext, stmts, seen, in_methods, all_stmts)
    if haskey(all_stmts, stmts)
        delete!(all_stmts, stmts)
    end
    for i in 1:length(stmts)
        ex = stmts[i]
        if isa(ex, Vector{Any})
            verify_value_infomap_rescan(ctx, ex, seen, in_methods, all_stmts)
            continue
        elseif isa(ex, SSAValue)
            verify_seen_info(ctx, seen, ValueUse(stmts, i))
            continue
        elseif isa(ex, Slot)
            verify_seen_info(ctx, seen, ValueUse(stmts, i))
            continue
        elseif isa(ex, NewvarNode)
            verify_seen_info(ctx, seen, ValueDef(ex, stmts, i))
            continue
        elseif !isa(ex, Expr)
            continue
        end
        head = ex.head
        is_meta_expr_head(head) && continue
        if head === :(=)
            rhs = ex.args[2]
            lhs = ex.args[1]
            lhs_slot = isa(lhs, Slot)
            rhs_slot = isa(rhs, Slot)
            if lhs_slot && rhs_slot
                @check_ast(ctx, !symequal(lhs, rhs))
            end
            if lhs_slot || isa(lhs, SSAValue)
                verify_seen_info(ctx, seen, ValueDef(ex, stmts, i))
            end
            if isa(rhs, Expr)
                ex = rhs
                head = ex.head
            else
                if rhs_slot || isa(rhs, SSAValue)
                    verify_seen_info(ctx, seen, ValueUse(stmts, i, ex, 2))
                end
                continue
            end
        end
        eargs = ex.args
        isccall = head === :foreigncall
        for j in 1:length(eargs)
            ea = eargs[j]
            if j == 1 && head === :method
                if isa(ea, Slot)
                    @check_ast(ctx, haskey(in_methods, SlotNumber(slot_id(ea))))
                    continue
                end
            end
            if isccall && isa(ea, Expr)
                @check_ast(ctx, ea.head === :& || ea.head === :boundscheck)
                ea2 = ea.args[1]
                if isa(ea2, Slot) || isa(ea2, SSAValue)
                    verify_seen_info(ctx, seen, ValueUse(stmts, i, ea, 1))
                end
            elseif isa(ea, Slot) || isa(ea, SSAValue)
                verify_seen_info(ctx, seen, ValueUse(stmts, i, ex, j))
            end
        end
    end
end

function optimize_value!(ctx::AllocOptContext, key)
    info = ctx.infomap[key]
    if info.has_method
        return
    end
    remove_invalid!(info, ctx.changes)
    if isempty(info.uses)
        delete_value!(ctx, info, key)
        return
    elseif isempty(info.defs)
        # Undefined but used variable these can only be variables whose uses are guarded by
        # conditions that will never be true. Ignore them for now.
        # TODO: slots introduced by inlining multiple-`return` functions might have their
        # defs removed by DCE but not be marked StaticUndef.
        #@assert ((!key.second && key.first <= ctx.sv.nargs) ||
        #         var_has_static_undef(ctx.sv.src, key.first, key.second))
        return
    elseif var_has_undef(ctx.sv.src, key.first, key.second)
        return
    end
    # Split assignments of leaftypes that do no have overlaping uses with each other
    split_disjoint_assign!(ctx, info, key) && return
    # If we've found SSAValue or Slot as def, no need to try other optimizations
    merge_value_ssa!(ctx, info, key) && return
    # Check if it's just a constant
    propagate_const_def!(ctx, info, key) && return
    # Split/eliminate allocation
    split_struct_alloc!(ctx, info, key) && return
    return
end

const enable_verify_valueinfo = Array{Bool,0}(uninitialized)
enable_verify_valueinfo[] = false

# Simplify the AST and eliminate unnecessary allocations
# This does the following optimizations iteratively until there's no changes to be made
# 1. Remove def of variables that is never used
# 2. Merge variables that have identical SSA definitions
# 3. Replace variables that always take the same constant definition with the constant
# 4. Deconstruct variables with defs that are only constant or inlined allocations
#    and with only known non-escape uses into its fields.
# This pass requires the IR to be linear, in particular, the only allowed nested `Expr` are
# RHS of a `Expr(:(=))` and the `Expr(:(&))` argument of a `ccall`.
function alloc_elim_pass!(sv::OptimizationState)
    body = sv.src.code
    infomap = collect_value_infos(body, sv.src, sv.nargs)
    ctx = AllocOptContext(infomap, sv)
    enable_verify_valueinfo[] && verify_value_infomap(ctx)
    while !isempty(ctx.todo)
        k, v = first(ctx.todo)
        k = k::Pair{Int,Bool}
        delete!(ctx.todo, k)
        optimize_value!(ctx, k)
        enable_verify_valueinfo[] && verify_value_infomap(ctx)
    end
    len = length(body)
    next_i = 1
    while next_i <= len
        i = next_i
        next_i += 1
        ex = body[i]
        if isa(ex, Vector{Any})
            len += length(ex) - 1
            next_i = i
            splice!(body, i, ex)
        elseif (isa(ex, Expr) && ex.head === :call && length(ex.args) == 2 &&
                ex.args[1] === throw_undefreferror_ifnot)
            len += 3
            next_i = i + 4
            flag_slot = ex.args[2]
            _growat!(body, i, 3)

            not_flag = newvar!(ctx.sv, Bool)
            not_flag_val = :($(GlobalRef(Core.Intrinsics, :not_int))($flag_slot))
            not_flag_val.typ = Bool
            body[i] = :($not_flag = $not_flag_val)

            after_lbl = genlabel(ctx.sv)
            body[i + 1] = Expr(:gotoifnot, not_flag, after_lbl.label)

            throw_err = Expr(:call, GlobalRef(Core, :throw), UndefRefError())
            throw_err.typ = Union{}
            body[i + 2] = throw_err
            body[i + 3] = after_lbl
        elseif (isa(ex, Expr) && ex.head === :call && length(ex.args) == 3 &&
                ex.args[1] === check_new_field_type)
            val_ex = ex.args[2]
            typ_ex = ex.args[3]

            len += 4
            next_i = i + 5
            _growat!(body, i, 4)

            flag = newvar!(ctx.sv, Bool)
            flag_val = :($(GlobalRef(Core, :isa))($val_ex, $typ_ex))
            flag_val.typ = Bool
            body[i] = :($flag = $flag_val)

            not_flag = newvar!(ctx.sv, Bool)
            not_flag_val = :($(GlobalRef(Core.Intrinsics, :not_int))($flag))
            not_flag_val.typ = Bool
            body[i + 1] = :($not_flag = $not_flag_val)

            after_lbl = genlabel(ctx.sv)
            body[i + 2] = Expr(:gotoifnot, not_flag, after_lbl.label)

            throw_err = Expr(:foreigncall, QuoteNode(:jl_type_error_new_expr),
                             Union{}, Core.svec(Any, Any), QuoteNode(:ccall), 2, typ_ex, val_ex)
            throw_err.typ = Union{}
            body[i + 3] = throw_err
            body[i + 4] = after_lbl
        end
    end
end

const meta_pop_loc = Expr(:meta, :pop_loc)

function copy_expr_in_array!(ary, seen)
    for i in 1:length(ary)
        ex = ary[i]
        isa(ex, Expr) || continue
        ex = ex::Expr
        if ex.head === :meta
            # Try to save some memory by using the same object for all `:pop_loc` meta node
            if ex !== meta_pop_loc && length(ex.args) == 1 && ex.args[1] === :pop_loc
                ary[i] = meta_pop_loc
            end
            continue # No need to copy meta expressions
        end
        if haskey(seen, ex)
            newex = Expr(ex.head)
            append!(newex.args, ex.args)
            newex.typ = ex.typ
            ary[i] = ex = newex
            # No need to add to `seen`, there's no way there can be another one of the copied
            # version in the AST....
        else
            seen[ex] = nothing
            if haskey(seen, ex.args)
                # Haven't actually seen this happen but it's pretty easy to check
                ex.args = copy(ex.args)
            else
                seen[ex.args] = nothing
            end
        end
        copy_expr_in_array!(ex.args, seen)
    end
end

# Clone expressions that appears multiple times in the code
function copy_duplicated_expr_pass!(sv::OptimizationState)
    copy_expr_in_array!(sv.src.code, ObjectIdDict())
end

# fix label numbers to always equal the statement index of the label
function reindex_labels!(sv::OptimizationState)
    body = sv.src.code
    mapping = get_label_map(body)
    for i = 1:length(body)
        el = body[i]
        # For goto and enter, the statement and the target has to be
        # both reachable or both not.
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
                @assert labelnum !== 0
                el.args[2] = labelnum
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

let fs = Any[typeinf_ext, typeinf, typeinf_edge, pure_eval_call],
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
