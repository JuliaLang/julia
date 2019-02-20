# This file is a part of Julia. License is MIT: https://julialang.org/license

const LineNum = Int

mutable struct InferenceState
    params::Params # describes how to compute the result
    result::InferenceResult # remember where to put the result
    linfo::MethodInstance   # used here for the tuple (specTypes, env, Method) and world-age validity
    sptypes::Vector{Any}    # types of static parameter
    slottypes::Vector{Any}
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
    cur_hand #::Union{Nothing, Pair{LineNum, prev_handler}}
    handler_at::Vector{Any}
    n_handlers::Int
    # ssavalue sparsity and restart info
    ssavalue_uses::Vector{BitSet}

    cycle_backedges::Vector{Tuple{InferenceState, LineNum}} # call-graph backedges connecting from callee to caller
    callers_in_cycle::Vector{InferenceState}
    parent::Union{Nothing, InferenceState}

    # TODO: move these to InferenceResult / Params?
    cached::Bool
    limited::Bool
    inferred::Bool
    dont_work_on_me::Bool

    # src is assumed to be a newly-allocated CodeInfo, that can be modified in-place to contain intermediate results
    function InferenceState(result::InferenceResult, src::CodeInfo,
                            cached::Bool, params::Params)
        linfo = result.linfo
        code = src.code::Array{Any,1}
        toplevel = !isa(linfo.def, Method)

        sp = sptypes_from_meth_instance(linfo::MethodInstance)

        nssavalues = src.ssavaluetypes::Int
        src.ssavaluetypes = Any[ NOT_FOUND for i = 1:nssavalues ]

        n = length(code)
        s_edges = Any[ nothing for i = 1:n ]
        s_types = Any[ nothing for i = 1:n ]

        # initial types
        nslots = length(src.slotflags)
        argtypes = result.argtypes
        nargs = length(argtypes)
        s_argtypes = VarTable(undef, nslots)
        slottypes = Vector{Any}(undef, nslots)
        for i in 1:nslots
            at = (i > nargs) ? Bottom : argtypes[i]
            s_argtypes[i] = VarState(at, i > nargs)
            slottypes[i] = at
        end
        s_types[1] = s_argtypes

        ssavalue_uses = find_ssavalue_uses(code, nssavalues)

        # exception handlers
        cur_hand = nothing
        handler_at = Any[ nothing for i=1:n ]
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
            sp, slottypes, inmodule, 0,
            src, min_valid, max_valid,
            nargs, s_types, s_edges,
            Union{}, W, 1, n,
            cur_hand, handler_at, n_handlers,
            ssavalue_uses,
            Vector{Tuple{InferenceState,LineNum}}(), # cycle_backedges
            Vector{InferenceState}(), # callers_in_cycle
            #=parent=#nothing,
            cached, false, false, false)
        result.result = frame
        cached && push!(params.cache, result)
        return frame
    end
end

function InferenceState(result::InferenceResult, cached::Bool, params::Params)
    # prepare an InferenceState object for inferring lambda
    src = retrieve_code_info(result.linfo)
    src === nothing && return nothing
    validate_code_in_debug_mode(result.linfo, src, "lowered")
    return InferenceState(result, src, cached, params)
end

function sptypes_from_meth_instance(linfo::MethodInstance)
    toplevel = !isa(linfo.def, Method)
    if !toplevel && isempty(linfo.sparam_vals) && isa(linfo.def.sig, UnionAll)
        # linfo is unspecialized
        sp = Any[]
        sig = linfo.def.sig
        while isa(sig, UnionAll)
            push!(sp, sig.var)
            sig = sig.body
        end
    else
        sp = collect(Any, linfo.sparam_vals)
    end
    for i = 1:length(sp)
        v = sp[i]
        if v isa TypeVar
            fromArg = 0
            # if this parameter came from arg::Type{T}, then `arg` is more precise than
            # Type{T} where lb<:T<:ub
            sig = linfo.def.sig
            temp = sig
            for j = 1:i-1
                temp = temp.body
            end
            Pi = temp.var
            while temp isa UnionAll
                temp = temp.body
            end
            sigtypes = temp.parameters
            for j = 1:length(sigtypes)
                tj = sigtypes[j]
                if isType(tj) && tj.parameters[1] === Pi
                    fromArg = j
                    break
                end
            end
            if fromArg > 0
                ty = fieldtype(linfo.specTypes, fromArg)
            else
                ub = v.ub
                while ub isa TypeVar
                    ub = ub.ub
                end
                if has_free_typevars(ub)
                    ub = Any
                end
                lb = v.lb
                while lb isa TypeVar
                    lb = lb.lb
                end
                if has_free_typevars(lb)
                    lb = Bottom
                end
                if Any <: ub && lb <: Bottom
                    ty = Any
                else
                    tv = TypeVar(v.name, lb, ub)
                    ty = UnionAll(tv, Type{tv})
                end
            end
        else
            ty = Const(v)
        end
        sp[i] = ty
    end
    return sp
end

_topmod(sv::InferenceState) = _topmod(sv.mod)

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

update_valid_age!(edge::InferenceState, sv::InferenceState) = update_valid_age!(edge.min_valid, edge.max_valid, sv)
update_valid_age!(li::MethodInstance, sv::InferenceState) = update_valid_age!(min_world(li), max_world(li), sv)

function record_ssa_assign(ssa_id::Int, @nospecialize(new), frame::InferenceState)
    old = frame.src.ssavaluetypes[ssa_id]
    if old === NOT_FOUND || !(new ⊑ old)
        frame.src.ssavaluetypes[ssa_id] = tmerge(old, new)
        W = frame.ip
        s = frame.stmt_types
        for r in frame.ssavalue_uses[ssa_id]
            if s[r] !== nothing # s[r] === nothing => unreached statement
                if r < frame.pc´´
                    frame.pc´´ = r
                end
                push!(W, r)
            end
        end
    end
    nothing
end

function add_cycle_backedge!(frame::InferenceState, caller::InferenceState, currpc::Int)
    update_valid_age!(frame, caller)
    backedge = (caller, currpc)
    contains_is(frame.cycle_backedges, backedge) || push!(frame.cycle_backedges, backedge)
    return frame
end

# temporarily accumulate our edges to later add as backedges in the callee
function add_backedge!(li::MethodInstance, caller::InferenceState)
    isa(caller.linfo.def, Method) || return # don't add backedges to toplevel exprs
    if caller.stmt_edges[caller.currpc] === nothing
        caller.stmt_edges[caller.currpc] = []
    end
    push!(caller.stmt_edges[caller.currpc], li)
    update_valid_age!(li, caller)
    nothing
end

# used to temporarily accumulate our no method errors to later add as backedges in the callee method table
function add_mt_backedge!(mt::Core.MethodTable, @nospecialize(typ), caller::InferenceState)
    isa(caller.linfo.def, Method) || return # don't add backedges to toplevel exprs
    if caller.stmt_edges[caller.currpc] === nothing
        caller.stmt_edges[caller.currpc] = []
    end
    push!(caller.stmt_edges[caller.currpc], mt)
    push!(caller.stmt_edges[caller.currpc], typ)
    nothing
end

function poison_callstack(infstate::InferenceState, topmost::InferenceState, poison_topmost::Bool)
    poison_topmost && (topmost = topmost.parent)
    while !(infstate === topmost)
        if call_result_unused(infstate)
            # If we won't propagate the result any further (since it's typically unused),
            # it's OK that we keep and cache the "limited" result in the parents
            # (non-typically, this means that we lose the ability to detect a guaranteed StackOverflow in some cases)
            # TODO: we might be able to halt progress much more strongly here,
            # since now we know we won't be able to keep anything much that we learned.
            # We were mainly only here to compute the calling convention return type,
            # but in most situations now, we are unlikely to be able to use that information.
            break
        end
        infstate.limited = true
        for infstate_cycle in infstate.callers_in_cycle
            infstate_cycle.limited = true
        end
        infstate = infstate.parent
        infstate === nothing && return
    end
end

function is_specializable_vararg_slot(@nospecialize(arg), nargs::Int, vargs::Vector{Any})
    return (isa(arg, Slot) && slot_id(arg) == nargs && !isempty(vargs))
end

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
