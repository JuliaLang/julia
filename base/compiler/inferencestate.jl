# This file is a part of Julia. License is MIT: https://julialang.org/license

const LineNum = Int

mutable struct InferenceState
    params::Params # describes how to compute the result
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

    # TODO: move these to InferenceResult / Params?
    optimize::Bool
    cached::Bool
    limited::Bool
    inferred::Bool
    dont_work_on_me::Bool

    # src is assumed to be a newly-allocated CodeInfo, that can be modified in-place to contain intermediate results
    function InferenceState(result::InferenceResult, src::CodeInfo,
                            optimize::Bool, cached::Bool, params::Params)
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
            if _any(t->isa(t,TypeVar), sp)
                sp = collect(Any, sp)
            end
        end
        if !isa(sp, SimpleVector)
            for i = 1:length(sp)
                v = sp[i]
                if v isa TypeVar
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
                    sp[i] = TypeVar(v.name, lb, ub)
                end
            end
            sp = svec(sp...)
        end

        nssavalues = src.ssavaluetypes::Int
        src.ssavaluetypes = Any[ NOT_FOUND for i = 1:nssavalues ]

        n = length(code)
        s_edges = Any[ () for i = 1:n ]
        s_types = Any[ () for i = 1:n ]

        # initial types
        nslots = length(src.slotnames)
        argtypes = get_argtypes(result)
        vararg_type_container = nothing
        nargs = length(argtypes)
        s_argtypes = VarTable(undef, nslots)
        src.slottypes = Vector{Any}(undef, nslots)
        for i in 1:nslots
            at = (i > nargs) ? Bottom : argtypes[i]
            if !toplevel && linfo.def.isva && i == nargs
                if !(at == Tuple) # would just be a no-op
                    vararg_type_container = unwrap_unionall(at)
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

function InferenceState(linfo::MethodInstance, optimize::Bool, cached::Bool, params::Params)
    return InferenceState(InferenceResult(linfo), optimize, cached, params)
end

function InferenceState(result::InferenceResult, optimize::Bool, cached::Bool, params::Params)
    # prepare an InferenceState object for inferring lambda
    src = retrieve_code_info(result.linfo)
    src === nothing && return nothing
    validate_code_in_debug_mode(result.linfo, src, "lowered")
    return InferenceState(result, src, optimize, cached, params)
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

function add_backedge!(frame::InferenceState, caller::InferenceState, currpc::Int)
    update_valid_age!(frame, caller)
    backedge = (caller, currpc)
    contains_is(frame.backedges, backedge) || push!(frame.backedges, backedge)
    return frame
end

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

# used to temporarily accumulate our no method errors to later add as backedges in the callee method table
function add_mt_backedge!(mt::Core.MethodTable, @nospecialize(typ), caller::InferenceState)
    isa(caller.linfo.def, Method) || return # don't add backedges to toplevel exprs
    if caller.stmt_edges[caller.currpc] === ()
        caller.stmt_edges[caller.currpc] = []
    end
    push!(caller.stmt_edges[caller.currpc], mt)
    push!(caller.stmt_edges[caller.currpc], typ)
    nothing
end

function is_specializable_vararg_slot(@nospecialize(arg), sv::InferenceState)
    return (isa(arg, Slot) && slot_id(arg) == sv.nargs &&
            isa(sv.vararg_type_container, DataType))
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
