# This file is a part of Julia. License is MIT: https://julialang.org/license

# build (and start inferring) the inference frame for the linfo
function typeinf(interp::AbstractInterpreter, result::InferenceResult, cached::Bool)
    frame = InferenceState(result, cached, interp)
    frame === nothing && return false
    cached && (result.linfo.inInference = true)
    return typeinf(interp, frame)
end

function typeinf(interp::AbstractInterpreter, frame::InferenceState)
    typeinf_nocycle(interp, frame) || return false # frame is now part of a higher cycle
    # with no active ip's, frame is done
    frames = frame.callers_in_cycle
    isempty(frames) && push!(frames, frame)
    for caller in frames
        @assert !(caller.dont_work_on_me)
        caller.dont_work_on_me = true
    end
    for caller in frames
        finish(caller, interp)
    end
    # collect results for the new expanded frame
    results = InferenceResult[ frames[i].result for i in 1:length(frames) ]
    # empty!(frames)
    min_valid = frame.min_valid
    max_valid = frame.max_valid
    cached = frame.cached
    if cached || frame.parent !== nothing
        for caller in results
            opt = caller.src
            if opt isa OptimizationState
                optimize(opt, OptimizationParams(interp), caller.result)
                finish(opt.src, interp)
                # finish updating the result struct
                validate_code_in_debug_mode(opt.linfo, opt.src, "optimized")
                if opt.const_api
                    if caller.result isa Const
                        caller.src = caller.result
                    else
                        @assert isconstType(caller.result)
                        caller.src = Const(caller.result.parameters[1])
                    end
                elseif opt.src.inferred
                    caller.src = opt.src::CodeInfo # stash a copy of the code (for inlining)
                else
                    caller.src = nothing
                end
                if min_valid < opt.min_valid
                    min_valid = opt.min_valid
                end
                if max_valid > opt.max_valid
                    max_valid = opt.max_valid
                end
            end
        end
    end
    if max_valid == get_world_counter()
        max_valid = typemax(UInt)
    end
    for caller in frames
        caller.min_valid = min_valid
        caller.max_valid = max_valid
        caller.src.min_world = min_valid
        caller.src.max_world = max_valid
        if cached
            cache_result(interp, caller.result, min_valid, max_valid)
        end
        if max_valid == typemax(UInt)
            # if we aren't cached, we don't need this edge
            # but our caller might, so let's just make it anyways
            for caller in frames
                store_backedges(caller)
            end
        end
        # finalize and record the linfo result
        caller.inferred = true
    end
    return true
end

# inference completed on `me`
# update the MethodInstance and notify the edges
function cache_result(interp::AbstractInterpreter, result::InferenceResult, min_valid::UInt, max_valid::UInt)
    def = result.linfo.def
    toplevel = !isa(result.linfo.def, Method)

    # check if the existing linfo metadata is also sufficient to describe the current inference result
    # to decide if it is worth caching this
    already_inferred = !result.linfo.inInference
    if inf_for_methodinstance(interp, result.linfo, min_valid, max_valid) isa CodeInstance
        already_inferred = true
    end

    # TODO: also don't store inferred code if we've previously decided to interpret this function
    if !already_inferred
        inferred_result = result.src
        if inferred_result isa Const
            # use constant calling convention
            rettype_const = (result.src::Const).val
            const_flags = 0x3
        else
            if isa(result.result, Const)
                rettype_const = (result.result::Const).val
                const_flags = 0x2
            elseif isconstType(result.result)
                rettype_const = result.result.parameters[1]
                const_flags = 0x2
            else
                rettype_const = nothing
                const_flags = 0x00
            end
            if !toplevel && inferred_result isa CodeInfo
                cache_the_tree = result.src.inferred &&
                    (result.src.inlineable ||
                     ccall(:jl_isa_compileable_sig, Int32, (Any, Any), result.linfo.specTypes, def) != 0)
                if cache_the_tree
                    # compress code for non-toplevel thunks
                    nslots = length(inferred_result.slotflags)
                    resize!(inferred_result.slottypes, nslots)
                    resize!(inferred_result.slotnames, nslots)
                    inferred_result = ccall(:jl_compress_ir, Any, (Any, Any), def, inferred_result)
                else
                    inferred_result = nothing
                end
            end
        end
        if !isa(inferred_result, Union{CodeInfo, Vector{UInt8}})
            inferred_result = nothing
        end
        ccall(:jl_set_method_inferred, Ref{CodeInstance}, (Any, Any, Any, Any, Int32, UInt, UInt),
            result.linfo, widenconst(result.result), rettype_const, inferred_result,
            const_flags, min_valid, max_valid)
    end
    result.linfo.inInference = false
    nothing
end

function finish(me::InferenceState, interp::AbstractInterpreter)
    # prepare to run optimization passes on fulltree
    if me.limited && me.cached && me.parent !== nothing
        # a top parent will be cached still, but not this intermediate work
        # we can throw everything else away now
        me.cached = false
        me.linfo.inInference = false
        me.src.inlineable = false
    else
        # annotate fulltree with type information
        type_annotate!(me)
        run_optimizer = (me.cached || me.parent !== nothing)
        if run_optimizer
            # construct the optimizer for later use, if we're building this IR to cache it
            # (otherwise, we'll run the optimization passes later, outside of inference)
            opt = OptimizationState(me, OptimizationParams(interp), interp)
            me.result.src = opt
        end
    end
    me.result.result = me.bestguess
    nothing
end

function finish(src::CodeInfo, interp::AbstractInterpreter)
    # convert all type information into the form consumed by the cache for inlining and code-generation
    widen_all_consts!(src)
    src.inferred = true
    nothing
end

# record the backedges
function store_backedges(frame::InferenceState)
    toplevel = !isa(frame.linfo.def, Method)
    if !toplevel && (frame.cached || frame.parent !== nothing)
        caller = frame.result.linfo
        for edges in frame.stmt_edges
            store_backedges(caller, edges)
        end
        store_backedges(caller, frame.src.edges)
        frame.src.edges = nothing
    end
end

store_backedges(caller, edges::Nothing) = nothing
function store_backedges(caller, edges::Vector)
    i = 1
    while i <= length(edges)
        to = edges[i]
        if isa(to, MethodInstance)
            ccall(:jl_method_instance_add_backedge, Cvoid, (Any, Any), to, caller)
            i += 1
        else
            typeassert(to, Core.MethodTable)
            typ = edges[i + 1]
            ccall(:jl_method_table_add_backedge, Cvoid, (Any, Any, Any), to, typ, caller)
            i += 2
        end
    end
end

# widen all Const elements in type annotations
function widen_all_consts!(src::CodeInfo)
    for i = 1:length(src.ssavaluetypes)
        src.ssavaluetypes[i] = widenconst(src.ssavaluetypes[i])
    end

    for i = 1:length(src.code)
        x = src.code[i]
        if isa(x, PiNode)
            src.code[i] = PiNode(x.val, widenconst(x.typ))
        end
    end

    src.rettype = widenconst(src.rettype)

    return src
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
            e.args[i] = visit_slot_load!(subex, vtypes, sv, undefs)
        end
    end
end

function visit_slot_load!(sl::Slot, vtypes::VarTable, sv::InferenceState, undefs::Array{Bool,1})
    id = slot_id(sl)
    s = vtypes[id]
    vt = widenconditional(s.typ)
    if s.undef
        # find used-undef variables
        undefs[id] = true
    end
    # add type annotations where needed
    if !(sv.slottypes[id] ⊑ vt)
        return TypedSlot(id, vt)
    end
    return sl
end

function record_slot_assign!(sv::InferenceState)
    # look at all assignments to slots
    # and union the set of types stored there
    # to compute a lower bound on the storage required
    states = sv.stmt_types
    body = sv.src.code::Vector{Any}
    slottypes = sv.slottypes::Vector{Any}
    for i = 1:length(body)
        expr = body[i]
        st_i = states[i]
        # find all reachable assignments to locals
        if isa(st_i, VarTable) && isa(expr, Expr) && expr.head === :(=)
            lhs = expr.args[1]
            rhs = expr.args[2]
            if isa(lhs, Slot)
                vt = widenconst(sv.src.ssavaluetypes[i])
                if vt !== Bottom
                    id = slot_id(lhs)
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
    # delete dead statements only if we're building this IR to cache it
    # (otherwise, we'll run the optimization passes later, outside of inference)
    run_optimizer = (sv.cached || sv.parent !== nothing)

    # remove all unused ssa values
    gt = sv.src.ssavaluetypes
    for j = 1:length(gt)
        if gt[j] === NOT_FOUND
            gt[j] = Union{}
        end
        gt[j] = widenconditional(gt[j])
    end

    # compute the required type for each slot
    # to hold all of the items assigned into it
    record_slot_assign!(sv)
    sv.src.slottypes = sv.slottypes
    sv.src.rettype = sv.bestguess

    # annotate variables load types
    # remove dead code optimization
    # and compute which variables may be used undef
    src = sv.src
    states = sv.stmt_types
    nargs = sv.nargs
    nslots = length(states[1]::Array{Any,1})
    undefs = fill(false, nslots)
    body = src.code::Array{Any,1}
    nexpr = length(body)

    # replace gotoifnot with its condition if the branch target is unreachable
    for i = 1:nexpr
        expr = body[i]
        if isa(expr, Expr) && expr.head === :gotoifnot
            tgt = expr.args[2]::Int
            if !isa(states[tgt], VarTable)
                body[i] = expr.args[1]
            end
        end
    end

    i = 1
    oldidx = 0
    changemap = fill(0, nexpr)

    while i <= nexpr
        oldidx += 1
        st_i = states[i]
        expr = body[i]
        if isa(st_i, VarTable)
            # st_i === nothing  =>  unreached statement  (see issue #7836)
            if isa(expr, Expr)
                annotate_slot_load!(expr, st_i, sv, undefs)
            elseif isa(expr, Slot)
                body[i] = visit_slot_load!(expr, st_i, sv, undefs)
            end
        else
            if isa(expr, Expr) && is_meta_expr_head(expr.head)
                # keep any lexically scoped expressions
            elseif run_optimizer
                deleteat!(body, i)
                deleteat!(states, i)
                deleteat!(src.ssavaluetypes, i)
                deleteat!(src.codelocs, i)
                nexpr -= 1
                if oldidx < length(changemap)
                    changemap[oldidx + 1] = -1
                end
                continue
            else
                body[i] = Const(expr) # annotate that this statement actually is dead
            end
        end
        i += 1
    end

    if run_optimizer
        renumber_ir_elements!(body, changemap)
    end

    # finish marking used-undef variables
    for j = 1:nslots
        if undefs[j]
            src.slotflags[j] |= SLOT_USEDUNDEF | SLOT_STATICUNDEF
        end
    end
    nothing
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

function merge_call_chain!(parent::InferenceState, ancestor::InferenceState, child::InferenceState, limited::Bool)
    # add backedge of parent <- child
    # then add all backedges of parent <- parent.parent
    # and merge all of the callers into ancestor.callers_in_cycle
    # and ensure that walking the parent list will get the same result (DAG) from everywhere
    while true
        add_cycle_backedge!(child, parent, parent.currpc)
        union_caller_cycle!(ancestor, child)
        child = parent
        parent = child.parent
        child === ancestor && break
    end
    if limited
        for caller in ancestor.callers_in_cycle
            caller.limited = true
        end
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
    limited = false
    while isa(frame, InferenceState)
        uncached |= !frame.cached # ensure we never add an uncached frame to a cycle
        limited |= frame.limited
        if frame.linfo === linfo
            if uncached
                # our attempt to speculate into a constant call lead to an undesired self-cycle
                # that cannot be converged: poison our call-stack (up to the discovered duplicate frame)
                # with the limited flag and abort (set return type to Any) now
                poison_callstack(parent, frame, false)
                return true
            end
            merge_call_chain!(parent, frame, frame, limited)
            return frame
        end
        for caller in frame.callers_in_cycle
            if caller.linfo === linfo
                if uncached
                    poison_callstack(parent, frame, false)
                    return true
                end
                merge_call_chain!(parent, frame, caller, limited)
                return caller
            end
        end
        frame = frame.parent
    end
    return false
end

# compute (and cache) an inferred AST and return the current best estimate of the result type
function typeinf_edge(interp::AbstractInterpreter, method::Method, @nospecialize(atypes), sparams::SimpleVector, caller::InferenceState)
    mi = specialize_method(method, atypes, sparams)::MethodInstance
    code = inf_for_methodinstance(interp, mi, get_world_counter(interp))
    if code isa CodeInstance # return existing rettype if the code is already inferred
        update_valid_age!(min_world(code), max_world(code), caller)
        if isdefined(code, :rettype_const)
            return Const(code.rettype_const), mi
        else
            return code.rettype, mi
        end
    end
    if !caller.cached && caller.parent === nothing
        # this caller exists to return to the user
        # (if we asked resolve_call_cyle, it might instead detect that there is a cycle that it can't merge)
        frame = false
    else
        frame = resolve_call_cycle!(mi, caller)
    end
    if frame === false
        # completely new
        mi.inInference = true
        result = InferenceResult(mi)
        frame = InferenceState(result, #=cached=#true, interp) # always use the cache for edge targets
        if frame === nothing
            # can't get the source for this, so we know nothing
            mi.inInference = false
            return Any, nothing
        end
        if caller.cached || caller.limited # don't involve uncached functions in cycle resolution
            frame.parent = caller
        end
        typeinf(interp, frame)
        update_valid_age!(frame, caller)
        return widenconst_bestguess(frame.bestguess), frame.inferred ? mi : nothing
    elseif frame === true
        # unresolvable cycle
        return Any, nothing
    end
    # return the current knowledge about this cycle
    frame = frame::InferenceState
    update_valid_age!(frame, caller)
    return widenconst_bestguess(frame.bestguess), nothing
end

function widenconst_bestguess(bestguess)
    !isa(bestguess, Const) && !isa(bestguess, Type) && return widenconst(bestguess)
    return bestguess
end

#### entry points for inferring a MethodInstance given a type signature ####

# compute an inferred AST and return type
function typeinf_code(interp::AbstractInterpreter, method::Method, @nospecialize(atypes), sparams::SimpleVector, run_optimizer::Bool)
    mi = specialize_method(method, atypes, sparams)::MethodInstance
    ccall(:jl_typeinf_begin, Cvoid, ())
    result = InferenceResult(mi)
    frame = InferenceState(result, false, interp)
    frame === nothing && return (nothing, Any)
    if typeinf(interp, frame) && run_optimizer
        opt_params = OptimizationParams(interp)
        opt = OptimizationState(frame, opt_params, interp)
        optimize(opt, opt_params, result.result)
        opt.src.inferred = true
    end
    ccall(:jl_typeinf_end, Cvoid, ())
    frame.inferred || return (nothing, Any)
    return (frame.src, widenconst(result.result))
end

# compute (and cache) an inferred AST and return type
function typeinf_ext(interp::AbstractInterpreter, mi::MethodInstance)
    method = mi.def::Method
    for i = 1:2 # test-and-lock-and-test
        i == 2 && ccall(:jl_typeinf_begin, Cvoid, ())
        code = inf_for_methodinstance(interp, mi, get_world_counter(interp))
        if code isa CodeInstance
            # see if this code already exists in the cache
            inf = code.inferred
            if invoke_api(code) == 2
                i == 2 && ccall(:jl_typeinf_end, Cvoid, ())
                tree = ccall(:jl_new_code_info_uninit, Ref{CodeInfo}, ())
                tree.code = Any[ Expr(:return, quoted(code.rettype_const)) ]
                nargs = Int(method.nargs)
                tree.slotnames = ccall(:jl_uncompress_argnames, Vector{Symbol}, (Any,), method.slot_syms)
                tree.slotflags = fill(0x00, nargs)
                tree.ssavaluetypes = 1
                tree.codelocs = Int32[1]
                tree.linetable = [LineInfoNode(method, method.file, Int(method.line), 0)]
                tree.inferred = true
                tree.ssaflags = UInt8[0]
                tree.pure = true
                tree.inlineable = true
                tree.parent = mi
                tree.rettype = Core.Typeof(code.rettype_const)
                tree.min_world = code.min_world
                tree.max_world = code.max_world
                return tree
            elseif isa(inf, CodeInfo)
                i == 2 && ccall(:jl_typeinf_end, Cvoid, ())
                if !(inf.min_world == code.min_world &&
                     inf.max_world == code.max_world &&
                     inf.rettype === code.rettype)
                    inf = copy(inf)
                    inf.min_world = code.min_world
                    inf.max_world = code.max_world
                    inf.rettype = code.rettype
                end
                return inf
            elseif isa(inf, Vector{UInt8})
                i == 2 && ccall(:jl_typeinf_end, Cvoid, ())
                inf = _uncompressed_ir(code, inf)
                return inf
            end
        end
    end
    mi.inInference = true
    frame = InferenceState(InferenceResult(mi), #=cached=#true, interp)
    frame === nothing && return nothing
    typeinf(interp, frame)
    ccall(:jl_typeinf_end, Cvoid, ())
    frame.src.inferred || return nothing
    return frame.src
end

# compute (and cache) an inferred AST and return the inferred return type
function typeinf_type(interp::AbstractInterpreter, method::Method, @nospecialize(atypes), sparams::SimpleVector)
    if contains_is(unwrap_unionall(atypes).parameters, Union{})
        return Union{} # don't ask: it does weird and unnecessary things, if it occurs during bootstrap
    end
    mi = specialize_method(method, atypes, sparams)::MethodInstance
    for i = 1:2 # test-and-lock-and-test
        i == 2 && ccall(:jl_typeinf_begin, Cvoid, ())
        code = inf_for_methodinstance(interp, mi, get_world_counter(interp))
        if code isa CodeInstance
            # see if this rettype already exists in the cache
            i == 2 && ccall(:jl_typeinf_end, Cvoid, ())
            return code.rettype
        end
    end
    frame = InferenceResult(mi)
    typeinf(interp, frame, true)
    ccall(:jl_typeinf_end, Cvoid, ())
    frame.result isa InferenceState && return nothing
    return widenconst(frame.result)
end

# This is a bridge for the C code calling `jl_typeinf_func()`
typeinf_ext_toplevel(mi::MethodInstance, world::UInt) = typeinf_ext_toplevel(NativeInterpreter(world), mi)
function typeinf_ext_toplevel(interp::AbstractInterpreter, linfo::MethodInstance)
    if isa(linfo.def, Method)
        # method lambda - infer this specialization via the method cache
        src = typeinf_ext(interp, linfo)
    else
        src = linfo.uninferred::CodeInfo
        if !src.inferred
            # toplevel lambda - infer directly
            ccall(:jl_typeinf_begin, Cvoid, ())
            if !src.inferred
                result = InferenceResult(linfo)
                frame = InferenceState(result, src, #=cached=#true, interp)
                typeinf(interp, frame)
                @assert frame.inferred # TODO: deal with this better
                src = frame.src
            end
            ccall(:jl_typeinf_end, Cvoid, ())
        end
    end
    return src
end


function return_type(@nospecialize(f), @nospecialize(t))
    world = ccall(:jl_get_tls_world_age, UInt, ())
    return ccall(:jl_call_in_typeinf_world, Any, (Ptr{Ptr{Cvoid}}, Cint), Any[_return_type, f, t, world], 4)
end

_return_type(@nospecialize(f), @nospecialize(t), world) = _return_type(NativeInterpreter(world), f, t)

function _return_type(interp::AbstractInterpreter, @nospecialize(f), @nospecialize(t))
    rt = Union{}
    if isa(f, Builtin)
        rt = builtin_tfunction(interp, f, Any[t.parameters...], nothing)
        if isa(rt, TypeVar)
            rt = rt.ub
        else
            rt = widenconst(rt)
        end
    else
        for m in _methods(f, t, -1, get_world_counter(interp))
            ty = typeinf_type(interp, m[3], m[1], m[2])
            ty === nothing && return Any
            rt = tmerge(rt, ty)
            rt === Any && break
        end
    end
    return rt
end
