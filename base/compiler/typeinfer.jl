# This file is a part of Julia. License is MIT: https://julialang.org/license

# Tracking of newly-inferred MethodInstances during precompilation
const track_newly_inferred = RefValue{Bool}(false)
const newly_inferred = MethodInstance[]

# build (and start inferring) the inference frame for the top-level MethodInstance
function typeinf(interp::AbstractInterpreter, result::InferenceResult, cache::Symbol)
    frame = InferenceState(result, cache, interp)
    frame === nothing && return false
    cache === :global && lock_mi_inference(interp, result.linfo)
    return typeinf(interp, frame)
end

"""
The module `Core.Compiler.Timings` provides a simple implementation of nested timers that
can be used to measure the exclusive time spent inferring each method instance that is
recursively inferred during type inference.

This is meant to be internal to the compiler, and makes some specific assumptions about
being used for this purpose alone.
"""
module Timings

using Core.Compiler: -, +, :, Vector, length, first, empty!, push!, pop!, @inline,
    @inbounds, copy, backtrace

# What we record for any given frame we infer during type inference.
struct InferenceFrameInfo
    mi::Core.MethodInstance
    world::UInt64
    sptypes::Vector{Any}
    slottypes::Vector{Any}
    nargs::Int
end

function _typeinf_identifier(frame::Core.Compiler.InferenceState)
    mi_info = InferenceFrameInfo(
        frame.linfo,
        frame.world,
        copy(frame.sptypes),
        copy(frame.slottypes),
        length(frame.result.argtypes),
    )
    return mi_info
end

"""
    Core.Compiler.Timing(mi_info, start_time, ...)

Internal type containing the timing result for running type inference on a single
MethodInstance.
"""
struct Timing
    mi_info::InferenceFrameInfo
    start_time::UInt64
    cur_start_time::UInt64
    time::UInt64
    children::Core.Array{Timing,1}
    bt         # backtrace collected upon initial entry to typeinf
end
Timing(mi_info, start_time, cur_start_time, time, children) = Timing(mi_info, start_time, cur_start_time, time, children, nothing)
Timing(mi_info, start_time) = Timing(mi_info, start_time, start_time, UInt64(0), Timing[])

_time_ns() = ccall(:jl_hrtime, UInt64, ())  # Re-implemented here because Base not yet available.

# We keep a stack of the Timings for each of the MethodInstances currently being timed.
# Since type inference currently operates via a depth-first search (during abstract
# evaluation), this vector operates like a call stack. The last node in _timings is the
# node currently being inferred, and its parent is directly before it, etc.
# Each Timing also contains its own vector for all of its children, so that the tree
# call structure through type inference is recorded. (It's recorded as a tree, not a graph,
# because we create a new node for duplicates.)
const _timings = Timing[]
# ROOT() is an empty function used as the top-level Timing node to measure all time spent
# *not* in type inference during a given recording trace. It is used as a "dummy" node.
function ROOT() end
const ROOTmi = Core.Compiler.specialize_method(
    first(Core.Compiler.methods(ROOT)), Tuple{typeof(ROOT)}, Core.svec())
"""
    Core.Compiler.reset_timings()

Empty out the previously recorded type inference timings (`Core.Compiler._timings`), and
start the ROOT() timer again. `ROOT()` measures all time spent _outside_ inference.
"""
function reset_timings()
    empty!(_timings)
    push!(_timings, Timing(
        # The MethodInstance for ROOT(), and default empty values for other fields.
        InferenceFrameInfo(ROOTmi, 0x0, Any[], Any[Core.Const(ROOT)], 1),
        _time_ns()))
    return nothing
end
reset_timings()

# (This is split into a function so that it can be called both in this module, at the top
# of `enter_new_timer()`, and once at the Very End of the operation, by whoever started
# the operation and called `reset_timings()`.)
# NOTE: the @inline annotations here are not to make it faster, but to reduce the gap between
# timer manipulations and the tasks we're timing.
@inline function close_current_timer()
    stop_time = _time_ns()
    parent_timer = _timings[end]
    accum_time = stop_time - parent_timer.cur_start_time

    # Add in accum_time ("modify" the immutable struct)
    @inbounds begin
        _timings[end] = Timing(
            parent_timer.mi_info,
            parent_timer.start_time,
            parent_timer.cur_start_time,
            parent_timer.time + accum_time,
            parent_timer.children,
            parent_timer.bt,
        )
    end
    return nothing
end

@inline function enter_new_timer(frame)
    # Very first thing, stop the active timer: get the current time and add in the
    # time since it was last started to its aggregate exclusive time.
    close_current_timer()

    mi_info = _typeinf_identifier(frame)

    # Start the new timer right before returning
    push!(_timings, Timing(mi_info, UInt64(0)))
    len = length(_timings)
    new_timer = @inbounds _timings[len]
    # Set the current time _after_ appending the node, to try to exclude the
    # overhead from measurement.
    start = _time_ns()

    @inbounds begin
        _timings[len] = Timing(
            new_timer.mi_info,
            start,
            start,
            new_timer.time,
            new_timer.children,
        )
    end

    return nothing
end

# _expected_frame_ is not needed within this function; it is used in the `@assert`, to
# assert that indeed we are always returning to a parent after finishing all of its
# children (that is, asserting that inference proceeds via depth-first-search).
@inline function exit_current_timer(_expected_frame_)
    # Finish the new timer
    stop_time = _time_ns()

    expected_mi_info = _typeinf_identifier(_expected_frame_)

    # Grab the new timer again because it might have been modified in _timings
    # (since it's an immutable struct)
    # And remove it from the current timings stack
    new_timer = pop!(_timings)
    Core.Compiler.@assert new_timer.mi_info.mi === expected_mi_info.mi

    # Prepare to unwind one level of the stack and record in the parent
    parent_timer = _timings[end]

    accum_time = stop_time - new_timer.cur_start_time
    # Add in accum_time ("modify" the immutable struct)
    new_timer = Timing(
        new_timer.mi_info,
        new_timer.start_time,
        new_timer.cur_start_time,
        new_timer.time + accum_time,
        new_timer.children,
        parent_timer.mi_info.mi === ROOTmi ? backtrace() : nothing,
    )
    # Record the final timing with the original parent timer
    push!(parent_timer.children, new_timer)

    # And finally restart the parent timer:
    len = length(_timings)
    @inbounds begin
        _timings[len] = Timing(
            parent_timer.mi_info,
            parent_timer.start_time,
            _time_ns(),
            parent_timer.time,
            parent_timer.children,
            parent_timer.bt,
        )
    end

    return nothing
end

end  # module Timings

"""
    Core.Compiler.__set_measure_typeinf(onoff::Bool)

If set to `true`, record per-method-instance timings within type inference in the Compiler.
"""
__set_measure_typeinf(onoff::Bool) = __measure_typeinf__[] = onoff
const __measure_typeinf__ = fill(false)

# Wrapper around _typeinf that optionally records the exclusive time for each invocation.
function typeinf(interp::AbstractInterpreter, frame::InferenceState)
    if __measure_typeinf__[]
        Timings.enter_new_timer(frame)
        v = _typeinf(interp, frame)
        Timings.exit_current_timer(frame)
        return v
    else
        return _typeinf(interp, frame)
    end
end

function finish!(interp::AbstractInterpreter, caller::InferenceResult)
    # If we didn't transform the src for caching, we may have to transform
    # it anyway for users like typeinf_ext. Do that here.
    opt = caller.src
    if opt isa OptimizationState # implies `may_optimize(interp) === true`
        if opt.ir !== nothing
            caller.src = ir_to_codeinf!(opt)
        end
    end
    return caller.src
end

function _typeinf(interp::AbstractInterpreter, frame::InferenceState)
    typeinf_nocycle(interp, frame) || return false # frame is now part of a higher cycle
    # with no active ip's, frame is done
    frames = frame.callers_in_cycle
    isempty(frames) && push!(frames, frame)
    valid_worlds = WorldRange()
    for caller in frames
        @assert !(caller.dont_work_on_me)
        caller.dont_work_on_me = true
        # might might not fully intersect these earlier, so do that now
        valid_worlds = intersect(caller.valid_worlds, valid_worlds)
    end
    for caller in frames
        caller.valid_worlds = valid_worlds
        finish(caller, interp)
        # finalize and record the linfo result
        caller.inferred = true
    end
    # collect results for the new expanded frame
    results = Tuple{InferenceResult, Vector{Any}, Bool}[
            ( frames[i].result,
              frames[i].stmt_edges[1]::Vector{Any},
              frames[i].cached )
        for i in 1:length(frames) ]
    empty!(frames)
    for (caller, _, _) in results
        opt = caller.src
        if opt isa OptimizationState # implies `may_optimize(interp) === true`
            analyzed = optimize(interp, opt, OptimizationParams(interp), caller)
            if isa(analyzed, ConstAPI)
                # XXX: The work in ir_to_codeinf! is essentially wasted. The only reason
                # we're doing it is so that code_llvm can return the code
                # for the `return ...::Const` (which never runs anyway). We should do this
                # as a post processing step instead.
                ir_to_codeinf!(opt)
                caller.src = analyzed
            end
            caller.valid_worlds = (opt.inlining.et::EdgeTracker).valid_worlds[]
        end
    end
    for (caller, edges, cached) in results
        valid_worlds = caller.valid_worlds
        if last(valid_worlds) >= get_world_counter()
            # if we aren't cached, we don't need this edge
            # but our caller might, so let's just make it anyways
            store_backedges(caller, edges)
        end
        if cached
            cache_result!(interp, caller)
        end
        finish!(interp, caller)
    end
    return true
end

function CodeInstance(
    result::InferenceResult, @nospecialize(inferred_result), valid_worlds::WorldRange)
    local const_flags::Int32
    result_type = result.result
    @assert !(result_type isa LimitedAccuracy)
    if inferred_result isa ConstAPI
        # use constant calling convention
        rettype_const = inferred_result.val
        const_flags = 0x3
        inferred_result = nothing
    else
        if isa(result_type, Const)
            rettype_const = result_type.val
            const_flags = 0x2
        elseif isa(result_type, PartialOpaque)
            rettype_const = result_type
            const_flags = 0x2
        elseif isconstType(result_type)
            rettype_const = result_type.parameters[1]
            const_flags = 0x2
        elseif isa(result_type, PartialStruct)
            rettype_const = result_type.fields
            const_flags = 0x2
        elseif isa(result_type, InterConditional)
            rettype_const = result_type
            const_flags = 0x2
        else
            rettype_const = nothing
            const_flags = 0x00
        end
    end
    relocatability = isa(inferred_result, Vector{UInt8}) ? inferred_result[end] : UInt8(0)
    return CodeInstance(result.linfo,
        widenconst(result_type), rettype_const, inferred_result,
        const_flags, first(valid_worlds), last(valid_worlds),
        # TODO: Actually do something with non-IPO effects
	    encode_effects(result.ipo_effects), encode_effects(result.ipo_effects), result.argescapes,
        relocatability)
end

# For the NativeInterpreter, we don't need to do an actual cache query to know
# if something was already inferred. If we reach this point, but the inference
# flag has been turned off, then it's in the cache. This is purely a performance
# optimization.
already_inferred_quick_test(interp::NativeInterpreter, mi::MethodInstance) =
    !mi.inInference
already_inferred_quick_test(interp::AbstractInterpreter, mi::MethodInstance) =
    false

function maybe_compress_codeinfo(interp::AbstractInterpreter, linfo::MethodInstance, ci::CodeInfo)
    def = linfo.def
    toplevel = !isa(def, Method)
    if toplevel
        return ci
    end
    if may_discard_trees(interp)
        cache_the_tree = ci.inferred && (ci.inlineable || isa_compileable_sig(linfo.specTypes, def))
    else
        cache_the_tree = true
    end
    if cache_the_tree
        if may_compress(interp)
            nslots = length(ci.slotflags)
            resize!(ci.slottypes::Vector{Any}, nslots)
            resize!(ci.slotnames, nslots)
            return ccall(:jl_compress_ir, Vector{UInt8}, (Any, Any), def, ci)
        else
            return ci
        end
    else
        return nothing
    end
end

function transform_result_for_cache(interp::AbstractInterpreter, linfo::MethodInstance,
                                    valid_worlds::WorldRange, @nospecialize(inferred_result),
                                    ipo_effects::Effects)
    # If we decided not to optimize, drop the OptimizationState now.
    # External interpreters can override as necessary to cache additional information
    if inferred_result isa OptimizationState
        inferred_result = ir_to_codeinf!(inferred_result)
    end
    if inferred_result isa CodeInfo
        inferred_result.min_world = first(valid_worlds)
        inferred_result.max_world = last(valid_worlds)
        inferred_result = maybe_compress_codeinfo(interp, linfo, inferred_result)
    end
    # The global cache can only handle objects that codegen understands
    if !isa(inferred_result, Union{CodeInfo, Vector{UInt8}, ConstAPI})
        inferred_result = nothing
    end
    return inferred_result
end

function cache_result!(interp::AbstractInterpreter, result::InferenceResult)
    valid_worlds = result.valid_worlds
    if last(valid_worlds) == get_world_counter()
        # if we've successfully recorded all of the backedges in the global reverse-cache,
        # we can now widen our applicability in the global cache too
        valid_worlds = WorldRange(first(valid_worlds), typemax(UInt))
    end
    # check if the existing linfo metadata is also sufficient to describe the current inference result
    # to decide if it is worth caching this
    linfo = result.linfo
    already_inferred = already_inferred_quick_test(interp, linfo)
    if !already_inferred && haskey(WorldView(code_cache(interp), valid_worlds), linfo)
        already_inferred = true
    end

    # TODO: also don't store inferred code if we've previously decided to interpret this function
    if !already_inferred
        inferred_result = transform_result_for_cache(interp, linfo, valid_worlds, result.src, result.ipo_effects)
        code_cache(interp)[linfo] = CodeInstance(result, inferred_result, valid_worlds)
        if track_newly_inferred[]
            m = linfo.def
            if isa(m, Method)
                m.module != Core && push!(newly_inferred, linfo)
            end
        end
    end
    unlock_mi_inference(interp, linfo)
    nothing
end

function cycle_fix_limited(@nospecialize(typ), sv::InferenceState)
    if typ isa LimitedAccuracy
        if sv.parent === nothing
            # when part of a cycle, we might have unintentionally introduced a limit marker
            @assert !isempty(sv.callers_in_cycle)
            return typ.typ
        end
        causes = copy(typ.causes)
        delete!(causes, sv)
        for caller in sv.callers_in_cycle
            delete!(causes, caller)
        end
        if isempty(causes)
            return typ.typ
        end
        if length(causes) != length(typ.causes)
            return LimitedAccuracy(typ.typ, causes)
        end
    end
    return typ
end

function adjust_effects(sv::InferenceState)
    ipo_effects = Effects(sv)

    # Always throwing an error counts or never returning both count as consistent,
    # but we don't currently model idempontency using dataflow, so we don't notice.
    # Fix that up here to improve precision.
    if !ipo_effects.inbounds_taints_consistency && sv.bestguess === Union{}
        ipo_effects = Effects(ipo_effects; consistent=ALWAYS_TRUE)
    end

    # override the analyzed effects using manually annotated effect settings
    def = sv.linfo.def
    if isa(def, Method)
        override = decode_effects_override(def.purity)
        if is_effect_overridden(override, :consistent)
            ipo_effects = Effects(ipo_effects; consistent=ALWAYS_TRUE)
        end
        if is_effect_overridden(override, :effect_free)
            ipo_effects = Effects(ipo_effects; effect_free=ALWAYS_TRUE)
        end
        if is_effect_overridden(override, :nothrow)
            ipo_effects = Effects(ipo_effects; nothrow=ALWAYS_TRUE)
        end
        if is_effect_overridden(override, :terminates_globally)
            ipo_effects = Effects(ipo_effects; terminates=ALWAYS_TRUE)
        end
    end

    return ipo_effects
end

# inference completed on `me`
# update the MethodInstance
function finish(me::InferenceState, interp::AbstractInterpreter)
    # prepare to run optimization passes on fulltree
    s_edges = me.stmt_edges[1]
    if s_edges === nothing
        s_edges = me.stmt_edges[1] = []
    end
    for edges in me.stmt_edges
        edges === nothing && continue
        edges === s_edges && continue
        append!(s_edges, edges)
        empty!(edges)
    end
    if me.src.edges !== nothing
        append!(s_edges, me.src.edges::Vector)
        me.src.edges = nothing
    end
    # inspect whether our inference had a limited result accuracy,
    # else it may be suitable to cache
    me.bestguess = cycle_fix_limited(me.bestguess, me)
    limited_ret = me.bestguess isa LimitedAccuracy
    limited_src = false
    if !limited_ret
        gt = me.src.ssavaluetypes::Vector{Any}
        for j = 1:length(gt)
            gt[j] = gtj = cycle_fix_limited(gt[j], me)
            if gtj isa LimitedAccuracy && me.parent !== nothing
                limited_src = true
                break
            end
        end
    end
    if limited_ret
        # a parent may be cached still, but not this intermediate work:
        # we can throw everything else away now
        me.result.src = nothing
        me.cached = false
        me.src.inlineable = false
        unlock_mi_inference(interp, me.linfo)
    elseif limited_src
        # a type result will be cached still, but not this intermediate work:
        # we can throw everything else away now
        me.result.src = nothing
        me.src.inlineable = false
    else
        # annotate fulltree with type information,
        # either because we are the outermost code, or we might use this later
        doopt = (me.cached || me.parent !== nothing)
        type_annotate!(me, doopt)
        if doopt && may_optimize(interp)
            me.result.src = OptimizationState(me, OptimizationParams(interp), interp)
        else
            me.result.src = me.src::CodeInfo # stash a convenience copy of the code (e.g. for reflection)
        end
    end
    me.result.valid_worlds = me.valid_worlds
    me.result.result = me.bestguess
    me.ipo_effects = me.result.ipo_effects = adjust_effects(me)
    validate_code_in_debug_mode(me.linfo, me.src, "inferred")
    nothing
end

# record the backedges
function store_backedges(frame::InferenceResult, edges::Vector{Any})
    toplevel = !isa(frame.linfo.def, Method)
    if !toplevel
        store_backedges(frame.linfo, edges)
    end
    nothing
end

function store_backedges(caller::MethodInstance, edges::Vector{Any})
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
    ssavaluetypes = src.ssavaluetypes::Vector{Any}
    for i = 1:length(ssavaluetypes)
        ssavaluetypes[i] = widenconst(ssavaluetypes[i])
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

function widen_ssavaluetypes!(sv::InferenceState)
    ssavaluetypes = sv.src.ssavaluetypes::Vector{Any}
    for j = 1:length(ssavaluetypes)
        t = ssavaluetypes[j]
        ssavaluetypes[j] = t === NOT_FOUND ? Bottom : widenconditional(t)
    end
    return nothing
end

function record_slot_assign!(sv::InferenceState)
    # look at all assignments to slots
    # and union the set of types stored there
    # to compute a lower bound on the storage required
    states = sv.stmt_types
    body = sv.src.code::Vector{Any}
    slottypes = sv.slottypes::Vector{Any}
    ssavaluetypes = sv.src.ssavaluetypes::Vector{Any}
    for i = 1:length(body)
        expr = body[i]
        st_i = states[i]
        # find all reachable assignments to locals
        if isa(st_i, VarTable) && isexpr(expr, :(=))
            lhs = expr.args[1]
            if isa(lhs, SlotNumber)
                vt = widenconst(ssavaluetypes[i])
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
    sv.src.slottypes = slottypes
    return nothing
end

function record_bestguess!(sv::InferenceState)
    bestguess = sv.bestguess
    @assert !(bestguess isa LimitedAccuracy)
    sv.src.rettype = bestguess
    return nothing
end

function annotate_slot_load!(undefs::Vector{Bool}, vtypes::VarTable, sv::InferenceState,
    @nospecialize x)
    if isa(x, SlotNumber)
        id = slot_id(x)
        vt = vtypes[id]
        if vt.undef
            # mark used-undef variables
            undefs[id] = true
        end
        # add type annotations where needed
        typ = widenconditional(ignorelimited(vt.typ))
        if !(sv.slottypes[id] ⊑ typ)
            return TypedSlot(id, typ)
        end
        return x
    elseif isa(x, Expr)
        head = x.head
        i0 = 1
        if is_meta_expr_head(head) || head === :const
            return x
        end
        if head === :(=) || head === :method
            i0 = 2
        end
        for i = i0:length(x.args)
            x.args[i] = annotate_slot_load!(undefs, vtypes, sv, x.args[i])
        end
        return x
    elseif isa(x, ReturnNode) && isdefined(x, :val)
        return ReturnNode(annotate_slot_load!(undefs, vtypes, sv, x.val))
    elseif isa(x, GotoIfNot)
        return GotoIfNot(annotate_slot_load!(undefs, vtypes, sv, x.cond), x.dest)
    end
    return x
end

# annotate types of all symbols in AST
function type_annotate!(sv::InferenceState, run_optimizer::Bool)
    widen_ssavaluetypes!(sv)

    # compute the required type for each slot
    # to hold all of the items assigned into it
    record_slot_assign!(sv)

    record_bestguess!(sv)

    # annotate variables load types
    # remove dead code optimization
    # and compute which variables may be used undef
    states = sv.stmt_types
    stmt_info = sv.stmt_info
    src = sv.src
    body = src.code::Vector{Any}
    nexpr = length(body)
    codelocs = src.codelocs
    ssavaluetypes = src.ssavaluetypes
    ssaflags = src.ssaflags
    slotflags = src.slotflags
    nslots = length(slotflags)
    undefs = fill(false, nslots)

    # eliminate GotoIfNot if either of branch target is unreachable
    if run_optimizer
        for idx = 1:nexpr
            stmt = body[idx]
            if isa(stmt, GotoIfNot) && widenconst(argextype(stmt.cond, src, sv.sptypes)) === Bool
                # replace live GotoIfNot with:
                # - GotoNode if the fallthrough target is unreachable
                # - no-op if the branch target is unreachable
                if states[idx+1] === nothing
                    body[idx] = GotoNode(stmt.dest)
                elseif states[stmt.dest] === nothing
                    body[idx] = nothing
                end
            end
        end
    end

    # dead code elimination for unreachable regions
    i = 1
    oldidx = 0
    changemap = fill(0, nexpr)
    while i <= nexpr
        oldidx += 1
        st_i = states[i]
        expr = body[i]
        if isa(st_i, VarTable)
            # introduce temporary TypedSlot for the later optimization passes
            # and also mark used-undef slots
            body[i] = annotate_slot_load!(undefs, st_i, sv, expr)
        else # unreached statement (see issue #7836)
            if is_meta_expr(expr)
                # keep any lexically scoped expressions
            elseif run_optimizer
                deleteat!(body, i)
                deleteat!(states, i)
                deleteat!(ssavaluetypes, i)
                deleteat!(codelocs, i)
                deleteat!(stmt_info, i)
                deleteat!(ssaflags, i)
                nexpr -= 1
                changemap[oldidx] = -1
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
            slotflags[j] |= SLOT_USEDUNDEF | SLOT_STATICUNDEF
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

function merge_call_chain!(parent::InferenceState, ancestor::InferenceState, child::InferenceState)
    # add backedge of parent <- child
    # then add all backedges of parent <- parent.parent
    # and merge all of the callers into ancestor.callers_in_cycle
    # and ensure that walking the parent list will get the same result (DAG) from everywhere
    # Also taint the termination effect, because we can no longer guarantee the absence
    # of recursion.
    tristate_merge!(parent, Effects(EFFECTS_TOTAL; terminates=TRISTATE_UNKNOWN))
    while true
        add_cycle_backedge!(child, parent, parent.currpc)
        union_caller_cycle!(ancestor, child)
        tristate_merge!(child, Effects(EFFECTS_TOTAL; terminates=TRISTATE_UNKNOWN))
        child = parent
        child === ancestor && break
        parent = child.parent::InferenceState
    end
end

function is_same_frame(interp::AbstractInterpreter, linfo::MethodInstance, frame::InferenceState)
    return linfo === frame.linfo
end

function poison_callstack(infstate::InferenceState, topmost::InferenceState)
    push!(infstate.pclimitations, topmost)
    nothing
end

# Walk through `linfo`'s upstream call chain, starting at `parent`. If a parent
# frame matching `linfo` is encountered, then there is a cycle in the call graph
# (i.e. `linfo` is a descendant callee of itself). Upon encountering this cycle,
# we "resolve" it by merging the call chain, which entails unioning each intermediary
# frame's `callers_in_cycle` field and adding the appropriate backedges. Finally,
# we return `linfo`'s pre-existing frame. If no cycles are found, `nothing` is
# returned instead.
function resolve_call_cycle!(interp::AbstractInterpreter, linfo::MethodInstance, parent::InferenceState)
    frame = parent
    uncached = false
    while isa(frame, InferenceState)
        uncached |= !frame.cached # ensure we never add an uncached frame to a cycle
        if is_same_frame(interp, linfo, frame)
            if uncached
                # our attempt to speculate into a constant call lead to an undesired self-cycle
                # that cannot be converged: poison our call-stack (up to the discovered duplicate frame)
                # with the limited flag and abort (set return type to Any) now
                poison_callstack(parent, frame)
                return true
            end
            merge_call_chain!(parent, frame, frame)
            return frame
        end
        for caller in frame.callers_in_cycle
            if is_same_frame(interp, linfo, caller)
                if uncached
                    poison_callstack(parent, frame)
                    return true
                end
                merge_call_chain!(parent, frame, caller)
                return caller
            end
        end
        frame = frame.parent
    end
    return false
end

generating_sysimg() = ccall(:jl_generating_output, Cint, ()) != 0 && JLOptions().incremental == 0

ipo_effects(code::CodeInstance) = decode_effects(code.ipo_purity_bits)

struct EdgeCallResult
    rt #::Type
    edge::Union{Nothing,MethodInstance}
    edge_effects::Effects
    function EdgeCallResult(@nospecialize(rt),
                            edge::Union{Nothing,MethodInstance},
                            edge_effects::Effects)
        return new(rt, edge, edge_effects)
    end
end

# compute (and cache) an inferred AST and return the current best estimate of the result type
function typeinf_edge(interp::AbstractInterpreter, method::Method, @nospecialize(atype), sparams::SimpleVector, caller::InferenceState)
    mi = specialize_method(method, atype, sparams)::MethodInstance
    code = get(code_cache(interp), mi, nothing)
    if code isa CodeInstance # return existing rettype if the code is already inferred
        if code.inferred === nothing && is_stmt_inline(get_curr_ssaflag(caller))
            # we already inferred this edge before and decided to discard the inferred code,
            # nevertheless we re-infer it here again and keep it around in the local cache
            # since the inliner will request to use it later
            cache = :local
        else
            effects = ipo_effects(code)
            update_valid_age!(caller, WorldRange(min_world(code), max_world(code)))
            rettype = code.rettype
            if isdefined(code, :rettype_const)
                rettype_const = code.rettype_const
                # the second subtyping conditions are necessary to distinguish usual cases
                # from rare cases when `Const` wrapped those extended lattice type objects
                if isa(rettype_const, Vector{Any}) && !(Vector{Any} <: rettype)
                    rettype = PartialStruct(rettype, rettype_const)
                elseif isa(rettype_const, PartialOpaque) && rettype <: Core.OpaqueClosure
                    rettype = rettype_const
                elseif isa(rettype_const, InterConditional) && !(InterConditional <: rettype)
                    rettype = rettype_const
                else
                    rettype = Const(rettype_const)
                end
            end
            return EdgeCallResult(rettype, mi, effects)
        end
    else
        cache = :global # cache edge targets by default
    end
    if ccall(:jl_get_module_infer, Cint, (Any,), method.module) == 0 && !generating_sysimg()
        return EdgeCallResult(Any, nothing, Effects())
    end
    if !caller.cached && caller.parent === nothing
        # this caller exists to return to the user
        # (if we asked resolve_call_cyle, it might instead detect that there is a cycle that it can't merge)
        frame = false
    else
        frame = resolve_call_cycle!(interp, mi, caller)
    end
    if frame === false
        # completely new
        lock_mi_inference(interp, mi)
        result = InferenceResult(mi)
        frame = InferenceState(result, cache, interp) # always use the cache for edge targets
        if frame === nothing
            # can't get the source for this, so we know nothing
            unlock_mi_inference(interp, mi)
            return EdgeCallResult(Any, nothing, Effects())
        end
        if caller.cached || caller.parent !== nothing # don't involve uncached functions in cycle resolution
            frame.parent = caller
        end
        typeinf(interp, frame)
        update_valid_age!(frame, caller)
        edge = frame.inferred ? mi : nothing
        return EdgeCallResult(frame.bestguess, edge, Effects(frame)) # effects are adjusted already within `finish`
    elseif frame === true
        # unresolvable cycle
        return EdgeCallResult(Any, nothing, Effects())
    end
    # return the current knowledge about this cycle
    frame = frame::InferenceState
    update_valid_age!(frame, caller)
    return EdgeCallResult(frame.bestguess, nothing, adjust_effects(frame))
end

#### entry points for inferring a MethodInstance given a type signature ####

# compute an inferred AST and return type
function typeinf_code(interp::AbstractInterpreter, method::Method, @nospecialize(atype), sparams::SimpleVector, run_optimizer::Bool)
    frame = typeinf_frame(interp, method, atype, sparams, run_optimizer)
    frame === nothing && return nothing, Any
    frame.inferred || return nothing, Any
    code = frame.src
    rt = widenconst(ignorelimited(frame.result.result))
    return code, rt
end

# compute an inferred frame
function typeinf_frame(interp::AbstractInterpreter, method::Method, @nospecialize(atype), sparams::SimpleVector, run_optimizer::Bool)
    mi = specialize_method(method, atype, sparams)::MethodInstance
    ccall(:jl_typeinf_begin, Cvoid, ())
    result = InferenceResult(mi)
    frame = InferenceState(result, run_optimizer ? :global : :no, interp)
    frame === nothing && return nothing
    typeinf(interp, frame)
    ccall(:jl_typeinf_end, Cvoid, ())
    return frame
end

# compute (and cache) an inferred AST and return type
function typeinf_ext(interp::AbstractInterpreter, mi::MethodInstance)
    method = mi.def::Method
    for i = 1:2 # test-and-lock-and-test
        i == 2 && ccall(:jl_typeinf_begin, Cvoid, ())
        code = get(code_cache(interp), mi, nothing)
        if code isa CodeInstance
            # see if this code already exists in the cache
            inf = code.inferred
            if use_const_api(code)
                i == 2 && ccall(:jl_typeinf_end, Cvoid, ())
                tree = ccall(:jl_new_code_info_uninit, Ref{CodeInfo}, ())
                rettype_const = code.rettype_const
                tree.code = Any[ ReturnNode(quoted(rettype_const)) ]
                nargs = Int(method.nargs)
                tree.slotnames = ccall(:jl_uncompress_argnames, Vector{Symbol}, (Any,), method.slot_syms)
                tree.slotflags = fill(IR_FLAG_NULL, nargs)
                tree.ssavaluetypes = 1
                tree.codelocs = Int32[1]
                tree.linetable = [LineInfoNode(method.module, method.name, method.file, method.line, Int32(0))]
                tree.inferred = true
                tree.ssaflags = UInt8[0]
                tree.pure = true
                tree.inlineable = true
                tree.parent = mi
                tree.rettype = Core.Typeof(rettype_const)
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
    if ccall(:jl_get_module_infer, Cint, (Any,), method.module) == 0 && !generating_sysimg()
        return retrieve_code_info(mi)
    end
    lock_mi_inference(interp, mi)
    frame = InferenceState(InferenceResult(mi), #=cache=#:global, interp)
    frame === nothing && return nothing
    typeinf(interp, frame)
    ccall(:jl_typeinf_end, Cvoid, ())
    frame.src.inferred || return nothing
    return frame.src
end

# compute (and cache) an inferred AST and return the inferred return type
function typeinf_type(interp::AbstractInterpreter, method::Method, @nospecialize(atype), sparams::SimpleVector)
    if contains_is(unwrap_unionall(atype).parameters, Union{})
        return Union{} # don't ask: it does weird and unnecessary things, if it occurs during bootstrap
    end
    mi = specialize_method(method, atype, sparams)::MethodInstance
    for i = 1:2 # test-and-lock-and-test
        i == 2 && ccall(:jl_typeinf_begin, Cvoid, ())
        code = get(code_cache(interp), mi, nothing)
        if code isa CodeInstance
            # see if this rettype already exists in the cache
            i == 2 && ccall(:jl_typeinf_end, Cvoid, ())
            return code.rettype
        end
    end
    result = InferenceResult(mi)
    typeinf(interp, result, :global)
    ccall(:jl_typeinf_end, Cvoid, ())
    result.result isa InferenceState && return nothing
    return widenconst(ignorelimited(result.result))
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
                frame = InferenceState(result, src, #=cache=#:global, interp)
                typeinf(interp, frame)
                @assert frame.inferred # TODO: deal with this better
                src = frame.src
            end
            ccall(:jl_typeinf_end, Cvoid, ())
        end
    end
    return src
end

function return_type(@nospecialize(f), t::DataType) # this method has a special tfunc
    world = ccall(:jl_get_tls_world_age, UInt, ())
    args = Any[_return_type, NativeInterpreter(world), Tuple{Core.Typeof(f), t.parameters...}]
    return ccall(:jl_call_in_typeinf_world, Any, (Ptr{Ptr{Cvoid}}, Cint), args, length(args))
end

function return_type(@nospecialize(f), t::DataType, world::UInt)
    return return_type(Tuple{Core.Typeof(f), t.parameters...}, world)
end

function return_type(t::DataType)
    world = ccall(:jl_get_tls_world_age, UInt, ())
    return return_type(t, world)
end

function return_type(t::DataType, world::UInt)
    args = Any[_return_type, NativeInterpreter(world), t]
    return ccall(:jl_call_in_typeinf_world, Any, (Ptr{Ptr{Cvoid}}, Cint), args, length(args))
end

function _return_type(interp::AbstractInterpreter, t::DataType)
    rt = Union{}
    f = singleton_type(t.parameters[1])
    if isa(f, Builtin)
        args = Any[t.parameters...]
        popfirst!(args)
        rt = builtin_tfunction(interp, f, args, nothing)
        rt = widenconst(rt)
    else
        for match in _methods_by_ftype(t, -1, get_world_counter(interp))::Vector
            match = match::MethodMatch
            ty = typeinf_type(interp, match.method, match.spec_types, match.sparams)
            ty === nothing && return Any
            rt = tmerge(rt, ty)
            rt === Any && break
        end
    end
    return rt
end
