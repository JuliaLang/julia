# This file is a part of Julia. License is MIT: https://julialang.org/license

# Tracking of newly-inferred CodeInstances during precompilation
const track_newly_inferred = RefValue{Bool}(false)
const newly_inferred = CodeInstance[]

# build (and start inferring) the inference frame for the top-level MethodInstance
function typeinf(interp::AbstractInterpreter, result::InferenceResult, cache_mode::Symbol)
    frame = InferenceState(result, cache_mode, interp)
    frame === nothing && return false
    cache_mode === :global && lock_mi_inference(interp, result.linfo)
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
    sptypes::Vector{Core.Compiler.VarState}
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

_typeinf_identifier(frame::InferenceFrameInfo) = frame

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
        InferenceFrameInfo(ROOTmi, 0x0, Core.Compiler.VarState[], Any[Core.Const(ROOT)], 1),
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

# Wrapper around `_typeinf` that optionally records the exclusive time for each invocation.
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

function finish!(interp::AbstractInterpreter, caller::InferenceState)
    result = caller.result
    valid_worlds = result.valid_worlds
    if last(valid_worlds) >= get_world_counter()
        # if we aren't cached, we don't need this edge
        # but our caller might, so let's just make it anyways
        store_backedges(result, caller.stmt_edges[1])
    end
    opt = result.src
    if opt isa OptimizationState
        result.src = opt = ir_to_codeinf!(opt)
    end
    return nothing
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
        finish(caller, caller.interp)
    end
    for caller in frames
        opt = caller.result.src
        if opt isa OptimizationState # implies `may_optimize(caller.interp) === true`
            optimize(caller.interp, opt, caller.result)
        end
    end
    for caller in frames
        finish!(caller.interp, caller)
        if is_cached(caller)
            cache_result!(caller.interp, caller.result)
        end
    end
    empty!(frames)
    return true
end

function is_result_constabi_eligible(result::InferenceResult)
    result_type = result.result
    return isa(result_type, Const) && is_foldable_nothrow(result.ipo_effects) && is_inlineable_constant(result_type.val)
end
function CodeInstance(interp::AbstractInterpreter, result::InferenceResult;
                      can_discard_trees::Bool=may_discard_trees(interp))
    local const_flags::Int32
    result_type = result.result
    @assert !(result_type === nothing || result_type isa LimitedAccuracy)
    if is_result_constabi_eligible(result)
        # use constant calling convention
        rettype_const = result_type.val
        const_flags = 0x3
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
        elseif isa(result_type, InterMustAlias)
            rettype_const = result_type
            const_flags = 0x2
        else
            rettype_const = nothing
            const_flags = 0x00
        end
    end
    relocatability = 0x0
    owner = cache_owner(interp)
    if const_flags == 0x3 && can_discard_trees
        inferred_result = nothing
        relocatability = 0x1
    else
        inferred_result = transform_result_for_cache(interp, result.linfo, result.valid_worlds, result, can_discard_trees)
        if inferred_result isa CodeInfo
            edges = inferred_result.debuginfo
            uncompressed = inferred_result
            inferred_result = maybe_compress_codeinfo(interp, result.linfo, inferred_result, can_discard_trees)
            result.is_src_volatile |= uncompressed !== inferred_result
        elseif owner === nothing
            # The global cache can only handle objects that codegen understands
            inferred_result = nothing
        end
        if isa(inferred_result, String)
            t = @_gc_preserve_begin inferred_result
            relocatability = unsafe_load(unsafe_convert(Ptr{UInt8}, inferred_result), Core.sizeof(inferred_result))
            @_gc_preserve_end t
        elseif inferred_result === nothing
            relocatability = 0x1
        end
    end
    # n.b. relocatability = (isa(inferred_result, String) && inferred_result[end]) || inferred_result === nothing
    if !@isdefined edges
        edges = DebugInfo(result.linfo)
    end
    return CodeInstance(result.linfo, owner,
        widenconst(result_type), widenconst(result.exc_result), rettype_const, inferred_result,
        const_flags, first(result.valid_worlds), last(result.valid_worlds),
        # TODO: Actually do something with non-IPO effects
        encode_effects(result.ipo_effects), encode_effects(result.ipo_effects), result.analysis_results,
        relocatability, edges)
end

function transform_result_for_cache(interp::AbstractInterpreter,
        ::MethodInstance, valid_worlds::WorldRange, result::InferenceResult,
        can_discard_trees::Bool=may_discard_trees(interp))
    return result.src
end

function maybe_compress_codeinfo(interp::AbstractInterpreter, mi::MethodInstance, ci::CodeInfo,
                                 can_discard_trees::Bool=may_discard_trees(interp))
    def = mi.def
    isa(def, Method) || return ci # don't compress toplevel code
    cache_the_tree = true
    if can_discard_trees
        cache_the_tree = is_inlineable(ci) || isa_compileable_sig(mi.specTypes, mi.sparam_vals, def)
    end
    if cache_the_tree
        if may_compress(interp)
            nslots = length(ci.slotflags)
            resize!(ci.slottypes::Vector{Any}, nslots)
            resize!(ci.slotnames, nslots)
            return ccall(:jl_compress_ir, String, (Any, Any), def, ci)
        else
            return ci
        end
    else
        return nothing
    end
end

function cache_result!(interp::AbstractInterpreter, result::InferenceResult)
    if last(result.valid_worlds) == get_world_counter()
        # if we've successfully recorded all of the backedges in the global reverse-cache,
        # we can now widen our applicability in the global cache too
        result.valid_worlds = WorldRange(first(result.valid_worlds), typemax(UInt))
    end
    # check if the existing linfo metadata is also sufficient to describe the current inference result
    # to decide if it is worth caching this
    mi = result.linfo
    already_inferred = already_inferred_quick_test(interp, mi)
    cache = WorldView(code_cache(interp), result.valid_worlds)
    if !already_inferred && haskey(cache, mi)
        ci = cache[mi]
        # Even if we already have a CI for this, it's possible that the new CI has more
        # information (E.g. because the source was limited before, but is no longer - this
        # happens during bootstrap). In that case, allow the result to be recached.
        if result.src === nothing || (ci.inferred !== nothing || ci.invoke != C_NULL)
            already_inferred = true
        end
    end

    # TODO: also don't store inferred code if we've previously decided to interpret this function
    if !already_inferred
        code_cache(interp)[mi] = ci = CodeInstance(interp, result)
        result.ci = ci
        if track_newly_inferred[]
            m = mi.def
            if isa(m, Method) && m.module != Core
                ccall(:jl_push_newly_inferred, Cvoid, (Any,), ci)
            end
        end
    end
    unlock_mi_inference(interp, mi)
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

function adjust_effects(ipo_effects::Effects, def::Method)
    # override the analyzed effects using manually annotated effect settings
    override = decode_effects_override(def.purity)
    if is_effect_overridden(override, :consistent)
        ipo_effects = Effects(ipo_effects; consistent=ALWAYS_TRUE)
    end
    if is_effect_overridden(override, :effect_free)
        ipo_effects = Effects(ipo_effects; effect_free=ALWAYS_TRUE)
    end
    if is_effect_overridden(override, :nothrow)
        ipo_effects = Effects(ipo_effects; nothrow=true)
    end
    if is_effect_overridden(override, :terminates_globally)
        ipo_effects = Effects(ipo_effects; terminates=true)
    end
    if is_effect_overridden(override, :notaskstate)
        ipo_effects = Effects(ipo_effects; notaskstate=true)
    end
    if is_effect_overridden(override, :inaccessiblememonly)
        ipo_effects = Effects(ipo_effects; inaccessiblememonly=ALWAYS_TRUE)
    end
    if is_effect_overridden(override, :noub)
        ipo_effects = Effects(ipo_effects; noub=ALWAYS_TRUE)
    elseif is_effect_overridden(override, :noub_if_noinbounds) && ipo_effects.noub !== ALWAYS_TRUE
        ipo_effects = Effects(ipo_effects; noub=NOUB_IF_NOINBOUNDS)
    end
    return ipo_effects
end

function adjust_effects(sv::InferenceState)
    ipo_effects = sv.ipo_effects

    # refine :consistent-cy effect using the return type information
    # TODO this adjustment tries to compromise imprecise :consistent-cy information,
    # that is currently modeled in a flow-insensitive way: ideally we want to model it
    # with a proper dataflow analysis instead
    rt = sv.bestguess
    if rt === Bottom
        # always throwing an error counts or never returning both count as consistent
        ipo_effects = Effects(ipo_effects; consistent=ALWAYS_TRUE)
    end
    if sv.exc_bestguess === Bottom
        # if the exception type of this frame is known to be `Bottom`,
        # this frame is known to be safe
        ipo_effects = Effects(ipo_effects; nothrow=true)
    end
    if is_inaccessiblemem_or_argmemonly(ipo_effects) && all(1:narguments(sv, #=include_va=#true)) do i::Int
            return is_mutation_free_argtype(sv.slottypes[i])
        end
        ipo_effects = Effects(ipo_effects; inaccessiblememonly=ALWAYS_TRUE)
    end
    if is_consistent_if_notreturned(ipo_effects) && is_identity_free_argtype(rt)
        # in a case when the :consistent-cy here is only tainted by mutable allocations
        # (indicated by `CONSISTENT_IF_NOTRETURNED`), we may be able to refine it if the return
        # type guarantees that the allocations are never returned
        consistent = ipo_effects.consistent & ~CONSISTENT_IF_NOTRETURNED
        ipo_effects = Effects(ipo_effects; consistent)
    end
    if is_consistent_if_inaccessiblememonly(ipo_effects)
        if is_inaccessiblememonly(ipo_effects)
            consistent = ipo_effects.consistent & ~CONSISTENT_IF_INACCESSIBLEMEMONLY
            ipo_effects = Effects(ipo_effects; consistent)
        elseif is_inaccessiblemem_or_argmemonly(ipo_effects)
        else # `:inaccessiblememonly` is already tainted, there will be no chance to refine this
            ipo_effects = Effects(ipo_effects; consistent=ALWAYS_FALSE)
        end
    end
    if is_effect_free_if_inaccessiblememonly(ipo_effects)
        if is_inaccessiblememonly(ipo_effects)
            effect_free = ipo_effects.effect_free & ~EFFECT_FREE_IF_INACCESSIBLEMEMONLY
            ipo_effects = Effects(ipo_effects; effect_free)
        elseif is_inaccessiblemem_or_argmemonly(ipo_effects)
        else # `:inaccessiblememonly` is already tainted, there will be no chance to refine this
            ipo_effects = Effects(ipo_effects; effect_free=ALWAYS_FALSE)
        end
    end

    # override the analyzed effects using manually annotated effect settings
    def = sv.linfo.def
    if isa(def, Method)
        ipo_effects = adjust_effects(ipo_effects, def)
    end

    return ipo_effects
end

function refine_exception_type(@nospecialize(exc_bestguess), ipo_effects::Effects)
    ipo_effects.nothrow && return Bottom
    return exc_bestguess
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
    end
    # inspect whether our inference had a limited result accuracy,
    # else it may be suitable to cache
    bestguess = me.bestguess = cycle_fix_limited(me.bestguess, me)
    exc_bestguess = me.exc_bestguess = cycle_fix_limited(me.exc_bestguess, me)
    limited_ret = bestguess isa LimitedAccuracy || exc_bestguess isa LimitedAccuracy
    limited_src = false
    if !limited_ret
        gt = me.ssavaluetypes
        for j = 1:length(gt)
            gt[j] = gtj = cycle_fix_limited(gt[j], me)
            if gtj isa LimitedAccuracy && me.parent !== nothing
                limited_src = true
                break
            end
        end
    end
    me.result.valid_worlds = me.valid_worlds
    me.result.result = bestguess
    ipo_effects = me.result.ipo_effects = me.ipo_effects = adjust_effects(me)
    me.result.exc_result = me.exc_bestguess = refine_exception_type(me.exc_bestguess, ipo_effects)

    if limited_ret
        # a parent may be cached still, but not this intermediate work:
        # we can throw everything else away now
        me.result.src = nothing
        me.cache_mode = CACHE_MODE_NULL
        set_inlineable!(me.src, false)
        unlock_mi_inference(interp, me.linfo)
    elseif limited_src
        # a type result will be cached still, but not this intermediate work:
        # we can throw everything else away now
        me.result.src = nothing
        set_inlineable!(me.src, false)
    else
        # annotate fulltree with type information,
        # either because we are the outermost code, or we might use this later
        type_annotate!(interp, me)
        mayopt = may_optimize(interp)
        doopt = mayopt &&
                # disable optimization if we don't use this later
                (me.cache_mode != CACHE_MODE_NULL || me.parent !== nothing) &&
                # disable optimization if we've already obtained very accurate result
                !result_is_constabi(interp, me.result, mayopt)
        if doopt
            me.result.src = OptimizationState(me, interp)
        else
            me.result.src = me.src # for reflection etc.
        end
    end

    maybe_validate_code(me.linfo, me.src, "inferred")
    nothing
end

# record the backedges
store_backedges(caller::InferenceResult, edges::Vector{Any}) = store_backedges(caller.linfo, edges)
function store_backedges(caller::MethodInstance, edges::Vector{Any})
    isa(caller.def, Method) || return nothing # don't add backedges to toplevel method instance
    for itr in BackedgeIterator(edges)
        callee = itr.caller
        if isa(callee, MethodInstance)
            ccall(:jl_method_instance_add_backedge, Cvoid, (Any, Any, Any), callee, itr.sig, caller)
        else
            typeassert(callee, MethodTable)
            ccall(:jl_method_table_add_backedge, Cvoid, (Any, Any, Any), callee, itr.sig, caller)
        end
    end
    return nothing
end

function record_slot_assign!(sv::InferenceState)
    # look at all assignments to slots
    # and union the set of types stored there
    # to compute a lower bound on the storage required
    body = sv.src.code::Vector{Any}
    slottypes = sv.slottypes::Vector{Any}
    ssavaluetypes = sv.ssavaluetypes
    for i = 1:length(body)
        expr = body[i]
        # find all reachable assignments to locals
        if was_reached(sv, i) && isexpr(expr, :(=))
            lhs = expr.args[1]
            if isa(lhs, SlotNumber)
                typ = ssavaluetypes[i]
                @assert typ !== NOT_FOUND "active slot in unreached region"
                vt = widenconst(typ)
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

# find the dominating assignment to the slot `id` in the block containing statement `idx`,
# returns `nothing` otherwise
function find_dominating_assignment(id::Int, idx::Int, sv::InferenceState)
    block = block_for_inst(sv.cfg, idx)
    for pc in reverse(sv.cfg.blocks[block].stmts) # N.B. reverse since the last assignment is dominating this block
        pc < idx || continue # N.B. needs pc ≠ idx as `id` can be assigned at `idx`
        stmt = sv.src.code[pc]
        isexpr(stmt, :(=)) || continue
        lhs = stmt.args[1]
        isa(lhs, SlotNumber) || continue
        slot_id(lhs) == id || continue
        return pc
    end
    return nothing
end

# annotate types of all symbols in AST, preparing for optimization
function type_annotate!(interp::AbstractInterpreter, sv::InferenceState)
    # widen `Conditional`s from `slottypes`
    slottypes = sv.slottypes
    for i = 1:length(slottypes)
        slottypes[i] = widenconditional(slottypes[i])
    end

    # compute the required type for each slot
    # to hold all of the items assigned into it
    record_slot_assign!(sv)

    # annotate variables load types
    src = sv.src
    stmts = src.code
    nstmt = length(stmts)
    ssavaluetypes = sv.ssavaluetypes
    nslots = length(src.slotflags)

    # widen slot wrappers (`Conditional` and `MustAlias`) and remove `NOT_FOUND` from `ssavaluetypes`
    # and mark any unreachable statements by wrapping them in Const(...), to distinguish them from
    # must-throw statements which also have type Bottom
    for i = 1:nstmt
        expr = stmts[i]
        if was_reached(sv, i)
            ssavaluetypes[i] = widenslotwrapper(ssavaluetypes[i]) # 3
        else # i.e. any runtime execution will never reach this statement
            push!(sv.unreachable, i)
            if is_meta_expr(expr) # keep any lexically scoped expressions
                ssavaluetypes[i] = Any # 3
            else
                ssavaluetypes[i] = Bottom # 3
                # annotate that this statement actually is dead
                stmts[i] = Const(expr)
            end
        end
    end

    # widen slot wrappers (`Conditional` and `MustAlias`) in `bb_vartables`
    for varstate in sv.bb_vartables
        if varstate !== nothing
            for slot in 1:nslots
                vt = varstate[slot]
                widened_type = widenslotwrapper(ignorelimited(vt.typ))
                varstate[slot] = VarState(widened_type, vt.undef)
            end
        end
    end

    return nothing
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

function merge_call_chain!(interp::AbstractInterpreter, parent::InferenceState, ancestor::InferenceState, child::InferenceState)
    # add backedge of parent <- child
    # then add all backedges of parent <- parent.parent
    # and merge all of the callers into ancestor.callers_in_cycle
    # and ensure that walking the parent list will get the same result (DAG) from everywhere
    while true
        add_cycle_backedge!(parent, child, parent.currpc)
        union_caller_cycle!(ancestor, child)
        child = parent
        child === ancestor && break
        parent = frame_parent(child)
        while !isa(parent, InferenceState)
            # XXX we may miss some edges here?
            parent = frame_parent(parent::IRInterpretationState)
        end
        parent = parent::InferenceState
    end
end

function is_same_frame(interp::AbstractInterpreter, mi::MethodInstance, frame::InferenceState)
    return mi === frame_instance(frame) && cache_owner(interp) === cache_owner(frame.interp)
end

function poison_callstack!(infstate::InferenceState, topmost::InferenceState)
    push!(infstate.pclimitations, topmost)
    nothing
end

# Walk through `mi`'s upstream call chain, starting at `parent`. If a parent
# frame matching `mi` is encountered, then there is a cycle in the call graph
# (i.e. `mi` is a descendant callee of itself). Upon encountering this cycle,
# we "resolve" it by merging the call chain, which entails unioning each intermediary
# frame's `callers_in_cycle` field and adding the appropriate backedges. Finally,
# we return `mi`'s pre-existing frame. If no cycles are found, `nothing` is
# returned instead.
function resolve_call_cycle!(interp::AbstractInterpreter, mi::MethodInstance, parent::AbsIntState)
    # TODO (#48913) implement a proper recursion handling for irinterp:
    # This works just because currently the `:terminate` condition guarantees that
    # irinterp doesn't fail into unresolved cycles, but it's not a good solution.
    # We should revisit this once we have a better story for handling cycles in irinterp.
    isa(parent, InferenceState) || return false
    frame = parent
    uncached = false
    while isa(frame, InferenceState)
        uncached |= !is_cached(frame) # ensure we never add an uncached frame to a cycle
        if is_same_frame(interp, mi, frame)
            if uncached
                # our attempt to speculate into a constant call lead to an undesired self-cycle
                # that cannot be converged: poison our call-stack (up to the discovered duplicate frame)
                # with the limited flag and abort (set return type to Any) now
                poison_callstack!(parent, frame)
                return true
            end
            merge_call_chain!(interp, parent, frame, frame)
            return frame
        end
        for caller in callers_in_cycle(frame)
            if is_same_frame(interp, mi, caller)
                if uncached
                    poison_callstack!(parent, frame)
                    return true
                end
                merge_call_chain!(interp, parent, frame, caller)
                return caller
            end
        end
        frame = frame_parent(frame)
    end
    return false
end

ipo_effects(code::CodeInstance) = decode_effects(code.ipo_purity_bits)

struct EdgeCallResult
    rt
    exct
    edge::Union{Nothing,MethodInstance}
    effects::Effects
    volatile_inf_result::Union{Nothing,VolatileInferenceResult}
    function EdgeCallResult(@nospecialize(rt), @nospecialize(exct),
                            edge::Union{Nothing,MethodInstance},
                            effects::Effects,
                            volatile_inf_result::Union{Nothing,VolatileInferenceResult} = nothing)
        return new(rt, exct, edge, effects, volatile_inf_result)
    end
end

# return cached result of regular inference
function return_cached_result(::AbstractInterpreter, codeinst::CodeInstance, caller::AbsIntState)
    rt = cached_return_type(codeinst)
    effects = ipo_effects(codeinst)
    update_valid_age!(caller, WorldRange(min_world(codeinst), max_world(codeinst)))
    return EdgeCallResult(rt, codeinst.exctype, codeinst.def, effects)
end

# compute (and cache) an inferred AST and return the current best estimate of the result type
function typeinf_edge(interp::AbstractInterpreter, method::Method, @nospecialize(atype), sparams::SimpleVector, caller::AbsIntState)
    mi = specialize_method(method, atype, sparams)::MethodInstance
    codeinst = get(code_cache(interp), mi, nothing)
    force_inline = is_stmt_inline(get_curr_ssaflag(caller))
    if codeinst isa CodeInstance # return existing rettype if the code is already inferred
        inferred = @atomic :monotonic codeinst.inferred
        if inferred === nothing && force_inline
            # we already inferred this edge before and decided to discard the inferred code,
            # nevertheless we re-infer it here again in order to propagate the re-inferred
            # source to the inliner as a volatile result
            cache_mode = CACHE_MODE_VOLATILE
        else
            @assert codeinst.def === mi "MethodInstance for cached edge does not match"
            return return_cached_result(interp, codeinst, caller)
        end
    else
        cache_mode = CACHE_MODE_GLOBAL # cache edge targets globally by default
    end
    if ccall(:jl_get_module_infer, Cint, (Any,), method.module) == 0 && !generating_output(#=incremental=#false)
        add_remark!(interp, caller, "Inference is disabled for the target module")
        return EdgeCallResult(Any, Any, nothing, Effects())
    end
    if !is_cached(caller) && frame_parent(caller) === nothing
        # this caller exists to return to the user
        # (if we asked resolve_call_cycle!, it might instead detect that there is a cycle that it can't merge)
        frame = false
    else
        frame = resolve_call_cycle!(interp, mi, caller)
    end
    if frame === false
        # completely new
        lock_mi_inference(interp, mi)
        result = InferenceResult(mi, typeinf_lattice(interp))
        frame = InferenceState(result, cache_mode, interp) # always use the cache for edge targets
        if frame === nothing
            add_remark!(interp, caller, "Failed to retrieve source")
            # can't get the source for this, so we know nothing
            unlock_mi_inference(interp, mi)
            return EdgeCallResult(Any, Any, nothing, Effects())
        end
        if is_cached(caller) || frame_parent(caller) !== nothing # don't involve uncached functions in cycle resolution
            frame.parent = caller
        end
        typeinf(interp, frame)
        update_valid_age!(caller, frame.valid_worlds)
        isinferred = is_inferred(frame)
        edge = isinferred ? mi : nothing
        effects = isinferred ? frame.result.ipo_effects : adjust_effects(Effects(), method) # effects are adjusted already within `finish` for ipo_effects
        exc_bestguess = refine_exception_type(frame.exc_bestguess, effects)
        # propagate newly inferred source to the inliner, allowing efficient inlining w/o deserialization:
        # note that this result is cached globally exclusively, so we can use this local result destructively
        volatile_inf_result = isinferred ? VolatileInferenceResult(result) : nothing
        return EdgeCallResult(frame.bestguess, exc_bestguess, edge, effects, volatile_inf_result)
    elseif frame === true
        # unresolvable cycle
        return EdgeCallResult(Any, Any, nothing, Effects())
    end
    # return the current knowledge about this cycle
    frame = frame::InferenceState
    update_valid_age!(caller, frame.valid_worlds)
    effects = adjust_effects(Effects(), method)
    exc_bestguess = refine_exception_type(frame.exc_bestguess, effects)
    return EdgeCallResult(frame.bestguess, exc_bestguess, nothing, effects)
end

function cached_return_type(code::CodeInstance)
    rettype = code.rettype
    isdefined(code, :rettype_const) || return rettype
    rettype_const = code.rettype_const
    # the second subtyping/egal conditions are necessary to distinguish usual cases
    # from rare cases when `Const` wrapped those extended lattice type objects
    if isa(rettype_const, Vector{Any}) && !(Vector{Any} <: rettype)
        return PartialStruct(rettype, rettype_const)
    elseif isa(rettype_const, PartialOpaque) && rettype <: Core.OpaqueClosure
        return rettype_const
    elseif isa(rettype_const, InterConditional) && rettype !== InterConditional
        return rettype_const
    elseif isa(rettype_const, InterMustAlias) && rettype !== InterMustAlias
        return rettype_const
    else
        return Const(rettype_const)
    end
end

#### entry points for inferring a MethodInstance given a type signature ####

"""
    codeinfo_for_const(interp::AbstractInterpreter, mi::MethodInstance, worlds::WorldRange, @nospecialize(val))

Return a fake CodeInfo that just contains `return \$val`. This function is used in various reflection APIs when asking
for the code of a function that inference has found to just return a constant. For such functions, no code is actually
stored - the constant is used directly. However, because this is an ABI implementation detail, it is nice to maintain
consistency and just synthesize a CodeInfo when the reflection APIs ask for them - this function does that.
"""
function codeinfo_for_const(interp::AbstractInterpreter, mi::MethodInstance, @nospecialize(val))
    method = mi.def::Method
    tree = ccall(:jl_new_code_info_uninit, Ref{CodeInfo}, ())
    tree.code = Any[ ReturnNode(quoted(val)) ]
    nargs = Int(method.nargs)
    tree.slotnames = ccall(:jl_uncompress_argnames, Vector{Symbol}, (Any,), method.slot_syms)
    tree.slotflags = fill(0x00, nargs)
    tree.ssavaluetypes = 1
    tree.debuginfo = DebugInfo(mi)
    tree.ssaflags = UInt32[0]
    set_inlineable!(tree, true)
    tree.parent = mi
    return tree
end

"""
    codeinstance_for_const_with_code(interp::AbstractInterpreter, code::CodeInstance)

Given a constabi `CodeInstance`, create another (uncached) CodeInstance that contains the dummy code created
by [`codeinfo_for_const`](@ref) for use in reflection functions that require this. See [`codeinfo_for_const`](@ref) for
more details.
"""
function codeinstance_for_const_with_code(interp::AbstractInterpreter, code::CodeInstance)
    src = codeinfo_for_const(interp, code.def, code.rettype_const)
    return CodeInstance(code.def, cache_owner(interp), code.rettype, code.exctype, code.rettype_const, src,
        Int32(0x3), code.min_world, code.max_world,
        code.ipo_purity_bits, code.purity_bits, code.analysis_results,
        code.relocatability, src.debuginfo)
end

result_is_constabi(interp::AbstractInterpreter, result::InferenceResult,
                   run_optimizer::Bool=may_optimize(interp)) =
    run_optimizer && may_discard_trees(interp) && is_result_constabi_eligible(result)

# compute an inferred AST and return type
typeinf_code(interp::AbstractInterpreter, match::MethodMatch, run_optimizer::Bool) =
    typeinf_code(interp, specialize_method(match), run_optimizer)
typeinf_code(interp::AbstractInterpreter, method::Method, @nospecialize(atype), sparams::SimpleVector,
             run_optimizer::Bool) =
    typeinf_code(interp, specialize_method(method, atype, sparams), run_optimizer)
function typeinf_code(interp::AbstractInterpreter, mi::MethodInstance, run_optimizer::Bool)
    frame = typeinf_frame(interp, mi, run_optimizer)
    frame === nothing && return nothing, Any
    is_inferred(frame) || return nothing, Any
    if result_is_constabi(interp, frame.result, run_optimizer)
        rt = frame.result.result::Const
        return codeinfo_for_const(interp, frame.linfo, rt.val), widenconst(rt)
    end
    code = frame.src
    rt = widenconst(ignorelimited(frame.result.result))
    return code, rt
end

"""
    typeinf_ircode(interp::AbstractInterpreter, match::MethodMatch,
                   optimize_until::Union{Integer,AbstractString,Nothing}) -> (ir::Union{IRCode,Nothing}, returntype::Type)
    typeinf_ircode(interp::AbstractInterpreter,
                   method::Method, atype, sparams::SimpleVector,
                   optimize_until::Union{Integer,AbstractString,Nothing}) -> (ir::Union{IRCode,Nothing}, returntype::Type)
    typeinf_ircode(interp::AbstractInterpreter, mi::MethodInstance,
                   optimize_until::Union{Integer,AbstractString,Nothing}) -> (ir::Union{IRCode,Nothing}, returntype::Type)

Infer a `method` and return an `IRCode` with inferred `returntype` on success.
"""
typeinf_ircode(interp::AbstractInterpreter, match::MethodMatch,
               optimize_until::Union{Integer,AbstractString,Nothing}) =
    typeinf_ircode(interp, specialize_method(match), optimize_until)
typeinf_ircode(interp::AbstractInterpreter, method::Method, @nospecialize(atype), sparams::SimpleVector,
               optimize_until::Union{Integer,AbstractString,Nothing}) =
    typeinf_ircode(interp, specialize_method(method, atype, sparams), optimize_until)
function typeinf_ircode(interp::AbstractInterpreter, mi::MethodInstance,
                        optimize_until::Union{Integer,AbstractString,Nothing})
    start_time = ccall(:jl_typeinf_timing_begin, UInt64, ())
    frame = typeinf_frame(interp, mi, false)
    if frame === nothing
        ccall(:jl_typeinf_timing_end, Cvoid, (UInt64,), start_time)
        return nothing, Any
    end
    (; result) = frame
    opt = OptimizationState(frame, interp)
    ir = run_passes_ipo_safe(opt.src, opt, result, optimize_until)
    rt = widenconst(ignorelimited(result.result))
    ccall(:jl_typeinf_timing_end, Cvoid, (UInt64,), start_time)
    return ir, rt
end

# compute an inferred frame
typeinf_frame(interp::AbstractInterpreter, match::MethodMatch, run_optimizer::Bool) =
    typeinf_frame(interp, specialize_method(match), run_optimizer)
typeinf_frame(interp::AbstractInterpreter, method::Method, @nospecialize(atype), sparams::SimpleVector,
              run_optimizer::Bool) =
    typeinf_frame(interp, specialize_method(method, atype, sparams), run_optimizer)
function typeinf_frame(interp::AbstractInterpreter, mi::MethodInstance, run_optimizer::Bool)
    start_time = ccall(:jl_typeinf_timing_begin, UInt64, ())
    result = InferenceResult(mi, typeinf_lattice(interp))
    cache_mode = run_optimizer ? :global : :no
    frame = InferenceState(result, cache_mode, interp)
    frame === nothing && return nothing
    typeinf(interp, frame)
    ccall(:jl_typeinf_timing_end, Cvoid, (UInt64,), start_time)
    return frame
end

# N.B.: These need to be aligned with the C side headers
"""
    SOURCE_MODE_NOT_REQUIRED

Indicates to inference that the source is not required and the only fields
of the resulting `CodeInstance` that the caller is interested in are types
and effects. Inference is still free to create a CodeInstance with source,
but is not required to do so.
"""
const SOURCE_MODE_NOT_REQUIRED = 0x0

"""
    SOURCE_MODE_ABI

Indicates to inference that it should return a CodeInstance that can
either be `->invoke`'d (because it has already been compiled or because
it has constabi) or one that can be made so by compiling its `->inferred`
field.

N.B.: The `->inferred` field is volatile and the compiler may delete it.
In such a case, it will first set the `invoke` field to a method that
will block the thread until compilation is completed.
"""
const SOURCE_MODE_ABI = 0x1

"""
    SOURCE_MODE_FORCE_SOURCE

Indicates that inference must always produce source in the `->inferred` field.
This may mean that inference will need to re-do inference (if the `->inferred`
field was previously deleted by the JIT) or may need to synthesize source for
other kinds of CodeInstances.

N.B.: The same caching considerations as SOURCE_MODE_ABI apply.
"""
const SOURCE_MODE_FORCE_SOURCE = 0x2

"""
    SOURCE_MODE_FORCE_SOURCE_UNCACHED

Like `SOURCE_MODE_FORCE_SOURCE`, but ensures that the resulting code instance is
not part of the cache hierarchy, so the `->inferred` field may be safely used
without the possibility of deletion by the compiler.
"""
const SOURCE_MODE_FORCE_SOURCE_UNCACHED = 0x3

function ci_has_source(code::CodeInstance)
    inf = @atomic :monotonic code.inferred
    return isa(inf, CodeInfo) || isa(inf, String)
end

"""
    ci_has_abi(code::CodeInstance)

Determine whether this CodeInstance is something that could be invoked if we gave it
to the runtime system (either because it already has an ->invoke ptr, or because it
has source that could be compiled).
"""
function ci_has_abi(code::CodeInstance)
    ci_has_source(code) && return true
    return code.invoke !== C_NULL
end

function ci_meets_requirement(code::CodeInstance, source_mode::UInt8, ci_is_cached::Bool)
    source_mode == SOURCE_MODE_NOT_REQUIRED && return true
    source_mode == SOURCE_MODE_ABI && return ci_has_abi(code)
    source_mode == SOURCE_MODE_FORCE_SOURCE && return ci_has_source(code)
    source_mode == SOURCE_MODE_FORCE_SOURCE_UNCACHED && return (!ci_is_cached && ci_has_source(code))
    return false
end

_uncompressed_ir(codeinst::CodeInstance, s::String) =
    ccall(:jl_uncompress_ir, Ref{CodeInfo}, (Any, Any, Any), codeinst.def.def::Method, codeinst, s)

# compute (and cache) an inferred AST and return type
function typeinf_ext(interp::AbstractInterpreter, mi::MethodInstance, source_mode::UInt8)
    start_time = ccall(:jl_typeinf_timing_begin, UInt64, ())
    code = get(code_cache(interp), mi, nothing)
    if code isa CodeInstance
        # see if this code already exists in the cache
        if source_mode in (SOURCE_MODE_FORCE_SOURCE, SOURCE_MODE_FORCE_SOURCE_UNCACHED) && use_const_api(code)
            code = codeinstance_for_const_with_code(interp, code)
            ccall(:jl_typeinf_timing_end, Cvoid, (UInt64,), start_time)
            return code
        end
        if ci_meets_requirement(code, source_mode, true)
            ccall(:jl_typeinf_timing_end, Cvoid, (UInt64,), start_time)
            return code
        end
    end
    def = mi.def
    if isa(def, Method)
        if ccall(:jl_get_module_infer, Cint, (Any,), def.module) == 0 && !generating_output(#=incremental=#false)
            src = retrieve_code_info(mi, get_inference_world(interp))
            src isa CodeInfo || return nothing
            return CodeInstance(mi, cache_owner(interp), Any, Any, nothing, src, Int32(0),
                get_inference_world(interp), get_inference_world(interp),
                UInt32(0), UInt32(0), nothing, UInt8(0), src.debuginfo)
        end
    end
    lock_mi_inference(interp, mi)
    result = InferenceResult(mi, typeinf_lattice(interp))
    frame = InferenceState(result, #=cache_mode=#source_mode == SOURCE_MODE_FORCE_SOURCE_UNCACHED ? :volatile : :global, interp)
    frame === nothing && return nothing
    typeinf(interp, frame)
    ccall(:jl_typeinf_timing_end, Cvoid, (UInt64,), start_time)
    if isdefined(result, :ci)
        if ci_meets_requirement(result.ci, source_mode, true)
            # Inference result was cacheable and is in global cache. Return it.
            return result.ci
        elseif use_const_api(result.ci)
            code = codeinstance_for_const_with_code(interp, result.ci)
            @assert ci_meets_requirement(code, source_mode, false)
            return code
        end
    end

    # Inference result is not cacheable or is was cacheable, but we do not want to
    # store the source in the cache, but the caller wanted it anyway (e.g. for reflection).
    # We construct a new CodeInstance for it that is not part of the cache hierarchy.
    can_discard_trees = source_mode ≠ SOURCE_MODE_FORCE_SOURCE &&
                        source_mode ≠ SOURCE_MODE_FORCE_SOURCE_UNCACHED &&
                        is_result_constabi_eligible(result)
    code = CodeInstance(interp, result; can_discard_trees)

    # If the caller cares about the code and this is constabi, still use our synthesis function
    # anyway, because we will have not finished inferring the code inside the CodeInstance once
    # we realized it was constabi, but we want reflection to pretend that we did.
    if use_const_api(code) && source_mode in (SOURCE_MODE_FORCE_SOURCE, SOURCE_MODE_FORCE_SOURCE_UNCACHED)
        return codeinstance_for_const_with_code(interp, code)
    end
    @assert ci_meets_requirement(code, source_mode, false)
    return code
end

# compute (and cache) an inferred AST and return the inferred return type
function typeinf_type(interp::AbstractInterpreter, method::Method, @nospecialize(atype), sparams::SimpleVector)
    if contains_is(unwrap_unionall(atype).parameters, Union{})
        return Union{} # don't ask: it does weird and unnecessary things, if it occurs during bootstrap
    end
    return typeinf_type(interp, specialize_method(method, atype, sparams))
end
typeinf_type(interp::AbstractInterpreter, match::MethodMatch) =
    typeinf_type(interp, specialize_method(match))
function typeinf_type(interp::AbstractInterpreter, mi::MethodInstance)
    start_time = ccall(:jl_typeinf_timing_begin, UInt64, ())
    code = get(code_cache(interp), mi, nothing)
    if code isa CodeInstance
        # see if this rettype already exists in the cache
        ccall(:jl_typeinf_timing_end, Cvoid, (UInt64,), start_time)
        return code.rettype
    end
    result = InferenceResult(mi, typeinf_lattice(interp))
    typeinf(interp, result, :global)
    ccall(:jl_typeinf_timing_end, Cvoid, (UInt64,), start_time)
    is_inferred(result) || return nothing
    return widenconst(ignorelimited(result.result))
end

# This is a bridge for the C code calling `jl_typeinf_func()`
typeinf_ext_toplevel(mi::MethodInstance, world::UInt, source_mode::UInt8) = typeinf_ext_toplevel(NativeInterpreter(world), mi, source_mode)
function typeinf_ext_toplevel(interp::AbstractInterpreter, mi::MethodInstance, source_mode::UInt8)
    return typeinf_ext(interp, mi, source_mode)
end

function return_type(@nospecialize(f), t::DataType) # this method has a special tfunc
    world = tls_world_age()
    args = Any[_return_type, NativeInterpreter(world), Tuple{Core.Typeof(f), t.parameters...}]
    return ccall(:jl_call_in_typeinf_world, Any, (Ptr{Ptr{Cvoid}}, Cint), args, length(args))
end

function return_type(@nospecialize(f), t::DataType, world::UInt)
    return return_type(Tuple{Core.Typeof(f), t.parameters...}, world)
end

function return_type(t::DataType)
    world = tls_world_age()
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
        for match in _methods_by_ftype(t, -1, get_inference_world(interp))::Vector
            ty = typeinf_type(interp, match::MethodMatch)
            ty === nothing && return Any
            rt = tmerge(rt, ty)
            rt === Any && break
        end
    end
    return rt
end
