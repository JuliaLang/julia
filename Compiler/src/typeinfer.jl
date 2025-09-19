# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
The module `Core.Compiler.Timings` provides a simple implementation of nested timers that
can be used to measure the exclusive time spent inferring each method instance that is
recursively inferred during type inference.

This is meant to be internal to the compiler, and makes some specific assumptions about
being used for this purpose alone.
"""
module Timings

using ..Core
using ..Compiler: -, +, :, Vector, length, first, empty!, push!, pop!, @inline,
    @inbounds, copy, backtrace, _time_ns

# What we record for any given frame we infer during type inference.
struct InferenceFrameInfo
    mi::Core.MethodInstance
    sptypes::Vector{Compiler.VarState}
    slottypes::Vector{Any}
    nargs::Int
end

function _typeinf_identifier(frame::Compiler.InferenceState)
    mi_info = InferenceFrameInfo(
        frame.linfo,
        copy(frame.sptypes),
        copy(frame.slottypes),
        length(frame.result.argtypes),
    )
    return mi_info
end

_typeinf_identifier(frame::InferenceFrameInfo) = frame

"""
    Compiler.Timing(mi_info, start_time, ...)

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
const ROOTmi = Compiler.specialize_method(
    first(Compiler.methods(ROOT)), Tuple{typeof(ROOT)}, Core.svec())
"""
    Compiler.reset_timings()

Empty out the previously recorded type inference timings (`Compiler._timings`), and
start the ROOT() timer again. `ROOT()` measures all time spent _outside_ inference.
"""
function reset_timings() end
push!(_timings, Timing(
    # The MethodInstance for ROOT(), and default empty values for other fields.
    InferenceFrameInfo(ROOTmi, Compiler.VarState[], Any[Core.Const(ROOT)], 1),
    _time_ns()))
function close_current_timer() end
function enter_new_timer(frame) end
function exit_current_timer(_expected_frame_) end

end  # module Timings

"""
    Compiler.__set_measure_typeinf(onoff::Bool)

If set to `true`, record per-method-instance timings within type inference in the Compiler.
"""
__set_measure_typeinf(onoff::Bool) = __measure_typeinf__[] = onoff
const __measure_typeinf__ = RefValue{Bool}(false)

function result_edges(::AbstractInterpreter, caller::InferenceState)
    result = caller.result
    opt = result.src
    if isa(opt, OptimizationState)
        return Core.svec(opt.inlining.edges...)
    else
        return Core.svec(caller.edges...)
    end
end

function finish!(interp::AbstractInterpreter, caller::InferenceState, validation_world::UInt, time_before::UInt64)
    result = caller.result
    edges = result_edges(interp, caller)
    #@assert last(result.valid_worlds) <= get_world_counter() || isempty(edges)
    if isdefined(result, :ci)
        ci = result.ci
        mi = result.linfo
        inferred_result = nothing
        uncompressed = result.src
        const_flag = is_result_constabi_eligible(result)
        debuginfo = nothing
        discard_src = caller.cache_mode === CACHE_MODE_NULL || const_flag
        if !discard_src
            inferred_result = transform_result_for_cache(interp, result, edges)
            if inferred_result !== nothing
                uncompressed = inferred_result
                debuginfo = get_debuginfo(inferred_result)
                # Inlining may fast-path the global cache via `VolatileInferenceResult`, so store it back here
                result.src = inferred_result
            else
                if isa(result.src, OptimizationState)
                    debuginfo = get_debuginfo(ir_to_codeinf!(result.src))
                elseif isa(result.src, CodeInfo)
                    debuginfo = get_debuginfo(result.src)
                end
            end
            # TODO: do we want to augment edges here with any :invoke targets that we got from inlining (such that we didn't have a direct edge to it already)?
            if inferred_result isa CodeInfo
                if may_compress(interp)
                    nslots = length(inferred_result.slotflags)
                    resize!(inferred_result.slottypes::Vector{Any}, nslots)
                    resize!(inferred_result.slotnames, nslots)
                end
                inferred_result = maybe_compress_codeinfo(interp, mi, inferred_result)
                result.is_src_volatile = false
            elseif ci.owner === nothing
                # The global cache can only handle objects that codegen understands
                inferred_result = nothing
            end
        end
        if debuginfo === nothing
            debuginfo = DebugInfo(mi)
        end
        # if we aren't cached, we don't need this edge
        # but our caller might, so let's just make it anyways
        if last(result.valid_worlds) >= validation_world
            # if we can record all of the backedges in the global reverse-cache,
            # we can now widen our applicability in the global cache too
            store_backedges(ci, edges)
        end
        min_world, max_world = first(result.valid_worlds), last(result.valid_worlds)
        ipo_effects = encode_effects(result.ipo_effects)
        time_now = _time_ns()
        time_self_ns = caller.time_self_ns + (time_now - time_before)
        time_total = (time_now - caller.time_start - caller.time_paused) * 1e-9
        ccall(:jl_update_codeinst, Cvoid, (Any, Any, Int32, UInt, UInt, UInt32, Any, Float64, Float64, Float64, Any, Any),
            ci, inferred_result, const_flag, min_world, max_world, ipo_effects,
            result.analysis_results, time_total, caller.time_caches, time_self_ns * 1e-9, debuginfo, edges)
        if is_cached(caller) # CACHE_MODE_GLOBAL
            cache_result!(interp, result, ci)
        end
        engine_reject(interp, ci)
        codegen = codegen_cache(interp)
        if !discard_src && codegen !== nothing && (isa(uncompressed, CodeInfo) || isa(uncompressed, OptimizationState))
            if isa(uncompressed, OptimizationState)
                uncompressed = ir_to_codeinf!(uncompressed, edges)
            end
            # record that the caller could use this result to generate code when required, if desired, to avoid repeating n^2 work
            codegen[ci] = uncompressed
            if bootstrapping_compiler && inferred_result == nothing
                # This is necessary to get decent bootstrapping performance
                # when compiling the compiler to inject everything eagerly
                # where codegen can start finding and using it right away
                if mi.def isa Method && isa_compileable_sig(mi) && is_cached(caller)
                    ccall(:jl_add_codeinst_to_jit, Cvoid, (Any, Any), ci, uncompressed)
                end
            end
        end
    elseif caller.cache_mode === CACHE_MODE_LOCAL
        result.src = transform_result_for_local_cache(interp, result)
    end
    return nothing
end

function cache_result!(interp::AbstractInterpreter, result::InferenceResult, ci::CodeInstance)
    code_cache(interp)[result.linfo] = ci
    nothing
end

function finish!(interp::AbstractInterpreter, mi::MethodInstance, ci::CodeInstance, src::CodeInfo)
    user_edges = src.edges
    edges = user_edges isa SimpleVector ? user_edges : user_edges === nothing ? Core.svec() : Core.svec(user_edges...)
    const_flag = false
    di = src.debuginfo
    rettype = Any
    exctype = Any
    const_flags = 0x0
    ipo_effects = zero(UInt32)
    min_world = src.min_world
    max_world = src.max_world
    if max_world >= get_world_counter()
        max_world = typemax(UInt)
    end
    if max_world == typemax(UInt)
        # if we can record all of the backedges in the global reverse-cache,
        # we can now widen our applicability in the global cache too
        store_backedges(ci, edges)
    end
    ccall(:jl_fill_codeinst, Cvoid, (Any, Any, Any, Any, Int32, UInt, UInt, UInt32, Any, Any, Any),
        ci, rettype, exctype, nothing, const_flags, min_world, max_world, ipo_effects, nothing, di, edges)
    ccall(:jl_update_codeinst, Cvoid, (Any, Any, Int32, UInt, UInt, UInt32, Any, Float64, Float64, Float64, Any, Any),
        ci, nothing, const_flag, min_world, max_world, ipo_effects, nothing, 0.0, 0.0, 0.0, di, edges)
    code_cache(interp)[mi] = ci
    codegen = codegen_cache(interp)
    if codegen !== nothing
        codegen[ci] = src
    end
    engine_reject(interp, ci)
    return nothing
end

function finish_nocycle(::AbstractInterpreter, frame::InferenceState, time_before::UInt64)
    opt_cache = IdDict{MethodInstance,CodeInstance}()
    finishinfer!(frame, frame.interp, frame.cycleid, opt_cache)
    opt = frame.result.src
    if opt isa OptimizationState # implies `may_optimize(caller.interp) === true`
        optimize(frame.interp, opt, frame.result)
    end
    empty!(opt_cache)
    validation_world = get_world_counter()
    finish!(frame.interp, frame, validation_world, time_before)
    if isdefined(frame.result, :ci)
        # After validation, under the world_counter_lock, set max_world to typemax(UInt) for all dependencies
        # (recursively). From that point onward the ordinary backedge mechanism is responsible for maintaining
        # validity.
        ccall(:jl_promote_ci_to_current, Cvoid, (Any, UInt), frame.result.ci, validation_world)
    end
    if frame.cycleid != 0
        frames = frame.callstack::Vector{AbsIntState}
        @assert frames[end] === frame
        pop!(frames)
    end
    return nothing
end

function finish_cycle(::AbstractInterpreter, frames::Vector{AbsIntState}, cycleid::Int, time_before::UInt64)
    cycle_valid_worlds = WorldRange()
    cycle_valid_effects = EFFECTS_TOTAL
    for frameid = cycleid:length(frames)
        caller = frames[frameid]::InferenceState
        @assert caller.cycleid == cycleid
        # converge the world age range and effects for this cycle here:
        # all frames in the cycle should have the same bits of `valid_worlds` and `effects`
        # that are simply the intersection of each partial computation, without having
        # dependencies on each other (unlike rt and exct)
        cycle_valid_worlds = intersect(cycle_valid_worlds, caller.valid_worlds)
        cycle_valid_effects = merge_effects(cycle_valid_effects, caller.ipo_effects)
    end
    opt_cache = IdDict{MethodInstance,CodeInstance}()
    for frameid = cycleid:length(frames)
        caller = frames[frameid]::InferenceState
        adjust_cycle_frame!(caller, cycle_valid_worlds, cycle_valid_effects)
        finishinfer!(caller, caller.interp, cycleid, opt_cache)
        time_now = _time_ns()
        caller.time_self_ns += (time_now - time_before)
        time_before = time_now
    end
    time_caches = 0.0 # the total and adjusted time of every entry in the cycle are the same
    time_paused = UInt64(0)
    for frameid = cycleid:length(frames)
        caller = frames[frameid]::InferenceState
        opt = caller.result.src
        if opt isa OptimizationState # implies `may_optimize(caller.interp) === true`
            optimize(caller.interp, opt, caller.result)
            time_now = _time_ns()
            caller.time_self_ns += (time_now - time_before)
            time_before = time_now
        end
        time_caches += caller.time_caches
        time_paused += caller.time_paused
        caller.time_paused = UInt64(0)
        caller.time_caches = 0.0
    end
    empty!(opt_cache)
    cycletop = frames[cycleid]::InferenceState
    time_start = cycletop.time_start
    validation_world = get_world_counter()
    cis = CodeInstance[]
    for frameid = cycleid:length(frames)
        caller = frames[frameid]::InferenceState
        caller.time_start = time_start
        caller.time_caches = time_caches
        caller.time_paused = time_paused
        finish!(caller.interp, caller, validation_world, time_before)
        if isdefined(caller.result, :ci)
            push!(cis, caller.result.ci)
        end
    end
    if cycletop.parentid != 0
        parent = frames[cycletop.parentid]
        parent.time_caches += time_caches
        parent.time_paused += time_paused
    end
    # After validation, under the world_counter_lock, set max_world to typemax(UInt) for all dependencies
    # (recursively). From that point onward the ordinary backedge mechanism is responsible for maintaining
    # validity.
    ccall(:jl_promote_cis_to_current, Cvoid, (Ptr{CodeInstance}, Csize_t, UInt), cis, length(cis), validation_world)
    resize!(frames, cycleid - 1)
    return nothing
end

function adjust_cycle_frame!(sv::InferenceState, cycle_valid_worlds::WorldRange, cycle_valid_effects::Effects)
    update_valid_age!(sv, cycle_valid_worlds)
    sv.ipo_effects = cycle_valid_effects
    # traverse the callees of this cycle that are tracked within `sv.cycle_backedges`
    # and adjust their statements so that they are consistent with the new `cycle_valid_effects`
    new_flags = flags_for_effects(cycle_valid_effects)
    for (callee, pc) in sv.cycle_backedges
        old_currpc = callee.currpc
        callee.currpc = pc
        set_curr_ssaflag!(callee, new_flags, IR_FLAGS_EFFECTS)
        callee.currpc = old_currpc
    end
    return nothing
end

function get_debuginfo(src)
    isa(src, CodeInfo) && return src.debuginfo
    isa(src, OptimizationState) && return src.src.debuginfo
    return nothing
end

function is_result_constabi_eligible(result::InferenceResult)
    result_type = result.result
    return isa(result_type, Const) && is_foldable_nothrow(result.ipo_effects) && is_inlineable_constant(result_type.val)
end

function compute_inlining_cost(interp::AbstractInterpreter, result::InferenceResult)
    src = result.src
    isa(src, OptimizationState) || return MAX_INLINE_COST
    compute_inlining_cost(interp, result, src.optresult)
end

function compute_inlining_cost(interp::AbstractInterpreter, result::InferenceResult, optresult#=::OptimizationResult=#)
    return inline_cost_model(interp, result, optresult.inline_flag, optresult.ir)
end

function inline_cost_model(interp::AbstractInterpreter, result::InferenceResult,
        inline_flag::UInt8, ir::IRCode)

    inline_flag === SRC_FLAG_DECLARED_NOINLINE && return MAX_INLINE_COST

    mi = result.linfo
    (; def, specTypes) = mi
    if !isa(def, Method)
        return MAX_INLINE_COST
    end

    declared_inline = inline_flag === SRC_FLAG_DECLARED_INLINE

    rt = result.result
    @assert !(rt isa LimitedAccuracy)
    rt = widenslotwrapper(rt)

    sig = unwrap_unionall(specTypes)
    if !(isa(sig, DataType) && sig.name === Tuple.name)
        return MAX_INLINE_COST
    end
    if !declared_inline && rt === Bottom
        return MAX_INLINE_COST
    end

    if declared_inline && isdispatchtuple(specTypes)
        # obey @inline declaration if a dispatch barrier would not help
        return MIN_INLINE_COST
    else
        # compute the cost (size) of inlining this code
        params = OptimizationParams(interp)
        cost_threshold = default = params.inline_cost_threshold
        if ⊑(optimizer_lattice(interp), rt, Tuple) && !isconcretetype(widenconst(rt))
            cost_threshold += params.inline_tupleret_bonus
        end
        # if the method is declared as `@inline`, increase the cost threshold 20x
        if declared_inline
            cost_threshold += 19*default
        end
        # a few functions get special treatment
        if def.module === _topmod(def.module)
            name = def.name
            if name === :iterate || name === :unsafe_convert || name === :cconvert
                cost_threshold += 4*default
            end
        end
        return inline_cost_model(ir, params, cost_threshold)
    end
end

function transform_result_for_local_cache(interp::AbstractInterpreter, result::InferenceResult)
    ## XXX: this must perform the exact same operations as transform_result_for_cache to avoid introducing soundness bugs
    if is_result_constabi_eligible(result)
        return nothing
    end
    src = result.src
    if isa(src, OptimizationState)
        # Compute and store any information required to determine the inlineability of the callee.
        opt = src
        opt.src.inlining_cost = compute_inlining_cost(interp, result)
    end
    return src
end

function transform_result_for_cache(interp::AbstractInterpreter, result::InferenceResult, edges::SimpleVector)
    inlining_cost = nothing
    src = result.src
    if isa(src, OptimizationState)
        opt = src
        inlining_cost = compute_inlining_cost(interp, result, opt.optresult)
        discard_optimized_result(interp, inlining_cost) && return nothing
        src = ir_to_codeinf!(opt)
    end
    if isa(src, CodeInfo)
        src.edges = edges
        if inlining_cost !== nothing
            src.inlining_cost = inlining_cost
        elseif may_optimize(interp)
            src.inlining_cost = compute_inlining_cost(interp, result)
        end
    end
    return src
end

function discard_optimized_result(interp::AbstractInterpreter, inlining_cost::InlineCostType)
    may_discard_trees(interp) || return false
    return inlining_cost == MAX_INLINE_COST
end

function maybe_compress_codeinfo(interp::AbstractInterpreter, mi::MethodInstance, ci::CodeInfo)
    def = mi.def
    isa(def, Method) || return ci # don't compress toplevel code
    can_discard_trees = may_discard_trees(interp)
    cache_the_tree = !can_discard_trees || is_inlineable(ci)
    cache_the_tree || return nothing
    # TODO: do we want to augment edges here with any :invoke targets that we got from inlining (such that we didn't have a direct edge to it already)?
    may_compress(interp) && return ccall(:jl_compress_ir, String, (Any, Any), def, ci)
    return ci
end

function cycle_fix_limited(@nospecialize(typ), sv::InferenceState, cycleid::Int)
    if typ isa LimitedAccuracy
        frames = sv.callstack::Vector{AbsIntState}
        causes = typ.causes
        for frameid = cycleid:length(frames)
            caller = frames[frameid]::InferenceState
            caller in causes || continue
            causes === typ.causes && (causes = copy(causes))
            pop!(causes, caller)
            if isempty(causes)
                return typ.typ
            end
        end
        @assert sv.parentid != 0
        if causes !== typ.causes
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
    if is_effect_overridden(override, :consistent_overlay)
        ipo_effects = Effects(ipo_effects; nonoverlayed=CONSISTENT_OVERLAY)
    end
    if is_effect_overridden(override, :nortcall)
        ipo_effects = Effects(ipo_effects; nortcall=true)
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

const empty_edges = Core.svec()

# inference completed on `me`
# update the MethodInstance
function finishinfer!(me::InferenceState, interp::AbstractInterpreter, cycleid::Int,
                      opt_cache::IdDict{MethodInstance, CodeInstance})
    # prepare to run optimization passes on fulltree
    @assert isempty(me.ip)
    # inspect whether our inference had a limited result accuracy,
    # else it may be suitable to cache
    bestguess = me.bestguess = cycle_fix_limited(me.bestguess, me, cycleid)
    exc_bestguess = me.exc_bestguess = cycle_fix_limited(me.exc_bestguess, me, cycleid)
    limited_ret = bestguess isa LimitedAccuracy || exc_bestguess isa LimitedAccuracy
    limited_src = false
    if limited_ret
        @assert me.parentid != 0
    else
        gt = me.ssavaluetypes
        for j = 1:length(gt)
            gt[j] = gtj = cycle_fix_limited(gt[j], me, cycleid)
            if gtj isa LimitedAccuracy
                @assert me.parentid != 0
                limited_src = true
                break
            end
        end
    end
    result = me.result
    result.valid_worlds = me.valid_worlds
    result.result = bestguess
    ipo_effects = result.ipo_effects = me.ipo_effects = adjust_effects(me)
    result.exc_result = me.exc_bestguess = refine_exception_type(me.exc_bestguess, ipo_effects)
    me.src.rettype = widenconst(ignorelimited(bestguess))
    me.src.ssaflags = me.ssaflags
    me.src.min_world = first(me.valid_worlds)
    me.src.max_world = last(me.valid_worlds)
    istoplevel = !(me.linfo.def isa Method)
    istoplevel || compute_edges!(me) # don't add backedges to toplevel method instance

    if limited_ret || limited_src
        # A parent may be cached still, but not this intermediate work:
        # we can throw everything else away now. Caching anything can confuse later
        # heuristics to consider it worth trying to pursue compiling this further and
        # finding infinite work as a result. Avoiding caching helps to ensure there is only
        # a finite amount of work that can be discovered later (although potentially still a
        # large multiplier on it).
        result.src = nothing
        result.tombstone = true
        me.cache_mode = CACHE_MODE_NULL
        set_inlineable!(me.src, false)
    else
        # annotate fulltree with type information,
        # either because we are the outermost code, or we might use this later
        type_annotate!(interp, me)
        mayopt = may_optimize(interp)
        doopt = mayopt &&
                # disable optimization if we don't use this later (because it is not cached)
                me.cache_mode != CACHE_MODE_NULL &&
                # disable optimization if we've already obtained very accurate result
                !result_is_constabi(interp, result)
        if doopt
            result.src = OptimizationState(me, interp, opt_cache)
        else
            result.src = me.src # for reflection etc.
        end
    end

    maybe_validate_code(me.linfo, me.src, "inferred")

    # finish populating inference results into the CodeInstance if possible, and maybe cache that globally for use elsewhere
    if isdefined(result, :ci)
        result_type = result.result
        result_type isa LimitedAccuracy && (result_type = result_type.typ)
        @assert !(result_type === nothing)
        if isa(result_type, Const)
            rettype_const = result_type.val
            const_flags = is_result_constabi_eligible(result) ? 0x3 : 0x2
        elseif isa(result_type, PartialOpaque)
            rettype_const = result_type
            const_flags = 0x2
        elseif isconstType(result_type)
            rettype_const = result_type.parameters[1]
            const_flags = 0x2
        elseif isa(result_type, PartialStruct)
            rettype_const = (_getundefs(result_type), result_type.fields)
            const_flags = 0x2
        elseif isa(result_type, InterConditional)
            rettype_const = result_type
            const_flags = 0x2
        elseif isa(result_type, InterMustAlias)
            rettype_const = result_type
            const_flags = 0x2
        else
            rettype_const = nothing
            const_flags = 0x0
        end

        di = nothing
        edges = empty_edges # `edges` will be updated within `finish!`
        ci = result.ci
        min_world, max_world = first(result.valid_worlds), last(result.valid_worlds)
        ccall(:jl_fill_codeinst, Cvoid, (Any, Any, Any, Any, Int32, UInt, UInt, UInt32, Any, Any, Any),
            ci, widenconst(result_type), widenconst(result.exc_result), rettype_const, const_flags,
            min_world, max_world,
            encode_effects(result.ipo_effects), result.analysis_results, di, edges)
        if is_cached(me) # CACHE_MODE_GLOBAL
            if is_already_cached(me.interp, result, ci)
                # convert to a local cache
                engine_reject(interp, ci)
                me.cache_mode = CACHE_MODE_LOCAL
                push!(get_inference_cache(interp), result)
            else
                opt_cache[result.linfo] = ci
            end
        end
    end
    nothing
end

function is_already_cached(interp::AbstractInterpreter, result::InferenceResult, ::CodeInstance)
    # check if the existing linfo metadata is also sufficient to describe the current inference result
    # to decide if it is worth caching this right now
    mi = result.linfo
    cache = code_cache(interp, result.valid_worlds)
    if haskey(cache, mi)
        # n.b.: accurate edge representation might cause the CodeInstance for this to be constructed later
        @assert isdefined(cache[mi], :inferred)
        return true
    end
    return false
end

# Iterate a series of back-edges that need registering, based on the provided forward edge list.
# Back-edges are returned as (invokesig, item), where the item is a Binding, MethodInstance, or
# MethodTable.
struct ForwardToBackedgeIterator
    forward_edges::SimpleVector
end

function Base.iterate(it::ForwardToBackedgeIterator, i::Int = 1)
    edges = it.forward_edges
    i > length(edges) && return nothing
    while i ≤ length(edges)
        item = edges[i]
        if item isa Int
            i += 2
            continue # ignore the query information if present but process the contents
        elseif isa(item, Method)
            # ignore `Method`-edges (from e.g. failed `abstract_call_method`)
            i += 1
            continue
        elseif isa(item, Core.Binding)
            return ((nothing, item), i + 1)
        end
        if isa(item, CodeInstance)
            item = get_ci_mi(item)
            return ((nothing, item), i + 1)
        elseif isa(item, MethodInstance) # regular dispatch
            return ((nothing, item), i + 1)
        else
            invokesig = item
            callee = edges[i+1]
            isa(callee, Method) && (i += 2; continue) # ignore `Method`-edges (from e.g. failed `abstract_call_method`)
            if isa(callee, MethodTable)
                # abstract dispatch (legacy style edges)
                return ((invokesig, callee), i + 2)
            else
                # `invoke` edge
                callee = isa(callee, CodeInstance) ? get_ci_mi(callee) : callee::MethodInstance
                return ((invokesig, callee), i + 2)
            end
        end
    end
    return nothing
end

# record the backedges

function maybe_add_binding_backedge!(b::Core.Binding, edge::Union{Method, CodeInstance})
    meth = isa(edge, Method) ? edge : get_ci_mi(edge).def
    ccall(:jl_maybe_add_binding_backedge, Cint, (Any, Any, Any), b, edge, meth)
    return nothing
end

function store_backedges(caller::CodeInstance, edges::SimpleVector)
    isa(get_ci_mi(caller).def, Method) || return # don't add backedges to toplevel method instance

    backedges = ForwardToBackedgeIterator(edges)
    for (i, (invokesig, item)) in enumerate(backedges)
        # check for any duplicate edges we've already registered
        duplicate_found = false
        for (i′, (invokesig′, item′)) in enumerate(backedges)
            i == i′ && break
            if item′ === item && invokesig′ == invokesig
                duplicate_found = true
                break
            end
        end

        if !duplicate_found
            if item isa Core.Binding
                maybe_add_binding_backedge!(item, caller)
            elseif item isa MethodTable
                ccall(:jl_method_table_add_backedge, Cvoid, (Any, Any), invokesig, caller)
            else
                item::MethodInstance
                ccall(:jl_method_instance_add_backedge, Cvoid, (Any, Any, Any), item, invokesig, caller)
            end
        end
    end
    nothing
end

function compute_edges!(sv::InferenceState)
    edges = sv.edges
    for i in 1:length(sv.stmt_info)
        add_edges!(edges, sv.stmt_info[i])
    end
    user_edges = sv.src.edges
    if user_edges !== nothing && user_edges !== empty_edges
        append!(edges, user_edges)
    end
    nothing
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
function type_annotate!(::AbstractInterpreter, sv::InferenceState)
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

function merge_call_chain!(::AbstractInterpreter, parent::InferenceState, child::InferenceState)
    # add backedge of parent <- child
    # then add all backedges of parent <- parent.parent
    frames = parent.callstack::Vector{AbsIntState}
    @assert child.callstack === frames
    ancestorid = child.cycleid
    while true
        add_cycle_backedge!(parent, child)
        parent.cycleid === ancestorid && break
        child = parent
        parent = cycle_parent(child)::InferenceState
    end
    # ensure that walking the callstack has the same cycleid (DAG)
    for frameid = reverse(ancestorid:length(frames))
        frame = frames[frameid]::InferenceState
        frame.cycleid == ancestorid && break
        @assert frame.cycleid > ancestorid
        frame.cycleid = ancestorid
    end
end

function add_cycle_backedge!(caller::InferenceState, frame::InferenceState)
    update_valid_age!(caller, frame.valid_worlds)
    backedge = (caller, caller.currpc)
    contains_is(frame.cycle_backedges, backedge) || push!(frame.cycle_backedges, backedge)
    return frame
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
# we "resolve" it by merging the call chain, which entails updating each intermediary
# frame's `cycleid` field and adding the appropriate backedges. Finally,
# we return `mi`'s pre-existing frame. If no cycles are found, `nothing` is
# returned instead.
function resolve_call_cycle!(interp::AbstractInterpreter, mi::MethodInstance, parent::AbsIntState)
    # TODO (#48913) implement a proper recursion handling for irinterp:
    # This works most of the time currently just because the irinterp code doesn't get used much with
    # `@assume_effects`, so it never sees a cycle normally, but that may not be a sustainable solution.
    parent isa InferenceState || return false
    frames = parent.callstack::Vector{AbsIntState}
    uncached = false
    for frameid = reverse(1:length(frames))
        frame = frames[frameid]
        isa(frame, InferenceState) || break
        uncached |= !is_cached(frame) # ensure we never add a (globally) uncached frame to a cycle
        if is_same_frame(interp, mi, frame)
            if uncached
                # our attempt to speculate into a constant call lead to an undesired self-cycle
                # that cannot be converged: if necessary, poison our call-stack (up to the discovered duplicate frame)
                # with the limited flag and abort (set return type to Any) now
                poison_callstack!(parent, frame)
                return true
            end
            merge_call_chain!(interp, parent, frame)
            return frame
        end
    end
    return false
end

ipo_effects(code::CodeInstance) = decode_effects(code.ipo_purity_bits)

# return cached result of regular inference
function return_cached_result(interp::AbstractInterpreter, method::Method, codeinst::CodeInstance, caller::AbsIntState, edgecycle::Bool, edgelimited::Bool)
    rt = cached_return_type(codeinst)
    exct = codeinst.exctype
    effects = ipo_effects(codeinst)
    update_valid_age!(caller, WorldRange(min_world(codeinst), max_world(codeinst)))
    caller.time_caches += reinterpret(Float16, codeinst.time_infer_total)
    caller.time_caches += reinterpret(Float16, codeinst.time_infer_cache_saved)
    return Future(MethodCallResult(interp, caller, method, rt, exct, effects, codeinst, edgecycle, edgelimited))
end

function MethodCallResult(::AbstractInterpreter, sv::AbsIntState, method::Method,
                          @nospecialize(rt), @nospecialize(exct), effects::Effects,
                          edge::Union{Nothing,CodeInstance}, edgecycle::Bool, edgelimited::Bool,
                          volatile_inf_result::Union{Nothing,VolatileInferenceResult}=nothing)
    if edge === nothing
        edgecycle = edgelimited = true
    end

    # we look for the termination effect override here as well, since the :terminates effect
    # may have been tainted due to recursion at this point even if it's overridden
    if is_effect_overridden(sv, :terminates_globally)
        # this frame is known to terminate
        effects = Effects(effects, terminates=true)
    elseif is_effect_overridden(method, :terminates_globally)
        # this edge is known to terminate
        effects = Effects(effects; terminates=true)
    elseif edgecycle
        # Some sort of recursion was detected.
        if edge !== nothing && !edgelimited && !is_edge_recursed(edge, sv)
            # no `MethodInstance` cycles -- don't taint :terminate
        else
            # we cannot guarantee that the call will terminate
            effects = Effects(effects; terminates=false)
        end
    end

    return MethodCallResult(rt, exct, effects, edge, edgecycle, edgelimited, volatile_inf_result)
end

# allocate a dummy `edge::CodeInstance` to be added by `add_edges!`, reusing an existing_edge if possible
# TODO: fill this in fully correctly (currently IPO info such as effects and return types are lost)
function codeinst_as_edge(interp::AbstractInterpreter, sv::InferenceState, @nospecialize existing_edge)
    mi = sv.linfo
    min_world, max_world = first(sv.valid_worlds), last(sv.valid_worlds)
    if max_world >= get_world_counter()
        max_world = typemax(UInt)
    end
    edges = Core.svec(sv.edges...)
    if existing_edge isa CodeInstance
        # return an existing_edge, if the existing edge has more restrictions already (more edges and narrower worlds)
        if existing_edge.min_world >= min_world &&
           existing_edge.max_world <= max_world &&
           existing_edge.edges == edges
            return existing_edge
        end
    end
    ci = CodeInstance(mi, cache_owner(interp), Any, Any, nothing, nothing, zero(Int32),
        min_world, max_world, zero(UInt32), nothing, nothing, edges)
    if max_world == typemax(UInt)
        # if we can record all of the backedges in the global reverse-cache,
        # we can now widen our applicability in the global cache too
        # TODO: this should probably come after we decide this edge is even useful
        store_backedges(ci, edges)
    end
    return ci
end

# compute (and cache) an inferred AST and return the current best estimate of the result type
function typeinf_edge(interp::AbstractInterpreter, method::Method, @nospecialize(atype), sparams::SimpleVector, caller::AbsIntState, edgecycle::Bool, edgelimited::Bool)
    mi = specialize_method(method, atype, sparams)
    cache_mode = CACHE_MODE_GLOBAL # cache edge targets globally by default
    force_inline = is_stmt_inline(get_curr_ssaflag(caller))
    edge_ci = nothing
    # check cache with SOURCE_MODE_NOT_REQUIRED source_mode first,
    # then with SOURCE_MODE_GET_SOURCE if inlining is needed
    let codeinst = get(code_cache(interp), mi, nothing)
        if codeinst isa CodeInstance # return existing rettype if the code is already inferred
            inferred = @atomic :monotonic codeinst.inferred
            need_inlineable_code = may_optimize(interp) && (force_inline || is_inlineable(inferred))
            if need_inlineable_code && !ci_meets_requirement(interp, codeinst, SOURCE_MODE_GET_SOURCE)
                # Re-infer to get the appropriate source representation
                cache_mode = CACHE_MODE_LOCAL
                edge_ci = codeinst
            else # no reinference needed
                @assert codeinst.def === mi "MethodInstance for cached edge does not match"
                return return_cached_result(interp, method, codeinst, caller, edgecycle, edgelimited)
            end
        end
    end
    if !InferenceParams(interp).force_enable_inference && ccall(:jl_get_module_infer, Cint, (Any,), method.module) == 0
        add_remark!(interp, caller, "[typeinf_edge] Inference is disabled for the target module")
        return Future(MethodCallResult(interp, caller, method, Any, Any, Effects(), nothing, edgecycle, edgelimited))
    end
    if !is_cached(caller) && frame_parent(caller) === nothing
        # this caller exists to return to the user
        # (if we asked resolve_call_cycle!, it might instead detect that there is a cycle that it can't merge)
        frame = false
    else
        frame = resolve_call_cycle!(interp, mi, caller)
    end
    if frame === false
        # completely new, but check again after reserving in the engine
        if cache_mode == CACHE_MODE_GLOBAL
            reserve_start = _time_ns() # subtract engine_reserve (thread-synchronization) time from callers to avoid double-counting
            ci_from_engine = engine_reserve(interp, mi)
            caller.time_paused += (_time_ns() - reserve_start)
            edge_ci = ci_from_engine
            codeinst = get(code_cache(interp), mi, nothing)
            if codeinst isa CodeInstance # return existing rettype if the code is already inferred
                engine_reject(interp, ci_from_engine)
                ci_from_engine = nothing
                inferred = @atomic :monotonic codeinst.inferred
                need_inlineable_code = may_optimize(interp) && (force_inline || is_inlineable(inferred))
                if need_inlineable_code && !ci_meets_requirement(interp, codeinst, SOURCE_MODE_GET_SOURCE)
                    cache_mode = CACHE_MODE_LOCAL
                    edge_ci = codeinst
                else
                    @assert codeinst.def === mi "MethodInstance for cached edge does not match"
                    return return_cached_result(interp, method, codeinst, caller, edgecycle, edgelimited)
                end
            end
        else
            ci_from_engine = nothing
        end
        result = InferenceResult(mi, typeinf_lattice(interp))
        if ci_from_engine !== nothing
            result.ci = ci_from_engine
        else
            result.ci = ccall(:jl_new_codeinst_uninit, Any, (Any, Any), mi, cache_owner(interp))::CodeInstance
        end
        frame = InferenceState(result, cache_mode, interp) # always use the cache for edge targets
        if frame === nothing
            add_remark!(interp, caller, "[typeinf_edge] Failed to retrieve source")
            # can't get the source for this, so we know nothing
            if ci_from_engine !== nothing
                engine_reject(interp, ci_from_engine)
            end
            return Future(MethodCallResult(interp, caller, method, Any, Any, Effects(), nothing, edgecycle, edgelimited))
        end
        assign_parentchild!(frame, caller)
        # the actual inference task for this edge is going to be scheduled within `typeinf_local` via the callstack queue
        # while splitting off the rest of the work for this caller into a separate workq thunk
        let mresult = Future{MethodCallResult}()
            push!(caller.tasks, function get_infer_result(interp, caller)
                update_valid_age!(caller, frame.valid_worlds)
                local isinferred = is_inferred(frame)
                local edge = isinferred ? edge_ci : nothing
                local effects = isinferred ? frame.result.ipo_effects : # effects are adjusted already within `finish` for ipo_effects
                    adjust_effects(effects_for_cycle(frame.ipo_effects), method)
                local bestguess = frame.bestguess
                local exc_bestguess = refine_exception_type(frame.exc_bestguess, effects)
                # propagate newly inferred source to the inliner, allowing efficient inlining w/o deserialization:
                # note that this result is cached globally exclusively, so we can use this local result destructively
                local volatile_inf_result = if isinferred && edge_ci isa CodeInstance
                    result.ci_as_edge = edge_ci # set the edge for the inliner usage
                    VolatileInferenceResult(result)
                end
                mresult[] = MethodCallResult(interp, caller, method, bestguess, exc_bestguess, effects,
                    edge, edgecycle, edgelimited, volatile_inf_result)
                return true
            end)
            return mresult
        end
    elseif frame === true
        # unresolvable cycle
        add_remark!(interp, caller, "[typeinf_edge] Unresolvable cycle")
        return Future(MethodCallResult(interp, caller, method, Any, Any, Effects(), nothing, edgecycle, edgelimited))
    end
    # return the current knowledge about this cycle
    frame = frame::InferenceState
    update_valid_age!(caller, frame.valid_worlds)
    effects = adjust_effects(effects_for_cycle(frame.ipo_effects), method)
    bestguess = frame.bestguess
    exc_bestguess = refine_exception_type(frame.exc_bestguess, effects)
    return Future(MethodCallResult(interp, caller, method, bestguess, exc_bestguess, effects, nothing, edgecycle, edgelimited))
end

# The `:terminates` effect bit must be conservatively tainted unless recursion cycle has
# been fully resolved. As for other effects, there's no need to taint them at this moment
# because they will be tainted as we try to resolve the cycle.
effects_for_cycle(effects::Effects) = Effects(effects; terminates=false)

function cached_return_type(code::CodeInstance)
    rettype = code.rettype
    isdefined(code, :rettype_const) || return rettype
    rettype_const = code.rettype_const
    # the second subtyping/egal conditions are necessary to distinguish usual cases
    # from rare cases when `Const` wrapped those extended lattice type objects
    if isa(rettype_const, Tuple{Vector{Union{Nothing,Bool}}, Vector{Any}}) && !(Tuple{Vector{Union{Nothing,Bool}}, Vector{Any}} <: rettype)
        undefs, fields = rettype_const
        return PartialStruct(fallback_lattice, rettype, undefs, fields)
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
    codeinfo_for_const(interp::AbstractInterpreter, mi::MethodInstance, worlds::WorldRange, edges::SimpleVector, @nospecialize(val))

Return a fake CodeInfo that just contains `return \$val`. This function is used in various reflection APIs when asking
for the code of a function that inference has found to just return a constant. For such functions, no code is actually
stored - the constant is used directly. However, because this is an ABI implementation detail, it is nice to maintain
consistency and just synthesize a CodeInfo when the reflection APIs ask for them - this function does that.
"""
function codeinfo_for_const(::AbstractInterpreter, mi::MethodInstance, worlds::WorldRange, edges::SimpleVector, @nospecialize(val))
    method = mi.def::Method
    tree = ccall(:jl_new_code_info_uninit, Ref{CodeInfo}, ())
    tree.code = Any[ ReturnNode(quoted(val)) ]
    nargs = Int(method.nargs)
    tree.slotnames = ccall(:jl_uncompress_argnames, Vector{Symbol}, (Any,), method.slot_syms)
    tree.slotflags = fill(0x00, nargs)
    tree.ssavaluetypes = 1
    tree.debuginfo = DebugInfo(mi)
    tree.ssaflags = [IR_FLAG_NULL]
    tree.rettype = Core.Typeof(val)
    tree.min_world = first(worlds)
    tree.max_world = last(worlds)
    tree.edges = edges
    set_inlineable!(tree, true)
    tree.parent = mi
    return tree
end

result_is_constabi(interp::AbstractInterpreter, result::InferenceResult) =
    may_discard_trees(interp) && is_result_constabi_eligible(result)

# compute an inferred AST and return type
typeinf_code(interp::AbstractInterpreter, match::MethodMatch, run_optimizer::Bool) =
    typeinf_code(interp, specialize_method(match), run_optimizer)
typeinf_code(interp::AbstractInterpreter, method::Method, @nospecialize(atype), sparams::SimpleVector,
             run_optimizer::Bool) =
    typeinf_code(interp, specialize_method(method, atype, sparams), run_optimizer)
function typeinf_code(interp::AbstractInterpreter, mi::MethodInstance, run_optimizer::Bool)
    frame = typeinf_frame(interp, mi, run_optimizer)
    frame === nothing && return nothing
    return frame.src
end

"""
    typeinf_ircode(interp::AbstractInterpreter, match::MethodMatch,
                   optimize_until::Union{Int,String,Nothing}) -> (ir::Union{IRCode,Nothing}, returntype::Type)
    typeinf_ircode(interp::AbstractInterpreter,
                   method::Method, atype, sparams::SimpleVector,
                   optimize_until::Union{Int,String,Nothing}) -> (ir::Union{IRCode,Nothing}, returntype::Type)
    typeinf_ircode(interp::AbstractInterpreter, mi::MethodInstance,
                   optimize_until::Union{Int,String,Nothing}) -> (ir::Union{IRCode,Nothing}, returntype::Type)

Infer a `method` and return an `IRCode` with inferred `returntype` on success.
"""
typeinf_ircode(interp::AbstractInterpreter, match::MethodMatch,
               optimize_until::Union{Int,String,Nothing}) =
    typeinf_ircode(interp, specialize_method(match), optimize_until)
typeinf_ircode(interp::AbstractInterpreter, method::Method, @nospecialize(atype), sparams::SimpleVector,
               optimize_until::Union{Int,String,Nothing}) =
    typeinf_ircode(interp, specialize_method(method, atype, sparams), optimize_until)
function typeinf_ircode(interp::AbstractInterpreter, mi::MethodInstance,
                        optimize_until::Union{Int,String,Nothing})
    frame = typeinf_frame(interp, mi, false)
    if frame === nothing
        return nothing, Any
    end
    (; result) = frame
    opt = OptimizationState(frame, interp)
    ir = run_passes_ipo_safe(opt.src, opt, optimize_until)
    rt = widenconst(ignorelimited(result.result))
    return ir, rt
end

# compute an inferred frame
typeinf_frame(interp::AbstractInterpreter, match::MethodMatch, run_optimizer::Bool) =
    typeinf_frame(interp, specialize_method(match), run_optimizer)
typeinf_frame(interp::AbstractInterpreter, method::Method, @nospecialize(atype), sparams::SimpleVector,
              run_optimizer::Bool) =
    typeinf_frame(interp, specialize_method(method, atype, sparams), run_optimizer)
function typeinf_frame(interp::AbstractInterpreter, mi::MethodInstance, run_optimizer::Bool)
    result = InferenceResult(mi, typeinf_lattice(interp))
    frame = InferenceState(result, #=cache_mode=#:no, interp)
    frame === nothing && return nothing
    typeinf(interp, frame)
    is_inferred(frame) || return nothing
    if run_optimizer
        if result_is_constabi(interp, frame.result)
            rt = frame.result.result::Const
            src = codeinfo_for_const(interp, frame.linfo, frame.valid_worlds, Core.svec(frame.edges...), rt.val)
        else
            opt = OptimizationState(frame, interp)
            optimize(interp, opt, frame.result)
            src = ir_to_codeinf!(opt, frame, Core.svec(opt.inlining.edges...))
        end
        result.src = frame.src = src
    end
    return frame
end

# N.B.: These need to be aligned with the C side headers
"""
    SOURCE_MODE_NOT_REQUIRED

Indicates to inference that the source is not required and the only fields of
the resulting `CodeInstance` that the caller is interested in are return or
exception types and IPO effects. Inference is still free to create source for
it or add it to the JIT even, but is not required or expected to do so.
"""
const SOURCE_MODE_NOT_REQUIRED = 0x0

"""
    SOURCE_MODE_ABI

Indicates to inference that it should return a CodeInstance that can
be `->invoke`'d (because it has already been compiled).
"""
const SOURCE_MODE_ABI = 0x1

"""
    SOURCE_MODE_GET_SOURCE

Indicates to inference that it should return a CodeInstance after it has
prepared interp to be able to provide source code for it.
"""
const SOURCE_MODE_GET_SOURCE = 0xf

"""
    ci_has_abi(interp::AbstractInterpreter, code::CodeInstance)

Determine whether this CodeInstance is something that could be invoked if
interp gave it to the runtime system (either because it already has an ->invoke
ptr, or because interp has source that could be compiled).
"""
function ci_has_abi(interp::AbstractInterpreter, code::CodeInstance)
    (@atomic :acquire code.invoke) !== C_NULL && return true
    return ci_has_source(interp, code)
end

function ci_get_source(interp::AbstractInterpreter, code::CodeInstance)
    codegen = codegen_cache(interp)
    codegen === nothing && return nothing
    use_const_api(code) &&
        return codeinfo_for_const(interp, get_ci_mi(code), WorldRange(code.min_world, code.max_world), code.edges, code.rettype_const)
    inf = get(codegen, code, nothing)
    inf === nothing || return inf
    return @atomic :monotonic code.inferred
end

"""
    ci_has_source(interp::AbstractInterpreter, code::CodeInstance)

Determine whether this CodeInstance is something that could be compiled from
source that interp has.
"""
function ci_has_source(interp::AbstractInterpreter, code::CodeInstance)
    codegen = codegen_cache(interp)
    codegen === nothing && return false
    use_const_api(code) && return true
    haskey(codegen, code) && return true
    inf = @atomic :monotonic code.inferred
    if isa(inf, String)
        inf = _uncompressed_ir(code, inf)
    end
    if code.owner === nothing
        if isa(inf, CodeInfo)
            codegen[code] = inf
            return true
        end
    elseif inf !== nothing
        return true
    end
    return false
end

function ci_has_invoke(code::CodeInstance)
    return (@atomic :monotonic code.invoke) !== C_NULL
end

function ci_meets_requirement(interp::AbstractInterpreter, code::CodeInstance, source_mode::UInt8)
    source_mode == SOURCE_MODE_NOT_REQUIRED && return true
    source_mode == SOURCE_MODE_ABI && return ci_has_abi(interp, code)
    source_mode == SOURCE_MODE_GET_SOURCE && return ci_has_source(interp, code)
    return false
end

# compute (and cache) an inferred AST and return type
function typeinf_ext(interp::AbstractInterpreter, mi::MethodInstance, source_mode::UInt8)
    start_time = ccall(:jl_typeinf_timing_begin, UInt64, ())
    let code = get(code_cache(interp), mi, nothing)
        if code isa CodeInstance
            # see if this code already exists in the cache
            if ci_meets_requirement(interp, code, source_mode)
                ccall(:jl_typeinf_timing_end, Cvoid, (UInt64,), start_time)
                return code
            end
        end
    end
    def = mi.def
    ci = engine_reserve(interp, mi)
    # check cache again if it is still new after reserving in the engine
    let code = get(code_cache(interp), mi, nothing)
        if code isa CodeInstance
            # see if this code already exists in the cache
            if ci_meets_requirement(interp, code, source_mode)
                engine_reject(interp, ci)
                ccall(:jl_typeinf_timing_end, Cvoid, (UInt64,), start_time)
                return code
            end
        end
    end
    if !InferenceParams(interp).force_enable_inference
        if isa(def, Method) && ccall(:jl_get_module_infer, Cint, (Any,), def.module) == 0
            src = retrieve_code_info(mi, get_inference_world(interp))
            if src isa CodeInfo
                finish!(interp, mi, ci, src)
            else
                engine_reject(interp, ci)
            end
            ccall(:jl_typeinf_timing_end, Cvoid, (UInt64,), start_time)
            return ci
        end
    end
    result = InferenceResult(mi, typeinf_lattice(interp))
    result.ci = ci
    frame = InferenceState(result, #=cache_mode=#:global, interp)
    if frame === nothing
        engine_reject(interp, ci)
        ccall(:jl_typeinf_timing_end, Cvoid, (UInt64,), start_time)
        return nothing
    end
    typeinf(interp, frame)
    ccall(:jl_typeinf_timing_end, Cvoid, (UInt64,), start_time)

    ci = result.ci # reload from result in case it changed
    codegen = codegen_cache(interp)
    @assert frame.cache_mode != CACHE_MODE_NULL
    @assert is_result_constabi_eligible(result) || codegen === nothing || haskey(codegen, ci)
    @assert is_result_constabi_eligible(result) == use_const_api(ci)
    @assert isdefined(ci, :inferred) "interpreter did not fulfill our expectations"
    return ci
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
    ci = typeinf_ext(interp, mi, SOURCE_MODE_NOT_REQUIRED)
    ci isa CodeInstance || return nothing
    return ci.rettype
end

# Resolve a call, as described by `argtype` to a single matching
# Method and return a compilable MethodInstance for the call, if
# it will be runtime-dispatched to exactly that MethodInstance
function compileable_specialization_for_call(interp::AbstractInterpreter, @nospecialize(argtype))
    mt = ccall(:jl_method_table_for, Any, (Any,), argtype)
    if mt === nothing
        # this would require scanning all method tables, so give up instead
        return nothing
    end

    matches = findall(argtype, method_table(interp); limit = 1)
    matches === nothing && return nothing
    length(matches.matches) == 0 && return nothing
    match = only(matches.matches)

    compileable_atype = get_compileable_sig(match.method, match.spec_types, match.sparams)
    compileable_atype === nothing && return nothing
    if match.spec_types !== compileable_atype
        sp_ = ccall(:jl_type_intersection_with_env, Any, (Any, Any), compileable_atype, match.method.sig)::SimpleVector
        sparams = sp_[2]::SimpleVector
        mi = specialize_method(match.method, compileable_atype, sparams)
    else
        mi = specialize_method(match.method, compileable_atype, match.sparams)
    end

    return mi
end

const QueueItems = Union{CodeInstance,MethodInstance,SimpleVector}

struct CompilationQueue
    tocompile::Vector{QueueItems}
    inspected::IdSet{QueueItems}
    interp::Union{AbstractInterpreter,Nothing}

    CompilationQueue(;
        interp::Union{AbstractInterpreter,Nothing}
    ) = new(QueueItems[], IdSet{QueueItems}(), interp)

    CompilationQueue(queue::CompilationQueue;
        interp::Union{AbstractInterpreter,Nothing}
    ) = new(empty!(queue.tocompile), empty!(queue.inspected), interp)
end

Base.push!(queue::CompilationQueue, item) = push!(queue.tocompile, item)
Base.append!(queue::CompilationQueue, items) = append!(queue.tocompile, items)
Base.pop!(queue::CompilationQueue) = pop!(queue.tocompile)
Base.empty!(queue::CompilationQueue) = (empty!(queue.tocompile); empty!(queue.inspected))
markinspected!(queue::CompilationQueue, item) = push!(queue.inspected, item)
isinspected(queue::CompilationQueue, item) = item in queue.inspected
Base.isempty(queue::CompilationQueue) = isempty(queue.tocompile)

# collect a list of all code that is needed along with CodeInstance to codegen it fully
function collectinvokes!(workqueue::CompilationQueue, ci::CodeInfo, sptypes::Vector{VarState};
                         invokelatest_queue::Union{CompilationQueue,Nothing} = nothing)
    src = ci.code
    for i = 1:length(src)
        stmt = src[i]
        isexpr(stmt, :(=)) && (stmt = stmt.args[2])
        if isexpr(stmt, :invoke) || isexpr(stmt, :invoke_modify)
            edge = stmt.args[1]
            edge isa CodeInstance && isdefined(edge, :inferred) && push!(workqueue, edge)
        end

        invokelatest_queue === nothing && continue
        if isexpr(stmt, :call)
            farg = stmt.args[1]
            !applicable(argextype, farg, ci, sptypes) && continue # TODO: Why is this failing during bootstrap
            ftyp = widenconst(argextype(farg, ci, sptypes))

            if ftyp === typeof(Core.finalizer) && length(stmt.args) == 3
                finalizer = argextype(stmt.args[2], ci, sptypes)
                obj = argextype(stmt.args[3], ci, sptypes)
                atype = argtypes_to_type(Any[finalizer, obj])
            else
                # No dynamic dispatch to resolve / enqueue
                continue
            end
        elseif isexpr(stmt, :cfunction) && length(stmt.args) == 5
            (_, f, _, at, _) = stmt.args
            linfo = ci.parent

            linfo isa MethodInstance || continue
            at isa SimpleVector || continue

            ft = argextype(f, ci, sptypes)
            argtypes = Any[ft]
            for i = 1:length(at)
                push!(argtypes, sp_type_rewrap(at[i], linfo, #= isreturn =# false))
            end
            atype = argtypes_to_type(argtypes)
        else
            # TODO: handle other StmtInfo like OpaqueClosure?
            continue
        end
        let workqueue = invokelatest_queue
            # make a best-effort attempt to enqueue the relevant code for the dynamic invokelatest call
            mi = compileable_specialization_for_call(workqueue.interp, atype)
            mi === nothing && continue

            push!(workqueue, mi)
        end
    end
end

function add_codeinsts_to_jit!(interp::AbstractInterpreter, ci, source_mode::UInt8)
    source_mode == SOURCE_MODE_ABI || return ci
    ci isa CodeInstance && !ci_has_invoke(ci) || return ci
    codegen = codegen_cache(interp)
    codegen === nothing && return ci
    workqueue = CompilationQueue(; interp)
    push!(workqueue, ci)
    while !isempty(workqueue)
        # ci_has_real_invoke(ci) && return ci # optimization: cease looping if ci happens to get compiled (not just jl_fptr_wait_for_compiled, but fully jl_is_compiled_codeinst)
        callee = pop!(workqueue)
        ci_has_invoke(callee) && continue
        isinspected(workqueue, callee) && continue
        src = ci_get_source(interp, callee)
        if !isa(src, CodeInfo)
            newcallee = typeinf_ext(workqueue.interp, callee.def, source_mode) # always SOURCE_MODE_ABI
            if newcallee isa CodeInstance
                callee === ci && (ci = newcallee) # ci stopped meeting the requirements after typeinf_ext last checked, try again with newcallee
                push!(workqueue, newcallee)
            end
            if newcallee !== callee
                markinspected!(workqueue, callee)
            end
            continue
        end
        markinspected!(workqueue, callee)
        mi = get_ci_mi(callee)
        sptypes = sptypes_from_meth_instance(mi)
        collectinvokes!(workqueue, src, sptypes)
        if iszero(ccall(:jl_mi_cache_has_ci, Cint, (Any, Any), mi, callee))
            cached = ccall(:jl_get_ci_equiv, Any, (Any, UInt), callee, get_inference_world(workqueue.interp))::CodeInstance
            if cached === callee
                # make sure callee is gc-rooted and cached, as required by jl_add_codeinst_to_jit
                code_cache(workqueue.interp)[mi] = callee
            else
                # use an existing CI from the cache, if there is available one that is compatible
                callee === ci && (ci = cached)
                callee = cached
            end
        end
        ccall(:jl_add_codeinst_to_jit, Cvoid, (Any, Any), callee, src)
    end
    return ci
end

function typeinf_ext_toplevel(interp::AbstractInterpreter, mi::MethodInstance, source_mode::UInt8)
    ci = typeinf_ext(interp, mi, source_mode)
    ci = add_codeinsts_to_jit!(interp, ci, source_mode)
    return ci
end

# This is a bridge for the C code calling `jl_typeinf_func()` on a single Method match
function typeinf_ext_toplevel(mi::MethodInstance, world::UInt, source_mode::UInt8, trim_mode::UInt8)
    inf_params = InferenceParams(; force_enable_inference = trim_mode != TRIM_NO)
    interp = NativeInterpreter(world; inf_params)
    return typeinf_ext_toplevel(interp, mi, source_mode)
end

function compile!(codeinfos::Vector{Any}, workqueue::CompilationQueue;
    invokelatest_queue::Union{CompilationQueue,Nothing} = nothing,
)
    interp = workqueue.interp
    world = get_inference_world(interp)
    while !isempty(workqueue)
        item = pop!(workqueue)
        # each item in this list is either a MethodInstance indicating something
        # to compile, or an svec(rettype, sig) describing a C-callable alias to create.
        if item isa MethodInstance
            isinspected(workqueue, item) && continue
            # if this method is generally visible to the current compilation world,
            # and this is either the primary world, or not applicable in the primary world
            # then we want to compile and emit this
            if item.def.primary_world <= world
                ci = typeinf_ext(interp, item, SOURCE_MODE_GET_SOURCE)
                ci isa CodeInstance && push!(workqueue, ci)
            end
            markinspected!(workqueue, item)
        elseif item isa SimpleVector
            invokelatest_queue === nothing && continue
            (rt::Type, sig::Type) = item
            # make a best-effort attempt to enqueue the relevant code for the ccallable
            mi = ccall(:jl_get_specialization1, Any,
                        (Any, Csize_t, Cint),
                        sig, world, #= mt_cache =# 0)
            if mi !== nothing
                mi = mi::MethodInstance
                ci = typeinf_ext(interp, mi, SOURCE_MODE_GET_SOURCE)
                ci isa CodeInstance && push!(invokelatest_queue, ci)
            end
            # additionally enqueue the ccallable entrypoint / adapter, which implicitly
            # invokes the above ci
            push!(codeinfos, item)
        elseif item isa CodeInstance
            callee = item
            isinspected(workqueue, callee) && continue
            mi = get_ci_mi(callee)
            # now make sure everything has source code, if desired
            if use_const_api(callee)
                src = codeinfo_for_const(interp, mi, WorldRange(callee.min_world, callee.max_world), callee.edges, callee.rettype_const)
            else
                src = get(interp.codegen, callee, nothing)
                if src === nothing
                    newcallee = typeinf_ext(interp, mi, SOURCE_MODE_GET_SOURCE)
                    if newcallee isa CodeInstance
                        @assert use_const_api(newcallee) || haskey(interp.codegen, newcallee)
                        push!(workqueue, newcallee)
                    end
                    if newcallee !== callee
                        markinspected!(workqueue, callee)
                    end
                    continue
                end
            end
            markinspected!(workqueue, callee)
            if src isa CodeInfo
                sptypes = sptypes_from_meth_instance(mi)
                collectinvokes!(workqueue, src, sptypes; invokelatest_queue)
                # try to reuse an existing CodeInstance from before to avoid making duplicates in the cache
                if iszero(ccall(:jl_mi_cache_has_ci, Cint, (Any, Any), mi, callee))
                    cached = ccall(:jl_get_ci_equiv, Any, (Any, UInt), callee, world)::CodeInstance
                    if cached === callee
                        code_cache(interp)[mi] = callee
                    else
                        # Use an existing CI from the cache, if there is available one that is compatible
                        callee = cached
                    end
                end
                push!(codeinfos, callee)
                push!(codeinfos, src)
            end
        else @assert false "unexpected item in queue" end
    end
    return codeinfos
end

# This is a bridge for the C code calling `jl_typeinf_func()` on set of Method matches
# The trim_mode can be any of:
const TRIM_NO = 0x0
const TRIM_SAFE = 0x1
const TRIM_UNSAFE = 0x2
const TRIM_UNSAFE_WARN = 0x3
function typeinf_ext_toplevel(methods::Vector{Any}, worlds::Vector{UInt}, trim_mode::UInt8)
    inf_params = InferenceParams(; force_enable_inference = trim_mode != TRIM_NO)

    # Create an "invokelatest" queue to enable eager compilation of speculative
    # invokelatest calls such as from `Core.finalizer` and `ccallable`
    invokelatest_queue = CompilationQueue(;
        interp = NativeInterpreter(get_world_counter(); inf_params)
    )

    codeinfos = []
    workqueue = CompilationQueue(; interp = nothing)
    for this_world in reverse!(sort!(worlds))
        workqueue = CompilationQueue(workqueue;
            interp = NativeInterpreter(this_world; inf_params)
        )

        append!(workqueue, methods)
        compile!(codeinfos, workqueue; invokelatest_queue)
    end

    if invokelatest_queue !== nothing
        # This queue is intentionally aliased, to handle e.g. a `finalizer` calling `Core.finalizer`
        # (it will enqueue into itself and immediately drain)
        compile!(codeinfos, invokelatest_queue; invokelatest_queue)
    end

    if trim_mode != TRIM_NO && trim_mode != TRIM_UNSAFE
        verify_typeinf_trim(codeinfos, trim_mode == TRIM_UNSAFE_WARN)
    end
    return codeinfos
end

const _verify_trim_world_age = RefValue{UInt}(typemax(UInt))
verify_typeinf_trim(codeinfos::Vector{Any}, onlywarn::Bool) = Core._call_in_world(_verify_trim_world_age[], verify_typeinf_trim, stdout, codeinfos, onlywarn)

function return_type(@nospecialize(f), t::DataType) # this method has a special tfunc
    world = tls_world_age()
    args = Any[_return_type, NativeInterpreter(world), Tuple{Core.Typeof(f), t.parameters...}]
    return ccall(:jl_call_in_typeinf_world, Any, (Ptr{Any}, Cint), args, length(args))
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
    return ccall(:jl_call_in_typeinf_world, Any, (Ptr{Any}, Cint), args, length(args))
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
