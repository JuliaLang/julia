# This file is a part of Julia. License is MIT: https://julialang.org/license

const COMPILER_TEMP_SYM = Symbol("#temp#")

# build (and start inferring) the inference frame for the linfo
function typeinf(result::InferenceResult, cached::Bool, params::Params)
    frame = InferenceState(result, cached, params)
    frame === nothing && return false
    cached && (result.linfo.inInference = true)
    return typeinf(frame)
end

function typeinf(frame::InferenceState)
    cached = frame.cached
    typeinf_nocycle(frame) || return false # frame is now part of a higher cycle
    # with no active ip's, frame is done
    frames = frame.callers_in_cycle
    isempty(frames) && push!(frames, frame)
    for caller in frames
        @assert !(caller.dont_work_on_me)
        caller.dont_work_on_me = true
    end
    for caller in frames
        finish(caller)
    end
    # collect results for the new expanded frame
    results = InferenceResult[ frames[i].result for i in 1:length(frames) ]
    # empty!(frames)
    min_valid = frame.min_valid
    max_valid = frame.max_valid
    if cached || frame.parent !== nothing
        for caller in results
            opt = caller.src
            if opt isa OptimizationState
                optimize(opt, caller.result)
                finish(opt.src)
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
        if cached
            for caller in results
                cache_result(caller, min_valid, max_valid)
            end
        end
    end
    # if we aren't cached, we don't need this edge
    # but our caller might, so let's just make it anyways
    for caller in frames
        finalize_backedges(caller)
    end
    if max_valid == typemax(UInt)
        for caller in frames
            store_backedges(caller)
        end
    end
    return true
end

# inference completed on `me`
# update the MethodInstance and notify the edges
function cache_result(result::InferenceResult, min_valid::UInt, max_valid::UInt)
    def = result.linfo.def
    toplevel = !isa(result.linfo.def, Method)
    if toplevel
        min_valid = UInt(0)
        max_valid = UInt(0)
    end

    # check if the existing linfo metadata is also sufficient to describe the current inference result
    # to decide if it is worth caching it again (which would also clear any generated code)
    already_inferred = !result.linfo.inInference
    if isdefined(result.linfo, :inferred)
        inf = result.linfo.inferred
        if !isa(inf, CodeInfo) || (inf::CodeInfo).inferred
            if min_world(result.linfo) == min_valid && max_world(result.linfo) == max_valid
                already_inferred = true
            end
        end
    end

    # don't store inferred code if we've decided to interpret this function
    if !already_inferred && invoke_api(result.linfo) != 4
        inferred_result = result.src
        if inferred_result isa Const
            # use constant calling convention
            inferred_const = (result.src::Const).val
            const_flags = 0x3
        else
            if isa(result.result, Const)
                inferred_const = (result.result::Const).val
                const_flags = 0x2
            elseif isconstType(result.result)
                inferred_const = result.result.parameters[1]
                const_flags = 0x2
            else
                inferred_const = nothing
                const_flags = 0x00
            end
            if !toplevel && inferred_result isa CodeInfo
                cache_the_tree = result.src.inferred &&
                    (result.src.inlineable ||
                     ccall(:jl_isa_compileable_sig, Int32, (Any, Any), result.linfo.specTypes, def) != 0)
                if cache_the_tree
                    # compress code for non-toplevel thunks
                    inferred_result = ccall(:jl_compress_ast, Any, (Any, Any), def, inferred_result)
                else
                    inferred_result = nothing
                end
            end
        end
        if !isa(inferred_result, Union{CodeInfo, Vector{UInt8}})
            inferred_result = nothing
        end
        cache = ccall(:jl_set_method_inferred, Ref{MethodInstance}, (Any, Any, Any, Any, Int32, UInt, UInt),
            result.linfo, widenconst(result.result), inferred_const, inferred_result,
            const_flags, min_valid, max_valid)
        if cache !== result.linfo
            result.linfo.inInference = false
            result.linfo = cache
        end
    end
    result.linfo.inInference = false
    nothing
end

function finish(me::InferenceState)
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
            opt = OptimizationState(me)
            me.result.src = opt
        end
    end
    me.result.result = me.bestguess
    nothing
end

function finish(src::CodeInfo)
    # convert all type information into the form consumed by the cache for inlining and code-generation
    widen_all_consts!(src)
    src.inferred = true
    nothing
end

function finalize_backedges(me::InferenceState)
    # update all of the (cycle) callers with real backedges
    # by traversing the temporary list of backedges
    for (i, _) in me.cycle_backedges
        add_backedge!(me.linfo, i)
    end

    # finalize and record the linfo result
    me.inferred = true
    nothing
end

# add the real backedges
function store_backedges(frame::InferenceState)
    toplevel = !isa(frame.linfo.def, Method)
    if !toplevel && (frame.cached || frame.parent !== nothing)
        caller = frame.result.linfo
        for edges in frame.stmt_edges
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

    return src
end

maybe_widen_conditional(@nospecialize vt) = vt
function maybe_widen_conditional(vt::Conditional)
    if vt.vtype === Bottom
        return Const(false)
    elseif vt.elsetype === Bottom
        return Const(true)
    else
        return Bool
    end
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
    vt = maybe_widen_conditional(s.typ)
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
        gt[j] = maybe_widen_conditional(gt[j])
    end

    # compute the required type for each slot
    # to hold all of the items assigned into it
    record_slot_assign!(sv)

    # annotate variables load types
    # remove dead code optimization
    # and compute which variables may be used undef
    src = sv.src
    states = sv.stmt_types
    nargs = sv.nargs
    nslots = length(states[1])
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
            # st_i === ()  =>  unreached statement  (see issue #7836)
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

function merge_call_chain!(parent::InferenceState, ancestor::InferenceState, child::InferenceState)
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
                return AbstractEvalConstant(code.inferred_const), code
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
        result = InferenceResult(code)
        frame = InferenceState(result, #=cached=#true, caller.params) # always use the cache for edge targets
        if frame === nothing
            # can't get the source for this, so we know nothing
            code.inInference = false
            return Any, nothing
        end
        if caller.cached || caller.limited # don't involve uncached functions in cycle resolution
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
function typeinf_code(method::Method, @nospecialize(argtypes), sparams::SimpleVector,
                      argvals::Union{Nothing, Vector{Any}},
                      run_optimizer::Bool, params::Params)
    code = code_for_method(method, argtypes, sparams, params.world)
    code === nothing && return (nothing, Any)
    ccall(:jl_typeinf_begin, Cvoid, ())
    result = InferenceResult(code)
    if argvals !== nothing
        nargs::Int = method.nargs
        method.isva && (nargs -= 1)
        atypes = get_argtypes(result)
        for i in 1:nargs
            isassigned(argvals, i) || continue
            atypes[i] = Const(argvals[i])
        end
        if method.isva
            vargs = argtypes.parameters[(nargs + 1):end]
            for i in 1:length(vargs)
                a = maybe_widen_conditional(vargs[i])
                if isassigned(argvals, nargs + i)
                    a = Const(argvals[nargs + i])
                end
                if i > length(result.vargs)
                    push!(result.vargs, a)
                elseif a isa Const
                    result.vargs[i] = a
                end
            end
        end
    end
    frame = InferenceState(result, false, params)
    frame === nothing && return (nothing, Any)
    if typeinf(frame) && run_optimizer
        opt = OptimizationState(frame)
        optimize(opt, result.result)
        opt.src.inferred = true
    end
    ccall(:jl_typeinf_end, Cvoid, ())
    frame.inferred || return (nothing, Any)
    return (frame.src, widenconst(result.result))
end

# compute (and cache) an inferred AST and return type
function typeinf_ext(linfo::MethodInstance, params::Params)
    for i = 1:2 # test-and-lock-and-test
        i == 2 && ccall(:jl_typeinf_begin, Cvoid, ())
        if isdefined(linfo, :inferred)
            # see if this code already exists in the cache
            # staged functions make this hard since they have two "inferred" conditions,
            # so need to check whether the code itself is also inferred
            if min_world(linfo) <= params.world <= max_world(linfo)
                inf = linfo.inferred
                if invoke_api(linfo) == 2
                    method = linfo.def::Method
                    tree = ccall(:jl_new_code_info_uninit, Ref{CodeInfo}, ())
                    tree.code = Any[ Expr(:return, quoted(linfo.inferred_const)) ]
                    tree.method_for_inference_limit_heuristics = nothing
                    tree.slotnames = Any[ COMPILER_TEMP_SYM for i = 1:method.nargs ]
                    tree.slotflags = fill(0x00, Int(method.nargs))
                    tree.ssavaluetypes = 0
                    tree.codelocs = Int32[1]
                    tree.linetable = [LineInfoNode(method.module, method.name, method.file, Int(method.line), 0)]
                    tree.inferred = true
                    tree.ssaflags = UInt8[]
                    tree.pure = true
                    tree.inlineable = true
                    i == 2 && ccall(:jl_typeinf_end, Cvoid, ())
                    return svec(linfo, tree)
                elseif isa(inf, CodeInfo)
                    if inf.inferred
                        i == 2 && ccall(:jl_typeinf_end, Cvoid, ())
                        return svec(linfo, inf)
                    end
                elseif isa(inf, Vector{UInt8})
                    inf = uncompressed_ast(linfo.def::Method, inf)
                    if inf.inferred
                        i == 2 && ccall(:jl_typeinf_end, Cvoid, ())
                        return svec(linfo, inf)
                    end
                end
            end
        end
    end
    linfo.inInference = true
    frame = InferenceState(InferenceResult(linfo), #=cached=#true, params)
    frame === nothing && return svec(nothing, nothing)
    typeinf(frame)
    ccall(:jl_typeinf_end, Cvoid, ())
    frame.src.inferred || return svec(nothing, nothing)
    return svec(frame.result.linfo, frame.src)
end

# compute (and cache) an inferred AST and return the inferred return type
function typeinf_type(method::Method, @nospecialize(atypes), sparams::SimpleVector, params::Params)
    if contains_is(unwrap_unionall(atypes).parameters, Union{})
        return Union{}
    end
    code = code_for_method(method, atypes, sparams, params.world)
    code === nothing && return nothing
    code = code::MethodInstance
    for i = 1:2 # test-and-lock-and-test
        i == 2 && ccall(:jl_typeinf_begin, Cvoid, ())
        if isdefined(code, :inferred)
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
    frame = InferenceResult(code)
    typeinf(frame, true, params)
    ccall(:jl_typeinf_end, Cvoid, ())
    frame.result isa InferenceState && return nothing
    return widenconst(frame.result)
end

@timeit function typeinf_ext(linfo::MethodInstance, world::UInt)
    if isa(linfo.def, Method)
        # method lambda - infer this specialization via the method cache
        return typeinf_ext(linfo, Params(world))
    else
        # toplevel lambda - infer directly
        ccall(:jl_typeinf_begin, Cvoid, ())
        result = InferenceResult(linfo)
        frame = InferenceState(result, linfo.inferred::CodeInfo,
                               #=cached=#true, Params(world))
        typeinf(frame)
        ccall(:jl_typeinf_end, Cvoid, ())
        @assert frame.inferred # TODO: deal with this better
        @assert frame.linfo === linfo
        linfo.rettype = widenconst(frame.bestguess)
        return svec(linfo, frame.src)
    end
end


function return_type(@nospecialize(f), @nospecialize(t))
    params = Params(ccall(:jl_get_tls_world_age, UInt, ()))
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
            ty = typeinf_type(m[3], m[1], m[2], params)
            ty === nothing && return Any
            rt = tmerge(rt, ty)
            rt === Any && break
        end
    end
    return rt
end
