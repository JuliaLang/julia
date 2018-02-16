# This file is a part of Julia. License is MIT: https://julialang.org/license

const COMPILER_TEMP_SYM = Symbol("#temp#")

# add the real backedges
function finalize_backedges(frame::InferenceState)
    toplevel = !isa(frame.linfo.def, Method)
    if !toplevel && (frame.cached || frame.parent !== nothing) && frame.max_valid == typemax(UInt)
        caller = frame.linfo
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
                       optimize::Bool, cached::Bool, params::Params)
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
                      optimize::Bool, cached::Bool, params::Params)
    code = code_for_method(method, atypes, sparams, params.world)
    code === nothing && return (nothing, nothing, Any)
    return typeinf_code(code::MethodInstance, optimize, cached, params)
end
function typeinf_code(linfo::MethodInstance, optimize::Bool, cached::Bool,
                      params::Params)
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
                    tree.slotnames = Any[ COMPILER_TEMP_SYM for i = 1:method.nargs ]
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
                      cached::Bool, params::Params)
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
        return typeinf_code(linfo, true, true, Params(world))
    else
        # toplevel lambda - infer directly
        ccall(:jl_typeinf_begin, Cvoid, ())
        result = InferenceResult(linfo)
        frame = InferenceState(result, linfo.inferred::CodeInfo,
                               true, true, Params(world))
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
                    condval = maybe_extract_const_bool(condt)
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
