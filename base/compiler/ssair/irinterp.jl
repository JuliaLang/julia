# This file is a part of Julia. License is MIT: https://julialang.org/license

# TODO (#48913) remove this overload to enable interprocedural call inference from irinterp
function abstract_call_gf_by_type(interp::AbstractInterpreter, @nospecialize(f),
    arginfo::ArgInfo, si::StmtInfo, @nospecialize(atype),
    sv::IRInterpretationState, max_methods::Int)
    return CallMeta(Any, Effects(), NoCallInfo())
end

function collect_limitations!(@nospecialize(typ), ::IRInterpretationState)
    @assert !isa(typ, LimitedAccuracy) "irinterp is unable to handle heavy recursion"
    return typ
end

function concrete_eval_invoke(interp::AbstractInterpreter,
    inst::Expr, mi::MethodInstance, irsv::IRInterpretationState)
    world = frame_world(irsv)
    mi_cache = WorldView(code_cache(interp), world)
    code = get(mi_cache, mi, nothing)
    code === nothing && return Pair{Any,Bool}(nothing, false)
    argtypes = collect_argtypes(interp, inst.args[2:end], nothing, irsv)
    argtypes === nothing && return Pair{Any,Bool}(Bottom, false)
    effects = decode_effects(code.ipo_purity_bits)
    if is_foldable(effects) && is_all_const_arg(argtypes, #=start=#1)
        args = collect_const_args(argtypes, #=start=#1)
        value = let world = get_world_counter(interp)
            try
                Core._call_in_world_total(world, args...)
            catch
                return Pair{Any,Bool}(Bottom, false)
            end
        end
        return Pair{Any,Bool}(Const(value), true)
    else
        if is_constprop_edge_recursed(mi, irsv)
            return Pair{Any,Bool}(nothing, is_nothrow(effects))
        end
        newirsv = IRInterpretationState(interp, code, mi, argtypes, world)
        if newirsv !== nothing
            newirsv.parent = irsv
            return _ir_abstract_constant_propagation(interp, newirsv)
        end
        return Pair{Any,Bool}(nothing, is_nothrow(effects))
    end
end

abstract_eval_ssavalue(s::SSAValue, sv::IRInterpretationState) = abstract_eval_ssavalue(s, sv.ir)

function abstract_eval_phi_stmt(interp::AbstractInterpreter, phi::PhiNode, ::Int, irsv::IRInterpretationState)
    return abstract_eval_phi(interp, phi, nothing, irsv)
end

function propagate_control_effects!(interp::AbstractInterpreter, idx::Int, stmt::GotoIfNot,
        irsv::IRInterpretationState, extra_reprocess::Union{Nothing,BitSet,BitSetBoundedMinPrioritySet})
    # Nothing to do for most abstract interpreters, but if the abstract
    # interpreter has control-dependent lattice effects, it can override
    # this method.
    return false
end

function abstract_call(interp::AbstractInterpreter, arginfo::ArgInfo, irsv::IRInterpretationState)
    si = StmtInfo(true) # TODO better job here?
    (; rt, effects, info) = abstract_call(interp, arginfo, si, irsv)
    irsv.ir.stmts[irsv.curridx][:info] = info
    return RTEffects(rt, effects)
end

function reprocess_instruction!(interp::AbstractInterpreter, idx::Int, bb::Union{Int,Nothing},
    @nospecialize(inst), @nospecialize(typ), irsv::IRInterpretationState,
    extra_reprocess::Union{Nothing,BitSet,BitSetBoundedMinPrioritySet})
    ir = irsv.ir
    if isa(inst, GotoIfNot)
        cond = inst.cond
        condval = maybe_extract_const_bool(argextype(cond, ir))
        if condval isa Bool
            function update_phi!(from::Int, to::Int)
                if length(ir.cfg.blocks[to].preds) == 0
                    # Kill the entire block
                    for bidx = ir.cfg.blocks[to].stmts
                        ir.stmts[bidx][:inst] = nothing
                        ir.stmts[bidx][:type] = Bottom
                        ir.stmts[bidx][:flag] = IR_FLAG_EFFECT_FREE | IR_FLAG_NOTHROW
                    end
                    return
                end
                for sidx = ir.cfg.blocks[to].stmts
                    sinst = ir.stmts[sidx][:inst]
                    isa(sinst, Nothing) && continue # allowed between `PhiNode`s
                    isa(sinst, PhiNode) || break
                    for (eidx, edge) in enumerate(sinst.edges)
                        if edge == from
                            deleteat!(sinst.edges, eidx)
                            deleteat!(sinst.values, eidx)
                            push!(irsv.ssa_refined, sidx)
                            break
                        end
                    end
                end
            end
            if isa(cond, SSAValue)
                kill_def_use!(irsv.tpdum, cond, idx)
            end
            if bb === nothing
                bb = block_for_inst(ir, idx)
            end
            ir.stmts[idx][:flag] |= IR_FLAG_NOTHROW
            if condval
                ir.stmts[idx][:inst] = nothing
                ir.stmts[idx][:type] = Any
                kill_edge!(ir, bb, inst.dest, update_phi!)
            else
                ir.stmts[idx][:inst] = GotoNode(inst.dest)
                kill_edge!(ir, bb, bb+1, update_phi!)
            end
            return true
        end
        return propagate_control_effects!(interp, idx, inst, irsv, extra_reprocess)
    end

    rt = nothing
    if isa(inst, Expr)
        head = inst.head
        if head === :call || head === :foreigncall || head === :new || head === :splatnew
            (; rt, effects) = abstract_eval_statement_expr(interp, inst, nothing, irsv)
            ir.stmts[idx][:flag] |= flags_for_effects(effects)
            if is_foldable(effects) && isa(rt, Const) && is_inlineable_constant(rt.val)
                ir.stmts[idx][:inst] = quoted(rt.val)
            end
        elseif head === :invoke
            rt, nothrow = concrete_eval_invoke(interp, inst, inst.args[1]::MethodInstance, irsv)
            if nothrow
                ir.stmts[idx][:flag] |= IR_FLAG_NOTHROW
                if isa(rt, Const) && is_inlineable_constant(rt.val)
                    ir.stmts[idx][:inst] = quoted(rt.val)
                end
            end
        elseif head === :throw_undef_if_not || # TODO: Terminate interpretation early if known false?
               head === :gc_preserve_begin ||
               head === :gc_preserve_end
            return false
        else
            error("reprocess_instruction!: unhandled expression found")
        end
    elseif isa(inst, PhiNode)
        rt = abstract_eval_phi_stmt(interp, inst, idx, irsv)
    elseif isa(inst, ReturnNode)
        # Handled at the very end
        return false
    elseif isa(inst, PiNode)
        rt = tmeet(optimizer_lattice(interp), argextype(inst.val, ir), widenconst(inst.typ))
    elseif inst === nothing
        return false
    elseif isa(inst, GlobalRef)
        # GlobalRef is not refinable
    else
        error("reprocess_instruction!: unhandled instruction found")
    end
    if rt !== nothing && !⊑(optimizer_lattice(interp), typ, rt)
        ir.stmts[idx][:type] = rt
        return true
    end
    return false
end

# Process the terminator and add the successor to `bb_ip`. Returns whether a backedge was seen.
function process_terminator!(ir::IRCode, @nospecialize(inst), idx::Int, bb::Int,
    all_rets::Vector{Int}, bb_ip::BitSetBoundedMinPrioritySet)
    if isa(inst, ReturnNode)
        if isdefined(inst, :val)
            push!(all_rets, idx)
        end
        return false
    elseif isa(inst, GotoNode)
        backedge = inst.label <= bb
        backedge || push!(bb_ip, inst.label)
        return backedge
    elseif isa(inst, GotoIfNot)
        backedge = inst.dest <= bb
        backedge || push!(bb_ip, inst.dest)
        push!(bb_ip, bb+1)
        return backedge
    elseif isexpr(inst, :enter)
        dest = inst.args[1]::Int
        @assert dest > bb
        push!(bb_ip, dest)
        push!(bb_ip, bb+1)
        return false
    else
        push!(bb_ip, bb+1)
        return false
    end
end

default_reprocess(::AbstractInterpreter, ::IRInterpretationState) = nothing
function _ir_abstract_constant_propagation(interp::AbstractInterpreter, irsv::IRInterpretationState;
    extra_reprocess::Union{Nothing,BitSet} = default_reprocess(interp, irsv))
    (; ir, tpdum, ssa_refined) = irsv

    bbs = ir.cfg.blocks
    bb_ip = BitSetBoundedMinPrioritySet(length(bbs))
    push!(bb_ip, 1)
    all_rets = Int[]

    # Fast path: Scan both use counts and refinement in one single pass of
    #            of the instructions. In the absence of backedges, this will
    #            converge.
    while !isempty(bb_ip)
        bb = popfirst!(bb_ip)
        stmts = bbs[bb].stmts
        lstmt = last(stmts)
        for idx = stmts
            irsv.curridx = idx
            inst = ir.stmts[idx][:inst]
            typ = ir.stmts[idx][:type]
            any_refined = false
            if extra_reprocess !== nothing
                if idx in extra_reprocess
                    pop!(extra_reprocess, idx)
                    any_refined = true
                end
            end
            for ur in userefs(inst)
                val = ur[]
                if isa(val, Argument)
                    any_refined |= irsv.argtypes_refined[val.n]
                elseif isa(val, SSAValue)
                    any_refined |= val.id in ssa_refined
                    count!(tpdum, val)
                end
            end
            if isa(inst, PhiNode) && idx in ssa_refined
                any_refined = true
                delete!(ssa_refined, idx)
            end
            if any_refined && reprocess_instruction!(interp,
                idx, bb, inst, typ, irsv, extra_reprocess)
                push!(ssa_refined, idx)
            end
            idx == lstmt && process_terminator!(ir, inst, idx, bb, all_rets, bb_ip) && @goto residual_scan
            if typ === Bottom && !isa(inst, PhiNode)
                break
            end
        end
    end
    @goto compute_rt

    # Slow path
    begin @label residual_scan
        stmt_ip = BitSetBoundedMinPrioritySet(length(ir.stmts))
        if extra_reprocess !== nothing
            append!(stmt_ip, extra_reprocess)
        end

        # Slow Path Phase 1.A: Complete use scanning
        while !isempty(bb_ip)
            bb = popfirst!(bb_ip)
            stmts = bbs[bb].stmts
            lstmt = last(stmts)
            for idx = stmts
                irsv.curridx = idx
                inst = ir.stmts[idx][:inst]
                for ur in userefs(inst)
                    val = ur[]
                    if isa(val, Argument)
                        if irsv.argtypes_refined[val.n]
                            push!(stmt_ip, idx)
                        end
                    elseif isa(val, SSAValue)
                        count!(tpdum, val)
                    end
                end
                idx == lstmt && process_terminator!(ir, inst, idx, bb, all_rets, bb_ip)
            end
        end

        # Slow Path Phase 1.B: Assemble def-use map
        complete!(tpdum)
        push!(bb_ip, 1)
        while !isempty(bb_ip)
            bb = popfirst!(bb_ip)
            stmts = bbs[bb].stmts
            lstmt = last(stmts)
            for idx = stmts
                irsv.curridx = idx
                inst = ir.stmts[idx][:inst]
                for ur in userefs(inst)
                    val = ur[]
                    if isa(val, SSAValue)
                        push!(tpdum[val.id], idx)
                    end
                end
                idx == lstmt && process_terminator!(ir, inst, idx, bb, all_rets, bb_ip)
            end
        end

        # Slow Path Phase 2: Use def-use map to converge cycles.
        # TODO: It would be possible to return to the fast path after converging
        #       each cycle, but that's somewhat complicated.
        for val in ssa_refined
            append!(stmt_ip, tpdum[val])
        end
        while !isempty(stmt_ip)
            idx = popfirst!(stmt_ip)
            irsv.curridx = idx
            inst = ir.stmts[idx][:inst]
            typ = ir.stmts[idx][:type]
            if reprocess_instruction!(interp,
                idx, nothing, inst, typ, irsv, stmt_ip)
                append!(stmt_ip, tpdum[idx])
            end
        end
    end

    begin @label compute_rt
        ultimate_rt = Bottom
        for idx in all_rets
            bb = block_for_inst(ir.cfg, idx)
            if bb != 1 && length(ir.cfg.blocks[bb].preds) == 0
                # Could have discovered this block is dead after the initial scan
                continue
            end
            inst = ir.stmts[idx][:inst]::ReturnNode
            rt = argextype(inst.val, ir)
            ultimate_rt = tmerge(optimizer_lattice(interp), ultimate_rt, rt)
        end
    end

    nothrow = true
    for idx = 1:length(ir.stmts)
        if (ir.stmts[idx][:flag] & IR_FLAG_NOTHROW) == 0
            nothrow = false
            break
        end
    end

    if last(irsv.valid_worlds) >= get_world_counter()
        # if we aren't cached, we don't need this edge
        # but our caller might, so let's just make it anyways
        store_backedges(frame_instance(irsv), irsv.edges)
    end

    return Pair{Any,Bool}(maybe_singleton_const(ultimate_rt), nothrow)
end

function ir_abstract_constant_propagation(interp::AbstractInterpreter, irsv::IRInterpretationState)
    irinterp = switch_to_irinterp(interp)
    if __measure_typeinf__[]
        inf_frame = Timings.InferenceFrameInfo(irsv.mi, irsv.world, VarState[], Any[], length(irsv.ir.argtypes))
        Timings.enter_new_timer(inf_frame)
        ret = _ir_abstract_constant_propagation(irinterp, irsv)
        append!(inf_frame.slottypes, irsv.ir.argtypes)
        Timings.exit_current_timer(inf_frame)
        return ret
    else
        return _ir_abstract_constant_propagation(irinterp, irsv)
    end
end
