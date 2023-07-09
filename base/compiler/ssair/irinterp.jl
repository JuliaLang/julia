# This file is a part of Julia. License is MIT: https://julialang.org/license

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
            return ir_abstract_constant_propagation(interp, newirsv)
        end
        return Pair{Any,Bool}(nothing, is_nothrow(effects))
    end
end

abstract_eval_ssavalue(s::SSAValue, sv::IRInterpretationState) = abstract_eval_ssavalue(s, sv.ir)

function abstract_eval_phi_stmt(interp::AbstractInterpreter, phi::PhiNode, ::Int, irsv::IRInterpretationState)
    return abstract_eval_phi(interp, phi, nothing, irsv)
end

function abstract_call(interp::AbstractInterpreter, arginfo::ArgInfo, irsv::IRInterpretationState)
    si = StmtInfo(true) # TODO better job here?
    (; rt, effects, info) = abstract_call(interp, arginfo, si, irsv)
    irsv.ir.stmts[irsv.curridx][:info] = info
    return RTEffects(rt, effects)
end

function update_phi!(irsv::IRInterpretationState, from::Int, to::Int)
    ir = irsv.ir
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
update_phi!(irsv::IRInterpretationState) = (from::Int, to::Int)->update_phi!(irsv, from, to)

function kill_terminator_edges!(irsv::IRInterpretationState, term_idx::Int, bb::Int=block_for_inst(irsv.ir, term_idx))
    ir = irsv.ir
    inst = ir[SSAValue(term_idx)][:inst]
    if isa(inst, GotoIfNot)
        kill_edge!(ir, bb, inst.dest, update_phi!(irsv))
        kill_edge!(ir, bb, bb+1, update_phi!(irsv))
    elseif isa(inst, GotoNode)
        kill_edge!(ir, bb, inst.label, update_phi!(irsv))
    elseif isa(inst, ReturnNode)
        # Nothing to do
    else
        @assert !isexpr(inst, :enter)
        kill_edge!(ir, bb, bb+1, update_phi!(irsv))
    end
end

function reprocess_instruction!(interp::AbstractInterpreter, idx::Int, bb::Union{Int,Nothing},
    @nospecialize(inst), @nospecialize(typ), irsv::IRInterpretationState)
    ir = irsv.ir
    if isa(inst, GotoIfNot)
        cond = inst.cond
        condval = maybe_extract_const_bool(argextype(cond, ir))
        if condval isa Bool
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
                kill_edge!(ir, bb, inst.dest, update_phi!(irsv))
            else
                ir.stmts[idx][:inst] = GotoNode(inst.dest)
                kill_edge!(ir, bb, bb+1, update_phi!(irsv))
            end
            return true
        end
        return false
    end
    rt = nothing
    if isa(inst, Expr)
        head = inst.head
        if head === :call || head === :foreigncall || head === :new || head === :splatnew || head === :static_parameter || head === :isdefined
            (; rt, effects) = abstract_eval_statement_expr(interp, inst, nothing, irsv)
            ir.stmts[idx][:flag] |= flags_for_effects(effects)
        elseif head === :invoke
            rt, nothrow = concrete_eval_invoke(interp, inst, inst.args[1]::MethodInstance, irsv)
            if nothrow
                ir.stmts[idx][:flag] |= IR_FLAG_NOTHROW
            end
        elseif head === :throw_undef_if_not
            condval = maybe_extract_const_bool(argextype(inst.args[2], ir))
            condval isa Bool || return false
            if condval
                ir.stmts[idx][:inst] = nothing
                # We simplified the IR, but we did not update the type
                return false
            end
            rt = Union{}
        elseif head === :gc_preserve_begin ||
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
        rt = tmeet(typeinf_lattice(interp), argextype(inst.val, ir), widenconst(inst.typ))
    elseif inst === nothing
        return false
    elseif isa(inst, GlobalRef)
        # GlobalRef is not refinable
    else
        rt = argextype(inst, irsv.ir)
    end
    if rt !== nothing
        if isa(rt, Const)
            ir.stmts[idx][:type] = rt
            if is_inlineable_constant(rt.val) && (ir.stmts[idx][:flag] & (IR_FLAG_EFFECT_FREE | IR_FLAG_NOTHROW)) == IR_FLAG_EFFECT_FREE | IR_FLAG_NOTHROW
                ir.stmts[idx][:inst] = quoted(rt.val)
            end
            return true
        elseif !⊑(typeinf_lattice(interp), typ, rt)
            ir.stmts[idx][:type] = rt
            return true
        end
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

function _ir_abstract_constant_propagation(interp::AbstractInterpreter, irsv::IRInterpretationState;
        externally_refined::Union{Nothing,BitSet} = nothing)
    interp = switch_to_irinterp(interp)

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
            flag = ir.stmts[idx][:flag]
            any_refined = false
            if (flag & IR_FLAG_REFINED) != 0
                any_refined = true
                ir.stmts[idx][:flag] &= ~IR_FLAG_REFINED
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
            is_terminator_or_phi = isa(inst, PhiNode) || isa(inst, GotoNode) || isa(inst, GotoIfNot) || isa(inst, ReturnNode) || isexpr(inst, :enter)
            if typ === Bottom && (idx != lstmt || !is_terminator_or_phi)
                continue
            end
            if (any_refined && reprocess_instruction!(interp,
                    idx, bb, inst, typ, irsv)) ||
               (externally_refined !== nothing && idx in externally_refined)
                push!(ssa_refined, idx)
                inst = ir.stmts[idx][:inst]
                typ = ir.stmts[idx][:type]
            end
            if typ === Bottom && !is_terminator_or_phi
                kill_terminator_edges!(irsv, lstmt, bb)
                if idx != lstmt
                    for idx2 in (idx+1:lstmt-1)
                        ir[SSAValue(idx2)] = nothing
                    end
                    ir[SSAValue(lstmt)][:inst] = ReturnNode()
                end
                break
            end
            if idx == lstmt
                process_terminator!(ir, inst, idx, bb, all_rets, bb_ip) && @goto residual_scan
            end
        end
    end
    @goto compute_rt

    # Slow path
    begin @label residual_scan
        stmt_ip = BitSetBoundedMinPrioritySet(length(ir.stmts))

        # Slow Path Phase 1.A: Complete use scanning
        while !isempty(bb_ip)
            bb = popfirst!(bb_ip)
            stmts = bbs[bb].stmts
            lstmt = last(stmts)
            for idx = stmts
                irsv.curridx = idx
                inst = ir.stmts[idx][:inst]
                flag = ir.stmts[idx][:flag]
                if (flag & IR_FLAG_REFINED) != 0
                    ir.stmts[idx][:flag] &= ~IR_FLAG_REFINED
                    push!(stmt_ip, idx)
                end
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
                idx, nothing, inst, typ, irsv)
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
            ultimate_rt = tmerge(typeinf_lattice(interp), ultimate_rt, rt)
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

function ir_abstract_constant_propagation(interp::NativeInterpreter, irsv::IRInterpretationState)
    if __measure_typeinf__[]
        inf_frame = Timings.InferenceFrameInfo(irsv.mi, irsv.world, VarState[], Any[], length(irsv.ir.argtypes))
        Timings.enter_new_timer(inf_frame)
        ret = _ir_abstract_constant_propagation(interp, irsv)
        append!(inf_frame.slottypes, irsv.ir.argtypes)
        Timings.exit_current_timer(inf_frame)
        return ret
    else
        return _ir_abstract_constant_propagation(interp, irsv)
    end
end
ir_abstract_constant_propagation(interp::AbstractInterpreter, irsv::IRInterpretationState) =
    _ir_abstract_constant_propagation(interp, irsv)
