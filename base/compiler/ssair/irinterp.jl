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
    if (is_foldable(effects) && is_all_const_arg(argtypes, #=start=#1) &&
        is_nonoverlayed(effects) && is_nonoverlayed(mi.def::Method))
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
            inst = ir[SSAValue(bidx)]
            inst[:stmt] = nothing
            inst[:type] = Bottom
            inst[:flag] = IR_FLAG_EFFECT_FREE | IR_FLAG_NOTHROW
        end
        return
    end
    for sidx = ir.cfg.blocks[to].stmts
        stmt = ir[SSAValue(sidx)][:stmt]
        isa(stmt, Nothing) && continue # allowed between `PhiNode`s
        isa(stmt, PhiNode) || break
        for (eidx, edge) in enumerate(stmt.edges)
            if edge == from
                deleteat!(stmt.edges, eidx)
                deleteat!(stmt.values, eidx)
                push!(irsv.ssa_refined, sidx)
                break
            end
        end
    end
end
update_phi!(irsv::IRInterpretationState) = (from::Int, to::Int)->update_phi!(irsv, from, to)

function kill_terminator_edges!(irsv::IRInterpretationState, term_idx::Int, bb::Int=block_for_inst(irsv.ir, term_idx))
    ir = irsv.ir
    stmt = ir[SSAValue(term_idx)][:stmt]
    if isa(stmt, GotoIfNot)
        kill_edge!(ir, bb, stmt.dest, update_phi!(irsv))
        kill_edge!(ir, bb, bb+1, update_phi!(irsv))
    elseif isa(stmt, GotoNode)
        kill_edge!(ir, bb, stmt.label, update_phi!(irsv))
    elseif isa(stmt, ReturnNode)
        # Nothing to do
    else
        @assert !isexpr(stmt, :enter)
        kill_edge!(ir, bb, bb+1, update_phi!(irsv))
    end
end

function reprocess_instruction!(interp::AbstractInterpreter, idx::Int, bb::Union{Int,Nothing},
    @nospecialize(stmt), @nospecialize(typ), irsv::IRInterpretationState)
    ir = irsv.ir
    inst = ir[SSAValue(idx)]
    if isa(stmt, GotoIfNot)
        cond = stmt.cond
        condval = maybe_extract_const_bool(argextype(cond, ir))
        if condval isa Bool
            if isa(cond, SSAValue)
                kill_def_use!(irsv.tpdum, cond, idx)
            end
            if bb === nothing
                bb = block_for_inst(ir, idx)
            end
            inst[:flag] |= IR_FLAG_NOTHROW
            if condval
                inst[:stmt] = nothing
                inst[:type] = Any
                kill_edge!(ir, bb, stmt.dest, update_phi!(irsv))
            else
                inst[:stmt] = GotoNode(stmt.dest)
                kill_edge!(ir, bb, bb+1, update_phi!(irsv))
            end
            return true
        end
        return false
    end
    rt = nothing
    if isa(stmt, Expr)
        head = stmt.head
        if head === :call || head === :foreigncall || head === :new || head === :splatnew || head === :static_parameter || head === :isdefined
            (; rt, effects) = abstract_eval_statement_expr(interp, stmt, nothing, irsv)
            inst[:flag] |= flags_for_effects(effects)
        elseif head === :invoke
            rt, nothrow = concrete_eval_invoke(interp, stmt, stmt.args[1]::MethodInstance, irsv)
            if nothrow
                inst[:flag] |= IR_FLAG_NOTHROW
            end
        elseif head === :throw_undef_if_not
            condval = maybe_extract_const_bool(argextype(stmt.args[2], ir))
            condval isa Bool || return false
            if condval
                inst[:stmt] = nothing
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
    elseif isa(stmt, PhiNode)
        rt = abstract_eval_phi_stmt(interp, stmt, idx, irsv)
    elseif isa(stmt, ReturnNode)
        # Handled at the very end
        return false
    elseif isa(stmt, PiNode)
        rt = tmeet(typeinf_lattice(interp), argextype(stmt.val, ir), widenconst(stmt.typ))
    elseif stmt === nothing
        return false
    elseif isa(stmt, GlobalRef)
        # GlobalRef is not refinable
    else
        rt = argextype(stmt, irsv.ir)
    end
    if rt !== nothing
        if isa(rt, Const)
            inst[:type] = rt
            if is_inlineable_constant(rt.val) && (inst[:flag] & (IR_FLAG_EFFECT_FREE | IR_FLAG_NOTHROW)) == IR_FLAG_EFFECT_FREE | IR_FLAG_NOTHROW
                inst[:stmt] = quoted(rt.val)
            end
            return true
        elseif !⊑(typeinf_lattice(interp), typ, rt)
            inst[:type] = rt
            return true
        end
    end
    return false
end

# Process the terminator and add the successor to `bb_ip`. Returns whether a backedge was seen.
function process_terminator!(ir::IRCode, @nospecialize(stmt), idx::Int, bb::Int,
    all_rets::Vector{Int}, bb_ip::BitSetBoundedMinPrioritySet)
    if isa(stmt, ReturnNode)
        if isdefined(stmt, :val)
            push!(all_rets, idx)
        end
        return false
    elseif isa(stmt, GotoNode)
        backedge = stmt.label <= bb
        backedge || push!(bb_ip, stmt.label)
        return backedge
    elseif isa(stmt, GotoIfNot)
        backedge = stmt.dest <= bb
        backedge || push!(bb_ip, stmt.dest)
        push!(bb_ip, bb+1)
        return backedge
    elseif isexpr(stmt, :enter)
        dest = stmt.args[1]::Int
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
            inst = ir[SSAValue(idx)]
            stmt = inst[:stmt]
            typ = inst[:type]
            flag = inst[:flag]
            any_refined = false
            if (flag & IR_FLAG_REFINED) != 0
                any_refined = true
                inst[:flag] &= ~IR_FLAG_REFINED
            end
            for ur in userefs(stmt)
                val = ur[]
                if isa(val, Argument)
                    any_refined |= irsv.argtypes_refined[val.n]
                elseif isa(val, SSAValue)
                    any_refined |= val.id in ssa_refined
                    count!(tpdum, val)
                end
            end
            if isa(stmt, PhiNode) && idx in ssa_refined
                any_refined = true
                delete!(ssa_refined, idx)
            end
            is_terminator_or_phi = isa(stmt, PhiNode) || isa(stmt, GotoNode) || isa(stmt, GotoIfNot) || isa(stmt, ReturnNode) || isexpr(stmt, :enter)
            if typ === Bottom && (idx != lstmt || !is_terminator_or_phi)
                continue
            end
            if (any_refined && reprocess_instruction!(interp,
                    idx, bb, stmt, typ, irsv)) ||
               (externally_refined !== nothing && idx in externally_refined)
                push!(ssa_refined, idx)
                stmt = inst[:stmt]
                typ = inst[:type]
            end
            if typ === Bottom && !is_terminator_or_phi
                kill_terminator_edges!(irsv, lstmt, bb)
                if idx != lstmt
                    for idx2 in (idx+1:lstmt-1)
                        ir[SSAValue(idx2)] = nothing
                    end
                    ir[SSAValue(lstmt)][:stmt] = ReturnNode()
                end
                break
            end
            if idx == lstmt
                process_terminator!(ir, stmt, idx, bb, all_rets, bb_ip) && @goto residual_scan
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
                inst = ir[SSAValue(idx)]
                stmt = inst[:stmt]
                flag = inst[:flag]
                if (flag & IR_FLAG_REFINED) != 0
                    inst[:flag] &= ~IR_FLAG_REFINED
                    push!(stmt_ip, idx)
                end
                for ur in userefs(stmt)
                    val = ur[]
                    if isa(val, Argument)
                        if irsv.argtypes_refined[val.n]
                            push!(stmt_ip, idx)
                        end
                    elseif isa(val, SSAValue)
                        count!(tpdum, val)
                    end
                end
                idx == lstmt && process_terminator!(ir, stmt, idx, bb, all_rets, bb_ip)
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
                inst = ir[SSAValue(idx)]
                stmt = inst[:stmt]
                for ur in userefs(stmt)
                    val = ur[]
                    if isa(val, SSAValue)
                        push!(tpdum[val.id], idx)
                    end
                end
                idx == lstmt && process_terminator!(ir, stmt, idx, bb, all_rets, bb_ip)
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
            inst = ir[SSAValue(idx)]
            stmt = inst[:stmt]
            typ = inst[:type]
            if reprocess_instruction!(interp,
                idx, nothing, stmt, typ, irsv)
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
            inst = ir[SSAValue(idx)][:stmt]::ReturnNode
            rt = argextype(inst.val, ir)
            ultimate_rt = tmerge(typeinf_lattice(interp), ultimate_rt, rt)
        end
    end

    nothrow = true
    for idx = 1:length(ir.stmts)
        if (ir[SSAValue(idx)][:flag] & IR_FLAG_NOTHROW) == 0
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
