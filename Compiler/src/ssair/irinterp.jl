# This file is a part of Julia. License is MIT: https://julialang.org/license

function collect_limitations!(@nospecialize(typ), ::IRInterpretationState)
    @assert !isa(typ, LimitedAccuracy) "irinterp is unable to handle heavy recursion correctly"
    return typ
end

function concrete_eval_invoke(interp::AbstractInterpreter, ci::CodeInstance, argtypes::Vector{Any}, parent::IRInterpretationState)
    world = get_inference_world(interp)
    effects = decode_effects(ci.ipo_purity_bits)
    if (is_foldable(effects) && is_all_const_arg(argtypes, #=start=#1) &&
        (is_nonoverlayed(interp) || is_nonoverlayed(effects)))
        args = collect_const_args(argtypes, #=start=#1)
        value = try
            Core._call_in_world_total(world, args...)
        catch
            return Pair{Any,Tuple{Bool,Bool}}(Bottom, (false, is_noub(effects)))
        end
        return Pair{Any,Tuple{Bool,Bool}}(Const(value), (true, true))
    else
        mi = get_ci_mi(ci)
        if is_constprop_edge_recursed(mi, parent)
            return Pair{Any,Tuple{Bool,Bool}}(nothing, (is_nothrow(effects), is_noub(effects)))
        end
        newirsv = IRInterpretationState(interp, ci, mi, argtypes)
        if newirsv !== nothing
            assign_parentchild!(newirsv, parent)
            return ir_abstract_constant_propagation(interp, newirsv)
        end
        return Pair{Any,Tuple{Bool,Bool}}(nothing, (is_nothrow(effects), is_noub(effects)))
    end
end

function abstract_eval_invoke_inst(interp::AbstractInterpreter, inst::Instruction, irsv::IRInterpretationState)
    stmt = inst[:stmt]::Expr
    ci = stmt.args[1]
    if ci isa MethodInstance
        mi_cache = code_cache(interp)
        code = get(mi_cache, ci, nothing)
        code === nothing && return Pair{Any,Tuple{Bool,Bool}}(nothing, (false, false))
    else
        code = ci::CodeInstance
    end
    argtypes = collect_argtypes(interp, stmt.args[2:end], StatementState(nothing, false), irsv)
    argtypes === nothing && return Pair{Any,Tuple{Bool,Bool}}(Bottom, (false, false))
    return concrete_eval_invoke(interp, code, argtypes, irsv)
end

abstract_eval_ssavalue(s::SSAValue, sv::IRInterpretationState) = abstract_eval_ssavalue(s, sv.ir)

function abstract_eval_phi_stmt(interp::AbstractInterpreter, phi::PhiNode, ::Int, irsv::IRInterpretationState)
    return abstract_eval_phi(interp, phi, StatementState(nothing, false), irsv)
end

function abstract_call(interp::AbstractInterpreter, arginfo::ArgInfo, sstate::StatementState, irsv::IRInterpretationState)
    si = StmtInfo(true, sstate.saw_latestworld) # TODO better job here?
    call = abstract_call(interp, arginfo, si, irsv)::Future
    Future{Any}(call, interp, irsv) do call, interp, irsv
        irsv.ir.stmts[irsv.curridx][:info] = call.info
        nothing
    end
    return call
end

function kill_block!(ir::IRCode, bb::Int)
    # Kill the entire block
    stmts = ir.cfg.blocks[bb].stmts
    for bidx = stmts
        inst = ir[SSAValue(bidx)]
        inst[:stmt] = nothing
        inst[:type] = Bottom
        inst[:flag] = IR_FLAGS_REMOVABLE
    end
    ir[SSAValue(last(stmts))][:stmt] = ReturnNode()
    return
end
kill_block!(ir::IRCode) = (bb::Int)->kill_block!(ir, bb)

function update_phi!(irsv::IRInterpretationState, from::Int, to::Int)
    ir = irsv.ir
    if length(ir.cfg.blocks[to].preds) == 0
        kill_block!(ir, to)
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
        kill_edge!(irsv, bb, stmt.dest)
        kill_edge!(irsv, bb, bb+1)
    elseif isa(stmt, GotoNode)
        kill_edge!(irsv, bb, stmt.label)
    elseif isa(stmt, ReturnNode)
        # Nothing to do
    else
        @assert !isa(stmt, EnterNode)
        kill_edge!(irsv, bb, bb+1)
    end
end

function kill_edge!(irsv::IRInterpretationState, from::Int, to::Int)
    kill_edge!(get!(irsv.lazyreachability), irsv.ir.cfg, from, to,
               update_phi!(irsv), kill_block!(irsv.ir))
end

function reprocess_instruction!(interp::AbstractInterpreter, inst::Instruction, idx::Int,
                                bb::Union{Int,Nothing}, irsv::IRInterpretationState)
    ir = irsv.ir
    stmt = inst[:stmt]
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
            add_flag!(inst, IR_FLAG_NOTHROW)
            if condval
                inst[:stmt] = nothing
                kill_edge!(irsv, bb, stmt.dest)
            else
                inst[:stmt] = GotoNode(stmt.dest)
                kill_edge!(irsv, bb, bb+1)
            end
            return true
        end
        return false
    end
    rt = nothing
    if isa(stmt, Expr)
        head = stmt.head
        if (head === :call || head === :foreigncall || head === :new || head === :splatnew ||
            head === :static_parameter || head === :isdefined || head === :boundscheck)
            @assert isempty(irsv.tasks) # TODO: this whole function needs to be converted to a stackless design to be a valid AbsIntState, but this should work here for now
            result = abstract_eval_statement_expr(interp, stmt, StatementState(nothing, false), irsv)
            reverse!(irsv.tasks)
            while true
                if length(irsv.callstack) > irsv.frameid
                    typeinf(interp, irsv.callstack[irsv.frameid + 1])
                elseif !doworkloop(interp, irsv)
                    break
                end
            end
            @assert length(irsv.callstack) == irsv.frameid && isempty(irsv.tasks)
            result isa Future && (result = result[])
            (; rt, effects) = result
            add_flag!(inst, flags_for_effects(effects))
        elseif head === :invoke  # COMBAK: || head === :invoke_modifyfield (similar to call, but for args[2:end])
            rt, (nothrow, noub) = abstract_eval_invoke_inst(interp, inst, irsv)
            if nothrow
                add_flag!(inst, IR_FLAG_NOTHROW)
            end
            if noub
                add_flag!(inst, IR_FLAG_NOUB)
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
        elseif head === :leave
            return false
        else
            Core.println(stmt)
            error("reprocess_instruction!: unhandled expression found")
        end
    elseif isa(stmt, PhiNode)
        rt = abstract_eval_phi_stmt(interp, stmt, idx, irsv)
    elseif isa(stmt, UpsilonNode)
        rt = argextype(stmt.val, irsv.ir)
    elseif isa(stmt, PhiCNode)
        # Currently not modeled
        return false
    elseif isa(stmt, EnterNode)
        # TODO: Propagate scope type changes
        return false
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
    @assert !(rt isa LimitedAccuracy)
    if rt !== nothing
        if has_flag(inst, IR_FLAG_UNUSED)
            # Don't bother checking the type if we know it's unused
            if has_flag(inst, IR_FLAGS_REMOVABLE)
                inst[:stmt] = nothing
            end
            return false
        end
        if isa(rt, Const)
            inst[:type] = rt
            if is_inlineable_constant(rt.val) && has_flag(inst, IR_FLAGS_REMOVABLE)
                inst[:stmt] = quoted(rt.val)
            end
            return true
        elseif !⊑(typeinf_lattice(interp), inst[:type], rt)
            inst[:type] = rt
            return true
        end
    end
    return false
end

# Process the terminator and add the successor to `bb_ip`. Returns whether a backedge was seen.
function process_terminator!(@nospecialize(stmt), bb::Int, bb_ip::BitSetBoundedMinPrioritySet)
    if isa(stmt, ReturnNode)
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
    elseif isa(stmt, EnterNode)
        dest = stmt.catch_dest
        if dest ≠ 0
            @assert dest > bb
            push!(bb_ip, dest)
        end
        push!(bb_ip, bb+1)
        return false
    else
        push!(bb_ip, bb+1)
        return false
    end
end

struct BBScanner
    ir::IRCode
    bb_ip::BitSetBoundedMinPrioritySet
end

function BBScanner(ir::IRCode)
    bbs = ir.cfg.blocks
    bb_ip = BitSetBoundedMinPrioritySet(length(bbs))
    push!(bb_ip, 1)
    return BBScanner(ir, bb_ip)
end

function scan!(callback, scanner::BBScanner, forwards_only::Bool)
    (; bb_ip, ir) = scanner
    bbs = ir.cfg.blocks
    while !isempty(bb_ip)
        bb = popfirst!(bb_ip)
        stmts = bbs[bb].stmts
        lstmt = last(stmts)
        for idx = stmts
            inst = ir[SSAValue(idx)]
            ret = callback(inst, lstmt, bb)
            ret === nothing && return true
            ret::Bool || break
            idx == lstmt && process_terminator!(inst[:stmt], bb, bb_ip) && forwards_only && return false
        end
    end
    return true
end

function populate_def_use_map!(tpdum::TwoPhaseDefUseMap, scanner::BBScanner)
    scan!(scanner, false) do inst::Instruction, lstmt::Int, bb::Int
        for ur in userefs(inst)
            val = ur[]
            if isa(val, SSAValue)
                push!(tpdum[val.id], inst.idx)
            end
        end
        return true
    end
end
populate_def_use_map!(tpdum::TwoPhaseDefUseMap, ir::IRCode) =
    populate_def_use_map!(tpdum, BBScanner(ir))

function is_all_const_call(@nospecialize(stmt), interp::AbstractInterpreter, irsv::IRInterpretationState)
    isexpr(stmt, :call) || return false
    @inbounds for i = 2:length(stmt.args)
        argtype = abstract_eval_value(interp, stmt.args[i], StatementState(nothing, false), irsv)
        is_const_argtype(argtype) || return false
    end
    return true
end

function ir_abstract_constant_propagation(interp::AbstractInterpreter, irsv::IRInterpretationState;
        externally_refined::Union{Nothing,BitSet} = nothing)
    (; ir, tpdum, ssa_refined) = irsv

    @assert isempty(ir.new_nodes) "IRCode should be compacted before irinterp"

    all_rets = Int[]
    scanner = BBScanner(ir)

    check_ret!(@nospecialize(stmt), idx::Int) = isa(stmt, ReturnNode) && isdefined(stmt, :val) && push!(all_rets, idx)

    # Fast path: Scan both use counts and refinement in one single pass of
    #            of the instructions. In the absence of backedges, this will
    #            converge.
    completed_scan = scan!(scanner, true) do inst::Instruction, lstmt::Int, bb::Int
        idx = inst.idx
        irsv.curridx = idx
        stmt = inst[:stmt]
        typ = inst[:type]
        flag = inst[:flag]
        any_refined = false
        if has_flag(flag, IR_FLAG_REFINED)
            any_refined = true
            sub_flag!(inst, IR_FLAG_REFINED)
        elseif is_all_const_call(stmt, interp, irsv)
            # force reinference on calls with all constant arguments
            any_refined = true
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
        check_ret!(stmt, idx)
        is_terminator_or_phi = (isa(stmt, PhiNode) || stmt === nothing || isterminator(stmt))
        if typ === Bottom && !(idx == lstmt && is_terminator_or_phi)
            return true
        end
        if (any_refined && reprocess_instruction!(interp, inst, idx, bb, irsv)) ||
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
            return false
        end
        return true
    end

    if !completed_scan
        # Slow path
        stmt_ip = BitSetBoundedMinPrioritySet(length(ir.stmts))

        # Slow Path Phase 1.A: Complete use scanning
        scan!(scanner, false) do inst::Instruction, lstmt::Int, bb::Int
            idx = inst.idx
            irsv.curridx = idx
            stmt = inst[:stmt]
            flag = inst[:flag]
            if has_flag(flag, IR_FLAG_REFINED)
                sub_flag!(inst, IR_FLAG_REFINED)
                push!(stmt_ip, idx)
            end
            check_ret!(stmt, idx)
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
            return true
        end

        # Slow Path Phase 1.B: Assemble def-use map
        complete!(tpdum); push!(scanner.bb_ip, 1)
        populate_def_use_map!(tpdum, scanner)

        # Slow Path Phase 2: Use def-use map to converge cycles.
        # TODO: It would be possible to return to the fast path after converging
        #       each cycle, but that's somewhat complicated.
        for val in ssa_refined
            for use in tpdum[val]
                if !(use in ssa_refined)
                    push!(stmt_ip, use)
                end
            end
        end
        while !isempty(stmt_ip)
            idx = popfirst!(stmt_ip)
            irsv.curridx = idx
            inst = ir[SSAValue(idx)]
            if reprocess_instruction!(interp, inst, idx, nothing, irsv)
                append!(stmt_ip, tpdum[idx])
            end
        end
    end

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

    nothrow = noub = true
    for idx = 1:length(ir.stmts)
        if ir[SSAValue(idx)][:stmt] === nothing
            # skip `nothing` statement, which might be inserted as a dummy node,
            # e.g. by `finish_current_bb!` without explicitly marking it as `:nothrow`
            continue
        end
        flag = ir[SSAValue(idx)][:flag]
        nothrow &= has_flag(flag, IR_FLAG_NOTHROW)
        noub &= has_flag(flag, IR_FLAG_NOUB)
        (nothrow | noub) || break
    end

    if irsv.frameid != 0
        callstack = irsv.callstack::Vector{AbsIntState}
        @assert callstack[end] === irsv && length(callstack) == irsv.frameid
        pop!(callstack)
    end

    return Pair{Any,Tuple{Bool,Bool}}(maybe_singleton_const(ultimate_rt), (nothrow, noub))
end
