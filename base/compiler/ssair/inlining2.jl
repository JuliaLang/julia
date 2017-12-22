struct InliningTodo
    idx::Int # The statement to replace
    # Properties of the call - these determine how arguments
    # need to be rewritten.
    isva::Bool
    isinvoke::Bool
    isapply::Bool
    na::Int
    method::Method  # The method being inlined
    sparams::Vector{Any} # The static parameters we computed for this call site
    metharg::Any
    # The LineTable and IR of the inlinee
    linetable::Vector{LineInfoNode}
    ir::IRCode
    # If the function being inlined is a single basic block we can use a
    # simpler inlining algorithm. This flag determines whether that's allowed
    linear_inline_eligible::Bool
end

struct ConstantCase
    val::Any
    method::Method
    sparams::Vector{Any}
    metharg::Any
end

struct DynamicCase
    method::Method
    sparams::Vector{Any}
    metharg::Any
end

struct UnionSplit
    idx::Int # The statement to replace
    fully_covered::Bool
    atype::Any
    isinvoke::Bool
    cases::Vector{Pair{Type, Any}}
    bbs::Vector{Int}
end
UnionSplit(idx, fully_covered, atype, isinvoke, cases) = UnionSplit(idx, fully_covered, atype, isinvoke, cases, Int[])

function ssa_inlining_pass!(ir::IRCode, linetable::Vector{LineInfoNode}, sv::OptimizationState)
    # Go through the function, perfoming simple ininlingin (e.g. replacing call by constants
    # and analyzing legality of inlining).
    @timeit "analysis" todo = assemble_inline_todo!(ir, linetable, sv)
    isempty(todo) && return ir
    # Do the actual inlining for every call we identified
    @timeit "execution" ir = batch_inline!(todo, ir, linetable, sv)
    return ir
end

mutable struct CFGInliningState
    new_cfg_blocks::Vector{BasicBlock}
    inserted_block_ranges::Vector{UnitRange{Int}}
    todo_bbs::Vector{Tuple{Int, Int}}
    first_bb::Int
    bb_rename::Vector{Int}
    split_targets::BitSet
    merged_orig_blocks::BitSet
    cfg::CFG
end

function CFGInliningState(ir::IRCode)
    CFGInliningState(
        BasicBlock[],
        UnitRange{Int}[],
        Tuple{Int, Int}[],
        0,
        zeros(Int, length(ir.cfg.blocks)),
        BitSet(),
        BitSet(),
        ir.cfg
    )
end

# Tells the inliner that we're now inlining into block `block`, meaning
# all previous blocks have been proceesed and can be added to the new cfg
function inline_into_block!(state::CFGInliningState, block::Int)
    if state.first_bb != block
        new_range = state.first_bb+1:block
        l = length(state.new_cfg_blocks)
        state.bb_rename[new_range] = (l+1:l+length(new_range))
        append!(state.new_cfg_blocks, map(copy, state.cfg.blocks[new_range]))
        push!(state.merged_orig_blocks, last(new_range))
    end
    state.first_bb = block
    return
end

function cfg_inline_item!(item::InliningTodo, state::CFGInliningState, from_unionsplit::Bool=false)
    inlinee_cfg = item.ir.cfg
    # Figure out if we need to split the BB
    need_split_before = false
    need_split = true
    block = block_for_inst(state.cfg, item.idx)
    inline_into_block!(state, block)

    if !isempty(inlinee_cfg.blocks[1].preds)
        need_split_before = true
    end

    last_block_idx = last(state.cfg.blocks[block].stmts)
    if false # TODO: ((idx+1) == last_block_idx && isa(ir[SSAValue(last_block_idx)], GotoNode))
        need_split = false
        post_bb_id = -ir[SSAValue(last_block_idx)].label
    else
        post_bb_id = length(state.new_cfg_blocks) + length(inlinee_cfg.blocks) + (need_split_before ? 1 : 0)
        need_split = true #!(idx == last_block_idx)
    end

    if !need_split
        delete!(state.merged_orig_blocks, last(new_range))
    end

    push!(state.todo_bbs, (length(state.new_cfg_blocks) - 1 + (need_split_before ? 1 : 0), post_bb_id))

    from_unionsplit || delete!(state.split_targets, length(state.new_cfg_blocks))
    orig_succs = copy(state.new_cfg_blocks[end].succs)
    empty!(state.new_cfg_blocks[end].succs)
    if need_split_before
        l = length(state.new_cfg_blocks)
        bb_rename_range = (1+l:length(inlinee_cfg.blocks)+l)
        push!(state.new_cfg_blocks[end].succs, length(state.new_cfg_blocks)+1)
        append!(state.new_cfg_blocks, inlinee_cfg.blocks)
    else
        # Merge the last block that was already there with the first block we're adding
        l = length(state.new_cfg_blocks)
        bb_rename_range = (l:length(inlinee_cfg.blocks)+l-1)
        append!(state.new_cfg_blocks[end].succs, inlinee_cfg.blocks[1].succs)
        append!(state.new_cfg_blocks, inlinee_cfg.blocks[2:end])
    end
    if need_split
        push!(state.new_cfg_blocks, BasicBlock(state.cfg.blocks[block].stmts,
            Int[], orig_succs))
        from_unionsplit || push!(state.split_targets, length(state.new_cfg_blocks))
    end
    new_block_range = (length(state.new_cfg_blocks)-length(inlinee_cfg.blocks)+1):length(state.new_cfg_blocks)
    push!(state.inserted_block_ranges, new_block_range)

    # Fixup the edges of the newely added blocks
    for (old_block, new_block) in enumerate(bb_rename_range)
        if old_block != 1 || need_split_before
            p = state.new_cfg_blocks[new_block].preds
            map!(p, p) do old_pred_block
                return bb_rename_range[old_pred_block]
            end
        end
        if new_block != last(new_block_range)
            s = state.new_cfg_blocks[new_block].succs
            map!(s, s) do old_succ_block
                return bb_rename_range[old_succ_block]
            end
        end
    end

    if need_split_before
        push!(state.new_cfg_blocks[first(bb_rename_range)].preds, first(bb_rename_range)-1)
    end

    for (old_block, new_block) in enumerate(bb_rename_range)
        if (length(state.new_cfg_blocks[new_block].succs) == 0)
            terminator_idx = last(inlinee_cfg.blocks[old_block].stmts)
            terminator = item.ir[SSAValue(terminator_idx)]
            if isa(terminator, ReturnNode) && isdefined(terminator, :val)
                push!(state.new_cfg_blocks[new_block].succs, post_bb_id)
                if need_split
                    push!(state.new_cfg_blocks[post_bb_id].preds, new_block)
                end
            end
        end
    end
end

function cfg_inline_unionsplit!(item::UnionSplit, state::CFGInliningState)
    block = block_for_inst(state.cfg, item.idx)
    inline_into_block!(state, block)
    from_bbs = Int[]
    delete!(state.split_targets, length(state.new_cfg_blocks))
    orig_succs = copy(state.new_cfg_blocks[end].succs)
    empty!(state.new_cfg_blocks[end].succs)
    for (i, (_, case)) in enumerate(item.cases)
        # The condition gets sunk into the previous block
        # Add a block for the union-split body
        push!(state.new_cfg_blocks, BasicBlock(StmtRange(item.idx, item.idx)))
        cond_bb = length(state.new_cfg_blocks)-1
        push!(state.new_cfg_blocks[end].preds, cond_bb)
        push!(state.new_cfg_blocks[cond_bb].succs, cond_bb+1)
        if isa(case, InliningTodo) && !case.linear_inline_eligible
            cfg_inline_item!(case, state, true)
        end
        bb = length(state.new_cfg_blocks)
        push!(from_bbs, bb)
        # TODO: Right now we unconditionally generate a fallback block
        # in case of subtyping errors - This is probably unnecessary.
        if true # i != length(item.cases) || !item.fully_covered
            # This block will have the next condition or the final else case
            push!(state.new_cfg_blocks, BasicBlock(StmtRange(item.idx, item.idx)))
            push!(state.new_cfg_blocks[cond_bb].succs, length(state.new_cfg_blocks))
            push!(state.new_cfg_blocks[end].preds, cond_bb)
            push!(item.bbs, length(state.new_cfg_blocks))
        end
    end
    # The edge from the fallback block.
    if !item.fully_covered
        push!(from_bbs, length(state.new_cfg_blocks))
    end
    # This block will be the block everyone returns to
    push!(state.new_cfg_blocks, BasicBlock(StmtRange(item.idx, item.idx), from_bbs, orig_succs))
    join_bb = length(state.new_cfg_blocks)
    push!(state.split_targets, join_bb)
    push!(item.bbs, join_bb)
    for bb in from_bbs
        push!(state.new_cfg_blocks[bb].succs, join_bb)
    end
end

function finish_cfg_inline!(state::CFGInliningState)
    new_range = (state.first_bb + 1):length(state.cfg.blocks)
    l = length(state.new_cfg_blocks)
    state.bb_rename[new_range] = (l+1:l+length(new_range))
    append!(state.new_cfg_blocks, state.cfg.blocks[new_range])

    # Rename edges original bbs
    for (orig_bb, bb) in pairs(state.bb_rename)
        p, s = state.new_cfg_blocks[bb].preds, state.new_cfg_blocks[bb].succs
        map!(p, p) do pred_bb
            pred_bb == length(state.bb_rename) && return length(state.new_cfg_blocks)
            return state.bb_rename[pred_bb + 1] - 1
        end
        if !(orig_bb in state.merged_orig_blocks)
            map!(s, s) do succ_bb
                return state.bb_rename[succ_bb]
            end
        end
    end

    for bb in collect(state.split_targets)
        s = state.new_cfg_blocks[bb].succs
        map!(s, s) do succ_bb
            return state.bb_rename[succ_bb]
        end
    end

    # Rename any annotated original bb references
    for bb in 1:length(state.new_cfg_blocks)
        s = state.new_cfg_blocks[bb].succs
        map!(s, s) do succ_bb
            return succ_bb < 0 ? state.bb_rename[-succ_bb] : succ_bb
        end
    end
end

function ir_inline_item!(compact::IncrementalCompact, idx::Int, argexprs::Vector{Any},
                         linetable::Vector{LineInfoNode}, item::InliningTodo,
                         boundscheck::Symbol, todo_bbs::Vector{Tuple{Int, Int}})
    # Ok, do the inlining here
    inline_cfg = item.ir.cfg
    stmt = compact.result[idx]
    linetable_offset = length(linetable)
    # Append the linetable of the inlined function to our line table
    inlined_at = compact.result_lines[idx]
    for entry in item.linetable
        push!(linetable, LineInfoNode(entry.mod, entry.method, entry.file, entry.line,
            (entry.inlined_at > 0 ? entry.inlined_at + linetable_offset : inlined_at)))
    end
    if item.isva
        vararg = mk_tuplecall!(compact, argexprs[item.na:end], compact.result_lines[idx])
        argexprs = Any[argexprs[1:(item.na - 1)]..., vararg]
    end
    flag = compact.result_flags[idx]
    boundscheck_idx = boundscheck
    if boundscheck_idx === :default || boundscheck_idx === :propagate
        if (flag & IR_FLAG_INBOUNDS) != 0
            boundscheck_idx = :off
        end
    end
    # If the iterator already moved on to the next basic block,
    # temorarily re-open in again.
    local return_value
    # Special case inlining that maintains the current basic block if there's only one BB in the target
    if item.linear_inline_eligible
        terminator = item.ir[SSAValue(last(inline_cfg.blocks[1].stmts))]
        #compact[idx] = nothing
        inline_compact = IncrementalCompact(compact, item.ir, compact.result_idx)
        for (idx′, stmt′) in inline_compact
            # This dance is done to maintain accurate usage counts in the
            # face of rename_arguments! mutating in place - should figure out
            # something better eventually.
            inline_compact[idx′] = nothing
            stmt′ = ssa_substitute!(idx′, stmt′, argexprs, item.method.sig, item.sparams, linetable_offset, boundscheck_idx, compact)
            if isa(stmt′, ReturnNode)
                isa(stmt′.val, SSAValue) && (compact.used_ssas[stmt′.val.id] += 1)
                return_value = stmt′.val
                stmt′ = nothing
            end
            inline_compact[idx′] = stmt′
        end
        just_fixup!(inline_compact)
        compact.result_idx = inline_compact.result_idx
    else
        bb_offset, post_bb_id = popfirst!(todo_bbs)
        # This implements the need_split_before flag above
        need_split_before = !isempty(item.ir.cfg.blocks[1].preds)
        if need_split_before
            finish_current_bb!(compact)
        end
        pn = PhiNode()
        #compact[idx] = nothing
        inline_compact = IncrementalCompact(compact, item.ir, compact.result_idx)
        for (idx′, stmt′) in inline_compact
            inline_compact[idx′] = nothing
            stmt′ = ssa_substitute!(idx′, stmt′, argexprs, item.method.sig, item.sparams, linetable_offset, boundscheck_idx, compact)
            if isa(stmt′, ReturnNode)
                if isdefined(stmt′, :val)
                    push!(pn.edges, inline_compact.active_result_bb-1)
                    push!(pn.values, stmt′.val)
                    stmt′ = GotoNode(post_bb_id)
                end
            elseif isa(stmt′, GotoNode)
                stmt′ = GotoNode(stmt′.label + bb_offset)
            elseif isa(stmt′, Expr) && stmt′.head == :enter
                stmt′ = Expr(:enter, stmt′.args[1] + bb_offset)
            elseif isa(stmt′, GotoIfNot)
                stmt′ = GotoIfNot(stmt′.cond, stmt′.dest + bb_offset)
            elseif isa(stmt′, PhiNode)
                stmt′ = PhiNode(Any[edge+bb_offset for edge in stmt′.edges], stmt′.values)
            end
            inline_compact[idx′] = stmt′
        end
        just_fixup!(inline_compact)
        compact.result_idx = inline_compact.result_idx
        compact.active_result_bb = inline_compact.active_result_bb
        for i = 1:length(pn.values)
            isassigned(pn.values, i) || continue
            if isa(pn.values[i], SSAValue)
                compact.used_ssas[pn.values[i].id] += 1
            end
        end
        if length(pn.edges) == 1
            return_value = pn.values[1]
        else
            return_value = insert_node_here!(compact, pn, stmt.typ, compact.result_lines[idx])
        end
    end
    return_value
end

function ir_inline_unionsplit!(compact::IncrementalCompact, topmod::Module, idx::Int,
                               argexprs::Vector{Any}, linetable::Vector{LineInfoNode},
                               item::UnionSplit, boundscheck::Symbol, todo_bbs::Vector{Tuple{Int, Int}})
    stmt, typ, line = compact.result[idx], compact.result_types[idx], compact.result_lines[idx]
    atype = item.atype
    generic_bb = item.bbs[end-1]
    join_bb = item.bbs[end]
    bb = compact.active_result_bb
    pn = PhiNode()
    has_generic = false
    @assert length(item.bbs) > length(item.cases)
    for ((metharg, case), next_cond_bb) in zip(item.cases, item.bbs)
        @assert !isa(metharg, UnionAll)
        cond = true
        @assert length(atype.parameters) == length(metharg.parameters)
        for i in 2:length(atype.parameters)
            a, m = atype.parameters[i], metharg.parameters[i]
            # If this is always true, we don't need to check for it
            a <: m && continue
            # Generate isa check
            isa_expr = Expr(:call, isa, argexprs[i], m)
            isa_expr.typ = Bool
            ssa = insert_node_here!(compact, isa_expr, Bool, line)
            if cond === true
                cond = ssa
            else
                and_expr = Expr(:call, and_int, cond, ssa)
                cond = insert_node_here!(compact, and_expr, Bool, line)
            end
        end
        insert_node_here!(compact, GotoIfNot(cond, next_cond_bb), Union{}, line)
        bb = next_cond_bb - 1
        finish_current_bb!(compact)
        argexprs′ = argexprs
        if !isa(case, ConstantCase)
            argexprs′ = copy(argexprs)
            for i = 2:length(metharg.parameters)
                a, m = atype.parameters[i], metharg.parameters[i]
                isa(argexprs[i], SSAValue) || continue
                if !(a <: m)
                    argexprs′[i] = insert_node_here!(compact, PiNode(argexprs′[i], m),
                                                     m, line)
                end
            end
        end
        if isa(case, InliningTodo)
            val = ir_inline_item!(compact, idx, argexprs′, linetable, case, boundscheck, todo_bbs)
        elseif isa(case, MethodInstance)
            val = insert_node_here!(compact, Expr(:invoke, case, argexprs′...), typ, line)
        else
            case = case::ConstantCase
            val = case.val
        end
        push!(pn.edges, bb)
        push!(pn.values, val)
        insert_node_here!(compact, GotoNode(join_bb), Union{}, line)
        finish_current_bb!(compact)
    end
    bb += 1
    # We're now in the fall through block, decide what to do
    if item.fully_covered
        e = Expr(:call, GlobalRef(topmod, :error), "fatal error in type inference (type bound)")
        e.typ = Union{}
        insert_node_here!(compact, e, Union{}, line)
        insert_node_here!(compact, ReturnNode(), Union{}, line)
        finish_current_bb!(compact)
    else
        ssa = insert_node_here!(compact, stmt, typ, line)
        push!(pn.edges, bb)
        push!(pn.values, ssa)
        insert_node_here!(compact, GotoNode(join_bb), Union{}, line)
        finish_current_bb!(compact)
    end

    # We're now in the join block.
    compact.ssa_rename[compact.idx-1] = insert_node_here!(compact, pn, typ, line)
end

function batch_inline!(todo::Vector{Any}, ir::IRCode, linetable::Vector{LineInfoNode}, sv::OptimizationState)
    # Compute the new CFG first (modulo statement ranges, which will be computed below)
    state = CFGInliningState(ir)
    for item in todo
        if isa(item, UnionSplit)
            cfg_inline_unionsplit!(item::UnionSplit, state)
        else
            item = item::InliningTodo
            # A linear inline does not modify the CFG
            item.linear_inline_eligible && continue
            cfg_inline_item!(item, state)
        end
    end
    finish_cfg_inline!(state)

    boundscheck = inbounds_option()
    if boundscheck === :default && sv.src.propagate_inbounds
        boundscheck = :propagate
    end

    let compact = IncrementalCompact(ir)
        compact.result_bbs = state.new_cfg_blocks
        # This needs to be a minimum and is more of a size hint
        nnewnodes = length(compact.result) + (sum(todo) do item
            return isa(item, InliningTodo) ? (length(item.ir.stmts) + length(item.ir.new_nodes)) : 0
        end)
        resize!(compact, nnewnodes)
        item = popfirst!(todo)
        inline_idx = item.idx
        for (idx, stmt) in compact
            if compact.idx - 1 == inline_idx
                argexprs = copy(stmt.args)
                refinish = false
                if compact.result_idx == first(compact.result_bbs[compact.active_result_bb].stmts)
                    compact.active_result_bb -= 1
                    refinish = true
                end
                # At the moment we will allow globalrefs in argument position, turn those into ssa values
                for aidx in 1:length(argexprs)
                    aexpr = argexprs[aidx]
                    if isa(aexpr, GlobalRef)
                        argexprs[aidx] = insert_node_here!(compact, aexpr, compact_exprtype(compact, aexpr), compact.result_lines[idx])
                    end
                end
                if item.isinvoke
                    argexprs = rewrite_invoke_exprargs!((node, typ)->insert_node_here!(compact, node, typ, compact.result_lines[idx]),
                                                arg->compact_exprtype(compact, arg), argexprs)
                end
                if isa(item, InliningTodo)
                    compact.ssa_rename[compact.idx-1] = ir_inline_item!(compact, idx, argexprs, linetable, item, boundscheck, state.todo_bbs)
                elseif isa(item, UnionSplit)
                    ir_inline_unionsplit!(compact, _topmod(sv.mod), idx, argexprs, linetable, item, boundscheck, state.todo_bbs)
                end
                compact[idx] = nothing
                refinish && finish_current_bb!(compact)
                if !isempty(todo)
                    item = popfirst!(todo)
                    inline_idx = item.idx
                else
                    inline_idx = -1
                end
            elseif isa(stmt, GotoNode)
                compact[idx] = GotoNode(state.bb_rename[stmt.label])
            elseif isa(stmt, Expr) && stmt.head == :enter
                compact[idx] = Expr(:enter, state.bb_rename[stmt.args[1]])
            elseif isa(stmt, GotoIfNot)
                compact[idx] = GotoIfNot(stmt.cond, state.bb_rename[stmt.dest])
            elseif isa(stmt, PhiNode)
                compact[idx] = PhiNode(Any[edge == length(state.bb_rename) ? length(state.new_cfg_blocks) : state.bb_rename[edge+1]-1 for edge in stmt.edges], stmt.values)
            end
        end

        ir = finish(compact)
    end
    return ir
end

function _spec_lambda(@nospecialize(atype), sv::OptimizationState, @nospecialize(invoke_data))
    if invoke_data === nothing
        return ccall(:jl_get_spec_lambda, Any, (Any, UInt), atype, sv.params.world)
    else
        invoke_data = invoke_data::InvokeData
        atype <: invoke_data.types0 || return nothing
        return ccall(:jl_get_invoke_lambda, Any, (Any, Any, Any, UInt),
                     invoke_data.mt, invoke_data.entry, atype, sv.params.world)
    end
end

function spec_lambda(@nospecialize(atype), sv::OptimizationState, @nospecialize(invoke_data))
    linfo = _spec_lambda(atype, sv, invoke_data)
    linfo !== nothing && add_backedge!(linfo, sv)
    linfo
end

function rewrite_apply_exprargs!(inserter, exprtype, argexprs::Vector{Any})
    new_argexprs = Any[argexprs[2]]
    # Flatten all tuples
    for arg in argexprs[3:end]
        tupT = exprtype(arg)
        t = widenconst(tupT)
        for i = 1:length(t.parameters)
            # Insert a getfield call here
            new_call = Expr(:call, Core.getfield, arg, i)
            new_call.typ = getfield_tfunc(tupT, Const(i))
            push!(new_argexprs, inserter(new_call, new_call.typ))
        end
    end
    argexprs = new_argexprs
    return argexprs
end

function rewrite_invoke_exprargs!(inserter, exprtype, argexprs::Vector{Any})
    argexpr0 = argexprs[2]
    argexprs = argexprs[4:end]
    pushfirst!(argexprs, argexpr0)
    return argexprs
end

function singleton_type(@nospecialize(ft))
    if isa(ft, Const)
        return ft.val
    end
    return nothing
end

function analyze_method!(idx, f, ft, metharg, methsp, method, stmt, atypes, sv, atype_unlimited, isinvoke, isapply, invoke_data)
    methsig = method.sig

    # Check whether this call just evaluates to a constant
    if isa(f, widenconst(ft)) && !isdefined(method, :generator) && method.pure &&
            isa(stmt.typ, Const) && stmt.typ.actual && is_inlineable_constant(stmt.typ.val)
        return ConstantCase(quoted(stmt.typ.val), method, Any[methsp...], metharg)
    end

    # Check that we habe the correct number of arguments
    na = Int(method.nargs)
    npassedargs = length(atypes)
    if na != npassedargs && !(na > 0 && method.isva)
        # we have a method match only because an earlier
        # inference step shortened our call args list, even
        # though we have too many arguments to actually
        # call this function
        return nothing
    end

    # Bail out if any static parameters are left as TypeVar
    ok = true
    for i = 1:length(methsp)
        isa(methsp[i], TypeVar) && return nothing
    end

    # Find the linfo for this methods
    linfo = code_for_method(method, metharg, methsp, sv.params.world, true) # Union{Nothing, MethodInstance}
    if !isa(linfo, MethodInstance)
        return spec_lambda(atype_unlimited, sv, invoke_data)
    end

    if invoke_api(linfo) == 2
        # in this case function can be inlined to a constant
        add_backedge!(linfo, sv)
        return ConstantCase(quoted(linfo.inferred_const), method, Any[methsp...], metharg)
    end

    res = find_inferred(linfo, atypes, sv)
    res === nothing && return nothing

    if length(res::Tuple) == 1
        return ConstantCase(res[1], method, Any[methsp...], metharg)
    end
    (rettype, inferred) = res::Tuple

    if inferred === nothing
        return spec_lambda(atype_unlimited, sv, invoke_data)
    end

    src_inferred = ccall(:jl_ast_flag_inferred, Bool, (Any,), inferred)
    src_inlineable = ccall(:jl_ast_flag_inlineable, Bool, (Any,), inferred)

    if !(src_inferred && src_inlineable)
        return spec_lambda(atype_unlimited, sv, invoke_data)
    end

    # At this point we're committedd to performing the inlining, add the backedge
    add_backedge!(linfo, sv)

    if isa(inferred, CodeInfo)
        src = inferred
        ast = copy_exprargs(inferred.code)
    else
        src = ccall(:jl_uncompress_ast, Any, (Any, Any), method, inferred::Vector{UInt8})::CodeInfo
        # TODO: It seems like PhiNodes are shared between compressed codeinfos, making this copy necessary
        ast = copy_exprargs(src.code)
    end

    @timeit "inline IR inflation" if src.codelocs === nothing
        topline = LineInfoNode(method.module, method.name, method.file, Int(method.line), 0)
        inline_linetable = [topline]
        push!(ast, LabelNode(length(ast) + 1))
        ir2 = just_construct_ssa(src, ast, na-1, inline_linetable)
    else
        ir2, inline_linetable = inflate_ir(src), src.linetable
    end
    #verify_ir(ir2)

    return InliningTodo(idx,
        na > 0 && method.isva,
        isinvoke, isapply, na,
        method, Any[methsp...], metharg,
        inline_linetable, ir2, linear_inline_eligible(ir2))
end

# Neither the product iterator not CartesianIndices are available
# here, so use this poor man's version
struct SimpleCartesian
    ranges::Vector{UnitRange{Int}}
end
function iterate(s::SimpleCartesian, state::Vector{Int}=Int[1 for _ in 1:length(s.ranges)])
    state[end] > last(s.ranges[end]) && return nothing
    vals = copy(state)
    any = false
    for i = 1:length(s.ranges)
        if state[i] < last(s.ranges[i])
            for j = 1:(i-1)
                state[j] = first(s.ranges[j])
            end
            state[i] += 1
            any = true
            break
        end
    end
    if !any
        state[end] += 1
    end
    (vals, state)
end

# Given a signure, iterate over the signatures to union split over
struct UnionSplitSignature
    it::SimpleCartesian
    typs::Vector{Any}
end

function UnionSplitSignature(atypes::Vector{Any})
    typs = Any[uniontypes(widenconst(atypes[i])) for i = 1:length(atypes)]
    ranges = UnitRange{Int}[1:length(typs[i]) for i = 1:length(typs)]
    UnionSplitSignature(SimpleCartesian(ranges), typs)
end

function iterate(split::UnionSplitSignature, state::Vector{Int}...)
    y = iterate(split.it, state...)
    y === nothing && return nothing
    idxs, state = y
    sig = Any[split.typs[i][j] for (i,j) in enumerate(idxs)]
    sig, state
end

function handle_single_case!(ir, stmt, idx, case, isinvoke, todo)
    if isa(case, ConstantCase)
        ir[SSAValue(idx)] = case.val
    elseif isa(case, MethodInstance)
        if isinvoke
            stmt.args = rewrite_invoke_exprargs!(
                (node, typ)->insert_node!(ir, idx, typ, node),
                arg->exprtype(arg, ir, ir.mod),
                stmt.args)
        end
        stmt.head = :invoke
        pushfirst!(stmt.args, case)
    elseif case === nothing
        # Do, well, nothing
    else
        push!(todo, case::InliningTodo)
    end
end

function assemble_inline_todo!(ir::IRCode, linetable::Vector{LineInfoNode}, sv::OptimizationState)
    # todo = (inline_idx, (isva, isinvoke, isapply, na), method, spvals, inline_linetable, inline_ir, lie)
    todo = Any[]
    for idx in 1:length(ir.stmts)
        stmt = ir.stmts[idx]
        isexpr(stmt, :call) || continue
        eargs = stmt.args
        isempty(eargs) && continue
        arg1 = eargs[1]

        ft = exprtype(arg1, ir, ir.mod)
        has_free_typevars(ft) && continue
        isa(ft, Conditional) && (ft = Bool)
        f = singleton_type(ft)

        atypes = Vector{Any}(undef, length(stmt.args))
        atypes[1] = ft
        ok = true
        for i = 2:length(stmt.args)
            a = exprtype(stmt.args[i], ir, ir.mod)
            (a === Bottom || isvarargtype(a)) && (ok = false; break)
            atypes[i] = a
        end
        ok || continue

        # Check if we match any of the early inliners
        res = early_inline_special_case(ir, f, ft, stmt, atypes, sv.params)
        if res !== nothing
            ir.stmts[idx] = res[1]
            continue
        end

        if f !== Core.invoke && f !== Core._apply && (isa(f, IntrinsicFunction) || ft ⊑ IntrinsicFunction || isa(f, Builtin) || ft ⊑ Builtin)
            # No inlining for builtins (other than what's handled in the early inliner)
            continue
        end

        # Special handling for Core.invoke and Core._apply, which can follow the normal inliner
        # logic with modified inlining target
        isapply = isinvoke = false

        # Handle _apply
        isapply = false
        if f === Core._apply
            new_atypes = Any[]
            ft = exprtype(stmt.args[2], ir, ir.mod)
            has_free_typevars(ft) && continue
            isa(ft, Conditional) && (ft = Bool)
            f = singleton_type(ft)
            # Push function type
            push!(new_atypes, ft)
            # Try to figure out the signature of the function being called
            ok = true
            for (typ, def) in zip(atypes[3:end], stmt.args[3:end])
                typ = widenconst(typ)
                # We don't know what'll be in a SimpleVector, bail out
                isa(typ, SimpleVector) && (ok = false; break)
                # TODO: We could basically run the iteration protocol here
                if !isa(typ, DataType) || typ.name !== Tuple.name || isvatuple(typ)
                    ok = false
                    break
                end
                length(typ.parameters) <= sv.params.MAX_TUPLE_SPLAT || (ok = false; break)
                # As a special case, if we can see the tuple() call, look at it's arguments to find
                # our types. They can be more precise (e.g. f(Bool, A...) would be lowered as
                # _apply(f, tuple(Bool)::Tuple{DataType}, A), which might not be precise enough to
                # get a good method match). This pattern is used in the array code a bunch.
                if isa(def, SSAValue) && is_tuple_call(ir, ir[def])
                    for tuparg in ir[def].args[2:end]
                        push!(new_atypes, exprtype(tuparg, ir, ir.mod))
                    end
                elseif isa(def, Argument) && def.n === length(ir.argtypes) && !isempty(sv.result_vargs)
                    append!(new_atypes, sv.result_vargs)
                else
                    append!(new_atypes, typ.parameters)
                end
            end
            ok || continue
            atypes = new_atypes
            isapply = true
        end

        # Independent of whether we can inline, the above analysis allows us to rewrite
        # this apply call to a regular call
        if isapply
            stmt.args = rewrite_apply_exprargs!((node, typ)->insert_node!(ir, idx, typ, node), arg->exprtype(arg, ir, ir.mod), stmt.args)
        end

        if f !== Core.invoke && (isa(f, IntrinsicFunction) || ft ⊑ IntrinsicFunction || isa(f, Builtin) || ft ⊑ Builtin)
            continue
        end

        # Handle invoke
        invoke_data = nothing
        if f === Core.invoke && length(atypes) >= 3
            res = compute_invoke_data(atypes, stmt.args, sv)
            res === nothing && continue
            (f, ft, atypes, argexprs, invoke_data) = res
        end
        isinvoke = (invoke_data !== nothing)

        atype_unlimited = argtypes_to_type(atypes)

        # In :invoke, make sure that the arguments we're passing are a subtype of the
        # signature we're invoking.
        (invoke_data === nothing || atype_unlimited <: invoke_data.types0) || continue

        # Bail out here if inlining is disabled
        sv.params.inlining || continue

        # Special case inliners for regular functions
        if late_inline_special_case!(ir, idx, stmt, atypes, f, ft, _topmod(ir.mod))
            continue
        end

        # Compute the limited type if necessary
        if length(atype_unlimited.parameters) - 1 > sv.params.MAX_TUPLETYPE_LEN
            atype = limit_tuple_type(atype_unlimited, sv.params)
        else
            atype = atype_unlimited
        end

        # Ok, now figure out what method to call
        if invoke_data !== nothing
            method = invoke_data.entry.func
            (metharg, methsp) = ccall(:jl_type_intersection_with_env, Any, (Any, Any),
                                    atype_unlimited, method.sig)::SimpleVector
            methsp = methsp::SimpleVector
            result = analyze_method!(idx, f, ft, metharg, methsp, method, stmt, atypes, sv, atype_unlimited, isinvoke, isapply, invoke_data)
            handle_single_case!(ir, stmt, idx, result, isinvoke, todo)
            continue
        end

        # Regular case: Perform method matching
        min_valid = UInt[typemin(UInt)]
        max_valid = UInt[typemax(UInt)]
        meth = _methods_by_ftype(atype, sv.params.MAX_METHODS, sv.params.world, min_valid, max_valid)
        if meth === false || length(meth) == 0
            # No applicable method, or too many applicable methods
            continue
        end

        cases = Pair{Type, Any}[]
        # TODO: This could be better
        signature_union = Union{Any[match[1]::Type for match in meth]...}
        signature_fully_covered = atype <: signature_union
        fully_covered = signature_fully_covered
        split_out_sigs = Any[]

        # For any method match that's a dispatch tuple, extract those cases first
        for (i, match) in enumerate(meth)
            (metharg, methsp, method) = (match[1]::Type, match[2]::SimpleVector, match[3]::Method)
            if !isdispatchtuple(metharg)
                fully_covered = false
                continue
            end
            case = analyze_method!(idx, f, ft, metharg, methsp, method, stmt, atypes, sv, metharg, isinvoke, isapply, invoke_data)
            if case === nothing
                fully_covered = false
                continue
            end
            push!(cases, Pair{Type,Any}(metharg, case))
            push!(split_out_sigs, metharg)
        end

        # Now, if profitable union split the atypes into dispatch tuples and match the appropriate method
        nu = countunionsplit(atypes)
        if nu != 1 && nu <= sv.params.MAX_UNION_SPLITTING
            fully_covered = true
            for sig in UnionSplitSignature(atypes)
                metharg′ = argtypes_to_type(sig)
                if !isdispatchtuple(metharg′)
                    fully_covered = false
                    continue
                elseif any(x->x === metharg′, split_out_sigs)
                    continue
                end
                # `meth` is in specificity order, so find the first applicable method
                found_any = false
                for (i, match) in enumerate(meth)
                    (metharg, methsp, method) = (match[1]::Type, match[2]::SimpleVector, match[3]::Method)
                    metharg′ <: method.sig || continue
                    case = analyze_method!(idx, f, ft, metharg′, methsp, method, stmt, atypes, sv, metharg′, isinvoke, isapply, invoke_data)
                    if case !== nothing
                        found_any = true
                        push!(cases, Pair{Type,Any}(metharg′, case))
                    end
                    break
                end
                if !found_any
                    fully_covered = false
                    continue
                end
            end
        end

        # If we're fully covered and there's only one applicable method,
        # we inline, even if the signature is not a dispatch tuple
        if signature_fully_covered && length(cases) == 0 && length(meth) == 1
            metharg = meth[1][1]::Type
            methsp = meth[1][2]::SimpleVector
            method = meth[1][3]::Method
            fully_covered = true
            case = analyze_method!(idx, f, ft, metharg, methsp, method, stmt, atypes, sv, atype_unlimited, isinvoke, isapply, invoke_data)
            case == nothing && continue
            push!(cases, Pair{Type,Any}(metharg, case))
        end

        # If we only have one case and that case is fully covered, we may either
        # be able to do the inlining now (for constant cases), or push it directly
        # onto the todo list
        if fully_covered && length(cases) == 1
            handle_single_case!(ir, stmt, idx, cases[1][2], isinvoke, todo)
            continue
        end
        length(cases) == 0 && continue
        push!(todo, UnionSplit(idx, fully_covered, atype_unlimited, isinvoke, cases))
    end
    todo
end

function mk_tuplecall!(compact::IncrementalCompact, args::Vector{Any}, line_idx::Int)
    e = Expr(:call, TOP_TUPLE, args...)
    e.typ = tuple_tfunc(Tuple{Any[widenconst(compact_exprtype(compact, args[i])) for i in 1:length(args)]...})
    return insert_node_here!(compact, e, e.typ, line_idx)
end

function linear_inline_eligible(ir::IRCode)
    length(ir.cfg.blocks) == 1 || return false
    terminator = ir[SSAValue(last(ir.cfg.blocks[1].stmts))]
    isa(terminator, ReturnNode) || return false
    isdefined(terminator, :val) || return false
    return true
end

function compute_invoke_data(@nospecialize(atypes), argexprs::Vector{Any}, sv::OptimizationState)
    ft = widenconst(atypes[2])
    invoke_tt = widenconst(atypes[3])
    if !(isconcretetype(ft) || ft <: Type) || !isType(invoke_tt) ||
            has_free_typevars(invoke_tt) || has_free_typevars(ft) || (ft <: Builtin)
        # TODO: this can be rather aggressive at preventing inlining of closures
        # XXX: this is wrong for `ft <: Type`, since we are failing to check that
        #      the result doesn't have subtypes, or to do an intersection lookup
        return nothing
    end
    if !(isa(invoke_tt.parameters[1], Type) &&
            invoke_tt.parameters[1] <: Tuple)
        return nothing
    end
    invoke_tt = invoke_tt.parameters[1]
    invoke_types = rewrap_unionall(Tuple{ft, unwrap_unionall(invoke_tt).parameters...}, invoke_tt)
    invoke_entry = ccall(:jl_gf_invoke_lookup, Any, (Any, UInt),
                            invoke_types, sv.params.world)
    invoke_entry === nothing && return nothing
    invoke_data = InvokeData(ft.name.mt, invoke_entry,
                             invoke_types, nothing, nothing)
    atype0 = atypes[2]
    argexpr0 = argexprs[2]
    atypes = atypes[4:end]
    argexprs = argexprs[4:end]
    pushfirst!(atypes, atype0)
    pushfirst!(argexprs, argexpr0)
    f = isdefined(ft, :instance) ? ft.instance : nothing
    return svec(f, ft, atypes, argexprs, invoke_data)
end

function early_inline_special_case(ir::IRCode, @nospecialize(f), @nospecialize(ft), e::Expr, atypes::Vector{Any}, params)
    if (f === typeassert || ft ⊑ typeof(typeassert)) && length(atypes) == 3
        # typeassert(x::S, T) => x, when S<:T
        a3 = atypes[3]
        if (isType(a3) && !has_free_typevars(a3) && atypes[2] ⊑ a3.parameters[1]) ||
            (isa(a3, Const) && isa(a3.val, Type) && atypes[2] ⊑ a3.val)
            return (e.args[2],)
        end
    end
    topmod = _topmod(ir.mod)
    # special-case inliners for known pure functions that compute types
    if params.inlining
        if isa(e.typ, Const) # || isconstType(e.typ)
            val = e.typ.val
            if (f === apply_type || f === fieldtype || f === typeof || f === (===) ||
                f === Core.sizeof || f === isdefined ||
                istopfunction(topmod, f, :typejoin) ||
                istopfunction(topmod, f, :isbits) ||
                istopfunction(topmod, f, :isbitstype) ||
                istopfunction(topmod, f, :promote_type) ||
                (f === Core.kwfunc && length(atypes) == 2) ||
                (is_inlineable_constant(val) &&
                 (contains_is(_PURE_BUILTINS, f) ||
                  (f === getfield && effect_free(e, ir, ir.mod, false)) ||
                  (isa(f, IntrinsicFunction) && is_pure_intrinsic_optim(f)))))
                return (quoted(val),)
            end
        end
    end

    return nothing
end

function late_inline_special_case!(ir::IRCode, idx::Int, stmt::Expr, atypes::Vector{Any}, @nospecialize(f), @nospecialize(ft), topmod::Module)
    if length(atypes) == 3 && istopfunction(topmod, f, :!==)
        # special-case inliner for !== that precedes _methods_by_ftype union splitting
        # and that works, even though inference generally avoids inferring the `!==` Method
        if isa(stmt.typ, Const)
            ir[SSAValue(idx)] = quoted(stmt.typ.val)
            return true
        end
        cmp_call = Expr(:call, GlobalRef(Core, :(===)), stmt.args[2], stmt.args[3])
        cmp_call.typ = Bool
        cmp_call_ssa = insert_node!(ir, idx, Bool, cmp_call)
        not_call = Expr(:call, GlobalRef(Core.Intrinsics, :not_int), cmp_call_ssa)
        not_call.typ = Bool
        ir[SSAValue(idx)] = not_call
        return true
    elseif length(atypes) == 3 && istopfunction(topmod, f, :(>:))
        # special-case inliner for issupertype
        # that works, even though inference generally avoids inferring the `>:` Method
        if isa(stmt.typ, Const)
            ir[SSAValue(idx)] = quoted(stmt.typ.val)
            return true
        end
        subtype_call = Expr(:call, GlobalRef(Core, :(<:)), stmt.args[3], stmt.args[2])
        subtype_call.typ = Bool
        ir[SSAValue(idx)] = subtype_call
        return true
    elseif f === return_type
        if isconstType(stmt.typ)
            ir[SSAValue(idx)] = quoted(stmt.typ.parameters[1])
            return true
        elseif isa(stmt.typ, Const)
            ir[SSAValue(idx)] = quoted(stmt.typ.val)
            return true
        end
    end
    return false
end

function ssa_substitute!(idx::Int, @nospecialize(val), arg_replacements::Vector{Any},
                         @nospecialize(spsig), spvals::Vector{Any},
                         linetable_offset::Int, boundscheck::Symbol, compact::IncrementalCompact)
    compact.result_flags[idx] &= ~IR_FLAG_INBOUNDS
    compact.result_lines[idx] += linetable_offset
    return ssa_substitute_op!(val, arg_replacements, spsig, spvals, boundscheck)
end

function ssa_substitute_op!(@nospecialize(val), arg_replacements::Vector{Any},
                            @nospecialize(spsig), spvals::Vector{Any}, boundscheck::Symbol)
    if isa(val, Argument)
        return arg_replacements[val.n]
    end
    if isa(val, Expr)
        e = val::Expr
        head = e.head
        if head === :static_parameter
            return quoted(spvals[e.args[1]])
        elseif head === :foreigncall
            @assert !isa(spsig, UnionAll) || !isempty(spvals)
            for i = 1:length(e.args)
                if i == 2
                    e.args[2] = ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), e.args[2], spsig, spvals)
                elseif i == 3
                    argtuple = Any[
                        ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), argt, spsig, spvals)
                        for argt
                        in e.args[3] ]
                    e.args[3] = svec(argtuple...)
                end
            end
        elseif head === :boundscheck
            if boundscheck === :off # inbounds == true
                return false
            elseif boundscheck === :propagate
                return e
            else # on or default
                return true
            end
        end
    end
    urs = userefs(val)
    for op in urs
        op[] = ssa_substitute_op!(op[], arg_replacements, spsig, spvals, boundscheck)
    end
    return urs[]
end

function find_inferred(linfo::MethodInstance, @nospecialize(atypes), sv::OptimizationState)
    # see if the method has a InferenceResult in the current cache
    # or an existing inferred code info store in `.inferred`
    haveconst = false
    for i in 1:length(atypes)
        a = atypes[i]
        if isa(a, Const) && !isdefined(typeof(a.val), :instance) && !(isa(a.val, Type) && issingletontype(a.val))
            # have new information from argtypes that wasn't available from the signature
            haveconst = true
            break
        end
    end
    if haveconst
        inf_result = cache_lookup(linfo, atypes, sv.params.cache) # Union{Nothing, InferenceResult}
    else
        inf_result = nothing
    end
    if isa(inf_result, InferenceResult) && isa(inf_result.src, CodeInfo)
        linfo = inf_result.linfo
        result = inf_result.result
        if (inf_result.src::CodeInfo).pure
            if isa(result, Const)
                inferred_const = result.val
            elseif isconstType(result)
                inferred_const = result.parameters[1]
            end
            if @isdefined(inferred_const) && is_inlineable_constant(inferred_const)
                add_backedge!(linfo, sv)
                return (quoted(inferred_const),)
            end
        end
        inferred = inf_result.src
        rettype = widenconst(result)
    elseif isdefined(linfo, :inferred)
        inferred = linfo.inferred
        rettype = linfo.rettype
    else
        rettype = Any
        inferred = nothing
    end
    return (rettype, inferred)
end
