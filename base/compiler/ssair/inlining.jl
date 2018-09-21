# This file is a part of Julia. License is MIT: https://julialang.org/license

struct InvokeData
    mt::Core.MethodTable
    entry::Core.TypeMapEntry
    types0
end

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
    metharg # ::Type
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
    ConstantCase(@nospecialize(val), method::Method, sparams::Vector{Any}, @nospecialize(metharg)) =
        new(val, method, sparams, metharg)
end

struct DynamicCase
    method::Method
    sparams::Vector{Any}
    metharg::Any
    DynamicCase(method::Method, sparams::Vector{Any}, @nospecialize(metharg)) =
        new(method, sparams, metharg)
end

struct UnionSplit
    idx::Int # The statement to replace
    fully_covered::Bool
    atype # ::Type
    isinvoke::Bool
    cases::Vector{Pair{Any, Any}}
    bbs::Vector{Int}
    UnionSplit(idx::Int, fully_covered::Bool, @nospecialize(atype), isinvoke::Bool,
               cases::Vector{Pair{Any, Any}}) =
        new(idx, fully_covered, atype, isinvoke, cases, Int[])
end

function ssa_inlining_pass!(ir::IRCode, linetable::Vector{LineInfoNode}, sv::OptimizationState)
    # Go through the function, performing simple ininlingin (e.g. replacing call by constants
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
    dead_blocks::Vector{Int}
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
        Vector{Int}(),
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
                return old_pred_block == 0 ? 0 : bb_rename_range[old_pred_block]
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

    any_edges = false
    for (old_block, new_block) in enumerate(bb_rename_range)
        if (length(state.new_cfg_blocks[new_block].succs) == 0)
            terminator_idx = last(inlinee_cfg.blocks[old_block].stmts)
            terminator = item.ir[SSAValue(terminator_idx)]
            if isa(terminator, ReturnNode) && isdefined(terminator, :val)
                any_edges = true
                push!(state.new_cfg_blocks[new_block].succs, post_bb_id)
                if need_split
                    push!(state.new_cfg_blocks[post_bb_id].preds, new_block)
                end
            end
        end
    end

    if !any_edges
        push!(state.dead_blocks, post_bb_id)
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

    # Kill dead blocks
    for block in state.dead_blocks
        for succ in state.new_cfg_blocks[block].succs
            kill_edge!(state.new_cfg_blocks, block, succ)
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
    inlined_at = Int(compact.result_lines[idx])
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
    # temporarily re-open in again.
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
                return_value = SSAValue(idx′)
                inline_compact[idx′] = stmt′.val
                val = stmt′.val
                inline_compact.result_types[idx′] = (isa(val, Argument) || isa(val, Expr)) ?
                    compact_exprtype(compact, stmt′.val) :
                    compact_exprtype(inline_compact, stmt′.val)
                break
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
            finish_current_bb!(compact, 0)
        end
        pn = PhiNode()
        #compact[idx] = nothing
        inline_compact = IncrementalCompact(compact, item.ir, compact.result_idx)
        for (idx′, stmt′) in inline_compact
            inline_compact[idx′] = nothing
            stmt′ = ssa_substitute!(idx′, stmt′, argexprs, item.method.sig, item.sparams, linetable_offset, boundscheck_idx, compact)
            if isa(stmt′, ReturnNode)
                if isdefined(stmt′, :val)
                    val = stmt′.val
                    # GlobalRefs can have side effects, but are currently
                    # allowed in arguments of ReturnNodes
                    push!(pn.edges, inline_compact.active_result_bb-1)
                    if isa(val, GlobalRef) || isa(val, Expr)
                        stmt′ = val
                        inline_compact.result_types[idx′] = (isa(val, Argument) || isa(val, Expr)) ?
                            compact_exprtype(compact, val) :
                            compact_exprtype(inline_compact, val)
                        insert_node_here!(inline_compact, GotoNode(post_bb_id),
                                          Any, compact.result_lines[idx′],
                                          true)
                        push!(pn.values, SSAValue(idx′))
                    else
                        push!(pn.values, val)
                        stmt′ = GotoNode(post_bb_id)
                    end

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
            return_value = insert_node_here!(compact, pn, compact_exprtype(compact, SSAValue(idx)), compact.result_lines[idx])
        end
    end
    return_value
end

const fatal_type_bound_error = ErrorException("fatal error in type inference (type bound)")

function ir_inline_unionsplit!(compact::IncrementalCompact, idx::Int,
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
        for i in 1:length(atype.parameters)
            a, m = atype.parameters[i], metharg.parameters[i]
            # If this is always true, we don't need to check for it
            a <: m && continue
            # Generate isa check
            isa_expr = Expr(:call, isa, argexprs[i], m)
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
        finish_current_bb!(compact, 0)
        argexprs′ = argexprs
        if !isa(case, ConstantCase)
            argexprs′ = copy(argexprs)
            for i = 1:length(metharg.parameters)
                a, m = atype.parameters[i], metharg.parameters[i]
                (isa(argexprs[i], SSAValue) || isa(argexprs[i], Argument)) || continue
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
        if !isempty(compact.result_bbs[bb].preds)
            push!(pn.edges, bb)
            push!(pn.values, val)
            insert_node_here!(compact, GotoNode(join_bb), Union{}, line)
        else
            insert_node_here!(compact, ReturnNode(), Union{}, line)
        end
        finish_current_bb!(compact, 0)
    end
    bb += 1
    # We're now in the fall through block, decide what to do
    if item.fully_covered
        e = Expr(:call, GlobalRef(Core, :throw), fatal_type_bound_error)
        insert_node_here!(compact, e, Union{}, line)
        insert_node_here!(compact, ReturnNode(), Union{}, line)
        finish_current_bb!(compact, 0)
    else
        ssa = insert_node_here!(compact, stmt, typ, line)
        push!(pn.edges, bb)
        push!(pn.values, ssa)
        insert_node_here!(compact, GotoNode(join_bb), Union{}, line)
        finish_current_bb!(compact, 0)
    end

    # We're now in the join block.
    compact.ssa_rename[compact.idx-1] = insert_node_here!(compact, pn, typ, line)
    nothing
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

    let compact = IncrementalCompact(ir, false)
        compact.result_bbs = state.new_cfg_blocks
        # This needs to be a minimum and is more of a size hint
        nn = 0
        for item in todo
            if isa(item, InliningTodo)
                nn += (length(item.ir.stmts) + length(item.ir.new_nodes))
            end
        end
        nnewnodes = length(compact.result) + nn
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
                    if isa(aexpr, GlobalRef) || isa(aexpr, Expr)
                        argexprs[aidx] = insert_node_here!(compact, aexpr, compact_exprtype(compact, aexpr), compact.result_lines[idx])
                    end
                end
                if item.isinvoke
                    argexprs = rewrite_invoke_exprargs!((node, typ)->insert_node_here!(compact, node, typ, compact.result_lines[idx]),
                                                argexprs)
                end
                if isa(item, InliningTodo)
                    compact.ssa_rename[compact.idx-1] = ir_inline_item!(compact, idx, argexprs, linetable, item, boundscheck, state.todo_bbs)
                elseif isa(item, UnionSplit)
                    ir_inline_unionsplit!(compact, idx, argexprs, linetable, item, boundscheck, state.todo_bbs)
                end
                compact[idx] = nothing
                refinish && finish_current_bb!(compact, 0)
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

function rewrite_apply_exprargs!(ir::IRCode, idx::Int, argexprs::Vector{Any}, atypes::Vector{Any}, sv::OptimizationState)
    new_argexprs = Any[argexprs[2]]
    new_atypes = Any[atypes[2]]
    # loop over original arguments and flatten any known iterators
    for i in 3:length(argexprs)
        def = argexprs[i]
        # As a special case, if we can see the tuple() call, look at it's arguments to find
        # our types. They can be more precise (e.g. f(Bool, A...) would be lowered as
        # _apply(f, tuple(Bool)::Tuple{DataType}, A), which might not be precise enough to
        # get a good method match). This pattern is used in the array code a bunch.
        if isa(def, SSAValue) && is_tuple_call(ir, ir[def])
            def_args = ir[def].args
            def_atypes = Any[argextype(def_args[i], ir, sv.sp) for i in 2:length(def_args)]
        elseif isa(def, Argument) && def.n === length(ir.argtypes) && !isempty(sv.result_vargs)
            def_atypes = sv.result_vargs
        else
            def_atypes = Any[]
            if isa(atypes[i], Const)
                for p in atypes[i].val
                    push!(def_atypes, Const(p))
                end
            else
                for p in widenconst(atypes[i]).parameters
                    if isa(p, DataType) && isdefined(p, :instance)
                        # replace singleton types with their equivalent Const object
                        p = Const(p.instance)
                    elseif isconstType(p)
                        p = Const(p.parameters[1])
                    end
                    push!(def_atypes, p)
                end
            end
        end
        # now push flattened types into new_atypes and getfield exprs into new_argexprs
        for j in 1:length(def_atypes)
            def_atype = def_atypes[j]
            new_call = Expr(:call, Core.getfield, def, j)
            new_argexpr = insert_node!(ir, idx, def_atype, new_call)
            push!(new_argexprs, new_argexpr)
            push!(new_atypes, def_atype)
        end
    end
    return new_argexprs, new_atypes
end

function rewrite_invoke_exprargs!(inserter, argexprs::Vector{Any})
    argexpr0 = argexprs[2]
    argexprs = argexprs[4:end]
    pushfirst!(argexprs, argexpr0)
    return argexprs
end

function singleton_type(@nospecialize(ft))
    if isa(ft, Const)
        return ft.val
    elseif ft isa DataType && isdefined(ft, :instance)
        return ft.instance
    end
    return nothing
end

function analyze_method!(idx::Int, @nospecialize(f), @nospecialize(ft), @nospecialize(metharg), methsp::SimpleVector,
                         method::Method, stmt::Expr, atypes::Vector{Any}, sv::OptimizationState, @nospecialize(atype_unlimited),
                         isinvoke::Bool, isapply::Bool, invoke_data::Union{InvokeData,Nothing}, @nospecialize(stmttyp))
    methsig = method.sig

    # Check whether this call just evaluates to a constant
    if isa(f, widenconst(ft)) && !isdefined(method, :generator) && method.pure &&
            isa(stmttyp, Const) && stmttyp.actual && is_inlineable_constant(stmttyp.val)
        return ConstantCase(quoted(stmttyp.val), method, Any[methsp...], metharg)
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

    isconst, inferred = find_inferred(linfo, atypes, sv)
    if isconst
        return ConstantCase(inferred, method, Any[methsp...], metharg)
    end
    if inferred === nothing
        return spec_lambda(atype_unlimited, sv, invoke_data)
    end

    src_inferred = ccall(:jl_ast_flag_inferred, Bool, (Any,), inferred)
    src_inlineable = ccall(:jl_ast_flag_inlineable, Bool, (Any,), inferred)

    if !(src_inferred && src_inlineable)
        return spec_lambda(atype_unlimited, sv, invoke_data)
    end

    # At this point we're committed to performing the inlining, add the backedge
    add_backedge!(linfo, sv)

    if isa(inferred, CodeInfo)
        src = inferred
        ast = copy_exprargs(inferred.code)
    else
        src = ccall(:jl_uncompress_ast, Any, (Any, Any), method, inferred::Vector{UInt8})::CodeInfo
        ast = src.code
    end

    @timeit "inline IR inflation" begin
        ir2, inline_linetable = inflate_ir(src, linfo), src.linetable
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
    return UnionSplitSignature(SimpleCartesian(ranges), typs)
end

function iterate(split::UnionSplitSignature, state::Vector{Int}...)
    y = iterate(split.it, state...)
    y === nothing && return nothing
    idxs, state = y
    sig = Any[split.typs[i][j] for (i, j) in enumerate(idxs)]
    return (sig, state)
end

function handle_single_case!(ir::IRCode, stmt::Expr, idx::Int, @nospecialize(case), isinvoke::Bool, todo::Vector{Any}, sv::OptimizationState)
    if isa(case, ConstantCase)
        ir[SSAValue(idx)] = case.val
    elseif isa(case, MethodInstance)
        if isinvoke
            stmt.args = rewrite_invoke_exprargs!(
                (node, typ)->insert_node!(ir, idx, typ, node),
                stmt.args)
        end
        stmt.head = :invoke
        pushfirst!(stmt.args, case)
    elseif case === nothing
        # Do, well, nothing
    else
        push!(todo, case::InliningTodo)
    end
    nothing
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

        ft = argextype(arg1, ir, sv.sp)
        has_free_typevars(ft) && continue
        f = singleton_type(ft)
        f === Core.Intrinsics.llvmcall && continue
        f === Core.Intrinsics.cglobal && continue

        atypes = Vector{Any}(undef, length(stmt.args))
        atypes[1] = ft
        ok = true
        for i = 2:length(stmt.args)
            a = argextype(stmt.args[i], ir, sv.sp)
            (a === Bottom || isvarargtype(a)) && (ok = false; break)
            atypes[i] = a
        end
        ok || continue

        # Check if we match any of the early inliners
        calltype = ir.types[idx]
        res = early_inline_special_case(ir, f, ft, stmt, atypes, sv, calltype)
        if res !== nothing
            ir.stmts[idx] = res
            continue
        end

        if f !== Core.invoke && f !== Core._apply &&
                (isa(f, IntrinsicFunction) || ft ⊑ IntrinsicFunction || isa(f, Builtin) || ft ⊑ Builtin)
            # No inlining for builtins (other than what's handled in the early inliner)
            # TODO: this test is wrong if we start to handle Unions of function types later
            continue
        end

        # Special handling for Core.invoke and Core._apply, which can follow the normal inliner
        # logic with modified inlining target
        isapply = isinvoke = false

        # Handle _apply
        if f === Core._apply
            ft = atypes[2]
            has_free_typevars(ft) && continue
            f = singleton_type(ft)
            # Try to figure out the signature of the function being called
            # and if rewrite_apply_exprargs can deal with this form
            ok = true
            for i = 3:length(atypes)
                typ = widenconst(atypes[i])
                # TODO: We could basically run the iteration protocol here
                if !isa(typ, DataType) || typ.name !== Tuple.name ||
                    isvatuple(typ) || length(typ.parameters) > sv.params.MAX_TUPLE_SPLAT
                    ok = false
                    break
                end
            end
            ok || continue
            isapply = true
            # Independent of whether we can inline, the above analysis allows us to rewrite
            # this apply call to a regular call
            stmt.args, atypes = rewrite_apply_exprargs!(ir, idx, stmt.args, atypes, sv)
        end

        if f !== Core.invoke && (isa(f, IntrinsicFunction) || ft ⊑ IntrinsicFunction || isa(f, Builtin) || ft ⊑ Builtin)
            # TODO: this test is wrong if we start to handle Unions of function types later
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

        atype = argtypes_to_type(atypes)

        # In :invoke, make sure that the arguments we're passing are a subtype of the
        # signature we're invoking.
        (invoke_data === nothing || atype <: invoke_data.types0) || continue

        # Bail out here if inlining is disabled
        sv.params.inlining || continue

        # Special case inliners for regular functions
        if late_inline_special_case!(ir, idx, stmt, atypes, f, ft) || is_return_type(f)
            continue
        end

        # Ok, now figure out what method to call
        if invoke_data !== nothing
            method = invoke_data.entry.func
            (metharg, methsp) = ccall(:jl_type_intersection_with_env, Any, (Any, Any),
                                    atype, method.sig)::SimpleVector
            methsp = methsp::SimpleVector
            result = analyze_method!(idx, f, ft, metharg, methsp, method, stmt, atypes, sv, atype, isinvoke, isapply, invoke_data,
                                     calltype)
            handle_single_case!(ir, stmt, idx, result, isinvoke, todo, sv)
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

        cases = Pair{Any, Any}[]
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
            case = analyze_method!(idx, f, ft, metharg, methsp, method, stmt, atypes, sv, metharg, isinvoke, isapply, invoke_data, calltype)
            if case === nothing
                fully_covered = false
                continue
            end
            push!(cases, Pair{Any,Any}(metharg, case))
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
                elseif _any(x->x === metharg′, split_out_sigs)
                    continue
                end
                # `meth` is in specificity order, so find the first applicable method
                found_any = false
                for (i, match) in enumerate(meth)
                    (metharg, methsp, method) = (match[1]::Type, match[2]::SimpleVector, match[3]::Method)
                    metharg′ <: method.sig || continue
                    case = analyze_method!(idx, f, ft, metharg′, methsp, method, stmt, atypes, sv, metharg′, isinvoke, isapply, invoke_data,
                                           calltype)
                    if case !== nothing
                        found_any = true
                        push!(cases, Pair{Any,Any}(metharg′, case))
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
            case = analyze_method!(idx, f, ft, metharg, methsp, method, stmt, atypes, sv, atype, isinvoke, isapply, invoke_data, calltype)
            case === nothing && continue
            push!(cases, Pair{Any,Any}(metharg, case))
        end

        # If we only have one case and that case is fully covered, we may either
        # be able to do the inlining now (for constant cases), or push it directly
        # onto the todo list
        if fully_covered && length(cases) == 1
            handle_single_case!(ir, stmt, idx, cases[1][2], isinvoke, todo, sv)
            continue
        end
        length(cases) == 0 && continue
        push!(todo, UnionSplit(idx, fully_covered, atype, isinvoke, cases))
    end
    todo
end

function mk_tuplecall!(compact::IncrementalCompact, args::Vector{Any}, line_idx::Int32)
    e = Expr(:call, TOP_TUPLE, args...)
    etyp = tuple_tfunc(Tuple{Any[widenconst(compact_exprtype(compact, args[i])) for i in 1:length(args)]...})
    return insert_node_here!(compact, e, etyp, line_idx)
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
    mt = argument_mt(ft)
    if mt === nothing || !isType(invoke_tt) || has_free_typevars(invoke_tt) ||
            has_free_typevars(ft) || (ft <: Builtin)
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
    invoke_data = InvokeData(mt, invoke_entry, invoke_types)
    atype0 = atypes[2]
    argexpr0 = argexprs[2]
    atypes = atypes[4:end]
    argexprs = argexprs[4:end]
    pushfirst!(atypes, atype0)
    pushfirst!(argexprs, argexpr0)
    f = isdefined(ft, :instance) ? ft.instance : nothing
    return svec(f, ft, atypes, argexprs, invoke_data)
end

# Check for a number of functions known to be pure
function ispuretopfunction(@nospecialize(f))
    return istopfunction(f, :typejoin) ||
        istopfunction(f, :isbits) ||
        istopfunction(f, :isbitstype) ||
        istopfunction(f, :promote_type)
end

function early_inline_special_case(ir::IRCode, @nospecialize(f), @nospecialize(ft), e::Expr, atypes::Vector{Any}, sv::OptimizationState,
                                   @nospecialize(etype))
    if (f === typeassert || ft ⊑ typeof(typeassert)) && length(atypes) == 3
        # typeassert(x::S, T) => x, when S<:T
        a3 = atypes[3]
        if (isType(a3) && !has_free_typevars(a3) && atypes[2] ⊑ a3.parameters[1]) ||
            (isa(a3, Const) && isa(a3.val, Type) && atypes[2] ⊑ a3.val)
            val = e.args[2]
            val === nothing && return QuoteNode(val)
            return val
        end
    end

    if sv.params.inlining
        if isa(etype, Const) # || isconstType(etype)
            val = etype.val
            is_inlineable_constant(val) || return nothing
            if isa(f, IntrinsicFunction)
                if is_pure_intrinsic_infer(f) &&
                    (intrinsic_nothrow(f) || intrinsic_nothrow(f, atypes[2:end]))
                    return quoted(val)
                end
            elseif ispuretopfunction(f) || contains_is(_PURE_BUILTINS, f)
                return quoted(val)
            elseif contains_is(_PURE_OR_ERROR_BUILTINS, f)
                if _builtin_nothrow(f, atypes[2:end], etype)
                    return quoted(val)
                end
            end
        end
    end

    return nothing
end

function late_inline_special_case!(ir::IRCode, idx::Int, stmt::Expr, atypes::Vector{Any}, @nospecialize(f), @nospecialize(ft))
    typ = ir.types[idx]
    if length(atypes) == 3 && istopfunction(f, :!==)
        # special-case inliner for !== that precedes _methods_by_ftype union splitting
        # and that works, even though inference generally avoids inferring the `!==` Method
        if isa(typ, Const)
            ir[SSAValue(idx)] = quoted(typ.val)
            return true
        end
        cmp_call = Expr(:call, GlobalRef(Core, :(===)), stmt.args[2], stmt.args[3])
        cmp_call_ssa = insert_node!(ir, idx, Bool, cmp_call)
        not_call = Expr(:call, GlobalRef(Core.Intrinsics, :not_int), cmp_call_ssa)
        ir[SSAValue(idx)] = not_call
        return true
    elseif length(atypes) == 3 && istopfunction(f, :(>:))
        # special-case inliner for issupertype
        # that works, even though inference generally avoids inferring the `>:` Method
        if isa(typ, Const)
            ir[SSAValue(idx)] = quoted(typ.val)
            return true
        end
        subtype_call = Expr(:call, GlobalRef(Core, :(<:)), stmt.args[3], stmt.args[2])
        ir[SSAValue(idx)] = subtype_call
        return true
    elseif is_return_type(f)
        if isconstType(typ)
            ir[SSAValue(idx)] = quoted(typ.parameters[1])
            return true
        elseif isa(typ, Const)
            ir[SSAValue(idx)] = quoted(typ.val)
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
        elseif head === :cfunction
            @assert !isa(spsig, UnionAll) || !isempty(spvals)
            e.args[3] = ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), e.args[3], spsig, spvals)
            e.args[4] = svec(Any[
                ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), argt, spsig, spvals)
                for argt
                in e.args[4] ]...)
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
    if isa(inf_result, InferenceResult)
        let inferred_src = inf_result.src
            if isa(inferred_src, CodeInfo)
                return svec(false, inferred_src)
            end
            if isa(inferred_src, Const) && is_inlineable_constant(inferred_src.val)
                add_backedge!(linfo, sv)
                return svec(true, quoted(inferred_src.val),)
            end
        end
    end
    if isdefined(linfo, :inferred)
        return svec(false, linfo.inferred)
    end
    return svec(false, nothing)
end
