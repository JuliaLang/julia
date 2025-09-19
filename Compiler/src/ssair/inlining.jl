# This file is a part of Julia. License is MIT: https://julialang.org/license

struct Signature
    f::Any
    ft::Any
    argtypes::Vector{Any}
    Signature(@nospecialize(f), @nospecialize(ft), argtypes::Vector{Any}) = new(f, ft, argtypes)
end

struct InliningTodo
    # The MethodInstance to be inlined
    mi::MethodInstance
    # The IR of the inlinee
    ir::IRCode
    # The SpecInfo for the inlinee
    spec_info::SpecInfo
    # The DebugInfo table for the inlinee
    di::DebugInfo
    # If the function being inlined is a single basic block we can use a
    # simpler inlining algorithm. This flag determines whether that's allowed
    linear_inline_eligible::Bool
    # Effects of the call statement
    effects::Effects
end
function InliningTodo(mi::MethodInstance, ir::IRCode, spec_info::SpecInfo, di::DebugInfo, effects::Effects)
    return InliningTodo(mi, ir, spec_info, di, linear_inline_eligible(ir), effects)
end

struct ConstantCase
    val::Any
    edge::CodeInstance
    ConstantCase(@nospecialize(val), edge::CodeInstance) = new(val, edge)
end

struct SomeCase
    val::Any
    SomeCase(@nospecialize val) = new(val)
end

struct InvokeCase
    invoke::Union{CodeInstance,MethodInstance}
    effects::Effects
    info::CallInfo
end

struct InliningCase
    sig  # Type
    item # Union{InliningTodo, InvokeCase, ConstantCase}
    function InliningCase(@nospecialize(sig), @nospecialize(item))
        @assert isa(item, Union{InliningTodo, InvokeCase, ConstantCase}) "invalid inlining item"
        return new(sig, item)
    end
end

struct UnionSplit
    handled_all_cases::Bool # All possible dispatches are included in the cases
    fully_covered::Bool # All handled cases are fully covering
    atype::DataType
    cases::Vector{InliningCase}
    bbs::Vector{Int}
    UnionSplit(handled_all_cases::Bool, fully_covered::Bool, atype::DataType, cases::Vector{InliningCase}) =
        new(handled_all_cases, fully_covered, atype, cases, Int[])
end

struct InliningEdgeTracker
    edges::Vector{Any}
    InliningEdgeTracker(state::InliningState) = new(state.edges)
end

add_inlining_edge!(et::InliningEdgeTracker, edge::CodeInstance) = add_inlining_edge!(et.edges, edge)
add_inlining_edge!(et::InliningEdgeTracker, edge::MethodInstance) = add_inlining_edge!(et.edges, edge)

function ssa_inlining_pass!(ir::IRCode, state::InliningState, propagate_inbounds::Bool)
    # Go through the function, performing simple inlining (e.g. replacing call by constants
    # and analyzing legality of inlining).
    @zone "CC: ANALYSIS" todo = assemble_inline_todo!(ir, state)
    isempty(todo) && return ir
    # Do the actual inlining for every call we identified
    @zone "CC: EXECUTION" ir = batch_inline!(ir, todo, propagate_inbounds, state.interp)
    return ir
end

mutable struct CFGInliningState
    new_cfg_blocks::Vector{BasicBlock}
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
# all previous blocks have been processed and can be added to the new cfg
function inline_into_block!(state::CFGInliningState, block::Int)
    if state.first_bb != block
        new_range = state.first_bb+1:block
        l = length(state.new_cfg_blocks)
        state.bb_rename[new_range] = (l+1:l+length(new_range))
        append!(state.new_cfg_blocks, (copy(block) for block in state.cfg.blocks[new_range]))
        push!(state.merged_orig_blocks, last(new_range))
    end
    state.first_bb = block
    return
end

function cfg_inline_item!(ir::IRCode, idx::Int, todo::InliningTodo, state::CFGInliningState, from_unionsplit::Bool=false)
    inlinee_cfg = todo.ir.cfg
    # Figure out if we need to split the BB
    need_split_before = false
    need_split = true
    block = block_for_inst(ir, idx)
    inline_into_block!(state, block)

    if length(inlinee_cfg.blocks[1].preds) > 1
        need_split_before = true
    else
        @assert inlinee_cfg.blocks[1].preds[1] == 0
    end
    last_block_idx = last(state.cfg.blocks[block].stmts)
    if false # TODO: ((idx+1) == last_block_idx && isa(ir[SSAValue(last_block_idx)], GotoNode))
        need_split = false
        post_bb_id = -ir[SSAValue(last_block_idx)][:stmt].label
    else
        post_bb_id = length(state.new_cfg_blocks) + length(inlinee_cfg.blocks) + (need_split_before ? 1 : 0)
        need_split = true #!(idx == last_block_idx)
    end

    need_split || delete!(state.merged_orig_blocks, last(new_range))

    push!(state.todo_bbs, (length(state.new_cfg_blocks) - 1 + (need_split_before ? 1 : 0), post_bb_id))

    from_unionsplit || delete!(state.split_targets, length(state.new_cfg_blocks))
    local orig_succs
    need_split && (orig_succs = copy(state.new_cfg_blocks[end].succs))
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

    # Fixup the edges of the newly added blocks
    for (old_block, new_block) in enumerate(bb_rename_range)
        if old_block != 1 || need_split_before
            p = state.new_cfg_blocks[new_block].preds
            let bb_rename_range = bb_rename_range
                map!(p, p) do old_pred_block
                    # the meaning of predecessor 0 depends on the block we encounter it:
                    #   - in the first block, it represents the function entry and so needs to be re-mapped
                    if old_block == 1 && old_pred_block == 0
                        return first(bb_rename_range) - 1
                    end
                    #   - elsewhere, it represents external control-flow from a caught exception which is un-affected by inlining
                    return old_pred_block == 0 ? 0 : bb_rename_range[old_pred_block]
                end
            end
        end
        if new_block != last(new_block_range)
            s = state.new_cfg_blocks[new_block].succs
            let bb_rename_range = bb_rename_range
                map!(s, s) do old_succ_block
                    return bb_rename_range[old_succ_block]
                end
            end
        end
    end

    any_edges = false
    for (old_block, new_block) in enumerate(bb_rename_range)
        if (length(state.new_cfg_blocks[new_block].succs) == 0)
            terminator_idx = last(inlinee_cfg.blocks[old_block].stmts)
            terminator = todo.ir[SSAValue(terminator_idx)][:stmt]
            if isa(terminator, ReturnNode) && isdefined(terminator, :val)
                any_edges = true
                push!(state.new_cfg_blocks[new_block].succs, post_bb_id)
                if need_split
                    push!(state.new_cfg_blocks[post_bb_id].preds, new_block)
                end
            end
        end
    end
    any_edges || push!(state.dead_blocks, post_bb_id)

    return nothing
end

function cfg_inline_unionsplit!(ir::IRCode, idx::Int, union_split::UnionSplit,
                                state::CFGInliningState, params::OptimizationParams)
    (; handled_all_cases, fully_covered, #=atype,=# cases, bbs) = union_split
    inline_into_block!(state, block_for_inst(ir, idx))
    from_bbs = Int[]
    delete!(state.split_targets, length(state.new_cfg_blocks))
    orig_succs = copy(state.new_cfg_blocks[end].succs)
    empty!(state.new_cfg_blocks[end].succs)
    for i in 1:length(cases)
        # The condition gets sunk into the previous block
        # Add a block for the union-split body
        push!(state.new_cfg_blocks, BasicBlock(StmtRange(idx, idx)))
        cond_bb = length(state.new_cfg_blocks)-1
        push!(state.new_cfg_blocks[end].preds, cond_bb)
        push!(state.new_cfg_blocks[cond_bb].succs, cond_bb+1)
        case = cases[i].item
        if isa(case, InliningTodo)
            if !case.linear_inline_eligible
                cfg_inline_item!(ir, idx, case, state, true)
            end
        end
        push!(from_bbs, length(state.new_cfg_blocks))
        if !(i == length(cases) && (handled_all_cases && fully_covered))
            # This block will have the next condition or the final else case
            push!(state.new_cfg_blocks, BasicBlock(StmtRange(idx, idx)))
            push!(state.new_cfg_blocks[cond_bb].succs, length(state.new_cfg_blocks))
            push!(state.new_cfg_blocks[end].preds, cond_bb)
            push!(bbs, length(state.new_cfg_blocks))
        end
    end
    # The edge from the fallback block.
    # NOTE This edge is only required for `!handled_all_cases` and not `!fully_covered`,
    #      since in the latter case we inline `Core.throw_methoderror` into the fallback
    #      block, which is must-throw, making the subsequent code path unreachable.
    !handled_all_cases && push!(from_bbs, length(state.new_cfg_blocks))
    # This block will be the block everyone returns to
    push!(state.new_cfg_blocks, BasicBlock(StmtRange(idx, idx), from_bbs, orig_succs))
    join_bb = length(state.new_cfg_blocks)
    push!(state.split_targets, join_bb)
    push!(bbs, join_bb)
    for bb in from_bbs
        push!(state.new_cfg_blocks[bb].succs, join_bb)
    end
end

function finish_cfg_inline!(state::CFGInliningState)
    new_range = (state.first_bb + 1):length(state.cfg.blocks)
    state.bb_rename[new_range] = let
        l = length(state.new_cfg_blocks)
        l+1:l+length(new_range)
    end
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

# TODO append `inlinee_debuginfo` to inner linetable when `inlined_at[2] ≠ 0`
function ir_inline_linetable!(debuginfo::DebugInfoStream, inlinee_debuginfo::DebugInfo, inlined_at::NTuple{3,Int32})
    # Append the linetable of the inlined function to our edges table
    linetable_offset = 1
    while true
        if linetable_offset > length(debuginfo.edges)
            push!(debuginfo.edges, inlinee_debuginfo)
            break
        elseif debuginfo.edges[linetable_offset] === inlinee_debuginfo
            break
        end
        linetable_offset += 1
    end
    return (inlined_at[1], Int32(linetable_offset), Int32(0))
end

function ir_prepare_inlining!(insert_node!::Inserter, inline_target::Union{IRCode, IncrementalCompact},
                              ir::IRCode, spec_info::SpecInfo, di::DebugInfo, mi::MethodInstance,
                              inlined_at::NTuple{3,Int32}, argexprs::Vector{Any})
    def = mi.def::Method
    debuginfo = inline_target isa IRCode ? inline_target.debuginfo : inline_target.ir.debuginfo
    topline = new_inlined_at = ir_inline_linetable!(debuginfo, di, inlined_at)
    if should_insert_coverage(def.module, di)
        insert_node!(NewInstruction(Expr(:code_coverage_effect), Nothing, topline))
    end
    spvals_ssa = nothing
    if !validate_sparams(mi.sparam_vals)
        # N.B. This works on the caller-side argexprs, (i.e. before the va fixup below)
        spvals_ssa = insert_node!(
            removable_if_unused(NewInstruction(Expr(:call, Core._compute_sparams, def, argexprs...), SimpleVector, topline)))
    end
    if spec_info.isva
        nargs_def = spec_info.nargs
        if nargs_def > 0
            argexprs = fix_va_argexprs!(insert_node!, inline_target, argexprs, nargs_def, topline)
        end
    end
    if def.is_for_opaque_closure
        # Replace the first argument by a load of the capture environment
        argexprs[1] = insert_node!(
            NewInstruction(Expr(:call, GlobalRef(Core, :getfield), argexprs[1], QuoteNode(:captures)),
            ir.argtypes[1], topline))
    end
    return SSASubstitute(mi, argexprs, spvals_ssa, new_inlined_at)
end

function adjust_boundscheck!(inline_compact::IncrementalCompact, idx′::Int, stmt::Expr, boundscheck::Symbol)
    if boundscheck === :off
        isempty(stmt.args) && push!(stmt.args, false)
    elseif boundscheck !== :propagate
        isempty(stmt.args) && push!(stmt.args, true)
    end
    return nothing
end

function ir_inline_item!(compact::IncrementalCompact, idx::Int, argexprs::Vector{Any},
                         item::InliningTodo, boundscheck::Symbol, todo_bbs::Vector{Tuple{Int, Int}})
    # Ok, do the inlining here
    inlined_at = compact.result[idx][:line]
    ssa_substitute = ir_prepare_inlining!(InsertHere(compact), compact, item.ir, item.spec_info, item.di, item.mi, inlined_at, argexprs)
    boundscheck = has_flag(compact.result[idx], IR_FLAG_INBOUNDS) ? :off : boundscheck

    # If the iterator already moved on to the next basic block,
    # temporarily re-open it again.
    local return_value
    # Special case inlining that maintains the current basic block if there's only one BB in the target
    new_new_offset = length(compact.new_new_nodes)
    late_fixup_offset = length(compact.late_fixup)
    if item.linear_inline_eligible
        #compact[idx] = nothing
        inline_compact = IncrementalCompact(compact, item.ir, compact.result_idx)
        @assert isempty(inline_compact.perm) && isempty(inline_compact.pending_perm) "linetable not in canonical form (missing compact call)"
        for ((lineidx, idx′), stmt′) in inline_compact
            # This dance is done to maintain accurate usage counts in the
            # face of rename_arguments! mutating in place - should figure out
            # something better eventually.
            inline_compact[idx′] = nothing
            # alter the line number information for InsertBefore to point to the current instruction in the new linetable
            inline_compact[SSAValue(idx′)][:line] = (ssa_substitute.inlined_at[1], ssa_substitute.inlined_at[2], Int32(lineidx))
            insert_node! = InsertBefore(inline_compact, SSAValue(idx′))
            stmt′ = ssa_substitute_op!(insert_node!, inline_compact[SSAValue(idx′)], stmt′, ssa_substitute)
            if isa(stmt′, ReturnNode)
                val = stmt′.val
                return_value = SSAValue(idx′)
                inline_compact[idx′] = val
                inline_compact.result[idx′][:type] =
                    argextype(val, isa(val, Argument) || isa(val, Expr) ? compact : inline_compact)
                # Everything legal in value position is guaranteed to be effect free in stmt position
                inline_compact.result[idx′][:flag] = IR_FLAGS_REMOVABLE
                break
            elseif isexpr(stmt′, :boundscheck)
                adjust_boundscheck!(inline_compact, idx′, stmt′, boundscheck)
            end
            inline_compact[idx′] = stmt′
        end
        just_fixup!(inline_compact, new_new_offset, late_fixup_offset)
        compact.result_idx = inline_compact.result_idx
    else
        bb_offset, post_bb_id = popfirst!(todo_bbs)
        # This implements the need_split_before flag above
        need_split_before = length(item.ir.cfg.blocks[1].preds) > 1
        if need_split_before
            finish_current_bb!(compact, 0)
        end
        pn = PhiNode()
        #compact[idx] = nothing
        inline_compact = IncrementalCompact(compact, item.ir, compact.result_idx)
        @assert isempty(inline_compact.perm) && isempty(inline_compact.pending_perm) "linetable not in canonical form (missing compact call)"
        for ((lineidx, idx′), stmt′) in inline_compact
            inline_compact[idx′] = nothing
            inline_compact[SSAValue(idx′)][:line] = (ssa_substitute.inlined_at[1], ssa_substitute.inlined_at[2], Int32(lineidx))
            insert_node! = InsertBefore(inline_compact, SSAValue(idx′))
            stmt′ = ssa_substitute_op!(insert_node!, inline_compact[SSAValue(idx′)], stmt′, ssa_substitute)
            if isa(stmt′, ReturnNode)
                if isdefined(stmt′, :val)
                    val = stmt′.val
                    @assert !isa(val, Expr) # GlobalRefs with side-effects are disallowed in value position in IRCode
                    push!(pn.edges, inline_compact.active_result_bb-1)
                    push!(pn.values, val)
                    stmt′ = GotoNode(post_bb_id)
                end
            elseif isa(stmt′, GotoNode)
                stmt′ = GotoNode(stmt′.label + bb_offset)
            elseif isa(stmt′, EnterNode)
                stmt′ = EnterNode(stmt′, stmt′.catch_dest == 0 ? 0 : stmt′.catch_dest + bb_offset)
            elseif isa(stmt′, GotoIfNot)
                stmt′ = GotoIfNot(stmt′.cond, stmt′.dest + bb_offset)
            elseif isa(stmt′, PhiNode)
                stmt′ = PhiNode(Int32[edge+bb_offset for edge in stmt′.edges], stmt′.values)
            elseif isexpr(stmt′, :boundscheck)
                adjust_boundscheck!(inline_compact, idx′, stmt′, boundscheck)
            end
            inline_compact[idx′] = stmt′
        end
        just_fixup!(inline_compact, new_new_offset, late_fixup_offset)
        compact.result_idx = inline_compact.result_idx
        compact.active_result_bb = inline_compact.active_result_bb
        if length(pn.edges) == 1
            return_value = pn.values[1]
        else
            return_value = insert_node_here!(compact,
                NewInstruction(pn, argextype(SSAValue(idx), compact), compact.result[idx][:line]))
        end
    end
    return_value
end

function fix_va_argexprs!(insert_node!::Inserter, inline_target::Union{IRCode, IncrementalCompact},
    argexprs::Vector{Any}, nargs_def::Int, line_idx::NTuple{3,Int32})
    newargexprs = argexprs[1:(nargs_def-1)]
    tuple_call = Expr(:call, TOP_TUPLE)
    tuple_typs = Any[]
    for i in nargs_def:length(argexprs)
        arg = argexprs[i]
        push!(tuple_call.args, arg)
        push!(tuple_typs, argextype(arg, inline_target))
    end
    tuple_typ = tuple_tfunc(SimpleInferenceLattice.instance, tuple_typs)
    tuple_inst = NewInstruction(tuple_call, tuple_typ, line_idx)
    push!(newargexprs, insert_node!(tuple_inst))
    return newargexprs
end

"""
    ir_inline_unionsplit!

The primary purpose of this function is to emulate the dispatch behavior by generating flat
`isa`-checks that correspond to the signatures of union-split dispatch candidates.
These checks allow us to inline the method bodies into respective `isa`-conditional blocks.

Note that two pre-conditions are required for this emulation to work correctly:

1. Ordered Dispatch Candidates

The dispatch candidates must be processed in order of their specificity.
The generated `isa`-checks should reflect this order,
especially since the method signatures may not be concrete.
For instance, with the methods:

    f(x::Int)    = ...
    f(x::Number) = ...
    f(x::Any)    = ...

A correct `isa`-based dispatch emulation for the call site `f(x::Any)` would look like:

    if isa(x, Int)
        [inlined/resolved f(x::Int)]
    elseif isa(x, Number)
        [inlined/resolved f(x::Number)]
    else
        [inlined/resolved f(x::Any)]
    end

`ml_matches` should already sort the matched method candidates correctly,
except in ambiguous cases, which we've already excluded at this state.

2. Type Equality Constraints

Another factor is the type equality constraint imposed by type variables.
Simple `isa`-checks are insufficient to capture the semantics in some cases.
For example, given the following method definition:

    g(x::T, y::T) where T<:Integer = ...

it is _invalid_ to optimize a cal site like `g(x::Any, y::Any)` into:

    if isa(x, Integer) && isa(y, Integer)
        [inlined/resolved g(x::Integer, y::Integer)]
    else
        g(x, y) # fallback dynamic dispatch
    end

since we also need to check that `x` and `y` are equal types.

But, we've already excluded such cases at this point,
mainly by filtering out `case.sig::UnionAll`,
so there is no need to worry about type equality at this point.

In essence, we can process the dispatch candidates sequentially,
assuming their order stays the same post-discovery in `ml_matches`.
"""
function ir_inline_unionsplit!(compact::IncrementalCompact, idx::Int, argexprs::Vector{Any},
                               union_split::UnionSplit, boundscheck::Symbol,
                               todo_bbs::Vector{Tuple{Int,Int}}, interp::AbstractInterpreter)
    (; handled_all_cases, fully_covered, atype, cases, bbs) = union_split
    stmt, typ, line = compact.result[idx][:stmt], compact.result[idx][:type], compact.result[idx][:line]
    join_bb = bbs[end]
    pn = PhiNode()
    local bb = compact.active_result_bb
    ncases = length(cases)
    @assert length(bbs) >= ncases
    for i = 1:ncases
        ithcase = cases[i]
        mtype = ithcase.sig::DataType # checked within `handle_cases!`
        case = ithcase.item
        next_cond_bb = bbs[i]
        cond = true
        nparams = fieldcount(atype)
        @assert nparams == fieldcount(mtype)
        if !(i == ncases && fully_covered && handled_all_cases)
            for i = 1:nparams
                aft, mft = fieldtype(atype, i), fieldtype(mtype, i)
                # If this is always true, we don't need to check for it
                aft <: mft && continue
                # Generate isa check
                isa_expr = Expr(:call, isa, argexprs[i], mft)
                isa_type = isa_tfunc(optimizer_lattice(interp), argextype(argexprs[i], compact), Const(mft))
                ssa = insert_node_here!(compact, NewInstruction(isa_expr, isa_type, line))
                if cond === true
                    cond = ssa
                else
                    and_expr = Expr(:call, and_int, cond, ssa)
                    and_type = and_int_tfunc(optimizer_lattice(interp), argextype(cond, compact), isa_type)
                    cond = insert_node_here!(compact, NewInstruction(and_expr, and_type, line))
                end
            end
            insert_node_here!(compact, NewInstruction(GotoIfNot(cond, next_cond_bb), Any, line))
        end
        bb = next_cond_bb - 1
        finish_current_bb!(compact, 0)
        argexprs′ = argexprs
        if !isa(case, ConstantCase)
            argexprs′ = copy(argexprs)
            for i = 1:nparams
                argex = argexprs[i]
                (isa(argex, SSAValue) || isa(argex, Argument)) || continue
                aft, mft = fieldtype(atype, i), fieldtype(mtype, i)
                if !(aft <: mft)
                    𝕃ₒ = optimizer_lattice(interp)
                    narrowed_type = tmeet(𝕃ₒ, argextype(argex, compact), mft)
                    argexprs′[i] = insert_node_here!(compact,
                        NewInstruction(PiNode(argex, mft), narrowed_type, line))
                end
            end
        end
        if isa(case, InliningTodo)
            val = ir_inline_item!(compact, idx, argexprs′, case, boundscheck, todo_bbs)
        elseif isa(case, InvokeCase)
            invoke_stmt = Expr(:invoke, case.invoke, argexprs′...)
            flag = flags_for_effects(case.effects)
            val = insert_node_here!(compact, NewInstruction(invoke_stmt, typ, case.info, line, flag))
        else
            case = case::ConstantCase
            val = case.val
        end
        if !isempty(compact.cfg_transform.result_bbs[bb].preds)
            push!(pn.edges, bb)
            push!(pn.values, val)
            insert_node_here!(compact,
                NewInstruction(GotoNode(join_bb), Any, line))
        else
            insert_node_here!(compact,
                NewInstruction(ReturnNode(), Union{}, line))
        end
        finish_current_bb!(compact, 0)
    end
    bb += 1
    # We're now in the fall through block, decide what to do
    if !handled_all_cases
        ssa = insert_node_here!(compact, NewInstruction(stmt, typ, line))
        push!(pn.edges, bb)
        push!(pn.values, ssa)
        insert_node_here!(compact, NewInstruction(GotoNode(join_bb), Any, line))
        finish_current_bb!(compact, 0)
    elseif !fully_covered
        insert_node_here!(compact, NewInstruction(Expr(:call, GlobalRef(Core, :throw_methoderror), argexprs...), Union{}, line))
        insert_node_here!(compact, NewInstruction(ReturnNode(), Union{}, line))
        finish_current_bb!(compact, 0)
        ncases == 0 && return insert_node_here!(compact, NewInstruction(nothing, Any, line))
    end
    # We're now in the join block.
    return insert_node_here!(compact, NewInstruction(pn, typ, line))
end

function batch_inline!(ir::IRCode, todo::Vector{Pair{Int,Any}}, propagate_inbounds::Bool, interp::AbstractInterpreter)
    params = OptimizationParams(interp)
    # Compute the new CFG first (modulo statement ranges, which will be computed below)
    state = CFGInliningState(ir)
    for (idx, item) in todo
        if isa(item, UnionSplit)
            cfg_inline_unionsplit!(ir, idx, item, state, params)
        else
            item = item::InliningTodo
            # A linear inline does not modify the CFG
            item.linear_inline_eligible && continue
            cfg_inline_item!(ir, idx, item, state, false)
        end
    end
    finish_cfg_inline!(state)

    boundscheck = propagate_inbounds ? :propagate : :default

    let compact = IncrementalCompact(ir, CFGTransformState!(state.new_cfg_blocks, false))
        # This needs to be a minimum and is more of a size hint
        nn = 0
        for (_, item) in todo
            if isa(item, InliningTodo)
                nn += (length(item.ir.stmts) + length(item.ir.new_nodes))
            end
        end
        nnewnodes = length(compact.result) + nn
        resize!(compact, nnewnodes)
        (inline_idx, item) = popfirst!(todo)
        for ((old_idx, idx), stmt) in compact
            if old_idx == inline_idx
                stmt = stmt::Expr
                if stmt.head === :invoke
                    argexprs = stmt.args[2:end]
                else
                    @assert stmt.head === :call
                    argexprs = copy(stmt.args)
                end
                refinish = false
                if compact.result_idx == first(compact.cfg_transform.result_bbs[compact.active_result_bb].stmts)
                    compact.active_result_bb -= 1
                    refinish = true
                end
                if isa(item, InliningTodo)
                    compact.ssa_rename[old_idx] = ir_inline_item!(compact, idx, argexprs, item, boundscheck, state.todo_bbs)
                elseif isa(item, UnionSplit)
                    compact.ssa_rename[old_idx] = ir_inline_unionsplit!(compact, idx, argexprs, item, boundscheck, state.todo_bbs, interp)
                end
                compact[idx] = nothing
                refinish && finish_current_bb!(compact, 0)
                if !isempty(todo)
                    (inline_idx, item) = popfirst!(todo)
                else
                    inline_idx = -1
                end
            elseif isa(stmt, GotoNode)
                compact[idx] = GotoNode(state.bb_rename[stmt.label])
            elseif isa(stmt, EnterNode)
                compact[idx] = EnterNode(stmt, stmt.catch_dest == 0 ? 0 : state.bb_rename[stmt.catch_dest])
            elseif isa(stmt, GotoIfNot)
                compact[idx] = GotoIfNot(stmt.cond, state.bb_rename[stmt.dest])
            elseif isa(stmt, PhiNode)
                compact[idx] = PhiNode(Int32[edge == length(state.bb_rename) ? length(state.new_cfg_blocks) : state.bb_rename[edge+1]-1 for edge in stmt.edges], stmt.values)
            end
        end

        ir = finish(compact)
    end
    return ir
end

# This assumes the caller has verified that all arguments to the _apply_iterate call are Tuples.
function rewrite_apply_exprargs!(todo::Vector{Pair{Int,Any}},
    ir::IRCode, idx::Int, stmt::Expr, argtypes::Vector{Any},
    arginfos::Vector{MaybeAbstractIterationInfo}, arg_start::Int, istate::InliningState)
    flag = ir.stmts[idx][:flag]
    argexprs = stmt.args
    new_argexprs = Any[argexprs[arg_start]]
    new_argtypes = Any[argtypes[arg_start]]
    # loop over original arguments and flatten any known iterators
    for i in (arg_start+1):length(argexprs)
        def = argexprs[i]
        def_type = argtypes[i]
        thisarginfo = arginfos[i-arg_start]
        if thisarginfo === nothing || !thisarginfo.complete
            if def_type isa PartialStruct
                # def_type.typ <: Tuple is assumed
                def_argtypes = def_type.fields
            else
                def_argtypes = Any[]
                if isa(def_type, Const) # && isa(def_type.val, Union{Tuple, SimpleVector}) is implied
                    for p in def_type.val
                        push!(def_argtypes, Const(p))
                    end
                else
                    ti = widenconst(def_type)::DataType # checked by `is_valid_type_for_apply_rewrite`
                    if ti.name === _NAMEDTUPLE_NAME
                        ti = ti.parameters[2]::DataType # checked by `is_valid_type_for_apply_rewrite`
                    end
                    for p in ti.parameters
                        if issingletontype(p)
                            # replace singleton types with their equivalent Const object
                            p = Const(p.instance)
                        elseif isconstType(p)
                            p = Const(p.parameters[1])
                        end
                        push!(def_argtypes, p)
                    end
                end
            end
            # now push flattened types into new_argtypes and getfield exprs into new_argexprs
            for j in 1:length(def_argtypes)
                def_atype = def_argtypes[j]
                if isa(def_atype, Const) && is_inlineable_constant(def_atype.val)
                    new_argexpr = quoted(def_atype.val)
                else
                    new_call = Expr(:call, GlobalRef(Core, :getfield), def, j)
                    new_argexpr = insert_node!(ir, idx, NewInstruction(new_call, def_atype))
                end
                push!(new_argexprs, new_argexpr)
                push!(new_argtypes, def_atype)
            end
        else
            state = Core.svec()
            for i = 1:length(thisarginfo.each)
                call = thisarginfo.each[i]
                new_stmt = Expr(:call, argexprs[2], def, state...)
                state1 = insert_node!(ir, idx, NewInstruction(new_stmt, call.rt))
                new_sig = call_sig(ir, new_stmt)::Signature
                new_info = call.info
                # See if we can inline this call to `iterate`
                handle_call!(todo, ir, state1.id, new_stmt, new_info, flag, new_sig, istate)
                if i != length(thisarginfo.each)
                    valT = getfield_tfunc(optimizer_lattice(istate.interp), call.rt, Const(1))
                    val_extracted = insert_node!(ir, idx, NewInstruction(
                        Expr(:call, GlobalRef(Core, :getfield), state1, 1),
                        valT))
                    push!(new_argexprs, val_extracted)
                    push!(new_argtypes, valT)
                    state_extracted = insert_node!(ir, idx, NewInstruction(
                        Expr(:call, GlobalRef(Core, :getfield), state1, 2),
                        getfield_tfunc(optimizer_lattice(istate.interp), call.rt, Const(2))))
                    state = Core.svec(state_extracted)
                end
            end
        end
    end
    stmt.args = new_argexprs
    return new_argtypes
end

function compileable_specialization(code::Union{MethodInstance,CodeInstance}, effects::Effects,
    et::InliningEdgeTracker, @nospecialize(info::CallInfo), state::InliningState)
    mi = code isa CodeInstance ? code.def : code
    mi_invoke = mi
    method, atype, sparams = mi.def::Method, mi.specTypes, mi.sparam_vals
    if OptimizationParams(state.interp).compilesig_invokes
        new_atype = get_compileable_sig(method, atype, sparams)
        new_atype === nothing && return nothing
        if atype !== new_atype
            sp_ = ccall(:jl_type_intersection_with_env, Any, (Any, Any), new_atype, method.sig)::SimpleVector
            sparams = sp_[2]::SimpleVector
            mi_invoke = specialize_method(method, new_atype, sparams)
            mi_invoke === nothing && return nothing
            code = mi_invoke
        end
    else
        # If this caller does not want us to optimize calls to use their
        # declared compilesig, then it is also likely they would handle sparams
        # incorrectly if there were any unknown typevars, so we conservatively return nothing
        if any(@nospecialize(t)->isa(t, TypeVar), mi.sparam_vals)
            return nothing
        end
    end
    # prefer using a CodeInstance gotten from the cache, since that is where the invoke target should get compiled to normally
    # TODO: can this code be gotten directly from inference sometimes?
    code = get(code_cache(state), mi_invoke, nothing)
    if !isa(code, CodeInstance)
        #println("missing code for ", mi_invoke, " for ", mi)
        code = mi_invoke
    end
    add_inlining_edge!(et, code) # to the code and edges
    return InvokeCase(code, effects, info)
end

struct InferredResult
    src::Any # CodeInfo or IRCode
    effects::Effects
    edge::CodeInstance
    InferredResult(@nospecialize(src), effects::Effects, edge::CodeInstance) = new(src, effects, edge)
end
@inline function get_cached_result(state::InliningState, mi::MethodInstance)
    code = get(code_cache(state), mi, nothing)
    if code isa CodeInstance
        if use_const_api(code)
            # in this case function can be inlined to a constant
            return ConstantCase(quoted(code.rettype_const), code)
        end
        return code
    end
    return nothing
end
@inline function get_local_result(inf_result::InferenceResult)
    @assert isdefined(inf_result, :ci_as_edge) "InferenceResult without ci_as_edge"
    effects = inf_result.ipo_effects
    if is_foldable_nothrow(effects)
        res = inf_result.result
        if isa(res, Const) && is_inlineable_constant(res.val)
            # use constant calling convention
            return ConstantCase(quoted(res.val), inf_result.ci_as_edge)
        end
    end
    return InferredResult(inf_result.src, effects, inf_result.ci_as_edge)
end

# the general resolver for usual and const-prop'ed calls
function resolve_todo(mi::MethodInstance, result::Union{Nothing,InferenceResult,VolatileInferenceResult},
    @nospecialize(info::CallInfo), flag::UInt32, state::InliningState)
    et = InliningEdgeTracker(state)

    preserve_local_sources = true
    if isa(result, InferenceResult)
        inferred_result = get_local_result(result)
    elseif isa(result, VolatileInferenceResult)
        inferred_result = get_local_result(result.inf_result)
        # volatile inference result can be inlined destructively
        preserve_local_sources = !result.inf_result.is_src_volatile | OptimizationParams(state.interp).preserve_local_sources
    else
        inferred_result = get_cached_result(state, mi)
    end
    if inferred_result isa ConstantCase
        add_inlining_edge!(et, inferred_result.edge)
        return inferred_result
    elseif inferred_result isa InferredResult
        (; src, effects, edge) = inferred_result
    elseif inferred_result isa CodeInstance
        src = ci_get_source(state.interp, inferred_result)
        effects = decode_effects(inferred_result.ipo_purity_bits)
        edge = inferred_result
    else # there is no cached source available for this, but there might be code for the compilation sig
        return compileable_specialization(mi, Effects(), et, info, state)
    end

    # the duplicated check might have been done already within `analyze_method!`, but still
    # we need it here too since we may come here directly using a constant-prop' result
    if !OptimizationParams(state.interp).inlining || is_stmt_noinline(flag)
        return compileable_specialization(edge, effects, et, info, state)
    end

    src_inlining_policy(state.interp, mi, src, info, flag) ||
        return compileable_specialization(edge, effects, et, info, state)

    add_inlining_edge!(et, edge)
    if inferred_result isa CodeInstance
        ir, spec_info, debuginfo = retrieve_ir_for_inlining(inferred_result, src)
    else
        ir, spec_info, debuginfo = retrieve_ir_for_inlining(mi, src, preserve_local_sources)
    end
    return InliningTodo(mi, ir, spec_info, debuginfo, effects)
end

# the special resolver for :invoke-d call
function resolve_todo(mi::MethodInstance, @nospecialize(info::CallInfo), flag::UInt32,
                      state::InliningState)
    if !OptimizationParams(state.interp).inlining || is_stmt_noinline(flag)
        return nothing
    end

    et = InliningEdgeTracker(state)

    cached_result = get_cached_result(state, mi)
    if cached_result isa ConstantCase
        add_inlining_edge!(et, cached_result.edge)
        return cached_result
    elseif cached_result isa CodeInstance
        src = ci_get_source(state.interp, cached_result)
        effects = decode_effects(cached_result.ipo_purity_bits)
    else # there is no cached source available, bail out
        return nothing
    end

    src_inlining_policy(state.interp, mi, src, info, flag) || return nothing
    ir, spec_info, debuginfo = retrieve_ir_for_inlining(cached_result, src)
    add_inlining_edge!(et, cached_result)
    return InliningTodo(mi, ir, spec_info, debuginfo, effects)
end

function validate_sparams(sparams::SimpleVector)
    for i = 1:length(sparams)
        spᵢ = sparams[i]
        (isa(spᵢ, TypeVar) || isvarargtype(spᵢ)) && return false
    end
    return true
end

function may_have_fcalls(m::Method)
    isdefined(m, :source) || return true
    src = m.source
    isa(src, MaybeCompressed) || return true
    return ccall(:jl_ir_flag_has_fcall, Bool, (Any,), src)
end

function analyze_method!(match::MethodMatch, argtypes::Vector{Any},
    @nospecialize(info::CallInfo), flag::UInt32, state::InliningState;
    allow_typevars::Bool,
    volatile_inf_result::Union{Nothing,VolatileInferenceResult}=nothing)
    method = match.method

    # Check that we have the correct number of arguments
    na = Int(method.nargs)
    npassedargs = length(argtypes)
    if na != npassedargs && !(na > 0 && method.isva)
        # we have a method match only because an earlier
        # inference step shortened our call args list, even
        # though we have too many arguments to actually
        # call this function
        return nothing
    end
    if !match.fully_covers
        # type-intersection was not able to give us a simple list of types, so
        # ir_inline_unionsplit won't be able to deal with inlining this
        spec_types = match.spec_types
        if !(spec_types isa DataType && length(spec_types.parameters) == npassedargs &&
             !isvarargtype(spec_types.parameters[end]))
            return nothing
        end
    end

    if !validate_sparams(match.sparams)
        (allow_typevars && !may_have_fcalls(match.method)) || return nothing
    end

    # Get the specialization for this method signature
    # (later we will decide what to do with it)
    mi = specialize_method(match)
    return resolve_todo(mi, volatile_inf_result, info, flag, state)
end

function retrieve_ir_for_inlining(cached_result::CodeInstance, src::String)
    src = _uncompressed_ir(cached_result, src)
    return inflate_ir!(src, cached_result.def), SpecInfo(src), src.debuginfo
end
function retrieve_ir_for_inlining(cached_result::CodeInstance, src::CodeInfo)
    return inflate_ir!(copy(src), cached_result.def), SpecInfo(src), src.debuginfo
end
function retrieve_ir_for_inlining(mi::MethodInstance, src::CodeInfo, preserve_local_sources::Bool)
    if preserve_local_sources
        src = copy(src)
    end
    return inflate_ir!(src, mi), SpecInfo(src), src.debuginfo
end
function retrieve_ir_for_inlining(mi::MethodInstance, ir::IRCode, preserve_local_sources::Bool)
    if preserve_local_sources
        ir = copy(ir)
    end
    # COMBAK this is not correct, we should make `InferenceResult` propagate `SpecInfo`
    spec_info = let m = mi.def::Method
        SpecInfo(Int(m.nargs), m.isva, false, nothing)
    end
    ir.debuginfo.def = mi
    return ir, spec_info, DebugInfo(ir.debuginfo, length(ir.stmts))
end
function retrieve_ir_for_inlining(mi::MethodInstance, opt::OptimizationState, preserve_local_sources::Bool)
    result = opt.optresult
    if result !== nothing
        !result.simplified && simplify_ir!(result)
        return retrieve_ir_for_inlining(mi, result.ir, preserve_local_sources)
    end
    retrieve_ir_for_inlining(mi, opt.src, preserve_local_sources)
end

function handle_single_case!(todo::Vector{Pair{Int,Any}},
    ir::IRCode, idx::Int, stmt::Expr, @nospecialize(case),
    isinvoke::Bool = false)
    if isa(case, ConstantCase)
        ir[SSAValue(idx)][:stmt] = case.val
    elseif isa(case, InvokeCase)
        is_foldable_nothrow(case.effects) && inline_const_if_inlineable!(ir[SSAValue(idx)]) && return nothing
        isinvoke && rewrite_invoke_exprargs!(stmt)
        if stmt.head === :invoke
            stmt.args[1] = case.invoke
        else
            stmt.head = :invoke
            pushfirst!(stmt.args, case.invoke)
        end
        add_flag!(ir[SSAValue(idx)], flags_for_effects(case.effects))
    elseif case === nothing
        # Do, well, nothing
    else
        isinvoke && rewrite_invoke_exprargs!(stmt)
        push!(todo, idx=>(case::InliningTodo))
    end
    return nothing
end

rewrite_invoke_exprargs!(expr::Expr) = (expr.args = invoke_rewrite(expr.args); expr)

function is_valid_type_for_apply_rewrite(@nospecialize(typ), params::OptimizationParams)
    if isa(typ, Const) && (v = typ.val; isa(v, SimpleVector))
        length(v) > params.max_tuple_splat && return false
        for p in v
            is_inlineable_constant(p) || return false
        end
        return true
    end
    typ = widenconst(typ)
    if isa(typ, DataType) && typ.name === _NAMEDTUPLE_NAME
        typ = typ.parameters[2]
        typ = unwraptv(typ)
    end
    isa(typ, DataType) || return false
    if typ.name === Tuple.name
        return !isvatuple(typ) && length(typ.parameters) <= params.max_tuple_splat
    else
        return false
    end
end

function inline_splatnew!(ir::IRCode, idx::Int, stmt::Expr, @nospecialize(rt), state::InliningState)
    𝕃ₒ = optimizer_lattice(state.interp)
    nf = nfields_tfunc(𝕃ₒ, rt)
    if nf isa Const
        eargs = stmt.args
        tup = eargs[2]
        tt = argextype(tup, ir)
        tnf = nfields_tfunc(𝕃ₒ, tt)
        # TODO: hoisting this tnf.val === nf.val check into codegen
        # would enable us to almost always do this transform
        if tnf isa Const && tnf.val === nf.val
            n = tnf.val::Int
            new_argexprs = Any[eargs[1]]
            for j = 1:n
                atype = getfield_tfunc(𝕃ₒ, tt, Const(j))
                new_call = Expr(:call, Core.getfield, tup, j)
                new_argexpr = insert_node!(ir, idx, NewInstruction(new_call, atype))
                push!(new_argexprs, new_argexpr)
            end
            stmt.head = :new
            stmt.args = new_argexprs
        end
    end
    return nothing
end

function call_sig(ir::IRCode, stmt::Expr)
    isempty(stmt.args) && return nothing
    if stmt.head === :call
        offset = 1
    elseif stmt.head === :invoke
        offset = 2
    else
        return nothing
    end
    ft = argextype(stmt.args[offset], ir)
    has_free_typevars(ft) && return nothing
    f = singleton_type(ft)
    f === Core.Intrinsics.llvmcall && return nothing
    f === Core.Intrinsics.cglobal && return nothing
    argtypes = Vector{Any}(undef, length(stmt.args))
    argtypes[1] = ft
    for i = (offset+1):length(stmt.args)
        a = argextype(stmt.args[i], ir)
        (a === Bottom || isvarargtype(a)) && return nothing
        argtypes[i] = a
    end
    return Signature(f, ft, argtypes)
end

function inline_apply!(todo::Vector{Pair{Int,Any}},
    ir::IRCode, idx::Int, stmt::Expr, sig::Signature, state::InliningState)
    while sig.f === Core._apply_iterate
        info = ir.stmts[idx][:info]
        if isa(info, UnionSplitApplyCallInfo)
            if length(info.infos) != 1
                # TODO: Handle union split applies?
                new_info = info = NoCallInfo()
            else
                info = info.infos[1]
                new_info = info.call
            end
        else
            @assert info === NoCallInfo()
            new_info = info = NoCallInfo()
        end
        arg_start = 3
        argtypes = sig.argtypes
        if arg_start > length(argtypes)
            return nothing
        end
        ft = argtypes[arg_start]
        if ft isa Const && ft.val === Core.tuple
            # if one argument is a tuple already, and the rest are empty, we can just return it
            # e.g. rewrite `((t::Tuple)...,)` to `t`
            nonempty_idx = 0
            𝕃ₒ = optimizer_lattice(state.interp)
            for i = (arg_start+1):length(argtypes)
                ti = argtypes[i]
                ⊑(𝕃ₒ, ti, Tuple{}) && continue
                if ⊑(𝕃ₒ, ti, Tuple) && nonempty_idx == 0
                    nonempty_idx = i
                    continue
                end
                nonempty_idx = 0
                break
            end
            if nonempty_idx != 0
                ir[SSAValue(idx)][:stmt] = stmt.args[nonempty_idx]
                return nothing
            end
        end
        # Try to figure out the signature of the function being called
        # and if rewrite_apply_exprargs can deal with this form
        arginfos = MaybeAbstractIterationInfo[]
        for i = (arg_start+1):length(argtypes)
            thisarginfo = nothing
            if !is_valid_type_for_apply_rewrite(argtypes[i], OptimizationParams(state.interp))
                isa(info, ApplyCallInfo) || return nothing
                thisarginfo = info.arginfo[i-arg_start]
                if thisarginfo === nothing || !thisarginfo.complete
                    return nothing
                end
            end
            push!(arginfos, thisarginfo)
        end
        # Independent of whether we can inline, the above analysis allows us to rewrite
        # this apply call to a regular call
        argtypes = rewrite_apply_exprargs!(todo,
            ir, idx, stmt, argtypes, arginfos, arg_start, state)
        ir.stmts[idx][:info] = new_info
        has_free_typevars(ft) && return nothing
        f = singleton_type(ft)
        sig = Signature(f, ft, argtypes)
    end
    sig
end

# TODO: this test is wrong if we start to handle Unions of function types later
function is_builtin(𝕃ₒ::AbstractLattice, s::Signature)
    isa(s.f, IntrinsicFunction) && return true
    ⊑(𝕃ₒ, s.ft, IntrinsicFunction) && return true
    isa(s.f, Builtin) && return true
    ⊑(𝕃ₒ, s.ft, Builtin) && return true
    return false
end

function handle_invoke_call!(todo::Vector{Pair{Int,Any}},
    ir::IRCode, idx::Int, stmt::Expr, @nospecialize(info), flag::UInt32,
    sig::Signature, state::InliningState)
    nspl = nsplit(info)
    nspl == 0 && return nothing # e.g. InvokeCICallInfo
    @assert nspl == 1
    mresult = getsplit(info, 1)
    match = mresult.matches[1]
    if !match.fully_covers
        # TODO: We could union split out the signature check and continue on
        return nothing
    end
    result = getresult(info, 1)
    if isa(result, ConcreteResult)
        item = concrete_result_item(result, info, state)
    elseif isa(result, SemiConcreteResult)
        item = semiconcrete_result_item(result, info, flag, state)
    else
        argtypes = invoke_rewrite(sig.argtypes)
        if isa(result, ConstPropResult)
            mi = result.result.linfo
            validate_sparams(mi.sparam_vals) || return nothing
            if Union{} !== argtypes_to_type(argtypes) <: mi.def.sig
                item = resolve_todo(mi, result.result, info, flag, state)
                handle_single_case!(todo, ir, idx, stmt, item, true)
                return nothing
            end
        end
        volatile_inf_result = result isa VolatileInferenceResult ? result : nothing
        item = analyze_method!(match, argtypes, info, flag, state; allow_typevars=false, volatile_inf_result)
    end
    handle_single_case!(todo, ir, idx, stmt, item, true)
    return nothing
end

function invoke_signature(argtypes::Vector{Any})
    ft, argtyps = widenconst(argtypes[2]), instanceof_tfunc(widenconst(argtypes[3]), false)[1]
    return rewrap_unionall(Tuple{ft, unwrap_unionall(argtyps).parameters...}, argtyps)
end

function narrow_opaque_closure!(ir::IRCode, stmt::Expr, @nospecialize(info::CallInfo), state::InliningState)
    if isa(info, OpaqueClosureCreateInfo)
        lbt = argextype(stmt.args[2], ir)
        lb, exact = instanceof_tfunc(lbt)
        exact || return
        ubt = argextype(stmt.args[3], ir)
        ub, exact = instanceof_tfunc(ubt)
        exact || return
        # Narrow opaque closure type
        𝕃ₒ = optimizer_lattice(state.interp)
        newT = widenconst(tmeet(𝕃ₒ, tmerge(𝕃ₒ, lb, info.unspec.rt), ub))
        if newT != ub
            # N.B.: Narrowing the ub requires a backedge on the mi whose type
            # information we're using, since a change in that function may
            # invalidate ub result.
            stmt.args[3] = newT
        end
    end
end

# As a matter of convenience, this pass also computes effect-freenes.
# For primitives, we do that right here. For proper calls, we will
# discover this when we consult the caches.
add_inst_flag!(inst::Instruction, ir::IRCode, state::InliningState) =
    add_inst_flag!(inst, ir, optimizer_lattice(state.interp))
function add_inst_flag!(inst::Instruction, ir::IRCode, 𝕃ₒ::AbstractLattice)
    flags = recompute_effects_flags(𝕃ₒ, inst[:stmt], inst[:type], ir)
    add_flag!(inst, flags)
    return !iszero(flags & IR_FLAGS_REMOVABLE)
end

# Handles all analysis and inlining of intrinsics and builtins. In particular,
# this method does not access the method table or otherwise process generic
# functions.
function process_simple!(todo::Vector{Pair{Int,Any}}, ir::IRCode, idx::Int, flag::UInt32,
                         state::InliningState)
    inst = ir[SSAValue(idx)]
    stmt = inst[:stmt]
    if !(stmt isa Expr)
        add_inst_flag!(inst, ir, state)
        return nothing
    end
    rt = inst[:type]
    head = stmt.head
    if head !== :call
        if head === :splatnew
            inline_splatnew!(ir, idx, stmt, rt, state)
        elseif head === :new_opaque_closure
            narrow_opaque_closure!(ir, stmt, inst[:info], state)
        elseif head === :invoke
            sig = call_sig(ir, stmt)
            sig === nothing && return nothing
            return stmt, sig
        end
        add_inst_flag!(inst, ir, state)
        return nothing
    end

    sig = call_sig(ir, stmt)
    sig === nothing && return nothing

    # Handle _apply_iterate
    sig = inline_apply!(todo, ir, idx, stmt, sig, state)
    sig === nothing && return nothing

    # Check if we match any of the early inliners
    earlyres = early_inline_special_case(ir, stmt, flag, rt, sig, state)
    if isa(earlyres, SomeCase)
        inst[:stmt] = earlyres.val
        return nothing
    end

    if add_inst_flag!(inst, ir, state)
        if sig.f === typeassert || ⊑(optimizer_lattice(state.interp), sig.ft, typeof(typeassert))
            # typeassert is a no-op if effect free
            inst[:stmt] = stmt.args[2]
            return nothing
        end
    end

    if is_builtin(optimizer_lattice(state.interp), sig)
        let f = sig.f
            if (f !== Core.invoke &&
                f !== Core.finalizer &&
                f !== modifyfield! &&
                f !== Core.modifyglobal! &&
                f !== Core.memoryrefmodify! &&
                f !== atomic_pointermodify)
                # No inlining defined for most builtins (just invoke/apply/typeassert/finalizer), so attempt an early exit for them
                return nothing
            end
        end
    end

    # Special case inliners for regular functions
    lateres = late_inline_special_case!(ir, idx, stmt, flag, rt, sig, state)
    if isa(lateres, SomeCase)
        inst[:stmt] = lateres.val
        add_inst_flag!(inst, ir, state)
        return nothing
    end

    return stmt, sig
end

function handle_any_const_result!(cases::Vector{InliningCase},
    @nospecialize(result), match::MethodMatch, argtypes::Vector{Any},
    @nospecialize(info::CallInfo), flag::UInt32, state::InliningState;
    allow_typevars::Bool)
    if isa(result, ConcreteResult)
        return handle_concrete_result!(cases, result, match, info, state)
    elseif isa(result, SemiConcreteResult)
        return handle_semi_concrete_result!(cases, result, match, info, flag, state)
    elseif isa(result, ConstPropResult)
        return handle_const_prop_result!(cases, result, match, info, flag, state; allow_typevars)
    else
        @assert result === nothing || result isa VolatileInferenceResult
        return handle_match!(cases, match, argtypes, info, flag, state; allow_typevars, volatile_inf_result = result)
    end
end

function info_effects(@nospecialize(result), match::MethodMatch, state::InliningState)
    if isa(result, ConcreteResult)
        return result.effects
    elseif isa(result, SemiConcreteResult)
        return result.effects
    elseif isa(result, ConstPropResult)
        return result.result.ipo_effects
    else
        mi = specialize_method(match; preexisting=true)
        if isa(mi, MethodInstance)
            code = get(code_cache(state), mi, nothing)
            if code isa CodeInstance
                return decode_effects(code.ipo_purity_bits)
            end
        end
        return Effects()
    end
end

function compute_inlining_cases(@nospecialize(info::CallInfo), flag::UInt32, sig::Signature,
    state::InliningState)
    nunion = nsplit(info)
    nunion === nothing && return nothing
    cases = InliningCase[]
    argtypes = sig.argtypes
    local handled_all_cases = local fully_covered = true
    local revisit_idx = nothing
    local all_result_count = 0
    local joint_effects = EFFECTS_TOTAL
    for i = 1:nunion
        meth = getsplit(info, i)
        if meth.ambig
            # Too many applicable methods
            # Or there is a (partial?) ambiguity
            return nothing
        end
        local split_fully_covered = false
        for (j, match) in enumerate(meth)
            all_result_count += 1
            result = getresult(info, all_result_count)
            joint_effects = merge_effects(joint_effects, info_effects(result, match, state))
            split_fully_covered |= match.fully_covers
            if !validate_sparams(match.sparams)
                if match.fully_covers
                    if revisit_idx === nothing
                        revisit_idx = (i, j, all_result_count)
                    else
                        handled_all_cases = false
                        revisit_idx = nothing
                    end
                else
                    handled_all_cases = false
                end
            elseif !(match.spec_types <: match.method.sig) # the requirement for correct union-split
                handled_all_cases = false
            else
                handled_all_cases &= handle_any_const_result!(cases,
                    result, match, argtypes, info, flag, state; allow_typevars=false)
            end
        end
        fully_covered &= split_fully_covered
    end

    (handled_all_cases & fully_covered) || (joint_effects = Effects(joint_effects; nothrow=false))

    if handled_all_cases
        if revisit_idx !== nothing
            # we handled everything except one match with unmatched sparams,
            # so try to handle it by bypassing validate_sparams
            (i, j, k) = revisit_idx
            match = getsplit(info, i)[j]
            result = getresult(info, k)
            handled_all_cases &= handle_any_const_result!(cases,
                result, match, argtypes, info, flag, state; allow_typevars=true)
        end
        if !fully_covered
            # We will emit an inline MethodError in this case, but that info already came inference, so we must already have the uncovered edge for it
        end
    elseif !isempty(cases)
        # if we've not seen all candidates, union split is valid only for dispatch tuples
        filter!(case::InliningCase->isdispatchtuple(case.sig), cases)
    end
    return cases, handled_all_cases, fully_covered, joint_effects
end

function handle_call!(todo::Vector{Pair{Int,Any}},
    ir::IRCode, idx::Int, stmt::Expr, @nospecialize(info::CallInfo), flag::UInt32, sig::Signature,
    state::InliningState)
    cases = compute_inlining_cases(info, flag, sig, state)
    cases === nothing && return nothing
    cases, handled_all_cases, fully_covered, joint_effects = cases
    atype = argtypes_to_type(sig.argtypes)
    atype === Union{} && return nothing # accidentally actually unreachable
    handle_cases!(todo, ir, idx, stmt, atype, cases, handled_all_cases, fully_covered, joint_effects)
end

function handle_match!(cases::Vector{InliningCase},
    match::MethodMatch, argtypes::Vector{Any}, @nospecialize(info::CallInfo), flag::UInt32,
    state::InliningState;
    allow_typevars::Bool, volatile_inf_result::Union{Nothing,VolatileInferenceResult})
    # We may see duplicated dispatch signatures here when a signature gets widened
    # during abstract interpretation: for the purpose of inlining, we can just skip
    # processing this dispatch candidate (unless unmatched type parameters are present)
    !allow_typevars && any(case::InliningCase->case.sig === match.spec_types, cases) && return true
    item = analyze_method!(match, argtypes, info, flag, state; allow_typevars, volatile_inf_result)
    item === nothing && return false
    push!(cases, InliningCase(match.spec_types, item))
    return true
end

function handle_const_prop_result!(cases::Vector{InliningCase}, result::ConstPropResult,
    match::MethodMatch, @nospecialize(info::CallInfo), flag::UInt32, state::InliningState;
    allow_typevars::Bool)
    mi = result.result.linfo
    if !validate_sparams(mi.sparam_vals)
        (allow_typevars && !may_have_fcalls(mi.def::Method)) || return false
    end
    item = resolve_todo(mi, result.result, info, flag, state)
    item === nothing && return false
    push!(cases, InliningCase(match.spec_types, item))
    return true
end

function semiconcrete_result_item(result::SemiConcreteResult,
        @nospecialize(info::CallInfo), flag::UInt32, state::InliningState)
    code = result.edge
    mi = get_ci_mi(code)
    et = InliningEdgeTracker(state)

    if (!OptimizationParams(state.interp).inlining || is_stmt_noinline(flag) ||
        # For `NativeInterpreter`, `SemiConcreteResult` may be produced for
        # a `@noinline`-declared method when it's marked as `@constprop :aggressive`.
        # Suppress the inlining here (unless inlining is requested at the callsite).
        (is_declared_noinline(mi.def::Method) && !is_stmt_inline(flag)))
        return compileable_specialization(code, result.effects, et, info, state)
    end
    src_inlining_policy(state.interp, mi, result.ir, info, flag) ||
        return compileable_specialization(code, result.effects, et, info, state)

    add_inlining_edge!(et, result.edge)
    preserve_local_sources = OptimizationParams(state.interp).preserve_local_sources
    ir, _, debuginfo = retrieve_ir_for_inlining(mi, result.ir, preserve_local_sources)
    return InliningTodo(mi, ir, result.spec_info, debuginfo, result.effects)
end

function handle_semi_concrete_result!(cases::Vector{InliningCase}, result::SemiConcreteResult,
    match::MethodMatch, @nospecialize(info::CallInfo), flag::UInt32, state::InliningState)
    mi = result.edge.def
    validate_sparams(mi.sparam_vals) || return false
    item = semiconcrete_result_item(result, info, flag, state)
    item === nothing && return false
    push!(cases, InliningCase(match.spec_types, item))
    return true
end

function handle_concrete_result!(cases::Vector{InliningCase}, result::ConcreteResult,
    match::MethodMatch, @nospecialize(info::CallInfo), state::InliningState)
    case = concrete_result_item(result, info, state)
    case === nothing && return false
    push!(cases, InliningCase(match.spec_types, case))
    return true
end

may_inline_concrete_result(result::ConcreteResult) =
    isdefined(result, :result) && is_inlineable_constant(result.result)

function concrete_result_item(result::ConcreteResult, @nospecialize(info::CallInfo), state::InliningState)
    if !may_inline_concrete_result(result)
        et = InliningEdgeTracker(state)
        return compileable_specialization(result.edge, result.effects, et, info, state)
    end
    @assert result.effects === EFFECTS_TOTAL
    return ConstantCase(quoted(result.result), result.edge)
end

function handle_cases!(todo::Vector{Pair{Int,Any}}, ir::IRCode, idx::Int, stmt::Expr,
    @nospecialize(atype), cases::Vector{InliningCase}, handled_all_cases::Bool, fully_covered::Bool,
    joint_effects::Effects)
    # If we only have one case and that case is fully covered, we may either
    # be able to do the inlining now (for constant cases), or push it directly
    # onto the todo list
    if fully_covered && handled_all_cases && length(cases) == 1
        handle_single_case!(todo, ir, idx, stmt, cases[1].item)
    elseif length(cases) > 0 || handled_all_cases
        isa(atype, DataType) || return nothing
        for case in cases
            isa(case.sig, DataType) || return nothing
        end
        push!(todo, idx=>UnionSplit(handled_all_cases, fully_covered, atype, cases))
    else
        add_flag!(ir[SSAValue(idx)], flags_for_effects(joint_effects))
    end
    return nothing
end

function handle_opaque_closure_call!(todo::Vector{Pair{Int,Any}},
    ir::IRCode, idx::Int, stmt::Expr, info::OpaqueClosureCallInfo,
    flag::UInt32, sig::Signature, state::InliningState)
    result = info.result
    if isa(result, ConstPropResult)
        mi = result.result.linfo
        validate_sparams(mi.sparam_vals) || return nothing
        item = resolve_todo(mi, result.result, info, flag, state)
    elseif isa(result, ConcreteResult)
        item = concrete_result_item(result, info, state)
    elseif isa(result, SemiConcreteResult)
        item = item = semiconcrete_result_item(result, info, flag, state)
    else
        @assert result === nothing || result isa VolatileInferenceResult
        volatile_inf_result = result
        item = analyze_method!(info.match, sig.argtypes, info, flag, state; allow_typevars=false, volatile_inf_result)
    end
    handle_single_case!(todo, ir, idx, stmt, item)
    return nothing
end

function handle_modifyop!_call!(ir::IRCode, idx::Int, stmt::Expr, info::ModifyOpInfo, state::InliningState)
    info = info.info
    info isa MethodResultPure && (info = info.info)
    info isa ConstCallInfo && (info = info.call)
    info isa MethodMatchInfo || return nothing
    length(info.edges) == length(info.results) == 1 || return nothing
    match = info.results[1]::MethodMatch
    match.fully_covers || return nothing
    edge = info.edges[1]
    edge === nothing && return nothing
    case = compileable_specialization(edge, Effects(), InliningEdgeTracker(state), info, state)
    case === nothing && return nothing
    stmt.head = :invoke_modify
    pushfirst!(stmt.args, case.invoke)
    ir[SSAValue(idx)][:stmt] = stmt
    return nothing
end

function handle_finalizer_call!(ir::IRCode, idx::Int, stmt::Expr, info::FinalizerInfo,
                                state::InliningState)
    # Finalizers don't return values, so if their execution is not observable,
    # we can just not register them
    if is_removable_if_unused(info.effects)
        ir[SSAValue(idx)] = nothing
        return nothing
    end

    # Only inline finalizers that are known nothrow and notls.
    # This avoids having to set up state for finalizer isolation
    is_finalizer_inlineable(info.effects) || return nothing

    ft = argextype(stmt.args[2], ir)
    has_free_typevars(ft) && return nothing
    f = singleton_type(ft)
    argtypes = Vector{Any}(undef, 2)
    argtypes[1] = ft
    argtypes[2] = argextype(stmt.args[3], ir)
    sig = Signature(f, ft, argtypes)

    cases = compute_inlining_cases(info.info, #=flag=#UInt32(0), sig, state)
    cases === nothing && return nothing
    cases, all_covered, _ = cases
    if all_covered && length(cases) == 1
        # NOTE we don't append `item1` to `stmt` here so that we don't serialize
        # `Core.Compiler` data structure into the global cache
        item1 = cases[1].item
        if isa(item1, InliningTodo)
            code = get(code_cache(state), item1.mi, nothing) # COMBAK: this seems like a bad design, can we use stmt_info instead to store the correct info?
            if code isa CodeInstance
                push!(stmt.args, true)
                push!(stmt.args, code)
            end
        elseif isa(item1, InvokeCase)
            push!(stmt.args, false)
            push!(stmt.args, item1.invoke)
        elseif isa(item1, ConstantCase)
            push!(stmt.args, nothing)
        end
    end
    return nothing
end

function handle_invoke_expr!(todo::Vector{Pair{Int,Any}}, ir::IRCode,
    idx::Int, stmt::Expr, @nospecialize(info::CallInfo), flag::UInt32, sig::Signature, state::InliningState)
    edge = stmt.args[1]
    mi = isa(edge, MethodInstance) ? edge : get_ci_mi(edge::CodeInstance)
    case = resolve_todo(mi, info, flag, state)
    handle_single_case!(todo, ir, idx, stmt, case, false)
    return nothing
end

function inline_const_if_inlineable!(inst::Instruction)
    rt = inst[:type]
    if rt isa Const && is_inlineable_constant(rt.val)
        inst[:stmt] = quoted(rt.val)
        return true
    end
    add_flag!(inst, IR_FLAGS_REMOVABLE)
    return false
end

function assemble_inline_todo!(ir::IRCode, state::InliningState)
    todo = Pair{Int, Any}[]

    for idx in 1:length(ir.stmts)
        inst = ir.stmts[idx]
        flag = inst[:flag]

        simpleres = process_simple!(todo, ir, idx, flag, state)
        simpleres === nothing && continue
        stmt, sig = simpleres
        info = inst[:info]

        # `NativeInterpreter` won't need this, but provide a support for `:invoke` exprs here
        # for external `AbstractInterpreter`s that may run the inlining pass multiple times
        if isexpr(stmt, :invoke)
            handle_invoke_expr!(todo, ir, idx, stmt, info, flag, sig, state)
            continue
        end

        # Check whether this call was @pure and evaluates to a constant
        if info isa MethodResultPure
            inline_const_if_inlineable!(ir[SSAValue(idx)]) && continue
            info = info.info
        end
        if info === NoCallInfo()
            # Inference determined this couldn't be analyzed. Don't question it.
            continue
        end

        # handle special cased builtins
        if isa(info, OpaqueClosureCallInfo)
            handle_opaque_closure_call!(todo, ir, idx, stmt, info, flag, sig, state)
        elseif isa(info, ModifyOpInfo)
            handle_modifyop!_call!(ir, idx, stmt, info, state)
        elseif sig.f === Core.invoke
            handle_invoke_call!(todo, ir, idx, stmt, info, flag, sig, state)
        elseif isa(info, FinalizerInfo)
            handle_finalizer_call!(ir, idx, stmt, info, state)
        else
            # cascade to the generic (and extendable) handler
            handle_call!(todo, ir, idx, stmt, info, flag, sig, state)
        end
    end

    return todo
end

function linear_inline_eligible(ir::IRCode)
    length(ir.cfg.blocks) == 1 || return false
    terminator = ir[SSAValue(last(ir.cfg.blocks[1].stmts))][:stmt]
    isa(terminator, ReturnNode) || return false
    isdefined(terminator, :val) || return false
    return true
end

function early_inline_special_case(ir::IRCode, stmt::Expr, flag::UInt32,
                                   @nospecialize(type), sig::Signature, state::InliningState)
    OptimizationParams(state.interp).inlining || return nothing
    (; f, ft, argtypes) = sig

    if isa(type, Const) # || isconstType(type)
        val = type.val
        is_inlineable_constant(val) || return nothing
        if isa(f, IntrinsicFunction)
            if is_pure_intrinsic_infer(f) && has_flag(flag, IR_FLAG_NOTHROW)
                return SomeCase(quoted(val))
            end
        elseif contains_is(_PURE_BUILTINS, f)
            return SomeCase(quoted(val))
        elseif contains_is(_EFFECT_FREE_BUILTINS, f)
            if has_flag(flag, IR_FLAG_NOTHROW)
                return SomeCase(quoted(val))
            end
        end
    end
    if f === compilerbarrier
        # check if this `compilerbarrier` has already imposed a barrier on abstract interpretation
        # so that it can be eliminated here
        length(argtypes) == 3 || return nothing
        setting = argtypes[2]
        isa(setting, Const) || return nothing
        setting = setting.val
        isa(setting, Symbol) || return nothing
        # setting === :const || setting === :type barrier const evaluation,
        # so they can't be eliminated at IPO time
        setting === :conditional || return nothing
        # barriered successfully already, eliminate it
        return SomeCase(stmt.args[3])
    elseif f === Core.ifelse && length(argtypes) == 4
        cond = argtypes[2]
        if isa(cond, Const)
            if cond.val === true
                return SomeCase(stmt.args[3])
            elseif cond.val === false
                return SomeCase(stmt.args[4])
            end
        elseif ⊑(optimizer_lattice(state.interp), cond, Bool) && stmt.args[3] === stmt.args[4]
            return SomeCase(stmt.args[3])
        end
    end
    return nothing
end

# special-case some regular method calls whose results are not folded within `abstract_call_known`
# (and thus `early_inline_special_case` doesn't handle them yet)
# NOTE we manually inline the method bodies, and so the logic here needs to precisely sync with their definitions
function late_inline_special_case!(ir::IRCode, idx::Int, stmt::Expr, flag::UInt32,
                                   @nospecialize(type), sig::Signature, state::InliningState)
    OptimizationParams(state.interp).inlining || return nothing
    (; f, ft, argtypes) = sig
    if length(argtypes) == 3 && f === Core.:(!==)
        # special-case inliner for !== that precedes _methods_by_ftype union splitting
        # and that works, even though inference generally avoids inferring the `!==` Method
        if isa(type, Const)
            return SomeCase(quoted(type.val))
        end
        cmp_call = Expr(:call, GlobalRef(Core, :(===)), stmt.args[2], stmt.args[3])
        cmp_call_ssa = insert_node!(ir, idx, removable_if_unused(NewInstruction(cmp_call, Bool)))
        not_call = Expr(:call, GlobalRef(Core.Intrinsics, :not_int), cmp_call_ssa)
        return SomeCase(not_call)
    elseif length(argtypes) == 3 && f === Core.:(>:)
        # special-case inliner for issupertype
        # that works, even though inference generally avoids inferring the `>:` Method
        if isa(type, Const) && has_flag(flag, IR_FLAG_NOTHROW)
            return SomeCase(quoted(type.val))
        end
        subtype_call = Expr(:call, GlobalRef(Core, :(<:)), stmt.args[3], stmt.args[2])
        return SomeCase(subtype_call)
    elseif f === TypeVar && 2 <= length(argtypes) <= 4 && ⊑(optimizer_lattice(state.interp), argtypes[2], Symbol)
        typevar_call = Expr(:call, GlobalRef(Core, :_typevar), stmt.args[2],
            length(stmt.args) < 4 ? Bottom : stmt.args[3],
            length(stmt.args) == 2 ? Any : stmt.args[end])
        return SomeCase(typevar_call)
    elseif f === UnionAll && length(argtypes) == 3 && ⊑(optimizer_lattice(state.interp), argtypes[2], TypeVar)
        unionall_call = Expr(:foreigncall, Expr(:tuple, QuoteNode(:jl_type_unionall)), Any, svec(Any, Any),
            0, QuoteNode(:ccall), stmt.args[2], stmt.args[3])
        return SomeCase(unionall_call)
    elseif is_return_type(f)
        if isconstType(type)
            return SomeCase(quoted(type.parameters[1]))
        elseif isa(type, Const)
            return SomeCase(quoted(type.val))
        end
    end
    return nothing
end

struct SSASubstitute
    mi::MethodInstance
    arg_replacements::Vector{Any}
    spvals_ssa::Union{Nothing,SSAValue}
    inlined_at::NTuple{3,Int32} # TODO: add a map also, so that ssaidx doesn't need to equal inlined_idx?
end

function insert_spval!(insert_node!::Inserter, spvals_ssa::SSAValue, spidx::Int, do_isdefined::Bool)
    ret = insert_node!(
        removable_if_unused(NewInstruction(Expr(:call, Core._svec_ref, spvals_ssa, spidx), Any)))
    tcheck_not = nothing
    if do_isdefined
        tcheck = insert_node!(
            removable_if_unused(NewInstruction(Expr(:call, Core.isa, ret, Core.TypeVar), Bool)))
        tcheck_not = insert_node!(
            removable_if_unused(NewInstruction(Expr(:call, not_int, tcheck), Bool)))
    end
    return (ret, tcheck_not)
end

function ssa_substitute_op!(insert_node!::Inserter, subst_inst::Instruction, @nospecialize(val),
                            ssa_substitute::SSASubstitute)
    if isa(val, Argument)
        return ssa_substitute.arg_replacements[val.n]
    end
    if isa(val, Expr)
        e = val::Expr
        head = e.head
        sparam_vals = ssa_substitute.mi.sparam_vals
        if head === :static_parameter
            spidx = e.args[1]::Int
            val = sparam_vals[spidx]
            if !isa(val, TypeVar) && val !== Vararg
                return quoted(val)
            else
                flag = subst_inst[:flag]
                maybe_undef = !has_flag(flag, IR_FLAG_NOTHROW) && isa(val, TypeVar)
                (ret, tcheck_not) = insert_spval!(insert_node!, ssa_substitute.spvals_ssa::SSAValue, spidx, maybe_undef)
                if maybe_undef
                    insert_node!(
                        NewInstruction(Expr(:throw_undef_if_not, val.name, tcheck_not), Nothing))
                end
                return ret
            end
        elseif head === :isdefined && isa(e.args[1], Expr) && e.args[1].head === :static_parameter
            spidx = (e.args[1]::Expr).args[1]::Int
            val = sparam_vals[spidx]
            if !isa(val, TypeVar)
                return true
            else
                (_, tcheck_not) = insert_spval!(insert_node!, ssa_substitute.spvals_ssa::SSAValue, spidx, true)
                return tcheck_not
            end
        elseif head === :cfunction && ssa_substitute.spvals_ssa === nothing
            msig = (ssa_substitute.mi.def::Method).sig
            @assert !isa(msig, UnionAll) || !isempty(sparam_vals)
            e.args[3] = ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), e.args[3], msig, sparam_vals)
            e.args[4] = svec(Any[
                ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), argt, msig, sparam_vals)
                for argt in e.args[4]::SimpleVector ]...)
        elseif head === :foreigncall && ssa_substitute.spvals_ssa === nothing
            msig = (ssa_substitute.mi.def::Method).sig
            @assert !isa(msig, UnionAll) || !isempty(sparam_vals)
            for i = 1:length(e.args)
                if i == 2
                    e.args[2] = ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), e.args[2], msig, sparam_vals)
                elseif i == 3
                    e.args[3] = svec(Any[
                        ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), argt, msig, sparam_vals)
                        for argt in e.args[3]::SimpleVector ]...)
                end
            end
        end
    end
    isa(val, AnySSAValue) && return val # avoid infinite loop
    urs = userefs(val)
    for op in urs
        op[] = ssa_substitute_op!(insert_node!, subst_inst, op[], ssa_substitute)
    end
    return urs[]
end
