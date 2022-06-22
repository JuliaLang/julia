# This file is a part of Julia. License is MIT: https://julialang.org/license

@nospecialize

struct Signature
    f::Any
    ft::Any
    argtypes::Vector{Any}
    Signature(@nospecialize(f), @nospecialize(ft), argtypes::Vector{Any}) = new(f, ft, argtypes)
end

struct ResolvedInliningSpec
    # The LineTable and IR of the inlinee
    ir::IRCode
    # If the function being inlined is a single basic block we can use a
    # simpler inlining algorithm. This flag determines whether that's allowed
    linear_inline_eligible::Bool
    # Effects of the call statement
    effects::Effects
end
ResolvedInliningSpec(ir::IRCode, effects::Effects) =
    ResolvedInliningSpec(ir, linear_inline_eligible(ir), effects)

"""
Represents a callsite that our analysis has determined is legal to inline,
but did not resolve during the analysis step to allow the outer inlining
pass to apply its own inlining policy decisions.
"""
struct DelayedInliningSpec
    match::Union{MethodMatch, InferenceResult}
    argtypes::Vector{Any}
end

struct InliningTodo
    # The MethodInstance to be inlined
    mi::MethodInstance
    spec::Union{ResolvedInliningSpec, DelayedInliningSpec}
end

InliningTodo(mi::MethodInstance, match::MethodMatch, argtypes::Vector{Any}) =
    InliningTodo(mi, DelayedInliningSpec(match, argtypes))

InliningTodo(result::InferenceResult, argtypes::Vector{Any}) =
    InliningTodo(result.linfo, DelayedInliningSpec(result, argtypes))

struct ConstantCase
    val::Any
    ConstantCase(val) = new(val)
end

struct SomeCase
    val::Any
    SomeCase(val) = new(val)
end

struct InvokeCase
    invoke::MethodInstance
    effects::Effects
end

struct InliningCase
    sig  # Type
    item # Union{InliningTodo, MethodInstance, ConstantCase}
    function InliningCase(@nospecialize(sig), @nospecialize(item))
        @assert isa(item, Union{InliningTodo, InvokeCase, ConstantCase}) "invalid inlining item"
        return new(sig, item)
    end
end

struct UnionSplit
    fully_covered::Bool
    atype::DataType
    cases::Vector{InliningCase}
    bbs::Vector{Int}
    UnionSplit(fully_covered::Bool, atype::DataType, cases::Vector{InliningCase}) =
        new(fully_covered, atype, cases, Int[])
end

@specialize

function ssa_inlining_pass!(ir::IRCode, linetable::Vector{LineInfoNode}, state::InliningState, propagate_inbounds::Bool)
    # Go through the function, performing simple ininlingin (e.g. replacing call by constants
    # and analyzing legality of inlining).
    @timeit "analysis" todo = assemble_inline_todo!(ir, state)
    isempty(todo) && return ir
    # Do the actual inlining for every call we identified
    @timeit "execution" ir = batch_inline!(todo, ir, linetable, propagate_inbounds, state.params)
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

function cfg_inline_item!(ir::IRCode, idx::Int, spec::ResolvedInliningSpec, state::CFGInliningState, from_unionsplit::Bool=false)
    inlinee_cfg = spec.ir.cfg
    # Figure out if we need to split the BB
    need_split_before = false
    need_split = true
    block = block_for_inst(ir, idx)
    inline_into_block!(state, block)

    if !isempty(inlinee_cfg.blocks[1].preds)
        need_split_before = true
    end

    last_block_idx = last(state.cfg.blocks[block].stmts)
    if false # TODO: ((idx+1) == last_block_idx && isa(ir[SSAValue(last_block_idx)], GotoNode))
        need_split = false
        post_bb_id = -ir[SSAValue(last_block_idx)][:inst].label
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

    # Fixup the edges of the newely added blocks
    for (old_block, new_block) in enumerate(bb_rename_range)
        if old_block != 1 || need_split_before
            p = state.new_cfg_blocks[new_block].preds
            let bb_rename_range = bb_rename_range
                map!(p, p) do old_pred_block
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

    if need_split_before
        push!(state.new_cfg_blocks[first(bb_rename_range)].preds, first(bb_rename_range)-1)
    end

    any_edges = false
    for (old_block, new_block) in enumerate(bb_rename_range)
        if (length(state.new_cfg_blocks[new_block].succs) == 0)
            terminator_idx = last(inlinee_cfg.blocks[old_block].stmts)
            terminator = spec.ir[SSAValue(terminator_idx)][:inst]
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

function cfg_inline_unionsplit!(ir::IRCode, idx::Int,
                                (; fully_covered, #=atype,=# cases, bbs)::UnionSplit,
                                state::CFGInliningState,
                                params::OptimizationParams)
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
            spec = case.spec::ResolvedInliningSpec
            if !spec.linear_inline_eligible
                cfg_inline_item!(ir, idx, spec, state, true)
            end
        end
        push!(from_bbs, length(state.new_cfg_blocks))
        # TODO: Right now we unconditionally generate a fallback block
        # in case of subtyping errors - This is probably unnecessary.
        if i != length(cases) || (!fully_covered || (!params.trust_inference))
            # This block will have the next condition or the final else case
            push!(state.new_cfg_blocks, BasicBlock(StmtRange(idx, idx)))
            push!(state.new_cfg_blocks[cond_bb].succs, length(state.new_cfg_blocks))
            push!(state.new_cfg_blocks[end].preds, cond_bb)
            push!(bbs, length(state.new_cfg_blocks))
        end
    end
    # The edge from the fallback block.
    fully_covered || push!(from_bbs, length(state.new_cfg_blocks))
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

function ir_inline_linetable!(linetable::Vector{LineInfoNode}, inlinee_ir::IRCode,
                              inlinee::Method,
                              inlined_at::Int32)
    coverage = coverage_enabled(inlinee.module)
    linetable_offset::Int32 = length(linetable)
    # Append the linetable of the inlined function to our line table
    topline::Int32 = linetable_offset + Int32(1)
    coverage_by_path = JLOptions().code_coverage == 3
    push!(linetable, LineInfoNode(inlinee.module, inlinee.name, inlinee.file, inlinee.line, inlined_at))
    oldlinetable = inlinee_ir.linetable
    extra_coverage_line = 0
    for oldline in 1:length(oldlinetable)
        entry = oldlinetable[oldline]
        if !coverage && coverage_by_path && is_file_tracked(entry.file)
            # include topline coverage entry if in path-specific coverage mode, and any file falls under path
            coverage = true
        end
        newentry = LineInfoNode(entry.module, entry.method, entry.file, entry.line,
            (entry.inlined_at > 0 ? entry.inlined_at + linetable_offset + (oldline == 1) : inlined_at))
        if oldline == 1
            # check for a duplicate on the first iteration (likely true)
            if newentry === linetable[topline]
                continue
            else
                linetable_offset += 1
            end
        end
        push!(linetable, newentry)
    end
    if coverage && inlinee_ir.stmts[1][:line] + linetable_offset != topline
        extra_coverage_line = topline
    end
    return linetable_offset, extra_coverage_line
end

function ir_inline_item!(compact::IncrementalCompact, idx::Int, argexprs::Vector{Any},
                         linetable::Vector{LineInfoNode}, item::InliningTodo,
                         boundscheck::Symbol, todo_bbs::Vector{Tuple{Int, Int}})
    # Ok, do the inlining here
    spec = item.spec::ResolvedInliningSpec
    sparam_vals = item.mi.sparam_vals
    def = item.mi.def::Method
    inlined_at = compact.result[idx][:line]
    linetable_offset::Int32 = length(linetable)
    topline::Int32 = linetable_offset + Int32(1)
    linetable_offset, extra_coverage_line = ir_inline_linetable!(linetable, item.spec.ir, def, inlined_at)
    if extra_coverage_line != 0
        insert_node_here!(compact, NewInstruction(Expr(:code_coverage_effect), Nothing, extra_coverage_line))
    end
    if def.isva
        nargs_def = Int(def.nargs::Int32)
        if nargs_def > 0
            argexprs = fix_va_argexprs!(compact, argexprs, nargs_def, topline)
        end
    end
    if def.is_for_opaque_closure
        # Replace the first argument by a load of the capture environment
        argexprs[1] = insert_node_here!(compact,
            NewInstruction(Expr(:call, GlobalRef(Core, :getfield), argexprs[1], QuoteNode(:captures)),
            spec.ir.argtypes[1], topline))
    end
    if boundscheck === :default || boundscheck === :propagate
        if (compact.result[idx][:flag] & IR_FLAG_INBOUNDS) != 0
            boundscheck = :off
        end
    end
    # If the iterator already moved on to the next basic block,
    # temporarily re-open in again.
    local return_value
    sig = def.sig
    # Special case inlining that maintains the current basic block if there's only one BB in the target
    if spec.linear_inline_eligible
        #compact[idx] = nothing
        inline_compact = IncrementalCompact(compact, spec.ir, compact.result_idx)
        for ((_, idx′), stmt′) in inline_compact
            # This dance is done to maintain accurate usage counts in the
            # face of rename_arguments! mutating in place - should figure out
            # something better eventually.
            inline_compact[idx′] = nothing
            stmt′ = ssa_substitute!(idx′, stmt′, argexprs, sig, sparam_vals, linetable_offset, boundscheck, compact)
            if isa(stmt′, ReturnNode)
                val = stmt′.val
                return_value = SSAValue(idx′)
                inline_compact[idx′] = val
                inline_compact.result[idx′][:type] =
                    argextype(val, isa(val, Argument) || isa(val, Expr) ? compact : inline_compact)
                # Everything legal in value position is guaranteed to be effect free in stmt position
                inline_compact.result[idx′][:flag] = IR_FLAG_EFFECT_FREE
                break
            end
            inline_compact[idx′] = stmt′
        end
        just_fixup!(inline_compact)
        compact.result_idx = inline_compact.result_idx
    else
        bb_offset, post_bb_id = popfirst!(todo_bbs)
        # This implements the need_split_before flag above
        need_split_before = !isempty(spec.ir.cfg.blocks[1].preds)
        if need_split_before
            finish_current_bb!(compact, 0)
        end
        pn = PhiNode()
        #compact[idx] = nothing
        inline_compact = IncrementalCompact(compact, spec.ir, compact.result_idx)
        for ((_, idx′), stmt′) in inline_compact
            inline_compact[idx′] = nothing
            stmt′ = ssa_substitute!(idx′, stmt′, argexprs, sig, sparam_vals, linetable_offset, boundscheck, compact)
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
            elseif isa(stmt′, Expr) && stmt′.head === :enter
                stmt′ = Expr(:enter, stmt′.args[1]::Int + bb_offset)
            elseif isa(stmt′, GotoIfNot)
                stmt′ = GotoIfNot(stmt′.cond, stmt′.dest + bb_offset)
            elseif isa(stmt′, PhiNode)
                stmt′ = PhiNode(Int32[edge+bb_offset for edge in stmt′.edges], stmt′.values)
            end
            inline_compact[idx′] = stmt′
        end
        just_fixup!(inline_compact)
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

function fix_va_argexprs!(compact::IncrementalCompact,
    argexprs::Vector{Any}, nargs_def::Int, line_idx::Int32)
    newargexprs = argexprs[1:(nargs_def-1)]
    tuple_call = Expr(:call, TOP_TUPLE)
    tuple_typs = Any[]
    for i in nargs_def:length(argexprs)
        arg = argexprs[i]
        push!(tuple_call.args, arg)
        push!(tuple_typs, argextype(arg, compact))
    end
    tuple_typ = tuple_tfunc(tuple_typs)
    push!(newargexprs, insert_node_here!(compact, NewInstruction(tuple_call, tuple_typ, line_idx)))
    return newargexprs
end

const FATAL_TYPE_BOUND_ERROR = ErrorException("fatal error in type inference (type bound)")

"""
    ir_inline_unionsplit!

The core idea of this function is to simulate the dispatch semantics by generating
(flat) `isa`-checks corresponding to the signatures of union-split dispatch candidates,
and then inline their bodies into each `isa`-conditional block.
This `isa`-based virtual dispatch requires few pre-conditions to hold in order to simulate
the actual semantics correctly.

The first one is that these dispatch candidates need to be processed in order of their specificity,
and the corresponding `isa`-checks should reflect the method specificities, since now their
signatures are not necessarily concrete.
For example, given the following definitions:

    f(x::Int)    = ...
    f(x::Number) = ...
    f(x::Any)    = ...

and a callsite:

    f(x::Any)

then a correct `isa`-based virtual dispatch would be:

    if isa(x, Int)
        [inlined/resolved f(x::Int)]
    elseif isa(x, Number)
        [inlined/resolved f(x::Number)]
    else # implies `isa(x, Any)`, which fully covers this call signature,
         # otherwise we need to insert a fallback dynamic dispatch case also
        [inlined/resolved f(x::Any)]
    end

Fortunately, `ml_matches` should already sorted them in that way, except cases when there is
any ambiguity, from which we already bail out at this point.

Another consideration is type equality constraint from type variables: the `isa`-checks are
not enough to simulate the dispatch semantics in cases like:
Given a definition:

    g(x::T, y::T) where T<:Integer = ...

transform a callsite:

    g(x::Any, y::Any)

into the optimized form:

    if isa(x, Integer) && isa(y, Integer)
        [inlined/resolved g(x::Integer, y::Integer)]
    else
        g(x, y) # fallback dynamic dispatch
    end

But again, we should already bail out from such cases at this point, essentially by
excluding cases where `case.sig::UnionAll`.

In short, here we can process the dispatch candidates in order, assuming we haven't changed
their order somehow somewhere up to this point.
"""
function ir_inline_unionsplit!(compact::IncrementalCompact, idx::Int,
                               argexprs::Vector{Any}, linetable::Vector{LineInfoNode},
                               (; fully_covered, atype, cases, bbs)::UnionSplit,
                               boundscheck::Symbol, todo_bbs::Vector{Tuple{Int, Int}},
                               params::OptimizationParams)
    stmt, typ, line = compact.result[idx][:inst], compact.result[idx][:type], compact.result[idx][:line]
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
        if i != ncases || !fully_covered || !params.trust_inference
            for i = 1:nparams
                a, m = fieldtype(atype, i), fieldtype(mtype, i)
                # If this is always true, we don't need to check for it
                a <: m && continue
                # Generate isa check
                isa_expr = Expr(:call, isa, argexprs[i], m)
                ssa = insert_node_here!(compact, NewInstruction(isa_expr, Bool, line))
                if cond === true
                    cond = ssa
                else
                    and_expr = Expr(:call, and_int, cond, ssa)
                    cond = insert_node_here!(compact, NewInstruction(and_expr, Bool, line))
                end
            end
            insert_node_here!(compact, NewInstruction(GotoIfNot(cond, next_cond_bb), Union{}, line))
        end
        bb = next_cond_bb - 1
        finish_current_bb!(compact, 0)
        argexprs′ = argexprs
        if !isa(case, ConstantCase)
            argexprs′ = copy(argexprs)
            for i = 1:nparams
                argex = argexprs[i]
                (isa(argex, SSAValue) || isa(argex, Argument)) || continue
                a, m = fieldtype(atype, i), fieldtype(mtype, i)
                if !(a <: m)
                    argexprs′[i] = insert_node_here!(compact,
                        NewInstruction(PiNode(argex, m), m, line))
                end
            end
        end
        if isa(case, InliningTodo)
            val = ir_inline_item!(compact, idx, argexprs′, linetable, case, boundscheck, todo_bbs)
        elseif isa(case, InvokeCase)
            effect_free = is_removable_if_unused(case.effects)
            val = insert_node_here!(compact,
                NewInstruction(Expr(:invoke, case.invoke, argexprs′...), typ, nothing,
                    line, effect_free ? IR_FLAG_EFFECT_FREE : IR_FLAG_NULL, effect_free))
        else
            case = case::ConstantCase
            val = case.val
        end
        if !isempty(compact.result_bbs[bb].preds)
            push!(pn.edges, bb)
            push!(pn.values, val)
            insert_node_here!(compact,
                NewInstruction(GotoNode(join_bb), Union{}, line))
        else
            insert_node_here!(compact,
                NewInstruction(ReturnNode(), Union{}, line))
        end
        finish_current_bb!(compact, 0)
    end
    bb += 1
    # We're now in the fall through block, decide what to do
    if fully_covered
        if !params.trust_inference
            e = Expr(:call, GlobalRef(Core, :throw), FATAL_TYPE_BOUND_ERROR)
            insert_node_here!(compact, NewInstruction(e, Union{}, line))
            insert_node_here!(compact, NewInstruction(ReturnNode(), Union{}, line))
            finish_current_bb!(compact, 0)
        end
    else
        ssa = insert_node_here!(compact, NewInstruction(stmt, typ, line))
        push!(pn.edges, bb)
        push!(pn.values, ssa)
        insert_node_here!(compact, NewInstruction(GotoNode(join_bb), Union{}, line))
        finish_current_bb!(compact, 0)
    end

    # We're now in the join block.
    return insert_node_here!(compact, NewInstruction(pn, typ, line))
end

function batch_inline!(todo::Vector{Pair{Int, Any}}, ir::IRCode, linetable::Vector{LineInfoNode}, propagate_inbounds::Bool, params::OptimizationParams)
    # Compute the new CFG first (modulo statement ranges, which will be computed below)
    state = CFGInliningState(ir)
    for (idx, item) in todo
        if isa(item, UnionSplit)
            cfg_inline_unionsplit!(ir, idx, item, state, params)
        else
            item = item::InliningTodo
            spec = item.spec::ResolvedInliningSpec
            # A linear inline does not modify the CFG
            spec.linear_inline_eligible && continue
            cfg_inline_item!(ir, idx, spec, state, false)
        end
    end
    finish_cfg_inline!(state)

    boundscheck = inbounds_option()
    if boundscheck === :default && propagate_inbounds
        boundscheck = :propagate
    end

    let compact = IncrementalCompact(ir, false)
        compact.result_bbs = state.new_cfg_blocks
        # This needs to be a minimum and is more of a size hint
        nn = 0
        for (_, item) in todo
            if isa(item, InliningTodo)
                spec = item.spec::ResolvedInliningSpec
                nn += (length(spec.ir.stmts) + length(spec.ir.new_nodes))
            end
        end
        nnewnodes = length(compact.result) + nn
        resize!(compact, nnewnodes)
        (inline_idx, item) = popfirst!(todo)
        for ((old_idx, idx), stmt) in compact
            if old_idx == inline_idx
                stmt = stmt::Expr
                argexprs = copy(stmt.args)
                refinish = false
                if compact.result_idx == first(compact.result_bbs[compact.active_result_bb].stmts)
                    compact.active_result_bb -= 1
                    refinish = true
                end
                # It is possible for GlobalRefs and Exprs to be in argument position
                # at this point in the IR, though in that case they are required
                # to be effect-free. However, we must still move them out of argument
                # position, since `Argument` is allowed in PhiNodes, but `GlobalRef`
                # and `Expr` are not, so a substitution could anger the verifier.
                for aidx in 1:length(argexprs)
                    aexpr = argexprs[aidx]
                    if isa(aexpr, Expr) || isa(aexpr, GlobalRef)
                        ninst = effect_free(NewInstruction(aexpr, argextype(aexpr, compact), compact.result[idx][:line]))
                        argexprs[aidx] = insert_node_here!(compact, ninst)
                    end
                end
                if isa(item, InliningTodo)
                    compact.ssa_rename[old_idx] = ir_inline_item!(compact, idx, argexprs, linetable, item, boundscheck, state.todo_bbs)
                elseif isa(item, UnionSplit)
                    compact.ssa_rename[old_idx] = ir_inline_unionsplit!(compact, idx, argexprs, linetable, item, boundscheck, state.todo_bbs, params)
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
            elseif isa(stmt, Expr) && stmt.head === :enter
                compact[idx] = Expr(:enter, state.bb_rename[stmt.args[1]::Int])
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
function rewrite_apply_exprargs!(
    ir::IRCode, idx::Int, stmt::Expr, argtypes::Vector{Any},
    arginfos::Vector{MaybeAbstractIterationInfo}, arg_start::Int, istate::InliningState, todo::Vector{Pair{Int, Any}})
    flag = ir.stmts[idx][:flag]
    argexprs = stmt.args
    new_argexprs = Any[argexprs[arg_start]]
    new_argtypes = Any[argtypes[arg_start]]
    # loop over original arguments and flatten any known iterators
    for i in (arg_start+1):length(argexprs)
        def = argexprs[i]
        def_type = argtypes[i]
        thisarginfo = arginfos[i-arg_start]
        if thisarginfo === nothing
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
                    if ti.name === NamedTuple_typename
                        ti = ti.parameters[2]::DataType # checked by `is_valid_type_for_apply_rewrite`
                    end
                    for p in ti.parameters
                        if isa(p, DataType) && isdefined(p, :instance)
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
                if isa(new_info, ConstCallInfo)
                    handle_const_call!(
                        ir, state1.id, new_stmt, new_info, flag,
                        new_sig, istate, todo)
                elseif isa(new_info, MethodMatchInfo) || isa(new_info, UnionSplitInfo)
                    new_infos = isa(new_info, MethodMatchInfo) ? MethodMatchInfo[new_info] : new_info.matches
                    # See if we can inline this call to `iterate`
                    analyze_single_call!(
                        ir, state1.id, new_stmt, new_infos, flag,
                        new_sig, istate, todo)
                end
                if i != length(thisarginfo.each)
                    valT = getfield_tfunc(call.rt, Const(1))
                    val_extracted = insert_node!(ir, idx, NewInstruction(
                        Expr(:call, GlobalRef(Core, :getfield), state1, 1),
                        valT))
                    push!(new_argexprs, val_extracted)
                    push!(new_argtypes, valT)
                    state_extracted = insert_node!(ir, idx, NewInstruction(
                        Expr(:call, GlobalRef(Core, :getfield), state1, 2),
                        getfield_tfunc(call.rt, Const(2))))
                    state = Core.svec(state_extracted)
                end
            end
        end
    end
    stmt.args = new_argexprs
    return new_argtypes
end

function compileable_specialization(et::Union{EdgeTracker, Nothing}, match::MethodMatch, effects::Effects)
    mi = specialize_method(match; compilesig=true)
    mi === nothing && return nothing
    et !== nothing && push!(et, mi)
    return InvokeCase(mi, effects)
end

function compileable_specialization(et::Union{EdgeTracker, Nothing}, linfo::MethodInstance, effects::Effects)
    mi = specialize_method(linfo.def::Method, linfo.specTypes, linfo.sparam_vals; compilesig=true)
    mi === nothing && return nothing
    et !== nothing && push!(et, mi)
    return InvokeCase(mi, effects)
end

function compileable_specialization(et::Union{EdgeTracker, Nothing}, result::InferenceResult, effects::Effects)
    return compileable_specialization(et, result.linfo, effects)
end

function resolve_todo(todo::InliningTodo, state::InliningState, flag::UInt8)
    mi = todo.mi
    (; match, argtypes) = todo.spec::DelayedInliningSpec
    et = state.et

    #XXX: update_valid_age!(min_valid[1], max_valid[1], sv)
    if isa(match, InferenceResult)
        inferred_src = match.src
        if isa(inferred_src, ConstAPI)
            # use constant calling convention
            et !== nothing && push!(et, mi)
            return ConstantCase(quoted(inferred_src.val))
        else
            src = inferred_src # ::Union{Nothing,CodeInfo} for NativeInterpreter
        end
        effects = match.ipo_effects
    else
        code = get(state.mi_cache, mi, nothing)
        if code isa CodeInstance
            if use_const_api(code)
                # in this case function can be inlined to a constant
                et !== nothing && push!(et, mi)
                return ConstantCase(quoted(code.rettype_const))
            else
                src = code.inferred
            end
            effects = decode_effects(code.ipo_purity_bits)
        else # fallback pass for external AbstractInterpreter cache
            effects = Effects()
            src = code
        end
    end

    # the duplicated check might have been done already within `analyze_method!`, but still
    # we need it here too since we may come here directly using a constant-prop' result
    if !state.params.inlining || is_stmt_noinline(flag)
        return compileable_specialization(et, match, effects)
    end

    src = inlining_policy(state.interp, src, flag, mi, argtypes)

    src === nothing && return compileable_specialization(et, match, effects)

    et !== nothing && push!(et, mi)
    return InliningTodo(mi, retrieve_ir_for_inlining(mi, src), effects)
end

function resolve_todo((; fully_covered, atype, cases, #=bbs=#)::UnionSplit, state::InliningState, flag::UInt8)
    ncases = length(cases)
    newcases = Vector{InliningCase}(undef, ncases)
    for i in 1:ncases
        (; sig, item) = cases[i]
        newitem = resolve_todo(item, state, flag)
        push!(newcases, InliningCase(sig, newitem))
    end
    return UnionSplit(fully_covered, atype, newcases)
end

function validate_sparams(sparams::SimpleVector)
    for i = 1:length(sparams)
        (isa(sparams[i], TypeVar) || isvarargtype(sparams[i])) && return false
    end
    return true
end

function analyze_method!(match::MethodMatch, argtypes::Vector{Any},
                         flag::UInt8, state::InliningState,
                         do_resolve::Bool = true)
    method = match.method
    spec_types = match.spec_types

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
        if !(spec_types isa DataType && length(spec_types.parameters) == length(argtypes) && !isvarargtype(spec_types.parameters[end]))
            return nothing
        end
    end

    # Bail out if any static parameters are left as TypeVar
    validate_sparams(match.sparams) || return nothing

    et = state.et

    # See if there exists a specialization for this method signature
    mi = specialize_method(match; preexisting=true) # Union{Nothing, MethodInstance}
    isa(mi, MethodInstance) || return compileable_specialization(et, match, Effects())

    todo = InliningTodo(mi, match, argtypes)
    # If we don't have caches here, delay resolving this MethodInstance
    # until the batch inlining step (or an external post-processing pass)
    do_resolve && state.mi_cache === nothing && return todo
    return resolve_todo(todo, state, flag)
end

function InliningTodo(mi::MethodInstance, ir::IRCode, effects::Effects)
    return InliningTodo(mi, ResolvedInliningSpec(ir, effects))
end

function retrieve_ir_for_inlining(mi::MethodInstance, src::Array{UInt8, 1})
    src = ccall(:jl_uncompress_ir, Any, (Any, Ptr{Cvoid}, Any), mi.def, C_NULL, src::Vector{UInt8})::CodeInfo
    return inflate_ir!(src, mi)
end
retrieve_ir_for_inlining(mi::MethodInstance, src::CodeInfo) = inflate_ir(src, mi)::IRCode
retrieve_ir_for_inlining(mi::MethodInstance, ir::IRCode) = copy(ir)

function handle_single_case!(
    ir::IRCode, idx::Int, stmt::Expr,
    @nospecialize(case), todo::Vector{Pair{Int, Any}}, params::OptimizationParams, isinvoke::Bool = false)
    if isa(case, ConstantCase)
        ir[SSAValue(idx)][:inst] = case.val
    elseif isa(case, InvokeCase)
        is_total(case.effects) && inline_const_if_inlineable!(ir[SSAValue(idx)]) && return nothing
        isinvoke && rewrite_invoke_exprargs!(stmt)
        stmt.head = :invoke
        pushfirst!(stmt.args, case.invoke)
        if is_removable_if_unused(case.effects)
            ir[SSAValue(idx)][:flag] |= IR_FLAG_EFFECT_FREE
        end
    elseif case === nothing
        # Do, well, nothing
    else
        isinvoke && rewrite_invoke_exprargs!(stmt)
        push!(todo, idx=>(case::InliningTodo))
    end
    nothing
end

rewrite_invoke_exprargs!(expr::Expr) = (expr.args = invoke_rewrite(expr.args); expr)

function is_valid_type_for_apply_rewrite(@nospecialize(typ), params::OptimizationParams)
    if isa(typ, Const) && (v = typ.val; isa(v, SimpleVector))
        length(v) > params.MAX_TUPLE_SPLAT && return false
        for p in v
            is_inlineable_constant(p) || return false
        end
        return true
    end
    typ = widenconst(typ)
    if isa(typ, DataType) && typ.name === NamedTuple_typename
        typ = typ.parameters[2]
        typ = unwraptv(typ)
    end
    isa(typ, DataType) || return false
    if typ.name === Tuple.name
        return !isvatuple(typ) && length(typ.parameters) <= params.MAX_TUPLE_SPLAT
    else
        return false
    end
end

function inline_splatnew!(ir::IRCode, idx::Int, stmt::Expr, @nospecialize(rt))
    nf = nfields_tfunc(rt)
    if nf isa Const
        eargs = stmt.args
        tup = eargs[2]
        tt = argextype(tup, ir)
        tnf = nfields_tfunc(tt)
        # TODO: hoisting this tnf.val === nf.val check into codegen
        # would enable us to almost always do this transform
        if tnf isa Const && tnf.val === nf.val
            n = tnf.val::Int
            new_argexprs = Any[eargs[1]]
            for j = 1:n
                atype = getfield_tfunc(tt, Const(j))
                new_call = Expr(:call, Core.getfield, tup, j)
                new_argexpr = insert_node!(ir, idx, NewInstruction(new_call, atype))
                push!(new_argexprs, new_argexpr)
            end
            stmt.head = :new
            stmt.args = new_argexprs
        end
    end
    nothing
end

function call_sig(ir::IRCode, stmt::Expr)
    isempty(stmt.args) && return nothing
    ft = argextype(stmt.args[1], ir)
    has_free_typevars(ft) && return nothing
    f = singleton_type(ft)
    f === Core.Intrinsics.llvmcall && return nothing
    f === Core.Intrinsics.cglobal && return nothing
    argtypes = Vector{Any}(undef, length(stmt.args))
    argtypes[1] = ft
    for i = 2:length(stmt.args)
        a = argextype(stmt.args[i], ir)
        (a === Bottom || isvarargtype(a)) && return nothing
        argtypes[i] = a
    end
    return Signature(f, ft, argtypes)
end

function inline_apply!(
    ir::IRCode, idx::Int, stmt::Expr, sig::Signature,
    state::InliningState, todo::Vector{Pair{Int, Any}})
    while sig.f === Core._apply_iterate
        info = ir.stmts[idx][:info]
        if isa(info, UnionSplitApplyCallInfo)
            if length(info.infos) != 1
                # TODO: Handle union split applies?
                new_info = info = false
            else
                info = info.infos[1]
                new_info = info.call
            end
        else
            @assert info === nothing || info === false
            new_info = info = false
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
            for i = (arg_start + 1):length(argtypes)
                ti = argtypes[i]
                ti ⊑ Tuple{} && continue
                if ti ⊑ Tuple && nonempty_idx == 0
                    nonempty_idx = i
                    continue
                end
                nonempty_idx = 0
                break
            end
            if nonempty_idx != 0
                ir.stmts[idx][:inst] = stmt.args[nonempty_idx]
                return nothing
            end
        end
        # Try to figure out the signature of the function being called
        # and if rewrite_apply_exprargs can deal with this form
        arginfos = MaybeAbstractIterationInfo[]
        for i = (arg_start + 1):length(argtypes)
            thisarginfo = nothing
            if !is_valid_type_for_apply_rewrite(argtypes[i], state.params)
                if isa(info, ApplyCallInfo) && info.arginfo[i-arg_start] !== nothing
                    thisarginfo = info.arginfo[i-arg_start]
                else
                    return nothing
                end
            end
            push!(arginfos, thisarginfo)
        end
        # Independent of whether we can inline, the above analysis allows us to rewrite
        # this apply call to a regular call
        argtypes = rewrite_apply_exprargs!(
            ir, idx, stmt, argtypes,
            arginfos, arg_start, state, todo)
        ir.stmts[idx][:info] = new_info
        has_free_typevars(ft) && return nothing
        f = singleton_type(ft)
        sig = Signature(f, ft, argtypes)
    end
    sig
end

# TODO: this test is wrong if we start to handle Unions of function types later
is_builtin(s::Signature) =
    isa(s.f, IntrinsicFunction) ||
    s.ft ⊑ IntrinsicFunction ||
    isa(s.f, Builtin) ||
    s.ft ⊑ Builtin

function inline_invoke!(
    ir::IRCode, idx::Int, stmt::Expr, info::InvokeCallInfo, flag::UInt8,
    sig::Signature, state::InliningState, todo::Vector{Pair{Int, Any}})
    match = info.match
    if !match.fully_covers
        # TODO: We could union split out the signature check and continue on
        return nothing
    end
    result = info.result
    if isa(result, ConcreteResult)
        item = concrete_result_item(result, state)
    else
        argtypes = invoke_rewrite(sig.argtypes)
        if isa(result, ConstPropResult)
            (; mi) = item = InliningTodo(result.result, argtypes)
            validate_sparams(mi.sparam_vals) || return nothing
            if argtypes_to_type(argtypes) <: mi.def.sig
                state.mi_cache !== nothing && (item = resolve_todo(item, state, flag))
                handle_single_case!(ir, idx, stmt, item, todo, state.params, true)
                return nothing
            end
        end
        item = analyze_method!(match, argtypes, flag, state)
    end
    handle_single_case!(ir, idx, stmt, item, todo, state.params, true)
    return nothing
end

function narrow_opaque_closure!(ir::IRCode, stmt::Expr, @nospecialize(info), state::InliningState)
    if isa(info, OpaqueClosureCreateInfo)
        lbt = argextype(stmt.args[2], ir)
        lb, exact = instanceof_tfunc(lbt)
        exact || return
        ubt = argextype(stmt.args[3], ir)
        ub, exact = instanceof_tfunc(ubt)
        exact || return
        # Narrow opaque closure type
        newT = widenconst(tmeet(tmerge(lb, info.unspec.rt), ub))
        if newT != ub
            # N.B.: Narrowing the ub requires a backdge on the mi whose type
            # information we're using, since a change in that function may
            # invalidate ub result.
            stmt.args[3] = newT
        end
    end
end

# As a matter of convenience, this pass also computes effect-freenes.
# For primitives, we do that right here. For proper calls, we will
# discover this when we consult the caches.
function check_effect_free!(ir::IRCode, idx::Int, @nospecialize(stmt), @nospecialize(rt))
    if stmt_effect_free(stmt, rt, ir)
        ir.stmts[idx][:flag] |= IR_FLAG_EFFECT_FREE
        return true
    end
    return false
end

# Handles all analysis and inlining of intrinsics and builtins. In particular,
# this method does not access the method table or otherwise process generic
# functions.
function process_simple!(ir::IRCode, idx::Int, state::InliningState, todo::Vector{Pair{Int, Any}})
    stmt = ir.stmts[idx][:inst]
    rt = ir.stmts[idx][:type]
    if !(stmt isa Expr)
        check_effect_free!(ir, idx, stmt, rt)
        return nothing
    end
    head = stmt.head
    if head !== :call
        if head === :splatnew
            inline_splatnew!(ir, idx, stmt, rt)
        elseif head === :new_opaque_closure
            narrow_opaque_closure!(ir, stmt, ir.stmts[idx][:info], state)
        end
        check_effect_free!(ir, idx, stmt, rt)
        return nothing
    end

    sig = call_sig(ir, stmt)
    sig === nothing && return nothing

    # Handle _apply_iterate
    sig = inline_apply!(ir, idx, stmt, sig, state, todo)
    sig === nothing && return nothing

    # Check if we match any of the early inliners
    earlyres = early_inline_special_case(ir, stmt, rt, sig, state.params)
    if isa(earlyres, SomeCase)
        ir.stmts[idx][:inst] = earlyres.val
        return nothing
    end
    if (sig.f === modifyfield! || sig.ft ⊑ typeof(modifyfield!)) && 5 <= length(stmt.args) <= 6
        let info = ir.stmts[idx][:info]
            info isa MethodResultPure && (info = info.info)
            info isa ConstCallInfo && (info = info.call)
            info isa MethodMatchInfo || return nothing
            length(info.results) == 1 || return nothing
            match = info.results[1]::MethodMatch
            match.fully_covers || return nothing
            case = compileable_specialization(state.et, match, Effects())
            case === nothing && return nothing
            stmt.head = :invoke_modify
            pushfirst!(stmt.args, case.invoke)
            ir.stmts[idx][:inst] = stmt
        end
        return nothing
    end

    if check_effect_free!(ir, idx, stmt, rt)
        if sig.f === typeassert || sig.ft ⊑ typeof(typeassert)
            # typeassert is a no-op if effect free
            ir.stmts[idx][:inst] = stmt.args[2]
            return nothing
        end
    end

    if sig.f !== Core.invoke && sig.f !== Core.finalizer && is_builtin(sig)
        # No inlining for builtins (other invoke/apply/typeassert)
        return nothing
    end

    # Special case inliners for regular functions
    lateres = late_inline_special_case!(ir, idx, stmt, rt, sig, state.params)
    if isa(lateres, SomeCase)
        ir[SSAValue(idx)][:inst] = lateres.val
        check_effect_free!(ir, idx, lateres.val, rt)
        return nothing
    end

    return stmt, sig
end

# TODO inline non-`isdispatchtuple`, union-split callsites?
function compute_inlining_cases(
        infos::Vector{MethodMatchInfo}, flag::UInt8,
        sig::Signature, state::InliningState,
        do_resolve::Bool = true)
    argtypes = sig.argtypes
    cases = InliningCase[]
    local any_fully_covered = false
    local handled_all_cases = true
    for i in 1:length(infos)
        meth = infos[i].results
        if meth.ambig
            # Too many applicable methods
            # Or there is a (partial?) ambiguity
            return nothing
        elseif length(meth) == 0
            # No applicable methods; try next union split
            handled_all_cases = false
            continue
        end
        for match in meth
            handled_all_cases &= handle_match!(match, argtypes, flag, state, cases, true, do_resolve)
            any_fully_covered |= match.fully_covers
        end
    end

    if !handled_all_cases
        # if we've not seen all candidates, union split is valid only for dispatch tuples
        filter!(case::InliningCase->isdispatchtuple(case.sig), cases)
    end

    return cases, handled_all_cases & any_fully_covered
end

function analyze_single_call!(
    ir::IRCode, idx::Int, stmt::Expr, infos::Vector{MethodMatchInfo}, flag::UInt8,
    sig::Signature, state::InliningState, todo::Vector{Pair{Int, Any}})

    r = compute_inlining_cases(infos, flag, sig, state)
    r === nothing && return nothing
    cases, all_covered = r
    handle_cases!(ir, idx, stmt, argtypes_to_type(sig.argtypes), cases,
        all_covered, todo, state.params)
end

# similar to `analyze_single_call!`, but with constant results
function handle_const_call!(
    ir::IRCode, idx::Int, stmt::Expr, cinfo::ConstCallInfo, flag::UInt8,
    sig::Signature, state::InliningState, todo::Vector{Pair{Int, Any}})
    argtypes = sig.argtypes
    (; call, results) = cinfo
    infos = isa(call, MethodMatchInfo) ? MethodMatchInfo[call] : call.matches
    cases = InliningCase[]
    local any_fully_covered = false
    local handled_all_cases = true
    local j = 0
    for i in 1:length(infos)
        meth = infos[i].results
        if meth.ambig
            # Too many applicable methods
            # Or there is a (partial?) ambiguity
            return nothing
        elseif length(meth) == 0
            # No applicable methods; try next union split
            handled_all_cases = false
            continue
        end
        for match in meth
            j += 1
            result = results[j]
            any_fully_covered |= match.fully_covers
            if isa(result, ConcreteResult)
                case = concrete_result_item(result, state)
                push!(cases, InliningCase(result.mi.specTypes, case))
            elseif isa(result, ConstPropResult)
                handled_all_cases &= handle_const_prop_result!(result, argtypes, flag, state, cases, true)
            else
                @assert result === nothing
                handled_all_cases &= handle_match!(match, argtypes, flag, state, cases, true)
            end
        end
    end

    if !handled_all_cases
        # if we've not seen all candidates, union split is valid only for dispatch tuples
        filter!(case::InliningCase->isdispatchtuple(case.sig), cases)
    end

    handle_cases!(ir, idx, stmt, argtypes_to_type(argtypes), cases,
        handled_all_cases & any_fully_covered, todo, state.params)
end

function handle_match!(
    match::MethodMatch, argtypes::Vector{Any}, flag::UInt8, state::InliningState,
    cases::Vector{InliningCase}, allow_abstract::Bool = false,
    do_resolve::Bool = true)
    spec_types = match.spec_types
    allow_abstract || isdispatchtuple(spec_types) || return false
    # we may see duplicated dispatch signatures here when a signature gets widened
    # during abstract interpretation: for the purpose of inlining, we can just skip
    # processing this dispatch candidate
    _any(case->case.sig === spec_types, cases) && return true
    item = analyze_method!(match, argtypes, flag, state, do_resolve)
    item === nothing && return false
    push!(cases, InliningCase(spec_types, item))
    return true
end

function handle_const_prop_result!(
    result::ConstPropResult, argtypes::Vector{Any}, flag::UInt8, state::InliningState,
    cases::Vector{InliningCase}, allow_abstract::Bool = false)
    (; mi) = item = InliningTodo(result.result, argtypes)
    spec_types = mi.specTypes
    allow_abstract || isdispatchtuple(spec_types) || return false
    validate_sparams(mi.sparam_vals) || return false
    state.mi_cache !== nothing && (item = resolve_todo(item, state, flag))
    item === nothing && return false
    push!(cases, InliningCase(spec_types, item))
    return true
end

function concrete_result_item(result::ConcreteResult, state::InliningState)
    if !isdefined(result, :result) || !is_inlineable_constant(result.result)
        case = compileable_specialization(state.et, result.mi, result.effects)
        @assert case !== nothing "concrete evaluation should never happen for uncompileable callsite"
        return case
    end
    @assert result.effects === EFFECTS_TOTAL
    return ConstantCase(quoted(result.result))
end

function handle_cases!(ir::IRCode, idx::Int, stmt::Expr, @nospecialize(atype),
    cases::Vector{InliningCase}, fully_covered::Bool, todo::Vector{Pair{Int, Any}},
    params::OptimizationParams)
    # If we only have one case and that case is fully covered, we may either
    # be able to do the inlining now (for constant cases), or push it directly
    # onto the todo list
    if fully_covered && length(cases) == 1
        handle_single_case!(ir, idx, stmt, cases[1].item, todo, params)
    elseif length(cases) > 0
        isa(atype, DataType) || return nothing
        for case in cases
            isa(case.sig, DataType) || return nothing
        end
        push!(todo, idx=>UnionSplit(fully_covered, atype, cases))
    end
    return nothing
end

function handle_const_opaque_closure_call!(
    ir::IRCode, idx::Int, stmt::Expr, result::ConstPropResult, flag::UInt8,
    sig::Signature, state::InliningState, todo::Vector{Pair{Int, Any}})
    item = InliningTodo(result.result, sig.argtypes)
    isdispatchtuple(item.mi.specTypes) || return
    validate_sparams(item.mi.sparam_vals) || return
    state.mi_cache !== nothing && (item = resolve_todo(item, state, flag))
    handle_single_case!(ir, idx, stmt, item, todo, state.params)
    return nothing
end

function inline_const_if_inlineable!(inst::Instruction)
    rt = inst[:type]
    if rt isa Const && is_inlineable_constant(rt.val)
        inst[:inst] = quoted(rt.val)
        return true
    end
    inst[:flag] |= IR_FLAG_EFFECT_FREE
    return false
end

function assemble_inline_todo!(ir::IRCode, state::InliningState)
    todo = Pair{Int, Any}[]

    for idx in 1:length(ir.stmts)
        simpleres = process_simple!(ir, idx, state, todo)
        simpleres === nothing && continue
        stmt, sig = simpleres

        info = ir.stmts[idx][:info]

        # Check whether this call was @pure and evaluates to a constant
        if info isa MethodResultPure
            inline_const_if_inlineable!(ir[SSAValue(idx)]) && continue
            info = info.info
        end
        if info === false
            # Inference determined this couldn't be analyzed. Don't question it.
            continue
        end

        flag = ir.stmts[idx][:flag]

        if isa(info, OpaqueClosureCallInfo)
            result = info.result
            if isa(result, ConstPropResult)
                handle_const_opaque_closure_call!(
                    ir, idx, stmt, result, flag,
                    sig, state, todo)
            else
                if isa(result, ConcreteResult)
                    item = concrete_result_item(result, state)
                else
                    item = analyze_method!(info.match, sig.argtypes, flag, state)
                end
                handle_single_case!(ir, idx, stmt, item, todo, state.params)
            end
            continue
        end

        # Handle invoke
        if sig.f === Core.invoke
            if isa(info, InvokeCallInfo)
                inline_invoke!(ir, idx, stmt, info, flag, sig, state, todo)
            end
            continue
        end

        # Handle finalizer
        if sig.f === Core.finalizer
            if isa(info, FinalizerInfo)
                # Only inline finalizers that are known nothrow and notls.
                # This avoids having to set up state for finalizer isolation
                (is_nothrow(info.effects) && is_notaskstate(info.effects)) || continue

                info = info.info
                if isa(info, MethodMatchInfo)
                    infos = MethodMatchInfo[info]
                elseif isa(info, UnionSplitInfo)
                    infos = info.matches
                else
                    continue
                end

                ft = argextype(stmt.args[2], ir)
                has_free_typevars(ft) && return nothing
                f = singleton_type(ft)
                argtypes = Vector{Any}(undef, 2)
                argtypes[1] = ft
                argtypes[2] = argextype(stmt.args[3], ir)
                sig = Signature(f, ft, argtypes)

                cases, all_covered = compute_inlining_cases(infos, UInt8(0), sig, state, false)
                length(cases) == 0 && continue
                if all_covered && length(cases) == 1
                    if isa(cases[1], InliningCase)
                        case1 = cases[1].item
                        if isa(case1, InliningTodo)
                            push!(stmt.args, true)
                            push!(stmt.args, case1.mi)
                        elseif isa(case1, InvokeCase)
                            push!(stmt.args, false)
                            push!(stmt.args, case1.invoke)
                        end
                    end
                end
                continue
            end
        end

        # if inference arrived here with constant-prop'ed result(s),
        # we can perform a specialized analysis for just this case
        if isa(info, ConstCallInfo)
            handle_const_call!(
                ir, idx, stmt, info, flag,
                sig, state, todo)
            continue
        end

        # Ok, now figure out what method to call
        if isa(info, MethodMatchInfo)
            infos = MethodMatchInfo[info]
        elseif isa(info, UnionSplitInfo)
            infos = info.matches
        else
            continue # isa(info, ReturnTypeCallInfo), etc.
        end

        analyze_single_call!(ir, idx, stmt, infos, flag, sig, state, todo)
    end

    return todo
end

function linear_inline_eligible(ir::IRCode)
    length(ir.cfg.blocks) == 1 || return false
    terminator = ir[SSAValue(last(ir.cfg.blocks[1].stmts))][:inst]
    isa(terminator, ReturnNode) || return false
    isdefined(terminator, :val) || return false
    return true
end

# Check for a number of functions known to be pure
function ispuretopfunction(@nospecialize(f))
    return istopfunction(f, :typejoin) ||
        istopfunction(f, :isbits) ||
        istopfunction(f, :isbitstype) ||
        istopfunction(f, :promote_type)
end

function early_inline_special_case(
    ir::IRCode, stmt::Expr, @nospecialize(type), sig::Signature,
    params::OptimizationParams)
    params.inlining || return nothing
    (; f, ft, argtypes) = sig

    if isa(type, Const) # || isconstType(type)
        val = type.val
        is_inlineable_constant(val) || return nothing
        if isa(f, IntrinsicFunction)
            if is_pure_intrinsic_infer(f) && intrinsic_nothrow(f, argtypes[2:end])
                return SomeCase(quoted(val))
            end
        elseif ispuretopfunction(f) || contains_is(_PURE_BUILTINS, f)
            return SomeCase(quoted(val))
        elseif contains_is(_EFFECT_FREE_BUILTINS, f)
            if _builtin_nothrow(f, argtypes[2:end], type)
                return SomeCase(quoted(val))
            end
        elseif f === Core.get_binding_type
            length(argtypes) == 3 || return nothing
            if get_binding_type_effect_free(argtypes[2], argtypes[3])
                return SomeCase(quoted(val))
            end
        end
    end
    return nothing
end

# special-case some regular method calls whose results are not folded within `abstract_call_known`
# (and thus `early_inline_special_case` doesn't handle them yet)
# NOTE we manually inline the method bodies, and so the logic here needs to precisely sync with their definitions
function late_inline_special_case!(
    ir::IRCode, idx::Int, stmt::Expr, @nospecialize(type), sig::Signature,
    params::OptimizationParams)
    params.inlining || return nothing
    (; f, ft, argtypes) = sig
    if length(argtypes) == 3 && istopfunction(f, :!==)
        # special-case inliner for !== that precedes _methods_by_ftype union splitting
        # and that works, even though inference generally avoids inferring the `!==` Method
        if isa(type, Const)
            return SomeCase(quoted(type.val))
        end
        cmp_call = Expr(:call, GlobalRef(Core, :(===)), stmt.args[2], stmt.args[3])
        cmp_call_ssa = insert_node!(ir, idx, effect_free(NewInstruction(cmp_call, Bool)))
        not_call = Expr(:call, GlobalRef(Core.Intrinsics, :not_int), cmp_call_ssa)
        return SomeCase(not_call)
    elseif length(argtypes) == 3 && istopfunction(f, :(>:))
        # special-case inliner for issupertype
        # that works, even though inference generally avoids inferring the `>:` Method
        if isa(type, Const) && _builtin_nothrow(<:, Any[argtypes[3], argtypes[2]], type)
            return SomeCase(quoted(type.val))
        end
        subtype_call = Expr(:call, GlobalRef(Core, :(<:)), stmt.args[3], stmt.args[2])
        return SomeCase(subtype_call)
    elseif f === TypeVar && 2 <= length(argtypes) <= 4 && (argtypes[2] ⊑ Symbol)
        typevar_call = Expr(:call, GlobalRef(Core, :_typevar), stmt.args[2],
            length(stmt.args) < 4 ? Bottom : stmt.args[3],
            length(stmt.args) == 2 ? Any : stmt.args[end])
        return SomeCase(typevar_call)
    elseif f === UnionAll && length(argtypes) == 3 && (argtypes[2] ⊑ TypeVar)
        unionall_call = Expr(:foreigncall, QuoteNode(:jl_type_unionall), Any, svec(Any, Any),
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

function ssa_substitute!(idx::Int, @nospecialize(val), arg_replacements::Vector{Any},
                         @nospecialize(spsig), spvals::SimpleVector,
                         linetable_offset::Int32, boundscheck::Symbol, compact::IncrementalCompact)
    compact.result[idx][:flag] &= ~IR_FLAG_INBOUNDS
    compact.result[idx][:line] += linetable_offset
    return ssa_substitute_op!(val, arg_replacements, spsig, spvals, boundscheck)
end

function ssa_substitute_op!(@nospecialize(val), arg_replacements::Vector{Any},
                            @nospecialize(spsig), spvals::SimpleVector, boundscheck::Symbol)
    if isa(val, Argument)
        return arg_replacements[val.n]
    end
    if isa(val, Expr)
        e = val::Expr
        head = e.head
        if head === :static_parameter
            return quoted(spvals[e.args[1]::Int])
        elseif head === :cfunction
            @assert !isa(spsig, UnionAll) || !isempty(spvals)
            e.args[3] = ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), e.args[3], spsig, spvals)
            e.args[4] = svec(Any[
                ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), argt, spsig, spvals)
                for argt in e.args[4]::SimpleVector ]...)
        elseif head === :foreigncall
            @assert !isa(spsig, UnionAll) || !isempty(spvals)
            for i = 1:length(e.args)
                if i == 2
                    e.args[2] = ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), e.args[2], spsig, spvals)
                elseif i == 3
                    e.args[3] = svec(Any[
                        ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), argt, spsig, spvals)
                        for argt in e.args[3]::SimpleVector ]...)
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
    isa(val, Union{SSAValue, NewSSAValue}) && return val # avoid infinite loop
    urs = userefs(val)
    for op in urs
        op[] = ssa_substitute_op!(op[], arg_replacements, spsig, spvals, boundscheck)
    end
    return urs[]
end
