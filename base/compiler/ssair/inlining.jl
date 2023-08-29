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
    # If the function being inlined is a single basic block we can use a
    # simpler inlining algorithm. This flag determines whether that's allowed
    linear_inline_eligible::Bool
    # Effects of the call statement
    effects::Effects
end
function InliningTodo(mi::MethodInstance, ir::IRCode, effects::Effects)
    return InliningTodo(mi, ir, linear_inline_eligible(ir), effects)
end

struct ConstantCase
    val::Any
    ConstantCase(@nospecialize val) = new(val)
end

struct SomeCase
    val::Any
    SomeCase(@nospecialize val) = new(val)
end

struct InvokeCase
    invoke::MethodInstance
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
    fully_covered::Bool
    atype::DataType
    cases::Vector{InliningCase}
    bbs::Vector{Int}
    UnionSplit(fully_covered::Bool, atype::DataType, cases::Vector{InliningCase}) =
        new(fully_covered, atype, cases, Int[])
end

struct InliningEdgeTracker
    edges::Vector{Any}
    invokesig::Union{Nothing,Vector{Any}}
    InliningEdgeTracker(state::InliningState, invokesig::Union{Nothing,Vector{Any}}=nothing) =
        new(state.edges, invokesig)
end

function add_inlining_backedge!((; edges, invokesig)::InliningEdgeTracker, mi::MethodInstance)
    if invokesig === nothing
        push!(edges, mi)
    else # invoke backedge
        push!(edges, invoke_signature(invokesig), mi)
    end
    return nothing
end

function ssa_inlining_pass!(ir::IRCode, state::InliningState, propagate_inbounds::Bool)
    # Go through the function, performing simple inlining (e.g. replacing call by constants
    # and analyzing legality of inlining).
    @timeit "analysis" todo = assemble_inline_todo!(ir, state)
    isempty(todo) && return ir
    # Do the actual inlining for every call we identified
    @timeit "execution" ir = batch_inline!(ir, todo, propagate_inbounds, OptimizationParams(state.interp))
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

    if !isempty(inlinee_cfg.blocks[1].preds)
        need_split_before = true
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
            if !case.linear_inline_eligible
                cfg_inline_item!(ir, idx, case, state, true)
            end
        end
        push!(from_bbs, length(state.new_cfg_blocks))
        # TODO: Right now we unconditionally generate a fallback block
        # in case of subtyping errors - This is probably unnecessary.
        if i != length(cases) || (!fully_covered)
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

# duplicated from IRShow
function normalize_method_name(m)
    if m isa Method
        return m.name
    elseif m isa MethodInstance
        return (m.def::Method).name
    elseif m isa Symbol
        return m
    else
        return Symbol("")
    end
end
@noinline method_name(m::LineInfoNode) = normalize_method_name(m.method)

inline_node_is_duplicate(topline::LineInfoNode, line::LineInfoNode) =
    topline.module === line.module &&
    method_name(topline) === method_name(line) &&
    topline.file === line.file &&
    topline.line === line.line

function ir_inline_linetable!(linetable::Vector{LineInfoNode}, inlinee_ir::IRCode,
                              inlinee::MethodInstance, inlined_at::Int32)
    inlinee_def = inlinee.def::Method
    coverage = coverage_enabled(inlinee_def.module)
    linetable_offset::Int32 = length(linetable)
    # Append the linetable of the inlined function to our line table
    topline::Int32 = linetable_offset + Int32(1)
    coverage_by_path = JLOptions().code_coverage == 3
    push!(linetable, LineInfoNode(inlinee_def.module, inlinee_def.name, inlinee_def.file, inlinee_def.line, inlined_at))
    oldlinetable = inlinee_ir.linetable
    extra_coverage_line = zero(Int32)
    for oldline in eachindex(oldlinetable)
        entry = oldlinetable[oldline]
        if !coverage && coverage_by_path && is_file_tracked(entry.file)
            # include topline coverage entry if in path-specific coverage mode, and any file falls under path
            coverage = true
        end
        newentry = LineInfoNode(entry.module, entry.method, entry.file, entry.line,
            (entry.inlined_at > 0 ? entry.inlined_at + linetable_offset + (oldline == 1) : inlined_at))
        if oldline == 1
            # check for a duplicate on the first iteration (likely true)
            if inline_node_is_duplicate(linetable[topline], newentry)
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

function ir_prepare_inlining!(insert_node!::Inserter, inline_target::Union{IRCode, IncrementalCompact},
                              ir::IRCode, mi::MethodInstance, inlined_at::Int32, argexprs::Vector{Any})
    def = mi.def::Method
    linetable = inline_target isa IRCode ? inline_target.linetable : inline_target.ir.linetable
    topline::Int32 = length(linetable) + Int32(1)
    linetable_offset, extra_coverage_line = ir_inline_linetable!(linetable, ir, mi, inlined_at)
    if extra_coverage_line != 0
        insert_node!(NewInstruction(Expr(:code_coverage_effect), Nothing, extra_coverage_line))
    end
    spvals_ssa = nothing
    if !validate_sparams(mi.sparam_vals)
        # N.B. This works on the caller-side argexprs, (i.e. before the va fixup below)
        spvals_ssa = insert_node!(
            effect_free_and_nothrow(NewInstruction(Expr(:call, Core._compute_sparams, def, argexprs...), SimpleVector, topline)))
    end
    if def.isva
        nargs_def = Int(def.nargs::Int32)
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
    return SSASubstitute(mi, argexprs, spvals_ssa, linetable_offset)
end

function adjust_boundscheck!(inline_compact, idx′, stmt, boundscheck)
    if boundscheck === :off
        length(stmt.args) == 0 && push!(stmt.args, false)
    elseif boundscheck !== :propagate
        length(stmt.args) == 0 && push!(stmt.args, true)
    end
end

function ir_inline_item!(compact::IncrementalCompact, idx::Int, argexprs::Vector{Any},
                         item::InliningTodo, boundscheck::Symbol, todo_bbs::Vector{Tuple{Int, Int}})
    # Ok, do the inlining here
    inlined_at = compact.result[idx][:line]

    ssa_substitute = ir_prepare_inlining!(InsertHere(compact), compact, item.ir, item.mi, inlined_at, argexprs)

    if boundscheck === :default || boundscheck === :propagate
        if (compact.result[idx][:flag] & IR_FLAG_INBOUNDS) != 0
            boundscheck = :off
        end
    end
    # If the iterator already moved on to the next basic block,
    # temporarily re-open in again.
    local return_value
    # Special case inlining that maintains the current basic block if there's only one BB in the target
    new_new_offset = length(compact.new_new_nodes)
    late_fixup_offset = length(compact.late_fixup)
    if item.linear_inline_eligible
        #compact[idx] = nothing
        inline_compact = IncrementalCompact(compact, item.ir, compact.result_idx)
        for ((_, idx′), stmt′) in inline_compact
            # This dance is done to maintain accurate usage counts in the
            # face of rename_arguments! mutating in place - should figure out
            # something better eventually.
            inline_compact[idx′] = nothing
            insert_node! = InsertBefore(inline_compact, SSAValue(idx′))
            stmt′ = ssa_substitute!(insert_node!, inline_compact[SSAValue(idx′)], stmt′,
                                    ssa_substitute, boundscheck)
            if isa(stmt′, ReturnNode)
                val = stmt′.val
                return_value = SSAValue(idx′)
                inline_compact[idx′] = val
                inline_compact.result[idx′][:type] =
                    argextype(val, isa(val, Argument) || isa(val, Expr) ? compact : inline_compact)
                # Everything legal in value position is guaranteed to be effect free in stmt position
                inline_compact.result[idx′][:flag] = IR_FLAG_EFFECT_FREE | IR_FLAG_NOTHROW
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
        need_split_before = !isempty(item.ir.cfg.blocks[1].preds)
        if need_split_before
            finish_current_bb!(compact, 0)
        end
        pn = PhiNode()
        #compact[idx] = nothing
        inline_compact = IncrementalCompact(compact, item.ir, compact.result_idx)
        for ((_, idx′), stmt′) in inline_compact
            inline_compact[idx′] = nothing
            insert_node! = InsertBefore(inline_compact, SSAValue(idx′))
            stmt′ = ssa_substitute!(insert_node!, inline_compact[SSAValue(idx′)], stmt′,
                                    ssa_substitute, boundscheck)
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
    argexprs::Vector{Any}, nargs_def::Int, line_idx::Int32)
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
function ir_inline_unionsplit!(compact::IncrementalCompact, idx::Int, argexprs::Vector{Any},
                               union_split::UnionSplit, boundscheck::Symbol,
                               todo_bbs::Vector{Tuple{Int,Int}}, params::OptimizationParams)
    (; fully_covered, atype, cases, bbs) = union_split
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
        if i != ncases || !fully_covered
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
                NewInstruction(GotoNode(join_bb), Union{}, line))
        else
            insert_node_here!(compact,
                NewInstruction(ReturnNode(), Union{}, line))
        end
        finish_current_bb!(compact, 0)
    end
    bb += 1
    # We're now in the fall through block, decide what to do
    if !fully_covered
        ssa = insert_node_here!(compact, NewInstruction(stmt, typ, line))
        push!(pn.edges, bb)
        push!(pn.values, ssa)
        insert_node_here!(compact, NewInstruction(GotoNode(join_bb), Union{}, line))
        finish_current_bb!(compact, 0)
    end

    # We're now in the join block.
    return insert_node_here!(compact, NewInstruction(pn, typ, line))
end

function batch_inline!(ir::IRCode, todo::Vector{Pair{Int,Any}}, propagate_inbounds::Bool, params::OptimizationParams)
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

    boundscheck = :default
    if boundscheck === :default && propagate_inbounds
        boundscheck = :propagate
    end

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
                # It is possible for GlobalRefs and Exprs to be in argument position
                # at this point in the IR, though in that case they are required
                # to be effect-free. However, we must still move them out of argument
                # position, since `Argument` is allowed in PhiNodes, but `GlobalRef`
                # and `Expr` are not, so a substitution could anger the verifier.
                for aidx in 1:length(argexprs)
                    aexpr = argexprs[aidx]
                    if isa(aexpr, Expr) || isa(aexpr, GlobalRef)
                        ninst = effect_free_and_nothrow(NewInstruction(aexpr, argextype(aexpr, compact), compact.result[idx][:line]))
                        argexprs[aidx] = insert_node_here!(compact, ninst)
                    end
                end
                if isa(item, InliningTodo)
                    compact.ssa_rename[old_idx] = ir_inline_item!(compact, idx, argexprs, item, boundscheck, state.todo_bbs)
                elseif isa(item, UnionSplit)
                    compact.ssa_rename[old_idx] = ir_inline_unionsplit!(compact, idx, argexprs, item, boundscheck, state.todo_bbs, params)
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

function compileable_specialization(mi::MethodInstance, effects::Effects,
        et::InliningEdgeTracker, @nospecialize(info::CallInfo); compilesig_invokes::Bool=true)
    mi_invoke = mi
    method, atype, sparams = mi.def::Method, mi.specTypes, mi.sparam_vals
    if compilesig_invokes
        new_atype = get_compileable_sig(method, atype, sparams)
        new_atype === nothing && return nothing
        if atype !== new_atype
            sp_ = ccall(:jl_type_intersection_with_env, Any, (Any, Any), new_atype, method.sig)::SimpleVector
            if sparams === sp_[2]::SimpleVector
                mi_invoke = specialize_method(method, new_atype, sparams)
                mi_invoke === nothing && return nothing
            end
        end
    else
        # If this caller does not want us to optimize calls to use their
        # declared compilesig, then it is also likely they would handle sparams
        # incorrectly if there were any unknown typevars, so we conservatively return nothing
        if any(@nospecialize(t)->isa(t, TypeVar), mi.sparam_vals)
            return nothing
        end
    end
    add_inlining_backedge!(et, mi) # to the dispatch lookup
    push!(et.edges, method.sig, mi_invoke) # add_inlining_backedge to the invoke call
    return InvokeCase(mi_invoke, effects, info)
end

function compileable_specialization(match::MethodMatch, effects::Effects,
        et::InliningEdgeTracker, @nospecialize(info::CallInfo); compilesig_invokes::Bool=true)
    mi = specialize_method(match)
    return compileable_specialization(mi, effects, et, info; compilesig_invokes)
end

struct CachedResult
    src::Any
    effects::Effects
    CachedResult(@nospecialize(src), effects::Effects) = new(src, effects)
end
@inline function get_cached_result(state::InliningState, mi::MethodInstance)
    code = get(code_cache(state), mi, nothing)
    if code isa CodeInstance
        if use_const_api(code)
            # in this case function can be inlined to a constant
            return ConstantCase(quoted(code.rettype_const))
        else
            src = @atomic :monotonic code.inferred
        end
        effects = decode_effects(code.ipo_purity_bits)
        return CachedResult(src, effects)
    end
    return CachedResult(nothing, Effects())
end

# the general resolver for usual and const-prop'ed calls
function resolve_todo(mi::MethodInstance, result::Union{MethodMatch,InferenceResult},
        argtypes::Vector{Any}, @nospecialize(info::CallInfo), flag::UInt32,
        state::InliningState; invokesig::Union{Nothing,Vector{Any}}=nothing)
    et = InliningEdgeTracker(state, invokesig)

    if isa(result, InferenceResult)
        src = result.src
        effects = result.ipo_effects
        if is_foldable_nothrow(effects)
            res = result.result
            if isa(res, Const) && is_inlineable_constant(res.val)
                # use constant calling convention
                add_inlining_backedge!(et, mi)
                return ConstantCase(quoted(res.val))
            end
        end
    else
        cached_result = get_cached_result(state, mi)
        if cached_result isa ConstantCase
            add_inlining_backedge!(et, mi)
            return cached_result
        end
        (; src, effects) = cached_result
    end

    # the duplicated check might have been done already within `analyze_method!`, but still
    # we need it here too since we may come here directly using a constant-prop' result
    if !OptimizationParams(state.interp).inlining || is_stmt_noinline(flag)
        return compileable_specialization(mi, effects, et, info;
            compilesig_invokes=OptimizationParams(state.interp).compilesig_invokes)
    end

    src = inlining_policy(state.interp, src, info, flag, mi, argtypes)
    src === nothing && return compileable_specialization(mi, effects, et, info;
        compilesig_invokes=OptimizationParams(state.interp).compilesig_invokes)

    add_inlining_backedge!(et, mi)
    return InliningTodo(mi, retrieve_ir_for_inlining(mi, src), effects)
end

# the special resolver for :invoke-d call
function resolve_todo(mi::MethodInstance, argtypes::Vector{Any},
    @nospecialize(info::CallInfo), flag::UInt32, state::InliningState)
    if !OptimizationParams(state.interp).inlining || is_stmt_noinline(flag)
        return nothing
    end

    et = InliningEdgeTracker(state)

    cached_result = get_cached_result(state, mi)
    if cached_result isa ConstantCase
        add_inlining_backedge!(et, mi)
        return cached_result
    end
    (; src, effects) = cached_result

    src = inlining_policy(state.interp, src, info, flag, mi, argtypes)

    src === nothing && return nothing

    add_inlining_backedge!(et, mi)
    return InliningTodo(mi, retrieve_ir_for_inlining(mi, src), effects)
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
    allow_typevars::Bool, invokesig::Union{Nothing,Vector{Any}}=nothing)
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

    if !validate_sparams(match.sparams)
        (allow_typevars && !may_have_fcalls(match.method)) || return nothing
    end

    # Get the specialization for this method signature
    # (later we will decide what to do with it)
    mi = specialize_method(match)
    return resolve_todo(mi, match, argtypes, info, flag, state; invokesig)
end

function retrieve_ir_for_inlining(mi::MethodInstance, src::String)
    src = ccall(:jl_uncompress_ir, Any, (Any, Ptr{Cvoid}, Any), mi.def, C_NULL, src)::CodeInfo
    return inflate_ir!(src, mi)
end
retrieve_ir_for_inlining(mi::MethodInstance, src::CodeInfo) = inflate_ir(src, mi)
retrieve_ir_for_inlining(mi::MethodInstance, ir::IRCode) = copy(ir)

function flags_for_effects(effects::Effects)
    flags::UInt32 = 0
    if is_consistent(effects)
        flags |= IR_FLAG_CONSISTENT
    end
    if is_effect_free(effects)
        flags |= IR_FLAG_EFFECT_FREE
    end
    if is_nothrow(effects)
        flags |= IR_FLAG_NOTHROW
    end
    if is_noub(effects, false)
        flags |= IR_FLAG_NOUB
    end
    return flags
end

function handle_single_case!(todo::Vector{Pair{Int,Any}},
    ir::IRCode, idx::Int, stmt::Expr, @nospecialize(case),
    isinvoke::Bool = false)
    if isa(case, ConstantCase)
        ir[SSAValue(idx)][:stmt] = case.val
    elseif isa(case, InvokeCase)
        is_foldable_nothrow(case.effects) && inline_const_if_inlineable!(ir[SSAValue(idx)]) && return nothing
        isinvoke && rewrite_invoke_exprargs!(stmt)
        stmt.head = :invoke
        pushfirst!(stmt.args, case.invoke)
        ir[SSAValue(idx)][:flag] |= flags_for_effects(case.effects)
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
            for i = (arg_start + 1):length(argtypes)
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
        for i = (arg_start + 1):length(argtypes)
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
    ir::IRCode, idx::Int, stmt::Expr, info::InvokeCallInfo, flag::UInt32,
    sig::Signature, state::InliningState)
    match = info.match
    if !match.fully_covers
        # TODO: We could union split out the signature check and continue on
        return nothing
    end
    result = info.result
    invokesig = sig.argtypes
    if isa(result, ConcreteResult)
        item = concrete_result_item(result, info, state; invokesig)
    else
        argtypes = invoke_rewrite(sig.argtypes)
        if isa(result, ConstPropResult)
            mi = result.result.linfo
            validate_sparams(mi.sparam_vals) || return nothing
            if Union{} !== argtypes_to_type(argtypes) <: mi.def.sig
                item = resolve_todo(mi, result.result, argtypes, info, flag, state; invokesig)
                handle_single_case!(todo, ir, idx, stmt, item, true)
                return nothing
            end
        end
        item = analyze_method!(match, argtypes, info, flag, state; allow_typevars=false, invokesig)
    end
    handle_single_case!(todo, ir, idx, stmt, item, true)
    return nothing
end

function invoke_signature(argtypes::Vector{Any})
    ft, argtyps = widenconst(argtypes[2]), instanceof_tfunc(widenconst(argtypes[3]))[1]
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
function check_effect_free!(ir::IRCode, idx::Int, @nospecialize(stmt), @nospecialize(rt), state::InliningState)
    return check_effect_free!(ir, idx, stmt, rt, optimizer_lattice(state.interp))
end
function check_effect_free!(ir::IRCode, idx::Int, @nospecialize(stmt), @nospecialize(rt), 𝕃ₒ::AbstractLattice)
    (consistent, effect_free_and_nothrow, nothrow) = stmt_effect_flags(𝕃ₒ, stmt, rt, ir)
    if consistent
        ir.stmts[idx][:flag] |= IR_FLAG_CONSISTENT
    end
    if effect_free_and_nothrow
        ir.stmts[idx][:flag] |= IR_FLAG_EFFECT_FREE | IR_FLAG_NOTHROW
    elseif nothrow
        ir.stmts[idx][:flag] |= IR_FLAG_NOTHROW
    end
    if !isexpr(stmt, :call) && !isexpr(stmt, :invoke)
        # There is a bit of a subtle point here, which is that some non-call
        # statements (e.g. PiNode) can be UB:, however, we consider it
        # illegal to introduce such statements that actually cause UB (for any
        # input). Ideally that'd be handled at insertion time (TODO), but for
        # the time being just do that here.
        ir.stmts[idx][:flag] |= IR_FLAG_NOUB
    end
    return effect_free_and_nothrow
end

# Handles all analysis and inlining of intrinsics and builtins. In particular,
# this method does not access the method table or otherwise process generic
# functions.
function process_simple!(todo::Vector{Pair{Int,Any}}, ir::IRCode, idx::Int, state::InliningState)
    inst = ir[SSAValue(idx)]
    stmt = inst[:stmt]
    rt = inst[:type]
    if !(stmt isa Expr)
        check_effect_free!(ir, idx, stmt, rt, state)
        return nothing
    end
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
        check_effect_free!(ir, idx, stmt, rt, state)
        return nothing
    end

    sig = call_sig(ir, stmt)
    sig === nothing && return nothing

    # Handle _apply_iterate
    sig = inline_apply!(todo, ir, idx, stmt, sig, state)
    sig === nothing && return nothing

    # Check if we match any of the early inliners
    earlyres = early_inline_special_case(ir, stmt, rt, sig, state)
    if isa(earlyres, SomeCase)
        inst[:stmt] = earlyres.val
        return nothing
    end

    if check_effect_free!(ir, idx, stmt, rt, state)
        if sig.f === typeassert || ⊑(optimizer_lattice(state.interp), sig.ft, typeof(typeassert))
            # typeassert is a no-op if effect free
            inst[:stmt] = stmt.args[2]
            return nothing
        end
    end

    if (sig.f !== Core.invoke && sig.f !== Core.finalizer && sig.f !== modifyfield!) &&
        is_builtin(optimizer_lattice(state.interp), sig)
        # No inlining for builtins (other invoke/apply/typeassert/finalizer)
        return nothing
    end

    # Special case inliners for regular functions
    lateres = late_inline_special_case!(ir, idx, stmt, rt, sig, state)
    if isa(lateres, SomeCase)
        inst[:stmt] = lateres.val
        check_effect_free!(ir, idx, lateres.val, rt, state)
        return nothing
    end

    return stmt, sig
end

function handle_any_const_result!(cases::Vector{InliningCase},
    @nospecialize(result), match::MethodMatch, argtypes::Vector{Any},
    @nospecialize(info::CallInfo), flag::UInt32, state::InliningState;
    allow_abstract::Bool, allow_typevars::Bool)
    if isa(result, ConcreteResult)
        return handle_concrete_result!(cases, result, info, state)
    end
    if isa(result, SemiConcreteResult)
        result = inlining_policy(state.interp, result, info, flag, result.mi, argtypes)
        if isa(result, SemiConcreteResult)
            return handle_semi_concrete_result!(cases, result, info, flag, state; allow_abstract)
        end
    end
    if isa(result, ConstPropResult)
        return handle_const_prop_result!(cases, result, argtypes, info, flag, state; allow_abstract, allow_typevars)
    else
        @assert result === nothing
        return handle_match!(cases, match, argtypes, info, flag, state; allow_abstract, allow_typevars)
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
    local handled_all_cases::Bool = true
    local revisit_idx = nothing
    local only_method = nothing
    local meth::MethodLookupResult
    local all_result_count = 0
    local joint_effects::Effects = EFFECTS_TOTAL
    local fully_covered::Bool = true
    for i = 1:nunion
        meth = getsplit(info, i)
        if meth.ambig
            # Too many applicable methods
            # Or there is a (partial?) ambiguity
            return nothing
        elseif length(meth) == 0
            # No applicable methods; try next union split
            handled_all_cases = false
            continue
        else
            if length(meth) == 1 && only_method !== false
                if only_method === nothing
                    only_method = meth[1].method
                elseif only_method !== meth[1].method
                    only_method = false
                end
            else
                only_method = false
            end
        end
        local split_fully_covered::Bool = false
        for (j, match) in enumerate(meth)
            all_result_count += 1
            result = getresult(info, all_result_count)
            joint_effects = merge_effects(joint_effects, info_effects(result, match, state))
            split_fully_covered |= match.fully_covers
            if !validate_sparams(match.sparams)
                if !match.fully_covers
                    handled_all_cases = false
                    continue
                end
                if revisit_idx === nothing
                    revisit_idx = (i, j, all_result_count)
                else
                    handled_all_cases = false
                    revisit_idx = nothing
                end
            else
                handled_all_cases &= handle_any_const_result!(cases,
                    result, match, argtypes, info, flag, state; allow_abstract=true, allow_typevars=false)
            end
        end
        fully_covered &= split_fully_covered
    end

    (handled_all_cases && fully_covered) || (joint_effects = Effects(joint_effects; nothrow=false))

    if handled_all_cases && revisit_idx !== nothing
        # we handled everything except one match with unmatched sparams,
        # so try to handle it by bypassing validate_sparams
        (i, j, k) = revisit_idx
        match = getsplit(info, i)[j]
        result = getresult(info, k)
        handled_all_cases &= handle_any_const_result!(cases,
            result, match, argtypes, info, flag, state; allow_abstract=true, allow_typevars=true)
    elseif length(cases) == 0 && only_method isa Method
        # if the signature is fully covered and there is only one applicable method,
        # we can try to inline it even in the presence of unmatched sparams
        # -- But don't try it if we already tried to handle the match in the revisit_idx
        # case, because that'll (necessarily) be the same method.
        if nsplit(info)::Int > 1
            atype = argtypes_to_type(argtypes)
            (metharg, methsp) = ccall(:jl_type_intersection_with_env, Any, (Any, Any), atype, only_method.sig)::SimpleVector
            match = MethodMatch(metharg, methsp::SimpleVector, only_method, true)
            result = nothing
        else
            @assert length(meth) == 1
            match = meth[1]
            result = getresult(info, 1)
        end
        handle_any_const_result!(cases,
            result, match, argtypes, info, flag, state; allow_abstract=true, allow_typevars=true)
        fully_covered = handled_all_cases = match.fully_covers
    elseif !handled_all_cases
        # if we've not seen all candidates, union split is valid only for dispatch tuples
        filter!(case::InliningCase->isdispatchtuple(case.sig), cases)
    end

    return cases, (handled_all_cases & fully_covered), joint_effects
end

function handle_call!(todo::Vector{Pair{Int,Any}},
    ir::IRCode, idx::Int, stmt::Expr, @nospecialize(info::CallInfo), flag::UInt32, sig::Signature,
    state::InliningState)
    cases = compute_inlining_cases(info, flag, sig, state)
    cases === nothing && return nothing
    cases, all_covered, joint_effects = cases
    handle_cases!(todo, ir, idx, stmt, argtypes_to_type(sig.argtypes), cases,
        all_covered, joint_effects)
end

function handle_match!(cases::Vector{InliningCase},
    match::MethodMatch, argtypes::Vector{Any}, @nospecialize(info::CallInfo), flag::UInt32,
    state::InliningState;
    allow_abstract::Bool, allow_typevars::Bool)
    spec_types = match.spec_types
    allow_abstract || isdispatchtuple(spec_types) || return false
    # We may see duplicated dispatch signatures here when a signature gets widened
    # during abstract interpretation: for the purpose of inlining, we can just skip
    # processing this dispatch candidate (unless unmatched type parameters are present)
    !allow_typevars && _any(case->case.sig === spec_types, cases) && return true
    item = analyze_method!(match, argtypes, info, flag, state; allow_typevars)
    item === nothing && return false
    push!(cases, InliningCase(spec_types, item))
    return true
end

function handle_const_prop_result!(cases::Vector{InliningCase},
    result::ConstPropResult, argtypes::Vector{Any}, @nospecialize(info::CallInfo),
    flag::UInt32, state::InliningState;
    allow_abstract::Bool, allow_typevars::Bool)
    mi = result.result.linfo
    spec_types = mi.specTypes
    allow_abstract || isdispatchtuple(spec_types) || return false
    if !validate_sparams(mi.sparam_vals)
        (allow_typevars && !may_have_fcalls(mi.def::Method)) || return false
    end
    item = resolve_todo(mi, result.result, argtypes, info, flag, state)
    item === nothing && return false
    push!(cases, InliningCase(spec_types, item))
    return true
end

function semiconcrete_result_item(result::SemiConcreteResult,
        @nospecialize(info::CallInfo), flag::UInt32, state::InliningState)
    mi = result.mi
    if !OptimizationParams(state.interp).inlining || is_stmt_noinline(flag)
        et = InliningEdgeTracker(state)
        return compileable_specialization(mi, result.effects, et, info;
            compilesig_invokes=OptimizationParams(state.interp).compilesig_invokes)
    else
        return InliningTodo(mi, retrieve_ir_for_inlining(mi, result.ir), result.effects)
    end
end

function handle_semi_concrete_result!(cases::Vector{InliningCase}, result::SemiConcreteResult,
        @nospecialize(info::CallInfo), flag::UInt32, state::InliningState;
        allow_abstract::Bool)
    mi = result.mi
    spec_types = mi.specTypes
    allow_abstract || isdispatchtuple(spec_types) || return false
    validate_sparams(mi.sparam_vals) || return false
    item = semiconcrete_result_item(result, info, flag, state)
    item === nothing && return false
    push!(cases, InliningCase(spec_types, item))
    return true
end

function handle_concrete_result!(cases::Vector{InliningCase}, result::ConcreteResult, @nospecialize(info::CallInfo), state::InliningState)
    case = concrete_result_item(result, info, state)
    case === nothing && return false
    push!(cases, InliningCase(result.mi.specTypes, case))
    return true
end

may_inline_concrete_result(result::ConcreteResult) =
    isdefined(result, :result) && is_inlineable_constant(result.result)

function concrete_result_item(result::ConcreteResult, @nospecialize(info::CallInfo), state::InliningState;
    invokesig::Union{Nothing,Vector{Any}}=nothing)
    if !may_inline_concrete_result(result)
        et = InliningEdgeTracker(state, invokesig)
        return compileable_specialization(result.mi, result.effects, et, info;
            compilesig_invokes=OptimizationParams(state.interp).compilesig_invokes)
    end
    @assert result.effects === EFFECTS_TOTAL
    return ConstantCase(quoted(result.result))
end

function handle_cases!(todo::Vector{Pair{Int,Any}}, ir::IRCode, idx::Int, stmt::Expr,
    @nospecialize(atype), cases::Vector{InliningCase}, fully_covered::Bool,
    joint_effects::Effects)
    # If we only have one case and that case is fully covered, we may either
    # be able to do the inlining now (for constant cases), or push it directly
    # onto the todo list
    if fully_covered && length(cases) == 1
        handle_single_case!(todo, ir, idx, stmt, cases[1].item)
    elseif length(cases) > 0
        isa(atype, DataType) || return nothing
        for case in cases
            isa(case.sig, DataType) || return nothing
        end
        push!(todo, idx=>UnionSplit(fully_covered, atype, cases))
    else
        ir[SSAValue(idx)][:flag] |= flags_for_effects(joint_effects)
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
        item = resolve_todo(mi, result.result, sig.argtypes, info, flag, state)
    elseif isa(result, ConcreteResult)
        item = concrete_result_item(result, info, state)
    else
        if isa(result, SemiConcreteResult)
            result = inlining_policy(state.interp, result, info, flag, result.mi, sig.argtypes)
        end
        if isa(result, SemiConcreteResult)
            item = semiconcrete_result_item(result, info, flag, state)
        else
            item = analyze_method!(info.match, sig.argtypes, info, flag, state; allow_typevars=false)
        end
    end
    handle_single_case!(todo, ir, idx, stmt, item)
    return nothing
end

function handle_modifyfield!_call!(ir::IRCode, idx::Int, stmt::Expr, info::ModifyFieldInfo, state::InliningState)
    info = info.info
    info isa MethodResultPure && (info = info.info)
    info isa ConstCallInfo && (info = info.call)
    info isa MethodMatchInfo || return nothing
    length(info.results) == 1 || return nothing
    match = info.results[1]::MethodMatch
    match.fully_covers || return nothing
    case = compileable_specialization(match, Effects(), InliningEdgeTracker(state), info;
        compilesig_invokes=OptimizationParams(state.interp).compilesig_invokes)
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
            push!(stmt.args, true)
            push!(stmt.args, item1.mi)
        elseif isa(item1, InvokeCase)
            push!(stmt.args, false)
            push!(stmt.args, item1.invoke)
        elseif isa(item1, ConstantCase)
            push!(stmt.args, nothing)
            push!(stmt.args, item1.val)
        end
    end
    return nothing
end

function handle_invoke_expr!(todo::Vector{Pair{Int,Any}},
    idx::Int, stmt::Expr, @nospecialize(info::CallInfo), flag::UInt32, sig::Signature, state::InliningState)
    mi = stmt.args[1]::MethodInstance
    case = resolve_todo(mi, sig.argtypes, info, flag, state)
    if case !== nothing
        push!(todo, idx=>(case::InliningTodo))
    end
    return nothing
end

function inline_const_if_inlineable!(inst::Instruction)
    rt = inst[:type]
    if rt isa Const && is_inlineable_constant(rt.val)
        inst[:stmt] = quoted(rt.val)
        return true
    end
    inst[:flag] |= IR_FLAG_EFFECT_FREE | IR_FLAG_NOTHROW
    return false
end

function assemble_inline_todo!(ir::IRCode, state::InliningState)
    todo = Pair{Int, Any}[]

    for idx in 1:length(ir.stmts)
        simpleres = process_simple!(todo, ir, idx, state)
        simpleres === nothing && continue
        stmt, sig = simpleres

        flag = ir.stmts[idx][:flag]
        info = ir.stmts[idx][:info]

        # `NativeInterpreter` won't need this, but provide a support for `:invoke` exprs here
        # for external `AbstractInterpreter`s that may run the inlining pass multiple times
        if isexpr(stmt, :invoke)
            handle_invoke_expr!(todo, idx, stmt, info, flag, sig, state)
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
        elseif isa(info, ModifyFieldInfo)
            handle_modifyfield!_call!(ir, idx, stmt, info, state)
        elseif isa(info, InvokeCallInfo)
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

function early_inline_special_case(
    ir::IRCode, stmt::Expr, @nospecialize(type), sig::Signature,
    state::InliningState)
    OptimizationParams(state.interp).inlining || return nothing
    (; f, ft, argtypes) = sig

    if isa(type, Const) # || isconstType(type)
        val = type.val
        is_inlineable_constant(val) || return nothing
        if isa(f, IntrinsicFunction)
            if is_pure_intrinsic_infer(f) && intrinsic_nothrow(f, argtypes[2:end])
                return SomeCase(quoted(val))
            end
        elseif contains_is(_PURE_BUILTINS, f)
            return SomeCase(quoted(val))
        elseif contains_is(_EFFECT_FREE_BUILTINS, f)
            if _builtin_nothrow(optimizer_lattice(state.interp), f, argtypes[2:end], type)
                return SomeCase(quoted(val))
            end
        elseif f === Core.get_binding_type
            length(argtypes) == 3 || return nothing
            if get_binding_type_effect_free(argtypes[2], argtypes[3])
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
        end
    end
    return nothing
end

# special-case some regular method calls whose results are not folded within `abstract_call_known`
# (and thus `early_inline_special_case` doesn't handle them yet)
# NOTE we manually inline the method bodies, and so the logic here needs to precisely sync with their definitions
function late_inline_special_case!(
    ir::IRCode, idx::Int, stmt::Expr, @nospecialize(type), sig::Signature,
    state::InliningState)
    OptimizationParams(state.interp).inlining || return nothing
    (; f, ft, argtypes) = sig
    if length(argtypes) == 3 && istopfunction(f, :!==)
        # special-case inliner for !== that precedes _methods_by_ftype union splitting
        # and that works, even though inference generally avoids inferring the `!==` Method
        if isa(type, Const)
            return SomeCase(quoted(type.val))
        end
        cmp_call = Expr(:call, GlobalRef(Core, :(===)), stmt.args[2], stmt.args[3])
        cmp_call_ssa = insert_node!(ir, idx, effect_free_and_nothrow(NewInstruction(cmp_call, Bool)))
        not_call = Expr(:call, GlobalRef(Core.Intrinsics, :not_int), cmp_call_ssa)
        return SomeCase(not_call)
    elseif length(argtypes) == 3 && istopfunction(f, :(>:))
        # special-case inliner for issupertype
        # that works, even though inference generally avoids inferring the `>:` Method
        if isa(type, Const) && _builtin_nothrow(optimizer_lattice(state.interp), <:, Any[argtypes[3], argtypes[2]], type)
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

struct SSASubstitute
    mi::MethodInstance
    arg_replacements::Vector{Any}
    spvals_ssa::Union{Nothing,SSAValue}
    linetable_offset::Int32
end

function ssa_substitute!(insert_node!::Inserter, subst_inst::Instruction, @nospecialize(val),
                         ssa_substitute::SSASubstitute, boundscheck::Symbol)
    subst_inst[:line] += ssa_substitute.linetable_offset
    return ssa_substitute_op!(insert_node!, subst_inst, val, ssa_substitute, boundscheck)
end

function insert_spval!(insert_node!::Inserter, spvals_ssa::SSAValue, spidx::Int, do_isdefined::Bool)
    ret = insert_node!(
        effect_free_and_nothrow(NewInstruction(Expr(:call, Core._svec_ref, spvals_ssa, spidx), Any)))
    tcheck_not = nothing
    if do_isdefined
        tcheck = insert_node!(
            effect_free_and_nothrow(NewInstruction(Expr(:call, Core.isa, ret, Core.TypeVar), Bool)))
        tcheck_not = insert_node!(
            effect_free_and_nothrow(NewInstruction(Expr(:call, not_int, tcheck), Bool)))
    end
    return (ret, tcheck_not)
end

function ssa_substitute_op!(insert_node!::Inserter, subst_inst::Instruction, @nospecialize(val),
                            ssa_substitute::SSASubstitute, boundscheck::Symbol)
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
                maybe_undef = (flag & IR_FLAG_NOTHROW) == 0 && isa(val, TypeVar)
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
    isa(val, Union{SSAValue, NewSSAValue}) && return val # avoid infinite loop
    urs = userefs(val)
    for op in urs
        op[] = ssa_substitute_op!(insert_node!, subst_inst, op[], ssa_substitute, boundscheck)
    end
    return urs[]
end
