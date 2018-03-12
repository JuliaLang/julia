Core.PhiNode() = PhiNode(Any[], Any[])
@inline isexpr(@nospecialize(stmt), head::Symbol) = isa(stmt, Expr) && stmt.head === head

struct Argument
    n::Int
end

struct GotoIfNot
    cond
    dest::Int
    GotoIfNot(@nospecialize(cond), dest::Int) = new(cond, dest)
end

struct ReturnNode
    val
    ReturnNode(@nospecialize(val)) = new(val)
    # unassigned val indicates unreachable
    ReturnNode() = new()
end

"""
Like UnitRange{Int}, but can handle the `last` field, being temporarily
< first (this can happen during compacting)
"""
struct StmtRange <: AbstractUnitRange{Int}
    first::Int
    last::Int
end
first(r::StmtRange) = r.first
last(r::StmtRange) = r.last
start(r::StmtRange) = 0
done(r::StmtRange, state) = r.last - r.first < state
next(r::StmtRange, state) = (r.first + state, state + 1)

StmtRange(range::UnitRange{Int}) = StmtRange(first(range), last(range))

struct BasicBlock
    stmts::StmtRange
    preds::Vector{Int}
    succs::Vector{Int}
end
function BasicBlock(stmts::StmtRange)
    BasicBlock(stmts, Int[], Int[])
end
function BasicBlock(old_bb, stmts)
    BasicBlock(stmts, old_bb.preds, old_bb.succs)
end

struct CFG
    blocks::Vector{BasicBlock}
    index::Vector{Int}
end

function block_for_inst(index, inst)
    searchsortedfirst(index, inst, lt=(<=))
end
block_for_inst(cfg::CFG, inst) = block_for_inst(cfg.index, inst)

function compute_basic_blocks(stmts::Vector{Any})
    jump_dests = IdSet{Int}(1)
    # First go through and compute jump destinations
    for (idx, stmt) in pairs(stmts)
        # Terminators
        if isa(stmt, Union{GotoIfNot, GotoNode, ReturnNode})
            if isa(stmt, GotoIfNot)
                push!(jump_dests, idx+1)
                push!(jump_dests, stmt.dest)
            else
                # This is a fake dest to force the next stmt to start a bb
                idx < length(stmts) && push!(jump_dests, idx+1)
                if isa(stmt, GotoNode)
                    push!(jump_dests, stmt.label)
                end
            end
        end
    end
    bb_starts = sort(collect(jump_dests))
    for i = length(stmts):-1:1
        if stmts[i] != nothing
            push!(bb_starts, i+1)
            break
        end
    end
    # Compute ranges
    basic_block_index = Int[]
    blocks = BasicBlock[]
    sizehint!(blocks, length(bb_starts)-1)
    for (first, last) in Iterators.zip(bb_starts, Iterators.drop(bb_starts, 1))
        push!(basic_block_index, first)
        push!(blocks, BasicBlock(StmtRange(first, last-1)))
    end
    popfirst!(basic_block_index)
    # Compute successors/predecessors
    for (num, b) in pairs(blocks)
        terminator = stmts[last(b.stmts)]
        # Conditional Branch
        if isa(terminator, GotoIfNot)
            block′ = block_for_inst(basic_block_index, terminator.dest)
            push!(blocks[block′].preds, num)
            push!(b.succs, block′)
        end
        if isa(terminator, GotoNode)
            block′ = block_for_inst(basic_block_index, terminator.label)
            push!(blocks[block′].preds, num)
            push!(b.succs, block′)
        elseif !isa(terminator, ReturnNode)
            if num + 1 <= length(blocks)
                push!(blocks[num+1].preds, num)
                push!(b.succs, num+1)
            end
        end
    end
    CFG(blocks, basic_block_index)
end

function first_insert_for_bb(code, cfg, block)
    for idx in cfg.blocks[block].stmts
        stmt = code[idx]
        if !isa(stmt, LabelNode) && !isa(stmt, PhiNode)
            return idx
        end
    end
end


const NewNode = Tuple{Int, Any, Any, #=LineNumber=#Int}

struct IRCode
    stmts::Vector{Any}
    types::Vector{Any}
    lines::Vector{Int}
    argtypes::Vector{Any}
    cfg::CFG
    new_nodes::Vector{NewNode}
    mod::Module
    meta::Vector{Any}

    function IRCode(stmts::Vector{Any}, lines::Vector{Int}, cfg::CFG, argtypes::Vector{Any}, mod::Module, meta::Vector{Any})
        return new(stmts, Any[], lines, argtypes, cfg, NewNode[], mod, meta)
    end
    function IRCode(ir::IRCode, stmts::Vector{Any}, types::Vector{Any}, lines::Vector{Int}, cfg::CFG, new_nodes::Vector{NewNode})
        return new(stmts, types, lines, ir.argtypes, cfg, new_nodes, ir.mod, ir.meta)
    end
end

function getindex(x::IRCode, s::SSAValue)
    if s.id <= length(x.stmts)
        return x.stmts[s.id]
    else
        return x.new_nodes[s.id - length(x.stmts)][3]
    end
end

function setindex!(x::IRCode, repl, s::SSAValue)
    @assert s.id <= length(x.stmts)
    x.stmts[s.id] = repl
    nothing
end


struct OldSSAValue
    id::Int
end

struct NewSSAValue
    id::Int
end

mutable struct UseRefIterator
    stmt::Any
end
getindex(it::UseRefIterator) = it.stmt

struct UseRef
    urs::UseRefIterator
    use::Int
end

struct OOBToken
end

struct UndefToken
end

function getindex(x::UseRef)
    stmt = x.urs.stmt
    if isa(stmt, Expr) && stmt.head === :(=)
        rhs = stmt.args[2]
        if isa(rhs, Expr) && is_relevant_expr(rhs)
            x.use > length(rhs.args) && return OOBToken()
            return rhs.args[x.use]
        end
        x.use == 1 || return OOBToken()
        return rhs
    elseif isa(stmt, Expr) && is_relevant_expr(stmt)
        x.use > length(stmt.args) && return OOBToken()
        return stmt.args[x.use]
    elseif isa(stmt, GotoIfNot)
        x.use == 1 || return OOBToken()
        return stmt.cond
    elseif isa(stmt, ReturnNode) || isa(stmt, PiNode)
        isdefined(stmt, :val) || return OOBToken()
        x.use == 1 || return OOBToken()
        return stmt.val
    elseif isa(stmt, PhiNode)
        x.use > length(stmt.values) && return OOBToken()
        isassigned(stmt.values, x.use) || return UndefToken()
        return stmt.values[x.use]
    else
        return OOBToken()
    end
end

function is_relevant_expr(e::Expr)
    return e.head in (:call, :invoke, :new, :(=), :(&),
                      :gc_preserve_begin, :gc_preserve_end,
                      :foreigncall, :isdefined, :copyast,
                      :undefcheck, :throw_undef_if_not)
end

function setindex!(x::UseRef, @nospecialize(v))
    stmt = x.urs.stmt
    if isa(stmt, Expr) && stmt.head === :(=)
        rhs = stmt.args[2]
        if isa(rhs, Expr) && is_relevant_expr(rhs)
            x.use > length(rhs.args) && throw(BoundsError())
            rhs.args[x.use] = v
        else
            x.use == 1 || throw(BoundsError())
            stmt.args[2] = v
        end
    elseif isa(stmt, Expr) && is_relevant_expr(stmt)
        x.use > length(stmt.args) && throw(BoundsError())
        stmt.args[x.use] = v
    elseif isa(stmt, GotoIfNot)
        x.use == 1 || throw(BoundsError())
        x.urs.stmt = GotoIfNot(v, stmt.dest)
    elseif isa(stmt, ReturnNode)
        x.use == 1 || throw(BoundsError())
        x.urs.stmt = typeof(stmt)(v)
    elseif isa(stmt, PiNode)
        x.use == 1 || throw(BoundsError())
        x.urs.stmt = typeof(stmt)(v, stmt.typ)
    elseif isa(stmt, PhiNode)
        x.use > length(stmt.values) && throw(BoundsError())
        isassigned(stmt.values, x.use) || throw(BoundsError())
        stmt.values[x.use] = v
    else
        throw(BoundsError())
    end
    return x
end

function userefs(@nospecialize(x))
    if (isa(x, Expr) && is_relevant_expr(x)) ||
        isa(x, Union{GotoIfNot, ReturnNode, PiNode, PhiNode})
        UseRefIterator(x)
    else
        ()
    end
end

start(it::UseRefIterator) = 1
function next(it::UseRefIterator, use)
    x = UseRef(it, use)
    v = x[]
    v === UndefToken() && return next(it, use + 1)
    x, use + 1
end
function done(it::UseRefIterator, use)
    x, _ = next(it, use)
    v = x[]
    v === OOBToken() && return true
    false
end

function scan_ssa_use!(used, @nospecialize(stmt))
    if isa(stmt, SSAValue)
        push!(used, stmt.id)
    end
    for useref in userefs(stmt)
        val = useref[]
        if isa(val, SSAValue)
            push!(used, val.id)
        end
    end
end

function ssamap(f, @nospecialize(stmt))
    urs = userefs(stmt)
    urs === () && return stmt
    for op in urs
        val = op[]
        if isa(val, SSAValue)
            op[] = f(val)
        end
    end
    urs[]
end

function foreachssa(f, @nospecialize(stmt))
    for op in userefs(stmt)
        val = op[]
        if isa(val, SSAValue)
            f(val)
        end
    end
end

function insert_node!(ir::IRCode, pos::Int, @nospecialize(typ), @nospecialize(val))
    line = ir.lines[pos]
    push!(ir.new_nodes, (pos, typ, val, line))
    return SSAValue(length(ir.stmts) + length(ir.new_nodes))
end

# For bootstrapping
function my_sortperm(v)
    p = Vector{Int}(undef, length(v))
    for i = 1:length(v)
        p[i] = i
    end
    sort!(p, Sort.DEFAULT_UNSTABLE, Order.Perm(Sort.Forward,v))
    p
end

mutable struct IncrementalCompact
    ir::IRCode
    result::Vector{Any}
    result_types::Vector{Any}
    result_lines::Vector{Int}
    ssa_rename::Vector{Any}
    used_ssas::Vector{Int}
    late_fixup::Vector{Int}
    # This could be Stateful, but bootstrapping doesn't like that
    perm::Vector{Int}
    new_nodes_idx::Int
    idx::Int
    result_idx::Int
    function IncrementalCompact(code::IRCode)
        perm = my_sortperm(Int[code.new_nodes[i][1] for i in 1:length(code.new_nodes)])
        new_len = length(code.stmts) + length(code.new_nodes)
        result = Array{Any}(undef, new_len)
        result_types = Array{Any}(undef, new_len)
        result_lines = Array{Int}(undef, new_len)
        ssa_rename = Any[SSAValue(i) for i = 1:new_len]
        used_ssas = fill(0, new_len)
        late_fixup = Vector{Int}()
        return new(code, result, result_types, result_lines, ssa_rename, used_ssas, late_fixup, perm, 1, 1, 1)
    end
end

struct TypesView
    ir::Union{IRCode, IncrementalCompact}
end
types(ir::Union{IRCode, IncrementalCompact}) = TypesView(ir)

function getindex(compact::IncrementalCompact, idx)
    if idx < compact.result_idx
        return compact.result[idx]
    else
        return compact.ir.stmts[idx]
    end
end

function getindex(view::TypesView, v::OldSSAValue)
    return view.ir.ir.types[v.id]
end

function setindex!(compact::IncrementalCompact, v, idx)
    if idx < compact.result_idx
        # Kill count for current uses
        for ops in userefs(compact.result[idx])
            val = ops[]
            isa(val, SSAValue) && (compact.used_ssas[val.id] -= 1)
        end
        compact.result[idx] = v
        # Add count for new use
        if isa(v, SSAValue)
            compact.used_ssas[v.id] += 1
        else
            for ops in userefs(compact.result[idx])
                val = ops[]
                isa(val, SSAValue) && (compact.used_ssas[val.id] += 1)
            end
        end
    else
        compact.ir.stmts[idx] = v
    end
    return nothing
end

function getindex(view::TypesView, idx)
    isa(idx, SSAValue) && (idx = idx.id)
    if isa(view.ir, IncrementalCompact) && idx < view.ir.result_idx
        return view.ir.result_types[idx]
    else
        ir = isa(view.ir, IncrementalCompact) ? view.ir.ir : view.ir
        if idx <= length(ir.types)
            return ir.types[idx]
        else
            return ir.new_nodes[idx - length(ir.types)][2]
        end
    end
end

# maybe use expr_type?
function value_typ(ir::IRCode, value)
    isa(value, SSAValue) && return ir.types[value.id]
    isa(value, GlobalRef) && return abstract_eval_global(value.mod, value.name)
    isa(value, Argument) && return ir.argtypes[value.n]
    # TODO: isa QuoteNode, etc.
    return typeof(value)
end

function value_typ(ir::IncrementalCompact, value)
    isa(value, SSAValue) && return types(ir)[value.id]
    isa(value, GlobalRef) && return abstract_eval_global(value.mod, value.name)
    isa(value, Argument) && return ir.ir.argtypes[value.n]
    # TODO: isa QuoteNode, etc.
    return typeof(value)
end


start(compact::IncrementalCompact) = (1,1,1)
function done(compact::IncrementalCompact, (idx, _a, _b)::Tuple{Int, Int, Int})
    return idx > length(compact.ir.stmts) && (compact.new_nodes_idx > length(compact.perm))
end

function process_node!(result::Vector{Any}, result_idx::Int, ssa_rename::Vector{Any},
        late_fixup::Vector{Int}, used_ssas::Vector{Int}, @nospecialize(stmt),
        idx::Int, processed_idx::Int)
    ssa_rename[idx] = SSAValue(result_idx)
    if stmt === nothing
        ssa_rename[idx] = stmt
    elseif isa(stmt, GotoNode) || isa(stmt, GlobalRef)
        result[result_idx] = stmt
        result_idx += 1
    elseif isa(stmt, Expr) || isa(stmt, PiNode) || isa(stmt, GotoIfNot) || isa(stmt, ReturnNode)
        result[result_idx] = renumber_ssa!(stmt, ssa_rename, true, used_ssas)
        result_idx += 1
    elseif isa(stmt, PhiNode)
        values = Vector{Any}(undef, length(stmt.values))
        for i = 1:length(stmt.values)
            isassigned(stmt.values, i) || continue
            val = stmt.values[i]
            if isa(val, SSAValue)
                if val.id > processed_idx
                    push!(late_fixup, result_idx)
                    val = OldSSAValue(val.id)
                else
                    val = renumber_ssa!(val, ssa_rename, true, used_ssas)
                end
            end
            values[i] = val
        end
        result[result_idx] = PhiNode(stmt.edges, values)
        result_idx += 1
    elseif isa(stmt, SSAValue)
        # identity assign, replace uses of this ssa value with its result
        stmt = ssa_rename[stmt.id]
        ssa_rename[idx] = stmt
    else
        # Constant assign, replace uses of this ssa value with its result
        ssa_rename[idx] = stmt
    end
    return result_idx
end
function process_node!(compact::IncrementalCompact, result_idx::Int, @nospecialize(stmt), idx::Int, processed_idx::Int)
    return process_node!(compact.result, result_idx, compact.ssa_rename,
        compact.late_fixup, compact.used_ssas, stmt, idx, processed_idx)
end

function next(compact::IncrementalCompact, (idx, active_bb, old_result_idx)::Tuple{Int, Int, Int})
    if length(compact.result) < old_result_idx
        resize!(compact.result, old_result_idx)
        resize!(compact.result_types, old_result_idx)
        resize!(compact.result_lines, old_result_idx)
    end
    bb = compact.ir.cfg.blocks[active_bb]
    if compact.new_nodes_idx <= length(compact.perm) && compact.ir.new_nodes[compact.perm[compact.new_nodes_idx]][1] == idx
        new_idx = compact.perm[compact.new_nodes_idx]
        compact.new_nodes_idx += 1
        _, typ, new_node, new_line = compact.ir.new_nodes[new_idx]
        new_idx += length(compact.ir.stmts)
        compact.result_types[old_result_idx] = typ
        compact.result_lines[old_result_idx] = new_line
        result_idx = process_node!(compact, old_result_idx, new_node, new_idx, idx)
        (old_result_idx == result_idx) && return next(compact, (idx, active_bb, result_idx))
        compact.result_idx = result_idx
        return (old_result_idx, compact.result[old_result_idx]), (compact.idx, active_bb, compact.result_idx)
    end
    # This will get overwritten in future iterations if
    # result_idx is not, incremented, but that's ok and expected
    compact.result_types[old_result_idx] = compact.ir.types[idx]
    compact.result_lines[old_result_idx] = compact.ir.lines[idx]
    result_idx = process_node!(compact, old_result_idx, compact.ir.stmts[idx], idx, idx)
    if idx == last(bb.stmts)
        # If this was the last statement in the BB and we decided to skip it, insert a
        # dummy `nothing` node, to prevent changing the structure of the CFG
        if result_idx == first(bb.stmts)
            compact.result[old_result_idx] = nothing
            result_idx = old_result_idx + 1
        end
        compact.ir.cfg.blocks[active_bb] = BasicBlock(bb, StmtRange(first(bb.stmts), result_idx-1))
        active_bb += 1
        if active_bb <= length(compact.ir.cfg.blocks)
            new_bb = compact.ir.cfg.blocks[active_bb]
            compact.ir.cfg.blocks[active_bb] = BasicBlock(new_bb,
                StmtRange(result_idx, last(new_bb.stmts)))
        end
    end
    (old_result_idx == result_idx) && return next(compact, (idx + 1, active_bb, result_idx))
    compact.idx = idx + 1
    compact.result_idx = result_idx
    if !isassigned(compact.result, old_result_idx)
        @assert false
    end
    return (old_result_idx, compact.result[old_result_idx]), (compact.idx, active_bb, compact.result_idx)
end

function maybe_erase_unused!(extra_worklist, compact, idx)
    effect_free = stmt_effect_free(compact.result[idx], compact, compact.ir.mod)
    if effect_free
        for ops in userefs(compact.result[idx])
            val = ops[]
            if isa(val, SSAValue)
                if compact.used_ssas[val.id] == 1
                    if val.id < idx
                        push!(extra_worklist, val.id)
                    end
                end
                compact.used_ssas[val.id] -= 1
            end
        end
        compact.result[idx] = nothing
    end
end

function finish(compact::IncrementalCompact)
    for idx in compact.late_fixup
        stmt = compact.result[idx]::PhiNode
        values = Vector{Any}(undef, length(stmt.values))
        for i = 1:length(stmt.values)
            isassigned(stmt.values, i) || continue
            val = stmt.values[i]
            if isa(val, OldSSAValue)
                val = compact.ssa_rename[val.id]
                if isa(val, SSAValue)
                    compact.used_ssas[val.id] += 1
                end
            end
            values[i] = val
        end
        compact.result[idx] = PhiNode(stmt.edges, values)
    end
    # Record this somewhere?
    result_idx = compact.result_idx
    resize!(compact.result, result_idx-1)
    resize!(compact.result_types, result_idx-1)
    resize!(compact.result_lines, result_idx-1)
    bb = compact.ir.cfg.blocks[end]
    compact.ir.cfg.blocks[end] = BasicBlock(bb,
                StmtRange(first(bb.stmts), result_idx-1))
    # Perform simple DCE for unused values
    extra_worklist = Int[]
    for (idx, nused) in Iterators.enumerate(compact.used_ssas)
        idx >= result_idx && break
        nused == 0 || continue
        maybe_erase_unused!(extra_worklist, compact, idx)
    end
    while !isempty(extra_worklist)
        maybe_erase_unused!(extra_worklist, compact, pop!(extra_worklist))
    end
    cfg = CFG(compact.ir.cfg.blocks, Int[first(bb.stmts) for bb in compact.ir.cfg.blocks[2:end]])
    return IRCode(compact.ir, compact.result, compact.result_types, compact.result_lines, cfg, NewNode[])
end

function compact!(code::IRCode)
    compact = IncrementalCompact(code)
    # Just run through the iterator without any processing
    state = start(compact)
    while !done(compact, state)
        _, state = next(compact, state)
    end
    return finish(compact)
end

struct BBIdxStmt
    ir::IRCode
end

bbidxstmt(ir) = BBIdxStmt(ir)

start(x::BBIdxStmt) = (1,1)
done(x::BBIdxStmt, (idx, bb)) = idx > length(x.ir.stmts)
function next(x::BBIdxStmt, (idx, bb))
    active_bb = x.ir.cfg.blocks[bb]
    next_bb = bb
    if idx == last(active_bb.stmts)
        next_bb += 1
    end
    return (bb, idx, x.ir.stmts[idx]), (idx + 1, next_bb)
end
