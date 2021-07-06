# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Tapir for Julia IR.

This file implements the compiler passes for the parallel instructions in
Julia IR that are designed based on Tapir (Schardl et al., 2019). By lowering
parallel code at the end of Julia's optimization phase, the Julia compiler can
analyze and optimize the code containing the parallel tasks.

# Overview

* `lower_tapir!`: Lowers Tapir constructs to the calls to the parallel task runtime.
  This pass is called outside the usual `run_passes` to allow IPO across functions
  containing Tapir constructs.

* `lower_tapir_output!` called from `run_passes` via `early_tapir_pass!`. This
  transforms task outputs (`Tapir.Output` struct) to Upsilon and PhiC nodes.

# References

* Schardl, Tao B., William S. Moses, and Charles E. Leiserson.
  "Tapir: Embedding Recursive Fork-Join Parallelism into LLVM's Intermediate
  Representation." ACM Transactions on Parallel Computing 6, no. 4 (December
  17, 2019): 19:1–19:33. https://doi.org/10.1145/3365655.

* OpenCilk project: https://cilk.mit.edu/
  (https://github.com/OpenCilk/opencilk-project)
"""

"""
    ChildTask

ChildTask represents *detached sub-CFG* (p.13, Schardl et al., 2019) composed
of a subset of basic blocks `.blocks` that are represented as the indices of
corresponding `ir.cfg.blocks`. The related instructions (`.detach` and
`.reattaches`) are represented as indices to corresponding `ir.stmts`.

Some properties (see p.13 of Schardl et al. (2019) for the full explanation
of the properties of Tapir itself):

* There is a detach edge from a basic block (outside of the child task)
  terminated by `.detach` to the entry block of the child task `.blocks[1]`.
* Every path starting from `.block[1]` reaches a block `.block[i]` that is
  terminated by some `.reattaches[j]`.
* The basic blocks of the child tasks returned from `child_tasks(ir)` are`
  sorted.
"""
struct ChildTask
    """The node that detaches this task."""
    detach::Int
    """The reattach nodes that terminates the last blocks of this task."""
    reattaches::Vector{Int}
    """A list of basic block indices that defines this task."""
    blocks::Vector{Int}
    """A list of child tasks. `nothing` means `[]` (optimization)."""
    subtasks::Union{Nothing,Vector{ChildTask}}
end
ChildTask(detach::Int) = ChildTask(detach, Int[], Int[], nothing)

ChildTask(task::ChildTask, subtasks::Vector{ChildTask}) =
    ChildTask(task.detach, task.reattaches, task.blocks, subtasks)

"""
    foreach_task_depth_first(f, tasks::Vector{ChildTask})

Iterate `tasks` recursively in the depth-first order.
"""
function foreach_task_depth_first(f, tasks)
    stack = [collect(ChildTask, tasks)]
    while true
        tasks = stack[end]
        if isempty(tasks)
            pop!(stack)
            if isempty(stack)
                return true
            else
                t = pop!(stack[end])
                f(t) || return false
            end
            continue
        end
        t = tasks[end]
        if t.subtasks === nothing
            pop!(tasks)
            f(t) || return false
        else
            push!(stack, copy(t.subtasks))
        end
    end
end

"""
    task_by_detach(tasks::Vector{ChildTask}, detach::Int) -> nothing or task::ChildTask
"""
function task_by_detach(tasks, detach::Int)
    worklist = collect(ChildTask, tasks)
    while !isempty(worklist)
        local task::ChildTask
        task = pop!(worklist)
        if task.detach == detach
            return task
        end
        if task.subtasks !== nothing
            append!(worklist, task.subtasks)
        end
    end
    return nothing
end

function is_stmt_in_task(task::ChildTask, ir::IRCode, position::Int)
    for ibb in task.blocks
        if position in ir.cfg.blocks[ibb].stmts
            return true
        end
    end
    return false
end

"""
    child_tasks(ir::IRCode) -> tasks::Vector{ChildTask}

Discover immediate child tasks.
"""
function child_tasks(ir::IRCode)
    tasks = ChildTask[]
    isempty(ir.cfg.blocks) && return tasks
    visited = falses(length(ir.cfg.blocks))
    foreach_descendant(1, ir.cfg, visited) do ibb, bb
        term = ir.stmts[bb.stmts[end]][:inst]
        if term isa DetachNode
            @assert ibb + 1 in bb.succs
            @assert term.label in bb.succs
            @assert length(bb.succs) == 2
            push!(tasks, detached_sub_cfg!(ibb, ir, visited))
        end
        return true
    end
    return tasks
end

"""
    detached_sub_cfg!(detacher::Int, ir::IRCode, visited) -> task::ChildTask

Find a sub CFG detached by the detach node `ir.stmts[ir.cfg.blocks[detacher].stmts[end]]`.
It mutates `visited` but not other arguments.
"""
function detached_sub_cfg!(detacher::Int, ir::IRCode, visited)
    detach = ir.cfg.blocks[detacher].stmts[end]
    @assert ir.stmts.inst[detach] isa DetachNode
    task = ChildTask(detach)
    subtasks = nothing
    foreach_descendant(detacher + 1, ir.cfg, visited) do ibb, bb
        push!(task.blocks, ibb)
        term = ir.stmts[bb.stmts[end]][:inst]
        if term isa DetachNode
            t = detached_sub_cfg!(ibb, ir, visited)
            if subtasks === nothing
                subtasks = ChildTask[]
            end
            push!(subtasks::Vector{ChildTask}, t)
            return true  # continue on reattach edge
        elseif term isa ReattachNode
            @assert bb.succs == [term.label] "bb.succs == [term.label]"
            continuation = ir.cfg.blocks[term.label]
            for k in continuation.preds
                i = ir.cfg.blocks[k].stmts[end]
                if i == detach
                    push!(task.reattaches, bb.stmts[end])
                    return false
                end
            end
            error("unbalanced detach-reattach")
            # TODO: handle missing reattach?
            # This can happen when reattach nodes are replaced with unreachable.
            # Currently, handling this case has not been required so far as we
            # detect such tasks in `always_throws` and serial-project before the
            # CFG is altered.
        end
        return true
    end
    if subtasks isa Vector{ChildTask}
        for t in subtasks
            append!(task.blocks, t.blocks)
        end
        task = ChildTask(task, subtasks)
    end
    sort!(task.blocks)
    @assert task.blocks[1] == detacher + 1  # entry block
    return task
end

"""
    append_uses_and_args!(uses::Vector{<:Integer}, args::Vector{<:Integer}, stmt)

Examine the instruction `stmt` and append the uses of `SSAValue` into `uses` and
`Argument` to `args` if given.
"""
function append_uses_and_args!(uses, args, @nospecialize(stmt))
    map_id(identity, stmt) do v
        if v isa SSAValue
            push!(uses, v.id)
        elseif v isa Argument
            args === nothing && return v
            push!(args, v.n)
        end
        v
    end
    return
end

append_uses!(uses, @nospecialize(stmt)) = append_uses_and_args!(uses, nothing, stmt)

"""
    map_id(on_value, on_label, stmt::T) -> stmt′::T
    map_id(on_value, on_phi_label, on_goto_label, stmt::T) -> stmt′::T

Map over the locations containing IDs in `stmt`; i.e., run the function
`on_value` on `SSAValue` or `Arguments` in `stmt` and the function `on_label`
on basic block label in `stmt`. Return a new instruction `stmt′` with
corresponding "fields" updated by these functions.

# Arguments
* `stmt`: an IR statement (e.g., an `Expr(:call, ...)`)
* `on_value`: a function that accepts an `SSAValue`, `Argument`, `SlotNumber`,
  or `TypedSlot`.
* `on_label`: a function that accepts a basic block label (e.g., `goto.label`
  of a `goto::GotoNode` statement)
"""
map_id(on_value, on_label, @nospecialize(stmt)) = map_id(on_value, on_label, on_label, stmt)

function map_id(on_value, on_phi_label, on_goto_label, @nospecialize(stmt))
    recurse(@nospecialize x) = map_id(on_value, on_phi_label, on_goto_label, x)
    if stmt isa Union{SSAValue,Argument,SlotNumber,TypedSlot}
        on_value(stmt)
    elseif stmt isa GotoNode
        GotoNode(on_goto_label(stmt.label))
    elseif stmt isa GotoIfNot
        GotoIfNot(recurse(stmt.cond), on_goto_label(stmt.dest))
    elseif stmt isa ReturnNode
        if isdefined(stmt, :val) && stmt.val isa SSAValue
            ReturnNode(recurse(stmt.val))
        else
            stmt
        end
    elseif stmt isa PiNode
        PiNode(recurse(stmt.val), stmt.typ)
    elseif stmt isa Union{PhiNode,PhiCNode}
        newvalues = similar(stmt.values)
        for i in eachindex(stmt.values)
            if isassigned(stmt.values, i)
                newvalues[i] = recurse(stmt.values[i])
            end
        end
        if stmt isa PhiNode
            PhiNode(Int32[on_phi_label(x) for x in stmt.edges], newvalues)
        else
            PhiCNode(newvalues)
        end
    elseif stmt isa UpsilonNode
        if isdefined(stmt, :val)
            UpsilonNode(recurse(stmt.val))
        else
            stmt
        end
    elseif stmt isa DetachNode
        DetachNode(recurse(stmt.syncregion), on_goto_label(stmt.label))
    elseif stmt isa ReattachNode
        ReattachNode(recurse(stmt.syncregion), on_goto_label(stmt.label))
    elseif stmt isa SyncNode
        SyncNode(recurse(stmt.syncregion))
    elseif stmt isa Expr
        if is_meta_expr_head(stmt.head)
            stmt
        elseif stmt.head === :(=) && stmt.args[2] isa Expr
            Expr(stmt.head, stmt.args[1], recurse(stmt.args[2]))
        elseif stmt.head === :enter
            label = get(stmt.args, 1, nothing)
            if label isa Integer
                Expr(stmt.head, on_goto_label(stmt.args[1]))
            else
                stmt  # malformed?
            end
        else
            Expr(stmt.head, Any[recurse(a) for a in stmt.args]...)
        end
    else
        stmt
    end
end

function foreach_id(on_value::F, on_label::G, @nospecialize(stmt)) where {F, G}
    function f(x)
        on_value(x)
        x
    end
    function g(x)
        on_label(x)
        x
    end
    # TODO: non-reconstructing version (i.e., don't use `map_id`)
    map_id(f, g, stmt)
    return
end

"""
    foreach_descendant(f, ibb::Int, cfg::CFG, [visited::AbstractVector{Bool}]) -> exhausted::Bool

Evaluate a function `f` on the basic block `ibb` and all of its descendants,
until either `f` returns `false` on all the paths or all the basic blocks are
visited. The function `f` takes two arguments; the index `ibb` of the basic
block and the corresponding basic block `cfg.blocks[ibb]`. Return `false`
iff `f` returns `false` at least once.
"""
function foreach_descendant(
    f::F,
    ibb::Int,
    cfg::CFG,
    visited = falses(length(cfg.blocks)),
) where {F}
    worklist = [ibb]
    acc = true
    while !isempty(worklist)
        ibb = pop!(worklist)
        visited[ibb] && continue
        visited[ibb] = true
        bb = cfg.blocks[ibb]
        c = f(ibb, bb)
        acc &= c
        c && append!(worklist, bb.succs)
    end
    return acc
end

function foreach_ancestor(
    f::F,
    ibb::Int,
    cfg::CFG,
    visited = falses(length(cfg.blocks)),
) where {F}
    worklist = [ibb]
    acc = true
    while !isempty(worklist)
        ibb = pop!(worklist)
        visited[ibb] && continue
        visited[ibb] = true
        bb = cfg.blocks[ibb]
        c = f(ibb, bb)
        acc &= c
        c && append!(worklist, bb.preds)
    end
    return acc
end

"""
    foreach_def(f, v::SSAValue, ir::IRCode) -> exhausted::Bool

Evaluate `f` on the statement `stmt::Instruction` corresponding to `v` and also
all of its arguments recursively ("upwards") as long as `f` returns `true`.
Note that `f` may receive non-`Instruction` argument (e.g., `Argument`).  Return
`false` iff `f` returns `false` at least once.
"""
function foreach_def(
    f::F,
    v::SSAValue,
    ir::IRCode,
    visited = falses(length(ir.stmts)),
) where {F}
    worklist = [v.id]
    acc = RefValue(true)
    while !isempty(worklist)
        i = pop!(worklist)
        visited[i] && continue
        visited[i] = true
        stmt = ir.stmts[i]
        inst = stmt[:inst]
        c = f(stmt)
        acc.x &= c
        foreach_id(identity, inst) do v
            if v isa SSAValue
                c && push!(worklist, v.id)
            else
                acc.x &= f(v)
            end
        end
    end
    return acc.x
end

function any_assigned(f, xs::Array)
    for i in 1:length(xs)
        if isassigned(xs, i)
            f(xs[i]) && return true
        end
    end
    return false
end

function foreach_assigned_pair(f, xs::Array)
    for i in 1:length(xs)
        if isassigned(xs, i)
            f(i, xs[i])
        end
    end
end

function empty_ircode(ir::IRCode, len::Int)
    stmts = InstructionStream(len)
    for i in 1:len
        stmts.inst[i] = nothing
        stmts.type[i] = Any
        stmts.info[i] = nothing
    end
    cfg = CFG([BasicBlock(StmtRange(1, len))], Int[])
    return IRCode(ir, stmts, cfg, NewNodeStream())
end

function stmt_at(ir::IRCode, pos::Int)
    pos <= length(ir.stmts) && return ir.stmts[pos]
    return ir.new_nodes.stmts[pos-length(ir.stmts)]
end

valuetypeof(ir::IRCode, v::SSAValue) = stmt_at(ir, v.id)[:type]
valuetypeof(ir::IRCode, v::Argument) = ir,argtypes[v.n]
valuetypeof(::IRCode, v::QuoteNode) = typeof(v.value)
valuetypeof(::IRCode, v) = typeof(v)

function insert_pos(ir::IRCode, pos::Int)
    pos <= length(ir.stmts) && return pos
    return ir.new_nodes.info[pos-length(ir.stmts)].pos
end

function cfg_reindex!(cfg::CFG)
    resize!(cfg.index, length(cfg.blocks) - 1)
    for ibb in 2:length(cfg.blocks)
        cfg.index[ibb-1] = first(cfg.blocks[ibb].stmts)
    end
    return cfg
end

function cumsum(xs)
    ys = collect(xs)
    acc = ys[1]
    for i in 2:length(ys)
        acc += ys[i]
        ys[i] = acc
    end
    return ys
end

function fixup_linetable!(ir::IRCode)
    indexmap = _fixup_linetable!(ir.linetable)
    verify_linetable(ir.linetable)
    lines = ir.stmts.line
    for (i, l) in pairs(lines)
        if l > 0
            lines[i] = indexmap[lines[i]]
        end
    end
end

function _fixup_linetable!(linetable::Vector{LineInfoNode})
    n = length(linetable)
    indexmap = Vector{Int}(undef, n)
    shift = 0
    for i in 1:length(linetable)
        line = linetable[i]
        shift = max(shift, line.inlined_at - i + 1)
        indexmap[i] = i + shift
    end
    resize!(linetable, n + shift)
    linetable[end] = linetable[n]
    for i in n-1:-1:1
        line = linetable[i]
        for j in indexmap[i]:indexmap[i+1]-1
            linetable[j] = line
        end
        indexmap[i] == i && break
    end
    return indexmap
end

"""
    allocate_new_blocks!(ir::IRCode, statement_positions) -> info

Create new "singleton" basic blocks (i.e., it contains a single instruction)
before `statement_positions`.  This function adds `2 * length(statement_positions)`
blocks; i.e., `length(statement_positions)` blocks for the new singleton basic
blocks and the remaining `length(statement_positions)` blocks for the basic
block containing the instructions starting at each `statement_positions`.  This
function expects that `statement_positions` is sorted.

Note that this function does not wire up the CFG for newly created BBs. It just
inserts the dummy `GotoNode(0)` at the end of the new singleton BBs and the BB
_before_ (in terms of `ir.cfg.bocks`) it.  The predecessors of the BB just
before the newly added singleton BB and the successors of the BB just after the
newly added singleton BB are re-wired.  See `allocate_gotoifnot_sequence!` for
an example for creating a valid CFG.

The returned object `info` can be passed to `foreach_allocated_new_block` for
iterating over allocated basic blocks.  An indexable object `info.ssachangemap`
can be used for mapping old SSA values to the new locations.
"""
function allocate_new_blocks!(ir::IRCode, statement_positions)
    @assert issorted(statement_positions)
    ssachangemap = Vector{Int}(undef, length(ir.stmts) + length(ir.new_nodes.stmts))
    let iold = 1, inew = 1
        for pos in statement_positions
            while iold < pos
                ssachangemap[iold] = inew
                iold += 1
                inew += 1
            end
            inew += 2
        end
        while iold <= length(ssachangemap)
            ssachangemap[iold] = inew
            iold += 1
            inew += 1
        end
    end

    target_blocks = BitSet()
    block_to_positions = Vector{Vector{Int}}(undef, length(ir.cfg.blocks))
    for pos in statement_positions
        ipos = insert_pos(ir, pos)
        ibb = block_for_inst(ir.cfg, ipos)
        if ibb in target_blocks
            poss = block_to_positions[ibb]
        else
            push!(target_blocks, ibb)
            poss = block_to_positions[ibb] = Int[]
        end
        push!(poss, ipos)
    end

    bbchangemap = cumsum(
        if ibb in target_blocks
            1 + 2 * length(block_to_positions[ibb])
        else
            1
        end for ibb in 1:length(ir.cfg.blocks)
    )
    newblocks = 2 * length(statement_positions)

    # Insert `newblocks` new blocks:
    oldnblocks = length(ir.cfg.blocks)
    resize!(ir.cfg.blocks, oldnblocks + newblocks)
    # Copy pre-existing blocks:
    for iold in oldnblocks:-1:1
        bb = ir.cfg.blocks[iold]
        for labels in (bb.preds, bb.succs)
            for (i, l) in pairs(labels)
                labels[i] = bbchangemap[l]
            end
        end
        start = ssachangemap[bb.stmts.start]
        stop = ssachangemap[bb.stmts.stop]
        ir.cfg.blocks[bbchangemap[iold]] = BasicBlock(bb, StmtRange(start, stop))
    end
    # Insert new blocks:
    for iold in target_blocks
        positions = block_to_positions[iold]
        ilst = bbchangemap[iold]  # using bbchangemap as it's already moved
        bblst = ir.cfg.blocks[ilst]

        inew = get(bbchangemap, iold - 1, 0)
        preoldpos = 0  # for detecting duplicated positions
        p1 = first(bblst.stmts)
        isfirst = true
        for (i, oldpos) in pairs(positions)
            if preoldpos == oldpos
                p2 = p1
            else
                preoldpos = oldpos
                p2 = ssachangemap[oldpos-1] + 1
                if isfirst
                    isfirst = false
                    p1 = min(p1, p2)
                end
            end
            p3 = p2 + 1
            @assert p1 <= p2 < p3
            ir.cfg.blocks[inew+1] = BasicBlock(StmtRange(p1, p2))
            ir.cfg.blocks[inew+2] = BasicBlock(StmtRange(p3, p3))
            p1 = p3 + 1
            inew += 2
        end
        ifst = get(bbchangemap, iold - 1, 0) + 1
        bbfst = ir.cfg.blocks[ifst]
        for p in bblst.preds
            k = findfirst(==(ilst), ir.cfg.blocks[p].succs)
            @assert k !== nothing
            ir.cfg.blocks[p].succs[k] = ifst
        end
        copy!(bbfst.preds, bblst.preds)
        empty!(bblst.preds)
        stmts = StmtRange(ssachangemap[positions[end]], last(bblst.stmts))
        ir.cfg.blocks[bbchangemap[iold]] = BasicBlock(bblst, stmts)
        @assert !isempty(stmts)
    end
    for bb in ir.cfg.blocks
        @assert !isempty(bb.stmts)
    end
    cfg_reindex!(ir.cfg)

    # Like `bbchangemap` but maps to the first added BB (not the last)
    gotolabelchangemap = cumsum(
        if ibb in target_blocks
            1 + 2 * length(block_to_positions[ibb])
        else
            1
        end for ibb in 0:length(ir.cfg.blocks)-1
    )

    on_value(a) = a
    on_value(v::SSAValue) = SSAValue(ssachangemap[v.id])
    on_phi_label(l) = bbchangemap[l]
    on_goto_label(l) = gotolabelchangemap[l]
    for stmts in (ir.stmts, ir.new_nodes.stmts)
        for i in 1:length(stmts)
            st = stmts[i]
            st[:inst] = map_id(on_value, on_phi_label, on_goto_label, st[:inst])
        end
    end
    minpos = statement_positions[1]  # it's sorted
    for (i, info) in pairs(ir.new_nodes.info)
        if info.pos >= minpos
            ir.new_nodes.info[i] = if info.attach_after
                NewNodeInfo(ssachangemap[info.pos], info.attach_after)
            else
                NewNodeInfo(get(ssachangemap, info.pos - 1, 0) + 1, info.attach_after)
            end
        end
    end
    for (i, info) in pairs(ir.linetable)
        1 <= info.inlined_at <= length(ssachangemap) || continue
        if info.inlined_at >= minpos
            ir.linetable[i] = LineInfoNode(
                info.module,
                info.method,
                info.file,
                info.line,
                ssachangemap[info.inlined_at],
            )
        end
    end
    fixup_linetable!(ir)

    function allocate_stmts!(xs, filler)
        n = length(xs)
        resize!(xs, length(xs) + newblocks)
        for i in n:-1:1
            xs[ssachangemap[i]] = xs[i]
        end
        for i in 2:n
            for j in ssachangemap[i-1]+1:ssachangemap[i]-1
                xs[j] = filler
            end
        end
        for js in (1:ssachangemap[1]-1, ssachangemap[end]+1:length(xs))
            for j in js
                xs[j] = filler
            end
        end
    end

    allocate_stmts!(ir.stmts.inst, GotoNode(0))  # dummy
    allocate_stmts!(ir.stmts.type, Any)
    allocate_stmts!(ir.stmts.info, nothing)
    allocate_stmts!(ir.stmts.line, 0)
    allocate_stmts!(ir.stmts.flag, 0)

    return (; target_blocks, block_to_positions, ssachangemap, bbchangemap)
end

"""
    allocate_new_blocks!(ir::IRCode, statement_positions) -> info

Insert a new basic block before each `statement_positions` and a `GotoIfNot`
that jumps over the newly added BB.  Unlike `allocate_new_blocks!`, this
function results in an IR with valid CFG.
"""
function allocate_gotoifnot_sequence!(ir::IRCode, statement_positions)
    isempty(statement_positions) && return nothing
    blocks = allocate_new_blocks!(ir, statement_positions)
    (; target_blocks, block_to_positions, bbchangemap) = blocks
    for iold in target_blocks
        ibb = get(bbchangemap, iold - 1, 0) + 1
        for _ in block_to_positions[iold]
            b0 = ir.cfg.blocks[ibb]
            b1 = ir.cfg.blocks[ibb+1]
            b2 = ir.cfg.blocks[ibb+2]
            push!(b0.succs, ibb + 1, ibb + 2)
            push!(b1.preds, ibb)
            push!(b1.succs, ibb + 2)
            push!(b2.preds, ibb, ibb + 1)
            @assert ir.stmts.inst[last(b0.stmts)] === GotoNode(0)
            @assert ir.stmts.inst[last(b1.stmts)] === GotoNode(0)
            ir.stmts.inst[last(b0.stmts)] = GotoIfNot(false, ibb + 2)  # dummy
            ir.stmts.inst[last(b1.stmts)] = GotoNode(ibb + 2)
            ibb += 2
        end
    end
    return blocks
end

foreach_allocated_new_block(_, ::Nothing) = nothing
function foreach_allocated_new_block(f, blocks)
    (; target_blocks, block_to_positions, bbchangemap) = blocks
    for iold in target_blocks
        inew = get(bbchangemap, iold - 1, 0)
        for _ in block_to_positions[iold]
            f(inew + 1)
            inew += 2
        end
    end
end

is_sequential(stmts::InstructionStream) = !any(inst -> inst isa DetachNode, stmts.inst)
is_sequential(ir::IRCode) = is_sequential(ir.stmts) && is_sequential(ir.new_nodes.stmts)

has_tapir(x) = !is_sequential(x)

function havecommon(x::AbstractSet, y::AbstractSet)
    (b, c) = length(x) > length(y) ? (x, y) : (y, x)
    return any(in(b), c)
end

function transitive_closure_on(rel, xs)
    T = eltype(xs)
    graph = IdDict{T,IdSet{T}}()
    cache = IdDict{Tuple{T,T},Bool}()
    for x in xs, y in xs
        cache[(x, y)] = r = rel(x, y)
        if r
            push!(get!(IdSet{T}, graph, x), y)
        end
    end
    closure(x, y) = get!(cache, (x, y)) do
        any(z -> closure(z, y), graph[x])
    end
    return closure
end

function foldunion(types)
    T = Union{}
    for x in types
        T = Union{T, widenconst(x)}
    end
    return T
end

struct _InaccessibleValue end

function remove_stmt!(stmt::Instruction)
    stmt[:inst] = _InaccessibleValue()
    stmt[:type] = Any
end

"""
    resolve_linear_chain(ir, v::Union{PiNode,SSAValue}) -> v′::Union{PiNode,SSAValue}

Resolve simple chain of statements that are SSAValue or PiNode.
"""
function resolve_linear_chain end
function resolve_linear_chain(ir::IRCode, v::PiNode)
    n = v.val
    if n isa SSAValue
        return resolve_linear_chain(ir, n)
    else
        return v
    end
end
function resolve_linear_chain(ir::IRCode, v::SSAValue)
    v0 = v
    while true
        n = ir[v]
        if n isa PiNode
            n = n.val
        end
        if n isa SSAValue
            v = n
        else
            break
        end
        v === v0 && error("cycle detected")
    end
    return v
end

# Ref: abstract_eval_special_value
function resolve_special_value(@nospecialize(v))
    if v isa GlobalRef
        isdefined(v.mod, v.name) || return nothing, false
        v = getfield(v.mod, v.name)
    elseif v isa QuoteNode
        return v.value, true
    end
    return v, true
end

resolve_ssavalue(ir::IRCode, v::SSAValue) = resolve_ssavalue(ir.stmts.inst, v)
function resolve_ssavalue(code::Vector{Any}, v::SSAValue)
    v0 = v
    while v isa SSAValue
        v = code[v.id]
        v === v0 && error("cycle detected")
    end
    return v
end

function resolve_callee(code::Vector{Any}, @nospecialize(inst))::Tuple{Any,Bool}
    isexpr(inst, :call) || return nothing, false
    isempty(inst.args) && return nothing, false
    f, = inst.args
    if f isa SSAValue
        f = resolve_ssavalue(code, f)
    end
    return resolve_special_value(f)
end

function calls(inst, r::GlobalRef, nargs::Int)
    isexpr(inst, :call) || return nothing
    length(inst.args) == nargs + 1 || return nothing
    f, = inst.args
    if f isa GlobalRef
        f === r || return nothing
    else
        isdefined(r.mod, r.name) || return nothing
        g = getfield(r.mod, r.name)
        f === g || return nothing
        # TODO: check if this path is required
    end
    return inst.args
end

function find_method_instance_from_sig(
    interp::AbstractInterpreter,
    @nospecialize(sig::Type);
    sparms::SimpleVector = svec(),
    preexisting::Bool = false,
    compilesig::Bool = false,
)
    result = findsup(sig, method_table(interp))
    result === nothing && return nothing
    method, = result
    return specialize_method(method, sig, sparms, preexisting, compilesig)
end

function is_in_loop(cfg::CFG, subcfg, needle::Int)
    subcfg = BitSet(subcfg)
    ref = RefValue(false)
    foreach_descendant(cfg, needle) do ibb
        ibb in subcfg || return false
        if ibb < needle
            ref.x = true
            return false
        else
            return true
        end
    end
    return ref.x
end

has_loop(ir::IRCode, blocklabels) =
    any(blocklabels) do ibb
        bb = ir.cfg.blocks[ibb]
        any(<(ibb), bb.succs)
    end

function always_throws(ir::IRCode, task::ChildTask)
    visited = falses(length(ir.cfg.blocks))
    detacher = block_for_inst(ir, task.detach)
    for i in task.reattaches
        # If all non-terminator instructions just before all reattach nodes have
        # `Union{}`, this task always throw.  Handle the case where there is
        # only one terminator instruction in a BB using `foreach_ancestor`:
        throws = RefValue(true)
        foreach_ancestor(block_for_inst(ir, i), ir.cfg, visited) do ibb, bb
            detacher == ibb && return false  # stop; outside the task
            stmt = ir.stmts[bb.stmts[end]]
            if isterminator(stmt[:inst])
                if length(bb.stmts) < 2
                    return true  # continue checking successor
                end
                stmt = ir.stmts[bb.stmts[end-1]]
            end
            if stmt[:type] !== Union{}
                throws.x = false
            end
            return false
        end
        throws.x || return false
    end
    return true
end

function try_resolve(@nospecialize(x))
    if x isa GlobalRef
        if isdefined(x.mod, x.name)
            return getfield(x.mod, x.name), true
        end
    end
    return nothing, false
end

function is_trivial_for_spawn(@nospecialize(inst))
    if isterminator(inst)
        return true
    elseif inst isa Union{PhiNode,PhiCNode,UpsilonNode,PiNode,Nothing}
        return true
    elseif inst isa Expr
        if is_meta_expr_head(inst.head)
            return true
        elseif inst.head === :call
            f, = try_resolve(inst.args[1])
            if f isa Builtin
                return !(f === Core.Intrinsics.invoke || f === Core.Intrinsics.llvmcall)
            end
        elseif (
            inst.head === :new ||
            inst.head === :enter ||
            inst.head === :leave ||
            inst.head === :the_exception ||
            inst.head === :pop_exception
        )
            return true
        end
    end
    return false
end

is_trivial_for_spawn(ir::IRCode, bb::BasicBlock) = is_trivial_for_spawn(ir, bb.stmts)
function is_trivial_for_spawn(ir::IRCode, stmts::StmtRange)
    for istmt in stmts
        is_trivial_for_spawn(ir.stmts.inst[istmt]) || return false
    end
    return true
end

function is_trivial_for_spawn(ir::IRCode, task::ChildTask)
    has_loop(ir, task.blocks) && return false
    always_throws(ir, task) && return true

    for ibb in task.blocks
        is_trivial_for_spawn(ir, ir.cfg.blocks[ibb]) || return false
    end
    return true
end

function remove_syncregions!(ir::IRCode, stmts = 1:length(ir.stmts))
    for i in stmts
        stmt = ir.stmts[i]
        if isexpr(stmt[:inst], :syncregion)
            stmt[:inst] = nothing
        end
    end
end

function remove_tapir_terminator!(stmt::Instruction)
    term = stmt[:inst]
    if term isa DetachNode
        stmt[:inst] = GotoIfNot(true, term.label)
    elseif term isa ReattachNode
        stmt[:inst] = GotoNode(term.label)
    elseif term isa SyncNode
        stmt[:inst] = nothing
    end
    return term
end

remove_tapir!(ir::IRCode) = remove_tapir_in_blocks!(ir, 1:length(ir.cfg.blocks))

function remove_tapir_in_blocks!(ir::IRCode, blocklabels)
    for ibb in blocklabels
        bb = ir.cfg.blocks[ibb]
        remove_tapir_terminator!(ir.stmts[last(bb.stmts)])
        remove_syncregions!(ir, bb.stmts)
    end
    return ir
end

function remove_tapir!(ir::IRCode, task::ChildTask)
    remove_tapir_in_blocks!(ir, task.blocks)
    remove_tapir_terminator!(ir.stmts[task.detach])::DetachNode
    return ir
end

"""
    parallel_getfield_elim_checker(ir) -> phiblocks -> is_promotable::Bool

Return a function that checks if inserting phi nodes for a given list of BB is compatible
with Tapir.

See also `check_tapir_race!`
"""
function parallel_getfield_elim_checker(ir::IRCode)

    # Lazily analyze child tasks:
    cache = RefValue{Union{Nothing,Vector{ChildTask}}}(nothing)
    function get_child_tasks()
        tasks = cache[]
        tasks === nothing || return tasks
        tasks = cache[] = child_tasks(ir)
        return tasks
    end

    function is_parallel_promotable(phiblocks)
        @assert eltype(phiblocks) === BBNumber
        result = RefValue(true)
        foreach_task_depth_first(get_child_tasks()) do task
            det = ir.stmts.inst[task.detach]::DetachNode
            continuation = det.label
            if continuation in phiblocks
                result.x = false
                return false  # break
            else
                return true  # keep going
            end
        end
        return result.x
    end

    return is_parallel_promotable
end

"""
    early_tapir_pass!(ir::IRCode) -> (ir′::IRCode, racy::Bool)

Mainly operates on task output variables.
"""
function early_tapir_pass!(ir::IRCode)
    is_sequential(ir) && return ir, false
    @timeit "Lower task output" ir = lower_tapir_output!(ir)
    @timeit "Check task output" ir, racy = check_tapir_race!(ir)
    racy && return ir, racy
    @timeit "Fixup syncregion" ir = fixup_syncregion_phic!(ir)
    return ir, racy
end

function resolve_syncregion(ir::IRCode, inst)
    @nospecialize inst
    inst0 = inst
    while true
        if inst isa PhiCNode
            @assert length(inst.values) == 1
            ups = ir[inst.values[1]::SSAValue]::UpsilonNode
            inst = ups.val::SSAValue
        elseif inst isa SSAValue
            sr = ir[inst]
            isexpr(sr, :syncregion) && return inst
            inst = sr
        end
        if inst === inst0
            error("cycle detected")
        end
    end
end

"""
    fixup_syncregion_phic!(ir::IRCode) -> ir′
"""
function fixup_syncregion_phic!(ir::IRCode)
    if !isempty(ir.new_nodes.stmts.inst)
        ir = compact!(ir)
    end

    for bb in ir.cfg.blocks
        isync = last(bb.stmts)
        sync = ir.stmts[isync][:inst]
        sync isa SyncNode || continue
        inst = ir[sync.syncregion::SSAValue]
        isexpr(inst, :syncregion) && continue
        ssa = resolve_syncregion(ir, inst)
        # TODO: validate that syncregion dominates sync
        ir.stmts.inst[isync] = SyncNode(ssa)
    end

    return ir
end

foreach_task_output_load(f::F, ir::IRCode) where {F} =
    foreach_task_output_load(f, ir.stmts.inst)

function foreach_task_output_load(f, code::Vector{Any})
    Tapir = tapir_module()
    Tapir isa Module || return
    isdefined(Tapir, :_load) || return  # avoid error while bootstrapping

    for (i, inst) in pairs(code)
        if isexpr(inst, :(=))
            _, call = inst.args
        else
            call = inst
        end
        callee, = resolve_callee(code, call)
        if callee === Tapir._load && length(call.args) == 3
            f(i, inst, call.args)
        end
    end
end

foreach_task_output_decl(f::F, ir::IRCode) where {F} =
    foreach_task_output_decl(f, ir.stmts.inst)
function foreach_task_output_decl(f, code::Vector{Any})
    for (i, inst) in pairs(code)
        isexpr(inst, :task_output) || continue
        f(i, inst.args[1])
    end
end

"""
    task_output_slots(ci::CodeInfo, code::Vector{Any}) -> slotids::BitSet

Return a set of slot IDs that should be kept in the `IRCode` genrated by
slot2reg.  These slots are processed later in `lower_tapir_output!`.
"""
function task_output_slots(::CodeInfo, code::Vector{Any})
    outputs = BitSet()
    foreach_task_output_load(code) do _i, _inst, (_f, out, _name)
        if out isa SSAValue
            out = resolve_ssavalue(code, out)
        end
        if out isa Union{SlotNumber,TypedSlot}
            push!(outputs, slot_id(out))
        end
    end
    foreach_task_output_decl(code) do _, out
        if out isa Union{SlotNumber,TypedSlot}
            push!(outputs, slot_id(out))
        end
    end
    return outputs
end

"""
    lower_tapir_output!(ir::IRCode) -> ir′

Lower task output slots.
"""
function lower_tapir_output!(ir::IRCode)
    ir = lower_tapir_task_output!(ir)
    ir = lower_tapir_phic_output!(ir)
    return ir
end

"""
    lower_tapir_task_output!(ir::IRCode) -> ir′

Lower task outputs marked by `Tapir.@output`.
"""
function lower_tapir_task_output!(ir::IRCode)
    Tapir = tapir_module()
    Tapir isa Module || return ir

    # TODO: make it work without compaction?
    if !isempty(ir.new_nodes.stmts.inst)
        ir = compact!(ir)
    end

    # TODO: turn slots into PhiC/Upsilon nodes if possible?

    outputinfo = IdDict{Int,Tuple{Symbol,LineNumberNode}}()
    foreach_task_output_decl(ir) do i, out
        if out isa Union{SlotNumber,TypedSlot}
            stmt = ir.stmts[i]
            _, qnode, lineno = stmt[:inst].args
            name = (qnode::QuoteNode).value::Symbol
            lineno::LineNumberNode
            outputinfo[slot_id(out)] = (name, lineno)
            remove_stmt!(stmt)
        end
    end

    isempty(outputinfo) && return ir

    # Determine the type of each slot
    slottypes = IdDict{Int,Type}()      # slot id -> type
    stores = IdDict{Int,Vector{Int}}()  # slot id -> statement positions
    loaded_slots = BitSet()
    loaded_positions = BitSet()
    for i in 1:length(ir.stmts)
        stmt = ir.stmts[i]
        inst = stmt[:inst]

        if (
            isexpr(inst, :(=)) &&
            begin
                lhs, rhs = inst.args
                lhs isa Union{SlotNumber,TypedSlot}
            end &&
            haskey(outputinfo, slot_id(lhs))
        )
            t0 = get(slottypes, slot_id(lhs), Union{})
            t1 = widenconst(stmt[:type])
            slottypes[slot_id(lhs)] = Union{t0,t1}
            push!(get!(() -> Int[], stores, slot_id(lhs)), i)
            inst = rhs
        end

        foreach_id(identity, inst) do out
            if out isa Union{SlotNumber,TypedSlot}
                if haskey(outputinfo, slot_id(out))
                    push!(loaded_positions, i)
                    push!(loaded_slots, slot_id(out))
                end
            end
        end
    end

    unused = setdiff!(BitSet(keys(stores)), loaded_slots)
    for oid in unused
        for i in stores[oid]
            stmt = ir.stmts[i]
            _, rhs = stmt[:inst].args
            stmt[:inst] = rhs
        end
    end
    isempty(loaded_slots) && return ir

    # Insert Refs
    outputrefs = IdDict{Int,SSAValue}()
    for oid in keys(outputinfo)
        T = get(slottypes, oid, Union{})
        R = Tapir.UndefableRef{T}
        alloc_pos = 1   # [^alloca-position]
        ref = insert_node!(ir, alloc_pos, NewInstruction(Expr(:new, R), R))
        outputrefs[oid] = ref
        setset = NewInstruction(
            Expr(:call, setfield!, ref, QuoteNode(:set), QuoteNode(false)),
            Any,
        )
        insert_node!(ir, alloc_pos, setset)
    end

    # Insert loads
    for i in loaded_positions
        stmt = ir.stmts[i]
        stmt[:inst] = map_id(identity, stmt[:inst]) do out
            if out isa Union{SlotNumber,TypedSlot}
                ref = get(outputrefs, slot_id(out), nothing)
                if ref isa SSAValue
                    isset_ex = Expr(:call, GlobalRef(Core, :getfield), ref, QuoteNode(:set))
                    isset = insert_node!(ir, i, NewInstruction(isset_ex, Bool))
                    name, = outputinfo[slot_id(out)]
                    undefcheck = Expr(:throw_undef_if_not, name, isset)
                    insert_node!(ir, i, NewInstruction(undefcheck, Any))
                    value_ex = Expr(:call, GlobalRef(Core, :getfield), ref, QuoteNode(:x))
                    T = get(slottypes, slot_id(out), Union{})
                    return insert_node!(ir, i, NewInstruction(value_ex, T))
                end
            end
            return out
        end
    end

    # Insert stores (that are loaded somewhere)
    for oid in setdiff!(BitSet(keys(stores)), unused)
        ref = outputrefs[oid]
        for i in stores[oid]
            stmt = ir.stmts[i]
            _, rhs = stmt[:inst].args
            stmt[:inst] = rhs

            value = SSAValue(i)
            ex = Expr(:call, GlobalRef(Core, :setfield!), ref, QuoteNode(:x), value)
            insert_node!(ir, i, NewInstruction(ex, Any), true)
            ex = Expr(:call, GlobalRef(Core, :setfield!), ref, QuoteNode(:set), true)
            insert_node!(ir, i, NewInstruction(ex, Any), true)
        end
    end

    return ir
end

"""
    lower_tapir_phic_output!(ir::IRCode) -> ir′

Lower output slots marked by `Tapir._load` using Upsilon and PhiC nodes.

This pass relies on the frontend to use the slots in write-many-load-once
manner.  TODO: verify?

It transforms

    ...
        slot = %value
    ...
        %out = Tapir._load(slot, %name)

to

    ...
        %undefinit = ϒ(true)
    ...
        %store = ϒ(%value)
        %notundef = ϒ(true)
    ...
        %undef = φᶜ(%undefinit, %notundef)
        goto #ok if not %undef
    #throw
        throw(UndefVarError(%name))
    #ok
        %out = φᶜ(%store)
"""
function lower_tapir_phic_output!(ir::IRCode)
    Tapir = tapir_module()
    Tapir isa Module || return ir
    Base = Main.Base::Module

    # TODO: make it work without compaction?
    if !isempty(ir.new_nodes.stmts.inst)
        ir = compact!(ir)
    end

    outputs = BitSet()
    loads = Tuple{Int,Int}[]  # pairs of (load position, slot id)
    load_positions = BitSet()
    output_names = Vector{Any}(undef, length(ir.stmts)) # big enough since #stmts >= #slots
    foreach_task_output_load(ir) do i, _inst,  (_f, out, name)
        if out isa SSAValue
            # Since the frontend ensures that this slot is read once, resolving
            # a linear chain of SSA values and Pi nodes is equivalent to
            # deferring the load (which is fine).
            out = resolve_linear_chain(ir, out)
            if out isa SSAValue
                out = ir[out]
            end
        end
        if out isa Union{SlotNumber,TypedSlot}
            push!(outputs, slot_id(out))
            push!(loads, (i, slot_id(out)))
            push!(load_positions, i)
            output_names[slot_id(out)] = name
        end
    end
    maxslotid = isempty(outputs) ? 0 : maximum(outputs)

    # Find stores, isdefined and escapes:
    stores = Tuple{Int,Int}[]  # pairs of (store position, slot id)
    defchecks = Vector{Vector{SSAValue}}(undef, maxslotid)
    deletelater = Int[]
    for i in 1:length(ir.stmts)
        stmt = ir.stmts[i]
        inst = stmt[:inst]
        if (
            isexpr(inst, :(=)) &&
            begin
                lhs, _ = inst.args
                lhs isa Union{SlotNumber,TypedSlot}
            end &&
            slot_id(lhs) in outputs
        )
            push!(stores, (i, slot_id(lhs)))
            continue
        end

        i in load_positions && continue

        # Record SSA values that would not be used once this pass is done:
        let out = inst
            if out isa Union{SSAValue,PiNode}
                # The way SSA/Pi are resolved should match with the escape check
                # below (i.e., to ensure that these instructions are indeed not
                # used).
                out = resolve_linear_chain(ir, out)
                if out isa SSAValue
                    out = ir[out]
                end
            end
            if out isa Union{SlotNumber,TypedSlot} && slot_id(inst) in outputs
                push!(deletelater, i)
                continue
            end
        end

        if (
            isexpr(inst, :isdefined) &&
            begin
                slot, = inst.args
                slot isa Union{SlotNumber,TypedSlot}
            end &&
            slot_id(slot) in outputs
        )
            if isassigned(defchecks, slot_id(slot))
                ssas = defchecks[slot_id(slot)]
            else
                ssas = defchecks[slot_id(slot)] = SSAValue[]
            end
            push!(ssas, SSAValue(i))
            continue
        end

        # Handle escaped slots:
        #
        # The frontend does not produce escaping task outputs.  But it's
        # possible (i.e., a valid Julia IR up to this point) that the output
        # variables to be captured (e.g., due to metaprogramming).  Since
        # loading slot other than well-defined locations marked by `Tapir._load`
        # is potentially racy, let's insert a throw if we detect an escape.
        escaped = RefValue(false)
        local escaped_variable  # TODO: handle multiple variables?
        foreach_id(identity, inst) do out
            if out isa SSAValue
                out = resolve_linear_chain(ir, out)
                if out isa SSAValue
                    out = ir[out]
                end
            end
            if out isa Union{SlotNumber,TypedSlot}
                if slot_id(out) in outputs
                    escaped.x = true
                    escaped_variable = output_names[slot_id(out)]
                end
            end
        end
        if escaped.x
            stmt[:inst] = nothing
            if escaped_variable isa SSAValue
                th = Expr(:call, Tapir.escaping_task_output_error)
            else
                th = Expr(:call, Tapir.escaping_task_output_error, escaped_variable)
            end
            insert_node!(ir, i, NewInstruction(th, Any))
            # Not using `Union{}` so that we can preserve CFG.
        end
    end

    for i in deletelater
        remove_stmt!(ir.stmts[i])
    end

    isempty(outputs) && return ir

    # Handle stores
    storemap = Vector{Vector{Any}}(undef, maxslotid)
    undefmap = Vector{Vector{Any}}(undef, maxslotid)
    for (i, oid) in stores
        oid in outputs || continue
        stmt = ir.stmts[i]
        _lhs, value = stmt[:inst].args

        if !isassigned(storemap, oid)
            newup = NewInstruction(UpsilonNode(QuoteNode(true)), Const(true))
            undefinit = insert_node!(
                ir,
                1, # [^alloca-position]
                newup,
            )
            undefs = undefmap[oid] = Any[undefinit]
            stores = storemap[oid] = Any[]
        else
            undefs = undefmap[oid]
            stores = storemap[oid]
        end

        # Reuse this statement to keep `MethodMatchInfo` (for inlining):
        stmt[:inst] = value
        ssavalue = SSAValue(i)

        newinst = NewInstruction(UpsilonNode(ssavalue), stmt[:type])
        ups = insert_node!(ir, i, newinst, true)
        push!(stores, ups)

        newinst = NewInstruction(UpsilonNode(QuoteNode(false)), Const(false))
        ups = insert_node!(ir, i, newinst, true)
        push!(undefs, ups)
    end

    for oid in outputs
        isassigned(defchecks, oid) || continue
        for ssa in defchecks[oid]
            # Duplicate upsilons to respect the load-once semantics:
            undefs = Iterators.map(undefmap[oid]) do ssa::SSAValue
                stmt = stmt_at(ir, ssa.id)
                ups = stmt[:inst]::UpsilonNode
                typ = stmt[:type]
                insert_node!(ir, insert_pos(ir, ssa.id), NewInstruction(ups, typ))
            end
            undefs = collect(Any, undefs)
            undefssa = insert_node!(ir, ssa.id, NewInstruction(PhiCNode(undefs), Bool))
            ir[ssa] = Expr(:call, GlobalRef(Intrinsics, :not_int), undefssa)
        end
    end

    # Handle loads
    undef_checks = Tuple{Int,Any,SSAValue}[]
    for (i, oid) in loads
        oid in outputs || continue

        # Replace load with PhiC
        ir.stmts.inst[i] = PhiCNode(storemap[oid])

        # Preparing for undef check
        name = output_names[oid]
        undefs = undefmap[oid]
        undefssa = insert_node!(ir, i, NewInstruction(PhiCNode(undefs), Bool))
        push!(undef_checks, (i, name, undefssa))
    end

    # Insert throw on undef:
    allocated = allocate_gotoifnot_sequence!(ir, map(first, undef_checks))
    undef_checks_index = RefValue(0)
    foreach_allocated_new_block(allocated) do ibb
        # `ibb` is the index of BB inserted at the use position `undef_checks[i][1]`
        i = undef_checks_index.x += 1
        (_, name, undefssa) = undef_checks[i]
        undefssa = SSAValue(allocated.ssachangemap[undefssa.id])

        b0 = ir.cfg.blocks[ibb]
        b1 = ir.cfg.blocks[ibb+1]
        @assert ir.stmts.inst[last(b0.stmts)] === GotoIfNot(false, ibb + 2)  # dummy
        @assert ir.stmts.inst[last(b1.stmts)] === GotoNode(ibb + 2)  # dummy

        # If defined (not undef), skip over the throw:
        ir.stmts.inst[last(b0.stmts)] = GotoIfNot(undefssa, ibb + 2)

        newex_ex = Expr(:call, GlobalRef(Base, :KeyError), name)
        newex = insert_node!(ir, last(b1.stmts), NewInstruction(newex_ex, Any))
        throw_ex = Expr(:call, GlobalRef(Base, :throw), newex)
        insert_node!(ir, last(b1.stmts), NewInstruction(throw_ex, Union{}))
        ir.stmts.inst[last(b1.stmts)] = ReturnNode()
        cfg_delete_edge!(ir.cfg, ibb + 1, ibb + 2)
    end

    if JLOptions().debug_level == 2
        verify_ir(ir)
        verify_linetable(ir.linetable)
    end

    return ir
end
# [^alloca-position]: TODO: The placement of the allocations of task output is not
# optimal.  Ideally, we should use the inner-most `Expr(:syncregion)` that
# dominates all loads.

"""
    check_tapir_race!(ir::IRCode) -> (ir′::IRCode, racy::Bool)

Inject error-throwing code when a racy phi node is found.

# Examples
```
julia> function f()
           a = 0
           Tapir.@sync begin
               Tapir.@spawn a += 1
               a += 2
           end
           a
       end;

julia> f()
ERROR: Tapir: racy load of a value that may be read concurrently: 1
```

# Design notes

In Julia, we disallow variables (slots) as task output for the sake of clarity.
Thanks to this design decision, we can conclude that any `slot2reg` promotion
that crosses task boundaries (manifested as phi nodes with continue edges) is a
result from an attempt to update variables in a parent task (that are read in
the continuation).

In principle, this detection can be done inside `slot2reg`, just like how we use
`parallel_getfield_elim_checker` (which is similar to Tapir/LLVM's
`TaskInfo::isAllocaParallelPromotable`) for disabling invalid promotion in the
SROA pass (`getfield_elim_pass!`). However, since slot is not a valid
representation in `IRCode`, we cannot refuse to promote a given slot.  Thus, the
best thing we can do is to insert helpful errors in the generated code; which is
a nonlocal transformation.  Since `slot2reg` is a delicate chain of processing
on half-constructed `IRCode`, it is more convenient and concise to do this after
`slot2reg`.  Furthermore, since the promotion checking works by "attempting" to
do `slot2reg` (i.e., requires IDF) anyway, actually doing `slot2reg` first is
equivalent.  Additionally, separating this out as a pass makes it easy to
short-circuit `run_passes`.
"""
function check_tapir_race!(ir::IRCode)
    Tapir = tapir_module()
    Tapir isa Module || return ir, false

    if !isempty(ir.new_nodes.stmts.inst)
        ir = compact!(ir)
    end

    local tasks, throw_if_uses
    racy = false
    for (ibb, bb) in pairs(ir.cfg.blocks)
        det = ir.stmts.inst[bb.stmts[end]]
        det isa DetachNode || continue
        continuation = ir.cfg.blocks[det.label]
        for iphi in continuation.stmts
            phi = ir.stmts.inst[iphi]
            phi isa PhiNode || continue
            # Racy Phi node found
            if !racy
                tasks = child_tasks(ir)
                throw_if_uses = BitSet()
            end
            racy = true
            # Insert a throw after "store" (def in child task):
            task = task_by_detach(tasks, bb.stmts[end])::ChildTask
            for (i, e) in pairs(phi.edges)
                e == ibb && continue  # skip the continue edge
                isassigned(phi.values, i) || continue  # impossible?
                v = phi.values[i]
                v isa SSAValue || continue  # constant? let's handle this on "load" side
                foreach_def(v, ir) do stmt
                    if stmt isa Instruction
                        is_stmt_in_task(task, ir, stmt.idx) || return false # break
                        stmt[:inst] isa Union{PhiNode,PiNode} && return true  # keep going

                        # This instruction is in the task and is not a branch.
                        # Insert the racy store error:
                        th = Expr(:call, Tapir.racy_store, SSAValue(stmt.idx))
                        pos = insert_pos(ir, stmt.idx)
                        attach_after = pos == stmt.idx
                        insert_node!(ir, pos, NewInstruction(th, Any), attach_after)
                        # Not using `Union{}` so that we can preserve CFG.
                    end
                    return false # break
                end
            end
            # Insert a throw before "load" (use in continuation):
            push!(throw_if_uses, iphi)
        end
    end
    racy || return  ir, false

    for i in 1:length(ir.stmts)
        inst = ir.stmts.inst[i]
        if inst isa PhiNode
            foreach_assigned_pair(inst.values) do k, v
                if v isa SSAValue && v.id in throw_if_uses
                    bb = ir.cfg.blocks[inst.edges[k]]
                    iterm = bb.stmts[end]
                    attach_after = !isterminator(ir.stmts.inst[iterm])
                    th = Expr(:call, Tapir.racy_load, v)
                    insert_node!(ir, iterm, NewInstruction(th, Any), attach_after)
                    # Not using `Union{}` so that we can preserve CFG.
                end
            end
        else
            foreach_id(identity, inst) do v
                if v isa SSAValue && v.id in throw_if_uses
                    th = Expr(:call, Tapir.racy_load, v)
                    insert_node!(ir, i, NewInstruction(th, Any))
                    # Not using `Union{}` so that we can preserve CFG.
                end
            end
        end
    end

    warn = Expr(:call, Tapir.warn_race)
    insert_node!(ir, 1, NewInstruction(warn, Any))

    ir = remove_tapir!(ir)
    ir = compact!(ir)

    if JLOptions().debug_level == 2
        @timeit "verify (race)" (verify_ir(ir); verify_linetable(ir.linetable))
    end

    return ir, true
end
# TODO: Do this at the level of abstract interpretation?
#
# The above check cannot detect a race in
#
#     function f()
#         local a
#         Tapir.@sync begin
#             Tapir.@spawn a = 1
#             a = 2
#         end
#         a
#     end
#
# ... since abstract interpretation removes `a = 1`. Also, producing a good
# error message in `IRCode` is very hard since variable names are not preserved
# any more.

"""
    remove_trivial_spawns!(ir::IRCode) -> ir′

Replace detached tasks containing only trivial code with the serial projection.
"""
function remove_trivial_spawns!(ir::IRCode)
    # This pass only looks into `ir.stmts` at the moment:
    @assert isempty(ir.new_nodes.stmts.inst)

    tasks = child_tasks(ir)
    isempty(tasks) && return ir

    cache = RefValue{Union{Vector{Vector{Int}},Nothing}}(nothing)
    function get_syncregion_to_syncs()
        syncregion_to_syncs = cache[]
        syncregion_to_syncs === nothing || return syncregion_to_syncs
        syncregion_to_syncs = Vector{Vector{Int}}(undef, length(ir.stmts))
        for bb in ir.cfg.blocks
            isync = last(bb.stmts)
            sync = ir.stmts.inst[isync]
            sync isa SyncNode || continue
            sr = (sync.syncregion::SSAValue).id
            if isassigned(syncregion_to_syncs, sr)
                ids = syncregion_to_syncs[sr]
            else
                ids = syncregion_to_syncs[sr] = Int[]
            end
            push!(ids, isync)
        end
        cache[] = syncregion_to_syncs
        return syncregion_to_syncs
    end

    syncregion_to_ntasks = zeros(Int, length(ir.stmts))
    syncregions = BitSet()
    foreach_task_depth_first(tasks) do task
        det = ir.stmts.inst[task.detach]::DetachNode
        sr = (det.syncregion::SSAValue).id
        push!(syncregions, sr)

        if is_trivial_for_spawn(ir, task)
            remove_tapir!(ir, task)
        else
            # Check if continuation is trivial:
            is_trivial = RefValue(true)
            syncs = get_syncregion_to_syncs()[sr]
            foreach_descendant(det.label, ir.cfg) do _ibb, bb
                is_trivial.x || return false
                if det.label in bb.succs  # loop
                    is_trivial.x = false
                    return false
                end
                if !is_trivial_for_spawn(ir, bb)
                    is_trivial.x = false
                    return false
                end
                return !(bb.stmts[end] in syncs)
            end
            if is_trivial.x
                remove_tapir!(ir, task)
            else
                syncregion_to_ntasks[sr] += 1
            end
        end

        return true  # continue
    end

    # Remove empty syncregions
    for sr in syncregions
        syncregion_to_ntasks[sr] > 0 && continue

        ir.stmts.inst[sr] = nothing
        for isync in get_syncregion_to_syncs()[sr]
            ir.stmts.inst[isync] = nothing
        end
    end

    @assert isempty(ir.new_nodes.stmts.inst)
    return ir
end

"""
    outline_child_task(task::ChildTask, ir::IRCode)
"""
function outline_child_task(task::ChildTask, ir::IRCode)
    Tapir = tapir_module()::Module

    uses = BitSet()
    defs = BitSet()
    args = BitSet()
    for ibb in task.blocks
        bb = ir.cfg.blocks[ibb]
        for i in bb.stmts
            inst = ir.stmts[i][:inst]
            push!(defs, i)
            inst isa ReattachNode && continue
            append_uses_and_args!(uses, args, inst)
        end
    end
    capture = setdiff(uses, defs)
    locals = setdiff(defs, capture)

    outside = setdiff!(BitSet(1:length(ir.cfg.blocks)), task.blocks)
    all_outside_uses = BitSet()
    for ibb in outside
        bb = ir.cfg.blocks[ibb]
        for i in bb.stmts
            append_uses!(all_outside_uses, ir.stmts[i][:inst])
        end
    end
    outside_uses = intersect(all_outside_uses, defs)

    nargs = length(args)
    ncaps = length(capture)
    nouts = length(outside_uses)

    ssachangemap = zeros(Int, length(ir.stmts))
    outvaluemap = zeros(Int, length(ir.stmts))
    for (i, iold) in enumerate(capture)
        inew = nargs + i
        ssachangemap[iold] = inew
    end
    offset = nargs + ncaps + nouts
    for (i, iold) in enumerate(defs)
        inew = offset + i
        ssachangemap[iold] = inew
        if iold in outside_uses
            outvaluemap[iold] = inew
            offset += 2  # two more instructions for each outside_uses
        end
    end
    @assert offset == nargs + ncaps + 3 * nouts
    labelchangemap = zeros(Int, length(ir.cfg.blocks))
    for (inew, iold) in enumerate(task.blocks)
        labelchangemap[iold] = inew
    end
    argchangemap = zeros(Int, length(ir.argtypes))
    for (inew, iold) in enumerate(args)
        argchangemap[iold] = inew
    end
    outrefmap = zeros(Int, length(ir.stmts))
    for (i, iold) in enumerate(outside_uses)
        inew = nargs + ncaps + i
        outrefmap[iold] = inew
    end

    on_label(i) = labelchangemap[i]
    on_value(v::SSAValue) = SSAValue(ssachangemap[v.id])
    on_value(v::Argument) = SSAValue(argchangemap[v.n])

    # Create a new instruction for the new outlined task:
    stmts = InstructionStream()
    resize!(stmts, nargs + ncaps + 3 * nouts + length(locals))
    # If we add new statement at the end of BB, we need to use the new statement
    # instead. `bbendmap` (initially an identity) tracks these shifts:
    bbendmap = collect(1:length(stmts))
    for (inew, iold) in enumerate(args)
        stmts.inst[inew] = Expr(:call, getfield, Argument(1), inew)
        stmts.type[inew] = ir.argtypes[iold]
        # ASK: Is this valid to declare the type of the captured variables? Is
        #      it better to insert type assertions?
    end
    for (i, iold) in enumerate(capture)
        inew = nargs + i
        stmts[inew] = ir.stmts[iold]
        stmts.inst[inew] = Expr(:call, getfield, Argument(1), inew)
        # ASK: ditto
    end
    for (i, iold) in enumerate(outside_uses)
        inew = nargs + ncaps + i
        stmts.inst[inew] = Expr(:call, getfield, Argument(1), inew)
        stmts.type[inew] = Tapir.UndefableRef{widenconst(ir.stmts[iold][:type])}
        # ASK: ditto
    end
    # Actual computation executed in the child task:
    for iold in defs
        inew = ssachangemap[iold]
        stmts[inew] = ir.stmts[iold]
        stmts.inst[inew] = map_id(on_value, on_label, stmts.inst[inew])
    end
    for iold in outside_uses
        ival = outvaluemap[iold]
        if stmts.inst[ival] isa UpsilonNode
            value = stmts.inst[ival].val
        else
            value = SSAValue(ival)
        end
        refvalue = SSAValue(outrefmap[iold])  # ::UndefableRef
        stmts.inst[ival+1] = Expr(:call, setfield!, refvalue, QuoteNode(:x), value)
        stmts.inst[ival+2] = Expr(:call, setfield!, refvalue, QuoteNode(:set), true)
        stmts.type[ival+1] = Any
        stmts.type[ival+2] = Any
        bbendmap[ival] = ival + 2
    end

    # Turn reattach nodes into return nodes (otherwise, they introduce edges to
    # invalid blocks and also the IR does not contain returns).
    for i in task.reattaches
        inew = ssachangemap[i]
        @assert stmts.inst[inew] isa ReattachNode
        stmts.inst[inew] = ReturnNode(nothing)
    end

    blocks = map(enumerate(task.blocks)) do (i, ibb)
        isentry = i == 1
        bb = ir.cfg.blocks[ibb]
        start = ssachangemap[bb.stmts[begin]]
        stop = bbendmap[ssachangemap[bb.stmts[end]]]
        preds = Int[labelchangemap[i] for i in bb.preds]
        succs = Int[labelchangemap[i] for i in bb.succs]
        if isentry
            empty!(preds)
            start = 1
        else
            @assert !(0 in preds)
        end
        # Remove edges to the BBs outside this `task`:
        # TODO: verify that these are due to reattach as expected?
        filter!(>(0), succs)

        @assert all(>(0), preds)
        @assert all(>(0), succs)

        BasicBlock(StmtRange(start, stop), preds, succs)
    end
    cfg = CFG(
        blocks,
        Int[ssachangemap[ir.cfg.index[task.blocks[i]]] for i in 1:length(task.blocks)-1],
    )
    # Due to `sort!(task.blocks)` in `child_tasks`, we should get sorted `cfg.index` here:
    @assert issorted(cfg.index)
    meta = Any[]  # TODO: copy something from `ir.meta`?
    linetable = copy(ir.linetable)  # TODO: strip off?
    argtypes = Any[Any]  # ASK: what's the appropriate "self" type for the opaque closure?
    sptypes = ir.sptypes  # TODO: strip off unused sptypes?
    taskir = IRCode(stmts, cfg, linetable, argtypes, meta, sptypes)

    # Variables to be passed onto the child task:
    captured_variables = Any[]
    for i in args
        push!(captured_variables, Argument(i))
    end
    for i in capture
        push!(captured_variables, SSAValue(i))
    end

    outputs = Tuple{Type,Int}[]
    for i in outside_uses
        T = widenconst(ir.stmts.type[i])
        push!(outputs, (T, i))
    end

    return (taskir, captured_variables, outputs), (ssachangemap, labelchangemap)
end

"""
    outline_child_task!(task::ChildTask, ir::IRCode) -> (taskir, arguments, outputs)

Extract `task` in `ir` as a new `taskir::IRCode`. Mutate `tasksir.subtasks` to
respect the changes in SSA positions and BB labels.  The second argument `ir` is
not mutated.
"""
function outline_child_task!(task::ChildTask, ir::IRCode)
    result, (ssachangemap, labelchangemap) = outline_child_task(task, ir)
    renumber_subtasks!(task, ssachangemap, labelchangemap)
    return result
end
# TODO: Calling this at each recurse of `lower_tapir_tasks!` is not great
# (quadratic in depth). Maybe keep the stack of changemaps and renumber lazily?

function renumber_subtasks!(task::ChildTask, ssachangemap, labelchangemap)
    subtasks = task.subtasks
    subtasks === nothing && return task
    for (i, t) in pairs(subtasks)
        renumber_subtasks!(t, ssachangemap, labelchangemap)
        for (j, k) in pairs(t.blocks)
            t.blocks[j] = labelchangemap[k]
        end
        for (j, k) in pairs(t.reattaches)
            t.reattaches[j] = ssachangemap[k]
        end
        subtasks[i] = ChildTask(ssachangemap[t.detach], t.reattaches, t.blocks, t.subtasks)
    end
    return task
end

"""
    tapir_module() -> Main.Base.Experimental.Tapir or nothing
"""
function tapir_module()
    isdefined(Main, :Base) || return nothing
    Base = Main.Base::Module
    isdefined(Base, :Experimental) || return nothing
    Experimental = Base.Experimental::Module
    isdefined(Experimental, :Tapir) || return nothing
    return Experimental.Tapir::Module
end

function lower_tapir!(ir::IRCode, interp::AbstractInterpreter)
    Tapir = tapir_module()
    Tapir isa Module || return ir

    tasks = child_tasks(ir)
    isempty(tasks) && return remove_tapir!(ir)

    # Replace `Expr(:syncregion)` with `%tg = Tapir.taskgroup()`.
    for i in 1:length(ir.stmts)
        stmt = ir.stmts[i]
        if isexpr(stmt[:inst], :syncregion)
            mi = find_method_instance_from_sig(
                interp,
                Tuple{typeof(Tapir.taskgroup)};
                compilesig = true,
            )
            if mi === nothing
                stmt[:inst] = Expr(:call, GlobalRef(Tapir, :taskgroup))
            else
                stmt[:inst] = Expr(:invoke, mi, GlobalRef(Tapir, :taskgroup))
            end
            stmt[:type] = Tapir.TaskGroup
        end
    end

    # Replace `sync within %tg` with `Tapir.sync!(%tg)`.
    for bb in ir.cfg.blocks
        isync = last(bb.stmts)
        sync = ir.stmts[isync][:inst]
        sync isa SyncNode || continue
        tg = sync.syncregion::SSAValue
        mi = find_method_instance_from_sig(
            interp,
            Tuple{typeof(Tapir.sync!),Tapir.TaskGroup};
            compilesig = true,
        )
        if mi === nothing
            ir.stmts.inst[isync] = Expr(:call, GlobalRef(Tapir, :sync!), tg)
        else
            ir.stmts.inst[isync] = Expr(:invoke, mi, GlobalRef(Tapir, :sync!), tg)
        end
        ir.stmts.type[isync] = Any
    end

    return lower_tapir_tasks!(ir, tasks, interp)
end

"""
    lower_tapir_task!(ir::IRCode, tasks::Vector{ChildTask}, interp) -> ir′

Process detaches and reattaches recursively. It expects that syncregion and
SyncNode are transformed to runtime function calls.
"""
function lower_tapir_tasks!(ir::IRCode, tasks::Vector{ChildTask}, interp::AbstractInterpreter)
    Tapir = tapir_module()::Module
    Tapir isa Module || return ir

    # Lower each detach of child task to the call to `Tapir.spawn!`.  It also
    # removes the detach edge to child.  That is to say, we transform
    #
    #     #detacher
    #         ...
    #         detach within %tg, #child, #continuation
    #     #child
    #         $child_code
    #         reattach within %tg, #continuation
    #     #continuation
    #     ...
    #
    # to
    #
    #     #detacher
    #         ...
    #         %oc = new_opaque_closure(%outlined_child_code, capture...)
    #         Tapir.spawn!(%tg, %oc)
    #         goto #continuation
    #     #continuation
    #     ...
    #
    # and then remove the detach edge from #detacher to #child.

    # Mapping from original def to a 2-tuple (type, ref SSA value):
    tobeloaded = IdDict{Int,Tuple{Type,SSAValue}}()
    for task in tasks
        det = ir.stmts.inst[task.detach]::DetachNode
        tg = det.syncregion::SSAValue

        taskir, arguments, outputs = outline_child_task!(task, ir)
        if task.subtasks !== nothing
            taskir = lower_tapir_tasks!(taskir, task.subtasks, interp)
        end
        meth = opaque_closure_method_from_ssair(taskir)
        for (T, iout) in outputs
            R = Tapir.UndefableRef{T}
            ref = insert_node!(ir, tg.id, NewInstruction(Expr(:new, R), R))
            setset = NewInstruction(
                Expr(:call, setfield!, ref, QuoteNode(:set), QuoteNode(false)),
                Any,
            )
            insert_node!(ir, tg.id, setset)
            push!(arguments, ref)
            @assert !haskey(tobeloaded, iout)
            tobeloaded[iout] = (T, ref)
        end
        oc_inst = NewInstruction(
            Expr(:new_opaque_closure, Tuple{}, false, Union{}, Any, meth, arguments...),
            Any,
        )
        oc = insert_node!(ir, task.detach, oc_inst)
        mi = find_method_instance_from_sig(
            interp,
            Tuple{typeof(Tapir.spawn!),Tapir.TaskGroup,Any};
            compilesig = true,
        )
        if mi === nothing
            spawn_ex = Expr(:call, Tapir.spawn!, tg, oc)
        else
            spawn_ex = Expr(:invoke, mi, Tapir.spawn!, tg, oc)
        end
        insert_node!(ir, task.detach, NewInstruction(spawn_ex, Any))
        ir.stmts.inst[task.detach] = GotoNode(det.label)
        detacher = block_for_inst(ir.cfg, task.detach)
        cfg_delete_edge!(ir.cfg, detacher, detacher + 1)
    end

    # Load task outputs:
    #
    #     ...
    #         %isset = ref.set
    #         goto #anycase if not %isset
    #     #found
    #         %load = ref.x
    #         %upsilon = ϒ(%load)
    #     #anycase
    #         %phic = φᶜ(%upsilon, ...)
    #     ...

    # Vector of 4-tuple (use position, original def position, type, ref SSA value):
    task_outputs = Tuple{Int,Int,Type,SSAValue}[]
    output_users = BitSet()
    for iuse in 1:length(ir.stmts)
        foreach_id(identity, ir.stmts[iuse][:inst]) do v
            if v isa SSAValue
                tbl = get(tobeloaded, v.id, nothing)
                if tbl !== nothing
                    push!(output_users, iuse)
                    T, ref = tbl
                    push!(task_outputs, (iuse, v.id, T, ref))
                end
            end
        end
    end
    ir1 = copy(ir)

    undef_checks = allocate_gotoifnot_sequence!(ir, map(first, task_outputs))
    original_outputs = BitSet()
    output_upsilon = IdDict{Tuple{Int,Int},Int}()  # (use pos, def pos) -> pos
    output_value = IdDict{Tuple{Int,Int},Int}()
    output_isset = IdDict{Tuple{Int,Int},Int}()
    output_index = RefValue(0)
    foreach_allocated_new_block(undef_checks) do ibb
        # `ibb` is the index of BB inserted at the use position `task_outputs[i][1]`
        i = output_index.x += 1
        (iuse, iout, T, ref) = task_outputs[i]
        iuse = undef_checks.ssachangemap[iuse]
        iout = undef_checks.ssachangemap[iout]
        ref = SSAValue(undef_checks.ssachangemap[ref.id])

        b0 = ir.cfg.blocks[ibb]
        b1 = ir.cfg.blocks[ibb+1]
        @assert ir.stmts.inst[last(b0.stmts)] === GotoIfNot(false, ibb + 2)
        @assert ir.stmts.inst[last(b1.stmts)] === GotoNode(ibb + 2)  # already set

        isset_ex = Expr(:call, getfield, ref, QuoteNode(:set))
        isset = insert_node!(ir, last(b0.stmts), NewInstruction(isset_ex, Bool))
        ir.stmts.inst[last(b0.stmts)] = GotoIfNot(isset, ibb + 2)

        # If `ref.set`, then `ref.x`:
        load_ex = Expr(:call, getfield, ref, QuoteNode(:x))
        load = insert_node!(ir, last(b1.stmts), NewInstruction(load_ex, T))

        ups = insert_node!(ir, last(b1.stmts), NewInstruction(UpsilonNode(load), T))

        output_upsilon[iuse, iout] = ups.id
        output_value[iuse, iout] = load.id
        output_isset[iuse, iout] = isset.id
        push!(original_outputs, iout)
    end
    # TODO: check that all getfield calls are dominated by sync!

    for iuse in output_users
        iuse = undef_checks.ssachangemap[iuse]
        stmt = ir.stmts[iuse]
        isphic = stmt[:inst] isa PhiCNode
        stmt[:inst] = map_id(identity, stmt[:inst]) do v
            if v isa SSAValue
                if v.id in original_outputs
                    if isphic
                        return SSAValue(output_upsilon[iuse, v.id])
                    else
                        # If the original use is not PhiC, it was used
                        # unconditionally; thus, we always load the value.
                        println(stderr, "** tapir: incomplete concversion to PhiC/Upsilon **")
                        # TODO: turn this into a proper verification pass
                        oid = output_value[iuse, v.id]
                        load = stmt_at(ir, oid)
                        newinst = NewInstruction(load[:inst], load[:type])
                        return insert_node!(ir, insert_pos(ir, output_isset[iuse, v.id]), newinst)
                    end
                end
            end
            return v
        end
    end

    # TODO: remove redundant PhiC and Upsilon nodes

    remove_syncregions!(ir)

    # Finalize the changes in the IR (clears the node inserted to `ir.new_nodes`):
    ir = compact!(ir, true)
    # Remove the dead code in the detached sub-CFG (child tasks):
    ir = cfg_simplify!(ir)
    if JLOptions().debug_level == 2
        verify_ir(ir)
        verify_linetable(ir.linetable)
    end
    return ir
end

function code_info_from_ssair(ir::IRCode)
    if JLOptions().debug_level == 2
        verify_ir(ir)
        verify_linetable(ir.linetable)
    end
    nargs = length(ir.argtypes)
    ci = ccall(:jl_new_code_info_uninit, Ref{CodeInfo}, ())
    # ci.slotnames = [Symbol(:arg, i - 1) for i in 1:nargs]  # how do I concatenate strings in Julia?
    ci.slotnames = [gensym(:arg) for _ in 1:nargs]
    ci.slotnames[1] = Symbol("#self#")
    ci.slotflags = fill(0x00, nargs)
    replace_code_newstyle!(ci, ir, nargs - 1)
    return ci
end

# ASK: Are `jl_new_code_info_uninit` and `jl_make_opaque_closure_method`
#      allowed during optimization?
"""
    opaque_closure_method_from_ssair(ir::IRCode) -> closure::Method

Create an opaque `closure` from an SSA `ir`.
"""
function opaque_closure_method_from_ssair(ir::IRCode)
    # TODO: more accurate module/functionloc detection
    if isempty(ir.linetable)
        mod = Main
        functionloc = LineNumberNode(0)
    else
        lin, = ir.linetable
        mod = lin.module
        functionloc = LineNumberNode(lin.line, lin.file)
    end
    ci = code_info_from_ssair(ir)
    nargs = length(ir.argtypes)
    name = :_tapir_outlined_function
    return ccall(
        :jl_make_opaque_closure_method,
        Ref{Method},
        (Any,Any,Any,Any,Any),
        mod, name, nargs - 1, functionloc, ci,
    )
end

is_sequential(src::CodeInfo) = all(x -> !(x isa DetachNode), src.code)

function _lower_tapir(interp::AbstractInterpreter, linfo::MethodInstance, ci::CodeInfo)
    # Making a copy here, as `convert_to_ircode` mutates `ci`:
    ci = copy(ci)

    # Ref: _typeinf(interp::AbstractInterpreter, frame::InferenceState)
    params = OptimizationParams(interp)
    opt = OptimizationState(linfo, copy(ci), params, interp)
    nargs = Int(opt.nargs) - 1 # Ref: optimize(interp, opt, params, result)

    # Ref: run_passes
    preserve_coverage = coverage_enabled(opt.mod)
    ir = convert_to_ircode(ci, copy_exprargs(ci.code), preserve_coverage, nargs, opt)
    ir = slot2reg(ir, ci, nargs, opt)
    if JLOptions().debug_level == 2
        @timeit "verify pre-tapir" (verify_ir(ir); verify_linetable(ir.linetable))
    end
    @timeit "tapir" ir = lower_tapir!(ir, interp)
    return ir, params, opt
end

function lower_tapir(interp::AbstractInterpreter, linfo::MethodInstance, ci::CodeInfo)
    is_sequential(ci) && return remove_tapir(ci)
    ir, params, opt = _lower_tapir(interp, linfo, ci)
    if JLOptions().debug_level == 2
        @timeit "verify tapir" (verify_ir(ir); verify_linetable(ir.linetable))
    end

    finish(interp, opt, params, ir, Any) # Ref: optimize(interp, opt, params, result)
    src = ir_to_codeinf!(opt)

    return remove_tapir!(src)
end

"""
    lower_tapir(linfo::MethodInstance, ci::CodeInfo) -> ci′::CodeInfo

This is called from `jl_emit_code` (`codegen.cpp`); i.e., just before compilation to
to LLVM.
"""
lower_tapir(linfo::MethodInstance, ci::CodeInfo) =
    lower_tapir(NativeInterpreter(linfo.def.primary_world), linfo, ci)
# ASK: Should we use the world age from `jl_codegen_params_t`?

# Useful for debugging:
lower_tapir_to_ircode(linfo::MethodInstance, ci::CodeInfo) =
    lower_tapir_to_ircode(NativeInterpreter(linfo.def.primary_world), linfo, ci)
lower_tapir_to_ircode(interp::AbstractInterpreter, linfo::MethodInstance, ci::CodeInfo) =
    first(_lower_tapir(interp, linfo, ci))

"""
    remove_tapir!(src::CodeInfo)
    remove_tapir!(_::Any)

Remove Tapir instructions from `src` (if any). This transformation is always valid
due to the (assumed) serial projection property of the source program.
"""
function remove_tapir!(src::CodeInfo)
    for (i, x) in pairs(src.code)
        if x isa Union{DetachNode,SyncNode}
            src.code[i] = nothing
        elseif x isa ReattachNode
            src.code[i] = GotoNode(x.label)
        elseif isexpr(x, :syncregion)
            src.code[i] = nothing
        end
    end
    return src
end
remove_tapir!(::Any) = nothing

function remove_tapir(src::CodeInfo)
    any(src.code) do x
        (x isa Union{DetachNode,ReattachNode,SyncNode}) || isexpr(x, :syncregion)
    end && return remove_tapir!(copy(src))  # warn?
    return src
end
