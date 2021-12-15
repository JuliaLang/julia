# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Tapir for Julia IR.

This file implements the compiler passes for the parallel instructions in
Julia IR that are designed based on Tapir (Schardl et al., 2019). By lowering
parallel code at the end of Julia's optimization phase, the Julia compiler can
analyze and optimize the code containing the parallel tasks.

# Overview

Called via `run_passes`:

* `early_tapir_pass!`: Mainly handles output lowering and race detection.
* `tapir_dead_store_elimination_pass!`: Simple DSE on task outputs.
* `late_tapir_pass!`: Puny-task elimination and cleanups.

Called via `jl_emit_code`:

* `lower_tapir!`: Lowers Tapir constructs to the calls to the parallel task runtime.
  This pass is called outside the usual `run_passes` to allow IPO across functions
  containing Tapir constructs.

# References

* Schardl, Tao B., William S. Moses, and Charles E. Leiserson.
  "Tapir: Embedding Recursive Fork-Join Parallelism into LLVM's Intermediate
  Representation." ACM Transactions on Parallel Computing 6, no. 4 (December
  17, 2019): 19:1–19:33. https://doi.org/10.1145/3365655.

* OpenCilk project: https://cilk.mit.edu/
  (https://github.com/OpenCilk/opencilk-project)
"""

# Ideally, we can use `add_remark` inside optimization passes. But we don't have
# access to `interp` inside optimizations ATM. Hence some global state...
const _TAPIR_REMARK = RefValue(false)
const _TAPIR_REMARK_LOG = []

"""
    tapir_remark(msg...)

Insert remark message `msg`. This function acts like `println(msg...)` inside
`Tapir.with_remarks` context.
"""
function tapir_remark(msg...)
    @nospecialize
    _TAPIR_REMARK.x || return
    push!(_TAPIR_REMARK_LOG, msg)
    return
end

function set_tapir_remark(enable::Bool)
    old = _TAPIR_REMARK.x
    _TAPIR_REMARK.x = enable
    return old
end

function tapir_get_remarks!()
    log = copy(_TAPIR_REMARK_LOG)
    empty!(_TAPIR_REMARK_LOG)
    return log
end

struct TapirRemarkInstruction
    idx::Int
    inst::Any
end

function tapir_remark_begin(sv::OptimizationState)
    @nospecialize
    _TAPIR_REMARK.x || return
    push!(_TAPIR_REMARK_LOG, sv.linfo)
    return
end

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

Iterate `tasks` recursively in the depth-first and reversed order.

Note: `remove_trivial_tapir!` relies on that this is reversed order within the
same level of tasks.
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
    foreach_task(f, tasks)

Iterate `tasks` recursively in an unspecified order.
"""
function foreach_task(f, tasks)
    worklist = collect(ChildTask, tasks)
    acc = true
    while !isempty(worklist)
        local task::ChildTask
        task = pop!(worklist)
        c = f(task)
        acc &= c
        if c && task.subtasks !== nothing
            append!(worklist, task.subtasks)
        end
    end
    return acc
end

"""
    task_by_detach(tasks::Vector{ChildTask}, detach::Int) -> nothing or task::ChildTask
"""
function task_by_detach(tasks, detach::Int)
    found = RefValue{Union{Nothing,ChildTask}}(nothing)
    foreach_task(tasks) do task
        if task.detach == detach
            found.x = task
            return false
        end
        return true
    end
    return found.x
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

Analyze `ir` and return a task tree.
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
        if isdefined(stmt, :val)
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

function foreach_assigned_pair(f, xs::Array)
    for i in 1:length(xs)
        if isassigned(xs, i)
            f(i, xs[i])
        end
    end
end

function stmt_at(ir::IRCode, pos::Int)
    pos <= length(ir.stmts) && return ir.stmts[pos]
    return ir.new_nodes.stmts[pos-length(ir.stmts)]
end

function insert_pos(ir::IRCode, pos::Int)
    pos <= length(ir.stmts) && return pos
    return ir.new_nodes.info[pos-length(ir.stmts)].pos
end

has_tapir(src::CodeInfo) = has_tapir(src.code)
has_tapir(ir::IRCode) = has_tapir(ir.stmts.inst) || has_tapir(ir.new_nodes.stmts.inst)
has_tapir(code::Vector{Any}) =
    any(code) do inst
        return inst isa Union{DetachNode,ReattachNode,SyncNode} ||
               isexpr(inst, :syncregion) ||
               isexpr(inst, :task_output)
    end

struct _InaccessibleValue end

function remove_stmt!(stmt::Instruction)
    stmt[:inst] = _InaccessibleValue()
    stmt[:type] = Any
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

function typeof_stmt_arg(ir::IRCode, @nospecialize(arg))
    if arg isa SSAValue
        return ir.stmts[arg.id][:type]
    else
        return Const(resolve_special_value(arg)[1])
    end
end

function find_method_instance_from_sig(
    interp::AbstractInterpreter,
    @nospecialize(sig::Type);
    sparams::SimpleVector = svec(),
    preexisting::Bool = false,
    compilesig::Bool = false,
)
    result = findsup(sig, method_table(interp))
    result === nothing && return nothing
    method, = result
    return specialize_method(method, sig, sparams; preexisting, compilesig)
end
# TODO: backedge?

has_loop(ir::IRCode, blocklabels) =
    any(blocklabels) do ibb
        bb = ir.cfg.blocks[ibb]
        any(<(ibb), bb.succs)
    end

function always_throws(ir::IRCode, task::ChildTask)
    seenthrow = RefValue(false)
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
            if stmt[:type] === Union{}
                seenthrow.x = true
            else
                throws.x = false
            end
            return false
        end
        throws.x || return false
    end
    return seenthrow.x
end

function try_resolve(@nospecialize(x))
    if x isa GlobalRef
        if isdefined(x.mod, x.name)
            return getfield(x.mod, x.name), true
        end
    end
    return nothing, false
end

"""
    _is_trivial_for_spawn(inst) -> ans::Bool

Check if statement `inst` is trivial for spawn. This only handles statements
that are not covered by `stmt_effect_free`.
"""
function _is_trivial_for_spawn(@nospecialize(inst))
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
        stmt = ir.stmts[istmt]
        stmt[:type] === Bottom && continue
        stmt_effect_free(stmt[:inst], stmt[:type], ir) && continue
        if !_is_trivial_for_spawn(stmt[:inst])
            tapir_remark(
                "non-trivial for spawn: ",
                TapirRemarkInstruction(istmt, stmt[:inst]),
            )
            return false
        end
    end
    return true
end

function is_trivial_for_spawn(ir::IRCode, task::ChildTask)
    detacher = block_for_inst(ir, task.detach)
    if has_loop(ir, task.blocks)
        tapir_remark(
            "non-trivial spawn: task detached from block ",
            detacher,
            " contains a loop",
        )
        return false
    end
    if always_throws(ir, task)
        tapir_remark("task detached from block ", detacher, " always throws")
        return true
    end

    for ibb in task.blocks
        is_trivial_for_spawn(ir, ir.cfg.blocks[ibb]) || return false
    end
    return true
end

""" Remove syncregion and try to remove the corresponding call to the taskgroup factory """
function remove_syncregion!(ir::IRCode, position::Integer)
    stmt = ir.stmts[position]
    ex = stmt[:inst]::Expr
    @assert ex.head === :syncregion

    # Remove `Expr(:syncregion, ...)` itself
    stmt[:inst] = nothing

    # Remove call to `Tapir.taskgroup()`
    tg = ex.args[1]
    if tg isa SSAValue
        ir[tg] = nothing
    end
end

function remove_syncregions!(ir::IRCode, stmts = 1:length(ir.stmts))
    for i in stmts
        stmt = ir.stmts[i]
        if isexpr(stmt[:inst], :syncregion)
            remove_syncregion!(ir, i)
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

"""
    remove_tapir!(ir::IRCode) -> ir

Remove Tapir instructions from `src` (if any). This transformation is always valid
due to the (assumed) serial projection property of the source program.
"""
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
    parallel_sroa_checker(ir) -> phiblocks -> is_promotable::Bool

Return a function that checks if inserting phi nodes for a given list of BB is compatible
with Tapir.

See also `check_tapir_race!`
"""
function parallel_sroa_checker(ir::IRCode)

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
    has_tapir(ir) || return ir, false
    @timeit "Lower task output" ir = lower_tapir_output!(ir)
    @timeit "Check task output" ir, racy = check_tapir_race!(ir)
    racy && return ir, racy
    @timeit "Normalize syncregion" ir = normalize_syncregions!(ir)
    return ir, racy
end

"""
    resolve_syncregion(ir::IRCode, inst) -> ssa::SSAValue or nothing

Return an `SSAValue` `ssa` such that `ir[ssa]` is a syncregion expression if
found. Return `nothing` otherwise.
"""
function resolve_syncregion(ir::IRCode, @nospecialize(inst), seen = nothing)
    inst0 = inst
    while true
        if inst isa PhiCNode
            @assert length(inst.values) == 1
            ups = ir[inst.values[1]::SSAValue]::UpsilonNode
            inst = ups.val::SSAValue
        elseif inst isa PhiNode
            seen = seen === nothing ? IdSet{SSAValue}() : seen
            for v in inst.values
                if v isa SSAValue
                    if v in seen
                        error("cycle detected")
                    else
                        push!(seen, v)
                    end
                end
            end
            let seen = seen
                resolved = Union{SSAValue,Nothing}[
                    resolve_syncregion(ir, v, seen) for v in inst.values
                ]
                isempty(resolved) && return nothing
                r = resolved[1]
                if all(x -> x == r, resolved)
                    return r
                else
                    return nothing
                end
            end
        elseif inst isa SSAValue
            sr = ir[inst]
            isexpr(sr, :syncregion) && return inst
            inst = sr
        else
            return nothing
        end
        if inst === inst0
            error("cycle detected")
        end
    end
end

"""
    normalize_syncregions!(ir::IRCode) -> ir′
"""
function normalize_syncregions!(ir::IRCode)
    if !isempty(ir.new_nodes.stmts.inst)
        ir = compact!(ir)
    end

    for bb in ir.cfg.blocks
        stmt = ir.stmts[bb.stmts[end]]
        inst = stmt[:inst]
        if inst isa SyncNode
            inst = ir[inst.syncregion::SSAValue]
            isexpr(inst, :syncregion) && continue
            ssa = resolve_syncregion(ir, inst)::SSAValue
            # TODO: validate that syncregion dominates sync
            stmt[:inst] = SyncNode(ssa)
        elseif inst isa Union{DetachNode, ReattachNode}
            # Inside a closure, syncregion may not be an SSAValue
            inst.syncregion isa SSAValue && continue
            ssa = insert_node!(ir, bb.stmts[end], NewInstruction(inst.syncregion, Any))
            if inst isa DetachNode
                stmt[:inst] = DetachNode(ssa, inst.label)
            else
                inst::ReattachNode
                stmt[:inst] = ReattachNode(ssa, inst.label)
            end
        end
    end

    return ir
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
    foreach_task_output_decl(code) do _, out
        if out isa Union{SlotNumber,TypedSlot}
            push!(outputs, slot_id(out))
        end
    end
    return outputs
end

""" "Path" to nested field; .e.g, `[1, 2, 3]` means `x[1][2][3]`. """
const TaskOutputFieldPath = Vector{Int}

""" Pair of `SSAValue` to a ref and the path to the field. """
const TaskOutputField = Tuple{SSAValue,TaskOutputFieldPath}

struct TaskOutput
    type::Type
    name::Symbol
    isset::SSAValue
    fields::Vector{TaskOutputField}
end

"""
    allocate_task_output!(ir::IRCode, t::Type, name::Symbol) -> output::TaskOutput

Insert `Tapir.OutputRef`s for representing a task.

Immutable aggregate types are decomposed into scalars to promote downstream optimizations.
"""
function allocate_task_output!(ir::IRCode, @nospecialize(t::Type), name::Symbol)
    Tapir = tapir_module()::Module
    function newref(t::Type, args...)
        @nospecialize
        args::Union{Tuple{}, Tuple{Any}}
        alloc_pos = 1   # [^alloca-position]
        R = Tapir.OutputRef{t}
        ref = insert_node!(ir, alloc_pos, NewInstruction(Expr(:new, R, args...), R))
        stmt_at(ir, ref.id)[:flag] = IR_FLAG_EFFECT_FREE
        return ref
    end

    isset = newref(Bool, false)

    fields = TaskOutputField[]
    worklist = Tuple{Type,TaskOutputFieldPath}[(t, Int[])]
    while !isempty(worklist)
        local (t, path) = pop!(worklist)
        if isconcretetype(t) && isstructtype(t) && !ismutabletype(t)
            for i in 1:fieldcount(t)
                ft = fieldtype(t, i)
                push!(worklist, (ft, push!(copy(path), i)))
            end
        else
            push!(fields, (newref(t), path))
        end
    end

    reverse!(fields)
    @assert issorted(fields, by = ((_, path),) -> path)

    return TaskOutput(t, name, isset, fields)
end
# [^alloca-position]: TODO: The placement of the allocations of task output is not
# optimal.  Ideally, we should use the inner-most `Expr(:syncregion)` that
# dominates all loads.

function insert_load!(
    ir::IRCode,
    output::TaskOutput,
    position::Int;
    attach_after::Bool = false,
)
    # Insert undef check
    isset_ex = Expr(:call, GlobalRef(Core, :getfield), output.isset, QuoteNode(:x))
    isset = insert_node!(ir, position, NewInstruction(isset_ex, Bool), attach_after)
    stmt_at(ir, isset.id)[:flag] = IR_FLAG_EFFECT_FREE
    undefcheck = Expr(:throw_undef_if_not, output.name, isset)
    insert_node!(ir, position, NewInstruction(undefcheck, Any), attach_after)

    # Load fields
    fields = Vector{SSAValue}[SSAValue[], SSAValue[]]
    currentpath = Int[]
    function flush_substructs(depth::Integer)
        # Construct nested immutables:
        while length(fields) > depth + 1
            subfields = pop!(fields)
            S = foldl(fieldtype, currentpath[1:length(fields)-1]; init = output.type)
            strct_ex = Expr(:new, S, subfields...)
            strct = insert_node!(ir, position, NewInstruction(strct_ex, S), attach_after)
            stmt_at(ir, strct.id)[:flag] = IR_FLAG_EFFECT_FREE
            push!(fields[end], strct)
        end
    end
    for (ref, path) in output.fields
        T = foldl(fieldtype, path; init = output.type)
        load_ex = Expr(:call, GlobalRef(Core, :getfield), ref, QuoteNode(:x))
        load = insert_node!(ir, position, NewInstruction(load_ex, T), attach_after)
        stmt_at(ir, load.id)[:flag] = IR_FLAG_EFFECT_FREE

        if isempty(path)  # happens only when `length(output.fields) == 1`
            subfields = pop!(fields)
            @assert isempty(subfields)
        elseif path[1:end-1] != currentpath
            prefixlength = 0
            for (i, j) in zip(path, currentpath)
                if i == j
                    prefixlength += 1
                else
                    break
                end
            end

            flush_substructs(prefixlength + 1)
            pop!(append!(empty!(currentpath), path))
            append!(fields, (SSAValue[] for _ in 1:length(path)+1-length(fields)))
        end

        push!(fields[end], load)
    end
    flush_substructs(0)

    return fields[1][1]
end

function insert_store!(ir::IRCode, output::TaskOutput, value::SSAValue)
    position = insert_pos(ir, value.id)

    subvalues = SSAValue[value]
    currentpath = Int[]
    for (ref, path) in output.fields
        prefixlength = 0
        for (i, j) in zip(path, currentpath)
            if i == j
                prefixlength += 1
            else
                break
            end
        end

        isempty(path) || pop!(append!(empty!(currentpath), path))
        resize!(subvalues, prefixlength+1)

        element = subvalues[end]
        for i in prefixlength+1:length(path)
            T = foldl(fieldtype, path[1:i]; init = output.type)
            ex = Expr(:call, GlobalRef(Core, :getfield), element, path[i])
            element = insert_node!(ir, position, NewInstruction(ex, T), true)
            stmt_at(ir, element.id)[:flag] = IR_FLAG_EFFECT_FREE
            push!(subvalues, element)
        end

        store_ex = Expr(:call, GlobalRef(Core, :setfield!), ref, QuoteNode(:x), element)
        insert_node!(ir, position, NewInstruction(store_ex, Any), true)
    end

    ex = Expr(:call, GlobalRef(Core, :setfield!), output.isset, QuoteNode(:x), true)
    insert_node!(ir, position, NewInstruction(ex, Any), true)
    return
end

"""
    lower_tapir_output!(ir::IRCode) -> ir′

Lower task output slots.
"""
function lower_tapir_output!(ir::IRCode)
    Tapir = tapir_module()
    Tapir isa Module || return ir

    # TODO: make it work without compaction?
    if !isempty(ir.new_nodes.stmts.inst)
        ir = compact!(ir)
    end

    # Collect task output slots
    outputinfo = IdDict{Int,Tuple{Symbol,LineNumberNode}}()  # slot id -> (name, line)
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

    # Analyze the slots. Determine slot types and store/load locations.
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
    taskoutputs = IdDict{Int,TaskOutput}()
    for oid in keys(outputinfo)
        T = get(slottypes, oid, Union{})
        name, = outputinfo[oid]
        taskoutputs[oid] = allocate_task_output!(ir, T, name)
    end

    # Insert loads
    for i in loaded_positions
        stmt = ir.stmts[i]
        inst = stmt[:inst]
        if inst isa PhiNode
            foreach_assigned_pair(inst.values) do k, out
                if out isa Union{SlotNumber,TypedSlot}
                    iterm = ir.cfg.blocks[inst.edges[k]].stmts[end]
                    term = ir.stmts[iterm]
                    output = get(taskoutputs, slot_id(out), nothing)
                    if output !== nothing
                        attach_after = !isterminator(term)
                        inst.values[k] = insert_load!(ir, output, iterm; attach_after)
                    end
                end
            end
        elseif isexpr(inst, :isdefined)
            out = inst.args[1]::Union{SlotNumber,TypedSlot}
            ref = taskoutputs[slot_id(out)].isset
            isset_ex = Expr(:call, GlobalRef(Core, :getfield), ref, QuoteNode(:x))
            isset = insert_node!(ir, i, NewInstruction(isset_ex, Bool))
            stmt_at(ir, isset.id)[:flag] = IR_FLAG_EFFECT_FREE
            stmt[:inst] = isset
        else
            stmt[:inst] = map_id(identity, inst) do out
                if out isa Union{SlotNumber,TypedSlot}
                    output = get(taskoutputs, slot_id(out), nothing)
                    if output !== nothing
                        return insert_load!(ir, output, i)
                    end
                end
                return out
            end
        end
    end

    # Insert stores (that are loaded somewhere)
    for oid in setdiff!(BitSet(keys(stores)), unused)
        output = taskoutputs[oid]
        for i in stores[oid]
            stmt = ir.stmts[i]
            _, rhs = stmt[:inst].args
            stmt[:inst] = rhs
            insert_store!(ir, output, SSAValue(i))
        end
    end

    return ir
end

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

In Julia, we disallow arbitrary variables (slots) as task output for the sake of
clarity.  Due to to this design decision, we can conclude that any `slot2reg`
promotion that crosses task boundaries (manifested as phi nodes with continue
edges) is a result from an attempt to update variables in a parent task (that
are read in the continuation).

In principle, this detection can be done inside `slot2reg`, just like how we use
`parallel_sroa_checker` (which is similar to Tapir/LLVM's
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
        racy_block = false
        for iphi in continuation.stmts
            phi = ir.stmts.inst[iphi]
            phi isa PhiNode || continue
            # Racy Phi node found
            if !racy
                tasks = child_tasks(ir)
                throw_if_uses = BitSet()
            end
            racy = true
            racy_block = true
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
                        th = Expr(:call, GlobalRef(Tapir, :racy_store), SSAValue(stmt.idx))
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
        if racy_block
            tapir_remark("Racy Phi node found in continuation block ", det.label)
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
                    th = Expr(:call, GlobalRef(Tapir, :racy_load), v)
                    insert_node!(ir, iterm, NewInstruction(th, Any), attach_after)
                    # Not using `Union{}` so that we can preserve CFG.
                end
            end
        else
            foreach_id(identity, inst) do v
                if v isa SSAValue && v.id in throw_if_uses
                    th = Expr(:call, GlobalRef(Tapir, :racy_load), v)
                    insert_node!(ir, i, NewInstruction(th, Any))
                    # Not using `Union{}` so that we can preserve CFG.
                end
            end
        end
    end

    warn = Expr(:call, GlobalRef(Tapir, :warn_race))
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

function tapir_dead_store_elimination_pass!(ir::IRCode)
    Tapir = tapir_module()
    Tapir isa Module || return ir
    isdefined(Tapir, :OutputRef) || return ir
    (; OutputRef) = Tapir

    if !isempty(ir.new_nodes.stmts.inst)
        ir = compact!(ir)
    end

    function is_ref_allocation(ref::SSAValue)
        local stmt = ir.stmts[ref.id]
        return widenconst(stmt[:type]) <: OutputRef && isexpr(stmt[:inst], :new)
    end

    stores = IdDict{Int,Vector{Int}}()  # ref SSA position -> setter positions
    loads = BitSet()
    for i in 1:length(ir.stmts)
        inst = ir.stmts.inst[i]

        if (
            isexpr(inst, :call) &&
            length(inst.args) == 4 &&
            begin
                f, ref, field, _value = inst.args
                field === QuoteNode(:x)
            end &&
            resolve_special_value(f)[1] === setfield! &&
            ref isa SSAValue &&
            is_ref_allocation(ref)
        )
            push!(get!(() -> Int[], stores, ref.id), i)
        else
            # Treat everything else as a load
            foreach_id(identity, inst) do v
                if v isa SSAValue
                    if is_ref_allocation(v)
                        push!(loads, v.id)
                    end
                end
            end
        end
    end

    # Remove task outputs that are not used:
    unused = setdiff!(BitSet(keys(stores)), loads)
    for i in unused
        ir.stmts[i][:inst] = nothing
        for j in stores[i]
            ir.stmts[j][:inst] = nothing
        end
    end

    return ir
end

"""
    late_tapir_pass!(ir::IRCode) -> ir′

Late but not final.
"""
function late_tapir_pass!(ir::IRCode)
    tasks = nothing
    @timeit "Detect misplaced detach" (ir, tasks) = disallow_detach_after_sync!(ir, tasks)
    @timeit "Remove trivial Tapir" (ir, tasks) = remove_trivial_tapir!(ir, tasks)
    return ir
end

"""
    disallow_detach_after_sync!(ir::IRCode, tasks) -> ir′

Detect detach after sync and insert runtime errors.

This can happen when a taskgroup is captured by a closure and then get inlined
(at a wrong position).

Example:

    local closure
    Tapir.@sync begin
        closure = function ()
            Tapir.@spawn ...
        end
        ...
    end
    closure()
"""
function disallow_detach_after_sync!(ir::IRCode, tasks)
    # This pass only looks into `ir.stmts` at the moment:
    @assert isempty(ir.new_nodes.stmts.inst)

    Tapir = tapir_module()
    Tapir isa Module || return (remove_tapir!(ir), tasks)

    if tasks === nothing
        tasks = child_tasks(ir)
    end

    changed = RefValue(false)
    foreach_task(tasks) do task
        det = ir.stmts.inst[task.detach]::DetachNode
        sr = resolve_syncregion(ir, det.syncregion::SSAValue)
        sr isa SSAValue || return true

        should_remove = RefValue(false)
        detacher = block_for_inst(ir, task.detach)
        foreach_ancestor(detacher, ir.cfg) do ibb, bb
            should_remove.x && return false  # short-circuit

            inst = ir.stmts.inst[bb.stmts[end]]
            if inst isa SyncNode
                if resolve_syncregion(ir, inst.syncregion) === sr
                    should_remove.x = true
                end
            end

            if sr.id in bb.stmts
                # This block contains the corresponding syncregion. Stop ascending.
                return false
            end
            return true
        end

        if should_remove.x
            changed.x = true
            ex = Expr(:call, GlobalRef(Tapir, :detach_after_sync_error))
            insert_node!(ir, task.detach, NewInstruction(ex, Any))
            remove_tapir!(ir, task)
        end
        return true
    end

    if changed.x
        ir = compact!(ir)
        return ir, nothing
    else
        return ir, tasks
    end
end

"""
    remove_trivial_tapir!(ir::IRCode, tasks) -> ir′

Remove trivial spawns and syncregions.

* Remove trivial spawns (serial projection).
* Remove empty syncregions.

This is a single monolithic function at the moment for sharing the computed
mapping `syncregion_to_syncs` between the above semantically distinct passes.
"""
function remove_trivial_tapir!(ir::IRCode, tasks)
    # This pass only looks into `ir.stmts` at the moment:
    @assert isempty(ir.new_nodes.stmts.inst)

    if tasks === nothing
        tasks = child_tasks(ir)
    end
    # [^empty-tasks]: we can't bail out here even if `isempty(tasks)` since `ir`
    # may also have some empty syncregions (which are processed below).

    # Scan syncregion and SyncNode
    syncregions = BitSet()
    syncregion_to_syncs = Vector{Vector{Int}}(undef, length(ir.stmts))
    for bb in ir.cfg.blocks
        isync = last(bb.stmts)
        sync = ir.stmts.inst[isync]
        sync isa SyncNode || continue
        sr = (sync.syncregion::SSAValue).id
        push!(syncregions, sr)
        if isassigned(syncregion_to_syncs, sr)
            ids = syncregion_to_syncs[sr]
        else
            ids = syncregion_to_syncs[sr] = Int[]
        end
        push!(ids, isync)
    end

    # Remove trivial spawns:
    removed_tasks = RefValue(0)
    foreach_task_depth_first(tasks) do task
        ibb_det = block_for_inst(ir, task.detach)
        det = ir.stmts.inst[task.detach]::DetachNode
        sr = det.syncregion::SSAValue

        if is_trivial_for_spawn(ir, task)
            remove_tapir!(ir, task)
            removed_tasks.x += 1
            tapir_remark("removed a trivial spawn detached at block ", ibb_det)
        elseif isassigned(syncregion_to_syncs, sr.id) # has a SyncNode
            # Check if the continuation is trivial. This is done only when the
            # task is not spawned from a closure (that captures the syncregion
            # token). If so, it's not possible to see the entire continuation.
            is_trivial = RefValue(true)
            syncs = syncregion_to_syncs[sr.id]
            foreach_descendant(det.label, ir.cfg) do ibb, bb
                is_trivial.x || return false
                term = ir.stmts[bb.stmts[end]]
                if term[:inst] isa DetachNode
                    # This also detects a loop
                    is_trivial.x = false
                    tapir_remark(
                        "non-trivial spawn: detach found in block ",
                        ibb,
                        " which is a part of continuation of detach at block ",
                        ibb_det,
                    )
                    return false
                end
                if !is_trivial_for_spawn(ir, bb)
                    is_trivial.x = false
                    return false
                end
                if any(<(ibb), bb.succs)
                    tapir_remark(
                        "non-trivial spawn: continuation of detach at block ",
                        ibb_det,
                        " contains a loop at block ",
                        ibb,
                    )
                    is_trivial.x = false
                    return false
                end
                return !(bb.stmts[end] in syncs)
            end
            if is_trivial.x
                remove_tapir!(ir, task)
                removed_tasks.x += 1
                tapir_remark(
                    "removed a spawn (detached at block ",
                    ibb_det,
                    ") with trivial continuation",
                )
            end
        end

        return true  # continue
    end

    isempty(syncregions) && return ir, tasks

    # Find used syncregions:
    # Note: Scanning all statements (not just detach and reattach) since the
    # syncregion token may be captured by closures.
    used = BitSet()
    for i in 1:length(ir.stmts)
        inst = ir.stmts.inst[i]
        inst isa SyncNode && continue
        foreach_id(identity, inst) do v
            if v isa SSAValue
                if v.id in syncregions
                    push!(used, v.id)
                end
            end
        end
    end

    # Remove empty syncregions:
    for sr in syncregions
        sr in used && continue
        remove_syncregion!(ir, sr)
        for isync in syncregion_to_syncs[sr]
            ir.stmts.inst[isync] = nothing
        end
    end

    nremoved = length(syncregions) - length(used)
    if nremoved > 0
        tapir_remark(
            "removed ",
            length(used) == 0 ? "all " : "",
            nremoved,
            " syncregion(s) and ",
            removed_tasks.x,
            " task(s)",
        )
    end

    return ir, tasks
end

"""
    outline_child_task(task::ChildTask, ir::IRCode)
"""
function outline_child_task(task::ChildTask, ir::IRCode)
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

    nargs = length(args)
    ncaps = length(capture)

    ssachangemap = zeros(Int, length(ir.stmts))
    for (i, iold) in enumerate(capture)
        inew = nargs + i
        ssachangemap[iold] = inew
    end
    offset = nargs + ncaps
    for (i, iold) in enumerate(defs)
        inew = offset + i
        ssachangemap[iold] = inew
    end
    @assert offset == nargs + ncaps
    labelchangemap = zeros(Int, length(ir.cfg.blocks))
    for (inew, iold) in enumerate(task.blocks)
        labelchangemap[iold] = inew
    end
    argchangemap = zeros(Int, length(ir.argtypes))
    for (inew, iold) in enumerate(args)
        argchangemap[iold] = inew
    end

    on_label(i) = labelchangemap[i]
    on_value(v::SSAValue) = SSAValue(ssachangemap[v.id])
    on_value(v::Argument) = SSAValue(argchangemap[v.n])

    # Create a new instruction for the new outlined task:
    stmts = InstructionStream()
    resize!(stmts, nargs + ncaps + length(locals))
    # If we add new statement at the end of BB, we need to use the new statement
    # instead. `bbendmap` (initially an identity) tracks these shifts:
    bbendmap = collect(1:length(stmts))
    for (inew, iold) in enumerate(args)
        stmts.inst[inew] = Expr(:call, GlobalRef(Core, :getfield), Argument(1), inew)
        stmts.type[inew] = ir.argtypes[iold]
        # ASK: Is this valid to declare the type of the captured variables? Is
        #      it better to insert type assertions?
    end
    for (i, iold) in enumerate(capture)
        inew = nargs + i
        stmts[inew] = ir.stmts[iold]
        stmts.inst[inew] = Expr(:call, GlobalRef(Core, :getfield), Argument(1), inew)
        # ASK: ditto
    end
    selftype = Tuple{Any[widenconst(stmts.type[i]) for i in 1:nargs + ncaps]...}
    # Actual computation executed in the child task:
    for iold in defs
        inew = ssachangemap[iold]
        stmts[inew] = ir.stmts[iold]
        stmts.inst[inew] = map_id(on_value, on_label, stmts.inst[inew])
    end

    # Turn reattach nodes into return nodes (otherwise, they introduce edges to
    # invalid blocks and also the IR does not contain returns).
    for i in task.reattaches
        inew = ssachangemap[i]
        @assert stmts.inst[inew] isa ReattachNode
        stmts.inst[inew] = ReturnNode(nothing)
    end

    blocks = Iterators.map(enumerate(task.blocks)) do (i, ibb)
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
    blocks = collect(BasicBlock, blocks)
    cfg = CFG(
        blocks,
        Int[ssachangemap[ir.cfg.index[task.blocks[i]]] for i in 1:length(task.blocks)-1],
    )
    # Due to `sort!(task.blocks)` in `child_tasks`, we should get sorted `cfg.index` here:
    @assert issorted(cfg.index)
    meta = Any[]  # TODO: copy something from `ir.meta`?
    linetable = copy(ir.linetable)  # TODO: strip off?
    argtypes = Any[selftype]
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

    return (taskir, captured_variables), (ssachangemap, labelchangemap)
end

"""
    outline_child_task!(task::ChildTask, ir::IRCode) -> (taskir, arguments)

Extract `task` in `ir` as a new `taskir::IRCode`. Mutate `tasksir.subtasks` to
respect the changes in SSA positions and BB labels.  The second argument `ir` is
not mutated.

The sub-CFG in `ir` corresponding to `task` must not have uncommitted new nodes
in `ir.new_nodes`.
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
    Tapir isa Module || return remove_tapir!(ir)

    tasks = child_tasks(ir)  # [^empty-tasks]

    ir, tasks = optimize_taskgroups!(ir, tasks, interp)

    # Replace `Expr(:syncregion, %tg)` with `%tg`.
    for i in 1:length(ir.stmts)
        stmt = ir.stmts[i]
        if isexpr(stmt[:inst], :syncregion)
            value = stmt[:inst].args[1]
            stmt[:inst] = value
            stmt[:type] = typeof_stmt_arg(ir, value)
        end
    end

    # Replace `sync within %tg` with `Tapir.sync!(%tg)`.
    for bb in ir.cfg.blocks
        isync = last(bb.stmts)
        sync = ir.stmts[isync][:inst]
        sync isa SyncNode || continue
        tg = sync.syncregion::SSAValue
        TaskGroup = widenconst(ir.stmts[tg.id][:type])::Type
        mi = find_method_instance_from_sig(
            interp,
            Tuple{typeof(Tapir.sync!),TaskGroup};
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
    optimize_taskgroups!(ir::IRCode, tasks, interp::AbstractInterpreter) -> (ir′, tasks′)

If we know a static bound on the number of tasks spawned, we don't need to
allocate the taskgroup. Instead of using `Tapir.spawn!(taskgroup, f)` and
`Tapir.sync!(tg)`, we lower the tasks to `Tapir.spawn(TaskGroup::Type, f)` and
`Tapir.synctasks(tasks...)`.
"""
function optimize_taskgroups!(
    ir::IRCode,
    tasks::Vector{ChildTask},
    interp::AbstractInterpreter,
)
    Tapir = tapir_module()::Module

    # TODO: another possible optimization is to use tree of taskgroups to avoid
    # using the locked taskgroup (i.e., channel) entirely.

    # Find syncregions that do not have to allocate TaskGroup
    syncregions = BitSet()
    isinlinable = trues(length(ir.stmts))
    tasks_by_syncregion = IdDict{Int,Vector{ChildTask}}()
    foreach_task(tasks) do task
        det = ir.stmts.inst[task.detach]::DetachNode
        sr = det.syncregion::SSAValue
        push!(get!(() -> ChildTask[], tasks_by_syncregion, sr.id), task)
        isinlinable[sr.id] || return true

        srstmt = stmt_at(ir, sr.id)
        srinst = srstmt[:inst]
        if !isexpr(srinst, :syncregion)
            isinlinable[sr.id] = false
            return true
        elseif !isconcretetype(widenconst(typeof_stmt_arg(ir, srinst.args[1])))
            isinlinable[sr.id] = false
            return true
        end

        push!(syncregions, sr.id)

        # Check if this syncregion is used in subtasks.
        for ibb in task.blocks
            local bb = ir.cfg.blocks[ibb]
            local iterm = bb.stmts[end]
            local term = ir.stmts.inst[iterm]
            if term isa DetachNode
                if term.syncregion === sr
                    isinlinable[sr.id] = false
                    return true
                end
            end
        end

        # Check if detach is used in a loop inside the syncregion.
        detacher = block_for_inst(ir, task.detach)
        continuation = det.label
        visited = falses(length(ir.cfg.blocks))
        foreach_descendant(continuation, ir.cfg, visited) do ibb, bb
            if ibb == detacher
                isinlinable[sr.id] = false
                return false
            end
            iterm = bb.stmts[end]
            term = ir.stmts.inst[iterm]
            if term isa SyncNode
                if term.syncregion === sr
                    return false
                end
            elseif term isa DetachNode
                # Don't iterate inside sibling tasks (Note: we can' "cut" the
                # whole sub-CFG corresponding to the child task by marking the
                # entry BB visited since it only has a single entry):
                visited[ibb + 1] = true
            end
            return true
        end
    end

    syncnodes_by_syncregion = IdDict{Int,Vector{Int}}()
    for bb in ir.cfg.blocks
        isync = bb.stmts[end]
        sync = ir.stmts.inst[isync]
        sync isa SyncNode || continue
        sr = sync.syncregion::SSAValue
        isinlinable[sr.id] || continue
        push!(get!(() -> Int[], syncnodes_by_syncregion, sr.id), isync)
    end
    isempty(syncnodes_by_syncregion) && return ir, tasks

    cache = IdDict{Type,Type}()
    function sync_return_type(@nospecialize(TaskGroup::Type))
        @assert isconcretetype(TaskGroup)  # due to isinlinable check above
        return get!(cache, TaskGroup) do
            ccall(:jl_typeinf_begin, Cvoid, ())
            mi = find_method_instance_from_sig(
                interp,
                Tuple{typeof(Tapir.spawn),Type{TaskGroup},Any};
                compilesig = true,
            )
            if mi === nothing
                # TODO: world age issue?
                print(stderr, "WARNING: cannot find a method for `Tapir.spawn` on ")
                println(stderr, "`Type{", TaskGroup, "}`")
                T = Any
            else
                result = InferenceResult(mi)
                # TODO: Is it safe to use the global cache when it's called from
                # `jl_emit_code`?
                frame = InferenceState(result, :no, interp)
                if frame !== nothing
                    typeinf(interp, frame)
                end
                T = widenconst(ignorelimited(result.result))
            end
            ccall(:jl_typeinf_end, Cvoid, ())
            return T
        end
    end

    # Insert runtime calls
    ConcreteMaybe = Tapir.ConcreteMaybe
    for i in syncregions
        isinlinable[i] || continue
        tg, = ir.stmts[i][:inst].args
        TaskGroup = widenconst(typeof_stmt_arg(ir, tg))
        TaskType = sync_return_type(TaskGroup)
        if isconcretetype(TaskType)
            MaybeTask = ConcreteMaybe{TaskType}
        else
            MaybeTask = ConcreteMaybe{Any}
        end
        remove_syncregion!(ir, i)
        taskgroup = Vector{PhiCNode}[PhiCNode[] for _ in syncnodes_by_syncregion[i]]
        for task in tasks_by_syncregion[i]
            detstmt = ir.stmts[task.detach]
            det = detstmt[:inst]::DetachNode

            # Place a dummy instruction for marking that this detach should be
            # handled with Tapir.spawn and not Tapir.spawn!.
            placeholder = Expr(:spawn_placeholder, TaskGroup)
            dest = insert_node!(ir, task.detach, NewInstruction(placeholder, TaskType))
            detstmt[:inst] = DetachNode(dest, det.label)

            for phics in taskgroup
                i0 = NewInstruction(UpsilonNode(nothing), Nothing)
                i1 = NewInstruction(UpsilonNode(dest), TaskType)
                t0 = insert_node!(ir, i, i0)
                t1 = insert_node!(ir, task.detach, i1)
                push!(phics, PhiCNode(Any[t0, t1]))
            end
        end
        # Insert `Tapir.synctasks(...)`
        for (isync, phics) in zip(syncnodes_by_syncregion[i], taskgroup)
            syncargs = Any[
                let load = insert_node!(
                        ir,
                        isync,
                        NewInstruction(phic, Union{Nothing,TaskType}),
                    )
                    insert_node!(
                        ir,
                        isync,
                        NewInstruction(Expr(:new, MaybeTask, load), MaybeTask),
                    )
                end for phic in phics
            ]
            mi = find_method_instance_from_sig(
                interp,
                Tuple{typeof(Tapir.synctasks),[MaybeTask for _ in syncargs]...};
                compilesig = true,
            )
            if mi === nothing
                ir.stmts.inst[isync] =
                    Expr(:call, GlobalRef(Tapir, :synctasks), syncargs...)
            else
                ir.stmts.inst[isync] =
                    Expr(:invoke, mi, GlobalRef(Tapir, :synctasks), syncargs...)
            end
            ir.stmts.type[isync] = Any
        end
    end

    # Require `compact!` here since `outline_child_task!` expects no pending new
    # nodes.
    ir = compact!(ir)

    # TODO: Store BB labels in ChildTask so that we don't have to re-compute it here
    return ir, child_tasks(ir)
end

"""
    lower_tapir_task!(ir::IRCode, tasks::Vector{ChildTask}, interp) -> ir′

Process detaches and reattaches recursively. It expects that syncregion and
SyncNode are transformed to runtime function calls.
"""
function lower_tapir_tasks!(ir::IRCode, tasks::Vector{ChildTask}, interp::AbstractInterpreter)
    Tapir = tapir_module()::Module

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
    # and then remove the detach edge from #detacher to #child. Use
    # `Tapir.spawn` instead if `%tg` points to `:spawn_placeholder`.
    for task in tasks
        taskir, arguments = outline_child_task!(task, ir)
        if task.subtasks !== nothing
            taskir = lower_tapir_tasks!(taskir, task.subtasks, interp)
        end
        meth = opaque_closure_method_from_ssair(taskir)
        oc_inst = NewInstruction(
            Expr(:new_opaque_closure, Tuple{}, false, Union{}, Any, meth, arguments...),
            Any,
        )

        det = ir.stmts.inst[task.detach]::DetachNode
        tg = det.syncregion::SSAValue
        if isexpr(ir[tg], :spawn_placeholder)
            TaskGroup = ir[tg].args[1]::Type
            oc = insert_node!(ir, tg.id, oc_inst)
            mi = find_method_instance_from_sig(
                interp,
                Tuple{typeof(Tapir.spawn),Type{TaskGroup},Any};
                compilesig = true,
            )
            if mi === nothing
                spawn_ex = Expr(:call, GlobalRef(Tapir, :spawn), TaskGroup, oc)
            else
                spawn_ex = Expr(:invoke, mi, GlobalRef(Tapir, :spawn), TaskGroup, oc)
            end
            ir[tg] = spawn_ex
        else
            TaskGroup = widenconst(ir.stmts[tg.id][:type])
            oc = insert_node!(ir, task.detach, oc_inst)
            mi = find_method_instance_from_sig(
                interp,
                Tuple{typeof(Tapir.spawn!),TaskGroup,Any};
                compilesig = true,
            )
            if mi === nothing
                spawn_ex = Expr(:call, GlobalRef(Tapir, :spawn!), tg, oc)
            else
                spawn_ex = Expr(:invoke, mi, GlobalRef(Tapir, :spawn!), tg, oc)
            end
            insert_node!(ir, task.detach, NewInstruction(spawn_ex, Any))
        end
        ir.stmts.inst[task.detach] = GotoNode(det.label)
        detacher = block_for_inst(ir.cfg, task.detach)
        cfg_delete_edge!(ir.cfg, detacher, detacher + 1)
    end

    ir = remove_tapir!(ir)

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
    ci.slottypes = Any[widenconst(ir.argtypes[i]) for i in 1:nargs]
    ci.slotnames = Symbol[gensym(:arg) for _ in 1:nargs]
    ci.slotnames[1] = Symbol("#self#")
    ci.slotflags = fill(0x00, nargs)
    replace_code_newstyle!(ci, ir, nargs)
    ci.inferred = true
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

function _lower_tapir(interp::AbstractInterpreter, linfo::MethodInstance, ci::CodeInfo)
    # Making a copy here, as `convert_to_ircode` mutates `ci`:
    ci = copy(ci)

    # Ref: _typeinf(interp::AbstractInterpreter, frame::InferenceState)
    params = OptimizationParams(interp)
    opt = OptimizationState(linfo, copy(ci), params, interp)

    # Ref: run_passes
    ir = convert_to_ircode(ci, opt)
    ir = slot2reg(ir, ci, opt)
    if JLOptions().debug_level == 2
        @timeit "verify pre-tapir" (verify_ir(ir); verify_linetable(ir.linetable))
    end
    @timeit "tapir" ir = lower_tapir!(ir, interp)
    return ir, params, opt
end

function lower_tapir(interp::AbstractInterpreter, linfo::MethodInstance, ci::CodeInfo)
    has_tapir(ci) || return ci
    ir, params, opt = _lower_tapir(interp, linfo, ci)
    if JLOptions().debug_level == 2
        @timeit "verify tapir" (verify_ir(ir); verify_linetable(ir.linetable))
    end

    finish(interp, opt, params, ir, Any) # Ref: optimize(interp, opt, params, result)
    src = ir_to_codeinf!(opt)

    return src
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
