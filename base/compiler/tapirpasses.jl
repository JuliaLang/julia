"""
Passes on Julia IR to handle Tapir.

This file implements the compiler passes for the parallel instructions in
Julia IR that are designed based on Tapir (Schardl et al., 2019). The main
entry point is `lower_tapir!` that outlines the parallel instructions as
closures wrapped in the standard Julia `Task`. By lowering parallel code at
the end of Julia's optimization phase, the Julia compiler can analyze and
optimize the code containing the parallel tasks. In the future, we may be
able to push the outlining of the tasks further down in the compilation
pipeline using the OpenCilk compiler, to unlock the optimizations in the LLVM
passes.

# References

* Schardl, Tao B., William S. Moses, and Charles E. Leiserson.
  "Tapir: Embedding Recursive Fork-Join Parallelism into LLVM’s Intermediate
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
end
ChildTask(detach::Int) = ChildTask(detach, Int[], Int[])

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
            @assert term.reattach in bb.succs
            @assert term.label in bb.succs
            @assert length(bb.succs) == 2
            push!(tasks, detached_sub_cfg!(bb.stmts[end], ir, visited))
        end
        return true
    end
    return tasks
end

"""
    detached_sub_cfg!(detach::Int, ir::IRCode, visited) -> task::ChildTask

Find a sub CFG detached by the detach node `ir.stmts[detach]`. It mutates
`visited` but not other arguments.
"""
function detached_sub_cfg!(detach::Int, ir::IRCode, visited)
    detachnode = ir.stmts.inst[detach]::DetachNode
    task = ChildTask(detach)
    foreach_descendant(detachnode.label, ir.cfg, visited) do ibb, bb
        push!(task.blocks, ibb)
        bb = ir.cfg.blocks[ibb]
        term = ir.stmts[bb.stmts[end]][:inst]
        if term isa ReattachNode
            @assert bb.succs == [ibb + 1]
            continuation = ir.cfg.blocks[ibb + 1]
            for k in continuation.preds
                i = ir.cfg.blocks[k].stmts[end]
                if i == detach
                    push!(task.reattaches, bb.stmts[end])
                    return false
                end
            end
        end
        return true
    end
    sort!(task.blocks)
    @assert task.blocks[1] == detachnode.label  # entry block
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

Map over the locations containing IDs in `stmt`; i.e., run the function
`on_value` on `SSAValue` or `Arguments` in `stmt` and the function `on_label`
on basic block label in `stmt`. Return a new instruction `stmt′` with
corresponding "fields" updated by these functions.

# Arguments
* `stmt`: an IR statement (e.g., an `Expr(:call, ...)`)
* `on_value`: a function that accepts an `SSAValue` or `Argument`
* `on_label`: a function that accepts a basic block label (e.g., `goto.label`
  of a `goto::GotoNode` statement)
"""
function map_id(on_value, on_label, @nospecialize(stmt))
    recurse(@nospecialize x) = map_id(on_value, on_label, x)
    if stmt isa SSAValue
        on_value(stmt)
    elseif stmt isa Argument
        on_value(stmt)
    elseif stmt isa GotoNode
        GotoNode(on_label(stmt.label))
    elseif stmt isa GotoIfNot
        GotoIfNot(recurse(stmt.cond), on_label(stmt.dest))
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
            PhiNode(Int32[on_label(x) for x in stmt.edges], newvalues)
        else
            PhiCNode(newvalues)
        end
    elseif stmt isa UpsilonNode
        if isdefined(stmt, :val)
            UpsilonNode(stmt.val)
        else
            stmt
        end
    elseif stmt isa DetachNode
        DetachNode(recurse(stmt.syncregion), on_label(stmt.label), on_label(stmt.reattach))
    elseif stmt isa ReattachNode
        ReattachNode(recurse(stmt.syncregion), on_label(stmt.label))
    elseif stmt isa SyncNode
        SyncNode(recurse(stmt.syncregion))
    elseif stmt isa Expr
        if is_meta_expr_head(stmt.head)
            stmt
        elseif stmt.head === :(=) && stmt.args[2] isa Expr
            Expr(stmt.head, stmt.args[1], recurse(stmt.args[2]))
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
function foreach_descendant(f::F, ibb::Int, cfg::CFG, visited = falses(length(cfg.blocks))) where {F}
    function g(ibb)
        visited[ibb] && return true
        visited[ibb] = true
        bb = cfg.blocks[ibb]
        f(ibb, bb) || return false
        cond = true
        for succ in bb.succs
            cond &= g(succ)
        end
        return cond
    end
    return g(ibb)
end
# TODO: use worklist?

function foreach_def(f::F, v::SSAValue, ir::IRCode, visited = falses(length(ir.stmts))) where {F}
    function g(i)
        visited[i] && return true
        visited[i] = true
        stmt = ir.stmts[i]
        ans = f(stmt)
        ans || return false
        cont = true
        foreach_id(identity, stmt[:inst]) do v
            if v isa SSAValue
                cont &= g(v.id)
            else
                cont &= f(v)
            end
        end
        return cont
    end
    return g(v.id)
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

"""
    for_each_statement_in_each_block(ir) do foreach_statement, ibb, bb
        # use bb
        foreach_statement() do insert_position, stmt
            # use stmt
        end
    end

Iterate over basic blocks and statements including inserted nodes in the correct order.
"""
function for_each_statement_in_each_block(bb_user, ir::IRCode)
    new_nodes_info = ir.new_nodes.info
    indices = sortperm(
        new_nodes_info;
        alg = Sort.DEFAULT_STABLE,
        by = nni -> (nni.pos, nni.attach_after),
    )
    order = Order.By() do i
        if i isa NewNodeInfo
            nni = i
        else
            nni = new_nodes_info[i]
        end
        (nni.pos, nni.attach_after)
    end
    nn_offset = length(ir.stmts)
    hi = length(indices)
    lo::Int = 1
    for (ibb, bb) in enumerate(ir.cfg.blocks)
        bb_user(ibb, bb) do stmt_user
            for i in bb.stmts
                nni1 = NewNodeInfo(i, false)
                nni2 = NewNodeInfo(i, true)
                before = searchsorted(indices, nni1, lo, hi, order)
                after = searchsorted(indices, nni2, last(before) + 1, hi, order)
                lo = last(after) + 1
                for j in before
                    stmt = ir.new_nodes.stmts[indices[j]]
                    stmt_user(stmt.idx + length(ir.stmts), stmt)
                end
                stmt_user(i, ir.stmts[i])
                for j in after
                    stmt = ir.new_nodes.stmts[indices[j]]
                    stmt_user(stmt.idx + length(ir.stmts), stmt)
                end
            end
        end
    end
end

"""
    insert_new_nodes(ir::IRCode) -> ir′::IRCode

Process `ir.new_nodes` and create an equivalent `ir′` such that `ir′.new_nodes` is empty.
Return `ir` as-is if `ir.new_node)` is already empty.  It differs from `compact!(ir)` in
that it does not perform any DCE.
"""
function insert_new_nodes(ir::IRCode)
    isempty(ir.new_nodes.stmts.inst) && return ir
    stmts = InstructionStream(length(ir.stmts) + length(ir.new_nodes.stmts))
    ssachangemap = zeros(Int, length(stmts))
    bbstarts = zeros(Int, length(ir.cfg.blocks))
    bbstops = zeros(Int, length(ir.cfg.blocks))
    newpos::Int = 1
    for_each_statement_in_each_block(ir) do foreach_statement, ibb, _bb
        bbstarts[ibb] = newpos
        foreach_statement() do idx, stmt
            stmts[newpos] = stmt
            ssachangemap[idx] = newpos
            newpos += 1
        end
        bbstops[ibb] = newpos - 1
    end
    @assert all(>(0), ssachangemap)

    on_value(v::SSAValue) = SSAValue(ssachangemap[v.id])
    on_value(v::Argument) = v
    for i in 1:length(stmts)
        stmts.inst[i] = map_id(on_value, identity, stmts.inst[i])
    end

    blocks = BasicBlock[
        BasicBlock(bb, StmtRange(bbstarts[i], bbstops[i]))
        for (i, bb) in enumerate(ir.cfg.blocks)
    ]
    popfirst!(bbstarts)
    cfg = CFG(blocks, bbstarts)
    return IRCode(ir, stmts, cfg, NewNodeStream())
end

is_sequential(stmts::InstructionStream) = !any(inst -> inst isa DetachNode, stmts.inst)
is_sequential(ir::IRCode) = is_sequential(ir.stmts) && is_sequential(ir.new_nodes.stmts)

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

function early_tapir_pass!(ir::IRCode)
    is_sequential(ir) && return ir
    Tapir = tapir_module()
    Tapir isa Module || return ir
    Base = Main.Base::Module

    # Not calling `compact!(ir)` to avoid any DCE at this point. For example,
    # constant propagation makes it very difficult to determine where we should
    # initialize the memory.
    ir = insert_new_nodes(ir)
    @assert isempty(ir.new_nodes.stmts.inst)

    # TODO: lazily create?
    domtree = construct_domtree(ir.cfg.blocks)

    # maybe use bit-packed BitSet?
    isusedby_cache = IdDict{Tuple{Int,Int},Bool}()
    isusedby(def, use) = get!(isusedby_cache, (def, use)) do
        phi = ir.stmts.inst[use]::PhiNode
        any_assigned(phi.values) do v
            if v isa SSAValue
                if v.id <= length(ir.stmts)
                    v.id == def && return true
                    inst = ir.stmts.inst[v.id]
                    if inst isa PhiNode
                        return isusedby(def, v.id)
                    end
                end
            end
            return false
        end
    end

    ssamap = Vector{Any}(undef, length(ir.stmts))
    toref = BitSet()
    usersof = Vector{BitSet}(undef, length(ir.stmts))
    for (ichild, bb) in enumerate(ir.cfg.blocks)
        reattach = ir.stmts.inst[bb.stmts[end]]
        reattach isa ReattachNode || continue
        continuation = ir.cfg.blocks[reattach.label]
        for iphi in continuation.stmts
            phi = ir.stmts.inst[iphi]
            if phi isa PhiNode
                local idetacher::BBNumber
                local jchild, jdetacher
                jchild_n = 0
                jdetacher_n = 0
                for (j, e) in enumerate(phi.edges)
                    if e == ichild
                        jchild = j
                        jchild_n += 1
                        continue
                    end
                    term = ir.stmts.inst[ir.cfg.blocks[e].stmts[end]]
                    if term isa DetachNode && term.syncregion == reattach.syncregion
                        idetacher = e
                        jdetacher = j
                        jdetacher_n += 1
                    end
                end
                @assert jchild_n == 1
                @assert jdetacher_n == 1
                use_memory = true
                # The detach always happens (and the child task will set the
                # task output) if the continuation is dominated by the block
                # detaching the task:
                if dominates(domtree, idetacher, reattach.label)
                    # Next, check if the variable set in the child task is not
                    # used in the continuation.
                    used_in_continuation = RefValue(false)
                    foreach_descendant(reattach.label, ir.cfg) do _, bb
                        for i in bb.stmts
                            inst = ir.stmts.inst[i]
                            if inst isa SyncNode
                                inst.syncregion == reattach.syncregion && return false
                            end
                            foreach_id(identity, inst) do v
                                if v === SSAValue(iphi)
                                    used_in_continuation.x = true
                                end
                            end
                        end
                        return true
                    end
                    use_memory = used_in_continuation.x
                end
                @assert !any_assigned(phi.values) do v
                    v isa SSAValue && (isassigned(ssamap, v.id) || v.id in toref)
                end
                if use_memory
                    push!(toref, iphi)
                    users = usersof[iphi] = BitSet()
                    foreach_descendant(reattach.label, ir.cfg) do _, bb
                        for i in bb.stmts
                            inst = ir.stmts.inst[i]
                            if inst isa PhiNode
                                if isusedby(iphi, i)
                                    push!(users, i)
                                end
                            end
                        end
                        return true
                    end
                else
                    @assert !isassigned(ssamap, iphi)
                    ssamap[iphi] = phi.values[jchild]
                end
            end
        end
    end

    for i in 1:length(ir.stmts)
        stmt = ir.stmts[i]
        stmt[:inst] = map_id(identity, stmt[:inst]) do v
            if v isa SSAValue && isassigned(ssamap, v.id)
                return ssamap[v.id]
            end
            v
        end
    end
    isempty(toref) && return ir

    # Here, we have task outputs for which the tasks that will set them cannot
    # be statically determined. We support this by turning them to references
    # ("reg2mem"). Note that we still rely on that there is only one task that
    # sets each taks output (thus, no atomic read/write is nessesary).

    # [Identifying task output variables]
    # Partition `toref` into the set of phi nodes used by at least one other
    # shared phi node (i.e., the pairwise intersection of the sets of user phi
    # nodes is non-empty). Each set (partition) corresponds to a single output
    # variable. Since we only require pairwise intersection to be non-empty, we
    # need to take the transitive closure to obtain an equivalence
    # relationship:
    same_variable = transitive_closure_on(toref) do v, w
        havecommon(usersof[v], usersof[w])
    end
    # TODO: `transitive_closure_on` is O(n^2) (n = length(toref)). Is there a better way?
    output_variables = BitSet[]
    toref_next = BitSet()
    while !isempty(toref)
        v = pop!(toref)
        phis = BitSet((v,))
        push!(output_variables, phis)
        while !isempty(toref)
            w = pop!(toref)
            if same_variable(v, w)
                push!(phis, w)
            else
                push!(toref_next, w)
            end
        end
        toref_next, toref = toref, toref_next
    end
    phisets = BitSet[
        let set = BitSet(phis)
            for v in phis
                union!(set, usersof[v])
            end
            set
        end for phis in output_variables
    ]

    # Insert stores
    refs = SSAValue[]
    sizehint!(refs, length(phisets))
    for set in phisets
        TX = foldunion(ir.stmts.type[i] for i in set)
        if isconcretetype(TX)
            T = Tapir.UndefableRef{TX}
        else
            T = Tapir.UndefableRef{<:TX}
        end
        r = insert_node!(ir, 1, T, Expr(:call, Tapir.noinline_box, T))
        push!(refs, r)
        handled = falses(length(ir.stmts))
        is_inital_set::Bool = false
        local initial_value
        for i in set
            phi = ir.stmts.inst[i]::PhiNode
            foreach_def(SSAValue(i), ir) do v
                if v isa Instruction
                    handled[v.idx] && return false
                    handled[v.idx] = true
                    if !(v[:inst] isa PhiNode)
                        local x = SSAValue(v.idx)
                        local at = v.idx
                        insert_node!(ir, at, Any, Expr(:call, Base.setindex!, r, x), true)
                        return false
                    end
                    return true  # keep following the chain
                end
                if is_inital_set
                    @assert initial_value === v
                else
                    initial_value = v
                    is_inital_set = true
                end
                # `v` is either an `Argument` or a constant
                insert_node!(ir, 1, Any, Expr(:call, Base.setindex!, r, v))
                return false
            end
            foreach_assigned_pair(phi.values) do jjjjj, v
                if v isa SSAValue
                    local inst = ir.stmts.inst[v.id]
                elseif v isa Argument
                end
            end
        end
    end
    # Insert loads
    for i in 1:length(ir.stmts)
        stmt = ir.stmts[i]
        stmt[:inst] isa PhiNode && continue
        # TODO: support `@isdefined`
        stmt[:inst] = map_id(identity, stmt[:inst]) do v
            if v isa SSAValue
                for (set, r) in zip(phisets, refs)
                    if v.id in set
                        TX = foldunion(ir.stmts.type[i] for i in set)
                        load = insert_node!(ir, i, TX, Expr(:call, Base.getindex, r))
                        return load
                    end
                end
            end
            v
        end
    end

    return ir
end

"""
    outline_child_task(ir::IRCode, task::ChildTask) -> taskir::IRCode
"""
function outline_child_task(ir::IRCode, task::ChildTask)
    Base = Main.Base::Module

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
            offset += 1
        end
    end
    @assert offset == nargs + ncaps + 2 * nouts
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
    resize!(stmts, nargs + ncaps + 2 * nouts + length(locals))
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
        stmts.type[inew] = Base.RefValue{widenconst(ir.stmts[iold][:type])}
        # ASK: ditto
    end
    for iold in defs
        inew = ssachangemap[iold]
        stmts[inew] = ir.stmts[iold]
        stmts.inst[inew] = map_id(on_value, on_label, stmts.inst[inew])
    end
    for iold in outside_uses
        ival = outvaluemap[iold]
        stmts.inst[ival + 1] = Expr(
            :call,
            setfield!,
            SSAValue(outrefmap[iold]),  # ::RefValue
            QuoteNode(:x),              # name
            SSAValue(ival),             # value
        )
        stmts.type[ival + 1] = Any
    end

    # Turn reattach nodes into return nodes (otherwise, they introduce edges to
    # invalid blocks and also the IR does not contain returns).
    for i in task.reattaches
        @assert stmts.inst[ssachangemap[i]] isa ReattachNode
        stmts.inst[ssachangemap[i]] = ReturnNode(nothing)
    end

    # Fill in undefined return values
    for (i, inst) in pairs(stmts.inst)
        if inst isa ReturnNode && !isdefined(inst, :val)
            stmts.inst[i] = ReturnNode(nothing)
        end
    end
    # ASK/TODO: Is this a reasonable code or am I just hiding a bug?

    blocks = map(enumerate(task.blocks)) do (i, ibb)
        isentry = i == 1
        bb = ir.cfg.blocks[ibb]
        start = ssachangemap[bb.stmts[begin]]
        stop = ssachangemap[bb.stmts[end]]
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
    types = Any[]
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

    return taskir, captured_variables, outputs
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

function lower_tapir!(ir::IRCode)
    Tapir = tapir_module()
    Tapir isa Module || return ir
    Base = Main.Base::Module

    tasks = child_tasks(ir)
    isempty(tasks) && return ir

    taskgroups = IdDict{Int,Any}()
    tobeloaded = IdDict{Int,Vector{Tuple{Int,Type,SSAValue}}}()
    for task in tasks
        det = ir.stmts.inst[task.detach]::DetachNode
        # ASK: Can we always assume `syncregion` is an SSAValue?
        syncregion = (det.syncregion::SSAValue).id
        tg = get!(taskgroups, syncregion) do
            insert_node!(ir, syncregion, Tapir.TaskGroup, Expr(:call, Tapir.taskgroup), true)
        end

        taskir, arguments, outputs = outline_child_task(ir, task)
        taskir = lower_tapir!(taskir)
        meth = opaque_closure_method_from_ssair(taskir)
        if !isempty(outputs)
            tbl = get!(() -> Tuple{Int,Type,SSAValue}[], tobeloaded, syncregion)
        end
        for (T, iout) in outputs
            R = Base.RefValue{T}
            ref = insert_node!(ir, task.detach, R, Expr(:new, R))
            push!(arguments, ref)
            push!(tbl, (iout, T, ref))
        end
        oc_inst = Expr(:new_opaque_closure, Tuple{}, false, Union{}, Any, meth, arguments...)
        oc = insert_node!(ir, task.detach, Any, oc_inst)
        sp = insert_node!(ir, task.detach, Any, Expr(:call, Tapir.spawn!, tg, oc))
        ir.stmts.inst[task.detach] = GotoNode(det.reattach)
        cfg_delete_edge!(ir.cfg, block_for_inst(ir.cfg, task.detach), det.label)
        # i.e., transform
        #     %syncregion = Expr(:syncregion)
        #     ...
        #     detach within %syncregion, #child, #continuation
        # to
        #     %tg = Tapir.taskgroup()
        #     ...
        #     %oc = new_opaque_closure(%meth, capture...)
        #     Tapir.spawn!(%tg, %oc)
        #     goto #continuation
        # and then remove the detach edge from this BB to #child.
        #
        # ASK: Is this a valid way to transform the CFG in Julia?
    end
    # TODO: handle `@sync @spawn @spawn ...` (currently, we are assuming that the
    # syncregion is not used in nested tasks)

    original_outputs = BitSet()
    output_loader = zeros(Int, length(ir.stmts))
    for i in 1:length(ir.stmts)
        inst = ir.stmts[i][:inst]
        if inst isa SyncNode
            syncregion = (inst.syncregion::SSAValue).id
            tg = get(taskgroups, syncregion, nothing)
            if tg !== nothing
                insert_node!(ir, i, Any, Expr(:call, Tapir.sync!, tg::SSAValue))
                next_bb = block_for_inst(ir.cfg, i + 1)
                ir.stmts.inst[i] = GotoNode(next_bb)
                # i.e., transform
                #     %syncregion = Expr(:syncregion)
                #     ...
                #     sync within %syncregion
                # to
                #     %tg = Tapir.taskgroup()
                #     ...
                #     Tapir.sync!(%tg)
                #     goto #next_bb
                #
                # Note: using the "noop" goto node to avoid changing the CFG

                tbl = get(tobeloaded, syncregion, nothing)
                if tbl !== nothing
                    bb = ir.cfg.blocks[block_for_inst(ir.cfg, i)]
                    for (iout, T, ref) in tbl
                        ex = Expr(:call, getfield, ref, QuoteNode(:x))
                        load = insert_node!(ir, i, T, ex)
                        @assert load.id > 0
                        output_loader[iout] = load.id
                        push!(original_outputs, iout)
                    end
                end
            end
        end
    end

    for i in 1:length(ir.stmts)
        stmt = ir.stmts[i]
        replaced = RefValue(false)
        user = map_id(identity, stmt[:inst]) do v
            if v isa SSAValue
                if v.id in original_outputs
                    replaced.x = true
                    return SSAValue(output_loader[v.id])
                end
            end
            return v
        end
        if replaced.x
            stmt[:inst] = user
        end
    end

    for i in ir.cfg.blocks[1].stmts
        stmt = ir.stmts[i]
        inst = stmt[:inst]
        isexpr(inst, :invoke) && inst.args[2] === Tapir.noinline_box || continue
        stmt[:inst] = Expr(:call, Tapir.box, inst.args[3])
    end

    # Finalize the changes in the IR (clears the node inserted to `ir.new_nodes`):
    ir = compact!(ir, true)
    if JLOptions().debug_level == 2
        verify_ir(ir)
        verify_linetable(ir.linetable)
    end
    # Remove the dead code in the detached sub-CFG (child tasks):
    ir = cfg_simplify!(ir)
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

Create an oaque `closure` from an SSA `ir`.
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
    return ccall(
        :jl_make_opaque_closure_method,
        Ref{Method},
        (Any,Any,Any,Any),
        mod, nargs - 1, functionloc, ci,
    )
end

is_sequential(src::CodeInfo) = all(x -> !(x isa DetachNode), src.code)

function lower_tapir(interp::AbstractInterpreter, linfo::MethodInstance, ci::CodeInfo)
    ccall(:jl_breakpoint, Cvoid, (Any,), ci)
    is_sequential(ci) && return remove_tapir(ci)

    # Ref: _typeinf(interp::AbstractInterpreter, frame::InferenceState)
    params = OptimizationParams(interp)
    opt = OptimizationState(linfo, copy(ci), params, interp)
    nargs = Int(opt.nargs) - 1 # Ref: optimize(interp, opt, params)

    # Ref: run_passes
    preserve_coverage = coverage_enabled(opt.mod)
    ir = convert_to_ircode(ci, copy_exprargs(ci.code), preserve_coverage, nargs, opt)
    ir = slot2reg(ir, ci, nargs, opt)
    @timeit "tapir" ir = lower_tapir!(ir)
    if JLOptions().debug_level == 2
        @timeit "verify tapir" (verify_ir(ir); verify_linetable(ir.linetable))
    end

    finish(opt, params, ir, Any) # Ref: optimize(interp, opt, params)
    finish(opt.src, interp) # Ref: _typeinf(interp, frame)

    return remove_tapir!(opt.src)
end

lower_tapir(linfo::MethodInstance, ci::CodeInfo) =
    lower_tapir(NativeInterpreter(), linfo, ci)

"""
    remove_tapir!(src::CodeInfo)
    remove_tapir!(_::Any)

Remove Tapir instructions from `src` (if any). This transformation is always valid
due to the (assumed) serial projection property of the source program.
"""
function remove_tapir!(src::CodeInfo)
    for (i, x) in pairs(src.code)
        if x isa Union{DetachNode,ReattachNode,SyncNode}
            src.code[i] = nothing
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
