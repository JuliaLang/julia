# This file is a part of Julia. License is MIT: https://julialang.org/license

using .Compiler: DomTree, CFG, BasicBlock, StmtRange, dominates

struct SCCStackItem
    v::Int32
    # which child of `v` to scan
    child::Int32
    # the location of `parent` in the stack
    parent::Int32
    # the index in the (pre-order traversal of the) DFS tree
    preorder::Int32
    # the minimum node (by pre-order index) reachable from any node in the DFS sub-tree rooted at `v`
    minpreorder::Int32
    # whether this node is reachable from BasicBlock #1
    live::Bool
end

function SCCStackItem(item::SCCStackItem; child=item.child,
                      minpreorder=item.minpreorder, live=item.live)
    return SCCStackItem(
        item.v,        # v
        child,         # child
        item.parent,   # parent
        item.preorder, # preorder
        minpreorder,   # minpreorder
        live,          # live
    )
end

struct CFGReachability
    irreducible::BitVector # BBNumber -> Bool
    scc::Vector{Int}       # BBNumber -> SCCNumber
    domtree::DomTree

    _worklist::Vector{Int}       # for node removal
    _stack::Vector{SCCStackItem} # for Tarjan's SCC algorithm
end

function CFGReachability(cfg::CFG, domtree::DomTree)
    n_blocks = length(cfg.blocks)
    reachability = CFGReachability(
        BitVector(undef, n_blocks), # irreducible
        zeros(Int, n_blocks),       # scc
        domtree,                    # domtree
        Int[],                      # _worklist
        SCCStackItem[],             # _stack
    )
    tarjan!(reachability, cfg;
        # reducible back-edges don't need to be considered for reachability
        filter = (from::Int,to::Int)->!dominates(domtree, to, from)
    )
    return reachability
end

bb_unreachable(reach::CFGReachability, bb::Int) = reach.scc[bb] == 0

bb_in_irreducible_loop(reach::CFGReachability, bb::Int) = reach.irreducible[bb]

# Returns `true` if a node is 'rooted' as reachable, i.e. it is has an incoming
# edge from a resolved SCC other than its own (or it is BasicBlock #1).
#
# `tarjan!` takes the transitive closure of this relation in order to detect
# which BasicBlocks are unreachable.
function _bb_externally_reachable(reach::CFGReachability, cfg::CFG, bb::Int; filter)
    (; scc) = reach
    bb == 1 && return true
    for pred in cfg.blocks[bb].preds
        scc[pred] <= 0 && continue
        !filter(pred, bb) && continue
        @assert scc[pred] != scc[bb]
        return true
    end
    return false
end

"""
    tarjan!(reach::CFGReachability, cfg::CFG, root::Int=1)

Tarjan's strongly-connected components algorithm. Traverses the CFG starting at `root`, ignoring
nodes with resolved SCC's and updating outputs for all un-resolved nodes.

Returns true if any node was discovered to be unreachable, false otherwise.

Outputs:
  - `reach.scc`: strongly-connected components, ignoring backedges to (natural) loops
  - `reach.irreducible`: true iff a BasicBlock is part of a (non-trivial) SCC / irreducible loop
  - `reach._worklist`: if performing an incremental update (`root != 1`), any traversed nodes that
    are unreachable from BasicBlock #1 are enqueued to this worklist
"""
function tarjan!(reach::CFGReachability, cfg::CFG; root::Int=1,
    filter = (from::Int,to::Int)->true,
)
    (; scc, irreducible) = reach
    scc[root] != 0 && return scc
    live = _bb_externally_reachable(reach, cfg, root; filter)

    # the original algorithm has a separate stack and worklist (unrelated to `reach._worklist`)
    # here we use a single combined stack for improved memory/cache efficiency
    stack = reach._stack
    push!(stack, SCCStackItem(
        root, # v
        1,    # child
        0,    # parent
        1,    # preorder
        1,    # minpreorder
        live, # live
    ))
    scc[root] = -1
    cursor = length(stack)

    # worklist length before any new unreachable nodes are added
    worklist_len = length(reach._worklist)

    # last (pre-order) DFS label assigned to a node
    preorder_id = 1
    while true
        (; v, child, minpreorder, live) = item = stack[cursor]

        bb = cfg.blocks[v]
        if child <= length(bb.succs) # queue next child
            stack[cursor] = item = SCCStackItem(item; child=child+1)
            succ = bb.succs[child]

            # ignore any edges that don't pass the filter
            !filter(convert(Int, v), succ) && continue

            if scc[succ] < 0
                # next child is already in DFS tree
                child_preorder = stack[-scc[succ]].preorder

                # only need to update `minpreorder` for `v`
                stack[cursor] = item = SCCStackItem(item;
                                                    minpreorder=min(minpreorder, child_preorder))
            elseif scc[succ] == 0
                # next child is a new element in DFS tree
                preorder_id += 1
                live = live || _bb_externally_reachable(reach, cfg, succ; filter)
                push!(stack, SCCStackItem(
                    succ,        # v
                    1,           # child
                    cursor,      # parent (index in stack)
                    preorder_id, # preorder
                    preorder_id, # minpreorder
                    live,        # live
                ))
                scc[succ] = -length(stack)
                cursor = length(stack)
            else end # next child is a resolved SCC (do nothing)
        else # v's children are processed, finalize v
            if item.minpreorder == item.preorder
                has_one_element = stack[end].v == v
                while true
                    item = pop!(stack)
                    if live
                        scc[item.v] = v
                        scan_subgraph!(reach, cfg, convert(Int, item.v),
                            #= filter =# (pred,x)->(filter(pred, x) && scc[x] > typemax(Int)÷2),
                            #= action =# (x)->(scc[x] -= typemax(Int)÷2;),
                        )
                    else # this offset marks a node as 'maybe-dead'
                        scc[item.v] = v + typemax(Int)÷2
                        push!(reach._worklist, item.v)
                    end
                    irreducible[item.v] = !has_one_element
                    (item.v == v) && break
                end
                item.parent == 0 && break # all done
            elseif live
                stack[item.parent] = SCCStackItem(stack[item.parent]; live=true)
            end

            # update `minpreorder` for parent
            parent = stack[item.parent]
            minpreorder = min(parent.minpreorder, item.minpreorder)
            stack[item.parent] = SCCStackItem(parent; minpreorder)

            cursor = item.parent
        end
    end

    worklist = reach._worklist

    # filter the worklist, leaving any nodes not proven to be reachable from BB #1
    n_popped = 0
    for i = (worklist_len + 1):length(worklist)
        @assert worklist[i] != 1
        @assert scc[worklist[i]] > 0
        if scc[worklist[i]] > typemax(Int)÷2
            # node is unreachable, enqueue it
            scc[worklist[i]] = 0
            worklist[i - n_popped] = worklist[i]
        else
            n_popped += 1
        end
    end
    resize!(worklist, length(worklist) - n_popped)

    return length(worklist) > worklist_len # if true, a (newly) unreachable node was enqueued
end

"""
Scan the subtree rooted at `root`, excluding `root` itself

Note: This function will not detect cycles for you. The `filter` provided must
      protect against infinite cycle traversal.
"""
function scan_subgraph!(reach::CFGReachability, cfg::CFG, root::Int, filter, action)
    worklist = reach._worklist
    start_len = length(worklist)

    push!(worklist, root)
    while length(worklist) > start_len
        v = pop!(worklist)
        for succ in cfg.blocks[v].succs
            !filter(v, succ) && continue
            action(succ)
            push!(worklist, succ)
        end
    end
end

function enqueue_if_unreachable!(reach::CFGReachability, cfg::CFG, bb::Int)
    (; domtree, scc) = reach
    @assert scc[bb] != 0

    bb == 1 && return false
    if bb_in_irreducible_loop(reach, bb)
        # irreducible CFG
        # this requires a full scan of the irreducible loop

        # any reducible back-edges do not need to be considered as part of reachability
        # (very important optimization, since it means reducible CFGs will have no SCCs)
        filter = (from::Int, to::Int)->!dominates(domtree, to, from)

        scc′ = scc[bb]
        scc[bb] = 0
        scan_subgraph!(reach, cfg, bb, # set this SCC to 0
            #= filter =# (pred,x)->(filter(pred, x) && scc[x] == scc′),
            #= action =# (x)->(scc[x] = 0;),
        )

        # re-compute the SCC's for this portion of the CFG, adding any freshly
        # unreachable nodes to `reach._worklist`
        return tarjan!(reach, cfg; root=bb, filter)
    else
        # target is a reducible CFG node
        # this node lives iff it still has an incoming forward edge
        for pred in cfg.blocks[bb].preds
            # virtual edge does not count - if the enter is dead, that edge is
            # not taken.
            pred == 0 && continue
            !dominates(domtree, bb, pred) && return false # forward-edge
        end
        scc[bb] = 0
        push!(reach._worklist, bb)
        return true
    end
end

function kill_cfg_edge!(cfg::CFG, from::Int, to::Int)
    preds, succs = cfg.blocks[to].preds, cfg.blocks[from].succs
    deleteat!(preds, findfirst(x::Int->x==from, preds)::Int)
    deleteat!(succs, findfirst(x::Int->x==to, succs)::Int)
    return nothing
end

"""
Remove from `cfg` and `reach` the edge (from → to), as well as any blocks/edges
this causes to become unreachable.

Calls:
  - `block_callback` for every unreachable block.
  - `edge_callback` for every unreachable edge into a reachable block (may also
     be called for blocks which are later discovered to be unreachable).
"""
function kill_edge!(reach::CFGReachability, cfg::CFG, from::Int, to::Int,
                    edge_callback=nothing, block_callback=nothing)
    (reach.scc[from] == 0) && return # source is already unreachable
    @assert reach.scc[to] != 0

    # delete (from → to) edge
    kill_cfg_edge!(cfg, from, to)

    # check for unreachable target
    enqueued = enqueue_if_unreachable!(reach, cfg, to)
    if !enqueued && edge_callback !== nothing
        edge_callback(from, to)
    end
    while !isempty(reach._worklist)
        node = convert(Int, pop!(reach._worklist))

        # already marked unreachable, just need to notify
        @assert reach.scc[node] == 0 && node != 1
        if block_callback !== nothing
            block_callback(node)
        end

        for succ in cfg.blocks[node].succs
            # delete (node → succ) edge
            preds = cfg.blocks[succ].preds
            deleteat!(preds, findfirst(x::Int->x==node, preds)::Int)

            # check for newly unreachable target
            reach.scc[succ] == 0 && continue
            enqueued = enqueue_if_unreachable!(reach, cfg, succ)
            if !enqueued && edge_callback !== nothing
                edge_callback(node, succ)
            end
        end
        empty!(cfg.blocks[node].succs)
    end
end
