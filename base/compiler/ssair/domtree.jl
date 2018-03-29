struct DomTreeNode
    level::Int
    children::Vector{Int}
end
DomTreeNode() = DomTreeNode(1, Vector{Int}())

struct DomTree
    idoms::Vector{Int}
    nodes::Vector{DomTreeNode}
end

"""
    Checks if bb1 dominates bb2
"""
function dominates(domtree, bb1, bb2)
    bb1 == bb2 && return true
    target_level = domtree.nodes[bb1].level
    source_level = domtree.nodes[bb2].level
    source_level < target_level && return false
    for _ in (source_level-1):-1:target_level
        bb2 = domtree.idoms[bb2]
    end
    return bb1 == bb2
end

function update_level!(domtree, node, level)
    domtree[node] = DomTreeNode(level, domtree[node].children)
    foreach(domtree[node].children) do child
        update_level!(domtree, child, level+1)
    end
end

struct DominatedBlocks
    domtree::DomTree
    worklist::Vector{Int}
end

function dominated(domtree::DomTree, root::Int)
    doms = DominatedBlocks(domtree, Vector{Int}())
    push!(doms.worklist, root)
    doms
end

function start(doms::DominatedBlocks)
    nothing
end

function next(doms::DominatedBlocks, state::Nothing)
    bb = pop!(doms.worklist)
    for dominated in doms.domtree.nodes[bb].children
        push!(doms.worklist, dominated)
    end
    (bb, nothing)
end

function done(doms::DominatedBlocks, state::Nothing)
    isempty(doms.worklist)
end

# Construct Dom Tree
# Simple algorithm - TODO: Switch to the fast version (e.g. https://tanujkhattar.wordpress.com/2016/01/11/dominator-tree-of-a-directed-graph/)
function construct_domtree(cfg)
    dominators = IdSet{Int}[n == 1 ? IdSet{Int}(n) : IdSet{Int}(1:length(cfg.blocks)) for n = 1:length(cfg.blocks)]
    changed = true
    while changed
        changed = false
        for n = 2:length(cfg.blocks)
            isempty(cfg.blocks[n].preds) && continue
            firstp, rest = Iterators.peel(cfg.blocks[n].preds)
            new_doms = copy(dominators[firstp])
            for p in rest
                intersect!(new_doms, dominators[p])
            end
            push!(new_doms, n)
            changed |= (new_doms != dominators[n])
            dominators[n] = new_doms
        end
    end
    # Compute idoms
    idoms = fill(0, length(cfg.blocks))
    for i = 2:length(cfg.blocks)
        for dom in dominators[i]
            i == dom && continue
            any(p->p !== i && p !== dom && dom in dominators[p], dominators[i]) && continue
            idoms[i] = dom
        end
    end
    # Compute children
    domtree = DomTreeNode[DomTreeNode() for _ = 1:length(cfg.blocks)]
    for (idx, idom) in Iterators.enumerate(idoms)
        (idx == 1 || idom == 0) && continue
        push!(domtree[idom].children, idx)
    end
    # Recursively set level
    update_level!(domtree, 1, 1)
    DomTree(idoms, domtree)
end