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
function dominates(domtree::DomTree, bb1::Int, bb2::Int)
    bb1 == bb2 && return true
    target_level = domtree.nodes[bb1].level
    source_level = domtree.nodes[bb2].level
    source_level < target_level && return false
    for _ in (source_level - 1):-1:target_level
        bb2 = domtree.idoms[bb2]
    end
    return bb1 == bb2
end

bb_unreachable(domtree::DomTree, bb::Int) = bb != 1 && domtree.nodes[bb].level == 1

function update_level!(domtree::Vector{DomTreeNode}, node::Int, level::Int)
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

function iterate(doms::DominatedBlocks, state::Nothing=nothing)
    isempty(doms.worklist) && return nothing
    bb = pop!(doms.worklist)
    for dominated in doms.domtree.nodes[bb].children
        push!(doms.worklist, dominated)
    end
    return (bb, nothing)
end

# Construct Dom Tree
# Simple algorithm - TODO: Switch to the fast version (e.g. https://tanujkhattar.wordpress.com/2016/01/11/dominator-tree-of-a-directed-graph/)
function construct_domtree(cfg::CFG)
    nblocks = length(cfg.blocks)
    dom_all = BitSet(1:nblocks)
    dominators = BitSet[n == 1 ? BitSet(1) : copy(dom_all) for n = 1:nblocks]
    changed = true
    while changed
        changed = false
        for n = 2:nblocks
            isempty(cfg.blocks[n].preds) && continue
            firstp, rest = Iterators.peel(Iterators.filter(p->p != 0, cfg.blocks[n].preds))
            new_doms = copy(dominators[firstp])
            for p in rest
                intersect!(new_doms, dominators[p])
            end
            push!(new_doms, n)
            changed = changed || (new_doms != dominators[n])
            dominators[n] = new_doms
        end
    end
    # Compute idoms
    idoms = fill(0, nblocks)
    for i = 2:nblocks
        doms = collect(dominators[i])
        for dom in doms
            i == dom && continue
            let i = i, dom = dom
                any(p -> (p !== i && p !== dom && dom in dominators[p]), doms) && continue
            end
            idoms[i] = dom
        end
    end
    # Compute children
    domtree = DomTreeNode[DomTreeNode() for _ = 1:nblocks]
    for (idx, idom) in Iterators.enumerate(idoms)
        (idx == 1 || idom == 0) && continue
        push!(domtree[idom].children, idx)
    end
    # Recursively set level
    update_level!(domtree, 1, 1)
    DomTree(idoms, domtree)
end
