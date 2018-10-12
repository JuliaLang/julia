# This file is a part of Julia. License is MIT: https://julialang.org/license

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

function naive_idoms(cfg::CFG)
    nblocks = length(cfg.blocks)
    # The extra +1 helps us detect unreachable blocks below
    dom_all = BitSet(1:nblocks+1)
    dominators = BitSet[n == 1 ? BitSet(1) : copy(dom_all) for n = 1:nblocks]
    changed = true
    while changed
        changed = false
        for n = 2:nblocks
            if isempty(cfg.blocks[n].preds)
                continue
            end
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
        if dominators[i] == dom_all
            idoms[i] = 0
            continue
        end
        doms = collect(dominators[i])
        for dom in doms
            i == dom && continue
            hasany = false
            for p in doms
                if p !== i && p !== dom && dom in dominators[p]
                    hasany = true; break
                end
            end
            hasany && continue
            idoms[i] = dom
        end
    end
    idoms
end

# Construct Dom Tree
function construct_domtree(cfg::CFG)
    idoms = SNCA(cfg)
    # Compute children
    nblocks = length(cfg.blocks)
    domtree = DomTreeNode[DomTreeNode() for _ = 1:nblocks]
    for (idx, idom) in Iterators.enumerate(idoms)
        (idx == 1 || idom == 0) && continue
        push!(domtree[idom].children, idx)
    end
    # Recursively set level
    update_level!(domtree, 1, 1)
    DomTree(idoms, domtree)
end

#================================ [SNCA] ======================================#
#
#   This section implements the Semi-NCA (SNCA) dominator tree construction from
#   described in Georgiadis' PhD thesis [LG05], which itself is a simplification
#   of the Simple Lenguare-Tarjan (SLT) algorithm [LG79]. This algorithm matches
#   the algorithm choice in LLVM and seems to be a sweet spot in implementation
#   simplicity and efficiency.
#
#   [LG05]  Linear-Time Algorithms for Dominators and Related Problems
#           Loukas Georgiadis, Princeton University, November 2005, pp. 21-23:
#           ftp://ftp.cs.princeton.edu/reports/2005/737.pdf
#
#   [LT79]  A fast algorithm for finding dominators in a flowgraph
#           Thomas Lengauer, Robert Endre Tarjan, July 1979, ACM TOPLAS 1-1
#           http://www.dtic.mil/dtic/tr/fulltext/u2/a054144.pdf
#
begin
    # We could make these real structs, but probably not worth the extra
    # overhead. Still, give them names for documentary purposes.
    const BBNumber = UInt
    const DFSNumber = UInt

    """
    Keeps the per-BB state of the Semi NCA algorithm. In the original
    formulation, there are three separate length `n` arrays, `label`, `semi` and
    `ancestor`. Instead, for efficiency, we use one array in a array-of-structs
    style setup.
    """
    struct Node
        semi::DFSNumber
        label::DFSNumber
    end

    struct DFSTree
        # Maps DFS number to BB number
        numbering::Vector{BBNumber}
        # Maps BB number to DFS number
        reverse::Vector{DFSNumber}
        # Records parent relationships in the DFS tree (DFS number -> DFS number)
        # Storing it this way saves a few lookups in the snca_compress! algorithm
        parents::Vector{DFSNumber}
    end
    length(D::DFSTree) = length(D.numbering)
    preorder(D::DFSTree) = OneTo(length(D))
    _drop(xs::AbstractUnitRange, n::Integer) = (first(xs)+n):last(xs)

    function DFSTree(nblocks::Int)
        DFSTree(
            Vector{BBNumber}(undef, nblocks),
            zeros(DFSNumber, nblocks),
            Vector{DFSNumber}(undef, nblocks))
    end

    function DFS(cfg::CFG, current_node::BBNumber)::DFSTree
        dfs = DFSTree(length(cfg.blocks))
        # TODO: We could reuse the storage in DFSTree for our worklist. We're
        # guaranteed for the worklist to be smaller than the remaining space in
        # DFSTree
        worklist = Tuple{DFSNumber, BBNumber}[(0, current_node)]
        dfs_num = 1
        parent = 0
        while !isempty(worklist)
            (parent, current_node) = pop!(worklist)
            dfs.reverse[current_node] = dfs_num
            dfs.numbering[dfs_num] = current_node
            dfs.parents[dfs_num] = parent
            for succ in cfg.blocks[current_node].succs
                dfs.reverse[succ] != 0 && continue
                # Mark things that are currently in the worklist
                dfs.reverse[succ] = 1
                push!(worklist, (dfs_num, succ))
            end
            dfs_num += 1
        end
        # If all blocks are reachable, this is a no-op, otherwise,
        # we shrink these arrays.
        resize!(dfs.numbering, dfs_num - 1)
        resize!(dfs.parents, dfs_num - 1)
        dfs
    end

    """
    Matches the snca_compress algorithm in Figure 2.8 of [LG05], with the
    modification suggested in the paper to use `last_linked` to determine
    whether an ancestor has been processed rather than storing `0` in the
    ancestor array.
    """
    function snca_compress!(state::Vector{Node}, ancestors::Vector{DFSNumber},
                            v::DFSNumber, last_linked::DFSNumber)
        u = ancestors[v]
        @assert u < v
        if u >= last_linked
            snca_compress!(state, ancestors, u, last_linked)
            if state[u].label < state[v].label
                state[v] = Node(state[v].semi, state[u].label)
            end
            ancestors[v] = ancestors[u]
        end
        nothing
    end

    """
    The main Semi-NCA algrithm. Matches Figure 2.8 in [LG05].
    Note that the pseudocode in [LG05] is not entirely accurate.
    The best way to understand what's happening is to read [LT79], then the
    description of SLT in in [LG05] (warning: inconsistent notation), then
    the description of Semi-NCA.
    """
    function SNCA(cfg::CFG)
        D = DFS(cfg, BBNumber(1))
        # `label` is initialized to the identity mapping (though
        # the paper doesn't make that clear). The rational for this is Lemma
        # 2.4 in [LG05] (i.e. Theorem 4 in ). Note however, that we don't
        # ever look at `semi` until it is fully initialized, so we could leave
        # it unitialized here if we wanted to.
        state = Node[ Node(typemax(DFSNumber), w) for w in preorder(D) ]
        # Initialize idoms to parents. Note that while idoms are eventually
        # BB indexed, we keep it DFS indexed until a final post-processing
        # pass to avoid extra memory references during the O(N^2) phase below.
        idoms_dfs = copy(D.parents)
        # We abuse the parents array as the ancestors array.
        # Semi-NCA does not look at the parents array at all.
        # SLT would, but never simultaneously, so we could still
        # do this.
        ancestors = D.parents
        for w ∈ reverse(_drop(preorder(D), 1))
            # LLVM initializes this to the parent, the paper initializes this to
            # `w`, but it doesn't really matter (the parent is a predecessor,
            # so at worst we'll discover it below). Save a memory reference here.
            semi_w = typemax(DFSNumber)
            for v ∈ cfg.blocks[D.numbering[w]].preds
                # For the purpose of the domtree, ignore virtual predecessors
                # into catch blocks.
                v == 0 && continue
                vdfs = D.reverse[v]
                # Ignore unreachable predecessors
                vdfs == 0 && continue
                last_linked = DFSNumber(w + 1)
                # N.B.: This conditional is missing from the psuedocode
                # in figure 2.8 of [LG05]. It corresponds to the
                # `ancestor[v] != 0` check in the `eval` implementation in
                # figure 2.6
                if vdfs >= last_linked
                    snca_compress!(state, ancestors, vdfs, last_linked)
                end
                semi_w = min(semi_w, state[vdfs].label)
            end
            state[w] = Node(semi_w, semi_w)
        end
        for v ∈ _drop(preorder(D), 1)
            idom = idoms_dfs[v]
            vsemi = state[v].semi
            while idom > vsemi
                idom = idoms_dfs[idom]
            end
            idoms_dfs[v] = idom
        end
        idoms_bb = Int[ (i == 1 || D.reverse[i] == 0) ? 0 : D.numbering[idoms_dfs[D.reverse[i]]] for i = 1:length(cfg.blocks) ]
        idoms_bb
    end
end
