# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

include("setup_Compiler.jl")
include("irutils.jl")

using .Compiler: CFGReachability, DomTree, CFG, BasicBlock, StmtRange, dominates,
                     bb_unreachable, kill_edge!

function reachable(g::CFG, a::Int, b::Int; domtree=nothing)
    visited = BitVector(false for _ = 1:length(g.blocks))
    worklist = Int[a]
    while !isempty(worklist)
        node = pop!(worklist)
        node == b && return true
        visited[node] = true
        for child in g.blocks[node].succs
            if domtree !== nothing && dominates(domtree, child, node)
                continue # if provided `domtree`, ignore back-edges
            end

            !visited[child] && push!(worklist, child)
        end
    end
    return false
end

function rand_cfg(V, E)
    bbs = [BasicBlock(StmtRange(0,0), Int[], Int[]) for _ = 1:V]

    reachable = BitVector(false for _ = 1:V)
    reachable[1] = true

    targets = BitVector(false for _ = 1:V)

    for _ = 1:E
        # Pick any source (with at least 1 missing edge)
        source, dest = 0, 0
        while true
            source = rand(findall(reachable))
            for v = 1:V
                targets[v] = !in(v, bbs[source].succs)
            end
            any(targets) && break
        end

        # Pick any new target for source
        dest = rand(findall(targets))

        # Add edge to graph
        push!(bbs[source].succs, dest)
        push!(bbs[dest].preds, source)

        reachable[dest] = true
    end

    return CFG(bbs, zeros(Int, V + 1))
end

function get_random_edge(cfg::CFG, V)
    has_edge = [length(cfg.blocks[bb].succs) != 0 for bb in 1:V]
    source = rand(findall(has_edge))
    target = rand(cfg.blocks[source].succs)
    return source, target
end

# Generate a random CFG with the requested number of vertices and edges, then simulate
# `deletions` edge removals and verify that reachability is maintained correctly.
#
# If `all_checks` is true, verify internal data structures as well with O(E^2) checks.
function test_reachability(V, E; deletions = 2E ÷ 3, all_checks=false)

    function check_reachability(reachability, cfg, domtree, all_checks)
        for i = 1:V
            # All nodes should be reported as unreachable only if we cannot reach them from BB #1.
            @test reachable(cfg, 1, i) == !bb_unreachable(reachability, i)

            # All predecessors of a reachable block should be reachable.
            if !bb_unreachable(reachability, i)
                for pred in cfg.blocks[i].preds
                    @test !bb_unreachable(reachability, pred)
                end
            end
        end

        if all_checks # checks for internal data structures - O(E^2)

            # Nodes should be mutually reachable iff they are in the same SCC.
            scc = reachability.scc
            reachable_nodes = BitSet(v for v = 1:V if !bb_unreachable(reachability, v))
            for i ∈ reachable_nodes
                for j ∈ reachable_nodes
                    @test (reachable(cfg, i, j; domtree) && reachable(cfg, j, i; domtree)) == (scc[i] == scc[j])
                end
            end

            # Nodes in any non-trivial SCC (ignoring backedges) should be marked irreducible.
            irreducible = reachability.irreducible
            for i ∈ reachable_nodes
                in_nontrivial_scc = any(v != i && scc[v] == scc[i] for v = 1:V)
                @test Compiler.getindex(irreducible, i) == in_nontrivial_scc
            end
        end
    end

    cfg = rand_cfg(V, E)
    domtree = Compiler.construct_domtree(cfg)
    reachability = CFGReachability(cfg, domtree)
    check_reachability(reachability, cfg, domtree, all_checks)

    # track the reachable blocks/edges so that we can verify callbacks below
    blocks = Set{Int}()
    edges = Set{Tuple{Int,Int}}()
    for bb in 1:V
        !bb_unreachable(reachability, bb) && push!(blocks, bb)
        for succ in cfg.blocks[bb].succs
            push!(edges, (bb, succ))
        end
    end

    killed_edges = Tuple{Int,Int}[]
    killed_blocks = Int[]
    for k = 1:deletions
        length(blocks) == 1 && break # no more reachable blocks

        from, to = get_random_edge(cfg, V)
        kill_edge!(reachability, cfg, from, to,
            (from::Int, to::Int) -> push!(killed_edges, (from, to)),
            (bb::Int) -> push!(killed_blocks, bb),
        )

        # If these nodes are still reachable, to and from edges should have been removed.
        @test !reachable(cfg, 1, from) || !in(to, cfg.blocks[from].succs)
        @test !reachable(cfg, 1, to)   || !in(from, cfg.blocks[to].preds)

        check_reachability(reachability, cfg, domtree, all_checks)

        for bb in 1:V
            if bb_unreachable(reachability, bb) && in(bb, blocks)
                # If the block changed from reachable -> unreachable, we should have gotten a callback.
                @test bb in killed_blocks
                delete!(blocks, bb)
            end
        end
        for (from, to) in edges
            if !in(from, cfg.blocks[to].preds) && !bb_unreachable(reachability, to)
                # If the edge changed from reachable -> unreachable and feeds into a reachable BasicBlock,
                # we should have gotten a callback.
                @test (from, to) in killed_edges
                delete!(edges, (from, to))
            end
        end

        empty!(killed_edges)
        empty!(killed_blocks)
    end
end

@testset "CFGReachability tests" begin
    test_reachability(1, 0; all_checks=true)

    test_reachability(10, 15; all_checks=true)
    test_reachability(10, 15; all_checks=true)
    test_reachability(10, 15; all_checks=true)

    test_reachability(100, 150; all_checks=false)
    test_reachability(100, 150; all_checks=false)
    test_reachability(100, 1000; all_checks=false)
end

function cfg_from_succs(succs::Vector{Vector{Int}})
    preds = [Int[] for _ in succs]
    for (from, bb_succs) in pairs(succs), to in bb_succs
        push!(preds[to], from)
    end
    return CFG(BasicBlock[BasicBlock(StmtRange(0, 0), preds[bb], succs[bb]) for bb in eachindex(succs)], Int[])
end
function same_as_fresh_domtree(domtree, blocks::Vector{BasicBlock})
    fresh = Compiler.construct_domtree(blocks)
    domtree.idoms_bb == fresh.idoms_bb == Compiler.naive_idoms(blocks) || return false
    for bb in eachindex(blocks)
        bb_unreachable(fresh, bb) && continue
        domtree.nodes[bb].level == fresh.nodes[bb].level || return false
        sort(domtree.nodes[bb].children) == sort(fresh.nodes[bb].children) || return false
    end
    return true
end

# Test that toggling any edge of a small CFG twice, updating the dominator tree
# incrementally each time, matches recomputing the tree
let graphs = [
        [[3, 2], [5], [4], [6], Int[], [5, 3]],
        [[2, 3], [4, 5], [4], [5], Int[]],
        [[2], [3, 4], [2], Int[], [4]], # block 5 is unreachable
        [[5, 6], Int[], [3], [2], [4, 2], [2, 5]],
        [[7, 4], [4], [2, 6, 5], [6, 7, 4], [5, 3], [2, 7, 6], [6, 2, 5, 3, 4]], # deleting 2->5 changes idoms
    ]
    failures = Tuple{Vector{Vector{Int}},Int,Int}[]
    for succs in graphs
        cfg = cfg_from_succs(succs)
        domtree = Compiler.construct_domtree(cfg)
        for from in eachindex(succs), to in 2:length(succs), _ in 1:2
            if to in cfg.blocks[from].succs
                Compiler.cfg_delete_edge!(cfg, from, to)
                Compiler.domtree_delete_edge!(domtree, cfg.blocks, from, to)
            else
                Compiler.cfg_insert_edge!(cfg, from, to)
                Compiler.domtree_insert_edge!(domtree, cfg.blocks, from, to)
            end
            same_as_fresh_domtree(domtree, cfg.blocks) || push!(failures, (succs, from, to))
        end
    end
    @test isempty(failures)
end

# Test a CFG large enough for path compression to use its worklist
let n = 40
    succs = [bb < n ? [bb + 1] : [2] for bb in 1:n]
    push!(succs[10], 30)
    push!(succs[25], 5)
    cfg = cfg_from_succs(succs)
    domtree = Compiler.construct_domtree(cfg)
    @test domtree.idoms_bb == Compiler.naive_idoms(cfg.blocks)
    Compiler.cfg_delete_edge!(cfg, 25, 5)
    Compiler.domtree_delete_edge!(domtree, cfg.blocks, 25, 5)
    @test same_as_fresh_domtree(domtree, cfg.blocks)
end

# Test dominator queries
let cfg = cfg_from_succs([[3, 2], [5], [4], [6], Int[], [5, 3]])
    domtree = Compiler.construct_domtree(cfg)
    @test Compiler.nearest_common_dominator(domtree, 4, 6) == 4
    @test Compiler.nearest_common_dominator(domtree, 6, 4) == 4
    @test Compiler.nearest_common_dominator(domtree, 2, 6) == 1
    @test Compiler.nearest_common_dominator(domtree, 5, 5) == 5
    @test Compiler.nearest_common_dominator(domtree, 0, 3) == 0
    @test Compiler.nearest_common_dominator(domtree, 3, 0) == 0
    postdomtree = Compiler.construct_postdomtree(cfg)
    @test Compiler.nearest_common_dominator(postdomtree, 3, 6) == 6
    @test Compiler.nearest_common_dominator(postdomtree, 2, 3) == 5
    function dominated_blocks(root)
        bbs = Int[]
        for bb in Compiler.dominated(domtree, root)
            push!(bbs, bb)
        end
        return sort!(bbs)
    end
    @test dominated_blocks(1) == 1:6
    @test dominated_blocks(2) == [2]
    @test dominated_blocks(3) == [3, 4, 6]
end

# Test that renaming the blocks of a dominator tree, and dropping an unreachable
# one, gives the tree of the renamed CFG
let succs = [[3, 2], [5], [4], [6], Int[], [5, 3], [6]],
    rename = [1, 6, 2, 5, 4, 3, -1]
    domtree = Compiler.construct_domtree(cfg_from_succs(succs))
    renamed_succs = Vector{Vector{Int}}(undef, 6)
    for (old, new) in pairs(rename)
        new == -1 || (renamed_succs[new] = rename[succs[old]])
    end
    renamed = cfg_from_succs(renamed_succs)
    Compiler.rename_nodes!(domtree, rename)
    fresh = Compiler.construct_domtree(renamed)
    @test domtree.idoms_bb == [0, 1, 5, 1, 2, 1]
    @test same_as_fresh_domtree(domtree, renamed.blocks)
    @test domtree.dfs_tree.to_pre == fresh.dfs_tree.to_pre
    @test domtree.dfs_tree.to_post == fresh.dfs_tree.to_post
end
# ... and that an unreachable block can also be kept
let rename = [1, 2, 4, 3]
    domtree = Compiler.construct_domtree(cfg_from_succs([[2], [3], Int[], [3]]))
    renamed = cfg_from_succs([[2], [4], [4], Int[]])
    Compiler.rename_nodes!(domtree, rename)
    fresh = Compiler.construct_domtree(renamed)
    @test domtree.idoms_bb == [0, 1, 0, 2]
    @test same_as_fresh_domtree(domtree, renamed.blocks)
    @test domtree.dfs_tree.to_pre == fresh.dfs_tree.to_pre
    @test domtree.dfs_tree.to_post[[1, 2, 4]] == fresh.dfs_tree.to_post[[1, 2, 4]]
end

# Test that copies of a DFS tree are equal to it but share no storage with it
let dfs = Compiler.construct_domtree(cfg_from_succs([[3, 2], [5], [4], [6], Int[], [5, 3]])).dfs_tree
    fields(d) = (d.to_pre, d.from_pre, d.to_post, d.from_post, d.to_parent_pre)
    for dfs′ in (Compiler.copy(dfs), Compiler.copy!(Compiler.DFSTree(0), dfs))
        @test fields(dfs′) == fields(dfs)
        @test !any(map(===, fields(dfs′), fields(dfs)))
    end
end
