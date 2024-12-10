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

            # Nodes should be mutually reachable iff they are in the same SCompiler.
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
