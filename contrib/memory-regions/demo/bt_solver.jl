# Demonstrator A: backtracking graph K-colouring, regions vs the stock
# collector, on the SAME code. A batch of independent random instances; each
# instance's search allocates freely (a `Set` of used colours per node, a
# fresh assignment per instance), and in region mode the whole instance's
# garbage dies at one `unsafe_region_reset`. The answer copied out is a small Int
# checksum (region 0). In stock mode the same garbage falls to the collector.
#
# This is the natural, readable, allocating form -- the form a non-expert
# writes. A bitmask solver allocates nothing and regions rightly do not help
# it; the write-up says so. The win here is that the search's speculative
# allocations never reach a stop-the-world collection.
#   run with: julia bt_solver.jl
include(joinpath(@__DIR__, "demo_common.jl"))
using .DemoCommon
using Printf

@noinline region_set(n)   = ccall(:jl_gc_region_set, Cint, (Cint,), n)
@noinline unsafe_region_reset(n) = UInt64(ccall(:jl_gc_region_unsafe_reset, UInt64, (Cint,), n))
@noinline quarantined(n)  = Int(ccall(:jl_gc_region_quarantined, Cint, (Cint,), n)) != 0
const LEAF = 1

# A deterministic random graph: vertex count `n`, edge probability `p`, seeded
# by the instance index so region and stock runs build identical instances.
@noinline function make_graph(idx::Int, n::Int, p::Float64)
    st = UInt64(0x9E3779B97F4A7C15) * UInt64(idx + 1)
    nextrand() = (st = st * 6364136223846793005 + 1442695040888963407; (st >> 33) / 2.0^31)
    adj = [Int[] for _ in 1:n]
    for i in 1:n, j in i+1:n
        if nextrand() < p
            push!(adj[i], j); push!(adj[j], i)
        end
    end
    adj
end

# Natural allocating backtracking: a Set of used colours per node. A node-visit
# budget CAPS the search: in region mode the whole instance's garbage is held
# until the reset, so an unbounded search would grow the region without bound
# and OOM (the stock collector would reclaim the backtracked garbage mid-
# search). The cap bounds both time and region memory per instance; both the
# region and the stock run use the same cap, so they give the same answer.
const NODE_CAP = 3000

mutable struct Budget; visits::Int; end

function color!(adj, K, v, assignment, b::Budget)
    b.visits += 1
    b.visits > NODE_CAP && return true     # give up: capped, deterministic in both runs
    v > length(adj) && return true
    used = Set{Int}()                      # allocates, per search node
    @inbounds for u in adj[v]
        assignment[u] != 0 && push!(used, assignment[u])
    end
    avail = [c for c in 1:K if !(c in used)]  # a candidate list, more garbage per node
    @inbounds for c in avail
        assignment[v] = c
        color!(adj, K, v + 1, assignment, b) && return true
        assignment[v] = 0
    end
    return false
end

@noinline function solve_one(idx, n, p, K)
    adj = make_graph(idx, n, p)
    assignment = zeros(Int, n)
    b = Budget(0)
    colorable = color!(adj, K, 1, assignment, b)
    # The answer copied out is a small Int: the colouring checksum plus the
    # visit count, so a capped instance and a solved one still agree run to run.
    (colorable ? sum(assignment) : 0) + b.visits
end

# The batch, region mode: one region reset per instance.
function batch_region(ninst, n, p, K)
    acc = 0
    for idx in 1:ninst
        region_set(LEAF)
        acc += solve_one(idx, n, p, K)     # all of solve_one's garbage is in the leaf
        region_set(0)
        unsafe_region_reset(LEAF)                 # dropped wholesale
    end
    acc
end

# The batch, stock mode: identical work, the collector handles the garbage.
function batch_stock(ninst, n, p, K)
    acc = 0
    for idx in 1:ninst
        acc += solve_one(idx, n, p, K)
    end
    acc
end

function run_scale(label, ninst, n, p, K)
    reg, sto, equal = DemoCommon.ab(
        () -> batch_region(ninst, n, p, K),
        () -> batch_stock(ninst, n, p, K); reps = 5)
    report_table("A", "$label  ($ninst instances, n=$n, K=$K)", reg, sto)
    println("  same answer: ", equal, "   quarantine: ", quarantined(LEAF))
    equal && !quarantined(LEAF)
end

# Warm in STOCK mode FIRST, so solve_one/color!/make_graph compile at region 0.
# Compiling inside a region window would allocate the JIT's method-table
# entries in the region and store them into the global cache -- a real escape,
# which the barrier quarantines. Stock-first compilation avoids it; only then
# does the region path open a window over already-compiled code.
batch_stock(200, 30, 0.4, 3)
batch_region(200, 30, 0.4, 3)

# Hard instances near the colouring threshold (K=3, dense): the search hits the
# node cap, so each instance allocates NODE_CAP nodes' worth of garbage -- a
# large but BOUNDED working set that dies at the per-instance reset. Across
# thousands of instances that is real allocation pressure on the collector,
# while the region's peak stays one instance's search.
println("Demonstrator A -- backtracking graph colouring, regions vs stock\n")
allok = true
allok &= run_scale("small ", 1500, 30, 0.40, 3)
allok &= run_scale("medium", 3000, 32, 0.40, 3)
allok &= run_scale("large ", 5000, 34, 0.40, 3)
println()
println(allok ? "DEMO A: consistent (same answers, no escape)" : "DEMO A: CHECK (answers or escape)")
