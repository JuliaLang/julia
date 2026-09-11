# The tree's showcase: N mutually-isolated event leaves, one per worker task.
#
# The distinguishing feature of the tree over the chain is SIBLING isolation.
# The chain is a total order (region 1 < 2 < 3 ...), so two workers' regions
# are always ancestor-and-descendant: a store from one into the other is
# legal, and neither can reset while the other references in. The tree lets
# the workers' leaves be SIBLINGS -- declared children of the same parent --
# so they are mutually isolated, and each resets its own leaf per event with
# no coordination and no risk to another's transients.
#
# Here the shared network is permanent (region 0). Each of N workers owns a
# sibling leaf. Every event allocates a burst of transient messages (each
# referencing a network node -- a legal leaf -> region-0 edge), updates node
# state, then resets its leaf: the event's garbage dies at once and never
# reaches the stock collector. The same workload runs again WITHOUT regions,
# so the transients fall to the collector -- the showcase is the collection
# count.
#   run with: julia -t4 showcase_tree.jl
@noinline region_set(n)        = ccall(:jl_gc_region_set, Cint, (Cint,), n)
@noinline unsafe_region_reset(n)      = UInt64(ccall(:jl_gc_region_unsafe_reset, UInt64, (Cint,), n))
@noinline region_parent!(c, p) = ccall(:jl_gc_region_declare_parent, Cint, (Cint, Cint), c, p)
@noinline quarantined(n)       = Int(ccall(:jl_gc_region_quarantined, Cint, (Cint,), n)) != 0

# When REGIONS_TSV names a file, append one row of numbers to it; the first
# row into a new file is the header. results/run_all.sh collects every
# measurement this way and results/plot.py draws from the files.
function tsv_row(names, values)
    path = get(ENV, "REGIONS_TSV", "")
    isempty(path) && return nothing
    header = !isfile(path) || filesize(path) == 0
    open(path, "a") do io
        header && println(io, "# ", join(names, '\t'))
        println(io, join(values, '\t'))
    end
    return nothing
end

leaf_of(tid) = tid                    # workers own leaves 1..NWORKERS
const NWORKERS = 3
const NEVENTS = 2000
const BURST = 200
const NNODES = 64

mutable struct Node; id::Int; state::Int; end
mutable struct Message; payload::Vector{Int}; target::Node; end

@noinline build_network() = [Node(i, 0) for i in 1:NNODES]   # region 0, permanent

# One task's event loop over its OWN stripe of nodes (disjoint across workers,
# so the shared-network updates are race-free and the total is deterministic).
# `leaf == 0` is stock mode: no region, no reset, so the transients fall to
# the collector.
@noinline function run_events(nodes, stripe, leaf)
    ns = length(stripe)
    for _ in 1:NEVENTS
        leaf != 0 && region_set(leaf)
        msgs = Vector{Message}(undef, BURST)
        for b in 1:BURST
            @inbounds msgs[b] = Message(collect(1:8), nodes[stripe[(b % ns) + 1]])
        end
        for m in msgs
            m.target.state += length(m.payload)   # a region-0 node write: legal
        end
        leaf != 0 && region_set(0)
        leaf != 0 && unsafe_region_reset(leaf)            # the whole event dies at once
    end
end

function drive(mode::Symbol)
    nodes = build_network()
    stripes = [collect(w:NWORKERS:NNODES) for w in 1:NWORKERS]  # disjoint per worker
    gc0 = Base.gc_num(); t0 = time_ns()
    Threads.@threads for w in 1:NWORKERS
        run_events(nodes, stripes[w], mode === :tree ? leaf_of(w) : 0)
    end
    t1 = time_ns(); gc1 = Base.gc_num()
    (; wall_ms = (t1 - t0)/1e6,
       collections = gc1.pause - gc0.pause,
       gc_ms = (gc1.total_time - gc0.total_time)/1e6,
       finalstate = sum(n.state for n in nodes))
end

# Declare the workers' leaves as SIBLINGS: each a direct child of region 0.
# The chain default would make leaf 2 a child of leaf 1; declaring parent 0
# makes them siblings, mutually isolated.
for w in 1:NWORKERS
    region_parent!(leaf_of(w), 0)
end

drive(:tree); drive(:stock)           # warm
GC.gc(true); tree = drive(:tree)
GC.gc(true); stock = drive(:stock)

anyq = any(quarantined(leaf_of(w)) for w in 1:NWORKERS)
println("workers ", NWORKERS, " (sibling leaves)  events/worker ", NEVENTS, "  burst ", BURST)
println("tree   wall ", round(tree.wall_ms; digits=1), " ms   collections ", tree.collections,
        "   gc ", round(tree.gc_ms; digits=1), " ms")
println("stock  wall ", round(stock.wall_ms; digits=1), " ms   collections ", stock.collections,
        "   gc ", round(stock.gc_ms; digits=1), " ms")
println("same result: ", tree.finalstate == stock.finalstate, "   any quarantine: ", anyq)
if tree.collections < stock.collections && !anyq && tree.finalstate == stock.finalstate
    println("SHOWCASE TREE: ", NWORKERS, " sibling leaves reset per event and kept the collector out (",
            tree.collections, " vs ", stock.collections, " collections)")
else
    println("SHOWCASE TREE: UNEXPECTED (check the numbers)")
end

tsv_row(("showcase", "mode", "param", "wall_s", "collections", "gc_ms", "peak_rss_mb"),
        ("tree", "tree", NWORKERS, tree.wall_ms / 1e3, tree.collections, tree.gc_ms, "NA"))
tsv_row(("showcase", "mode", "param", "wall_s", "collections", "gc_ms", "peak_rss_mb"),
        ("tree", "stock", NWORKERS, stock.wall_ms / 1e3, stock.collections, stock.gc_ms, "NA"))
