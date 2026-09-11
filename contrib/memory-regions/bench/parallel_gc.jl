# What a stock collection costs when many threads mark it, with the region
# runtime unused.
#
#   julia -t T --gcthreads=G parallel_gc.jl [collections] [depth] [refs]
#
# The script opens no window and calls no region entry point, so the same
# file runs on a vanilla julia and on a julia that carries the region
# runtime. The difference between the two binaries at one thread count is
# what an unused region runtime costs a parallel collection.
#
# Why the row exists: the region runtime adds one relaxed load per object in
# the mark loops (the census filter), one byte test per page in the sweep,
# and three brackets that walk 64 region entries per heap. The first cost is
# a load of a global that every marking thread reads, the third grows with
# the number of heaps. A serial mark cannot show either. A hardware loop
# runs on the whole machine, so the whole machine is where the cost must be
# read.
#
# The live set is built by every thread, so every heap holds a part of it
# and the marking threads have work to steal. `collections` full collections
# run after a warm one; every collection is one row. The row reports the
# wall time of the call and, when the runtime publishes them, the mark and
# the sweep time of that collection and the time the collection waited for
# every thread to reach a safepoint. The last one is the delay a thread that
# runs Julia code imposes on the collection, and it grows with the thread
# count.
#
# Rows (REGIONS_TSV):
#
#   threads gcthreads collection wall_ms mark_ms sweep_ms safepoint_us live_mb
#
# A field the runtime does not publish is written as -1.
include(joinpath(@__DIR__, "report.jl"))

const COLLECTIONS = length(ARGS) >= 1 ? parse(Int, ARGS[1]) : 12
const DEPTH       = length(ARGS) >= 2 ? parse(Int, ARGS[2]) : 20
const REFS        = length(ARGS) >= 3 ? parse(Int, ARGS[3]) : 1_000_000

mutable struct Node
    left::Union{Node, Nothing}
    right::Union{Node, Nothing}
    v::Int
end

build_tree(depth) = depth == 0 ? nothing : Node(build_tree(depth - 1), build_tree(depth - 1), depth)

# One tree and one vector of boxes per thread, built on that thread, so the
# live set sits on every heap. The parts stay rooted in this vector for the
# whole run.
@noinline function build_live(nthreads)
    parts = Vector{Any}(undef, nthreads)
    Threads.@threads :static for t in 1:nthreads
        tree = build_tree(DEPTH)
        refs = Any[Ref(i) for i in 1:(REFS ÷ nthreads)]
        parts[t] = (tree, refs)
    end
    return parts
end

# The fields of GC_Num differ between versions; read what this runtime has.
field(gcn, name) = hasproperty(gcn, name) ? Float64(getproperty(gcn, name)) : -1.0

function main()
    nthreads = Threads.nthreads()
    gcthreads = try                          # the entry is not in every version
        Int(Threads.ngcthreads())
    catch
        Int(Base.JLOptions().nmarkthreads)
    end
    live = build_live(nthreads)
    GC.gc()                                  # a warm collection; not a row
    before = Base.gc_num()
    for c in 1:COLLECTIONS
        t0 = time_ns()
        GC.gc()
        wall_ms = (time_ns() - t0) / 1e6
        after = Base.gc_num()
        mark_ms = field(after, :total_mark_time) < 0 ? -1.0 :
                  (field(after, :total_mark_time) - field(before, :total_mark_time)) / 1e6
        sweep_ms = field(after, :total_sweep_time) < 0 ? -1.0 :
                   (field(after, :total_sweep_time) - field(before, :total_sweep_time)) / 1e6
        # total_time_to_safepoint is cumulative, so the difference is the
        # wait of this collection; max_time_to_safepoint is a running
        # maximum and would repeat itself on every row.
        safepoint_us = field(after, :total_time_to_safepoint) < 0 ? -1.0 :
                       (field(after, :total_time_to_safepoint) -
                        field(before, :total_time_to_safepoint)) / 1e3
        live_mb = Base.gc_live_bytes() / 2^20
        tsv_row(["threads", "gcthreads", "collection", "wall_ms", "mark_ms", "sweep_ms",
                 "safepoint_us", "live_mb"],
                Any[nthreads, gcthreads, c,          # Any: an Int stays an Int
                 round(wall_ms; digits = 3), round(mark_ms; digits = 3),
                 round(sweep_ms; digits = 3), round(safepoint_us; digits = 3),
                 round(live_mb; digits = 1)])
        println("threads=$nthreads gcthreads=$gcthreads collection=$c wall=$(round(wall_ms; digits=2)) ms ",
                "mark=$(round(mark_ms; digits=2)) ms sweep=$(round(sweep_ms; digits=2)) ms ",
                "safepoint=$(round(safepoint_us; digits=1)) us live=$(round(live_mb; digits=1)) MB")
        before = after
    end
    GC.@preserve live nothing
    return nothing
end

main()
