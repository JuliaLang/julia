# What a reset costs the machine, not only the caller.
#
#   julia -t T reset_pause.jl [checked|unsafe] [resets] [objects]
#
# The checked reset stops the world: it waits for every thread to reach a
# safepoint, marks from the execution roots of every thread, and frees in
# the same pause. Its cost therefore grows with the number of threads that
# run Julia code, and a hardware loop runs on the whole machine. The
# unchecked entry frees with no pause and is the control.
#
# The script runs `workers = T - 1` worker tasks. A worker does arithmetic,
# reaches a safepoint every round, and allocates a small object now and
# then, so it keeps a thread busy without filling the heap. It records every
# gap between two rounds that is longer than `GAP_MIN`, with the time the
# gap started. The main task fills
# region 1, closes the window, and times one reset; the interval of every
# reset is kept. A gap that overlaps a reset interval is a stall that the
# reset imposed on that worker.
#
# The workers allocate, so the stock collector runs during the row. The
# number of collections goes into every row: a run whose stalls sit far
# above the resets, with many collections, measures the collector and not
# the reset.
#
# Rows (REGIONS_TSV):
#
#   mode threads workers kind index value_us collections
#
#   kind = reset  index = the reset number  value_us = the caller's wall time
#   kind = stall  index = the worker        value_us = its longest stall
#                                                      inside a reset
#
# The script needs the region entry points, so it runs on this branch alone.
include(joinpath(@__DIR__, "report.jl"))

const MODE    = length(ARGS) >= 1 ? ARGS[1] : "checked"
const RESETS  = length(ARGS) >= 2 ? parse(Int, ARGS[2]) : 200
const OBJECTS = length(ARGS) >= 3 ? parse(Int, ARGS[3]) : 100_000
const GAP_MIN = 20_000                     # ns; below this a gap is scheduling noise
const CAP     = 1 << 14                    # gaps kept per worker

MODE in ("checked", "unsafe") || error("mode must be checked or unsafe")

region_set(n) = ccall(:jl_gc_region_set, Cint, (Cint,), n)
region_quarantined(n) = ccall(:jl_gc_region_quarantined, Cint, (Cint,), n)

function region_reset(n, checked)
    r = checked ? ccall(:jl_gc_region_reset, UInt64, (Cint,), n) :
                  ccall(:jl_gc_region_unsafe_reset, UInt64, (Cint,), n)
    r > typemax(UInt64) - 16 &&
        error("the reset of region $n was refused with code $(reinterpret(Int64, r))")
    return r
end

# The objects of the region live in this frame alone, and the frame returns
# before the reset: a local that names a region object refuses the checked
# reset with EROOT.
@noinline function fill_region!(n)
    region_set(n)
    ring = Vector{Any}(undef, 1024)
    for i in 1:OBJECTS
        @inbounds ring[(i & 1023) + 1] = Ref(i)
    end
    region_set(0)
    return nothing
end

# A worker: arithmetic, a safepoint every round, and a small allocation now
# and then. The safepoint is what a stop-the-world waits for, and the low
# allocation rate keeps the stock collector out of the measurement as far as
# a busy thread allows. The worker records every gap between two rounds that
# is longer than GAP_MIN, with the time the gap started.
#
# It captures no region object: a task closure that captured one would be a
# region-0 object holding a region reference, which the barrier reports as
# an escape.
function worker(stop::Threads.Atomic{Bool}, starts::Vector{UInt64}, gaps::Vector{UInt64},
                count::Base.RefValue{Int})
    acc = 0.0
    iter = 0
    last = time_ns()
    while !stop[]
        for _ in 1:64
            acc = muladd(1.0000001, acc, 1.0e-9)
        end
        iter += 1
        if (iter & 1023) == 0                # a little garbage, not a lot
            r = Ref(acc)
            acc = muladd(1.0e-12, r[], acc)
        end
        GC.safepoint()
        now = time_ns()
        gap = now - last
        if gap > GAP_MIN && count[] < CAP
            count[] += 1
            @inbounds starts[count[]] = last
            @inbounds gaps[count[]] = gap
        end
        last = now
    end
    return acc
end

function main()
    checked = MODE == "checked"
    nthreads = Threads.nthreads()
    workers = max(nthreads - 1, 0)
    stop = Threads.Atomic{Bool}(false)
    starts = [Vector{UInt64}(undef, CAP) for _ in 1:max(workers, 1)]
    gaps   = [Vector{UInt64}(undef, CAP) for _ in 1:max(workers, 1)]
    counts = [Ref(0) for _ in 1:max(workers, 1)]
    tasks = Task[]
    for w in 1:workers
        push!(tasks, Threads.@spawn worker(stop, starts[w], gaps[w], counts[w]))
    end

    fill_region!(1); region_reset(1, checked)          # warm the path
    gc0 = Base.gc_num().pause
    win_lo = Vector{UInt64}(undef, RESETS)
    win_hi = Vector{UInt64}(undef, RESETS)
    wall = Vector{Float64}(undef, RESETS)
    for i in 1:RESETS
        fill_region!(1)
        t0 = time_ns()
        region_reset(1, checked)
        t1 = time_ns()
        win_lo[i] = t0
        win_hi[i] = t1
        wall[i] = (t1 - t0) / 1e3
    end
    collections = Base.gc_num().pause - gc0
    stop[] = true
    foreach(wait, tasks)

    for i in 1:RESETS
        tsv_row(["mode", "threads", "workers", "kind", "index", "value_us", "collections"],
                [MODE, nthreads, workers, "reset", i, round(wall[i]; digits = 3), collections])
    end
    stall_max = 0.0
    for w in 1:workers
        best = 0.0
        for k in 1:counts[w][]
            s = starts[w][k]
            e = s + gaps[w][k]
            inside = false
            for i in 1:RESETS
                if s < win_hi[i] && e > win_lo[i]
                    inside = true
                    break
                end
            end
            inside && (best = max(best, gaps[w][k] / 1e3))
        end
        stall_max = max(stall_max, best)
        tsv_row(["mode", "threads", "workers", "kind", "index", "value_us", "collections"],
                [MODE, nthreads, workers, "stall", w, round(best; digits = 3), collections])
    end

    sorted = sort(wall)
    med = sorted[cld(length(sorted), 2)]
    println("mode=$MODE threads=$nthreads workers=$workers resets=$RESETS ",
            "median=$(round(med; digits = 1)) us max=$(round(maximum(wall); digits = 1)) us ",
            "worst worker stall=$(round(stall_max; digits = 1)) us collections=$collections")
    if region_quarantined(1) != 0
        println(stderr, "reset_pause: region 1 was quarantined; the row is invalid")
        exit(1)
    end
    return nothing
end

main()
