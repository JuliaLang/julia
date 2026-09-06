# The census: collect the Simulation region alone, and compare its pause
# against a full sweep.
#
#   julia census.jl scoped 2000000 [every] [K]
#   julia census.jl coop   2000000 [every] [K]
#   julia census.jl pooled 2000000 [every] [K]
#   julia census.jl full   2000000 [every] [K]
#   julia census.jl auto   2000000 [every] [K]   (GC on, no explicit collects)
#   julia census.jl batch  2000000 [every] [K] [W] [B]  (one Event window per B events)
#   julia census.jl autopool 2000000 [every] [K] [W]    (the batch handler under stock GC)
#   julia census.jl real   5000000 [every] [K] [W] [B]  (the real-world loop: one Event window per B events,
#                                                         the kept record allocated in the Simulation region, a
#                                                         cooperative census every `every` events at a slice boundary)
# `real` and `auto` are the pair a simulator cares about: the same handler
# - scratch that dies, a record that replaces the old one - under the regions
# and under the stock collector, each event's wall time recorded.
#
# `pooled` is the ownership-style variant: records are UPDATED IN PLACE, so
# the Simulation region makes no garbage at all -- the C++ free-on-replace
# equivalent -- and its collect pause is the pure floor (stop-the-world +
# mark of the live set).
#
# The model: a Simulation-region table of K live records with turnover --
# every event replaces one slot, so the replaced record becomes garbage in
# the Simulation region -- plus Event-region scratch, reset every event.
# Every `every` events the driver collects. `scoped` collects ONLY the
# Simulation region (jl_gc_region_collect: mark from the execution roots
# through the region filter, sweep the region's own pages). `full` keeps
# everything in the ordinary heap and runs GC.gc(). Rule 3 is what makes
# the scoped mark small: no global and no older-region root can point into
# the region, so only the task stacks are walked.

region_set(n)     = ccall(:jl_gc_region_set, Cint, (Cint,), n)
unsafe_region_reset(n)   = ccall(:jl_gc_region_unsafe_reset, UInt64, (Cint,), n)
region_collect(n) = ccall(:jl_gc_region_collect, Int64, (Cint,), n)
region_coop(n)    = ccall(:jl_gc_region_collect_coop, Int64, (Cint,), n)
region_verify(n)  = ccall(:jl_gc_region_verify, Cint, (Cint,), n)
region_stat(i)    = ccall(:jl_gc_region_stat, UInt64, (Cint,), i)
region_reserve(b) = ccall(:jl_gc_heap_reserve, UInt64, (UInt64,), b)

include(joinpath(@__DIR__, "report.jl"))

mutable struct Record
    id::Int
    a::Float64
    b::Float64
end

@noinline _touch(v::Vector{Float64}) = (isempty(v) && error("empty"); nothing)

const SIM = 1
const EVENT = 2
# Live records in the Simulation region; the fourth argument overrides.
const K = length(ARGS) >= 4 ? parse(Int, ARGS[4]) : 10_000
# Scratch floats allocated per event (the garbage knob); the fifth
# argument overrides. 3 is light; 200 is ~1.6 KB per event.
const W = length(ARGS) >= 5 ? parse(Int, ARGS[5]) : 3
# The batch size for the `batch` variant: one Event window and one reset
# per B events; the sixth argument overrides.
const B = length(ARGS) >= 6 ? parse(Int, ARGS[6]) : 1
# ARGS[7]: the heap reserve in MB, claimed and prefaulted before the loop
# (0 = none). A hard real-time loop claims its heap before it starts, so
# that no event takes a page fault.
const RESERVE_MB = length(ARGS) >= 7 ? parse(Int, ARGS[7]) : 0
# ARGS[8]: a file to dump the latency distribution to (empty = none): 512
# log-spaced points of "exceed-fraction <TAB> latency ns", the data behind
# the CCDF plot, so the plots are rebuilt from the logs like the tables.
const DUMP_FILE = length(ARGS) >= 8 ? ARGS[8] : ""

# The batch handler: scratch plus an in-place record update, with NO
# region calls inside -- the driver owns the window.
@noinline function handle_batch!(table::Vector{Record}, i::Int)
    scratch = Vector{Float64}(undef, W)
    @inbounds for j in 1:W
        scratch[j] = Float64(i + j)
    end
    _touch(scratch)
    acc = scratch[1] + scratch[end]
    slot = (i - 1) % K + 1
    r = table[slot]
    r.id = i
    r.a = acc
    r.b = 2 * acc
    return nothing
end

# The real-world handler: scratch in the current (Event) window, and the
# kept record allocated in the Simulation region so it outlives the slice -
# "what outlives the event must say so". The old record becomes Simulation
# garbage for the census. No reset here: the driver owns the window.
@noinline function handle_real!(table::Vector{Record}, i::Int)
    scratch = Vector{Float64}(undef, W)
    @inbounds for j in 1:W
        scratch[j] = Float64(i + j)
    end
    _touch(scratch)
    acc = scratch[1] + scratch[end]
    slot = (i - 1) % K + 1
    region_set(SIM)
    table[slot] = Record(i, acc, 2 * acc)
    region_set(EVENT)
    return nothing
end

function build_table(use_regions::Bool)
    use_regions && region_set(SIM)
    table = Vector{Record}(undef, K)
    for i in 1:K
        table[i] = Record(i, 1.0, 2.0)
    end
    use_regions && region_set(0)
    return table
end

@noinline function handle!(table::Vector{Record}, i::Int, use_regions::Bool)
    # Event-region scratch: dies at the reset below.
    use_regions && region_set(EVENT)
    scratch = Vector{Float64}(undef, W)
    @inbounds for j in 1:W
        scratch[j] = Float64(i + j)
    end
    _touch(scratch)
    acc = scratch[1] + scratch[end]
    use_regions && region_set(0)
    use_regions && unsafe_region_reset(EVENT)
    # Simulation-region turnover: the replaced record becomes garbage.
    use_regions && region_set(SIM)
    slot = (i - 1) % K + 1
    table[slot] = Record(i, acc, 2 * acc)
    use_regions && region_set(0)
    return nothing
end

# The ownership-style handler: the record is UPDATED, not replaced. No
# Simulation garbage exists, exactly like C++ freeing on replacement.
@noinline function handle_pooled!(table::Vector{Record}, i::Int)
    region_set(EVENT)
    scratch = Vector{Float64}(undef, W)
    @inbounds for j in 1:W
        scratch[j] = Float64(i + j)
    end
    _touch(scratch)
    acc = scratch[1] + scratch[end]
    region_set(0)
    unsafe_region_reset(EVENT)
    slot = (i - 1) % K + 1
    r = table[slot]
    r.id = i
    r.a = acc
    r.b = 2 * acc
    return nothing
end

checksum(table) = (s = 0; for r in table; s += r.id; end; s)

function main()
    variant = ARGS[1]
    events = parse(Int, ARGS[2])
    use_regions = variant != "full" && variant != "auto" && variant != "autopool" && variant != "sched"
    coop = variant == "coop"
    every = length(ARGS) >= 3 ? parse(Int, ARGS[3]) : 100_000
    # `every` = 0 means: no census at all. For `real` that shows the census's
    # own share of the distribution by its absence; the Simulation garbage
    # then accumulates on region pages for the whole run.
    census_on = every > 0

    # The in-process full-sweep reference, before any region exists.
    t0 = time_ns(); GC.gc(); t1 = time_ns()
    println("full GC reference (startup): ", round((t1 - t0) / 1e6; digits = 2), " ms")

    # Under regions, the collector must not run while region 1 holds live
    # objects (a real collection would leave stale marks on the skipped
    # region pages). The scoped collector is region 1's only collector.
    use_regions && GC.enable(false)

    if RESERVE_MB > 0
        mapped = region_reserve(RESERVE_MB * 1_000_000)
        println("heap reserve       ", round(mapped / 1e6; digits = 1), " MB claimed and prefaulted")
    end
    # The scheduling class this run got, so the log proves it: 1 = SCHED_FIFO,
    # 2 = SCHED_RR, 0 = SCHED_OTHER (the time-shared class, which any other
    # runnable task preempts). The priority is sched_param's first field.
    let pol = ccall(:sched_getscheduler, Cint, (Cint,), 0), prm = zeros(Cint, 1)
        ccall(:sched_getparam, Cint, (Cint, Ptr{Cint}), 0, prm)
        println("scheduler          ", pol == 1 ? "SCHED_FIFO" : pol == 2 ? "SCHED_RR" : "SCHED_OTHER",
                " priority ", prm[1], pol == 0 ? " (no real-time class: any task can preempt the loop)" : "")
    end
    # Lock every page, present and future, so that reclaim can not take one
    # back; the limit (ulimit -l) must cover the heap, else EPERM/ENOMEM.
    let r = ccall(:mlockall, Cint, (Cint,), 3)   # MCL_CURRENT | MCL_FUTURE
        println("memory locked      ", r == 0 ? "yes (mlockall)" :
                "no (mlockall failed, errno $(Libc.errno()): raise ulimit -l)")
    end
    table = build_table(use_regions)
    # Warm on a throwaway table so compilation stays out of the wall time.
    let warm = build_table(use_regions)
        for i in 1:10_000
            if variant == "pooled"
                handle_pooled!(warm, i)
            elseif variant == "batch" || variant == "autopool"
                handle_batch!(warm, i)
            elseif variant == "real"
                region_set(EVENT); handle_real!(warm, i); region_set(0)
            else
                handle!(warm, i, use_regions)
            end
        end
        use_regions && unsafe_region_reset(EVENT)
    end
    # Per-event wall time, for the pair a simulator compares: `real` and
    # `auto` (and the in-place pair `batch` / `autopool`). Preallocated
    # outside every window, so the recording allocates nothing per event.
    # batch and autopool are the THROUGHPUT variants: two time_ns calls per
    # event would measure the clock, not the mechanism, so only the
    # latency pair records per-event times.
    record_latency = variant in ("real", "auto", "sched")
    lat = record_latency ? Vector{Int64}(undef, events) : Int64[]
    fill!(lat, 0)   # touched now: the recording's own pages must not fault inside the loop
    pauses_ns = Int64[]
    freed_total = Int64(0)
    stats = zeros(UInt64, 8)
    # OS preemption is not memory management: count the involuntary context
    # switches across the measured loop, so a maximum that coincides with one
    # is named, and a run with zero is the one whose maximum is genuine.
    nivcsw() = (m = match(r"nonvoluntary_ctxt_switches:\s+(\d+)", read("/proc/self/status", String));
                m === nothing ? -1 : parse(Int, m.captures[1]))
    # The same counter per THREAD from getrusage, cheap enough to sample
    # every BLOCK events inside the loop (all variants alike): a block whose
    # count moved took a preemption, and the maximum over the other blocks
    # is the mechanism's own. ru_nivcsw is the last field of struct rusage
    # on Linux x86_64: two timevals and fourteen longs.
    rusage = zeros(UInt8, 160)
    thread_nivcsw() = (ccall(:getrusage, Cint, (Cint, Ptr{UInt8}), 1, rusage);
                       unsafe_load(Ptr{Int64}(pointer(rusage) + 136)))
    # ru_minflt, the same struct: two timevals and four longs before it. A
    # page fault inside the loop is memory management by the OS, not by the
    # collector; the best case for a hard real-time loop is zero.
    thread_minflt() = (ccall(:getrusage, Cint, (Cint, Ptr{UInt8}), 1, rusage);
                       unsafe_load(Ptr{Int64}(pointer(rusage) + 64)))
    BLOCK = 10_000
    preempted = falses(cld(events, BLOCK))
    ctx0 = nivcsw()
    flt0 = thread_minflt()
    tsw = record_latency ? thread_nivcsw() : Int64(0)
    gcn0 = Base.gc_num().pause       # the stock collector's own count, for the stock variants
    t_wall0 = time_ns()
    (variant == "batch" || variant == "real") && region_set(EVENT)
    for i in 1:events
        t_ev = record_latency ? time_ns() : Int64(0)
        if variant == "pooled"
            handle_pooled!(table, i)
        elseif variant == "batch" || variant == "autopool"
            handle_batch!(table, i)
        elseif variant == "real"
            handle_real!(table, i)
        else
            handle!(table, i, use_regions)
        end
        if (variant == "batch" || variant == "real") && i % B == 0
            region_set(0)
            unsafe_region_reset(EVENT)
            # The real-world census: at a slice boundary the engine owns,
            # with the window closed, cooperative - no stop-the-world.
            if variant == "real" && census_on && i % every == 0
                c0 = time_ns()
                f = region_coop(SIM)
                c1 = time_ns()
                f < 0 && error("region_coop failed: ", f)
                freed_total += f
                push!(pauses_ns, Int64(c1 - c0))
                for k in 1:8
                    stats[k] += region_stat(k - 1)
                end
            end
            i < events && region_set(EVENT)
        end
        # Apples to apples: the scheduled stock collection is charged to the
        # event at whose boundary it runs, exactly like the census.
        if variant == "sched" && census_on && i % every == 0
            c0 = time_ns(); GC.gc(false); c1 = time_ns()
            push!(pauses_ns, Int64(c1 - c0))
        end
        if record_latency
            @inbounds lat[i] = time_ns() - t_ev
            if i % BLOCK == 0
                sw = thread_nivcsw()
                sw != tsw && (preempted[i ÷ BLOCK] = true; tsw = sw)
            end
        end
        if census_on && i % every == 0 && variant != "auto" && variant != "batch" && variant != "autopool" && variant != "real" && variant != "sched"
            if use_regions
                c0 = time_ns()
                f = coop ? region_coop(SIM) : region_collect(SIM)
                c1 = time_ns()
                f < 0 && error("region_collect failed: ", f)
                freed_total += f
                push!(pauses_ns, Int64(c1 - c0))
                for k in 1:8
                    stats[k] += region_stat(k - 1)
                end
                region_verify(SIM) == 0 && region_verify(EVENT) == 0 ||
                    error("verify failed after collect at event ", i)
            else
                # 'sched' is the stock collector under the program's own
                # schedule: a young collection at the census cadence, one
                # full collection when the run ends. 'full' is the
                # full-collection-per-cadence reference.
                c0 = time_ns(); variant == "sched" ? GC.gc(false) : GC.gc(); c1 = time_ns()
                push!(pauses_ns, Int64(c1 - c0))
            end
        end
    end

    if (variant == "batch" || variant == "real") && events % B != 0
        region_set(0)
        unsafe_region_reset(EVENT)
    end
    if variant == "sched"
        f0 = time_ns(); GC.gc(true); f1 = time_ns()
        println("final full collection ", round((f1 - f0) / 1e6; digits = 2), " ms")
    end
    t_wall1 = time_ns()
    ctx1 = nivcsw()
    flt1 = thread_minflt()
    wall_s = (t_wall1 - t_wall0) / 1e9
    println("involuntary context switches during the run: ", ctx1 - ctx0)
    println("page faults during the run: ", flt1 - flt0)
    stock_collections_val = use_regions ? "NA" : Base.gc_num().pause - gcn0
    use_regions || println("stock collections during the run: ", stock_collections_val)
    pause_total_ms = sum(pauses_ns) / 1e6
    println("wall time          ", round(wall_s; digits = 3), " s (",
            round(events / wall_s / 1e6; digits = 2), " M events/s)")
    println("collection total   ", round(pause_total_ms; digits = 1), " ms (",
            round(pause_total_ms / 10 / wall_s; digits = 2), " % of wall)")
    # Correctness: slot s last written at event i = events - K + s.
    expect = K * (events - K) + K * (K + 1) ÷ 2
    got = checksum(table)
    println("table checksum     ", got == expect ? "correct" : "WRONG ($got vs $expect)")
    if use_regions && !isempty(pauses_ns)
        nc = length(pauses_ns)
        println("=== phase means over ", nc, " collections ===")
        println("stop-the-world     ", round(stats[2] / nc / 1e3; digits = 1), " us")
        println("mark               ", round(stats[3] / nc / 1e3; digits = 1), " us")
        println("sweep              ", round(stats[4] / nc / 1e3; digits = 1), " us")
        println("live cells kept    ", stats[5] ÷ nc)
        println("cells freed        ", stats[6] ÷ nc)
        println("pages walked       ", stats[7] ÷ nc)
        println("pages wholesale    ", stats[8] ÷ nc)
    end
    sort!(pauses_ns)
    println("=== ", variant, ": ", length(pauses_ns), " collections ===")
    if !isempty(pauses_ns)
        println("pause p50          ", round(pauses_ns[(end + 1) ÷ 2] / 1e6; digits = 3), " ms")
        println("pause max          ", round(pauses_ns[end] / 1e6; digits = 3), " ms")
    end
    use_regions && println("cells freed        ", freed_total,
                           " (turnover was ", events - K, " records + scratch)")
    if record_latency
        # The maximum over the blocks that took no preemption, before sorting
        # destroys the event order.
        clean_max = Int64(0); nbad = count(preempted)
        imax = 1; nbig = 0
        for i in 1:events
            @inbounds preempted[cld(i, BLOCK)] || (lat[i] > clean_max && (clean_max = lat[i]))
            @inbounds lat[i] > lat[imax] && (imax = i)
            @inbounds lat[i] > 200_000 && (nbig += 1)
        end
        # Where the maximum sat: a slice boundary (i % B == 0), a census
        # boundary (i % every == 0), or neither - and how many events exceeded
        # 200 us at all, so a one-off stall of the growing heap is told from
        # a recurring cost.
        println("max at event      ", imax, " (slice boundary: ", B > 1 && imax % B == 0,
                ", census boundary: ", census_on && imax % every == 0,
                "); events over 200us: ", nbig)
        # The first thirty of them, with the event index and the time: a
        # stall of the growing heap recurs at intervals of allocated bytes.
        shown = 0
        for i in 1:events
            @inbounds if lat[i] > 200_000 && shown < 30
                println("  over 200us: event ", i, "  ", lat[i] ÷ 1000, " us",
                        preempted[cld(i, BLOCK)] ? "  (preempted block)" : "")
                shown += 1
            end
        end
        sort!(lat)
        if !isempty(DUMP_FILE)
            open(DUMP_FILE, "w") do io
                println(io, "# exceed_fraction\tlatency_ns")
                n = length(lat)
                for k in 0:511
                    f = 10.0^(-7.0 * k / 511)   # 1 down to 1e-7, log-spaced
                    r = clamp(n - floor(Int, f * n) + 1, 1, n)
                    println(io, f, "\t", lat[r])
                end
                println(io, 1.0 / n, "\t", lat[end])
            end
        end
        q(p) = lat[max(1, ceil(Int, p * events))]
        println("=== per-event wall time, ", events, " events ===")
        println("p50               ", q(0.5), " ns")
        println("p99               ", q(0.99), " ns")
        println("p99.9             ", q(0.999), " ns")
        println("p99.99            ", q(0.9999), " ns")
        println("max               ", lat[end], " ns")
        println("max, no preemption ", clean_max, " ns (", nbad, " of ", length(preempted),
                " blocks of ", BLOCK, " events took a preemption and are excluded)")
        over = count(x -> x > 100_000, lat)
        println("over 100us target ", over, " events (", round(100 * over / events; digits = 5), " %)")
    end
    # Under the regions the stock accounting never subtracts what a reset or
    # a census frees, so gc_live_bytes is the total allocated; the memory a
    # run really takes is its peak resident set.
    peak_rss_val = Sys.maxrss() / 1e6
    println("live heap counter  ", round(Base.gc_live_bytes() / 1e6; digits = 1), " MB")
    println("peak RSS           ", round(peak_rss_val; digits = 1), " MB")

    collections = length(pauses_ns)
    have_phase = use_regions && !isempty(pauses_ns)
    stw_us = have_phase ? stats[2] / length(pauses_ns) / 1e3 : "NA"
    mark_us = have_phase ? stats[3] / length(pauses_ns) / 1e3 : "NA"
    sweep_us = have_phase ? stats[4] / length(pauses_ns) / 1e3 : "NA"
    live_cells = have_phase ? stats[5] ÷ length(pauses_ns) : "NA"
    freed_cells = have_phase ? stats[6] ÷ length(pauses_ns) : "NA"
    pause_p50 = isempty(pauses_ns) ? "NA" : pauses_ns[(end + 1) ÷ 2] / 1e6
    pause_max = isempty(pauses_ns) ? "NA" : pauses_ns[end] / 1e6
    tsv_row(("variant", "K", "W", "B", "every", "events", "collections", "stw_us", "mark_us",
             "sweep_us", "live_cells", "freed_cells", "pause_p50_ms", "pause_max_ms", "wall_s",
             "events_per_s", "p50_ns", "p99_ns", "p999_ns", "p9999_ns", "max_ns", "max_clean_ns",
             "over_100us", "switches", "page_faults", "stock_collections", "peak_rss_mb"),
            (variant, K, W, B, every, events, collections, stw_us, mark_us, sweep_us,
             live_cells, freed_cells, pause_p50, pause_max, wall_s, events / wall_s,
             record_latency ? q(0.5) : "NA", record_latency ? q(0.99) : "NA",
             record_latency ? q(0.999) : "NA", record_latency ? q(0.9999) : "NA",
             record_latency ? lat[end] : "NA", record_latency ? clean_max : "NA",
             record_latency ? over : "NA",
             ctx1 - ctx0, flt1 - flt0, stock_collections_val, peak_rss_val))
end

main()
