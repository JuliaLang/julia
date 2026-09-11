# The endurance run: does memory stay flat over a long paced run?
#
#   julia endurance.jl 18000000     # 30 min at 100 us
#
# The scratch model under regions, paced at one event per 100 us slot, with
# the collector off. The driver keeps NO per-event buffers: latencies go
# into a fixed log2 histogram, so the harness itself cannot grow, and any
# RSS growth is a real leak. Every 100k events (10 s) one sample records
# wall time, Sys.maxrss (high-water RSS), gc_live_bytes (the ACCOUNTING
# counter: region allocations count in, resets never subtract, so its slope
# is the allocation THROUGHPUT that reset recycles -- not a leak), the
# pages the reset returned, and the miss count. The high-water RSS stops
# growing after warmup, so the collector is never needed on the disciplined
# path, for arbitrarily long runs.

include(joinpath(@__DIR__, "kernel.jl"))
using .MiniKernel
include(joinpath(@__DIR__, "..", "regions.jl"))
using .Regions
include(joinpath(@__DIR__, "model_scratch.jl"))
include(joinpath(@__DIR__, "report.jl"))

const PERIOD_NS = 100_000
const SAMPLE_EVERY = 100_000

# log2 histogram of latency: bucket i holds counts for [2^(i-1), 2^i) ns.
hist_index(ns::Int64) = ns <= 0 ? 1 : min(64, 65 - leading_zeros(UInt64(ns)))

function hist_quantile(hist::Vector{Int64}, total::Int64, q::Float64)
    want = ceil(Int64, q * total)
    run = Int64(0)
    for i in 1:64
        run += hist[i]
        run >= want && return Int64(1) << (i - 1)   # bucket upper bound
    end
    return Int64(1) << 63
end

function run_paced!(network, events::Int, hist::Vector{Int64},
                    sample_t_s, sample_rss_mb, sample_live_mb,
                    sample_pages, sample_misses)
    count = 0
    misses = 0
    nsamples = 0
    latmax = Int64(0)
    latemax = Int64(0)
    t0 = time_ns() + 1_000_000
    while !isempty(network.queue)
        count == events && break
        best = 1
        @inbounds for i in 2:length(network.queue)
            event, top = network.queue[i], network.queue[best]
            (event.time < top.time ||
             (event.time == top.time && event.sequence < top.sequence)) && (best = i)
        end
        event = network.queue[best]
        deleteat!(network.queue, best)
        network.time = event.time
        count += 1
        deadline = t0 + UInt64(count - 1) * UInt64(PERIOD_NS)
        while time_ns() < deadline; end
        t1 = time_ns()
        event.action(network, event.environment)
        pages = unsafe_region_reset(1)
        t2 = time_ns()
        lat = Int64(t2 - t1)
        @inbounds hist[hist_index(lat)] += 1
        lat > latmax && (latmax = lat)
        late = Int64(t2) - Int64(deadline)
        late > latemax && (latemax = late)
        late > PERIOD_NS && (misses += 1)
        if count % SAMPLE_EVERY == 0
            nsamples += 1
            @inbounds sample_t_s[nsamples] = (t2 - t0) / 1e9
            @inbounds sample_rss_mb[nsamples] = Sys.maxrss() / 1e6
            @inbounds sample_live_mb[nsamples] = Base.gc_live_bytes() / 1e6
            @inbounds sample_pages[nsamples] = Int64(pages)
            @inbounds sample_misses[nsamples] = misses
        end
    end
    return count, nsamples, latmax, latemax, misses
end

function main()
    events = parse(Int, ARGS[1])
    relays = 4
    nmax = events ÷ SAMPLE_EVERY + 1
    sample_t_s = Vector{Float64}(undef, nmax)
    sample_rss_mb = Vector{Float64}(undef, nmax)
    sample_live_mb = Vector{Float64}(undef, nmax)
    sample_pages = Vector{Int64}(undef, nmax)
    sample_misses = Vector{Int64}(undef, nmax)
    deliveries = events ÷ (relays + 3) + 1
    hist = zeros(Int64, 64)

    network, sink = ModelScratch.build(relays, deliveries; use_regions = true)

    # Pre-touch the result columns: they are legitimate kept data that the
    # sink fills row by row, and an untouched page joining the RSS would
    # read as growth. After this, the RSS of the measured phase must be
    # FLAT, and any growth is a real leak.
    fill!(sink.ids, 0)
    fill!(sink.values, 0.0)

    # Warm: compile everything, then quiesce and switch the collector off.
    run_paced!(network, 10_000, hist,
               sample_t_s, sample_rss_mb, sample_live_mb,
               sample_pages, sample_misses)
    unsafe_region_reset(1)
    GC.gc()                        # legal: quiesced, no window open
    GC.enable(false)
    fill!(hist, 0)

    count, nsamples, latmax, latemax, misses =
        run_paced!(network, events, hist,
                   sample_t_s, sample_rss_mb, sample_live_mb,
                   sample_pages, sample_misses)

    println("=== endurance samples (every ", SAMPLE_EVERY, " events) ===")
    println("t[s]  rss[MB]  live[MB]  reset_pages  misses")
    for i in 1:nsamples
        println(round(sample_t_s[i]; digits = 1), "  ",
                round(sample_rss_mb[i]; digits = 1), "  ",
                round(sample_live_mb[i]; digits = 1), "  ",
                sample_pages[i], "  ", sample_misses[i])
        tsv_row(("t_s", "rss_mb", "live_mb", "reset_pages", "misses"),
                (sample_t_s[i], sample_rss_mb[i], sample_live_mb[i], sample_pages[i], sample_misses[i]))
    end
    total = Int64(count)
    println("=== endurance result ===")
    println("events            ", count)
    println("duration          ", round(count * PERIOD_NS / 1e9; digits = 1), " s")
    println("latency p50       <= ", hist_quantile(hist, total, 0.50), " ns (log2 bucket)")
    println("latency p99.9     <= ", hist_quantile(hist, total, 0.999), " ns (log2 bucket)")
    println("latency max       ", latmax, " ns")
    println("lateness max      ", latemax, " ns")
    println("slot misses       ", misses, " events (",
            round(misses / count * 100; sigdigits = 3), " %)")
    if nsamples >= 4
        half = nsamples ÷ 2
        rss_growth = sample_rss_mb[nsamples] - sample_rss_mb[half]
        thru = (sample_live_mb[nsamples] - sample_live_mb[half]) * 1e6 /
               ((nsamples - half) * SAMPLE_EVERY)
        println("rss growth 2nd half     ", round(rss_growth; digits = 1), " MB")
        println("alloc throughput        ", round(thru; digits = 1),
                " bytes/event (recycled by reset; accounting slope)")
    end
end

main()
