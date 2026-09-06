# The tail latencies of the pooled model, baseline against regions.
#
#   julia tail.jl baseline 20000000
#   julia tail.jl regions  20000000
#
# Both run the SAME model: pooled messages (the in-flight lifetime), isbits
# results in pre-sized columns, and ordinary allocating scratch in the sink.
# `baseline` leaves the scratch to the collector. `regions` allocates each
# event under the Event region and resets it after the event; the region
# swap and the reset are INSIDE the measured window, so their cost is paid
# where it occurs. The collector is off during the measured phase, and the
# run proves it could stay off: nothing accumulates.

include(joinpath(@__DIR__, "kernel.jl"))
using .MiniKernel
include(joinpath(@__DIR__, "..", "regions.jl"))
using .Regions
include(joinpath(@__DIR__, "model_scratch.jl"))
include(joinpath(@__DIR__, "report.jl"))

const TARGET_NS = 100_000

function percentile(sorted::Vector{Int64}, p::Float64)
    isempty(sorted) && return 0
    sorted[clamp(ceil(Int, p * length(sorted)), 1, length(sorted))]
end

function run_regions!(network, latencies_ns::Vector{Int64}, gc_ns::Vector{Int64})
    limit = length(latencies_ns)
    count = 0
    while !isempty(network.queue)
        count == limit && return count
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
        gc0 = Base.gc_num().total_time
        t0 = time_ns()
        event.action(network, event.environment)   # the model scopes region 1
        unsafe_region_reset(1)
        t1 = time_ns()
        gc1 = Base.gc_num().total_time
        @inbounds latencies_ns[count] = Int64(t1 - t0)
        @inbounds gc_ns[count] = Int64(gc1 - gc0)
    end
    return count
end

function report(label, latencies, gc_ns, count)
    lat = sort!(latencies[1:count])
    over = count - searchsortedlast(lat, TARGET_NS)
    gc_events = count - Base.count(iszero, view(gc_ns, 1:count))
    gc_total = sum(view(gc_ns, 1:count))
    println("=== ", label, " ===")
    println("events            ", count)
    println("p50               ", percentile(lat, 0.50), " ns")
    println("p99               ", percentile(lat, 0.99), " ns")
    println("p99.9             ", percentile(lat, 0.999), " ns")
    println("p99.99            ", percentile(lat, 0.9999), " ns")
    println("max               ", lat[end], " ns")
    println("over 100us target ", over, " events (",
            round(over / count * 100; sigdigits = 3), " %)")
    println("gc events         ", gc_events)
    println("gc total          ", round(gc_total / 1e6; digits = 1), " ms")
end

function main()
    variant = ARGS[1]
    events = parse(Int, ARGS[2])
    relays = 4
    # zeros, not undef: the result vectors are touched before the loop, so
    # no store of a result faults a page in (with transparent huge pages a
    # first store zeroes 2 MB, about 200 us).
    latencies = zeros(Int64, events)
    gc_ns = zeros(Int64, events)
    deliveries = events ÷ (relays + 3) + 1

    network, sink = ModelScratch.build(relays, deliveries;
                                      use_regions = variant == "regions")

    # The warm run compiles everything, then the buffers reset.
    warm_lat = Vector{Int64}(undef, min(10_000, events))
    warm_gc = Vector{Int64}(undef, min(10_000, events))
    if variant == "baseline"
        run_measured!(network, warm_lat, warm_gc)
        GC.gc()
        count = run_measured!(network, latencies, gc_ns)
    elseif variant == "regions"
        run_regions!(network, warm_lat, warm_gc)
        unsafe_region_reset(1)
        # Once region pages exist, the collector must not run again in this
        # process: reset is the region pages' only collector. The warm phase
        # is small, so nothing needs collecting; the report path is compiled
        # here so nothing compiles after the measured phase either.
        report("warm", warm_lat, warm_gc, min(10_000, events))
        GC.enable(false)
        count = run_regions!(network, latencies, gc_ns)
        println("region pages      ", region_pages(1))
    else
        error("variant must be baseline or regions")
    end
    report(variant, latencies, gc_ns, count)
    println("live heap         ", round(Base.gc_live_bytes() / 1e6; digits = 1), " MB")

    let lat = sort!(latencies[1:count])
        over = count - searchsortedlast(lat, TARGET_NS)
        gc_events = count - Base.count(iszero, view(gc_ns, 1:count))
        gc_total = sum(view(gc_ns, 1:count))
        tsv_row(("script", "variant", "events", "p50_ns", "p99_ns", "p999_ns", "p9999_ns",
                 "max_ns", "over_100us", "gc_events", "gc_ms", "peak_rss_mb"),
                ("tail", variant, count, percentile(lat, 0.50), percentile(lat, 0.99),
                 percentile(lat, 0.999), percentile(lat, 0.9999), lat[end], over,
                 gc_events, gc_total / 1e6, Sys.maxrss() / 1e6))
    end
end

main()
