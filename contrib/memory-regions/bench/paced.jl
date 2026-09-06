# The paced hardware-in-the-loop run.
#
#   julia --heap-size-hint=128M paced.jl baseline 1000000
#   julia paced.jl regions 1000000
#
# One event fires per 100 us slot, on the wall clock: the loop spins to the
# slot deadline, processes the event, and (regions) resets the Event region.
# The lateness column is what the hardware feels: how late an event COMPLETES
# relative to its slot start. A collector pause makes the events behind it
# late; a miss is an event that completes after its whole slot has passed.
#
# `baseline` is the allocating model with the collector ON — pass a heap
# hint so the collector actually fires inside the run, the way a pinned
# hardware box bounds memory. `regions` is the scratch model with the
# collector off: reset is the only collector.

include(joinpath(@__DIR__, "kernel.jl"))
using .MiniKernel
include(joinpath(@__DIR__, "..", "regions.jl"))
using .Regions
include(joinpath(@__DIR__, "model_alloc.jl"))
include(joinpath(@__DIR__, "model_scratch.jl"))
include(joinpath(@__DIR__, "report.jl"))

const PERIOD_NS = 100_000

function percentile(sorted::Vector{Int64}, p::Float64)
    isempty(sorted) && return 0
    sorted[clamp(ceil(Int, p * length(sorted)), 1, length(sorted))]
end

function run_paced!(network, use_regions::Bool, latencies_ns::Vector{Int64},
                    late_ns::Vector{Int64}, gc_ns::Vector{Int64})
    limit = length(latencies_ns)
    count = 0
    t0 = time_ns() + 1_000_000              # the first slot starts 1 ms out
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
        deadline = t0 + UInt64(count - 1) * UInt64(PERIOD_NS)
        while time_ns() < deadline; end     # spin to the slot start
        gc0 = Base.gc_num().total_time
        t1 = time_ns()
        event.action(network, event.environment)
        use_regions && unsafe_region_reset(1)
        t2 = time_ns()
        gc1 = Base.gc_num().total_time
        @inbounds latencies_ns[count] = Int64(t2 - t1)
        @inbounds late_ns[count] = Int64(t2) - Int64(deadline)
        @inbounds gc_ns[count] = Int64(gc1 - gc0)
    end
    return count
end

function report(label, latencies, late_ns, gc_ns, count)
    lat = sort!(latencies[1:count])
    late = sort!(late_ns[1:count])
    misses = count - searchsortedlast(late, Int64(PERIOD_NS))
    gc_events = count - Base.count(iszero, view(gc_ns, 1:count))
    gc_total = sum(view(gc_ns, 1:count))
    println("=== ", label, " (paced, one event per 100 us slot) ===")
    println("events            ", count)
    println("latency p50       ", percentile(lat, 0.50), " ns")
    println("latency p99.9     ", percentile(lat, 0.999), " ns")
    println("latency max       ", lat[end], " ns")
    println("lateness p99.9    ", percentile(late, 0.999), " ns")
    println("lateness max      ", late[end], " ns")
    println("slot misses       ", misses, " events (",
            round(misses / count * 100; sigdigits = 3), " %)")
    println("gc events         ", gc_events)
    println("gc total          ", round(gc_total / 1e6; digits = 1), " ms")
end

function main()
    variant = ARGS[1]
    events = parse(Int, ARGS[2])
    relays = 4
    # zeros, not undef: the result vectors are touched here, before the
    # loop. An untouched page faults in at its first store, and with
    # transparent huge pages that first store zeroes 2 MB (about 200 us):
    # the store of one event's result then delays the next slot.
    latencies = zeros(Int64, events)
    late_ns = zeros(Int64, events)
    gc_ns = zeros(Int64, events)
    deliveries = events ÷ (relays + 3) + 1

    warm_n = min(10_000, events)
    warm_lat = Vector{Int64}(undef, warm_n)
    warm_gc = Vector{Int64}(undef, warm_n)

    if variant == "baseline"
        network, sink = ModelAlloc.build(relays)
        run_measured!(network, warm_lat, warm_gc)      # compile, unpaced
        GC.gc()
        count = run_paced!(network, false, latencies, late_ns, gc_ns)
    elseif variant == "regions"
        network, sink = ModelScratch.build(relays, deliveries; use_regions = true)
        warm_late = Vector{Int64}(undef, warm_n)
        run_paced!(network, true, warm_lat, warm_late, warm_gc)
        unsafe_region_reset(1)
        # No collection happens once region pages exist. The report path
        # compiles here, before the measured phase.
        report("warm", warm_lat, warm_late, warm_gc, warm_n)
        GC.enable(false)
        count = run_paced!(network, true, latencies, late_ns, gc_ns)
        println("region pages      ", region_pages(1))
    else
        error("variant must be baseline or regions")
    end
    report(variant, latencies, late_ns, gc_ns, count)
    println("live heap         ", round(Base.gc_live_bytes() / 1e6; digits = 1), " MB")

    let lat = sort!(latencies[1:count])
        late = sort!(late_ns[1:count])
        misses = count - searchsortedlast(late, Int64(PERIOD_NS))
        gc_events = count - Base.count(iszero, view(gc_ns, 1:count))
        gc_total = sum(view(gc_ns, 1:count))
        tsv_row(("variant", "events", "latency_p50_ns", "latency_p999_ns", "latency_max_ns",
                 "lateness_p999_ns", "lateness_max_ns", "slot_misses", "gc_events", "gc_ms"),
                (variant, count, percentile(lat, 0.50), percentile(lat, 0.999), lat[end],
                 percentile(late, 0.999), late[end], misses, gc_events, gc_total / 1e6))
    end
end

main()
