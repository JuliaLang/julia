# The yardstick. Runs one model variant, measures every event, prints the
# latency distribution against the 100 microsecond pacing target.
#
#   julia yardstick.jl alloc  <events>
#   julia yardstick.jl pooled <events>

include(joinpath(@__DIR__, "kernel.jl"))
using .MiniKernel
include(joinpath(@__DIR__, "model_alloc.jl"))
include(joinpath(@__DIR__, "model_pooled.jl"))
include(joinpath(@__DIR__, "report.jl"))

const TARGET_NS = 100_000                # the pacing target: 100 microseconds

function percentile(sorted::Vector{Int64}, p::Float64)
    isempty(sorted) && return 0
    sorted[clamp(ceil(Int, p * length(sorted)), 1, length(sorted))]
end

function report(label, latencies, gc_ns, count, kept_bytes)
    lat = sort!(latencies[1:count])
    over = count - searchsortedlast(lat, TARGET_NS)
    gc_events = count - Base.count(iszero, view(gc_ns, 1:count))
    gc_total = sum(view(gc_ns, 1:count))
    println("=== ", label, " ===")
    println("events            ", count)
    println("kept bytes        ", kept_bytes)
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
    deliveries = events ÷ (relays + 3) + 1      # sink capacity bound

    if variant == "alloc"
        network, sink = ModelAlloc.build(relays)
    elseif variant == "pooled"
        network, sink = ModelPooled.build(relays, deliveries)
    else
        error("variant must be alloc or pooled")
    end

    # The warm run: compile everything before the measured phase, then reset
    # the buffers. Without it the first events measure the JIT, not the model.
    warm = min(10_000, events)
    run_measured!(network, view_buffer(latencies, warm), view_buffer(gc_ns, warm))
    GC.gc()

    count = run_measured!(network, latencies, gc_ns)
    kept = variant == "alloc" ? Base.summarysize(sink.results) :
           Base.summarysize(sink.ids) + Base.summarysize(sink.times) +
           Base.summarysize(sink.values)
    report(variant, latencies, gc_ns, count, kept)

    let lat = sort!(latencies[1:count])
        over = count - searchsortedlast(lat, TARGET_NS)
        gc_events = count - Base.count(iszero, view(gc_ns, 1:count))
        gc_total = sum(view(gc_ns, 1:count))
        tsv_row(("script", "variant", "events", "p50_ns", "p99_ns", "p999_ns", "p9999_ns",
                 "max_ns", "over_100us", "gc_events", "gc_ms", "peak_rss_mb"),
                ("yardstick", variant, count, percentile(lat, 0.50), percentile(lat, 0.99),
                 percentile(lat, 0.999), percentile(lat, 0.9999), lat[end], over,
                 gc_events, gc_total / 1e6, Sys.maxrss() / 1e6))
    end
end

# A resizable warm buffer without reallocating the main ones.
view_buffer(v, n) = Vector{Int64}(undef, n)

main()
