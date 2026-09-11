# A region-NATIVE model -- region primitives only, no pooling.
#
#   julia native.jl region 5000000 [census_every] [W]
#   julia native.jl stock  5000000 [census_every] [W]
#
# The dyna-shaped question: packets (with a payload, the INET-chunk-like
# weight) are allocated NATURALLY at send and dropped at delivery -- no
# pool, no reuse, no ownership bookkeeping. The whole simulation -- the
# network, the event queue, and every in-flight packet -- lives in ONE
# region (SIM). One window spans the whole run loop, so the window tax is
# per ask, not per event. Every `census_every` events the driver closes
# the window, runs the cooperative census, and reopens: the census marks
# the live graph (network -> queue -> in-flight packets) from the
# driver's stack and reclaims everything delivered since -- dead packets,
# dead payloads, dead scratch alike. `stock` is the identical model under
# the ordinary collector.

region_set(n)     = ccall(:jl_gc_region_set, Cint, (Cint,), n)
region_coop(n)    = ccall(:jl_gc_region_collect_coop, Int64, (Cint,), n)
region_verify(n)  = ccall(:jl_gc_region_verify, Cint, (Cint,), n)
region_stat(i)    = ccall(:jl_gc_region_stat, UInt64, (Cint,), i)

include(joinpath(@__DIR__, "report.jl"))

const SIM = 1
const W = length(ARGS) >= 4 ? parse(Int, ARGS[4]) : 3

mutable struct FlightPacket
    id::Int
    hops::Int
    payload::Vector{Float64}
end

mutable struct Event6
    time::Int
    sequence::Int
    target::Int              # 0 = source self-tick; 1..R relays; R+1 sink
    packet::Union{Nothing,FlightPacket}
end

mutable struct Net6
    time::Int
    sequence::Int
    relays::Int
    sent::Int
    received::Int
    checksum::Float64
    queue::Vector{Event6}
end

function build(relays::Int)
    net = Net6(0, 0, relays, 0, 0, 0.0, sizehint!(Event6[], 1024))
    push_event!(net, 1, 0, nothing)
    return net
end

function push_event!(net::Net6, time::Int, target::Int, packet)
    net.sequence += 1
    push!(net.queue, Event6(time, net.sequence, target, packet))
    return nothing
end

@noinline function handle!(net::Net6, evt::Event6)
    if evt.target == 0
        # The source: a fresh packet with a fresh payload, no pool.
        net.sent += 1
        payload = Vector{Float64}(undef, W)
        @inbounds for j in 1:W
            payload[j] = Float64(net.sent + j)
        end
        pkt = FlightPacket(net.sent, 0, payload)
        push_event!(net, net.time + 1, 1, pkt)          # into flight
        push_event!(net, net.time + 1, 0, nothing)      # self-tick
    elseif evt.target <= net.relays
        pkt = evt.packet::FlightPacket
        pkt.hops += 1
        push_event!(net, net.time + 1, evt.target + 1, pkt)
    else
        # The sink: read, account, DROP -- the packet becomes garbage.
        pkt = evt.packet::FlightPacket
        net.received += 1
        net.checksum += pkt.payload[1] + pkt.payload[end] + pkt.hops
    end
    return nothing
end

function run!(net::Net6, events::Int, census_every::Int, use_region::Bool,
              pauses_ns::Vector{Int64})
    count = 0
    freed_total = Int64(0)
    use_region && region_set(SIM)
    while count < events
        best = 1
        @inbounds for i in 2:length(net.queue)
            e, top = net.queue[i], net.queue[best]
            (e.time < top.time || (e.time == top.time && e.sequence < top.sequence)) &&
                (best = i)
        end
        evt = net.queue[best]
        deleteat!(net.queue, best)
        net.time = evt.time
        count += 1
        handle!(net, evt)
        if use_region && count % census_every == 0
            region_set(0)
            c0 = time_ns()
            f = region_coop(SIM)
            c1 = time_ns()
            f < 0 && error("census failed: ", f)
            freed_total += f
            push!(pauses_ns, Int64(c1 - c0))
            region_verify(SIM) == 0 || error("verify failed at event ", count)
            region_set(SIM)
        end
    end
    use_region && region_set(0)
    return freed_total
end

function main()
    variant = ARGS[1]
    events = parse(Int, ARGS[2])
    census_every = length(ARGS) >= 3 ? parse(Int, ARGS[3]) : 100_000
    use_region = variant == "region"

    # Warm on a throwaway net; then quiesce and, under regions, stop the
    # ordinary collector: the census is the only collector of SIM.
    let warm = (use_region && region_set(SIM); w = build(4); use_region && region_set(0); w)
        run!(warm, 20_000, census_every, use_region, Int64[])
    end
    if use_region
        region_coop(SIM)
        GC.gc()
        GC.enable(false)
    end

    use_region && region_set(SIM)
    net = build(4)
    use_region && region_set(0)
    pauses = Int64[]
    t0 = time_ns()
    freed = run!(net, events, census_every, use_region, pauses)
    t1 = time_ns()
    wall = (t1 - t0) / 1e9

    println("=== ", variant, " (payload ", W, " floats, census every ",
            census_every, ") ===")
    println("wall time          ", round(wall; digits = 3), " s (",
            round(events / wall / 1e6; digits = 2), " M events/s)")
    println("sent/received      ", net.sent, " / ", net.received)
    println("checksum           ", net.checksum)
    if use_region
        sort!(pauses)
        println("censuses           ", length(pauses))
        isempty(pauses) || println("census p50         ",
            round(pauses[(end + 1) ÷ 2] / 1e3; digits = 1), " us")
        isempty(pauses) || println("census max         ",
            round(pauses[end] / 1e3; digits = 1), " us")
        println("cells freed        ", freed)
        println("live at last census: ", region_stat(4), " cells, ",
                region_stat(6), " pages walked, ", region_stat(7), " wholesale")
    end
    println("live heap counter  ", round(Base.gc_live_bytes() / 1e6; digits = 1), " MB")

    tsv_row(("variant", "events", "W", "census_every", "wall_s", "events_per_s", "censuses",
             "census_p50_ms", "census_max_ms", "cells_freed", "peak_rss_mb"),
            (variant, events, W, census_every, wall, events / wall,
             use_region ? length(pauses) : "NA",
             use_region && !isempty(pauses) ? pauses[(end + 1) ÷ 2] / 1e6 : "NA",
             use_region && !isempty(pauses) ? pauses[end] / 1e6 : "NA",
             use_region ? freed : "NA",
             Sys.maxrss() / 1e6))
end

main()
