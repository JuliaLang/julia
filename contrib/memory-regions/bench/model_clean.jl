# The DISCIPLINED model — the same traffic as model_alloc, written under the
# region rules. The messages are in flight between events, so the packet and
# everything it carries is allocated in the SIMULATION region; only true
# transients — scratch that dies inside the handler — stay in the EVENT
# region. The checker must report zero violations, except the known container
# growth case, which the run avoids by pre-sizing the queue.

module ModelClean

using ..MiniKernel
using ..RegionCheck: RegionCheck, @in_region
import ..SIMULATION           # the example's chain, defined in checker_run.jl

mutable struct Packet <: EventEnvironment
    id::Int
    payload::Vector{Float64}
end

struct Sample                     # isbits: its stores embed no reference
    id::Int
    time::Int
    value::Float64
end

mutable struct Source <: AbstractModule
    id::Int
    sent::Int
    out::Any
    self::Any
end

mutable struct Relay <: AbstractModule
    id::Int
    out::Any
end

mutable struct Sink <: AbstractModule
    id::Int
    results::Vector{Sample}
end

function MiniKernel.handle_message!(net, m::Source, env, gate)
    m.sent += 1
    # The packet is IN FLIGHT after this handler returns, and the event queue
    # entry references it: both belong to the Simulation region.
    @in_region SIMULATION begin
        packet = Packet(m.sent, [1.0, 2.0, 3.0])
        send!(net, m.out::Gate, packet)
        send!(net, m.self::Gate, nothing)
    end
    nothing
end

MiniKernel.handle_message!(net, m::Relay, env, gate) =
    @in_region SIMULATION (send!(net, m.out::Gate, env); nothing)

function MiniKernel.handle_message!(net, m::Sink, env, gate)
    packet = env::Packet
    # A TRUE transient: scratch allocated in the Event region, dead before the
    # handler returns, referenced by nothing older.
    scratch = [packet.payload[1], packet.payload[2], packet.payload[3]]
    push!(m.results, Sample(packet.id, net.time, sum(scratch)))
    nothing
end

function build(relays::Int)
    sink = Sink(0, sizehint!(Sample[], 1 << 20))
    gate = Gate(sink)
    for i in 1:relays
        relay = Relay(i, gate)
        gate = Gate(relay)
    end
    network = Network()
    sizehint!(network.queue, 1 << 12)      # pre-size: no growth from Event
    source = Source(1000, 0, nothing, nothing)
    source_gate = Gate(source)
    source.out = gate
    source.self = source_gate
    send!(network, source_gate, nothing)
    return network, sink
end

end # module ModelClean
