# The POOLED model — the benefit upper bound. The same traffic and
# the same kept information, with every per-event allocation removed by hand:
# one reused packet, results in preallocated columns. This is what a perfect
# Event region delivers, minus the discipline.

module ModelPooled

using ..MiniKernel

mutable struct Packet <: EventEnvironment
    id::Int
    p1::Float64                   # the payload, inline instead of a Vector
    p2::Float64
    p3::Float64
end

mutable struct Source <: AbstractModule
    id::Int
    sent::Int
    out::Any
    self::Any
    packet::Packet                # the ONE packet, reused every send
end

mutable struct Relay <: AbstractModule
    id::Int
    out::Any
end

mutable struct Sink <: AbstractModule
    id::Int
    filled::Int                   # preallocated result columns
    ids::Vector{Int}
    times::Vector{Int}
    values::Vector{Float64}
end

function MiniKernel.handle_message!(net, m::Source, env, gate)
    m.sent += 1
    packet = m.packet
    packet.id = m.sent
    packet.p1 = 1.0; packet.p2 = 2.0; packet.p3 = 3.0
    send!(net, m.out::Gate, packet)
    send!(net, m.self::Gate, nothing)
    nothing
end

MiniKernel.handle_message!(net, m::Relay, env, gate) =
    (send!(net, m.out::Gate, env); nothing)

function MiniKernel.handle_message!(net, m::Sink, env, gate)
    packet = env::Packet
    i = m.filled + 1
    i > length(m.ids) && return nothing        # columns full: drop, keep running
    m.filled = i
    @inbounds m.ids[i] = packet.id
    @inbounds m.times[i] = net.time
    @inbounds m.values[i] = packet.p1 + packet.p2 + packet.p3
    nothing
end

function build(relays::Int, capacity::Int)
    sink = Sink(0, 0, Vector{Int}(undef, capacity),
                Vector{Int}(undef, capacity), Vector{Float64}(undef, capacity))
    gate = Gate(sink)
    for i in 1:relays
        relay = Relay(i, gate)
        gate = Gate(relay)
    end
    network = Network()
    source = Source(1000, 0, nothing, nothing, Packet(0, 0.0, 0.0, 0.0))
    source_gate = Gate(source)
    source.out = gate
    source.self = source_gate
    send!(network, source_gate, nothing)
    return network, sink
end

end # module ModelPooled
