# The ALLOCATING model — the baseline. The traffic is a self-rescheduling
# source feeding a relay chain that ends in a recording sink. The sink KEEPS a
# record per delivery, the way the recording path does, so the live heap grows
# through the run and the collector has real work. The source and the sink
# also make transient garbage, the way a model temporary does.

module ModelAlloc

using ..MiniKernel

mutable struct Packet <: EventEnvironment
    id::Int
    payload::Vector{Float64}      # transient garbage, dies at the sink
end

struct Sample                     # KEPT: one record per delivery
    id::Int
    time::Int
    value::Float64
end

mutable struct Source <: AbstractModule
    id::Int
    sent::Int
    out::Any                      # Gate into the relay chain
    self::Any                     # Gate back to itself
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
    packet = Packet(m.sent, [1.0, 2.0, 3.0])          # allocates, dies later
    send!(net, m.out::Gate, packet)
    send!(net, m.self::Gate, nothing)                  # keep the run going
    nothing
end

MiniKernel.handle_message!(net, m::Relay, env, gate) =
    (send!(net, m.out::Gate, env); nothing)

function MiniKernel.handle_message!(net, m::Sink, env, gate)
    packet = env::Packet
    value = packet.payload[1] + packet.payload[2] + packet.payload[3]
    push!(m.results, Sample(packet.id, net.time, value))   # KEPT
    nothing
end

"""Build the network: source → relays → sink, and the source ticks itself."""
function build(relays::Int)
    sink = Sink(0, Sample[])
    gate = Gate(sink)
    for i in 1:relays
        relay = Relay(i, gate)
        gate = Gate(relay)
    end
    network = Network()
    source = Source(1000, 0, nothing, nothing)
    source_gate = Gate(source)
    source.out = gate
    source.self = source_gate
    send!(network, source_gate, nothing)
    return network, sink
end

end # module ModelAlloc
