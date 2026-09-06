# A model of ordinary allocating code around a pooled message. The message
# is pooled, because its lifetime is IN FLIGHT, not one event; the kept
# results are isbits in pre-sized columns, which is region-clean by
# construction. What remains per event is ORDINARY ALLOCATING CODE — the sink
# builds real temporary vectors and drops them. The point is that such code
# needs no hand-pooling: the Event region reset reclaims it.

module ModelScratch

using ..MiniKernel

mutable struct Packet <: EventEnvironment
    id::Int
    p1::Float64
    p2::Float64
    p3::Float64
end

mutable struct Source <: AbstractModule
    id::Int
    sent::Int
    out::Any
    self::Any
    packet::Packet                # pooled: in-flight lifetime, region 0
end

mutable struct Relay <: AbstractModule
    id::Int
    out::Any
end

mutable struct Sink <: AbstractModule
    id::Int
    use_regions::Bool             # the ONE difference between the two runs
    filled::Int
    ids::Vector{Int}
    values::Vector{Float64}
end

@inline _region_set(n) = ccall(:jl_gc_region_set, Cint, (Cint,), n)

# The scratch must REALLY allocate: without an opaque consumer LLVM elides
# the arrays entirely (the endurance run caught the reset walking zero
# pages), and the benchmark then never exercises region reclamation.
@noinline _touch(v::Vector{Float64}) = (isempty(v) && error("empty"); nothing)

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
    # The per-event transients: ordinary code, no pooling. All of it is
    # garbage before the handler returns. THE MODEL marks the transient
    # section, because the model is what knows the lifetime — a region around
    # the whole event would capture the in-flight Event records that send!
    # pushes, which is a violation the checker flags.
    m.use_regions && _region_set(1)
    scratch = [packet.p1, packet.p2, packet.p3]
    weights = collect(1.0:4.0)
    _touch(scratch)
    _touch(weights)
    acc = 0.0
    for w in weights
        acc += w * (scratch[1] + scratch[2] + scratch[3])
    end
    m.use_regions && _region_set(0)
    i = m.filled + 1
    i > length(m.ids) && return nothing
    m.filled = i
    @inbounds m.ids[i] = packet.id
    @inbounds m.values[i] = acc
    nothing
end

function build(relays::Int, capacity::Int; use_regions::Bool = false)
    sink = Sink(0, use_regions, 0, Vector{Int}(undef, capacity), Vector{Float64}(undef, capacity))
    gate = Gate(sink)
    for i in 1:relays
        relay = Relay(i, gate)
        gate = Gate(relay)
    end
    network = Network()
    sizehint!(network.queue, 1 << 12)
    source = Source(1000, 0, nothing, nothing, Packet(0, 0.0, 0.0, 0.0))
    source_gate = Gate(source)
    source.out = gate
    source.self = source_gate
    send!(network, source_gate, nothing)
    return network, sink
end

end # module ModelScratch
