# The minimal sequential kernel: modules deliver messages to each other
# through gates, and an event queue orders the deliveries by time. The
# kernel never learns which module kinds exist.

module MiniKernel

export AbstractModule, EventEnvironment, Gate, Network,
       send!, run_measured!, handle_message!

abstract type AbstractModule end
abstract type EventEnvironment end

mutable struct Gate <: Function
    owner::Any          # the AbstractModule this gate delivers to
end

struct Event
    action::Gate
    environment::Union{Nothing,EventEnvironment}
    time::Int
    sequence::Int
end

function handle_message! end

@inline (gate::Gate)(network, environment) =
    handle_message!(network, gate.owner::AbstractModule, environment, gate)

mutable struct Network
    time::Int
    sequence::Int
    queue::Vector{Event}
end
Network() = Network(0, 0, sizehint!(Event[], 1024))

function push_event!(network::Network, time::Int, action::Gate, environment)
    network.sequence += 1
    push!(network.queue, Event(action, environment, time, network.sequence))
    nothing
end

function send!(network::Network, gate::Gate, environment)
    push_event!(network, network.time + 1, gate, environment)
    nothing
end

"""
Run the queue and measure every event. `latencies_ns[i]` takes the wall time
of event `i`; `gc_ns[i]` takes the collector time that fell into event `i`.
Both vectors are preallocated by the caller, so the harness itself allocates
nothing per event. Returns the number of events processed.
"""
function run_measured!(network::Network, latencies_ns::Vector{Int64},
                       gc_ns::Vector{Int64})
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
        event.action(network, event.environment)
        t1 = time_ns()
        gc1 = Base.gc_num().total_time
        @inbounds latencies_ns[count] = Int64(t1 - t0)
        @inbounds gc_ns[count] = Int64(gc1 - gc0)
    end
    return count
end

end # module MiniKernel
