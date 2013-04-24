
using Base.Collections


# Test dequeing in sorted order.
function test_issorted!(pq::PriorityQueue, priorities)
    last = dequeue!(pq)
    while !isempty(pq)
        value = dequeue!(pq)
        @test priorities[last] <= priorities[value]
        value = last
    end
end

pmax = 1000
n = 10000
priorities = Dict(1:n, rand(1:pmax, n))

# building from a dict
pq = PriorityQueue(priorities)
test_issorted!(pq, priorities)


# enqueing via enqueue!
pq = PriorityQueue()
for (k, v) in priorities
    enqueue!(pq, k, v)
end
test_issorted!(pq, priorities)


# enqueing via assign
pq = PriorityQueue()
for (k, v) in priorities
    pq[k] = v
end
test_issorted!(pq, priorities)


# changing priorities
pq = PriorityQueue()
for (k, v) in priorities
    pq[k] = v
end

for _ in 1:n
    k = rand(1:n)
    v = rand(1:pmax)
    pq[k] = v
    priorities[k] = v
end

test_issorted!(pq, priorities)


# low level heap operations
xs = heapify!([v for v in values(priorities)])
@test issorted([heappop!(xs) for _ in length(priorities)])

xs = Array(Int, 0)
for priority in values(priorities)
    heappush!(xs, priority)
end
@test issorted([heappop!(xs) for _ in length(priorities)])

