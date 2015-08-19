# This file is a part of Julia. License is MIT: http://julialang.org/license

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

function test_isrequested!(pq::PriorityQueue, keys)
    i = 0
    while !isempty(pq)
        krqst =  keys[i+=1]
        krcvd = dequeue!(pq, krqst)
        @test krcvd == krqst
    end
end

pmax = 1000
n = 10000
r = rand(1:pmax, n)
priorities = Dict(zip(1:n, r))

# building from a dict
pq = PriorityQueue(priorities)
test_issorted!(pq, priorities)

pq = PriorityQueue(priorities)
test_isrequested!(pq, 1:n)

# building from two lists
ks, vs = 1:n, rand(1:pmax, n)
pq = PriorityQueue(ks, vs)
priorities = Dict(zip(ks, vs))
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

# dequeuing
pq = PriorityQueue(priorities)
try
    dequeue!(pq, 0)
    error("should have resulted in KeyError")
catch ex
    @test isa(ex, KeyError)
end
@test 10 == dequeue!(pq, 10)
while !isempty(pq)
    @test 10 != dequeue!(pq)
end

# low level heap operations
xs = heapify!([v for v in values(priorities)])
@test issorted([heappop!(xs) for _ in length(priorities)])

xs = Array(Int, 0)
for priority in values(priorities)
    heappush!(xs, priority)
end
@test issorted([heappop!(xs) for _ in length(priorities)])


# test heap operations (heapify, heappeek, heappop!, heappush!, heappushpop!, heapreplace!)
maxheapsize = 7
for heapsize = 1:maxheapsize
    # heapifying all permutations is a bit of overkill (how much? see http://oeis.org/A132862 :) but easy
    # for each heap, we pop an element, or push/pushpop/replace a new element, and check that invariants still hold (heap property etc.)
    for perm in permutations(collect(2:2:2heapsize))
        xs_ref = heapify(perm)
        xs = copy(xs_ref)
        @test isheap(xs)
        @test heappeek(xs) == 2

        x = heappop!(xs)
        @test isheap(xs)
        @test x == 2

        for x in 1:(2*heapsize+1)
            xs = copy(xs_ref)
            heappush!(xs, x)
            @test isheap(xs)
            @test heappeek(xs) == min(x, 2)
            @test x in xs

            xs = copy(xs_ref)
            y = heappushpop!(xs, x) # push then pop
            @test isheap(xs)
            if x <= 2 # we pushed and popped it right back
                @test y == x
                @test xs == xs_ref
            else
                @test y == 2
                @test x in xs
            end

            xs = copy(xs_ref)
            heapreplace!(xs, x) # pop then push
            @test isheap(xs)
            @test x in xs
            if heapsize == 1
                @test heappeek(xs) == x
            else
                @test heappeek(xs) == min(x, 4)
            end
        end
    end
end

# edgecases
xs = Int64[]
@test isheap(xs)
@test_throws BoundsError heappeek(xs)
@test_throws BoundsError heappop!(xs)
@test_throws BoundsError heapreplace!(xs, 5)
@test heappushpop!(xs, 5) == 5
@test isempty(xs)
