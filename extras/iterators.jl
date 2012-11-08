
module Iterators
using Base

import Base.start, Base.next, Base.done

global count, take, repeat

export
    count,
    take,
    drop,
    cycle,
    repeat,
    chain,
    product


# Infinite counting

type Count
    start::Any
    step::Any
end

count(start, step) = Count(start, step)
count(start)       = Count(start, one(start))
count()            = Count(0, 1)

start(it::Count) = it.start
next(it::Count, state) = (state, state + it.step)
done(it::Count, state) = false


# Iterate through the first n elements

type Take
    xs::Any
    n::Int
end

take(xs, n) = Take(xs, n)

start(it::Take) = (it.n, start(it.xs))

function next(it::Take, state)
    n, xs_state = state
    v, xs_state = next(it.xs, xs_state)
    return v, (n - 1, xs_state)
end

function done(it::Take, state)
    n, xs_state = state
    return n <= 0 || done(it.xs, xs_state)
end


# Iterator through all but the first n elements

type Drop
    xs::Any
    n::Int
end

drop(xs, n) = Drop(xs, n)

function start(it::Drop)
    xs_state = start(it.xs)
    for i in 1:it.n
        if done(it.xs, xs_state)
            break
        end

        _, xs_state = next(it.xs, xs_state)
    end
    xs_state
end

next(it::Drop, state) = next(it.xs, state)
done(it::Drop, state) = done(it.xs, state)


# Cycle an iterator forever

type Cycle
    xs::Any
end

cycle(xs) = Cycle(xs)

function start(it::Cycle)
    s = start(it.xs)
    return s, done(it.xs, s)
end

function next(it::Cycle, state)
    s, d = state
    if done(it.xs, s)
        s = start(it.xs)
    end
    v, s = next(it.xs, s)
    return v, (s, false)
end

done(it::Cycle, state) = state[2]


# Repeat an object n (or infinitely many) times.

type Repeat
    x
    n::Int
end

repeat(x, n) = Repeat(x, n)

start(it::Repeat) = it.n
next(it::Repeat, state) = (it.x, state - 1)
done(it::Repeat, state) = state <= 0


type RepeatForever
    x
end

repeat(x) = RepeatForever(x)

start(it::RepeatForever) = nothing
next(it::RepeatForever, state) = (it.x, nothing)
done(it::RepeatForever, state) = false



# Concatenate the output of n iterators

type Chain
    xss::Vector{Any}
    function Chain(xss...)
        new({xss...})
    end
end

chain(xss...) = Chain(xss...)

function start(it::Chain)
    i = 1
    xs_state = nothing
    while i <= length(it.xss)
        xs_state = start(it.xss[i])
        if !done(it.xss[i], xs_state)
            break
        end
        i += 1
    end
    return i, xs_state
end

function next(it::Chain, state)
    i, xs_state = state
    v, xs_state = next(it.xss[i], xs_state)
    while done(it.xss[i], xs_state)
        i += 1
        if i > length(it.xss)
            break
        end
        xs_state = start(it.xss[i])
    end
    return v, (i, xs_state)
end

done(it::Chain, state) = state[1] > length(it.xss)


# Cartesian product as a sequence of tuples

type Product
    xss::Vector{Any}
    function Product(xss...)
        new({xss...})
    end
end

product(xss...) = Product(xss...)

function start(it::Product)
    n = length(it.xss)
    js = {start(xs) for xs in it.xss}
    if n == 0
        return js, nothing
    end
    for i = 1:n
        if done(it.xss[i], js[i])
            return js, nothing
        end
    end
    vs = Array(Any, n)
    for i = 1:n
        vs[i], js[i] = next(it.xss[i], js[i])
    end
    return js, vs
end

function next(it::Product, state)
    js, vs = state
    ans = tuple(vs...)

    n = length(it.xss)
    for i in 1:n
        if !done(it.xss[i], js[i])
            vs[i], js[i] = next(it.xss[i], js[i])
            break
        elseif i == n
            vs = nothing
            break
        end

        js[i] = start(it.xss[i])
        vs[i], js[i] = next(it.xss[i], js[i])
    end
    return ans, (js, vs)
end

done(it::Product, state) = state[2] === nothing

end # module Iterators

