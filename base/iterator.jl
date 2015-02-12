isempty(itr) = done(itr, start(itr))

# enumerate

immutable Enumerate{I}
    itr::I
end
enumerate(itr) = Enumerate(itr)

length(e::Enumerate) = length(e.itr)
start(e::Enumerate) = (1, start(e.itr))
function next(e::Enumerate, state)
    n = next(e.itr,state[2])
    (state[1],n[1]), (state[1]+1,n[2])
end
done(e::Enumerate, state) = done(e.itr, state[2])

eltype{I}(::Type{Enumerate{I}}) = (Int, eltype(I))

# zip

immutable Zip{I<:Tuple}
    itrs::I
    Zip(itrs) = new(itrs)
end
_mkZip{I}(itrs::I) = Zip{I}(itrs)
Zip(itrs...) = _mkZip(itrs)
zip(itrs...) = _mkZip(itrs)

length(z::Zip) = minimum(length, z.itrs)
start(z::Zip) = map(start, z.itrs)
function next(z::Zip, state)
    n = map(next, z.itrs, state)
    map(x->x[1], n), map(x->x[2], n)
end
done(z::Zip, state::()) = true
function done(z::Zip, state)
    for i = 1:length(z.itrs)
        if done(z.itrs[i], state[i])
            return true
        end
    end
    return false
end

eltype{I}(::Type{Zip{I}}) = map(eltype, I)

immutable Zip2{I1, I2}
    a::I1
    b::I2
end
zip(a, b) = Zip2(a, b)

length(z::Zip2) = min(length(z.a), length(z.b))
start(z::Zip2) = (start(z.a), start(z.b))
function next(z::Zip2, st)
    n1 = next(z.a,st[1])
    n2 = next(z.b,st[2])
    return ((n1[1], n2[1]), (n1[2], n2[2]))
end
done(z::Zip2, st) = done(z.a,st[1]) | done(z.b,st[2])

eltype{I1,I2}(::Type{Zip2{I1,I2}}) = (eltype(I1), eltype(I2))

# filter

immutable Filter{I}
    flt::Function
    itr::I
end
filter(flt::Function, itr) = Filter(flt, itr)

start(f::Filter) = start_filter(f.flt, f.itr)
function start_filter(pred, itr)
    s = start(itr)
    while !done(itr,s)
        v,t = next(itr,s)
        if pred(v)
            return (false, v, t)
        end
        s=t
    end
    (true,)
end

next(f::Filter, s) = advance_filter(f.flt, f.itr, s)
function advance_filter(pred, itr, st)
    _, v, s = st
    while !done(itr,s)
        w,t = next(itr,s)
        if pred(w)
            return v, (false, w, t)
        end
        s=t
    end
    v, (true,)
end

done(f::Filter, s) = s[1]

eltype{I}(::Type{Filter{I}}) = eltype(I)

# Rest -- iterate starting at the given state

immutable Rest{I,S}
    itr::I
    st::S
end
rest(itr,state) = Rest(itr,state)

start(i::Rest) = i.st
next(i::Rest, st) = next(i.itr, st)
done(i::Rest, st) = done(i.itr, st)

eltype{I}(::Type{Rest{I}}) = eltype(I)

# Count -- infinite counting

immutable Count{S<:Number}
    start::S
    step::S
end
countfrom(start::Number, step::Number) = Count(promote(start, step)...)
countfrom(start::Number)               = Count(start, one(start))
countfrom()                            = Count(1, 1)

eltype{S}(it::Count{S}) = S

start(it::Count) = it.start
next(it::Count, state) = (state, state + it.step)
done(it::Count, state) = false

# Take -- iterate through the first n elements

immutable Take{I}
    xs::I
    n::Int
end
take(xs, n::Int) = Take(xs, n)

eltype(it::Take) = eltype(it.xs)

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

# Drop -- iterator through all but the first n elements

immutable Drop{I}
    xs::I
    n::Int
end
drop(xs, n::Int) = Drop(xs, n)

eltype(it::Drop) = eltype(it.xs)

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

immutable Cycle{I}
    xs::I
end
cycle(xs) = Cycle(xs)

eltype(it::Cycle) = eltype(it.xs)

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

# Repeat an object infinitely many times

immutable Repeated{O}
    x::O
end
repeated(x) = Repeated(x)
eltype{O}(r::Repeated{O}) = O
start(it::Repeated) = nothing
next(it::Repeated, state) = (it.x, nothing)
done(it::Repeated, state) = false

repeated(x, n::Int) = take(repeated(x), n)
