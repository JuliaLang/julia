# enumerate

type Enumerate
    itr
end
enumerate(itr) = Enumerate(itr)

start(e::Enumerate) = (start(e.itr), 1)
function next(e::Enumerate, state)
    v, s = next(e.itr, state[1])
    (v,state[2]), (s,state[2]+1)
end
done(e::Enumerate, state) = done(e.itr, state[1])

# zip

type Zip
    itrs::Vector{Any}
    Zip(itrs...) = new({itrs...})
end
zip(itrs...) = Zip(itrs...)

start(z::Zip) = { start(itr) | itr in z.itrs }
function next(z::Zip, state)
    v = Array(Any, length(z.itrs))
    s = Array(Any, length(z.itrs))
    for i = 1:length(z.itrs)
        v[i], s[i] = next(z.itrs[i], state[i])
    end
    tuple(v...), s
end
function done(z::Zip, state)
    if isempty(z.itrs)
        return true
    end
    for i = 1:length(z.itrs)
        if done(z.itrs[i], state[i])
            return true
        end
    end
    return false
end

# filter

type Filter
    flt::Function
    itr
end
filter(flt::Function, itr) = Filter(flt, itr)

function _jl_advance_filter(f::Filter, s)
    while !done(f.itr,s)
        v,t = next(f.itr,s)
        if f.flt(v)
            return (v,s)
        end
        s=t
    end
    return (nothing,s)
end

start(f::Filter) = _jl_advance_filter(f,start(f.itr))
next(f::Filter, state) = (state[1],_jl_advance_filter(f,next(f.itr,state[2])[2]))
done(f::Filter, state) = done(f.itr,state[2])

# reverse

type Reverse
    itr
end
reverse(itr) = Reverse(itr)

start(r::Reverse) = length(r.itr)
next(r::Reverse, i) = (r.itr[i], i-1)
done(r::Reverse, i) = i < 1

# TODO: a more general "reversible" interface; this only
# works for objects that are indexable from 1 to length(itr)
