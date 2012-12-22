# enumerate

type Enumerate{I}
    itr::I
end
enumerate(itr) = Enumerate(itr)

length(e::Enumerate) = length(e.itr)
start(e::Enumerate) = (1, start(e.itr))
function next(e::Enumerate, state)
    v, s = next(e.itr, state[2])
    (state[1],v), (state[1]+1,s)
end
done(e::Enumerate, state) = done(e.itr, state[2])

# zip

type Zip
    itrs::Vector{Any}
    Zip(itrs...) = new({itrs...})
end
zip(itrs...) = Zip(itrs...)

length(z::Zip) = min(length, z.itrs)
start(z::Zip) = { start(itr) for itr in z.itrs }
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

type Filter{I}
    flt::Function
    itr::I
end
filter(flt::Function, itr) = Filter(flt, itr)

start(f::Filter) = _jl_start_filter(f.flt, f.itr)
function _jl_start_filter(pred, itr)
    s = start(itr)
    while !done(itr,s)
        v,t = next(itr,s)
        if pred(v)
            break
        end
        s=t
    end
    s
end

next(f::Filter, s) = _jl_advance_filter(f.flt, f.itr, s)
function _jl_advance_filter(pred, itr, s)
    v,s = next(itr,s)
    while !done(itr,s)
        w,t = next(itr,s)
        if pred(w)
            break
        end
        s=t
    end
    v,s
end

done(f::Filter, s) = done(f.itr,s)

# reverse

type Reverse
    itr
end
reverse(itr) = Reverse(itr)

length(r::Reverse) = length(r.itr)
start(r::Reverse) = length(r.itr)
next(r::Reverse, i) = (r.itr[i], i-1)
done(r::Reverse, i) = i < 1

# TODO: a more general "reversible" interface; this only
# works for objects that are indexable from 1 to length(itr)


