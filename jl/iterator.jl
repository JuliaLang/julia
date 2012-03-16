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
