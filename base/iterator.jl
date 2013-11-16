# enumerate

immutable Enumerate{I}
    itr::I
end
enumerate(itr) = Enumerate(itr)

length(e::Enumerate) = length(e.itr)
start(e::Enumerate) = (1, start(e.itr))
function next(e::Enumerate, state)
    (state[1],next(e.itr,state[2])[1]), (state[1]+1,next(e.itr,state[2])[2])
end
done(e::Enumerate, state) = done(e.itr, state[2])

eltype(e::Enumerate) = (Int, eltype(e.itr))

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

eltype(z::Zip) = map(eltype, z.itrs)

immutable Zip2{I1, I2}
    a::I1
    b::I2
end
zip(a, b) = Zip2(a, b)

length(z::Zip2) = min(length(z.a), length(z.b))
start(z::Zip2) = (start(z.a), start(z.b))
next(z::Zip2, st) = ((next(z.a,st[1])[1], next(z.b,st[2])[1]),
                     (next(z.a,st[1])[2], next(z.b,st[2])[2]))
done(z::Zip2, st) = done(z.a,st[1]) | done(z.b,st[2])

eltype(z::Zip2) = (eltype(z.a), eltype(z.b))

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
            break
        end
        s=t
    end
    s
end

next(f::Filter, s) = advance_filter(f.flt, f.itr, s)
function advance_filter(pred, itr, s)
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

eltype(f::Filter) = eltype(f.itr)

# Rest -- iterate starting at the given state
immutable Rest{I,S}
    itr::I
    st::S
end
rest(itr,state) = Rest(itr,state)

start(i::Rest) = i.st
next(i::Rest, st) = next(i.itr, st)
done(i::Rest, st) = done(i.itr, st)

eltype(r::Rest) = eltype(r.itr)

# TODO: a general "reversible" interface
