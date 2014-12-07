isempty(itr) = done(itr, start(itr))

# enumerate

immutable Enumerate{I}
    itr::I
end
enumerate(itr) = Enumerate(itr)

length(e::Enumerate) = length(e.itr)
start(e::Enumerate) = (1, start(e.itr))
nextval(e::Enumerate, state) = (state[1], nextval(e.itr,state[2]))
nextstate(e::Enumerate, state) = (state[1]+1, nextstate(e.itr,state[2]))
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
nextval(z::Zip, state) = map(nextval, z.itrs, state)
nextstate(z::Zip, state) = map(nextstate, z.itrs, state)
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
nextval(z::Zip2, st) = (nextval(z.a,st[1]), nextval(z.b,st[2]))
nextstate(z::Zip2, st) = (nextstate(z.a,st[1]), nextstate(z.b,st[2]))
done(z::Zip2, st) = done(z.a,st[1]) | done(z.b,st[2])

eltype(z::Zip2) = (eltype(z.a), eltype(z.b))

# filter

immutable Filter{I}
    flt::Function
    itr::I
end
filter(flt::Function, itr) = Filter(flt, itr)

function filter_state(pred, itr, s)
    while !done(itr,s)
        v = nextval(itr,s)
        t = nextstate(itr,s)
        if pred(v)
            return (false, v, t)
        end
        s=t
    end
    (true,)
end

start(f::Filter) = filter_state(f.flt, f.itr, start(f.itr))
done(f::Filter, s) = s[1]::Bool
nextval(f::Filter, s) = s[2]
nextstate(f::Filter, s) = filter_state(f.flt, f.itr, s[3])

eltype(f::Filter) = eltype(f.itr)

# Rest -- iterate starting at the given state
immutable Rest{I,S}
    itr::I
    st::S
end
rest(itr,state) = Rest(itr,state)

start(i::Rest) = i.st
nextval(i::Rest, st) = nextval(i.itr, st)
nextstate(i::Rest, st) = nextstate(i.itr, st)
done(i::Rest, st) = done(i.itr, st)

eltype(r::Rest) = eltype(r.itr)

# TODO: a general "reversible" interface
