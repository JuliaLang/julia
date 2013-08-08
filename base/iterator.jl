# enumerate

immutable Enumerate{I}
    itr::I
end
enumerate(itr) = Enumerate(itr)

immutable EnumerateState{S}
    i::Int
    s::S
end

length(e::Enumerate) = length(e.itr)
start(e::Enumerate) = EnumerateState(1, start(e.itr))
function next(e::Enumerate, state::EnumerateState)
    v, s = next(e.itr, state.s)
    (state.i,v), EnumerateState(state.i+1,s)
end
done(e::Enumerate, state::EnumerateState) = done(e.itr, state.s)

# zip

immutable Zip{N}
    itrs::NTuple{N}
    Zip(itrs...) = new(ntuple(i->itrs[i], length(itrs)))
end
zip(itrs...) = Zip{length(itrs)}(itrs...)

length(z::Zip) = min(length, z.itrs)

# This macro generates body functions like this:
#
#    (start(z.itrs[1]), start(z.itrs[2]), start(z.itrs[3]))
#
macro zip_start_ex(z,n)
    aa = {Expr(:call, :(Base.start), Expr(:ref, :($(esc(z)).itrs), i)) for i=1:n}
    Expr(:tuple, aa...)
end

start(z::Zip{0}) = ()
for i=1:10 @eval start(z::Zip{$i}) = @zip_start_ex(z, $i) end
start{N}(z::Zip{N}) = (ntuple(i->start(z.itrs[i]), N))

# This macro generates body functions like this:
#
#    v1,s1 = next(z.itrs[1], state[1])
#    v2,s2 = next(z.itrs[2], state[2])
#    v3,s3 = next(z.itrs[3], state[3])
#    (v1,v2,v3,),(s1,s2,s3,)
#
macro zip_next_ex(z,state,n)
    v = [gensym("v") for i=1:n]
    s = [gensym("s") for i=1:n]
    a = {Expr(:(=),
           Expr(:tuple, v[i], s[i]),
           Expr(:call, :(Base.next), Expr(:ref, :($(esc(z)).itrs), i), Expr(:ref, :($(esc(state))), i)))
           for i = 1:n}
    r = Expr(:tuple, Expr(:tuple, v...), Expr(:tuple, s...))
    Expr(:block, a..., r)
end

next(z::Zip{0}, state::NTuple{0}) = ((), ())
for i=1:10 @eval next(z::Zip{$i}, state::NTuple{$i}) = @zip_next_ex(z, state, $i) end
function next{N}(z::Zip{N}, state::NTuple{N})
    valstates = ntuple(i->next(z.itrs[i], state[i]), N)
    ntuple(i->valstates[i][1], N), ntuple(i->valstates[i][2], N)
end

# This macro generates body functions like this:
#
#    done(z.itrs[1], state[1]) || done(z.itrs[2], state[2]) || done(z.itrs[3], state[3])
#
macro zip_done_ex(z,state,n)
    a = {Expr(:call, :(Base.done), Expr(:ref, :($(esc(z)).itrs), i), Expr(:ref, :($(esc(state))), i)) for i = 1:n}
    Expr(:||, a...)
end

done(z::Zip{0}, state::NTuple{0}) = true
for i=1:10 @eval done(z::Zip{$i}, state::NTuple{$i}) = @zip_done_ex(z, state, $i) end
done{N}(z::Zip{N}, state::NTuple{N}) = any(ntuple(i->done(z.itrs[i], state[i]),N))

# filter

type Filter{I}
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

# Rest -- iterate starting at the given state
immutable Rest{I,S}
    itr::I
    st::S
end
rest(itr,state) = Rest(itr,state)

start(i::Rest) = i.st
next(i::Rest, st) = next(i.itr, st)
done(i::Rest, st) = done(i.itr, st)

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


