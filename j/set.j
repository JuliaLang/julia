type Set{T}
    hash::HashTable{T,Bool}

    Set() = new(HashTable(T,Bool))
end

set{T}(x::T...) = (s = Set{T}(); add(s, x...))
show(s::Set) = show_comma_array(s,'{','}')

isempty(s::Set) = isempty(s.hash)
length(s::Set)  = length(s.hash)

has(s::Set, x) = has(s.hash, x)

add(s::Set, x)	     = (s.hash[x] = true; s)
add(s::Set, xs...)   = (for x=xs; add(s, x); end; s)
add(s::Set, s2::Set) = (for x=s2; add(s, x); end; s)

del(s::Set, x)	     = (del(s.hash, x); s)
del(s::Set, xs...)   = (for x=xs; del(s, x); end; s)
del(s::Set, s2::Set) = (for x=s2; del(s, x); end; s)

start(s::Set)       = start(s.hash)
done(s::Set, state) = done(s.hash, state)
next(s::Set, state) = (((k,v),state) = next(s.hash, state); (k,state))

union{T}(sets::Set{T}...) = (u = Set{T}(); for s=sets; add(u,s); end; u)
