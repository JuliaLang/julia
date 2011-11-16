type Set{T}
    hash::HashTable{T,Bool}

    Set() = new(HashTable{T,Bool}())
end
Set() = Set{Any}()
Set(x...) = (s = Set{Any}(); add(s, x...))
Set{T}(x::T...) = (s = Set{T}(); add(s, x...))

show(s::Set) = (show(typeof(s)); show_comma_array(s,'(',')'))

isempty(s::Set) = isempty(s.hash)
length(s::Set)  = length(s.hash)

has(s::Set, x) = has(s.hash, x)

get(s::Set, x, deflt) = get(s.hash, x, false)

add(s::Set, x)	     = (s.hash[x] = true; s)
add(s::Set, xs...)   = (for x=xs; add(s, x); end; s)

del(s::Set, x)	     = (del(s.hash, x); s)
del(s::Set, xs...)   = (for x=xs; del(s, x); end; s)

del_all{T}(s::Set{T}) = (s.hash = HashTable{T,Bool}(); s)

start(s::Set)       = start(s.hash)
done(s::Set, state) = done(s.hash, state)
next(s::Set, state) = (((k,v),state) = next(s.hash, state); (k,state))

union{T}(sets::Set{T}...) = (u = Set{T}(); for s=sets; add(u,s); end; u)
