type Set{T}
    hash::Dict{T,Bool}

    Set() = new(Dict{T,Bool}())
    Set(x...) = add_each(new(Dict{T,Bool}(length(x))), x)
end
Set() = Set{Any}()
Set(x...) = Set{Any}(x...)
Set{T}(x::T...) = Set{T}(x...)

show(s::Set) = (show(typeof(s)); show_comma_array(s,'(',')'))

isempty(s::Set) = isempty(s.hash)
length(s::Set)  = length(s.hash)
eltype{T}(s::Set{T}) = T

has(s::Set, x) = has(s.hash, x)
get(s::Set, x, deflt) = get(s.hash, x, false)

add(s::Set, x) = (s.hash[x] = true; s)
del(s::Set, x) = (del(s.hash, x); s)

add_each(s::Set, xs) = (for x=xs; add(s,x); end; s)
del_each(s::Set, xs) = (for x=xs; del(s,x); end; s)

similar{T}(s::Set{T}) = Set{T}()
copy(s::Set) = add_each(similar(s), s)

del_all{T}(s::Set{T}) = (del_all(s.hash); s)

start(s::Set)       = start(s.hash)
done(s::Set, state) = done(s.hash, state)
next(s::Set, state) = (((k,v),state) = next(s.hash, state); (k,state))

union() = Set()
union(s::Set) = s
function union(s::Set, sets::Set...)
    U = eltype(s)
    for t in sets
        if U == Any
            break
        end
        T = eltype(t)
        U = subtype(T,U) ? U :
            subtype(U,T) ? T : Any
    end
    for t in sets
        add_each(s, t)
    end
    return s
end
