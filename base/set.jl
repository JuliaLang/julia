type Set{T,O}
    dict::Dict{T,Nothing,O}

    Set() = new(Dict{T,Nothing,O}())
    Set(x...) = union!(new(Dict{T,Nothing,O}()), x)
end
Set(x...) = Set{Any,Unordered}(x...)
Set{T}(x::T...) = Set{T,Unordered}(x...)

OrderedSet(x...) = Set{Any,Ordered}(x...)
OrderedSet{T}(x::T...) = Set{T,Ordered}(x...)

show(io::IO, s::Set) = (show(io, typeof(s)); show_comma_array(io, s,'(',')'))

isempty(s::Set) = isempty(s.dict)
length(s::Set)  = length(s.dict)
eltype{T}(s::Set{T}) = T

contains(s::Set, x) = haskey(s.dict, x)

add!(s::Set, x) = (s.dict[x] = nothing; s)
delete!(s::Set, x) = (delete!(s.dict, x); x)
delete!(s::Set, x, deflt) = delete!(s.dict, x, deflt)

union!(s::Set, xs) = (for x=xs; add!(s,x); end; s)
setdiff!(s::Set, xs) = (for x=xs; delete!(s,x,nothing); end; s)

similar{T,O}(s::Set{T,O}) = Set{T,O}()
copy(s::Set) = union!(similar(s), s)

empty!{T}(s::Set{T}) = (empty!(s.dict); s)

start(s::Set)       = start(s.dict)
done(s::Set, state) = done(s.dict, state)
# NOTE: manually optimized to take advantage of Dict representation
next(s::Set, i)                  = (s.dict.keys[i], skip_deleted(s.dict,i+1))
next{T}(s::Set{T,Ordered}, i)     = (s.dict.keys[s.dict.order[i]], skip_deleted(s.dict,i+1))

# TODO: simplify me?
pop!(s::Set) = (val = s.dict.keys[start(s.dict)]; delete!(s.dict, val); val)

union() = Set()
union(s::Set) = copy(s)
function union{T,O}(s::Set{T,O}, sets::Set...)
    U = eltype(s)
    if U != Any
        for t in sets
            T = eltype(t)
            U = subtype(T,U) ? U :
                subtype(U,T) ? T : Any # TODO: tigher upper bound
        end
    end
    u = Set{U,O}()
    union!(u,s)
    for t in sets
        union!(u,t)
    end
    return u
end

intersect(s::Set) = copy(s)
function intersect(s::Set, sets::Set...)
    i = copy(s)
    for x in s
        for t in sets
            if !contains(t,x) & contains(i,x)
                delete!(i,x)
            end
        end
    end
    return i
end

function setdiff(a::Set, b::Set)
    d = copy(a)
    for x in b
        if contains(d, x)
            delete!(d, x)
        end
    end
    d
end

isequal(l::Set, r::Set) = (length(l) == length(r)) && (l <= r)
isless(l::Set, r::Set) = (length(l) < length(r)) && (l <= r)
<=(l::Set, r::Set) = issubset(l, r)

function issubset(l, r)
    for elt in l
        if !contains(r, elt)
            return false
        end
    end
    return true
end

function unique(C)
    seen = Set{eltype(C),Ordered}()
    for x in C
        if !contains(seen, x)
            add!(seen, x)
        end
    end
    collect(seen)
end

function filter!(f::Function, s::Set)
    for x in s
        if !f(x)
            delete!(s, x)
        end
    end
    return s
end
filter(f::Function, s::Set) = filter!(f, copy(s))
