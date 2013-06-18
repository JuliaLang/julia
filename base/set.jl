type Set{T}
    dict::Dict{T,Nothing}

    Set() = new(Dict{T,Nothing}())
    Set(x...) = union!(new(Dict{T,Nothing}()), x)
end
Set() = Set{Any}()
Set(x...) = Set{Any}(x...)
Set{T}(x::T...) = Set{T}(x...)

show(io::IO, s::Set) = (show(io, typeof(s)); show_comma_array(io, s,'(',')'))

isempty(s::Set) = isempty(s.dict)
length(s::Set)  = length(s.dict)
eltype{T}(s::Set{T}) = T

contains(s::Set, x) = haskey(s.dict, x)

push!(s::Set, x) = (s.dict[x] = nothing; s)
pop!(s::Set, x) = (pop!(s.dict, x); x)
pop!(s::Set, x, deflt) = pop!(s.dict, x, deflt) == deflt ? deflt : x
delete!(s::Set, x) = (delete!(s.dict, x); s)

union!(s::Set, xs) = (for x=xs; push!(s,x); end; s)
setdiff!(s::Set, xs) = (for x=xs; delete!(s,x); end; s)

similar{T}(s::Set{T}) = Set{T}()
copy(s::Set) = union!(similar(s), s)

empty!{T}(s::Set{T}) = (empty!(s.dict); s)

start(s::Set)       = start(s.dict)
done(s::Set, state) = done(s.dict, state)
# NOTE: manually optimized to take advantage of Dict representation
next(s::Set, i)     = (s.dict.keys[i], skip_deleted(s.dict,i+1))

# TODO: simplify me?
pop!(s::Set) = (val = s.dict.keys[start(s.dict)]; delete!(s.dict, val); val)

union() = Set()
union(s::Set) = copy(s)
function union(s::Set, sets::Set...)
    U = eltype(s)
    if U != Any
        for t in sets
            T = eltype(t)
            U = subtype(T,U) ? U :
                subtype(U,T) ? T : Any # TODO: tigher upper bound
        end
    end
    u = Set{U}()
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
            if !contains(t,x)
                delete!(i,x)  # no error thrown on missing value
            end
        end
    end
    return i
end

function setdiff(a::Set, b::Set)
    d = copy(a)
    for x in b
        delete!(d, x)  # no error thrown on missing value
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
    out = Array(eltype(C),0)
    seen = Set{eltype(C)}()
    for x in C
        if !contains(seen, x)
            push!(seen, x)
            push!(out, x)
        end
    end
    out
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
