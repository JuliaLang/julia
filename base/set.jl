type Set{T}
    dict::Dict{T,Nothing}

    Set() = new(Dict{T,Nothing}())
    Set(itr) = union!(new(Dict{T,Nothing}()), itr)
end
Set() = Set{Any}()
Set(itr) = Set{eltype(itr)}(itr)

show(io::IO, s::Set) = (show(io, typeof(s)); show_comma_array(io, s,"({","})"))

isempty(s::Set) = isempty(s.dict)
length(s::Set)  = length(s.dict)
eltype{T}(s::Set{T}) = T

in(x, s::Set) = haskey(s.dict, x)

push!(s::Set, x) = (s.dict[x] = nothing; s)
pop!(s::Set, x) = (pop!(s.dict, x); x)
pop!(s::Set, x, deflt) = pop!(s.dict, x, deflt) == deflt ? deflt : x
delete!(s::Set, x) = (delete!(s.dict, x); s)

union!(s::Set, xs) = (for x=xs; push!(s,x); end; s)
setdiff!(s::Set, xs) = (for x=xs; delete!(s,x); end; s)

similar{T}(s::Set{T}) = Set{T}()
copy(s::Set) = union!(similar(s), s)

sizehint(s::Set, newsz) = sizehint(s.dict, newsz)
empty!{T}(s::Set{T}) = (empty!(s.dict); s)

start(s::Set)       = start(s.dict)
done(s::Set, state) = done(s.dict, state)
# NOTE: manually optimized to take advantage of Dict representation
next(s::Set, i)     = (s.dict.keys[i], skip_deleted(s.dict,i+1))

# TODO: simplify me?
pop!(s::Set) = (val = s.dict.keys[start(s.dict)]; delete!(s.dict, val); val)

join_eltype() = None
join_eltype(v1, vs...) = typejoin(eltype(v1), join_eltype(vs...))

union() = Set()
union(s::Set) = copy(s)
function union(s::Set, sets::Set...)
    u = Set{join_eltype(s, sets...)}()
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
            if !in(x,t)
                delete!(i,x)
                break
            end
        end
    end
    return i
end

function setdiff(a::Set, b::Set)
    d = copy(a)
    for x in b
        delete!(d, x)
    end
    d
end

isequal(l::Set, r::Set) = (length(l) == length(r)) && (l <= r)
<(l::Set, r::Set) = (length(l) < length(r)) && (l <= r)
<=(l::Set, r::Set) = issubset(l, r)

function issubset(l, r)
    for elt in l
        if !in(elt, r)
            return false
        end
    end
    return true
end

function unique(C)
    out = Array(eltype(C),0)
    seen = Set{eltype(C)}()
    for x in C
        if !in(x, seen)
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

hash(s::Set) = hash(sort(s.dict.keys[s.dict.slots .!= 0]))
