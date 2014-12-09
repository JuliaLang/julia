type Set{T}
    dict::Dict{T,Void}

    Set() = new(Dict{T,Void}())
    Set(itr) = union!(new(Dict{T,Void}()), itr)
end
Set() = Set{Any}()
Set(itr) = Set{eltype(itr)}(itr)

function show(io::IO, s::Set)
    print(io,"Set")
    if isempty(s)
        print(io,"{",eltype(s),"}()")
        return
    end
    print(io,"(")
    show_vector(io,s,"[","]")
    print(io,")")
end

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

sizehint!(s::Set, newsz) = (sizehint!(s.dict, newsz); s)
empty!{T}(s::Set{T}) = (empty!(s.dict); s)

rehash!(s::Set) = (rehash!(s.dict); s)

start(s::Set)       = start(s.dict)
done(s::Set, state) = done(s.dict, state)
# NOTE: manually optimized to take advantage of Dict representation
next(s::Set, i)     = (s.dict.keys[i], skip_deleted(s.dict,i+1))

# TODO: simplify me?
pop!(s::Set) = (val = s.dict.keys[start(s.dict)]; delete!(s.dict, val); val)

join_eltype() = Bottom
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
const ∪ = union

intersect(s::Set) = copy(s)
function intersect(s::Set, sets::Set...)
    i = similar(s)
    for x in s
        inall = true
        for t in sets
            if !in(x,t)
                inall = false
                break
            end
        end
        inall && push!(i, x)
    end
    return i
end
const ∩ = intersect

function setdiff(a::Set, b::Set)
    d = similar(a)
    for x in a
        if !(x in b)
            push!(d, x)
        end
    end
    d
end

==(l::Set, r::Set) = (length(l) == length(r)) && (l <= r)
< (l::Set, r::Set) = (length(l) < length(r)) && (l <= r)
<=(l::Set, r::Set) = issubset(l, r)

function issubset(l, r)
    for elt in l
        if !in(elt, r)
            return false
        end
    end
    return true
end
const ⊆ = issubset
⊊(l::Set, r::Set) = ⊆(l, r) && l!=r
⊈(l::Set, r::Set) = !⊆(l, r)

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
function filter(f::Function, s::Set)
    u = similar(s)
    for x in s
        if f(x)
            push!(u, x)
        end
    end
    return u
end
