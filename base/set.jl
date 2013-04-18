type Set{T}
    dict::Dict{T,Nothing}

    Set() = new(Dict{T,Nothing}())
    Set(x...) = add_each!(new(Dict{T,Nothing}()), x)
end
Set() = Set{Any}()
Set(x...) = Set{Any}(x...)
Set{T}(x::T...) = Set{T}(x...)

show(io::IO, s::Set) = (show(io, typeof(s)); show_comma_array(io, s,'(',')'))

isempty(s::Set) = isempty(s.dict)
length(s::Set)  = length(s.dict)
eltype{T}(s::Set{T}) = T

contains(s::Set, x) = has(s.dict, x)

add!(s::Set, x) = (s.dict[x] = nothing; s)
delete!(s::Set, x) = (delete!(s.dict, x); x)
# TODO: this method doesn't make much sense for sets:
delete!(s::Set, x, deflt) = has(s.dict, x) ? delete!(s.dict, x) : deflt

add_each!(s::Set, xs) = (for x=xs; add!(s,x); end; s)
del_each!(s::Set, xs) = (for x=xs; delete!(s,x); end; s)

similar{T}(s::Set{T}) = Set{T}()
copy(s::Set) = add_each!(similar(s), s)

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
    add_each!(u,s)
    for t in sets
        add_each!(u,t)
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

|(s::Set...) = union(s...)
(&)(s::Set...) = intersect(s...)
-(a::Set, b::Set) = setdiff(a,b)

isequal(l::Set, r::Set) = (length(l) == length(r)) && (l <= r)
isless(l::Set, r::Set) = (length(l) < length(r)) && (l <= r)
function <=(l::Set, r::Set)
    for elt in l
        if !contains(r, elt)
            return false
        end
    end
    return true
end

unique(C) = collect(add_each!(Set{eltype(C)}(), C))

function filter!(f::Function, s::Set)
    for x in s
        if !f(x)
            delete!(s, x)
        end
    end
    return s
end
filter(f::Function, s::Set) = filter!(f, copy(s))

