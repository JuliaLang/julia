type Set{T}
    hash::Dict{T,Bool}

    Set() = new((T=>Bool)[])
    Set(x...) = add_each!(new(Dict{T,Bool}()), x)
end
Set() = Set{Any}()
Set(x...) = Set{Any}(x...)
Set{T}(x::T...) = Set{T}(x...)

show(io::IO, s::Set) = (show(io, typeof(s)); show_comma_array(io, s,'(',')'))

isempty(s::Set) = isempty(s.hash)
length(s::Set)  = length(s.hash)
eltype{T}(s::Set{T}) = T

has(s::Set, x) = has(s.hash, x)
contains(s::Set, x) = has(s, x)

add!(s::Set, x) = (s.hash[x] = true; s)
delete!(s::Set, x) = (delete!(s.hash, x); x)
function delete!(s::Set, x, deflt)
    if delete!(s.hash, x, false)
        return x
    end
    return deflt
end

add_each!(s::Set, xs) = (for x=xs; add!(s,x); end; s)
del_each!(s::Set, xs) = (for x=xs; delete!(s,x); end; s)

similar{T}(s::Set{T}) = Set{T}()
copy(s::Set) = add_each!(similar(s), s)

empty!{T}(s::Set{T}) = (empty!(s.hash); s)

start(s::Set)       = start(s.hash)
done(s::Set, state) = done(s.hash, state)
# NOTE: manually optimized to take advantage of Dict representation
next(s::Set, i)     = (s.hash.keys[i], skip_deleted(s.hash,i+1))

# TODO: simplify me?
pop!(s::Set) = (val = s.hash.keys[start(s.hash)]; delete!(s.hash, val); val)

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
            if !has(t,x) & has(i,x)
                delete!(i,x)
            end
        end
    end
    return i
end

function setdiff(a::Set, b::Set)
    d = copy(a)
    for x in b
        if has(d, x)
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
        if !has(r, elt)
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

