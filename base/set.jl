type Set{T}
    hash::Dict{T,Bool}

    Set() = new((T=>Bool)[])
    Set(x...) = add_each(new(Dict{T,Bool}(length(x))), x)
end
Set() = Set{Any}()
Set(x...) = Set{Any}(x...)
Set{T}(x::T...) = Set{T}(x...)

show(io, s::Set) = (show(io, typeof(s)); show_comma_array(io, s,'(',')'))

isempty(s::Set) = isempty(s.hash)
length(s::Set)  = length(s.hash)
elements(s::Set) = keys(s.hash)
eltype{T}(s::Set{T}) = T

has(s::Set, x) = has(s.hash, x)
contains(s::Set, x) = has(s, x)
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
# NOTE: manually optimized to take advantage of Dict representation
next(s::Set, i)     = (s.hash.keys[i], skip_deleted(s.hash,i+1))

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
    add_each(u,s)
    for t in sets
        add_each(u,t)
    end
    return u
end

intersect(s::Set) = copy(s)
function intersect(s::Set, sets::Set...)
    i = copy(s)
    for x in s
        for t in sets
            if !has(t,x)
                del(i,x)
            end
        end
    end
    return i
end

function setdiff(a::Set, b::Set)
    d = copy(a)
    for x in b
        if has(d, x)
            del(d, x)
        end
    end
    d
end

|(s::Set...) = union(s...)
(&)(s::Set...) = intersect(s...)
-(a::Set, b::Set) = setdiff(a,b)

isequal(l::Set, r::Set) = length(l) == length(r) == length(intersect(l,r))
