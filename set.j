struct Set{T}
    items::Array{T,1}

    Set() = new(Array(Any,0))
    Set{T}(T::Type) = new(Array(T,0))
end

set{T}(x::T...) = (s = Set(T); add(s, x...))

isempty(set::Set) = isempty(set.items)
length(set::Set) = length(set.items)

has(set::Set, x) = anyp(y->isequal(x,y), set.items)

function add(set::Set, x)
    if !has(set,x)
        items = clone(set.items, length(set.items)+1)
        for i = 1:length(set.items)
            items[i] = set.items[i]
        end
        items[end] = x
        set.items = items
    end
    set
end

add(set::Set, xs...)   = (for x = xs; add(set, x); end; set)
add(set::Set, s2::Set) = (for x = s2; add(set, x); end; set)

function del(set::Set, x)
    if has(set,x)
        items = clone(set.items, length(set.items)-1)
        j = 1
        for i = 1:length(set.items)
            if !isequal(set.items[i],x)
                items[j] = set.items[i]
                j += 1
            end
        end
        set.items = items
    end
    set
end

del(set::Set, xs...)   = (for x = xs; del(set, x); end; set)
del(set::Set, s2::Set) = (for x = s2; del(set, x); end; set)

start(set::Set) = start(set.items)
done(set::Set, x) = done(set.items, x)
next(set::Set, x) = next(set.items, x)

function union{T}(sets::Set{T}...)
    u = Set(T)
    for set = sets
        add(u,set)
    end
    return u
end
