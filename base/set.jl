type Set{T}
    slots::Array{Uint8,1}
    keys::Array{T,1}
    ndel::Int
    count::Int

    function Set()
        n = 16
        new(zeros(Uint8,n), Array(T,n), 0, 0)
    end
    function Set(itr)
        //todo:  n should be _tablesz(length(itr)) if itr has a length function
        n = 16
        union!(new(zeros(Uint8,n), Array(T,n), 0, 0), itr)
    end
end
Set() = Set{Any}()
Set(itr) = Set{eltype(itr)}(itr)

show(io::IO, s::Set) = (show(io, typeof(s)); show_comma_array(io, s,"({","})"))

isempty(s::Set) = (s.count == 0)
length(s::Set)  = s.count
eltype{T}(s::Set{T}) = T

in(x, s::Set) = (sht_keyindex(s, x) >= 0)

function push!{T}(s::Set{T}, x) 
    key = convert(T,x)
    if !isequal(key,x)
        error(x, " is not a valid key for type ", T)
    end

    index = sht_keyindex2(s, key)

    if index > 0
        s.keys[index] = key
    else
        s.slots[-index] = 0x1
        s.keys[-index] = key
        s.count += 1

        sz = length(s.keys)
        # Rehash now if necessary
        if s.ndel >= ((3*sz)>>2) || s.count*3 > sz*2
           # > 3/4 deleted or > 2/3 full
           srehash(s, s.count > 64000 ? s.count*2 : s.count*4)
        end
    end
    s
end

function pop!(s::Set, x) 
    index = sht_keyindex(s, x)
    index > 0 ? _delete!(s, index) : throw(KeyError(key))
    x
end

function pop!(s::Set, x, deflt)
    index = sht_keyindex(s, key)
    if index > 0 
        _delete!(s, index)
        return x
    else 
        return deflt
    end
end

function delete!(s::Set, x) 
    index = sht_keyindex(s, x)
    if index > 0; _delete!(s, index); end
    s
end
union!(s::Set, xs) = (for x=xs; push!(s,x); end; s)
setdiff!(s::Set, xs) = (for x=xs; delete!(s,x); end; s)

similar{T}(s::Set{T}) = Set{T}()
copy(s::Set) = union!(similar(s), s)

function sizehint(s::Set, newsz)
    oldsz = length(s.slots)
    if newsz <= oldsz
        # todo: shrink
        # be careful: rehash() assumes everything fits. it was only designed
        # for growing.
        return s
    end
    # grow at least 25%
    newsz = max(newsz, (oldsz*5)>>2)
    srehash(s, newsz)
    s
end
function empty!{T}(s::Set{T}) 
    fill!(s.slots, 0x0)
    sz = length(s.slots)
    s.keys = Array(T, sz)
    s.ndel = 0
    s.count = 0
    s
end

start(s::Set) = skip_deleted(s, 1)
done(s::Set, i) = done(s.keys, i)
next(s::Set, i) = (s.keys[i], skip_deleted(s,i+1))



# TODO: Throw error on empty
function pop!(s::Set)
    index = skip_deleted(s,1)
    val = s.keys[index]
    _delete!(s, index)
    val
end

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

#implementation taken from dict.jl with Dict crossed out and Set written in with crayon
isslotempty(h::Set, i::Int) = h.slots[i] == 0x0
isslotfilled(h::Set, i::Int) = h.slots[i] == 0x1
isslotmissing(h::Set, i::Int) = h.slots[i] == 0x2

function srehash{T}(s::Set{T}, newsz)
    olds = s.slots
    oldk = s.keys
    sz = length(olds)
    newsz = _tablesz(newsz)
    if s.count == 0
        resize!(s.slots, newsz)
        fill!(s.slots, 0)
        resize!(s.keys, newsz)
        s.ndel = 0
        return s
    end

    slots = zeros(Uint8,newsz)
    keys = Array(T, newsz)
    count0 = s.count
    count = 0

    for i = 1:sz
        if olds[i] == 0x1
            k = oldk[i]
            index = hashindex(k, newsz)
            while slots[index] != 0
                index = (index & (newsz-1)) + 1
            end
            slots[index] = 0x1
            keys[index] = k
            count += 1

            if s.count != count0
                # if items are removed by finalizers, retry
                return srehash(s, newsz)
            end
        end
    end

    s.slots = slots
    s.keys = keys
    s.count = count
    s.ndel = 0

    return s
end

# get the index where a key is stored, or -1 if not present
function sht_keyindex{T}(s::Set{T}, key)
    sz = length(s.keys)
    iter = 0
    maxprobe = max(16, sz>>6)
    index = hashindex(key, sz)
    keys = s.keys

    while true
        if isslotempty(s,index)
            break
        end
        if !isslotmissing(s,index) && isequal(key,keys[index])
            return index
        end

        index = (index & (sz-1)) + 1
        iter+=1
        iter > maxprobe && break
    end

    return -1
end


# get the index where a key is stored, or -pos if not present
# and the key would be inserted at pos
# This version is for use by setindex! and get!
function sht_keyindex2{T}(s::Set{T}, key)
    sz = length(s.keys)
    iter = 0
    maxprobe = max(16, sz>>6)
    index = hashindex(key, sz)
    avail = 0
    keys = s.keys

    while true
        if isslotempty(s,index)
            avail < 0 && return avail
            return -index
        end

        if isslotmissing(s,index)
            if avail == 0
                # found an available slot, but need to keep scanning
                # in case "key" already exists in a later collided slot.
                avail = -index
            end
        elseif isequal(key, keys[index])
            return index
        end

        index = (index & (sz-1)) + 1
        iter+=1
        iter > maxprobe && break
    end

    avail < 0 && return avail

    srehash(s, s.count > 64000 ? sz*2 : sz*4)

    return sht_keyindex2(s, key)
end

function _delete!(s::Set, index)
    s.slots[index] = 0x2
    ccall(:jl_arrayunset, Void, (Any, Uint), s.keys, index-1)
    s.ndel += 1
    s.count -= 1
    s
end

function skip_deleted(s::Set, i)
    L = length(s.slots)
    while i<=L && !isslotfilled(s,i)
        i += 1
    end
    return i
end

