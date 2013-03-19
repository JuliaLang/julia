##############
# OrderedDict
# 
# This is a Dict which retains the insertion order of elements.  New
# key-value pairs are appended.  If a key from a key-value pair
# already exists in the dictionary, the original value is updated, and
# the key remains in its original position.
#
# In addition, many of the Dequeue-related functions are available:
#
#   push!(od, (k,v))       # Adds (k,v) to the end of the dictionary
#   pop!(od)               # Removes and returns the last key-value pair
#   unshift!(od, (k,v))    # Adds (k,v) to the front of the dictionary
#   shift!(od)             # Removes and returns the first key-value pair
#   insert!(od, i, (k,v))  # Insert (k,v) at position i
#   append!(od, items)     # Adds (k,v) pairs from items to the end of
#                          # the dictionary
#
# Note also that this is not a sorted dictionary, although it can be
# sorted with 
#
#   sort!(od)              # od is an OrderedDict()
#   sortby!(od, x->od[x])  # sort by value
#   od2 = sort(od)         # od is not modified
#
# You can also sort normal dictionaries, and get a sorted OrderedDict
# back:
#
#   od = sort(d)         # d is a Dict; returns a sorted OrderedDict
#   #sort!(d)            # error! Dicts can't be sorted in place!
#
# Additional AbstractArray-like features
#
#   indexof(od, key)     # returns the index of key according to the current order
#   getitem(od, 2)       # returns the second (k,v) pair according to the current order
#   od[2]                # same, but only works for OrderedDicts where the
#                        # keys are not Numbers
#   first(od) == od[1]   
#   last(od) == od[end]
#   reverse!(od)         # reverses od in-place
#   reverse(od)          # creates a reversed copy of od

######################
## OrderedDict type ##

type OrderedDict{K,V} <: AbstractHashDict{K,V}
    slots::Array{Uint8,1}
    keys::Array{K,1}
    vals::Array{V,1}
    ord_idxs::Array{Int,1}
    ord_slots::BitArray
    ord::Array{Int,1}
    ndel::Int
    odel::Int
    count::Int

    function OrderedDict()
        n = 16
        new(zeros(Uint8,n), Array(K,n), Array(V,n), Array(Int,n), BitArray(), Array(Int,0), 0, 0, 0)
    end
    function OrderedDict(ks, vs)
        n = length(ks)
        h = OrderedDict{K,V}()
        for i=1:n
            h[ks[i]] = vs[i]
        end
        return h
    end
end
OrderedDict() = OrderedDict{Any,Any}()

OrderedDict{K,V}(ks::AbstractArray{K}, vs::AbstractArray{V}) = OrderedDict{K,V}(ks,vs)
OrderedDict(ks, vs) = OrderedDict{Any,Any}(ks, vs)

# syntax entry points
OrderedDict{K,V}(ks::(K...), vs::(V...)) = OrderedDict{K  ,V  }(ks, vs)
OrderedDict{K  }(ks::(K...), vs::Tuple ) = OrderedDict{K  ,Any}(ks, vs)
OrderedDict{V  }(ks::Tuple , vs::(V...)) = OrderedDict{Any,V  }(ks, vs)
OrderedDict{K,V}(kvs::AbstractArray{(K,V)}) = OrderedDict{K,V}(zip(kvs...)...)
#OrderedDict{K,V}(d::Associative{K,V}) = OrderedDict{K,V}(collect(d))  ## Why doesn't this work?
OrderedDict{K,V}(d::Associative{K,V}) = OrderedDict(collect(d))

##########################
## Construction-related ##

similar{K,V}(d::OrderedDict{K,V}) = OrderedDict{K,V}()

### Provided by AbstractHashDict

# serialize(s, t::OrderedDict)                     ## TODO: not working for OrderedDict
# deserialize{K,V}(s, T::Type{OrderedDict{K,V}})   ## TODO: not working for OrderedDict
# isslotempty(h::OrderedDict, i::Int)
# isslotfilled(h::OrderedDict, i::Int)
# isslotmissing(h::OrderedDict, i::Int)
# ht_keyindex{K,V}(h::OrderedDict{K,V}, key)
# isempty(t::OrderedDict)
# length(t::OrderedDict)
# getindex{K,V}(h::OrderedDict{K,V}, key)          ## See additional defs below
# has(h::OrderedDict, key)
# get{K,V}(h::OrderedDict{K,V}, key, deflt)
# getkey{K,V}(h::OrderedDict{K,V}, key, deflt)
# delete!(h::OrderedDict, key)                     ## See additional defs below
# delete!(h::OrderedDict, key, default)


# OrderedDict version of rehash
function rehash{K,V}(h::OrderedDict{K,V}, newsz)
    newsz = _tablesz(newsz)
    nel = h.count
    h.ndel = h.count = 0
    if nel == 0
        resize!(h.slots, newsz)
        fill!(h.slots, 0)
        resize!(h.keys, newsz)
        resize!(h.vals, newsz)
        resize!(h.ord_idxs, newsz)
        resize!(h.ord_slots, 0)
        resize!(h.ord, 0)
        return h
    end
    olds = h.slots
    oldk = h.keys
    oldv = h.vals
    oldi = h.ord_idxs
    sz = length(olds)
    h.slots = zeros(Uint8,newsz)
    h.keys = Array(K, newsz)
    h.vals = Array(V, newsz)
    h.ord_idxs = Array(Int, newsz)

    for i = 1:sz
        if olds[i] == 0x1
            k = oldk[i]
            index = hashindex(k, newsz)
            while h.slots[index] != 0
                index = (index & (newsz-1)) + 1
            end
            h.slots[index] = 0x1
            h.keys[index] = k
            h.vals[index] = oldv[i]

            idx = oldi[i]
            h.ord_idxs[index] = idx
            h.ord[idx] = index
            h.count += 1
        end
    end

    return h
end


# Removes empty slots of ord array in OrderedDict
function _compact(h::OrderedDict)
    if h.odel == 0 return; end

    # start with first empty slot
    npos = findfirstnot(h.ord_slots)
    opos = findnext(h.ord_slots, npos)
    
    # fill down empty slots with consecutive filled slots
    while opos != 0
        index = h.ord[npos] = h.ord[opos]
        h.ord_idxs[index] = npos
        
        npos += 1
        opos = findnext(h.ord_slots, opos+1)
    end
    
    resize!(h.ord, h.count)
    resize!(h.ord_slots, h.count)
    h.ord_slots[1:end] = true
    h.odel = 0

    nothing
end

###############
## Iteration ##

skip_deleted(t::OrderedDict, i::Int) = findnext(t.ord_slots, i)

start(t::OrderedDict) = skip_deleted(t,1)
done(t::OrderedDict, i) = (i == 0)
next(t::OrderedDict, i) = (idx = t.ord[i]; ((t.keys[idx],t.vals[idx]), skip_deleted(t,i+1)))


#########################
## General Collections ##

function empty!{K,V}(h::OrderedDict{K,V})
    fill!(h.slots, 0x0)
    sz = length(h.slots)
    h.keys = Array(K, sz)
    h.vals = Array(V, sz)
    h.ord_idxs = Array(Int, sz)
    h.ord_slots = BitArray()
    h.ord = Array(Int, 0)
    h.ndel = 0
    h.odel = 0
    h.count = 0
    return h
end

###########################
## Indexable Collections ##

# As with ref, we want to allow assignment by index position as well, as key,
# but we ignore this when K<:Number

function setindex!{K,V}(h::OrderedDict{K,V}, kv::(Any,Any), index::Integer)
    (key,v) = kv
    ord_idx = indexof(h,key,0)          # calls _compact(h), so not needed here
    if ord_idx == index
        return setindex!(h, v, key)
    end
    # TODO: this can be more efficient
    delete!(h, h.keys[h.ord[index]])
    insert!(h, index, kv)
end

setindex!{K<:Number,V}(h::OrderedDict{K,V}, v::(Any,Any), key::Integer) = 
    invoke(setindex!, (OrderedDict{K,V}, Any, Any), h, v, key)
setindex!{K<:Number,V}(h::OrderedDict{K,V}, v, key::Integer) = 
    invoke(setindex!, (OrderedDict{K,V}, Any, Any), h, v, key)


function setindex!{K,V}(h::OrderedDict{K,V}, v, key)
    key = convert(K,key)
    v   = convert(V,  v)

    sz = length(h.keys)

    if h.ndel >= ((3*sz)>>2) || h.count*3 > sz*2
        # > 3/4 deleted or > 2/3 full
        rehash(h, h.count > 64000 ? h.count*2 : h.count*4)
        sz = length(h.keys)  # rehash may resize the table at this point!
    end

    if h.odel >= ((3*length(h.ord))>>2)
        # > 3/4 of ord array deleted
        _compact(h)
    end

    iter = 0
    maxprobe = max(16, sz>>6)
    index = hashindex(key, sz)
    orig = index
    avail = -1  # an available slot
    #keys = h.keys; vals = h.vals; ord_idxs = h.ord_idxs; ord_slots = h.ord_slots; ord = h.ord

    while true
        if isslotempty(h,index)
            if avail > 0; index = avail; end
            push!(h.ord, index)
            push!(h.ord_slots, true)
            h.slots[index] = 0x1
            h.keys[index] = key
            h.vals[index] = v
            h.ord_idxs[index] = length(h.ord)
            h.count += 1
            return h
        end

        if isslotmissing(h,index)
            if avail<0
                # found an available slot, but need to keep scanning
                # in case "key" already exists in a later collided slot.
                avail = index
            end
        elseif isequal(key, h.keys[index])
            h.vals[index] = v
            return h
        end

        index = (index & (sz-1)) + 1
        iter+=1
        if iter > maxprobe || index==orig
            break
        end
    end

    if avail>0
        index = avail
        push!(h.ord, index)
        push!(h.ord_slots, true)
        h.slots[index] = 0x1
        h.keys[index] = key
        h.vals[index] = v
        h.ord_idxs[index] = length(h.ord)
        h.count += 1
        return h
    end

    rehash(h, h.count > 64000 ? sz*2 : sz*4)

    setindex!(h, v, key)
end

# We want to allow the user to access the (k,v) pairs by index
# However, first and foremost, this is a dictionary, so if the
# keys are numbers, assume any reference using an integer
# as a key is attempting a has lookup.

# TODO: This might be confusing behavior, so consider disabling
# and simply sequential access through getitem()

# getindex{K,V}(h::AbstractHashDict{K,V}, key) defined in dict.jl

getindex{K,        V}(h::OrderedDict{K,V}, ord_idx::Integer) = getitem(h, ord_idx)
getindex{K<:Number,V}(h::OrderedDict{K,V}, key::Integer)     = invoke(getindex(OrderedDict{K,V}, Any), h, key)

function indexof{K,V}(h::OrderedDict{K,V}, key)
    index = ht_keyindex(h, key)
    return (index<0) ? throw(KeyError(key)) : (_compact(h); h.ord_idxs[index])
end

function indexof{K,V}(h::OrderedDict{K,V}, key, deflt)
    index = ht_keyindex(h, key)
    return (index<0) ? deflt : (_compact(h); h.ord_idxs[index])
end

findfirst(h::OrderedDict, v) = indexof(h, v, 0)
findnext(h::OrderedDict, v, start::Int) = (idx=indexof(h,v,0); idx >= start? idx : 0)
 
first(h::OrderedDict) = getitem(h, 1)
last(h::OrderedDict) = getitem(h, length(h))
endof(h::OrderedDict) = length(h)

first(h::OrderedDict) = getitem(h, 1)
last(h::OrderedDict) = getitem(h, h.count)

function reverse!(h::OrderedDict)
    _compact(h)
    reverse!(h.ord)
    reverse!(h.ord_slots)
    _update_order(h, 1, h.count)
    h
end
reverse(h::OrderedDict) = (_compact(h); reverse!(copy(h)))


#############################
## Associative Collections ##

## Defined for AbstractHashDict
# has(h::OrderedDict, key) = (ht_keyindex(h, key) >= 0)
# get{K,V}(h::OrderedDict{K,V}, key, deflt)
# getkey{K,V}(h::OrderedDict{K,V}, key, deflt)

function getitem{K,V}(h::OrderedDict{K,V}, ord_idx)
    _compact(h)
    index = h.ord[ord_idx]
    return (h.keys[index], h.vals[index])::(K,V)
end

function _delete!(h::OrderedDict, index)
    val = h.vals[index]
    idx = h.ord_idxs[index]
    h.slots[index] = 0x2
    ccall(:jl_arrayunset, Void, (Any, Uint), h.keys, index-1)
    ccall(:jl_arrayunset, Void, (Any, Uint), h.vals, index-1)
    # h.ord_idxs[index] = 0  ## not really necessary
    # ord[idx] = 0           ## not really necessary
    h.ord_slots[idx] = false
    h.ndel += 1
    h.odel += 1
    h.count -= 1
    return val
end

## Defined for AbstractHashDict
# delete!(h::OrderedDict, key)
# delete!(h::OrderedDict, key, default)

function delete!{K,V}(h::OrderedDict{K,V}, ord_idx::Integer)
    key = h.keys[h.ord[ord_idx]]
    (key, delete!(h, key))::(K,V)
end

function delete!{K,V}(h::OrderedDict{K,V}, ord_idx::Integer, default)
    key = h.keys[h.ord[ord_idx]]
    (key, delete!(h, key))::(K,V)
end

delete!{K<:Number,V}(h::OrderedDict{K,V}, key::Integer) = invoke(delete!, (OrderedDict{K,V}, Any), h, key)

##################
## Dequeue-like ##

# Add key-value pair at last slot
push!{K,V}(d::OrderedDict{K,V}, item) = insert!(d, length(d)+1, item)

# Remove and return last key-value pair
function pop!{K,V}(d::OrderedDict{K,V})
    if isempty(d)
        error("pop!: OrderedDict is empty")
    end
    _compact(d) # TODO: make more efficient: find last key index? See shift!
    key = d.keys[d.ord[end]]
    (key, delete!(d,key))::(K,V)
end

# Put key-value pair at front of dict
unshift!{K,V}(d::OrderedDict{K,V}, item) = insert!(d, 1, item)

# Remove and return first key-value pair
function shift!{K,V}(d::OrderedDict{K,V})
    if isempty(d)
        error("shift!: OrderedDict is empty")
    end
    idx = findfirst(d.ord_slots)
    key = d.keys[d.ord[idx]]
    (key, delete!(d,key))::(K,V)
end

# Add multiple items to dictionary, at end
function append!{K,V}(h::OrderedDict{K,V}, items)
    for item in items
        push!(h, item)
    end
    items
end

# Add item to dictionary at a particular linear location
# Note that if the key already exists in the dictionary, it is removed
# first, and this might change the inserted position by one
function insert!{K,V}(h::OrderedDict{K,V}, index::Integer, item::(Any,Any))
    (key,v) = item
    ord_idx = indexof(h, key, 0)
    if ord_idx > 0 && index > ord_idx
        index -= 1
    end
    setindex!(h, v, key)
    ord_idx = indexof(h, key)
    if ord_idx == index
        return item::(K,V)
    end
    _compact(h)  ## TODO: this could be removed, at the cost of more code
    move_item!(h.ord, ord_idx, index)
    _update_order(h, index, ord_idx)
    item::(K,V)
end

# move item in a from a[from] to a[to], shifting elements as needed
function move_item!{T}(a::AbstractArray{T}, from, to)
    item = a[from]
    if from < to
        for i = from:to-1
            a[i] = a[i+1]
        end
    elseif from > to
        for i = from:-1:to+1
            a[i] = a[i-1]
        end
    end
    a[to] = item
    nothing
end

function _update_order{K,V}(h::OrderedDict{K,V}, first, last)
    if first > last
        (first, last) = (last, first)
    end

    for i = first:last
        h.ord_idxs[h.ord[i]] = i
    end
end

# Sorting

import Base.sort!, Base.sort, Base.sortby!, Base.sortby, Base.sortperm

function sort!(h::OrderedDict, args...)
    _compact(h)
    p = sortperm(h.keys[h.ord], args...)
    h.ord[:] = h.ord[p]
    _update_order(h, 1, h.count)
    h
end

function sort{K,V}(h::Dict{K,V}, args...)
    d = OrderedDict{K,V}()
    sizehint(d, length(h.slots))
    for k in sort(keys(h))
        d[k] = h[k]
    end
    d
end

sortperm(v::OrderedDict, args...) = sortperm(v.keys[v.ord], args...)

# TODO: with multiple inheritance (or simplification of the sort module)
# almost everything below could be removed
# (This was simply copy and pasted from base/sort.jl, and AbstractVector was changed to OrderedDict)

sort (v::OrderedDict, a::Sort.Algorithm, o::Sort.Ordering) = sort!(copy(v), a, o)

sort!(v::OrderedDict, o::Sort.Ordering) = sort!(v, Sort.DEFAULT_STABLE, o)
sort (v::OrderedDict, o::Sort.Ordering) = sort (v, Sort.DEFAULT_STABLE, o)

for s in {:sort!, :sort, :sortperm}
    @eval begin
        # default to forward sort ordering
        $s(v::OrderedDict, a::Sort.Algorithm) = $s(v, a, Forward())
        $s(v::OrderedDict              ) = $s(v,    Forward())

        # auto-instntiate algorithms and orderings from types
        $s{A<:Sort.Algorithm,O<:Sort.Ordering}(v::OrderedDict, ::Type{A},         ::Type{O})        = $s(v, A(), O())
        $s{A<:Sort.Algorithm                 }(v::OrderedDict, ::Type{A},         o::Sort.Ordering) = $s(v, A(), o)
        $s{                  O<:Sort.Ordering}(v::OrderedDict, a::Sort.Algorithm, ::Type{O})        = $s(v, a,   O())
        $s{A<:Sort.Algorithm                 }(v::OrderedDict, ::Type{A})                           = $s(v, A())
        $s{                  O<:Sort.Ordering}(v::OrderedDict,                    ::Type{O})        = $s(v,      O())

        # also allow ordering before algorithm
        $s                                    (v::OrderedDict, o::Sort.Ordering, a::Sort.Algorithm) = $s(v, a, o)
        $s{A<:Sort.Algorithm,O<:Sort.Ordering}(v::OrderedDict, ::Type{O},        ::Type{A})         = $s(v, A(), O())
        $s{A<:Sort.Algorithm                 }(v::OrderedDict, o::Sort.Ordering, ::Type{A})         = $s(v, A(), o)
        $s{                  O<:Sort.Ordering}(v::OrderedDict, ::Type{O},        a::Sort.Algorithm) = $s(v, a,   O())
    end
end

for s in {:sort!, :sort, :sortperm}
    @eval begin
        $s{A<:Sort.Algorithm}(v::OrderedDict, a::Union(A,Type{A}), lt::Function) = $s(v, a, Sort.Lt(lt))
        $s{A<:Sort.Algorithm}(v::OrderedDict, lt::Function, a::Union(A,Type{A})) = $s(v, a, lt)
        $s                   (v::OrderedDict, lt::Function)                      = $s(v, Sort.Lt(lt))
        $s                   (lt::Function, v::OrderedDict, args...)             = $s(v, lt, args...)
    end
end

for (sb,s) in {(:sortby!, :sort!), (:sortby, :sort), (:sortpermby, :sortperm)}
    @eval begin
        $sb{A<:Sort.Algorithm}(v::OrderedDict, a::Union(A,Type{A}), by::Function) = $s(v, a, Sort.By(by))
        $sb{A<:Sort.Algorithm}(v::OrderedDict, by::Function, a::Union(A,Type{A})) = $s(v, a, Sort.By(by))
        $sb                   (v::OrderedDict, by::Function)                      = $s(v, Sort.By(by))
        $sb                   (by::Function, v::OrderedDict, args...)             = $s(v, Sort.By(by), args...)
    end
end

