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
#   push!(od, (k,v))      # Adds (k,v) to the end of the dictionary
#   pop!(od)              # Removes and returns the last key-value pair
#   unshift!(od, (k,v))   # Adds (k,v) to the front of the dictionary
#   shift!(od)            # Removes and returns the first key-value pair
#   insert!(od, i, (k,v)) # Inserts (k,v) at position i
#   append!(od, items)    # Adds (k,v) pairs from items to the end of
#                         # the dictionary
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

type OrderedDict{K,V} <: Associative{K,V}
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

function sizehint(d::OrderedDict, newsz)
    oldsz = length(d.slots)
    if newsz <= oldsz
        # todo: shrink
        # be careful: rehash() assumes everything fits. it was only designed
        # for growing.
        return d
    end
    # grow at least 25%
    newsz = max(newsz, (oldsz*5)>>2)
    rehash(d, newsz)
end

## TODO: some of these are simply copied from base/dict.jl,
##       and the type was changed from Dict -> OrderedDict
##
##       (Field names might also be slightly different, but
##       can be reverted.)
##
##       It would be nice if they were defined in terms
##       of an AbstractDict <: Associative

###################
## Serialization ##
# TODO: Remove these if AbstractDict versions become available

function serialize(s, t::OrderedDict)
    serialize_type(s, typeof(t))
    write(s, int32(length(t)))
    for (k,v) in t
        serialize(s, k)
        serialize(s, v)
    end
end

function deserialize{K,V}(s, T::Type{OrderedDict{K,V}})
    n = read(s, Int32)
    t = T(); sizehint(t, n)
    for i = 1:n
        k = deserialize(s)
        v = deserialize(s)
        t[k] = v
    end
    return t
end

########################################
## Dict/OrderedDict Utility functions ##

# TODO: Remove these if AbstractDict versions become available
isslotempty(h::OrderedDict, i::Int) = h.slots[i] == 0x0
isslotfilled(h::OrderedDict, i::Int) = h.slots[i] == 0x1
isslotmissing(h::OrderedDict, i::Int) = h.slots[i] == 0x2

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

# get the index where a key is stored, or -1 if not present
# TODO: remove if AbstractDict version becomes available
function ht_keyindex{K,V}(h::OrderedDict{K,V}, key)
    sz = length(h.keys)
    iter = 0
    maxprobe = max(16, sz>>6)
    index = hashindex(key, sz)
    orig = index
    keys = h.keys

    while true
        if isslotempty(h,index)
            break
        end
        if !isslotmissing(h,index) && isequal(key,keys[index])
            return index
        end

        index = (index & (sz-1)) + 1
        iter+=1
        if iter > maxprobe || index==orig
            break
        end
    end

    return -1
end


# Removes empty slots of order array in OrderedDict
function _compact(h::OrderedDict)
    # start with first empty slot
    npos = findfirstnot(h.ord_slots)
    if npos == 0 return; end
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

isempty(t::OrderedDict) = (t.count == 0)
length(t::OrderedDict) = t.count

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
    ord_idx = indexof(h,key,0)
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

getindex{K,V}(h::OrderedDict{K,V}, ord_idx::Integer) = getitem(h, ord_idx)

function getindex{K<:Number,V}(h::OrderedDict{K,V}, key::Integer)
    index = ht_keyindex(h, key)
    return (index<0) ? throw(KeyError(key)) : h.vals[index]::V
end

function getindex{K,V}(h::OrderedDict{K,V}, key)
    index = ht_keyindex(h, key)
    return (index<0) ? throw(KeyError(key)) : h.vals[index]::V
end

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

#############################
## Associative Collections ##

has(h::OrderedDict, key) = (ht_keyindex(h, key) >= 0)

function get{K,V}(h::OrderedDict{K,V}, key, deflt)
    index = ht_keyindex(h, key)
    return (index<0) ? deflt : h.vals[index]::V
end

function getkey{K,V}(h::OrderedDict{K,V}, key, deflt)
    index = ht_keyindex(h, key)
    return (index<0) ? deflt : h.keys[index]::K
end

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

function delete!{K,V}(h::OrderedDict{K,V}, key)
    index = ht_keyindex(h, key)
    index > 0 ? _delete!(h, index) : throw(KeyError(key))
end

function delete!{K,V}(h::OrderedDict{K,V}, ord_idx::Integer)
    key = h.keys[h.ord[ord_idx]]
    (key, delete!(h, key))::(K,V)
end

delete!{K<:Number,V}(h::OrderedDict{K,V}, key::Integer) = invoke(delete!, (OrderedDict{K,V}, Any), h, key)

function delete!(h::OrderedDict, key, default)
    index = ht_keyindex(h, key)
    index > 0 ? _delete!(h, index) : default
end

function reverse!(h::OrderedDict)
    _compact(h)
    reverse!(h.ord)
    _update_order(h, 1, length(h))
    h
end

function reverse(h::OrderedDict)
    d = similar(h)
    sizehint(d, length(h.slots))
    _compact(h)
    for idx in reverse(h.ord)
        d[h.keys[idx]] = h.vals[idx]
    end
    d
end

##################
## Dequeue-like ##

# Add key-value pair at last slot
push!{K,V}(d::OrderedDict{K,V}, item) = insert!(d, length(d)+1, item)

# Remove and return last key-value pair
function pop!{K,V}(d::OrderedDict{K,V})
    if isempty(d)
        error("pop!: OrderedDict is empty")
    end
    _compact(d) # TODO: more efficient: find last key index? See shift!
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
function sort!(h::OrderedDict, args...)
    _compact(h)
    p = sortperm(h.keys[h.ord], args...)
    h.ord[:] = h.ord[p]
    _update_order(h, 1, length(h))
    h
end

function sort{K,V}(h::Dict{K,V}, args...)
    d = OrderedDict{K,V}()
    sizehint(d, length(h.slots))
    for k in sort(keys(h), args...)
        d[k] = h[k]
    end
    d
end

sortperm(v::OrderedDict, args...) = sortperm(keys(v), args...)

# TODO: with multiple inheritance (or simplification of the sort module)
# almost everything below could be removed
# (This was simply copy and pasted from base/sort.jl, and AbstractVector was changed to OrderedDict)

sort (v::OrderedDict, a::Algorithm, o::Ordering) = sort!(copy(v), a, o)

sort!(v::OrderedDict, o::Ordering) = sort!(v, Sort.DEFAULT_STABLE, o)
sort (v::OrderedDict, o::Ordering) = sort (v, Sort.DEFAULT_STABLE, o)

for s in {:sort!, :sort, :sortperm}
    @eval begin
        # default to forward sort ordering
        $s(v::OrderedDict, a::Algorithm) = $s(v, a, Sort.Forward())
        $s(v::OrderedDict              ) = $s(v,    Sort.Forward())

        # auto-instntiate algorithms and orderings from types
        $s{A<:Algorithm,O<:Ordering}(v::OrderedDict, ::Type{A},    ::Type{O})   = $s(v, A(), O())
        $s{A<:Algorithm            }(v::OrderedDict, ::Type{A},    o::Ordering) = $s(v, A(), o)
        $s{             O<:Ordering}(v::OrderedDict, a::Algorithm, ::Type{O})   = $s(v, a,   O())
        $s{A<:Algorithm            }(v::OrderedDict, ::Type{A})                 = $s(v, A())
        $s{             O<:Ordering}(v::OrderedDict,               ::Type{O})   = $s(v,      O())

        # also allow ordering before algorithm
        $s                          (v::OrderedDict, o::Ordering, a::Algorithm) = $s(v, a, o)
        $s{A<:Algorithm,O<:Ordering}(v::OrderedDict, ::Type{O},   ::Type{A})    = $s(v, A(), O())
        $s{A<:Algorithm            }(v::OrderedDict, o::Ordering, ::Type{A})    = $s(v, A(), o)
        $s{             O<:Ordering}(v::OrderedDict, ::Type{O},   a::Algorithm) = $s(v, a,   O())
    end
end

for s in {:sort!, :sort, :sortperm}
    @eval begin
        $s{A<:Algorithm}(v::OrderedDict, a::Union(A,Type{A}), lt::Function) = $s(v, a, Sort.Lt(lt))
        $s{A<:Algorithm}(v::OrderedDict, lt::Function, a::Union(A,Type{A})) = $s(v, a, lt)
        $s              (v::OrderedDict, lt::Function)                      = $s(v, Sort.Lt(lt))
        $s              (lt::Function, v::OrderedDict, args...)             = $s(v, lt, args...)
    end
end

for (sb,s) in {(:sortby!, :sort!), (:sortby, :sort), (:sortpermby, :sortperm)}
    @eval begin
        $sb{A<:Algorithm}(v::OrderedDict, a::Union(A,Type{A}), by::Function) = $s(v, a, Sort.By(by))
        $sb{A<:Algorithm}(v::OrderedDict, by::Function, a::Union(A,Type{A})) = $s(v, a, Sort.By(by))
        $sb              (v::OrderedDict, by::Function)                      = $s(v, Sort.By(by))
        $sb              (by::Function, v::OrderedDict, args...)             = $s(v, Sort.By(by), args...)
    end
end

