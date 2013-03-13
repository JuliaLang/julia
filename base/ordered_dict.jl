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

###################
## DictItem type ##

type DictItem{K,V}
    k::K
    v::V
    idx::Int
end

######################
## OrderedDict type ##

type OrderedDict{K,V} <: Associative{K,V}
    ht::Dict{K,DictItem{K,V}}
    ord::Vector{DictItem{K,V}}
    ord_slots::BitArray
    ndel::Int

    OrderedDict() = new(Dict{K,DictItem{K,V}}(), similar(Array(DictItem{K,V},1), 0), BitArray(), 0)
    function OrderedDict(ks,vs)
        d = new(Dict{K,DictItem{K,V}}(), similar(Array(DictItem{K,V},1), 0), BitArray(), 0)
        for (i, (k, v)) in enumerate(zip(ks, vs))
            item = DictItem{K,V}(k,v,i)
            d.ht[k] = item
            push!(d.ord, item)
            push!(d.ord_slots, true)
        end
        d
    end
end
OrderedDict() = OrderedDict{Any, Any}()
OrderedDict(ks, vs) = OrderedDict{Any,Any}(ks, vs)
OrderedDict{K,V}(ks::AbstractArray{K}, vs::AbstractArray{V}) = OrderedDict{K,V}(ks, vs)

# syntax entry points
OrderedDict{K,V}(ks::(K...), vs::(V...)) = OrderedDict{K  ,V  }(ks, vs)
OrderedDict{K  }(ks::(K...), vs::Tuple ) = OrderedDict{K  ,Any}(ks, vs)
OrderedDict{V  }(ks::Tuple , vs::(V...)) = OrderedDict{Any,V  }(ks, vs)
OrderedDict{K,V}(kvs::AbstractArray{(K,V)}) = OrderedDict{K,V}(zip(kvs...)...)
OrderedDict{K,V}(kvs::AbstractArray{DictItem{K,V}}) = OrderedDict{K,V}(zip(kvs...)...)
#OrderedDict{K,V}(d::Associative{K,V}) = OrderedDict{K,V}(collect(d))  ## Why doesn't this work?
OrderedDict{K,V}(d::Associative{K,V}) = OrderedDict(collect(d))

##########################
## Construction-related ##
similar{K,V}(d::OrderedDict{K,V}) = OrderedDict{K,V}()
sizehint(d::OrderedDict, newsz) = sizehint(d.ht, newsz)

###################
## Serialization ##

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
## OrderedDict Utility functions ##

# Removes empty slots of order array in OrderedDict
function _compact(d::OrderedDict)
    d.ndel == 0 && return

    ord = d.ord
    slots = d.ord_slots

    # start with first empty slot
    s_pos = findfirstnot(slots)
    ord_pos = findnext(slots, s_pos)
    
    # fill down empty slots with consecutive filled slots
    while ord_pos != 0
        item = ord[s_pos] = ord[ord_pos]
        item.idx = s_pos
        
        s_pos += 1
        ord_pos = findnext(slots, ord_pos+1)
    end
    
    new_sz = length(d)
    resize!(d.ord, new_sz)
    resize!(d.ord_slots, new_sz)
    slots[1:end] = true
    d.ndel = 0

    nothing
end

###############
## Iteration ##

skip_deleted(d::OrderedDict, i) = findnext(d.ord_slots, i)

start(d::OrderedDict) = length(d.ord_slots) > 0 && skip_deleted(d,1)
done(d::OrderedDict, i) = (i == 0)
next(d::OrderedDict, i) = ((item=d.ord[i]; (item.k, item.v)), skip_deleted(d,i+1))

#########################
## General Collections ##

isempty(d::OrderedDict) = isempty(d.ht)
length(d::OrderedDict) = length(d.ht)      # d.ord may have empty slots

function empty!(d::OrderedDict)
    empty!(d.ht)
    empty!(d.ord)
    empty!(d.ord_slots)
    d.ndel = 0
end

###########################
## Indexable Collections ##

# As with getindex, we want to allow assignment by index position as well, as key,
# but we ignore this when K<:Number
function setindex!{K,V}(d::OrderedDict{K,V}, v, key)
    # 3/4 deleted?
    if d.ndel >= ((3*length(d))>>2)
        _compact(d)
    end

    if has(d, key)
        d.ht[key].v = v
    else
        item = DictItem{K,V}(key, v, length(d.ord)+1)
        d.ht[key] = item
        push!(d.ord, item)
        push!(d.ord_slots, true)
    end
    d
end

function setindex!{K,V}(h::OrderedDict{K,V}, kv::(Any,Any), index::Integer)
    (key,v) = kv
    ord_idx = indexof(h,key,0)
    if ord_idx == index
        return setindex!(h, v, key)
    end
    # TODO: this can made be more efficient
    delete!(h, getitem(h, index)[1])
    insert!(h, index, kv)
end

setindex!{K<:Number,V}(h::OrderedDict{K,V}, v::(Any,Any), key::Integer) = 
    invoke(setindex!, (OrderedDict{K,V}, Any, Any), h, v, key)
setindex!{K<:Number,V}(h::OrderedDict{K,V}, v, key::Integer) = 
    invoke(setindex!, (OrderedDict{K,V}, Any, Any), h, v, key)

# We want to allow the user to access the (k,v) pairs by index
# However, first and foremost, this is a dictionary, so if the
# keys are numbers, assume any reference using an integer
# as a key is attempting a has lookup.

# TODO: This might be confusing behavior, so consider disabling
# and simply sequential access through getitem()

getindex{K,        V}(h::OrderedDict{K,V}, key)              = getindex(h.ht, key).v
getindex{K,        V}(h::OrderedDict{K,V}, ord_idx::Integer) = getitem(h, ord_idx)
getindex{K<:Number,V}(h::OrderedDict{K,V}, key::Integer)     = getindex(h.ht, key).v

indexof{K,V}(h::OrderedDict{K,V}, key)        = (_compact(h); h.ht[key].idx)
indexof{K,V}(h::OrderedDict{K,V}, key, deflt) = has(h.ht, key) ? indexof(h, key) : deflt
findfirst(h::OrderedDict, v) = indexof(h, v, 0)
findnext(h::OrderedDict, v, start::Int) = (idx=indexof(h,v,0); idx >= start? idx : 0)

first(h::OrderedDict) = getitem(h, 1)
last(h::OrderedDict) = getitem(h, length(h))
endof(h::OrderedDict) = length(h)

function reverse!(h::OrderedDict)
    _compact(h)
    reverse!(h.ord)
    _update_order(h, 1, length(h))
    h
end

function reverse(h::OrderedDict)
    d = similar(h)
    sizehint(d, length(h.ht.slots))
    _compact(h)
    for item in reverse(h.ord)
        d[item.k] = item.v
    end
    d
end

#############################
## Associative Collections ##

has{K,V}(d::OrderedDict{K,V}, key) = has(d.ht, key)

get{K,V}(d::OrderedDict{K,V}, key, default) = has(d, key) ? d[key] : default
getkey{K,V}(d::OrderedDict{K,V}, key, default) = has(d, key) ? key : default
getitem{K,V}(h::OrderedDict{K,V}, idx::Int) = (_compact(h); item = h.ord[idx]; (item.k, item.v))

function delete!{K,V}(d::OrderedDict{K,V}, key)
    item = d.ht[key]
    delete!(d.ht, key)
    # Is this right?
    ccall(:jl_arrayunset, Void, (Any, Uint), d.ord, item.idx-1)
    d.ord_slots[item.idx] = false
    d.ndel += 1

    item.v
end

delete!{K,V}(d::OrderedDict{K,V}, key, default) = has(d, key) ? delete!(d, key) : default

function delete!{K,V}(h::OrderedDict{K,V}, ord_idx::Integer)
    key = h.ord[ord_idx].k
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
    _compact(d)
    key = d.ord[end].k
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
    key = d.ord[idx].k
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
# first, and this might decrement the inserted position by one
function insert!{K,V}(h::OrderedDict{K,V}, index::Integer, item::(Any,Any))
    (key,v) = item
    # Do we need to subtract 1 from index?
    cur_index = indexof(h, key, 0)
    if cur_index > 0 && index > cur_index
        index -= 1
    end
    # Add/set the element
    h[key] = v

    # Shift the element, if necessary
    cur_index = indexof(h, key)          # calls _compact()
    if cur_index == index
        return item::(K,V)
    end
    # _compact(h)  ## called in indexof() above...
    move_item!(h.ord, cur_index, index)
    _update_order(h, index, cur_index)
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
        h.ord[i].idx = i
    end
end

# Sorting
import Sort.Ordering, Sort.Algorithm

function sort!(h::OrderedDict, args...)
    _compact(h)
    p = sortperm(keys(h), args...)
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

