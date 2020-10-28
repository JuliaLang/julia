# Dict

"""
    Dict

`Dict`s are simply dictionaries whose entries have a particular order.  The order
refers to insertion order, which allows deterministic iteration over the dictionary or set.
"""
mutable struct Dict{K,V} <: AbstractDict{K,V}
    slots::Array{Int32,1}
    keys::Array{K,1}
    vals::Array{V,1}
    ndel::Int
    dirty::Bool

    function Dict{K,V}() where {K,V}
        new{K,V}(zeros(Int32,16), Vector{K}(), Vector{V}(), 0, false)
    end
    function Dict{K,V}(kv) where {K,V}
        h = Dict{K,V}()
        for (k,v) in kv
            h[k] = v
        end
        return h
    end
    Dict{K,V}(p::Pair) where {K,V} = setindex!(Dict{K,V}(), p.second, p.first)
    function Dict{K,V}(ps::Pair...) where {K,V}
        h = Dict{K,V}()
        sizehint!(h, length(ps))
        for p in ps
            h[p.first] = p.second
        end
        return h
    end
    function Dict{K,V}(d::Dict{K,V}) where {K,V}
        if d.ndel > 0
            rehash!(d)
        end
        @assert d.ndel == 0
        new{K,V}(copy(d.slots), copy(d.keys), copy(d.vals), 0)
    end
end
Dict() = Dict{Any,Any}()
Dict(kv::Tuple{}) = Dict()
copy(d::Dict) = Dict(d)

# TODO: this can probably be simplified using `eltype` as a THT (Tim Holy trait)
# Dict{K,V}(kv::Tuple{Vararg{Tuple{K,V}}})     = Dict{K,V}(kv)
# Dict{K  }(kv::Tuple{Vararg{Tuple{K,Any}}})   = Dict{K,Any}(kv)
# Dict{V  }(kv::Tuple{Vararg{Tuple{Any,V}}})   = Dict{Any,V}(kv)

# problematic line: Dict(kv::Tuple{Vararg{Pair{K,V}}}) where {K,V}  = Dict{K,V}(kv)

Dict(kv::AbstractArray{Tuple{K,V}}) where {K,V} = Dict{K,V}(kv)
Dict(kv::AbstractArray{Pair{K,V}}) where {K,V}  = Dict{K,V}(kv)
Dict(kv::AbstractDict{K,V}) where {K,V}         = Dict{K,V}(kv)

Dict(ps::Pair{K,V}...) where {K,V} = Dict{K,V}(ps)
Dict(ps::Pair...)                  = Dict(ps)

function Dict(kv)
    try
        dict_with_eltype((K, V) -> Dict{K, V}, kv, eltype(kv))
    catch e
        if isempty(methods(iterate, (typeof(kv),))) ||
            !all(x->isa(x, Union{Tuple,Pair}), kv)
            throw(ArgumentError("Dict(kv): kv needs to be an iterator of tuples or pairs"))
        else
            rethrow(e)
        end
    end
end

empty(d::Dict{K,V}) where {K,V} = Dict{K,V}()
empty(d::Dict, ::Type{K}, ::Type{V}) where {K, V} = Dict{K, V}()

length(d::Dict) = length(d.keys) - d.ndel
isempty(d::Dict) = (length(d) == 0)

"""
    isordered(::Type)

Property of associative containers, that is `true` if the container type has a
defined order (such as `Dict` and `SortedDict`), and `false` otherwise.
"""
isordered(::Type{T}) where {T<:AbstractDict} = false
isordered(::Type{T}) where {T<:Dict} = true

# conversion between Dict types
function convert(::Type{Dict{K,V}}, d::AbstractDict) where {K,V}
    if !isordered(typeof(d))
        Base.depwarn("Conversion to Dict is deprecated for unordered associative containers (in this case, $(typeof(d))). Use an ordered or sorted associative type, such as SortedDict and Dict.", :convert)
    end
    h = Dict{K,V}()
    for (k,v) in d
        ck = convert(K,k)
        if !haskey(h,ck)
            h[ck] = convert(V,v)
        else
            error("key collision during dictionary conversion")
        end
    end
    return h
end
convert(::Type{Dict{K,V}},d::Dict{K,V}) where {K,V} = d

function rehash!(h::Dict{K,V}, newsz = length(h.slots)) where {K,V}
    olds = h.slots
    keys = h.keys
    vals = h.vals
    sz = length(olds)
    newsz = _tablesz(newsz)
    h.dirty = true
    count0 = length(h)
    if count0 == 0
        resize!(h.slots, newsz)
        fill!(h.slots, 0)
        resize!(h.keys, 0)
        resize!(h.vals, 0)
        h.ndel = 0
        return h
    end

    slots = zeros(Int32, newsz)

    if h.ndel > 0
        ndel0 = h.ndel
        ptrs = !isbitstype(K)
        to = 1
        # TODO: to get the best performance we need to avoid reallocating these.
        # This algorithm actually works in place, unless the dict is modified
        # due to GC during this process.
        newkeys = similar(keys, count0)
        newvals = similar(vals, count0)
        @inbounds for from = 1:length(keys)
            if !ptrs || isassigned(keys, from)
                k = keys[from]
                hashk = hash(k)%Int
                isdeleted = false
                if !ptrs
                    iter = 0
                    maxprobe = max(16, sz>>6)
                    index = (hashk & (sz-1)) + 1
                    while iter <= maxprobe
                        si = olds[index]
                        #si == 0 && break  # shouldn't happen
                        si == from && break
                        si == -from && (isdeleted=true; break)
                        index = (index & (sz-1)) + 1
                        iter += 1
                    end
                end
                if !isdeleted
                    index = (hashk & (newsz-1)) + 1
                    while slots[index] != 0
                        index = (index & (newsz-1)) + 1
                    end
                    slots[index] = to
                    newkeys[to] = k
                    newvals[to] = vals[from]
                    to += 1
                end
                if h.ndel != ndel0
                    # if items are removed by finalizers, retry
                    return rehash!(h, newsz)
                end
            end
        end
        h.keys = newkeys
        h.vals = newvals
        h.ndel = 0
    else
        @inbounds for i = 1:count0
            k = keys[i]
            index = hashindex(k, newsz)
            while slots[index] != 0
                index = (index & (newsz-1)) + 1
            end
            slots[index] = i
            if h.ndel > 0
                # if items are removed by finalizers, retry
                return rehash!(h, newsz)
            end
        end
    end

    h.slots = slots
    return h
end

function sizehint!(d::Dict, newsz)
    slotsz = (newsz*3)>>1
    oldsz = length(d.slots)
    if slotsz <= oldsz
        # todo: shrink
        # be careful: rehash!() assumes everything fits. it was only designed
        # for growing.
        return d
    end
    # grow at least 25%
    slotsz = max(slotsz, (oldsz*5)>>2)
    rehash!(d, slotsz)
end

function empty!(h::Dict{K,V}) where {K,V}
    fill!(h.slots, 0)
    empty!(h.keys)
    empty!(h.vals)
    h.ndel = 0
    h.dirty = true
    return h
end

# get the index where a key is stored, or -1 if not present
function ht_keyindex(h::Dict{K,V}, key, direct) where {K,V}
    slots = h.slots
    sz = length(slots)
    iter = 0
    maxprobe = max(16, sz>>6)
    index = hashindex(key, sz)
    keys = h.keys

    @inbounds while iter <= maxprobe
        si = slots[index]
        si == 0 && break
        if si > 0 && isequal(key, keys[si])
            return ifelse(direct, oftype(index, si), index)
        end

        index = (index & (sz-1)) + 1
        iter += 1
    end

    return -1
end

# get the index where a key is stored, or -pos if not present
# and the key would be inserted at pos
# This version is for use by setindex! and get!
function ht_keyindex2(h::Dict{K,V}, key) where {K,V}
    slots = h.slots
    sz = length(slots)
    iter = 0
    maxprobe = max(16, sz>>6)
    index = hashindex(key, sz)
    keys = h.keys

    @inbounds while iter <= maxprobe
        si = slots[index]
        if si == 0
            return -index
        elseif si > 0 && isequal(key, keys[si])
            return oftype(index, si)
        end

        index = (index & (sz-1)) + 1
        iter += 1
    end

    rehash!(h, length(h) > 64000 ? sz*2 : sz*4)

    return ht_keyindex2(h, key)
end

function _setindex!(h::Dict, v, key, index)
    hk, hv = h.keys, h.vals
    #push!(h.keys, key)
    ccall(:jl_array_grow_end, Cvoid, (Any, UInt), hk, 1)
    nk = length(hk)
    @inbounds hk[nk] = key
    #push!(h.vals, v)
    ccall(:jl_array_grow_end, Cvoid, (Any, UInt), hv, 1)
    @inbounds hv[nk] = v
    @inbounds h.slots[index] = nk
    h.dirty = true

    sz = length(h.slots)
    cnt = nk - h.ndel
    # Rehash now if necessary
    if h.ndel >= ((3*nk)>>2) || cnt*3 > sz*2
        # > 3/4 deleted or > 2/3 full
        rehash!(h, cnt > 64000 ? cnt*2 : cnt*4)
    end
end

function setindex!(h::Dict{K,V}, v0, key0) where {K,V}
    key = convert(K, key0)
    if !isequal(key, key0)
        throw(ArgumentError("$key0 is not a valid key for type $K"))
    end
    v = convert(V, v0)

    index = ht_keyindex2(h, key)

    if index > 0
        @inbounds h.keys[index] = key
        @inbounds h.vals[index] = v
    else
        _setindex!(h, v, key, -index)
    end

    return h
end

function get!(h::Dict{K,V}, key0, default) where {K,V}
    key = convert(K, key0)
    if !isequal(key, key0)
        throw(ArgumentError("$key0 is not a valid key for type $K"))
    end

    index = ht_keyindex2(h, key)

    index > 0 && return h.vals[index]

    v = convert(V,  default)
    _setindex!(h, v, key, -index)
    return v
end

function get!(default::Base.Callable, h::Dict{K,V}, key0) where {K,V}
    key = convert(K, key0)
    if !isequal(key, key0)
        throw(ArgumentError("$key0 is not a valid key for type $K"))
    end

    index = ht_keyindex2(h, key)

    index > 0 && return h.vals[index]

    h.dirty = false
    v = convert(V,  default())
    if h.dirty  # calling default could have dirtied h
        index = ht_keyindex2(h, key)
    end
    if index > 0
        h.keys[index] = key
        h.vals[index] = v
    else
        _setindex!(h, v, key, -index)
    end
    return v
end

function getindex(h::Dict{K,V}, key) where {K,V}
    index = ht_keyindex(h, key, true)
    return (index<0) ? throw(KeyError(key)) : h.vals[index]::V
end

function get(h::Dict{K,V}, key, default) where {K,V}
    index = ht_keyindex(h, key, true)
    return (index<0) ? default : h.vals[index]::V
end

function get(default::Base.Callable, h::Dict{K,V}, key) where {K,V}
    index = ht_keyindex(h, key, true)
    return (index<0) ? default() : h.vals[index]::V
end

haskey(h::Dict, key) = (ht_keyindex(h, key, true) >= 0)
in(key, v::Base.KeySet{K,T}) where {K,T<:Dict{K}} = (ht_keyindex(v.dict, key, true) >= 0)

function getkey(h::Dict{K,V}, key, default) where {K,V}
    index = ht_keyindex(h, key, true)
    return (index<0) ? default : h.keys[index]::K
end

function _pop!(h::Dict, index)
    @inbounds val = h.vals[h.slots[index]]
    _delete!(h, index)
    return val
end

function pop!(h::Dict)
    h.ndel > 0 && rehash!(h)
    key = h.keys[end]
    index = ht_keyindex(h, key, false)
    return key => _pop!(h, index)
end

function pop!(h::Dict, key)
    index = ht_keyindex(h, key, false)
    index > 0 ? _pop!(h, index) : throw(KeyError(key))
end

function pop!(h::Dict, key, default)
    index = ht_keyindex(h, key, false)
    index > 0 ? _pop!(h, index) : default
end

function _delete!(h::Dict, index)
    @inbounds ki = h.slots[index]
    @inbounds h.slots[index] = -ki
    ccall(:jl_arrayunset, Cvoid, (Any, UInt), h.keys, ki-1)
    ccall(:jl_arrayunset, Cvoid, (Any, UInt), h.vals, ki-1)
    h.ndel += 1
    h.dirty = true
    return h
end

function delete!(h::Dict, key)
    index = ht_keyindex(h, key, false)
    if index > 0; _delete!(h, index); end
    return h
end

function iterate(t::Dict)
    t.ndel > 0 && rehash!(t)
    length(t.keys) < 1 && return nothing
    return (Pair(t.keys[1], t.vals[1]), 2)
end
function iterate(t::Dict, i)
    length(t.keys) < i && return nothing
    return (Pair(t.keys[i], t.vals[i]), i+1)
end

function _merge_kvtypes(d, others...)
    K, V = keytype(d), valtype(d)
    for other in others
        K = promote_type(K, keytype(other))
        V = promote_type(V, valtype(other))
    end
    return (K,V)
end

function merge(d::Dict, others::AbstractDict...)
    K,V = _merge_kvtypes(d, others...)
    merge!(Dict{K,V}(), d, others...)
end

function merge(combine::Function, d::Dict, others::AbstractDict...)
    K,V = _merge_kvtypes(d, others...)
    merge!(combine, Dict{K,V}(), d, others...)
end

function Base.map!(f, iter::Base.ValueIterator{<:Dict})
    dict = iter.dict
    vals = dict.vals
    elements = length(vals) - dict.ndel
    elements == 0 && return iter
    for i in dict.slots
        if i > 0
            @inbounds vals[i] = f(vals[i])
            elements -= 1
            elements == 0 && break
        end
    end
    return iter
end

@propagate_inbounds isslotempty(h::Dict, i::Int) = h.slots[i] == 0
@propagate_inbounds isslotfilled(h::Dict, i::Int) = h.slots[i] > 0

const AnyDict = Dict{Any,Any}
