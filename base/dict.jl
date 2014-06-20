# generic operations on associative collections

# generic operations on associative collections
abstract Associative{K,V} # TODO: change to Dictionary

## Interface
#
# Implemented by Associative:
#* haskey
#* copy
#* merge(!)
#* filter(!)
#* ==
#* convert
#* similar
#* push!
#* getindex
#* in
#
# To implement by the types:
# get(!)
# getkey
# pop!
# keys
# values
# empty!
# length
# isempty, eltype
# start, next, done
# sizehint
#
## Deprecated:
# delete!

const secret_table_token = :__c782dbf1cf4d6a2e5e3865d7e95634f2e09b5902__

haskey(d::Associative, k) = in(k, keys(d))

function in(p::(Any,Any), a::Associative)
    v = get(a,p[1],secret_table_token)
    !is(v, secret_table_token) && (v == p[2])
end

function summary(t::Associative)
    n = length(t)
    string(typeof(t), " with ", n, (n==1 ? " entry" : " entries"))
end

function show{K,V}(io::IO, t::Associative{K,V})
    if isempty(t)
        print(io, typeof(t),"()")
    else
        if K === Any && V === Any
            delims = ['{','}']
        else
            delims = ['[',']']
        end
        print(io, delims[1])
        first = true
        for (k, v) in t
            first || print(io, ',')
            first = false
            show(io, k)
            print(io, "=>")
            show(io, v)
        end
        print(io, delims[2])
    end
end

function _truncate_at_width_or_chars(str, width, chars="", truncmark="…")
    truncwidth = strwidth(truncmark)
    (width <= 0 || width < truncwidth) && return ""

    wid = truncidx = lastidx = 0
    idx = start(str)
    while !done(str, idx)
        lastidx = idx
        c, idx = next(str, idx)
        wid += charwidth(c)
        wid >= width - truncwidth && truncidx == 0 && (truncidx = lastidx)
        (wid >= width || c in chars) && break
    end

    str[lastidx] in chars && (lastidx = prevind(str, lastidx))
    truncidx == 0 && (truncidx = lastidx)
    if lastidx < sizeof(str)
        return bytestring(SubString(str, 1, truncidx) * truncmark)
    else
        return bytestring(str)
    end
end

showdict(t::Associative; kw...) = showdict(STDOUT, t; kw...)
function showdict{K,V}(io::IO, t::Associative{K,V}; limit::Bool = false,
                       sz=(s = tty_size(); (s[1]-3, s[2])))
    rows, cols = sz
    print(io, summary(t))
    isempty(t) && return
    print(io, ":")

    if limit
        rows < 2   && (print(io, " …"); return)
        cols < 12  && (cols = 12) # Minimum widths of 2 for key, 4 for value
        cols -= 6 # Subtract the widths of prefix "  " separator " => "
        rows -= 2 # Subtract the summary and final ⋮ continuation lines

        # determine max key width to align the output, caching the strings
        ks = Array(String, min(rows, length(t)))
        keylen = 0
        for (i, k) in enumerate(keys(t))
            i > rows && break
            ks[i] = sprint(show, k)
            keylen = clamp(length(ks[i]), keylen, div(cols, 3))
        end
    end

    for (i, (k, v)) in enumerate(t)
        print(io, "\n  ")
        limit && i > rows && (print(io, rpad("⋮", keylen), " => ⋮"); break)

        if limit
            key = rpad(_truncate_at_width_or_chars(ks[i], keylen, "\r\n"), keylen)
        else
            key = sprint(show, k)
        end
        print(io, key)
        print(io, " => ")

        val = sprint(show, v)
        if limit
            val = _truncate_at_width_or_chars(val, cols - keylen, "\r\n")
        end
        print(io, val)
    end
end

immutable KeyIterator{T<:Associative}
    dict::T
end
immutable ValueIterator{T<:Associative}
    dict::T
end

summary{T<:Union(KeyIterator,ValueIterator)}(iter::T) =
    string(T.name, " for a ", summary(iter.dict))

show(io::IO, iter::Union(KeyIterator,ValueIterator)) = show(io, collect(iter))

showkv(iter::Union(KeyIterator,ValueIterator); kw...) = showkv(STDOUT, iter; kw...)
function showkv{T<:Union(KeyIterator,ValueIterator)}(io::IO, iter::T; limit::Bool = false,
                                                     sz=(s = tty_size(); (s[1]-3, s[2])))
    rows, cols = sz
    print(io, summary(iter))
    isempty(iter) && return
    print(io, ". ", T<:KeyIterator ? "Keys" : "Values", ":")
    if limit
        rows < 2 && (print(io, " …"); return)
        cols < 4 && (cols = 4)
        cols -= 2 # For prefix "  "
        rows -= 2 # For summary and final ⋮ continuation lines
    end

    for (i, v) in enumerate(iter)
        print(io, "\n  ")
        limit && i >= rows && (print(io, "⋮"); break)

        str = sprint(show, v)
        limit && (str = _truncate_at_width_or_chars(str, cols, "\r\n"))
        print(io, str)
    end
end

length(v::Union(KeyIterator,ValueIterator)) = length(v.dict)
isempty(v::Union(KeyIterator,ValueIterator)) = isempty(v.dict)
eltype(v::KeyIterator) = eltype(v.dict)[1]
eltype(v::ValueIterator) = eltype(v.dict)[2]

start(v::Union(KeyIterator,ValueIterator)) = start(v.dict)
done(v::Union(KeyIterator,ValueIterator), state) = done(v.dict, state)

function next(v::KeyIterator, state)
    n = next(v.dict, state)
    n[1][1], n[2]
end

function next(v::ValueIterator, state)
    n = next(v.dict, state)
    n[1][2], n[2]
end

in(k, v::KeyIterator) = !is(get(v.dict, k, secret_table_token),
                            secret_table_token)

keys(a::Associative) = KeyIterator(a)
values(a::Associative) = ValueIterator(a)

function copy(a::Associative)
    b = similar(a)
    for (k,v) in a
        b[k] = v
    end
    return b
end

function merge!(d::Associative, others::Associative...)
    for other in others
        for (k,v) in other
            d[k] = v
        end
    end
    return d
end
merge(d::Associative, others::Associative...) = merge!(copy(d), others...)

function filter!(f::Function, d::Associative)
    for (k,v) in d
        if !f(k,v)
            delete!(d,k)
        end
    end
    return d
end
filter(f::Function, d::Associative) = filter!(f,copy(d))

eltype{K,V}(a::Associative{K,V}) = (K,V)

function isequal(l::Associative, r::Associative)
    if isa(l,ObjectIdDict) != isa(r,ObjectIdDict)
        return false
    end
    if length(l) != length(r) return false end
    for (key, value) in l
        if !isequal(value, get(r, key, secret_table_token))
            return false
        end
    end
    true
end

function ==(l::Associative, r::Associative)
    if isa(l,ObjectIdDict) != isa(r,ObjectIdDict)
        return false
    end
    if length(l) != length(r) return false end
    for (key, value) in l
        if value != get(r, key, secret_table_token)
            return false
        end
    end
    true
end

# conversion between Dict types
function convert{K,V}(T::Type{Associative{K,V}},d::Associative)
    h = T{K,V}()
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
convert{K,V}(T::Type{Associative{K,V}},d::Associative{K,V}) = d

# serialisation
function serialize(s, t::Associative)
    serialize_type(s, typeof(t))
    write(s, int32(length(t)))
    for (k,v) in t
        serialize(s, k)
        serialize(s, v)
    end
end

function deserialize{K,V}(s, T::Type{Associative{K,V}})
    n = read(s, Int32)
    t = T(); sizehint(t, n)
    for i = 1:n
        k = deserialize(s)
        v = deserialize(s)
        t[k] = v
    end
    return t
end

similar(d::Associative) = typeof(d)()

# some support functions

function getindex(t::Associative, key)
    v = get(t, key, secret_table_token)
    if is(v, secret_table_token)
        throw(KeyError(key))
    end
    return v
end

# t[k1,k2,ks...] is syntactic sugar for t[(k1,k2,ks...)].  (Note
# that we need to avoid dispatch loops if setindex!(t,v,k) is not defined.)
getindex(t::Associative, k1, k2, ks...) = getindex(t, tuple(k1,k2,ks...))
setindex!(t::Associative, v, k1, k2, ks...) = setindex!(t, v, tuple(k1,k2,ks...))

push!(t::Associative, key, v) = setindex!(t, v, key)


######
# Old ObjectIdDict

type ObjectIdDict <: Associative{Any,Any}
    ht::Array{Any,1}
    ObjectIdDict() = new(cell(32))

    function ObjectIdDict(itr)
        d = ObjectIdDict()
        for (k,v) in itr
            d[k] = v
        end
        d
    end

    function ObjectIdDict(o::ObjectIdDict)
        N = length(o.ht)
        ht = cell(N)
        ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Uint),
              ht, o.ht, N*sizeof(Ptr))
        new(ht)
    end
end

function setindex!(t::ObjectIdDict, v::ANY, k::ANY)
    t.ht = ccall(:jl_eqtable_put, Array{Any,1}, (Any, Any, Any), t.ht, k, v)
    return t
end

get(t::ObjectIdDict, key::ANY, default::ANY) =
    ccall(:jl_eqtable_get, Any, (Any, Any, Any), t.ht, key, default)

pop!(t::ObjectIdDict, key::ANY, default::ANY) =
    ccall(:jl_eqtable_pop, Any, (Any, Any, Any), t.ht, key, default)

function pop!(t::ObjectIdDict, key::ANY)
    val = pop!(t, key, secret_table_token)
    !is(val,secret_table_token) ? val : throw(KeyError(key))
end

function delete!(t::ObjectIdDict, key::ANY)
    ccall(:jl_eqtable_pop, Any, (Any, Any), t.ht, key)
    t
end

empty!(t::ObjectIdDict) = (t.ht = cell(length(t.ht)); t)

start(t::ObjectIdDict) = 0
done(t::ObjectIdDict, i) = is(next(t,i),())
next(t::ObjectIdDict, i) = ccall(:jl_eqtable_next, Any, (Any, Uint32), t.ht, i)

isempty(t::ObjectIdDict) = is(next(t,0),())

function length(d::ObjectIdDict)
    n = 0
    for pair in d
        n+=1
    end
    n
end

copy(o::ObjectIdDict) = ObjectIdDict(o)

###########
# Hash-table based dictionaries
abstract HashDictionary{K,V} <: Associative{K,V}
# it is assumed that the concrete types are constructed with @makeHashDictionary
# or have the same internal structure.

# constants
const ISEMPTY = 0x0
const ISFILLED = 0x1
const ISMISSING = 0x2

## helper types
immutable Unordered end
typealias Ordered Int
# This slows it down by a factor of 4 compared to the typealias!
# immutable Ordered 
#     a::Int
# end
# getindex(ord::Vector{Ordered}, i::Integer) = invoke(getindex, (Vector, Int), ord, int(i)).a
# setindex!(ord::Vector{Ordered}, val::Integer, i::Integer) = Core.arrayset(ord, Ordered(val), int(i))
# push!(ord::Vector{Ordered}, val::Integer) = invoke(push!, (Vector, Ordered), ord, Ordered(val))

## Common internal and external interface of HashDictionary which may
## need re-definition for special types of HashDictionary's.
numslots(h::HashDictionary) = length(h.slots)

# Transforms a key into an index.  sz has to be a power of 2.
hashindex(::HashDictionary, key, sz) = (int(hash(key)) & (sz-1)) + 1

isslotempty(h::HashDictionary, i::Int) = h.slots[i] == ISEMPTY
isslotfilled(h::HashDictionary, i::Int) = h.slots[i] == ISFILLED
isslotmissing(h::HashDictionary, i::Int) = h.slots[i] == ISMISSING

# These functions to access the h.keys and h.vals array, in case a transformation
# of the key is needed before setting/getting it:

# transforms key at index ind:
gkey(h::HashDictionary, ind) = h.keys[ind]  
# transform key back before setting it:
skey!(h::HashDictionary, key, ind) = (h.keys[ind] = key)
skey!(::HashDictionary, ar::Vector, key, ind) = (ar[ind] = key)
# transforms val at index ind:
gval(h::HashDictionary, ind) = h.vals[ind]  
# transform val back before setting it:
sval!(h::HashDictionary, val, ind) = (h.vals[ind] = val)
sval!(::HashDictionary, ar::Vector, val, ind) = (ar[ind] = val)

# key checking & converting as it comes in 
function keyconvert{K,V}(h::HashDictionary{K,V}, key0)
    key = convert(K,key0)
    if !isequal(key, key0)
        error(key0, " is not a valid key for type ", K)
    end
    key
end

# Entries which should be purged during calls of rehash and
# ht_keyindex.  For instance for weak-key dicts the reference may have
# been gc-ed.
topurge(::HashDictionary, key) = false

isordered(h::HashDictionary) = eltype(h.idxs)==Ordered

# new table size
_tablesz(x::Integer) = x < 16 ? 16 : one(x)<<((sizeof(x)<<3)-leading_zeros(x-1))

function rehash{K,V}(h::HashDictionary{K,V}, newsz)
    sz = numslots(h)
    newsz = _tablesz(newsz)
    if h.count == 0
        resize!(h.slots, newsz)
        fill!(h.slots, ISEMPTY)
        resize!(h.keys, newsz)
        resize!(h.vals, newsz)
        resize!(h.idxs, newsz)
        resize!(h.order, 0)
        sizehint(h.order, newsz) # TODO: profile whether this makes it better. 
        h.ndel = 0
        return h
    end
    ordered = isordered(h)
    if ordered
        _compact_order!(h)
    end

    slots = zeros(Uint8,newsz) # zero==ISEMPTY
    keys = Array(eltype(h.keys), newsz)
    vals = Array(eltype(h.vals), newsz)
    idxs = Array(eltype(h.idxs), newsz)
    order = Array(eltype(h.order), h.count)
    sizehint(order, newsz) # TODO: profile whether this makes it better. 
    count0 = h.count
    count = 0

    for i = 1:sz
        if h.slots[i] == ISFILLED
            k = gkey(h,i)
            if topurge(h,k)
                continue
            end
            v = gval(h,i)
            index = hashindex(h, k, newsz)
            while slots[index] != 0
                # adds one to index, wrapping around at newsz
                index = (index & (newsz-1)) + 1
            end
            slots[index] = ISFILLED
            skey!(h, keys, k, index)
            sval!(h, vals, v, index)
            if ordered
                idx = h.idxs[i]
                idxs[index] = idx
                order[idx] = index
            end
            count += 1

            if h.count != count0
                # if items are removed by finalizers, retry
                return rehash(h, newsz)
            end
        end
    end
    h.slots = slots
    h.keys = keys
    h.vals = vals
    h.idxs = idxs
    h.order = order
    h.count = count
    h.ndel = 0
    return h
end

# this is only used for ordered dicts
function _compact_order!(h::HashDictionary)
    if h.count == length(h.order)
        return
    end

    i = 1
    while h.order[i] > 0;  i += 1; end

    j = i+1
    while h.order[j] == 0; j += 1; end

    for k = j:length(h.order)
        idx = h.order[k]
        if idx > 0
            h.order[i] = idx
            h.idxs[idx] = i
            i += 1
        end
    end

    resize!(h.order, h.count)
    nothing
end

function sizehint(h::HashDictionary, newsz)
    oldsz = numslots(h)
    if newsz <= oldsz
        # todo: shrink
        # be careful: rehash() assumes everything fits. it was only designed
        # for growing.
        return h
    end
    # grow at least 25%
    newsz = max(newsz, (oldsz*5)>>2)
    rehash(h, newsz)
end

function empty!{K,V}(h::HashDictionary{K,V})
    fill!(h.slots, ISEMPTY)
    sz = numslots(h)
    h.keys = Array(eltype(h.keys), sz)
    h.vals = Array(eltype(h.vals), sz)
    h.idxs = Array(eltype(h.idxs), sz)
    h.order = Array(eltype(h.idxs), 0)
    h.ndel = 0
    h.count = 0
    return h
end


# get the index where a key is stored, or -1 if not present
function ht_keyindex{K,V}(h::HashDictionary{K,V}, key)
    sz = numslots(h)
    iter = 0
    maxprobe = max(16, sz>>6)
    index = hashindex(h, key, sz)

    while true
        if isslotempty(h,index)
            break
        end
        if !isslotmissing(h,index) && isequal(key, gkey(h, index))
            return index
        end
        topurge(h,key) && _delete!(h, index)

        index = (index & (sz-1)) + 1
        iter+=1
        iter > maxprobe && break
    end

    return -1
end

# Get the index where a key is stored, or -pos if not present
# and the key would be inserted at pos.
# This version is for use by setindex! and get!
function ht_keyindex!{K,V}(h::HashDictionary{K,V}, key)
    sz = numslots(h)
    iter = 0
    maxprobe = max(16, sz>>6)
    index = hashindex(h, key, sz)
    avail = 0

    while true
        if isslotempty(h,index)
            avail < 0 && return avail
            return -index
        end

        if isslotmissing(h,index)
            if avail == 0
                # found an available slot, but need to keep scanning
                # in case "key" already exists in a later collided slot.
                avail = -index
            end
        elseif isequal(key, gkey(h, index))
            return index
        end
        topurge(h,key) && _delete!(h, index)

        index = (index & (sz-1)) + 1
        iter+=1
        iter > maxprobe && break
    end

    avail < 0 && return avail

    # No slot available, rehash and try again:
    rehash(h, h.count > 64000 ? sz*2 : sz*4)
    return ht_keyindex!(h, key)
end

function _setindex!(h::HashDictionary, val, key, index)
    if index>0
        skey!(h, key, index)
        sval!(h, val, index)
    else # occupy new slot
        index = - index
        h.slots[index] = ISFILLED
        skey!(h, key, index)
        sval!(h, val, index)
        if isordered(h)
            push!(h.order, index)
            h.idxs[index] = length(h.order)
        end
        h.count += 1

        sz = numslots(h)
        # Rehash now if necessary
        if h.ndel >= ((3*sz)>>2) || h.count*3 > sz*2
            # > 3/4 deleted or > 2/3 full
            rehash(h, h.count > 64000 ? h.count*2 : h.count*4)
        end
    end
end

function setindex!{K,V}(h::HashDictionary{K,V}, v0, key)
    key = keyconvert(h, key)
    v = convert(V,  v0)

    index = ht_keyindex!(h, key)
    _setindex!(h, v, key, index)
    return h
end

function get!{K,V}(h::HashDictionary{K,V}, key, default)
    key = keyconvert(h, key)
    index = ht_keyindex!(h, key)

    index > 0 && return gval(h, index)

    v = convert(V,  default)
    _setindex!(h, v, key, index)
    return v
end

function get!{K,V}(default::Function, h::HashDictionary{K,V}, key)
    key = keyconvert(h, key)
    index = ht_keyindex!(h, key)

    index > 0 && return gval(h, index)

    v = convert(V,  default())
    _setindex!(h, v, key, index)
    return v
end

# NOTE: this macro is specific to Dict, not Associative, and should
#       therefore not be exported as-is: it's for internal use only.
macro get!(h, key, default)
    quote
        key = keyconvert($(esc(h)), $(esc(key)))
        index = ht_keyindex!($(esc(h)), key)
        if index < 0
            K, V = eltype($(esc(h)))
            v = convert(V, $(esc(default)))
            _setindex!($(esc(h)), v, key, index)
        else
            @inbounds v = gval($(esc(h)),index)
        end
        v
    end
end

function getindex{K,V}(h::HashDictionary{K,V}, key)
    index = ht_keyindex(h, key)
    return (index<0) ? throw(KeyError(key)) : gval(h, index)::V
end

function get{K,V}(h::HashDictionary{K,V}, key, deflt)
    index = ht_keyindex(h, key)
    return (index<0) ? deflt : gval(h, index)::V
end

function get{K,V}(deflt::Function, h::HashDictionary{K,V}, key)
    index = ht_keyindex(h, key)
    return (index<0) ? deflt() : gval(h, index)::V
end

haskey(h::HashDictionary, key) = (ht_keyindex(h, key) >= 0)
in{T<:HashDictionary}(key, v::KeyIterator{T}) = haskey(v.dict, key)

function getkey{K,V}(h::HashDictionary{K,V}, key, deflt)
    index = ht_keyindex(h, key)
    return (index<0) ? deflt : gkey(h, index)::K
end

function _pop!(h::HashDictionary, index)
    val = gval(h, index)
    _delete!(h, index)
    return val
end

function pop!(h::HashDictionary, key)
    index = ht_keyindex(h, key)
    index > 0 ? _pop!(h, index) : throw(KeyError(key))
end

function pop!(h::HashDictionary, key, default)
    index = ht_keyindex(h, key)
    index > 0 ? _pop!(h, index) : default
end

function _delete!(h::HashDictionary, index)
    h.slots[index] = ISMISSING
    ccall(:jl_arrayunset, Void, (Any, Uint), h.keys, index-1) # don't use gkey here!
    ccall(:jl_arrayunset, Void, (Any, Uint), h.vals, index-1)
    if isordered(h)
        h.order[h.idxs[index]] = 0
    end
    h.ndel += 1
    h.count -= 1
    h
end

function delete!(h::HashDictionary, key)
    index = ht_keyindex(h, key)
    if index > 0; _delete!(h, index); end
    h
end

function skip_deleted(h::HashDictionary, i)
    L = numslots(h)
    while i<=L && !isslotfilled(h,i)
        i += 1
    end
    return i
end

start(h::HashDictionary) = skip_deleted(h, 1)
done(h::HashDictionary, i) = done(h.vals, i)
next(h::HashDictionary, i) = ((gkey(h, i), gval(h, i)), skip_deleted(h, i+1))

isempty(h::HashDictionary) = (h.count == 0)
length(h::HashDictionary) = h.count

next{T<:HashDictionary}(v::KeyIterator{T}, i) = (gkey(v.dict, i), skip_deleted(v.dict,i+1))
next{T<:HashDictionary}(v::ValueIterator{T}, i) = (gval(v.dict, i), skip_deleted(v.dict,i+1))

## macro to make a subtype of a HashDictionary:
macro makeHashDictionary(TName, K, KK, V, VV, Order)
    # assert(isa(order, Union(Ordered, Unordered)))
    # if isa(order, Ordered)
    #     error("Ordered dict not implemented.")
    # end
    esc( # Escaping everything to make methods/variable names not mangeled.
         # Not sure this macro could safely be used outside this modules.
    quote
        type $TName{$K,$V} <: HashDictionary{$K,$V}
            slots::Array{Uint8,1} # flag on status of storage slot
            keys::Array{$KK,1}    # skey! maps K->KK
            vals::Array{$VV,1}    # gkey! maps KK->K
            idxs::Array{$Order,1} # order of keys
            order::Array{$Order,1}# order
            ndel::Int             # number of deleted items
            count::Int            # 

            function $TName()
                n = 16
                ord = Array($Order,0)
                sizehint(ord, n)  # TODO: profile whether this makes it better. 
                new(zeros(Uint8,n), Array($KK,n), Array($VV,n), Array($Order,n), ord, 0, 0)
            end
            function $TName(ks, vs)
                # TODO: eventually replace with a call to $TName(zip(ks,vs))
                n = min(length(ks), length(vs))
                h = $TName{$K,$V}()
                for i=1:n
                    h[ks[i]] = vs[i]
                end
                return h
            end
            function $TName(kv)
                h = $TName{$K,$V}()
                for (k,v) in kv
                    h[k] = v
                end
                return h
            end
        end

        $TName() = $TName{Any,Any}()
        
        # TODO:
        # - this does not add finalizers to weak-key dicts!  
        # - if V/K and VV/KK are not convertable this will also fail.
        $TName{$K,$V}(ks::AbstractArray{$K}, vs::AbstractArray{$V}) = $TName{$K,$V}(ks,vs)
        $TName(ks, vs) = $TName{Any,Any}(ks, vs)

        # syntax entry points
        $TName{$K,$V}(ks::($K...), vs::($V...)) = $TName{$K  ,$V  }(ks, vs)
        $TName{$K  }(ks::($K...), vs::Tuple ) = $TName{$K  ,Any}(ks, vs)
        $TName{$V  }(ks::Tuple , vs::($V...)) = $TName{Any,$V  }(ks, vs)

        $TName{$K,$V}(kv::AbstractArray{($K,$V)}) = $TName{$K,$V}(kv)
        $TName{$K,$V}(kv::Associative{$K,$V}) = $TName{$K,$V}(kv)
    end
    )
end

## The standard Dict
@makeHashDictionary(Dict, K, K, V, V, Unordered)
## Ordered Dict
@makeHashDictionary(OrderedDict, K, K, V, V, Ordered)
## ObjectIdDict
@makeHashDictionary(ObjectIdDict2, K, K, V, V, Unordered)
## WeakKeyDict
@makeHashDictionary(WeakKeyDict, K, WeakRef, V, V, Unordered)
## WeakObjectIdDict 
@makeHashDictionary(WeakObjectIdDict, K, WeakRef, V, V, Unordered)

# Update some methods for them
## ObjectID
typealias OIdDicts{K,V} Union(ObjectIdDict2{K,V}, WeakObjectIdDict{K,V})
hashindex(::OIdDicts, key, sz) = (int(object_id(key)) & (sz-1)) + 1 # object_id is a hash already

## Weak keys
# TODO: Constructors working on arrays will not add finalizers!
typealias WeakDicts{K,V} Union(WeakKeyDict{K,V}, WeakObjectIdDict{K,V})

# transforms key at index ind:
gkey(h::WeakDicts, ind) = h.keys[ind].value
# transform key back before setting it:
_skey_weak(key) = key==nothing ? throw(KeyError("'nothing' is not allowed as a weak-key")) : key
skey!(h::WeakDicts, key, ind) = (h.keys[ind] = WeakRef(_skey_weak(key)))
skey!(::WeakDicts, ar::Vector, key, ind) = (ar[ind] = WeakRef(_skey_weak(key)))

# finalizer for mutables:
function weak_key_delete!(t::WeakDicts, k)
    # when a weak key is finalized, remove from dictionary if it is still there
    wk = getkey(t, k, secret_table_token) # getkey returns the WeakRef.value
    if !is(wk,secret_table_token) && is(wk, k)
        delete!(t, k)
    end
end

# purge entries.  For instance for weak-key dicts the reference may
# have been gc-ed.
topurge(::WeakDicts, key) = key==nothing

function _setindex!(h::WeakDicts, val, key, index)
    # add a finalizer
    if ~isimmutable(key)
        deleter(x) = weak_key_delete!(h, x)
        finalizer(key, deleter)
    end
    
    # as in original method
    if index>0
        skey!(h, key, index)
        sval!(h, val, index)
    else # occupy new slot
        index = - index
        h.slots[index] = ISFILLED
        skey!(h, key, index)
        sval!(h, val, index)
        h.count += 1

        sz = numslots(h)
        # Rehash now if necessary
        if h.ndel >= ((3*sz)>>2) || h.count*3 > sz*2
            # > 3/4 deleted or > 2/3 full
            rehash(h, h.count > 64000 ? h.count*2 : h.count*4)
        end
    end
end

# add purging
function skip_deleted(h::WeakDicts, i)
    L = numslots(h)
    while i<=L
        if isslotfilled(h,i) 
            if topurge(h, gkey(h, i))
                _delete!(h, i)
            else
                break
            end
        end
        i += 1
    end
    return i
end

## Ordered Dicts
typealias OrderedDicts{K,V} Union(OrderedDict{K,V})

function skip_deleted(h::OrderedDicts, i)
    L = length(h.order)
    while i<=L && h.order[i] == 0
        i += 1
    end
    return i
end

done(h::OrderedDicts, i) = done(h.order, i)
next(h::OrderedDicts, i) = ((gkey(h, h.order[i]), gval(h, h.order[i])), skip_deleted(h,i+1))

next{T<:OrderedDicts}(v::KeyIterator{T}, i) = (gkey(v.dict, v.dict.order[i]), skip_deleted(v.dict,i+1))
next{T<:OrderedDicts}(v::ValueIterator{T}, i) = (gval(v.dict, v.dict.order[i]), skip_deleted(v.dict,i+1))

