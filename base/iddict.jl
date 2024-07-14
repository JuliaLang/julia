# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    IdDict([itr])

`IdDict{K,V}()` constructs a hash table using [`objectid`](@ref) as hash and
`===` as equality with keys of type `K` and values of type `V`. See [`Dict`](@ref)
for further help and [`IdSet`](@ref) for the set version of this.

In the example below, the `Dict` keys are all `isequal` and therefore get hashed
the same, so they get overwritten. The `IdDict` hashes by object-id, and thus
preserves the 3 different keys.

# Examples
```julia-repl
julia> Dict(true => "yes", 1 => "no", 1.0 => "maybe")
Dict{Real, String} with 1 entry:
  1.0 => "maybe"

julia> IdDict(true => "yes", 1 => "no", 1.0 => "maybe")
IdDict{Any, String} with 3 entries:
  true => "yes"
  1.0  => "maybe"
  1    => "no"
```
"""
mutable struct IdDict{K,V} <: AbstractDict{K,V}
    ht::Memory{Any}
    count::Int
    ndel::Int
    age::UInt
    IdDict{K,V}() where {K, V} = new{K,V}(Memory{Any}(undef, 32), 0, 0, 0)

    function IdDict{K,V}(itr) where {K, V}
        d = IdDict{K,V}()
        for (k,v) in itr; d[k] = v; end
        d
    end

    function IdDict{K,V}(pairs::Pair...) where {K, V}
        d = IdDict{K,V}()
        sizehint!(d, length(pairs))
        for (k,v) in pairs; d[k] = v; end
        d
    end

    IdDict{K,V}(d::IdDict{K,V}) where {K, V} = new{K,V}(copy(d.ht), d.count, d.ndel, d.age)
end

IdDict() = IdDict{Any,Any}()
IdDict(kv::Tuple{}) = IdDict()

IdDict(ps::Pair{K,V}...)           where {K,V} = IdDict{K,V}(ps)
IdDict(ps::Pair{K}...)             where {K}   = IdDict{K,Any}(ps)
IdDict(ps::(Pair{K,V} where K)...) where {V}   = IdDict{Any,V}(ps)
IdDict(ps::Pair...)                            = IdDict{Any,Any}(ps)

IdDict(kv) = dict_with_eltype((K, V) -> IdDict{K, V}, kv, eltype(kv))

empty(d::IdDict, ::Type{K}, ::Type{V}) where {K, V} = IdDict{K,V}()

function rehash!(d::IdDict, newsz = length(d.ht)%UInt)
    d.ht = ccall(:jl_idtable_rehash, Memory{Any}, (Any, Csize_t), d.ht, newsz)
    d.age += 1
    d
end

function sizehint!(d::IdDict, newsz)
    newsz = _tablesz(newsz*2)  # *2 for keys and values in same array
    oldsz = length(d.ht)
    # grow at least 25%
    if newsz < (oldsz*5)>>2
        return d
    end
    rehash!(d, newsz)
end

# get (index) for the key
#     index - where a key is stored, or -pos if not present
#             and was inserted at pos
function ht_keyindex2!(d::IdDict{K,V}, @nospecialize(key)) where {K, V}
    !isa(key, K) && throw(KeyTypeError(K, key))
    keyindex = RefValue{Cssize_t}(0)
    d.ht = ccall(:jl_eqtable_keyindex, Memory{Any}, (Memory{Any}, Any, Ptr{Cssize_t}), d.ht, key, keyindex)
    return keyindex[]
end

@propagate_inbounds function _setindex!(d::IdDict{K,V}, val::V, keyindex::Int) where {K, V}
    d.ht[keyindex+1] = val
    d.count += 1
    d.age += 1

    if d.ndel >= ((3*length(d.ht))>>2)
        rehash!(d, max((length(d.ht)%UInt)>>1, 32))
        d.ndel = 0
    end
    return nothing
end

@inline function setindex!(d::IdDict{K,V}, @nospecialize(val), @nospecialize(key)) where {K, V}
    !isa(key, K) && throw(KeyTypeError(K, key))
    if !(val isa V) # avoid a dynamic call
        val = convert(V, val)::V
    end
    if d.ndel >= ((3*length(d.ht))>>2)
        rehash!(d, max((length(d.ht)%UInt)>>1, 32))
        d.ndel = 0
    end
    inserted = RefValue{Cint}(0)
    d.ht = ccall(:jl_eqtable_put, Memory{Any}, (Any, Any, Any, Ptr{Cint}), d.ht, key, val, inserted)
    d.count += inserted[]
    d.age += 1
    return d
end

function get(d::IdDict{K,V}, @nospecialize(key), @nospecialize(default)) where {K, V}
    val = ccall(:jl_eqtable_get, Any, (Any, Any, Any), d.ht, key, default)
    val === default ? default : val::V
end

function getindex(d::IdDict{K,V}, @nospecialize(key)) where {K, V}
    val = ccall(:jl_eqtable_get, Any, (Any, Any, Any), d.ht, key, secret_table_token)
    val === secret_table_token && throw(KeyError(key))
    return val::V
end

function pop!(d::IdDict{K,V}, @nospecialize(key), @nospecialize(default)) where {K, V}
    found = RefValue{Cint}(0)
    val = ccall(:jl_eqtable_pop, Any, (Any, Any, Any, Ptr{Cint}), d.ht, key, default, found)
    d.age += 1
    if found[] === Cint(0)
        return default
    else
        d.count -= 1
        d.ndel += 1
        return val::V
    end
end

function pop!(d::IdDict{K,V}, @nospecialize(key)) where {K, V}
    val = pop!(d, key, secret_table_token)
    val === secret_table_token && throw(KeyError(key))
    return val::V
end

function delete!(d::IdDict{K}, @nospecialize(key)) where K
    pop!(d, key, secret_table_token)
    d
end

function empty!(d::IdDict)
    d.ht = Memory{Any}(undef, 32)
    ht = d.ht
    t = @_gc_preserve_begin ht
    memset(unsafe_convert(Ptr{Cvoid}, ht), 0, sizeof(ht))
    @_gc_preserve_end t
    d.ndel = 0
    d.count = 0
    d.age += 1
    return d
end

_oidd_nextind(a, i) = reinterpret(Int, ccall(:jl_eqtable_nextind, Csize_t, (Any, Csize_t), a, i))

function iterate(d::IdDict{K,V}, idx=0) where {K, V}
    idx = _oidd_nextind(d.ht, idx%UInt)
    idx == -1 && return nothing
    return (Pair{K, V}(d.ht[idx + 1]::K, d.ht[idx + 2]::V), idx + 2)
end

length(d::IdDict) = d.count

isempty(d::IdDict) = length(d) == 0

copy(d::IdDict) = typeof(d)(d)

function get!(d::IdDict{K,V}, @nospecialize(key), @nospecialize(default)) where {K, V}
    get!(()->default, d, key)
end

function get(default::Callable, d::IdDict{K,V}, @nospecialize(key)) where {K, V}
    val = ccall(:jl_eqtable_get, Any, (Any, Any, Any), d.ht, key, secret_table_token)
    if val === secret_table_token
        return default()
    else
        return val::V
    end
end

function get!(default::Callable, d::IdDict{K,V}, @nospecialize(key)) where {K, V}
    keyindex = ht_keyindex2!(d, key)

    if keyindex < 0
        # If convert call fails we need the key to be deleted
        d.ndel += 1
        age0 = d.age
        val = default()
        if !isa(val, V)
            val = convert(V, val)::V
        end
        d.ndel -= 1
        if d.age != age0
            @inline setindex!(d, val, key)
        else
            @inbounds _setindex!(d, val, -keyindex)
        end
        return val::V
    else
        d.age += 1
        return @inbounds d.ht[keyindex+1]::V
    end
end

in(@nospecialize(k), v::KeySet{<:Any,<:IdDict}) = get(v.dict, k, secret_table_token) !== secret_table_token

# For some AbstractDict types, it is safe to implement filter!
# by deleting keys during iteration.
filter!(f, d::IdDict) = filter_in_one_pass!(f, d)
