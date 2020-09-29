const StoreType = Union{<:Tuple, <:Vector}

"""
    LittleDict(keys, vals)<:AbstractDict

A ordered dictionary type for small numbers of keys.
Rather than using `hash` or some other sophisicated measure
to store the vals in a clever arrangement,
it just keeps everything in a pair of lists.

While theoretically this has expected time complexity _O(n)_,
vs the hash-based `OrderDict`/`Dict`'s expected time complexity _O(1)_,
and the search-tree-based `SortedDict`'s expected time complexity _O(log(n))_.
In practice it is really fast, because it is cache & SIMD friendly.

It is reasonable to expect it to outperform an `OrderedDict`,
with up to around 30 elements in general;
or with up to around 50 elements if using a `LittleDict` backed by `Tuples`
(see [`freeze`](@ref))
However, this depends on exactly how long `isequal` and `hash` take,
as well as on how many hash collisions occur etc.

!!! note
    When constructing a `LittleDict` it is faster to pass in the keys and 
    values each as seperate lists. So if you have them seperately already,
    do `LittleDict(ks, vs)` not `LittleDict(zip(ks, vs))`.
    Further keys or value lists that are passed as `Tuple`s will not require any
    copies to create the `LittleDict`, so `LittleDict(ks::Tuple, vs::Tuple)`
    is the fastest constructor of all.
"""
struct LittleDict{K,V,KS<:StoreType,VS<:StoreType} <: AbstractDict{K, V}
    keys::KS
    vals::VS

    function LittleDict{K,V,KS,VS}(keys,vals) where {K,V,KS,VS}
        if length(keys) != length(vals)
            throw(ArgumentError(
                "Number of keys ($(length(keys))) differs from " *
                "number of values ($(length(vals))"
            ))
        end
        K<:eltype(KS) || ArgumentError("Invalid store type $KS, for key type $K")
        V<:eltype(VS) || ArgumentError("Invalid store type $VS, for value type $K")
        
        return new(keys,vals)
    end
end

function LittleDict{K,V}(ks::KS, vs::VS) where {K,V, KS<:StoreType,VS<:StoreType}
    return LittleDict{K, V, KS, VS}(ks, vs)
end

function LittleDict(ks::KS, vs::VS) where {KS<:StoreType,VS<:StoreType}
    return LittleDict{eltype(KS), eltype(VS)}(ks, vs)
end


# Other iterators should be copied to a Vector
LittleDict(ks, vs) = LittleDict(collect(ks), collect(vs))


function LittleDict{K,V}(itr) where {K,V}
    ks = K[]
    vs = V[]
    for val in itr
        if !(val isa Union{Tuple{<:Any, <:Any}, Pair})
            throw(ArgumentError(
                "LittleDict(kv): kv needs to be an iterator of tuples or pairs")
            )
        end
        k, v = val
        push!(ks, k)
        push!(vs, v)
    end
    return LittleDict(ks, vs)
end

LittleDict{K,V}(itr...) where {K,V} = LittleDict{K,V}(itr)
LittleDict(itr...) = LittleDict(itr)
LittleDict(itr::T) where T = LittleDict{kvtype(eltype(T))...}(itr)

# Avoid contention between the core constructor, and the list of elements
LittleDict(itr1::Pair, itr2::Pair) = LittleDict(first.([itr1, itr2]), last.([itr1,itr2]))
LittleDict(itr1::Pair) = LittleDict([first(itr1)], [last(itr1)])
LittleDict{K, V}(itr::Pair) where {K, V} = LittleDict{K,V}(K[first(itr)], V[last(itr)])

kvtype(::Any) = (Any, Any)
kvtype(::Type{Union{}}) = (Any,Any)

kvtype(::Type{Pair{K,V}}) where {K,V} = (K,V)
kvtype(::Type{Pair{<:Any,V}}) where {V} = (Any,V)
kvtype(::Type{Pair{K,<:Any}}) where {K} = (K,Any)

kvtype(::Type{Tuple{K,V}}) where {K,V} = (K,V)
kvtype(::Type{Tuple{<:Any,V}}) where {V} = (Any,V)
kvtype(::Type{Tuple{K,<:Any}}) where {K} = (K,Any)


"""
    freeze(dd::AbstractDict)
Render an dictionary immutable by converting it to a `Tuple` backed
`LittleDict`.
The `Tuple` backed `LittleDict` is faster than the `Vector` backed `LittleDict`,
particularly when the keys are all concretely typed.
"""
function freeze(dd::AbstractDict)
    ks = Tuple(keys(dd))
    vs = Tuple(values(dd))
    return LittleDict(ks, vs)
end

isordered(::Type{<:LittleDict}) = true

# For now these are internal UnionAlls for dispatch purposes
const UnfrozenLittleDict{K,V} = LittleDict{K,V, Vector{K}, Vector{V}}
const FrozenLittleDict{K,V} = LittleDict{K,V, <:Tuple, <:Tuple}

##### Methods that all AbstractDicts should implement

Base.length(dd::LittleDict) = length(dd.keys)

function Base.getkey(dd::LittleDict, key, default)
    if key ∈ dd.keys
        return key
    else
        return default
    end
end

function Base.map!(f, iter::Base.ValueIterator{<:LittleDict})
    dict = iter.dict
    vals = dict.vals
    for i in 1:length(vals)
        @inbounds vals[i] = f(vals[i])
    end
    return iter
end

struct NotFoundSentinel end  # Struct to mark not not found
function Base.get(dd::LittleDict, key, default)
    @assert length(dd.keys) == length(dd.vals)
    for ii in 1:length(dd.keys)
        cand = @inbounds dd.keys[ii]
        isequal(cand, key) && return @inbounds(dd.vals[ii])
    end
    return default
end
function get(default::Base.Callable, dd::LittleDict, key)
    got = get(dd, key, NotFoundSentinel())
    if got isa NotFoundSentinel  # not found
        return default()
    else
        return got
    end
end

function Base.iterate(dd::LittleDict, ii=1)
    ii > length(dd.keys) && return nothing
    return (dd.keys[ii] => dd.vals[ii], ii+1)
end

function merge(d1::LittleDict, d2::AbstractDict)
    return merge((x,y)->y, d1, d2)
end

function merge(
    combine::Function,
    d::LittleDict,
    others::AbstractDict...
    )
    K,V = _merge_kvtypes(d, others...)
    dc = LittleDict{K,V}(d)
    for d2 in others
        for (k2,v2) in d2
            got = get(dc, k2, NotFoundSentinel())
            if got isa NotFoundSentinel
                add_new!(dc, k2, v2)
            else
                # GOLDPLATE: ideally we would avoid iterating this twice
                # once for get and once for setindex!
                dc[k2]=combine(got, v2)
            end
        end
    end
    return dc
end


Base.empty(dd::LittleDict{K,V}) where {K,V} = LittleDict{K,V}()

######## Methods that all mutable AbstractDict's should implement

function Base.sizehint!(dd::UnfrozenLittleDict, sz)
    sizehint!(dd.keys, sz)
    sizehint!(dd.vals,sz)
    return dd
end

function add_new!(dd::UnfrozenLittleDict{K, V}, key, value) where {K, V}
    kk = convert(K, key)
    vv = convert(V, value)

    # if we can convert it to the right type, and the dict is unfrozen
    # then neither push can fail, so the dict length with remain in sync
    push!(dd.keys, kk)
    push!(dd.vals, vv)

    return dd
end


function Base.setindex!(dd::LittleDict{K,V, <:Any, <:Vector}, value, key) where {K,V}
    # Note we only care if the Value store is mutable (<:Vector)
    # As we can have immutable keys, if we are setting the value of an existing key

    # Assertion below commented out as by standards of carefully optimised
    # setindex! it has huge code (26%), this does mean that if someone has messed
    # with the fields of the LittleDict directly, then the @inbounds could be invalid
    #@assert length(dd.keys) == length(dd.vals)

    kk = convert(K, key)
    vv = convert(V, value)
    for ii in 1:length(dd.keys)
        cand = @inbounds dd.keys[ii]
        if isequal(cand, kk)
            @inbounds(dd.vals[ii] = vv)
            return dd
        end
    end
    add_new!(dd, key, value)
    return dd
end

function Base.pop!(dd::UnfrozenLittleDict)
    pop!(dd.keys)
    return pop!(dd.vals)
end

function Base.pop!(dd::UnfrozenLittleDict, key)
    @assert length(dd.keys) == length(dd.vals)

    for ii in 1:length(dd.keys)
        cand = @inbounds dd.keys[ii]
        if isequal(cand, key)
            deleteat!(dd.keys, ii)
            val = @inbounds dd.vals[ii]
            deleteat!(dd.vals, ii)
            return val
        end
    end
end

function Base.delete!(dd::UnfrozenLittleDict, key)
    pop!(dd, key)
    return dd
end

Base.empty!(dd::UnfrozenLittleDict) = (empty!(dd.keys); empty!(dd.vals); dd)

function get!(default::Base.Callable, dd::UnfrozenLittleDict, key)
    got = get(dd, key, NotFoundSentinel())
    if got isa NotFoundSentinel  # not found
        val = default()
        add_new!(dd, key, val)
        return val
    else
        return got
    end
end
get!(dd::UnfrozenLittleDict, key, default) = get!(()->default, dd, key)
