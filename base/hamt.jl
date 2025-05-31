module HashArrayMappedTries

export HAMT

##
# Implements "Ideal Hash Trees" Phil Bagwell 2000
#
# Notable divergence is that we forgo a resizable root table.
# Root tables improve lookup performance for large sizes, but
# limit space efficiency if the HAMT is used for a persistent
# dictionary, since each persistent operation would duplicate
# the root table.
#
# We do not handle perfect hash-collision. We would need to
# add an additional node type for Collisions. Perfect hash
# collisions should not occur in practice since we perform
# rehashing after using 55 bits (MAX_SHIFT) of the original hash.
#
# Use https://github.com/vchuravy/HashArrayMappedTries.jl if
# you want to use this implementation in a package.
#
# A HAMT is formed by tree of levels, where at each level
# we use a portion of the bits of the hash for indexing
#
# We use a branching width (ENTRY_COUNT) of 32, giving us
# 5bits of indexing per level
# 0000_00000_00000_00000_00000_00000_00000_00000_00000_00000_00000_00000
# L11  L10   L9    L8    L7    L6    L5    L4    L3    L2    L1    L0
#
# At each level we use a 32bit bitmap to store which elements are occupied.
# Since our storage is "sparse" we need to map from index in [0,31] to
# the actual storage index. We mask the bitmap with (1 << i) - 1 and count
# the ones in the result. The number of set ones (+1) gives us the index
# into the storage array.
#
# HAMT can be both persistent and non-persistent.
# The `path` function searches for a matching entries, and for persistency
# optionally copies the path so that it can be safely mutated.

# TODO:
# When `trie.data` becomes empty we could remove it from it's parent,
# but we only know so fairly late. Maybe have a compact function?

const ENTRY_COUNT = UInt(32)
const BITMAP = UInt32
const NBITS = sizeof(UInt) * 8
# @assert ispow2(ENTRY_COUNT)
const BITS_PER_LEVEL = trailing_zeros(ENTRY_COUNT)
const LEVEL_MASK = (UInt(1) << BITS_PER_LEVEL) - UInt(1)
const MAX_SHIFT = (NBITS ÷ BITS_PER_LEVEL - 1) *  BITS_PER_LEVEL

mutable struct Leaf{K, V}
    const key::K
    const val::V
end

"""
    HAMT{K,V}

A HashArrayMappedTrie that optionally supports persistence.
"""
mutable struct HAMT{K, V}
    const data::Vector{Union{Leaf{K, V}, HAMT{K, V}}}
    bitmap::BITMAP
    HAMT{K,V}(data, bitmap) where {K,V} = new{K,V}(data, bitmap)
    HAMT{K, V}() where {K, V} = new{K,V}(Vector{Union{Leaf{K, V}, HAMT{K, V}}}(undef, 0), zero(BITMAP))
end

Base.@assume_effects :nothrow :effect_free function init_hamt(K, V, k, v)
    # For a single element we can't have a 'hash-collision
    trie = HAMT{K,V}(Vector{Union{Leaf{K, V}, HAMT{K, V}}}(undef, 1), zero(BITMAP))
    trie.data[1] = Leaf{K,V}(k,v)
    return trie
end

Base.@assume_effects :effect_free function HAMT{K,V}((k,v)::Pair{K,V}) where {K, V}
    trie = init_hamt(K, V, k, v)
    bi = BitmapIndex(HashState(k))
    set!(trie, bi)
    return trie
end
HAMT{K,V}(kv::Pair) where {K, V} = HAMT{K,V}(convert(Pair{K,V}, kv))

HAMT(pair::Pair{K,V}) where {K, V} = HAMT{K,V}(pair)

# TODO: Parameterize by hash function
struct HashState{K}
    key::K
    hash::UInt
    depth::Int
    shift::Int
end
HashState(key) = HashState(key, objectid(key), 0, 0)
# Reconstruct
Base.@assume_effects :terminates_locally function HashState(other::HashState, key)
    h = HashState(key)
    while h.depth !== other.depth
        h = next(h)
    end
    return h
end

function next(h::HashState)
    depth = h.depth + 1
    shift = h.shift + BITS_PER_LEVEL
    # Assert disabled for effect precision
    # @assert h.shift <= MAX_SHIFT
    if shift > MAX_SHIFT
        # Note we use `UInt(depth ÷ BITS_PER_LEVEL)` to seed the hash function
        # the hash docs, do we need to hash `UInt(depth ÷ BITS_PER_LEVEL)` first?
        h_hash = hash(objectid(h.key), UInt(depth ÷ BITS_PER_LEVEL))
        shift = 0
    else
        h_hash = h.hash
    end
    return HashState(h.key, h_hash, depth, shift)
end

struct BitmapIndex
    x::UInt
end
BitmapIndex(h::HashState) = BitmapIndex((h.hash >> h.shift) & LEVEL_MASK)

Base.:(<<)(v, bi::BitmapIndex) = v << bi.x
Base.:(>>)(v, bi::BitmapIndex) = v >> bi.x

isset(trie::HAMT, bi::BitmapIndex) = isodd(trie.bitmap >> bi)
function set!(trie::HAMT, bi::BitmapIndex)
    trie.bitmap |= (UInt32(1) << bi)
    # Invariant: count_ones(trie.bitmap) == Base.length(trie.data)
end

function unset!(trie::HAMT, bi::BitmapIndex)
    trie.bitmap &= ~(UInt32(1) << bi)
    # Invariant: count_ones(trie.bitmap) == Base.length(trie.data)
end

function entry_index(trie::HAMT, bi::BitmapIndex)
    mask = (UInt32(1) << bi.x) - UInt32(1)
    count_ones(trie.bitmap & mask) + 1
end

islevel_empty(trie::HAMT) = trie.bitmap == 0
islevel_empty(::Leaf) = false

"""
    path(trie, h, copyf)::(found, present, trie, i, top, level)

Internal function that walks a HAMT and finds the slot for hash.
Returns if a value is `present` and a value is `found`.

It returns the `trie` and the index `i` into `trie.data`, as well
as the current `level`.

If a copy function is provided `copyf` use the return `top` for the
new persistent tree.
"""
@inline @Base.assume_effects :noub :terminates_locally function path(trie::HAMT{K,V}, key, h::HashState, copy=false) where {K, V}
    if copy
        trie = top = HAMT{K,V}(Base.copy(trie.data), trie.bitmap)
    else
        trie = top = trie
    end
    while true
        bi = BitmapIndex(h)
        i = entry_index(trie, bi)
        if isset(trie, bi)
            next = @inbounds trie.data[i]
            if next isa Leaf{K,V}
                # Check if key match if not we will need to grow.
                found = next.key === h.key
                return found, true, trie, i, bi, top, h
            end
            if copy
                next = HAMT{K,V}(Base.copy(next.data), next.bitmap)
                # :noub because entry_index is guaranteed to be inbounds for trie.data
                @inbounds trie.data[i] = next
            end
            trie = next::HAMT{K,V}
        else
            # found empty slot
            return true, false, trie, i, bi, top, h
        end
        h = HashArrayMappedTries.next(h)
    end
end

"""
Internal function that given an obtained path, either set the value
or grows the HAMT by inserting a new trie instead.
"""
@inline @Base.assume_effects :terminates_locally function insert!(found, present, trie::HAMT{K,V}, i, bi, h, val) where {K,V}
    if found # we found a slot, just set it to the new leaf
        # replace or insert
        if present # replace
            @inbounds trie.data[i] = Leaf{K, V}(h.key, val)
        else
            Base.insert!(trie.data, i, Leaf{K, V}(h.key, val))
        end
        set!(trie, bi)
    else
        @assert present
        # collision -> grow
        leaf = @inbounds trie.data[i]::Leaf{K,V}
        leaf_h = HashState(h, leaf.key)
        if leaf_h.hash == h.hash
            error("Perfect hash collision")
        end
        while true
            new_trie = HAMT{K, V}()
            if present
                @inbounds trie.data[i] = new_trie
            else
                i = entry_index(trie, bi)
                Base.insert!(trie.data, i, new_trie)
            end
            set!(trie, bi)

            h = next(h)
            leaf_h = next(leaf_h)
            bi_new = BitmapIndex(h)
            bi_old = BitmapIndex(leaf_h)
            if bi_new == bi_old # collision in new trie -> retry
                trie = new_trie
                bi = bi_new
                present = false
                continue
            end
            i_new = entry_index(new_trie, bi_new)
            Base.insert!(new_trie.data, i_new, Leaf{K, V}(h.key, val))
            set!(new_trie, bi_new)

            i_old = entry_index(new_trie, bi_old)
            Base.insert!(new_trie.data, i_old, leaf)
            set!(new_trie, bi_old)

            break
        end
    end
end

Base.length(::Leaf) = 1
Base.length(trie::HAMT) = sum((length(trie.data[i]) for i in eachindex(trie.data)), init=0)

Base.isempty(::Leaf) = false
function Base.isempty(trie::HAMT)
    if islevel_empty(trie)
        return true
    end
    return all(isempty(trie.data[i]) for i in eachindex(trie.data))
end

# DFS
function Base.iterate(trie::HAMT, state=nothing)
    if state === nothing
        state = (;parent=nothing, trie, i=1)
    end
    while state !== nothing
        i = state.i
        if i > Base.length(state.trie.data)
            state = state.parent
            continue
        end
        trie = state.trie.data[i]
        state = (;parent=state.parent, trie=state.trie, i=i+1)
        if trie isa Leaf
            return (trie.key => trie.val, state)
        else
            # we found a new level
            state = (;parent=state, trie, i=1)
            continue
        end
    end
    return nothing
end

end # module HashArrayMapTries
