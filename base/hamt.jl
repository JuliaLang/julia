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
# The `path` function searches for a matching entry, and for persistency
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
    # `data` is never mutated in place, since it may be shared with other tries;
    # updates replace it with a new `Memory`
    data::Memory{Union{Leaf{K, V}, HAMT{K, V}}}
    bitmap::BITMAP
    HAMT{K,V}(data, bitmap) where {K,V} = new{K,V}(data, bitmap)
    HAMT{K, V}() where {K, V} = new{K,V}(Memory{Union{Leaf{K, V}, HAMT{K, V}}}(undef, 0), zero(BITMAP))
end

Base.@assume_effects :nothrow :effect_free function init_hamt(K, V, k, v)
    # For a single element we can't have a 'hash-collision
    data = Memory{Union{Leaf{K, V}, HAMT{K, V}}}(undef, 1)
    data[1] = Leaf{K,V}(k,v)
    return HAMT{K,V}(data, zero(BITMAP))
end

function setindex_copy(data::Memory, x, i)
    new = copy(data)
    @inbounds new[i] = x
    return new
end

function insert_copy(data::Memory{T}, i, x) where {T}
    n = length(data)
    new = Memory{T}(undef, n + 1)
    @inbounds begin
        unsafe_copyto!(new, 1, data, 1, i - 1)
        new[i] = x
        unsafe_copyto!(new, i + 1, data, i, n - i + 1)
    end
    return new
end

function deleteat_copy(data::Memory{T}, i) where {T}
    n = length(data)
    new = Memory{T}(undef, n - 1)
    @inbounds begin
        unsafe_copyto!(new, 1, data, 1, i - 1)
        unsafe_copyto!(new, i, data, i + 1, n - i)
    end
    return new
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
Base.@assume_effects :terminates_locally function HashState{K}(@nospecialize(other::HashState), @nospecialize(key)) where {K}
    h = HashState{K}(key, objectid(key), 0, 0)
    while h.depth !== other.depth
        h = next(h)
    end
    return h
end
Base.@assume_effects :terminates_locally HashState(other::HashState, key) = HashState{typeof(key)}(other, key)

function next(h::HashState{K}) where {K}
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
    return HashState{K}(h.key, h_hash, depth, shift)
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
@inline Base.@assume_effects :noub :terminates_locally function path(trie::HAMT{K,V}, h::HashState, copy=false) where {K, V}
    if copy
        trie = top = HAMT{K,V}(trie.data, trie.bitmap)
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
                next = HAMT{K,V}(next.data, next.bitmap)
                # :noub because entry_index is guaranteed to be inbounds for trie.data
                trie.data = setindex_copy(trie.data, next, i)
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
            trie.data = setindex_copy(trie.data, Leaf{K, V}(h.key, val), i)
        else
            trie.data = insert_copy(trie.data, i, Leaf{K, V}(h.key, val))
        end
        set!(trie, bi)
    else
        @assert present "!found && !present"
        # collision -> grow
        leaf = @inbounds trie.data[i]::Leaf{K,V}
        leaf_h = HashState{K}(h, leaf.key)
        if leaf_h.hash == h.hash
            error("Perfect hash collision")
        end
        while true
            new_trie = HAMT{K, V}()
            if present
                trie.data = setindex_copy(trie.data, new_trie, i)
            else
                i = entry_index(trie, bi)
                trie.data = insert_copy(trie.data, i, new_trie)
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
            new_leaf = Leaf{K, V}(h.key, val)
            data = Memory{Union{Leaf{K, V}, HAMT{K, V}}}(undef, 2)
            if bi_new.x < bi_old.x
                data[1] = new_leaf; data[2] = leaf
            else
                data[1] = leaf; data[2] = new_leaf
            end
            new_trie.data = data
            set!(new_trie, bi_new)
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
