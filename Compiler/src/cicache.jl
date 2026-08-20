# This file is a part of Julia. License is MIT: https://julialang.org/license

struct WorldRange
    min_world::UInt
    max_world::UInt
end
WorldRange() = WorldRange(typemin(UInt), typemax(UInt))
WorldRange(w::UInt) = WorldRange(w, w)
WorldRange(r::UnitRange) = WorldRange(first(r), last(r))
first(wr::WorldRange) = wr.min_world
last(wr::WorldRange) = wr.max_world
min_world(wr::WorldRange) = first(wr)
max_world(wr::WorldRange) = last(wr)

# World DAG predicates, mirroring the jl_world_* inlines in
# src/julia_internal.h: a world is a packed (segment, index) pair and
# (sa, ia) preceq (sb, ib) iff sa == sb ? ia <= ib : sa is an ancestor
# segment of sb. Worlds on the spine (the chain of segments the world
# counter traverses) always compare exactly with plain integer compares.
const WORLD_IDX_BITS = UInt === UInt64 ? 48 : 16
world_seg(w::UInt) = w >> WORLD_IDX_BITS
world_seg_reaches(sa::UInt, sb::UInt) =
    ccall(:jl_world_seg_reaches, Cint, (Csize_t, Csize_t), sa, sb) != 0
world_reaches(a::UInt, b::UInt) =
    world_seg(a) == world_seg(b) ? a <= b : world_seg_reaches(world_seg(a), world_seg(b))
world_at_most(w::UInt, maxw::UInt) =
    maxw == typemax(UInt) || !world_reaches(maxw + UInt(1), w)
world_in_range(w::UInt, minw::UInt, maxw::UInt) =
    world_reaches(minw, w) && world_at_most(w, maxw)
world_on_spine(w::UInt) = world_reaches(w, get_world_counter())
# The earliest spine world whose history includes both `a` and `b` (the join
# in the world DAG, projected onto the spine, where positions compare exactly
# as integers). For comparable worlds this is simply the later of the two.
function world_join(a::UInt, b::UInt)
    world_seg(a) == world_seg(b) && return max(a, b)
    return ccall(:jl_world_spine_join, Csize_t, (Csize_t, Csize_t), a, b) % UInt
end

in(world::UInt, wr::WorldRange) = world_in_range(world, wr.min_world, wr.max_world)

@inline function intersect(a::WorldRange, b::WorldRange)
    # the min side joins in the world DAG: bounds from different merged
    # branches are incomparable and their join lies on the spine
    minw = world_join(a.min_world, b.min_world)
    maxw = min(a.max_world, b.max_world)
    if minw > maxw
        # Interval arithmetic cannot describe the validity region of a world
        # off the spine; a point range there absorbs any bounds whose region
        # contains its world (e.g. bounds capped by an invalidation on the
        # spine, whose future cone does not contain the point's world).
        if a.min_world == a.max_world && a.min_world in b
            return a
        elseif b.min_world == b.max_world && b.min_world in a
            return b
        end
        @assert false "attempting to intersect disjoint world ranges"
    end
    return WorldRange(minw, maxw)
end

@inline function union(a::WorldRange, b::WorldRange)
    if b.min_world < a.min_world
        (b, a) = (a, b)
    end
    @assert a.max_world >= b.min_world - 1
    return WorldRange(a.min_world, b.max_world)
end

"""
    struct InternalCodeCache

Internally, each `MethodInstance` keeps a unique global cache of code instances
that have been created for the given method instance, stratified by world age
ranges. This struct abstracts over access to this cache.
"""
struct InternalCodeCache
    owner::Any # `jl_egal` is used for comparison
    worlds::WorldRange
    InternalCodeCache(@nospecialize(owner), wr::WorldRange) = new(owner, wr)
    InternalCodeCache(@nospecialize(owner), args...) = new(owner, WorldRange(args...))
end

function setindex!(cache::InternalCodeCache, ci::CodeInstance, mi::MethodInstance)
    @assert ci.owner === cache.owner
    m = mi.def
    if isa(m, Method)
        ccall(:jl_push_newly_inferred, Cvoid, (Any,), ci)
    end
    ccall(:jl_mi_cache_insert, Cvoid, (Any, Any), mi, ci)
    return cache
end

function haskey(wvc::InternalCodeCache, mi::MethodInstance)
    return ccall(:jl_rettype_inferred, Any, (Any, Any, UInt, UInt), wvc.owner, mi, first(wvc.worlds), last(wvc.worlds)) !== nothing
end

function get(wvc::InternalCodeCache, mi::MethodInstance, default)
    r = ccall(:jl_rettype_inferred, Any, (Any, Any, UInt, UInt), wvc.owner, mi, first(wvc.worlds), last(wvc.worlds))
    if r === nothing
        return default
    end
    return r::CodeInstance
end

function getindex(wvc::InternalCodeCache, mi::MethodInstance)
    r = get(wvc, mi, nothing)
    r === nothing && throw(KeyError(mi))
    return r::CodeInstance
end
