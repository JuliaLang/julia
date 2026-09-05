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
# Whether `w` is on the spine's trunk: only trunk worlds may publish interval
# validity, since their positions in the spine's total order are themselves.
# Worlds of merged side branches (e.g. grafted image histories) reach the
# spine but are not on its trunk; results computed at them publish point
# validity instead.
world_on_spine(w::UInt) =
    ccall(:jl_world_on_trunk, Cint, (Csize_t, Csize_t), w, get_world_counter()) != 0
# The earliest world in the observer's total order whose history includes
# both `a` and `b`. For comparable worlds this is simply the later of the
# two; for worlds on different branches it is the later of their merge
# points on the observer's trunk.
function world_join(a::UInt, b::UInt, observer::UInt)
    world_seg(a) == world_seg(b) && return max(a, b)
    return ccall(:jl_world_join, Csize_t, (Csize_t, Csize_t, Csize_t), a, b, observer) % UInt
end
# Combine two validity caps; 0 if their invalidation cones are incomparable
# (see jl_world_cap_meet), in which case the caller must degrade to point
# validity.
world_cap_meet(a::UInt, b::UInt) =
    ccall(:jl_world_cap_meet, Csize_t, (Csize_t, Csize_t), a, b) % UInt

in(world::UInt, wr::WorldRange) = world_in_range(world, wr.min_world, wr.max_world)

# Intersect two validity regions, as observed from `observer` (the world the
# result will be used at). Both endpoints may lie off the observer's trunk
# (e.g. a min_world within a grafted image history together with a cap from a
# later spine invalidation); such intervals are nonempty whenever the cap's
# invalidation cone does not contain the min. Only when the two caps'
# invalidation cones are incomparable (shadowing events on two different side
# branches) can a single interval not represent the region, and the result
# degrades to point validity at the observer.
@inline function intersect(a::WorldRange, b::WorldRange, observer::UInt)
    minw = world_join(a.min_world, b.min_world, observer)
    maxw = world_cap_meet(a.max_world, b.max_world)
    if iszero(maxw)
        @assert world_in_range(observer, a.min_world, a.max_world) &&
            world_in_range(observer, b.min_world, b.max_world) "attempting to intersect disjoint world ranges"
        return WorldRange(observer, observer)
    end
    @assert world_at_most(minw, maxw) "attempting to intersect disjoint world ranges"
    return WorldRange(minw, maxw)
end
intersect(a::WorldRange, b::WorldRange) = intersect(a, b, get_world_counter())

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
