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
in(world::UInt, wr::WorldRange) = wr.min_world <= world <= wr.max_world
min_world(wr::WorldRange) = first(wr)
max_world(wr::WorldRange) = last(wr)

function intersect(a::WorldRange, b::WorldRange)
    ret = WorldRange(max(a.min_world, b.min_world), min(a.max_world, b.max_world))
    @assert ret.min_world <= ret.max_world
    return ret
end

function union(a::WorldRange, b::WorldRange)
    if b.min_world < a.min_world
        (b, a) = (a, b)
    end
    @assert a.max_world >= b.min_world - 1
    return WorldRange(a.min_world, b.max_world)
end

"""
    struct InternalCodeCache

Internally, each `MethodInstance` keep a unique global cache of code instances
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
