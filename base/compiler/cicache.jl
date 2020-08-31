"""
    struct InternalCodeCache

Internally, each `MethodInstance` keep a unique global cache of code instances
that have been created for the given method instance, stratified by world age
ranges. This struct abstracts over access to this cache.
"""
struct InternalCodeCache
end

function setindex!(cache::InternalCodeCache, ci::CodeInstance, mi::MethodInstance)
    ccall(:jl_mi_cache_insert, Cvoid, (Any, Any), mi, ci)
end

const GLOBAL_CI_CACHE = InternalCodeCache()

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

function intersect(a::WorldRange, b::WorldRange)
    ret = WorldRange(max(a.min_world, b.min_world), min(a.max_world, b.max_world))
    @assert ret.min_world <= ret.max_world
    return ret
end

"""
    struct WorldView

Takes a given cache and provides access to the cache contents for the given
range of world ages, rather than defaulting to the current active world age.
"""
struct WorldView{Cache}
    cache::Cache
    worlds::WorldRange
    WorldView(cache::Cache, range::WorldRange) where Cache = new{Cache}(cache, range)
end
WorldView(cache, args...) = WorldView(cache, WorldRange(args...))
WorldView(wvc::WorldView, wr::WorldRange) = WorldView(wvc.cache, wr)
WorldView(wvc::WorldView, args...) = WorldView(wvc.cache, args...)

function haskey(wvc::WorldView{InternalCodeCache}, mi::MethodInstance)
    ccall(:jl_rettype_inferred, Any, (Any, UInt, UInt), mi, first(wvc.worlds), last(wvc.worlds))::Union{Nothing, CodeInstance} !== nothing
end

function get(wvc::WorldView{InternalCodeCache}, mi::MethodInstance, default)
    r = ccall(:jl_rettype_inferred, Any, (Any, UInt, UInt), mi, first(wvc.worlds), last(wvc.worlds))::Union{Nothing, CodeInstance}
    if r === nothing
        return default
    end
    return r::CodeInstance
end

function getindex(wvc::WorldView{InternalCodeCache}, mi::MethodInstance)
    r = get(wvc, mi, nothing)
    r === nothing && throw(KeyError(mi))
    return r::CodeInstance
end

setindex!(wvc::WorldView{InternalCodeCache}, ci::CodeInstance, mi::MethodInstance) =
    setindex!(wvc.cache, ci, mi)
