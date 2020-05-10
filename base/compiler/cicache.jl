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

"""
    struct WorldView

Takes a given cache and provides access to the cache contents for the given
range of world ages, rather than defaulting to the current active world age.
"""
struct WorldView{Cache}
    cache::Cache
    min_world::UInt
    max_world::UInt
end
WorldView(cache, r::UnitRange) = WorldView(cache, first(r), last(r))
WorldView(cache, world::UInt) = WorldView(cache, world, world)
WorldView(wvc::WorldView{InternalCodeCache}, min_world::UInt, max_world::UInt) =
    WorldView(wvc.cache, min_world, max_world)

function haskey(wvc::WorldView{InternalCodeCache}, mi::MethodInstance)
    ccall(:jl_rettype_inferred, Any, (Any, UInt, UInt), mi, wvc.min_world, wvc.max_world)::Union{Nothing, CodeInstance} !== nothing
end

function get(wvc::WorldView{InternalCodeCache}, mi::MethodInstance, default)
    r = ccall(:jl_rettype_inferred, Any, (Any, UInt, UInt), mi, wvc.min_world, wvc.max_world)::Union{Nothing, CodeInstance}
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
