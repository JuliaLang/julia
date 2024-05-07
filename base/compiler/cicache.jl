# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    struct InternalCodeCache

The internal code cache is keyed on type specializations, represented by
MethodSpecialization{DefaultSpec} aka MethodInstance. External abstract
interpreters may use this same structure by using a different `Spec` for their
`MethodSpecialization{Spec}`. `InternalCodeCache` will match such specializations
by type. Additionally, it is possible to specialize methods on properties other
than types, but this requires custom caching logic. `InternalCodeCache` currently
only supports type-based specialization.
"""
struct InternalCodeCache
    mitype::DataType # <: MethodSpecialization, but stored as DataType for efficient ===
    InternalCodeCache(T::Type{<:MethodSpecialization}) =
        new(T)
end
InternalCodeCache() = InternalCodeCache(MethodInstance)

function setindex!(cache::InternalCodeCache, ci::CodeInstance, mi::MethodInstance)
    ms::MethodSpecialization = mi
    while typeof(ms) !== cache.mitype
        if !isdefined(ms, :next)
            # No specialization for this spec. Try to allocate it now.
            newms = cache.mitype(mi.def, mi.specTypes)
            if @atomiconce :sequentially_consistent (ms.next = newms)
                ms = newms
                break
            end
        end
        ms = @atomic :acquire ms.next
    end
    ccall(:jl_mi_cache_insert, Cvoid, (Any, Any), ms, ci)
    return cache
end

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
    ms::MethodSpecialization = mi
    while typeof(ms) !== wvc.cache.mitype
        isdefined(ms, :next) || return false
        ms = ms.next
    end
    return ccall(:jl_rettype_inferred, Any, (Any, UInt, UInt), ms, first(wvc.worlds), last(wvc.worlds)) !== nothing
end

function get(wvc::WorldView{InternalCodeCache}, mi::MethodInstance, default)
    ms::MethodSpecialization = mi
    while typeof(ms) !== wvc.cache.mitype
        isdefined(ms, :next) || return default
        ms = ms.next
    end
    r = ccall(:jl_rettype_inferred, Any, (Any, UInt, UInt), ms, first(wvc.worlds), last(wvc.worlds))
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

function setindex!(wvc::WorldView{InternalCodeCache}, ci::CodeInstance, mi::MethodInstance)
    setindex!(wvc.cache, ci, mi)
    return wvc
end

function code_cache(interp::AbstractInterpreter)
    cache = InternalCodeCache()
    worlds = WorldRange(get_inference_world(interp))
    return WorldView(cache, worlds)
end
