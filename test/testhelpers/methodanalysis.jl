# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base: Callable, IdSet
using Core: MethodInstance

"""
    is_atrisk_type(tt)

Given a Tuple-type signature (e.g., `Tuple{typeof(sum),Vector{Int}}`), determine whether this signature
is "at risk" for invalidation. Essentially it returns `true` if one or more arguments are of abstract type,
although there are prominent exceptions:

- Constructor calls with arbitrary argument types
- `convert(X, x)` where `isa(x, X)`
- `setindex!` and `push!` methods where the valtype is a subtype of the eltype (for AbstractDicts, likewise for the keytype)
- `getindex`, `length`, `isempty`, and `iterate` on any tuple

All of these are "allowed," meaning that they return `false`.
Moreover, some specific non-concrete argument types---like `Union`s of concrete types and `Function`---
do not trigger a return of `true`, although other at-risk argument types can lead to an overall `true` return
for the signature.
"""
function is_atrisk_type(@nospecialize(typ))
    # signatures like `convert(Vector, a)`, `foo(::Vararg{Synbol,N}) where N` do not seem to pose a problem
    isa(typ, TypeVar) && return false
    # isbits parameters are not a problem
    isa(typ, Type) || return false
    if isa(typ, UnionAll)
        typ = Base.unwrap_unionall(typ)
    end
    # Exclude signatures with Union{}
    typ === Union{} && return false
    isa(typ, Union) && return is_atrisk_type(typ.a) | is_atrisk_type(typ.b)
    # Type{T}: signatures like `convert(::Type{AbstractString}, ::String)` are not problematic
    typ <: Type && return false
    if typ <: Tuple && length(typ.parameters) >= 1
        p1 = typ.parameters[1]
        # Constructor calls are not themselves a problem (any `convert`s they trigger might be, but those are covered)
        isa(p1, Type) && p1 <: Type && return false
        # convert(::Type{T}, ::S) where S<:T is not problematic
        if p1 === typeof(Base.convert) || p1 === typeof(Core.convert)
            p2, p3 = typ.parameters[2], typ.parameters[3]
            if isa(p2, Type)
                p2 = Base.unwrap_unionall(p2)
                if isa(p2, DataType) && length(p2.parameters) === 1
                    T = p2.parameters[1]
                    if isa(T, TypeVar)
                        T = T.ub
                    end
                    isa(p3, Type) && isa(T, Type) && p3 <: T && return false
                end
            end
        # `getindex`, `length`, etc are OK for various Tuple{T1,T2,...}
        elseif p1 === typeof(Base.getindex) ||
               p1 === typeof(Base.length)  ||
               p1 === typeof(Base.isempty) ||
               p1 === typeof(Base.iterate) || p1 === typeof(Core.iterate)
            p2 = typ.parameters[2]
            if isa(p2, Type)
                p2 = Base.unwrap_unionall(p2)
                p2 <: Tuple && return false
            end
        # show(io::IO, x) is OK as long as typeof(x) is safe
        elseif p1 === typeof(Base.show) || p1 === typeof(Base.print) || p1 === typeof(Base.println)
            # is_atrisk_type(typ.parameters[2]) && return true
            for i = 3:length(typ.parameters)
                is_atrisk_type(typ.parameters[i]) && return true
            end
            return false
        # setindex!(a, x, idx), push!(a, x), and similar are safe if typeof(x) <: eltype(a)
        elseif (p1 === typeof(Base.setindex!) || p1 === typeof(Base.push!) || p1 === typeof(Base.pushfirst!) ||
                p1 === typeof(Base.setproperty!)) && length(typ.parameters) >= 3
            p2, p3 = typ.parameters[2], typ.parameters[3]
            (isa(p2, TypeVar) || isa(p3, TypeVar)) && return true
            if p2 <: AbstractDict && length(typ.parameters) >= 4
                p4 = typ.parameters[4]
                isa(p4, TypeVar) && return true
                p3 <: safe_valtype(p2) && p4 <: safe_keytype(p2) && return false
                p2 <: IdDict && return false   # these are annotated @nospecialize
            elseif p2 <: Base.RefValue && length(typ.parameters) >= 4
                p4 = typ.parameters[4]
                isa(p4, TypeVar) && return true
                p4 <: eltype(p2) && return false
            else
                p3 <: eltype(p2) && return false
            end
        # likewise for `get!`
        elseif p1 === typeof(Base.get!) && length(typ.parameters) >= 4
            p3, p4 = typ.parameters[2], typ.parameters[4]
            (isa(p3, TypeVar) || isa(p4, TypeVar)) && return true
            if p3 <: AbstractDict
                p4 <: safe_valtype(p3) && return false
                p3 <: IdDict && return false   # these are annotated @nospecialize
            end
        # cull some @nospecialize methods
        elseif p1 === typeof(Base.which) || p1 === typeof(Base.hasmethod) || p1 === typeof(Base.rethrow)
            return false
        end
    end
    # Standard DataTypes
    isconcretetype(typ) && return false
    # ::Function args are excluded
    typ === Function && return false
    !isempty(typ.parameters) && (any(is_atrisk_type, typ.parameters) || return false)
    return true
end

safe_valtype(::Type{<:AbstractDict{K,V}}) where {K,V} = @isdefined(V) ? V : Union{}
safe_valtype(::Type) = Union{}
safe_keytype(::Type{<:AbstractDict{K,V}}) where {K,V} = @isdefined(K) ? K : Union{}
safe_keytype(::Type) = Union{}

# Get the name of a method as written in the code. This strips keyword-method mangling.
function codename(sym::Symbol)
    symstr = String(sym)
    # Body methods
    m = match(r"^#(.*?)#\d+$", symstr)
    m !== nothing && return Symbol(only(m.captures))
    # kw methods
    m = match(r"^(.*?)##kw$", symstr)
    m !== nothing && return Symbol(only(m.captures))
    return sym
end

isexported(mi::MethodInstance) = isdefined(Main, codename(mi.def.name))
getfunc(mi::MethodInstance) = getfunc(mi.def)
getfunc(m::Method) = getfield(m.module, m.name)
nmethods(mi::MethodInstance) = length(methods(getfunc(mi)))

# Test whether a module is Core.Compiler or inside it
# (Methods there are protected from invalidation by other means)
function fromcc(mod::Module)
    fn = fullname(mod)
    return length(fn) >= 2 && fn[1] === :Core && fn[2] === :Compiler
end

function atrisk_method(m::Method, atrisk_backedges)
    for mi in methodinstances(m)
        mi ∈ atrisk_backedges && return true
    end
    return false
end

function atrisk_triggers(m::Method, atrisk_instances)
    triggers = Set{MethodInstance}()
    for mi in atrisk_instances
        if atrisk_method(m, all_backedges(mi))
            push!(triggers, mi)
        end
    end
    return triggers
end

# This removes MethodInstances that no one in their right mind should ever invalidate by specialization.
function remove_unlikely_methodinstances(list)
    out = MethodInstance[]
    for mi in list
        mi = mi::MethodInstance   # must have MethodInstance elements
        # All `continue` statements below omit the MethodInstance
        name = codename(mi.def.name)
        name ∈ (:invokelatest, :unwrap_unionall, :rewrap_unionall) && continue
        # Vararg methods for printing etc
        if name ∈ (:print, :println, :sprint, :string, :error)
            nargs = length(mi.specTypes.parameters)   # includes the #self argument
            mparams = (Base.unwrap_unionall(mi.def.sig)::DataType).parameters
            (nargs > length(mparams) || mparams[nargs] <: Vararg) && continue
        end
        # No one should ever specialize on notify or schedule's `val` argument
        name === :notify && !is_atrisk_type(mi.specTypes.parameters[2]) &&
            !any(is_atrisk_type, mi.specTypes.parameters[4:end]) && continue
        name === :schedule && !any(is_atrisk_type, mi.specTypes.parameters[2:end-1]) && continue
        # Add more removal-filters here

        # We've decided to keep it
        push!(out, mi)
    end
    return out
end

# Check for inference quality in specific functions.
# This is valid only for functions that should always return a particular type for any valid call of their methods.
function function_returns(@nospecialize(f), @nospecialize(typ); allow_missing_for_missing=true, minargs=0)
    for m in methods(f)
        sig = Base.unwrap_unionall(m.sig)
        for rt in Base.return_types(Base.call_type(Base.unwrap_unionall(m.sig))...)
            rt <: typ && continue
            if allow_missing_for_missing && any(T->T===Missing, sig.parameters[2:end]) && rt === Missing
                continue
            end
            length(sig.parameters) - 1 < minargs && continue
            return false
        end
    end
    return true
end

function methodinstances()
    visited = IdSet{Any}()
    mis = MethodInstance[]
    for mod in Base.loaded_modules_array()
        methodinstances!(mis, mod, visited)
    end
    return mis
end

function methodinstances(@nospecialize parent)
    visited = IdSet{Any}()
    mis = MethodInstance[]
    return methodinstances!(mis, parent, visited)
end


function methodinstances!(mis, mod::Module, visited)
    mod ∈ visited && return mis
    push!(visited, mod)
    for nm in names(mod; all=true)
        if isdefined(mod, nm)
            obj = getfield(mod, nm)
            if isa(obj, Module)
                methodinstances!(mis, obj, visited)
            elseif isa(obj, Callable)
                methodinstances!(mis, obj, visited)
            end
        end
    end
    return mis
end

function methodinstances!(mis, @nospecialize(f::Callable), visited)
    f ∈ visited && return nothing
    f === Vararg && return nothing   # methods(Varargs) errors due to Type{Vararg}
    push!(visited, f)
    for m in methods(f)
        methodinstances!(mis, m, visited)
    end
    return mis
end

function methodinstances!(mis, m::Method, visited)
    m ∈ visited && return nothing
    m === Vararg && return nothing   # methods(Varargs) errors due to Type{Vararg}
    push!(visited, m)
    Base.visit(m.specializations) do mi
        push!(mis, mi)
    end
    return mis
end

all_backedges(mi::MethodInstance) = all_backedges!(Set{MethodInstance}(), mi)

function all_backedges!(backedges, mi)
    push!(backedges, mi)
    if isdefined(mi, :backedges)
        for be in mi.backedges
            be ∈ backedges && continue
            all_backedges!(backedges, be)
        end
    end
    return backedges
end
