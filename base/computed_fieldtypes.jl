# This file is a part of Julia. License is MIT: https://julialang.org/license

# Installation of field generators for structs with computed field types
# (declared field-type slots holding `Core.ComputedFieldType` markers).
# `_set_fieldtype_generator!` in essentials.jl calls
# `_validate_fieldtype_generator` when it is defined.
#
# Field types are computed by running the generator in RCJulia mode
# (`Core._rcjulia_call`) at the struct's definition world: restricted-
# capability evaluation traps every operation whose result could depend on
# mutable state, so any result (or thrown error) is `:foldable` by
# construction — no effects proof is required, and `Base.@assume_effects`
# vouching plays no soundness role. `:nothrow` is not required either: a
# generator that throws for particular parameter values degrades those
# instantiations' field types to `Union{}` (see `invoke_fieldtype_generator`
# in src/jltypes.c).
#
# The definition world is captured as a `Core.WorldToken`, an opaque world
# capture that survives precompilation by mapping into the image's preserved
# world segment. Because dispatch inside the mode resolves against that
# frozen world, helper redefinition and invalidation cannot change a type's
# field types: instantiations in any later session compute them with the
# definition-time semantics.

"""
    fieldtype_generator(T::Type) -> Union{Core.FieldtypeGenerator, Tuple}

For a (possibly partially applied) type `T` whose field types are computed from
the values of its type parameters, return the partially applied field
generator: a plain reflection object — deliberately *not* part of the type
system — that is applied with `apply_type`, like a partially applied
`UnionAll`. Applying some of the remaining free parameters yields another
`Core.FieldtypeGenerator`; applying all of them yields the tuple of field
types, evaluated in RCJulia mode with the definition-time (world-frozen)
semantics. If `T` has no remaining free parameters, the tuple is returned
directly.

```julia
struct StaticMatrix{R,C,T}
    data::NTuple{(R::Int)*(C::Int), T}
end

g = Base.fieldtype_generator(StaticMatrix{2})
g{3}           # a Core.FieldtypeGenerator
g{3, Float64}  # === (NTuple{6, Float64},)
```

Unlike instantiating the type, errors thrown by the field type expressions
propagate rather than degrading the field types to `Union{}`.
"""
function fieldtype_generator(@nospecialize(ty::Type))
    dt = unwrap_unionall(ty)
    dt isa DataType ||
        throw(ArgumentError("expected a (partially applied) struct type"))
    tn = dt.name
    fg = isdefined(tn, :fieldgen, :monotonic) ? (@atomic :monotonic tn.fieldgen) : nothing
    fg isa SimpleVector ||
        throw(ArgumentError("$(tn.name) does not have computed field types"))
    if !(ty isa UnionAll)
        # fully applied already: the application is complete
        for p in dt.parameters
            if p isa TypeVar || (p isa Type && has_free_typevars(p))
                throw(ArgumentError("type parameters of $(ty) are not fully specified"))
            end
        end
        res = ccall(:jl_call_fieldtype_generator, Any, (Any, Any), tn, dt.parameters)
        nf = length(tn.names)
        (res isa Tuple && nfields(res) == nf) ||
            error("field generator for ", tn.name, " returned an invalid result")
        return res
    end
    return Core.FieldtypeGenerator(ty)
end

function show(io::IO, g::Core.FieldtypeGenerator)
    print(io, "Base.fieldtype_generator(")
    show(io, g.ty)
    print(io, ")")
end

_fieldgen_error(tn::Core.TypeName, msg...) =
    error("invalid computed field types for ", tn.name, ": ", msg...)

function _validate_fieldtype_generator(@nospecialize(ty), @nospecialize(gen))
    dt = unwrap_unionall(ty)
    dt isa DataType || error("expected a type with computed field types")
    tn = dt.name
    np = length(dt.parameters)
    world = get_world_counter()
    tt = Tuple{typeof(gen), Vararg{Any,np}}
    match = _which(tt; world, raise=false)
    match === nothing && _fieldgen_error(tn, "no matching field generator method")
    # No purity proof is required: the generator executes in RCJulia mode,
    # which enforces foldability dynamically (a generator performing a
    # restricted operation consistently throws `CapabilityError`, degrading
    # the affected instantiations' field types to `Union{}`). Capture the
    # definition world so those semantics are pinned forever.
    return Core.svec(gen, world_token())
end
