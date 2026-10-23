# Trait slots (prototype)

Wrappers such as `SubArray` or `ReshapedArray` hide the wrapped type from dispatch, so a method
for `::CuArray` does not apply to a view of one. Trait slots let a method dispatch on a label
that a table assigns to the argument type, such as whether an array is backed by GPU memory.

## Semantics

A **trait** gives every type a **label**, a concrete subtype of the trait's declared top. Its
table holds at most one entry per type family:

```julia
abstract type Backend end
struct CPU <: Backend end
abstract type GPU <: Backend end
struct CUDADevice <: GPU end

@trait backend::Backend = CPU                   # top and default label
@trait backend(CuArray) = CUDADevice            # a fixed label
@trait backend(SubArray{T,N,P}) = backend(P)    # the label of a type parameter
```

A family without an entry has the default label. A fixed entry on an abstract family also
applies to its subtypes, which may then not have entries of their own: a subtree is sealed. A
delegating entry gives the label of one parameter, and is only allowed for families that are
not abstract. As a parameter is a value, a type parameter has the label all its values share,
and the default if they do not share one, so `SubArray{Float32,1,CuArray}` is on the GPU and
`SubArray{Float32,1,AbstractArray{Float32,2}}` is not. A parameter that is not a type has the
default label. No user code runs to compute a label.

A **guarded method** constrains the label of an argument:

```julia
@guarded copyto!(dst::AbstractArray, src::AbstractArray) where {backend(dst)::GPU} = ...
```

It applies to a call if the label of `Core.Typeof(dst)` is a subtype of `GPU`. For a type
argument `X`, `Type{X}` has the label of `X`.

The (argument, trait) pairs guarded by a function's methods form its **slot schema**.

**Specificity.** A method's bound for a slot is its guard, or `Any`. Method `a` is more specific
than `b` if its signature is more specific or equal, each of its bounds is a subtype of `b`'s,
and it is strictly more specific in one of them. So `(AbstractArray; GPU)` beats
`(AbstractArray; Any)`, while `(SubArray; Any)` and `(AbstractArray; GPU)` are ambiguous until
`(SubArray; GPU)` is defined. A signature never implies a guard: `(CuArray; Any)` and
`(AbstractArray; GPU)` are ambiguous too. Methods that differ only in their guards coexist.

**Regions.** For an argument type that is not a dispatch type, as in inference or `invoke`, the
slot type is the union of the labels its values can have, or the top. A free type variable
ranges over its bounds, including `Union{}` with the default label, unless it is the type of a
field that every instance initializes. So `SubArray{T,1,P} where P<:CuArray` has a single label,
as its parent has type `P`. A guarded method whose bound contains the slot type applies; one whose bound
intersects it may apply, but does not cover its signature, so no method it might shadow is
selected and the call stays dynamic.

**Entries.** Only the module that declared the trait or the type family may declare an entry,
and only the module that declared an entry may replace or delete it. An entry is for a whole
type family: `@trait backend(Vector) = ...` is rejected, as is an alias that restricts the
parameters of its family.

## Implementation

**Data.** `Method.guards` is `nothing` or a flat `svec` of `(argpos, trait, bound)` triples.
Call signatures, arity and calling conventions are unchanged. `jl_trait_schemas` (`src/gf.c`)
maps a function's `TypeName` to a flat `svec` of `(argpos, trait, min_world)`: a slot does not
exist before the world of the first method that guarded it. `jl_trait_tables` maps a trait to its
top, default, owner and entries, each `(typename, kind, value, module, min_world, max_world)`.
Both registries are replaced copy-on-write under the world counter lock. Slot types are passed to
the matcher as `(argpos, trait, type)` triples.

**Lookup.** `trait_region` walks a type: for a `DataType` it finds the entry of its family or of
an ancestor, and follows a delegation into the parameter with `trait_param_region`. Each lookup
also narrows a world range to the worlds in which every entry it consulted, or the absence of
one, holds. The runtime, inference and reflection share this lookup (`compute_trait_slots`).

**Matching.** `ml_matches` checks each guard of a candidate against its slot type `S`:
`S <: bound` proves it, an empty intersection excludes the method, and otherwise the method may
apply without covering. If all guards are proven, the ordinary sort applies, with interference
sets computed from the specificity above. Otherwise `sort_guarded_matches` drops the matches a
covering match beats and reports an ambiguity. For a function with a schema, `ml_matches`
neither uses nor fills the dispatch cache.

**Run time.** On a dispatch cache miss, `jl_mt_assoc_by_type` computes the slot types for the
call. The cache entry is exact and valid in that world only.

**Inference.** The slot types of a call signature come from the same lookup, and narrow the
valid worlds of the match. No edges are needed: changing an entry invalidates the callers of
every function guarding on the trait, including overlay methods and missing-method backedges of
calls and constructor calls.

**Reflection.** `invoke`, `which` and `hasmethod` look up a region of types, and a guarded
method is a candidate only if its guards hold on the whole region. So
`invoke(fill!, Tuple{UpperTriangular,Any}, A, x)` reaches the unguarded method for a GPU-backed
`A`. `Base.isambiguous` matches over the intersection of the signatures and, per slot, of the
guards. It does not split `Union` bounds, so pairs resolved only by several methods covering
separate parts of a bound are still reported as ambiguous.

**Invalidation.** Activating or deleting any method of a function with a schema, guarded or not,
by definition or package load, invalidates existing callers of the function
(`trait_invalidate_function`), as ordinary invalidation does not account for guards. Methods
activated in the new world are skipped; loaded code is subject to normal edge verification.
Adding, replacing or deleting an entry does the same for every function guarding on the trait
(`trait_invalidate_users`).

**`@guarded`.** `Base.Experimental.@guarded` strips the guards from the `where` clause and wraps
the last argument type, so that once that type is evaluated, the guards become pending for the
function and the current task (`jl_set_pending_method_guards`). This relies on lowering
evaluating the last argument type immediately before `jl_method_def`, which attaches guards
before insertion. Definitions evaluated earlier in the signature cannot take them. This side
channel stands in for lowering support.

**Not supported.** Keyword and default arguments; guards on a `Vararg` argument; combining
`@guarded` with `@overlay`, whose definitions it would move into a local scope; precompilation
(schemas and tables are not serialized, and neither guarded methods nor traits can be defined
while generating output); delegation by abstract families or to a combination of parameters,
such as the arrays of a `StructArray`; `methods(f, types)` treats every guard as unknown.
