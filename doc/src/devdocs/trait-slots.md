# Trait slots (prototype)

Wrappers such as `SubArray` or `ReshapedArray` hide the wrapped type from dispatch, so a method
for `::CuArray` does not apply to a view of one. Trait slots let a method dispatch on a property
computed from the argument type, such as whether an array is backed by GPU memory.

## Semantics

A **trait** is an ordinary function over types, here returning instances of a user-defined
hierarchy:

```julia
backend(::Type) = CPU()
backend(::Type{<:CuArray}) = GPU()
backend(::Type{<:SubArray{<:Any,<:Any,P}}) where {P} = backend(P)
```

A **guarded method** constrains a trait of an argument:

```julia
@guarded copyto!(dst::AbstractArray, src::AbstractArray) where {backend(dst)::GPU} = ...
```

It applies to a call if `backend(typeof(dst))` returns `v` with `Core.Typeof(v) <: GPU`. For a
type argument `X`, declared `::Type{T}` and guarded as `trait(T)`, the trait sees `Type{X}`.

The (argument, trait) pairs guarded by a function's methods form its **slot schema**. Inference
and dynamic dispatch cache misses may evaluate its traits even when no guarded signature
matches. Traits must be total and free of side effects, with results determined solely by
argument type and calling world. This contract is trusted.

**Specificity.** A method's bound for a slot is its guard, or `Any`. Method `a` is more specific
than `b` if its signature is more specific or equal, each of its bounds is a subtype of `b`'s,
and it is strictly more specific in one of them. So `(AbstractArray; GPU)` beats
`(AbstractArray; Any)`, while `(SubArray; Any)` and `(AbstractArray; GPU)` are ambiguous until
`(SubArray; GPU)` is defined. A signature never implies a guard: `(CuArray; Any)` and
`(AbstractArray; GPU)` are ambiguous too. Methods that differ only in their guards coexist.

**Unknown slots.** When the slot type is unknown (inference on an abstract argument type), a
guarded method whose bound intersects it may apply but does not cover its signature, so no
method it might shadow is selected and the call stays dynamic.

## Implementation

**Data.** `Method.guards` is `nothing` or a flat `svec` of `(argpos, trait, bound)` triples.
Call signatures, arity and calling conventions are unchanged. `jl_trait_schemas` (`src/gf.c`)
maps a function's `TypeName` to a flat `svec` of `(argpos, trait, min_world)`: a slot does not exist
before the world of the first method that guarded it. The registry is replaced copy-on-write
under the world counter lock. Slot types are passed to the matcher as `(argpos, trait, type)`
triples.

**Matching.** `ml_matches` checks each guard of a candidate against its slot type `S`:
`S <: bound` proves it, an empty intersection excludes the method, and otherwise the method may
apply without covering. If all guards are proven, the ordinary sort applies, with interference
sets computed from the specificity above. Otherwise `sort_guarded_matches` drops the matches a
covering match beats and reports an ambiguity. For a function with a schema, `ml_matches`
neither uses nor fills the dispatch cache.

**Run time.** On a dispatch cache miss, `jl_mt_assoc_by_type` calls the traits in the world of
the call, before taking locks; errors propagate. The cache entry is exact and valid in that
world only, so dynamic selections need no trait-change invalidation. The first dynamic call
per world and argument types must evaluate the traits again.

**Inference.** If an argument type determines a trait's value (it is concrete, or `Type{X}`),
the trait is called in the inference world, and the slot type is the return type of the trait's
CodeInstance. The caller gets an edge to that lookup and CodeInstance and is limited to their
worlds, so a new trait method, or a change to a function the trait calls, invalidates only the
code that used the value. Otherwise (abstract argument type, uninferred trait, overlay method
table) the slot is `Any`. `invoke` and `hasmethod` add the same edges and are not folded unless
the values were inferred exactly.

**Reflection.** `invoke`, `which` and `hasmethod` look up a region of types: a slot is evaluated
where the argument type determines it, and a guarded method is a candidate only if its guards
hold on the whole region. So `invoke(fill!, Tuple{UpperTriangular,Any}, A, x)` reaches the
unguarded method for a GPU-backed `A`. `Base.isambiguous` matches over the intersection of the
signatures and, per slot, of the guards. It does not split `Union` bounds, so pairs resolved only
by several methods covering separate parts of a bound are still reported as ambiguous.

**Invalidation.** Activating or deleting any method of a function with a schema, guarded or not,
by definition or package load, invalidates existing callers of the function
(`trait_invalidate_function`), as ordinary invalidation does not account for guards. Methods
activated in the new world are skipped; loaded code is subject to normal edge verification.

**`@guarded`.** `Base.Experimental.@guarded` strips the guards from the `where` clause and wraps
the last argument type, so that once that type is evaluated, the guards become pending for the
function and the current task (`jl_set_pending_method_guards`). This relies on lowering
evaluating the last argument type immediately before `jl_method_def`, which attaches guards
before insertion. Definitions evaluated earlier in the signature cannot take them. This side
channel stands in for lowering support.

**Not supported.** Keyword and default arguments; guards on a `Vararg` argument; precompilation
(schemas are not serialized, and guarded methods cannot be defined while generating output);
trait inference for abstract argument types; `methods(f, types)` treats every guard as unknown.
