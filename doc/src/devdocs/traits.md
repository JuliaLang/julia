# Traits and guarded methods

Wrappers such as `SubArray` or `Transpose` hide the type they wrap from dispatch: a method for
`::CuArray` does not apply to a view of one, and a method for views cannot say "of an array on
the GPU". *Traits* let a method dispatch on a label that a table derives from the argument type,
through any nesting of wrappers. They are experimental, see
[`Base.Experimental.@trait`](@ref) and [`Base.Experimental.@guarded`](@ref).

```julia
abstract type Backend end
struct CPU <: Backend end
abstract type GPU <: Backend end
struct CUDA <: GPU end

@trait backend::Backend = CPU                   # the labels are below `Backend`; `CPU` by default
@trait backend(CuArray) = CUDA                  # a fixed label
@trait backend(SubArray{T,N,P}) = backend(P)    # the label of a type parameter

@guarded copyto!(dst::AbstractArray, src::AbstractArray) where {backend(dst)::GPU} = ...
```

## Semantics

A **trait** gives every type one **label**, a concrete subtype of the trait's top. Its table has
at most one entry per type family (`TypeName`):

- A *fixed* entry gives the family a label. On an abstract family it also holds for all
  subtypes, which then cannot have entries: the label of an abstract type is a promise about
  types that do not exist yet.
- A *delegating* entry gives a type the label of one of its type parameters. Only families
  that are not abstract can delegate.
- A family without an entry has the default label.

A type parameter is a value, not an argument, so it needs a rule of its own: it has the label
all its values share, and the default label if they do not share one or if it is not a type.
`SubArray{Float32,1,CuArray}` is on the GPU, `SubArray{Float32,1,AbstractArray}` is not. For a
type argument `X`, of type `Type{X}`, the label is that of `X`. No user code runs to compute a
label.

A **guard** `backend(dst)::GPU` restricts a method to the calls where the label of `dst` is a
subtype of `GPU`. Guards are part of a method's identity: methods with the same signature and
different guards coexist.

**Specificity.** Dispatch calls the applicable method that is more specific than every
other applicable one. Method `a` is more specific than `b` if:

1. It is in the ordinary sense, extended to guards: its signature is more specific or equal,
   it has a guard at least as narrow for each guard of `b`, and one of the two strictly so.
   `(SubArray; GPU)` beats `(AbstractArray; GPU)`, which beats `(AbstractArray; -)`.
2. Otherwise, if `b` is not more specific in that sense either: it requires narrower labels.
   For each argument and trait that either method guards, a method *requires* the labels
   within the bound of its guard, if it has one, among those the table gives the types its
   signature matches. `(AbstractArray; GPU)` requires a label below `GPU`, `f(::CuArray)` the
   label `CUDA`, and `f(::SubArray)` or `f(::AbstractVector)` any label. `a` wins if what it
   requires is within what `b` requires for each of them, and narrower for one.
3. If they require the same labels: its signature is more specific.

So `(AbstractArray; GPU)` is called for a view of a `CuArray` rather than `f(::SubArray)`, and
`f(::CuArray)` for a `CuArray`. If each of two methods requires labels that the other does
not, as with guards on different arguments or traits, the pair stays unordered: a call they
both apply to is ambiguous unless a third method is more specific than both.

Without this rule, a guarded method would be ambiguous with every unguarded method for a
narrower signature, such as `f(::AbstractVector)`: defining one would break the packages that
guard `f`. A trait selects an implementation for a kind of argument, in any shape; signatures
specialize implementations for that kind. A guarded method is thus responsible for all the
shapes it takes over, and can forward to an unguarded method with `invoke`. This is also how
a method for a `Union` of wrapper types is ordered today.

**Non-concrete types.** For inference and reflection, an argument type that is not concrete has
a *region*: the union of the labels its values can have, or the top. An abstract type without
a fixed entry has the top. A free type variable ranges over its bounds, including `Union{}`
with the default label, unless it is the type of a field that every instance initializes: so
`SubArray{T,1,P} where P<:CuArray` has the label `CUDA`, as its parent has type `P`. A guard
*holds* for a signature if the region is a subtype of its bound, *fails* if they are disjoint,
and is otherwise undecided.

**Entries.** An entry is for a whole type family; `@trait backend(Vector) = ...` is rejected.
Any module can declare an entry, like any module can add a method. Declaring the entry a
family already has does nothing; only the module that declared an entry can replace or delete
it. Entries are versioned by world age, like methods.

## Implementation

**Data.** A trait is a `Core.Trait`. Its `entries` map a `TypeName` to the records of that
family, `(kind, value, module, min_world, max_world)`; the table is replaced copy-on-write under
the world counter lock and read without a lock. `Method.guards` is `nothing` or a flat `svec` of
`(argpos, trait, bound)`. `@guarded` declares a guarded argument as
`x::Core.Guard(T, trait, bound)`, which `jl_method_def` takes as an argument of type `T` with a
guard. Lowering copies argument declarations to the methods it generates, so keyword arguments
work: the `Core.kwcall` method has the same guards. A method that leaves out an optional
argument has no declaration for it, and so no guard on it.

**Matching** (`src/gf.c`). `ml_matches` checks the guards of each candidate against the region
of the query's argument type (`check_method_guards`), and narrows the world range of the result
to the worlds in which the tables give the same answer. A method with a failing guard is not a
match. One with an undecided guard may apply, but does not cover the query; lookups that need
a covering method (`invoke`, `which`, `hasmethod`) skip it. So
`invoke(f, Tuple{SubArray}, x)` reaches the unguarded method for views.

**Specificity.** The interference sets only record the first rule above, which does not
depend on the tables. So when the candidates include a guarded method, the ordinary sort is
not used. Either one candidate covers the query and is more specific than every other one
(`guarded_winner`, `method_beats`), or the candidates that may be called are returned as
ambiguous (`prune_guarded_matches`): inference then keeps the call dynamic, and a concrete
call fails.

**Caching.** The dispatch cache needs nothing special: the method selected for a concrete call
only depends on its types and the world. A cache entry is not widened to a signature for
which a guard is undecided.

**Shortcuts.** Julia often derives that a method applies, or dominates another, from signatures
alone: when verifying the edges of a package image, when deciding what a new method
invalidates, in the method table search. None of that holds for a function with guarded
methods. `JL_TN_GUARDED` in the `trait_flags` of the function's `TypeName` marks such a
function (for `Core.kwcall`, the function called), and these shortcuts are skipped for it in
favor of a lookup.

**Invalidation.** A new method invalidates what it would without guards, except the
specializations, cache entries and missing-method edges for which one of its guards fails.
Changing an entry can change any selection among methods guarded on the trait, so it
invalidates all callers of the functions that have such methods
(`trait_invalidate_users`). But every lookup marks the type families whose entry it reads, or
finds missing, with `JL_TN_CONSULTED`. Changing the entry of an unmarked family cannot affect
any cached result and invalidates nothing. That is the common case: an entry declared with
its type, before any call sees it.

**Images.** A trait and its table are saved with the image that created it. The entries a
package declares for the traits of other packages are saved separately, and declared again
when the image is loaded, before its methods are activated.

## Limitations

- Guards on a `Vararg` argument, and bounds that depend on static parameters.
- Delegation to a combination of parameters, such as the component arrays of a `StructArray`.
- An abstract family cannot say that its subtypes have labels below some bound, such as
  "every `AbstractGPUArray` is on some GPU". A method for it therefore implies no guard.
- A label query `backend(T)` is a call at run time; inference does not fold it.
- Where a guarded method is involved, `Base.isambiguous` only considers the methods that
  apply to all calls two methods share, and does not see that two arguments of the same type
  have the same label.
- Changing the entry of a type family that has been used invalidates more than necessary.

## API

```@docs
Base.Experimental.@trait
Base.Experimental.@guarded
Base.Experimental.delete_trait_entry
```
