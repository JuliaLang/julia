# Trait slots (prototype)

Wrappers such as `SubArray`, `Adjoint` or `ReshapedArray` hide the wrapped type from dispatch,
so a method for `::CuArray` does not apply to a view of one. Whether an array is backed by GPU
memory is a function of its type structure, which a *trait* can compute. Trait slots let methods
dispatch on such a computed property.

## Semantics

A **trait** is an ordinary function over types whose results are ordered by a user-defined
type hierarchy:

```julia
abstract type Backend end
struct CPU <: Backend end
struct GPU <: Backend end

backend(::Type) = CPU()
backend(::Type{<:CuArray}) = GPU()
backend(::Type{<:SubArray{<:Any,<:Any,P}}) where {P} = backend(P)
```

A trait must be total and free of side effects: its result may only depend on its argument and
the world. Traits are called during dispatch and inference, any number of times.

A **guarded method** constrains a trait of an argument:

```julia
@guarded copyto!(dst::AbstractArray, src::AbstractArray) where {backend(dst)::GPU} = ...
```

The method only applies to a call if `backend(typeof(dst))` returns a value `v` with
`Core.Typeof(v) <: GPU`. If the guarded argument is a type `X` (declared `::Type{T}` and guarded
as `trait(T)`), the trait is called on `Type{X}`.

The (argument, trait) pairs used by the guards of a function's methods form its **slot schema**.
Each slot has a **slot type**: the type of the trait's value for the call, or `Any` if it is not
known. A method's bound for a slot is its guard, or `Any` if it has none.

**Specificity.** Method `a` is more specific than method `b` if `a`'s signature is more specific
than or equal to `b`'s (as for ordinary methods), each of `a`'s slot bounds is a subtype of
`b`'s, and `a` is strictly more specific in one of the two. So `(AbstractArray; GPU)` is more
specific than `(AbstractArray; Any)`, while `(SubArray; Any)` and `(AbstractArray; GPU)` are
ambiguous, which a method `(SubArray; GPU)` resolves. A signature never implies a guard: even
though `backend(CuArray) === GPU()`, `(CuArray; Any)` and `(AbstractArray; GPU)` are ambiguous.
Methods with the same signature and different guards are different methods.

**Dispatch.** A call computes the slot types from the argument types, in the world of the call,
and selects the most specific applicable method as usual: a guarded method applies if each slot
type is a subtype of its bound.

**Unknown slots.** When a slot type is not known (inference on an abstract argument type), a
guarded method whose bound intersects it may apply, but does not cover its signature: no method
it might shadow is selected, and the call stays dynamic.

## Implementation

### Data

- `Method.guards`: `nothing`, or a flat `svec` of `(argpos, trait, bound)` triples, `argpos`
  counting from 1 after the function. Signatures, arity and ABI are unchanged.
- `jl_trait_schemas` (`src/gf.c`): a flat `svec` of `(TypeName, schema)` pairs, a schema being a
  flat `svec` of `(argpos, trait, min_world)` triples. A slot does not exist in the worlds
  before `min_world`, the world of the first method that guarded it, so older worlds do not
  call traits that may not exist there. Schemas only grow. The registry is replaced
  copy-on-write under the world counter lock, so readers take no lock.
- Slot types are passed to the matcher as a flat `svec` of `(argpos, trait, slot type)`
  triples; `NULL`/`nothing` means all unknown.

### Matching

`ml_matches` takes the slot types. After the structural intersection, each guard of a candidate
is checked against its slot type `S`: `S <: bound` proves it, an empty intersection excludes the
method, and otherwise the method may apply but does not cover its signature. If every guard of
the matches is proven, the ordinary sort applies, with interference sets computed using the
specificity above (`method_morespecific_ins`). Otherwise `sort_guarded_matches` drops the
matches that a fully covering match is more specific than and reports an ambiguity, which keeps
inference from selecting a method.

For a function with a schema, `ml_matches` neither uses nor fills the dispatch cache, since its
entries hold selections for slot values computed at run time.

### Run-time dispatch

On a dispatch cache miss, `jl_mt_assoc_by_type` calls the traits of the schema on the argument
types in the world of the call, before taking any lock. Errors propagate to the caller. The
resulting cache entry is keyed on the exact argument types and is only valid in that world, so a
change to a trait or anything it calls cannot leave a stale selection behind. This makes each
first call per world and argument type pay for the traits and the matching.

### Inference

`compute_trait_slots` (`Compiler/src/abstractinterpretation.jl`) computes the slot types of a
call signature. If an argument type determines a trait's value (it is concrete, or `Type{X}`),
the trait is called in the inference world, which also infers it, and the slot type is the
return type inferred for that call. The caller gets an ordinary edge for the trait call and is
limited to the worlds of its lookup and of the trait's CodeInstance, so a new trait method, or a
change to a function the trait calls, invalidates the code that depends on the value. Otherwise
(abstract argument type, trait not inferred, overlay method tables) the slot is `Any`.

`invoke` and `hasmethod`, whose slots the runtime computes, add the same edges, and are not
folded if the runtime may rely on a value that inference did not prove exactly.

### Reflection

`invoke(f, T, args...)`, `which(f, T)` and `hasmethod(f, T)` look up the methods for the region
`T`: a slot is evaluated if an argument type in `T` determines it, and is unknown otherwise, and
a guarded method is only a candidate if its guards hold on all of `T`. So `which` with concrete
types returns what a call selects, and `invoke(fill!, Tuple{UpperTriangular,Any}, A, x)` reaches
the unguarded method for `UpperTriangular` even for a GPU-backed `A`.

`Base.isambiguous(m1, m2)` matches over the region where both apply: the intersection of their
signatures and, per slot, of their guards (disjoint guards never apply to the same call).
Guarded methods whose guards do not hold on all of that region cannot resolve the ambiguity.
Tie-breakers that only cover a region together, like one per member of a `Union` bound, are not
recognized, so such a pair is reported as ambiguous. `Test.detect_ambiguities` builds on
`isambiguous`.

### Invalidation

Adding or deleting a guarded method invalidates all code that depends on dispatch of its
function (`trait_invalidate_function`). This is coarse: making the ordinary insertion
invalidation take guards into account would limit it to what the method can take calls from.
Changes to traits need no special handling (see above).

### `@guarded`

`Base.Experimental.@guarded` removes the guards from the `where` clause, sets them as pending for
the function and the current task (`jl_set_pending_method_guards`), and evaluates the plain
definition, holding a lock; `jl_method_def` attaches the pending guards to the method it creates
for that function before inserting it. This global side channel stands in for lowering support.

### Not supported

- Keyword and default arguments, and guards on a `Vararg` argument.
- Precompilation: schemas are not serialized, and guarded methods cannot be defined while
  generating output.
- Abstract interpretation of traits for non-concrete argument types.
- `methods(f, types)` treats every guard as unknown.
