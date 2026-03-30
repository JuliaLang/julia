# Plan For ImmediateOrRef Support

This note outlines a plan for implementing the feature discussed in
[JuliaLang/julia#47735](https://github.com/JuliaLang/julia/issues/47735):
a Julia-managed reference type that can hold either a normal heap reference
or a small immediate value encoded in the reference bits.

The issue discussion converges on a few points that should shape the work:

- The feature should be a general runtime primitive, not a one-off `BigInt`
  optimization.
- For the prototype, the public name is `ImmediateOrRef`.
- The runtime must participate directly. A pure Julia wrapper around `Ptr`
  does not solve GC scanning, write barriers, or serialization.
- The first implementation should focus on making the primitive correct and
  usable. Follow-on consumers such as faster big integers, short strings, or
  compact AST/source-location fields can come later.
- The initial target is 64-bit Julia only, and the datatype should always
  occupy 64 bits.

## Goals

- Add a first-class Julia type for storing either an immediate payload or a
  Julia object reference in one machine word.
- Preserve GC correctness, including tracing, remembered sets, and system
  image serialization.
- Expose a small API that lets user code test whether a value is immediate and
  extract or install the raw payload cheaply.
- Keep the design general enough that future optimizations for strings,
  algebraic types, and compact unions can reuse the same machinery.

## Non-goals for the first milestone

- Replacing `BigInt`, `BigFloat`, or `String` in Base immediately.
- Auto-optimizing arbitrary `Union{BitsType, RefType}` fields.
- Designing a full tagged-union feature for all Julia aggregates.
- Supporting every possible packed layout from day one.

## Prototype decisions

These choices are fixed for the first prototype unless implementation work
shows that one of them is untenable:

- `ImmediateOrRef{T}` is a dedicated primitive type with 64-bit storage.
- The prototype is supported on 64-bit Julia only.
- The first implementation targets struct fields and ordinary object
  serialization on the stock GC. Tagged array / `Memory` elements and MMTK
  support can come later.
- The runtime distinguishes only two states at the representation level:
  ordinary Julia reference and immediate payload. Higher-level payload schemes
  are left to user code.

### Proposed Core/Base shape

The intended surface shape is:

```julia
primitive type ImmediateOrRef{T} <: Ref{T} 64 end
```

with Base methods layered on top for `getindex`, `isassigned`, `isimmediate`,
`getrawvalue`, and `setrawvalue`.

Subtyping `Ref{T}` keeps the user model close to the original issue and aligns
with existing `Ref`-based APIs. The runtime representation is still special:
it is not a raw native pointer and it cannot be modeled correctly as an
ordinary `primitive type` without runtime support.

### Proposed encoded representation

At the machine level, an `ImmediateOrRef` slot stores one `UInt64`.

- `0` means "unassigned".
- a word with low bit `0` and nonzero value is a normal `jl_value_t*`
  reference;
- a word with low bit `1` is an immediate payload.

This keeps the GC rule simple: only nonzero words with low bit `0` are traced.
It also matches the original issue sketch.

The prototype should assume that Julia object references stored in
`ImmediateOrRef` have at least three low zero bits on supported 64-bit builds.
However, only bit 0 needs to be interpreted by the runtime itself in v1.
Bits 1 and 2 remain available to user-level encoding schemes.

### Scope restriction for v1

The first implementation should support `ImmediateOrRef` in struct fields before
attempting to support it in `Memory`/array element layouts.

The current prototype also targets the stock Julia GC only. Extending the
design to MMTK is deferred until the core approach has been validated and the
MMTK maintainers can coordinate the binding-side changes.

That restriction keeps the initial runtime work bounded:

- datatype layout changes are local to normal object fields;
- GC scanning can be implemented as an extension of object scanning first;
- serialization can piggyback on ordinary object field traversal;
- codegen only needs field load/store support at first.

If that lands cleanly, array-like storage can reuse the same slot semantics in
a follow-up change.

### Preferred layout-metadata strategy

The existing object layout machinery mainly tracks two classes of embedded
fields: unconditional pointers and everything else. Reusing the current
`isptr` bit for tagged slots would force too many ambiguous special cases into
the runtime.

For the prototype, prefer extending `jl_datatype_layout_t` with explicit tagged
slot metadata instead:

- keep `npointers` / `first_ptr` / pointer offset tables for unconditional
  references;
- add `ntaggedptrs` and a parallel table of tagged-slot offsets;
- treat `ImmediateOrRef` fields as 8-byte inline fields in the ordinary field
  descriptors, but record their offsets again in the tagged-slot table.

This keeps existing unconditional-pointer fast paths mostly intact while
giving the GC, serializer, and code generator a precise way to find tagged
slots.

### First runtime touchpoints

Given the strategy above, the first implementation steps should be:

1. Add `ImmediateOrRef` to Core/Base as a built-in primitive type declaration in
   `base/boot.jl`.
2. Teach datatype layout construction in `src/datatype.c` to recognize fields
   of type `ImmediateOrRef{T}` and populate the new tagged-slot metadata.
3. Extend `jl_datatype_layout_t` and its helpers in `src/julia.h` so the
   runtime can iterate tagged slots separately from unconditional pointers.
4. Add one helper used by GC, serialization, and codegen to classify a stored
   word as unassigned, reference, or immediate payload.

## Proposed implementation stages

### 1. Freeze the semantic model

Before touching the runtime, decide the surface contract:

- Use `ImmediateOrRef{T}` as the prototype and implementation name.
- Implement it as a dedicated type first, rather than as field syntax or as a
  general `Union` storage optimization.
- Specify the invariants:
  - the prototype only supports 64-bit runtimes;
  - the representation is always 64 bits wide;
  - object references stored in `ImmediateOrRef` have at least three low zero bits
    on supported 64-bit Julia runtimes;
  - whether v1 guarantees at least 3 tag bits, as suggested in the thread;
  - v1 only permits payload types that are represented as ordinary boxed objects,
    not inline/bits-like payload types such as `Int`;
  - mutability is not the criterion: immutable non-inline object types remain valid.
- Specify the minimal API: construction from a Julia value, `isassigned`,
  `isimmediate`, `[]`, raw load/store of the encoded word,
  conversion rules, and
  pointer-conversion behavior for `ccall`.

This stage should end with a short design section in this document being
updated into a normative description.

### 2. Extend object-layout metadata

Current datatype layout metadata only distinguishes between "pointer" and
"not a pointer". That is not enough for a field whose contents are scanned like
a pointer only when a tag test succeeds.

Expected touchpoints:

- `src/julia.h`
- `src/datatype.c`
- `Compiler/src/Compiler.jl`
- Base reflection helpers that expose `DataTypeFieldDesc`

For the prototype, prefer explicit tagged-slot metadata in
`jl_datatype_layout_t` instead of overloading the existing `isptr` bit in field
descriptors. Concretely, keep the current unconditional pointer metadata and
add a separate tagged-slot count plus offset table.

### 3. Teach the stock GC how to scan tagged-reference slots

Once layout metadata can identify tagged-reference fields, the collectors need
to treat them as conditionally traced edges.

Expected touchpoints:

- `src/gc-stock.c`
- `src/gc-wb-stock.h`

Required work:

- Add a helper for "load slot, test tag bits, recover `jl_value_t*` if this is
  a real reference".
- Update mark-stack traversal for objects and array-like storage if tagged
  slots are allowed there.
- Update remembered-set/write-barrier logic so that storing an immediate value
  does not enqueue a fake edge, while storing a young object still preserves
  generational invariants.
- Audit debug assertions and heap verifiers that currently assume every pointer
  slot is an untagged `jl_value_t*`.

MMTK support is intentionally deferred for this prototype.

### 4. Update serialization and system-image support

The issue explicitly calls out serialization. Any object that contains a tagged
reference field must serialize the heap reference case as a reference and the
immediate case as raw bits.

Expected touchpoints:

- `src/staticdata.c`
- Base-level serialization tests

Required work:

- Queue referenced objects only when the slot currently stores a real Julia
  object.
- Preserve the raw encoded word for immediate cases.
- Ensure relocation logic never tries to relocate an immediate payload as if it
  were a pointer.
- Add round-trip tests for ordinary serialization and system-image generation.

### 5. Add codegen and inference support

Even with correct GC behavior, the feature is not useful unless loads, stores,
and roots are modeled correctly by the compiler.

Expected touchpoints:

- `src/codegen.cpp`
- `Compiler/src/abstractinterpretation.jl`
- any builtins/intrinsics used to access the raw representation

Required work:

- Prevent "pointerfree" and "all pointers" fast paths from misclassifying
  tagged-reference layouts.
- Ensure inline-root extraction accounts for tagged-reference fields.
- Decide whether `isimmediate`, raw-load, and raw-store operations are builtins,
  intrinsics, or ordinary functions lowered to builtins.
- Make field load/store lowering emit the correct tag test, write barrier, and
  type information.

This is also the right place to decide how much the optimizer should know about
the new primitive. A minimal first version can treat it conservatively.

### 6. Add the Base/Core surface API

After the runtime path exists, wire up the user-visible type and operations.

Expected touchpoints:

- `base/boot.jl`
- `base/refpointer.jl`

Candidate API surface for v1:

- `ImmediateOrRef{T}`
- constructor from `T`
- zero-argument constructor for an unassigned slot
- `isassigned`
- `getindex`
- `isimmediate`
- `getrawvalue`
- `setrawvalue`

Deferred for later:

- nested `RefValue{ImmediateOrRef{T}}` mutation support

The v1 API should stay intentionally small. Higher-level helpers for small
integers, short strings, or compact rationals belong in follow-up work.

### 7. Validation and bring-up

The feature touches the object model, so validation needs to be broader than a
normal Base API addition.

Test areas:

- runtime layout/reflection tests;
- GC tests that exercise young/old edges, immediate writes, and stress
  collection;
- serialization and system-image round trips;
- compiler/codegen tests for field access and rooting;
- one end-to-end proof-of-concept type, likely a small `FastBigInt`-style
  experiment kept in tests rather than in Base.

## Recommended order of execution

1. Finalize the semantic model and write down the invariants.
2. Extend layout metadata so the runtime can distinguish tagged-reference
   fields.
3. Make GC scanning and barriers correct.
4. Make serialization and system-image handling correct.
5. Expose compiler hooks and the public API.
6. Add a proof-of-concept consumer and benchmark it.

This order keeps correctness work ahead of API expansion and avoids building a
surface feature on top of ad hoc runtime checks.

## Open design questions

- Bit budget: whether the runtime should guarantee exactly 3 low zero bits for
  stored references, or merely rely on current alignment and document it.
- Exact constructor contract for non-reference-like values of type `T`,
  especially boxed immutables.
- Relation to existing `Union` optimizations: separate feature, or eventual
  storage backend for some union fields.

## Follow-up work after the primitive lands

- Prototype a `FastBigInt`-style package or internal benchmark.
- Explore short-string support.
- Revisit compact `Expr` source-location storage.
- Evaluate whether some `Union{bits, ref}` layouts should eventually lower to
  the same runtime representation.
