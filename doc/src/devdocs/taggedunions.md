# Tagged union layout

On 64-bit platforms, a field or `Memory` element whose type is a `Union` mixing
*reference* members with a few small *primitive* members is stored as a single
64-bit word — a "tagged pointer" in the LISP/GAP/FLINT tradition. Storing or
loading a small primitive value then involves no allocation and no pointer
chasing; reference members are stored as plain object pointers.

This layout is a documented ABI: C code interoperating with Julia data may rely
on the encoding described here.

## Which unions qualify

A union `U` gets the tagged layout exactly when all of the following hold
(checked by `jl_uniontype_istagged`; all-or-nothing — when any check fails, the
union falls back to its previous layout):

1. The union is **not** storable inline: at least one member is a
   reference-only type. (An all-`isbits` union — including ones with ghost
   singletons such as `Nothing` — keeps the selector-byte layout described in
   [isbits Union Optimizations](isbitsunionarrays.md).)
2. The *immediate candidates* — members that are primitive types with
   `Core.bitsizeof(T) <= 63` — number between 1 and 4. All other members
   ("reference members": mutable types, non-inline immutables, abstract types,
   singletons) are stored by reference and are unconstrained.
3. With `k` tag bits (see below), every immediate candidate satisfies
   `Core.bitsizeof(T) <= 64 - k`.

On 32-bit platforms the classifier always fails (everything keeps the old
layout).

`@atomic` fields and `AtomicMemory` elements use the same layout; a slot is one
aligned word, so plain word atomics apply.

## The encoding

One 8-byte, pointer-aligned word per slot. Tags are odd, so bit 0 alone
answers pointer-vs-immediate without knowing the union type:

| immediates | k (tag bits) | tags       | payload bits |
|:----------:|:------------:|:-----------|:------------:|
| 1          | 1            | 1          | 63           |
| 2          | 2            | 1, 3       | 62           |
| 3–4        | 3            | 1, 3, 5, 7 | 61           |

* `word == 0` — the slot is `#undef` (fields and fresh `Memory` elements start
  undefined, exactly like boxed union slots did).
* `word != 0` and bit 0 clear — an untagged `jl_value_t*` to a reference
  member. Heap objects are at least 8-byte aligned, so a pointer word always
  has its low three bits clear; the member is identified by the pointee's type
  tag.
* bit 0 set — an immediate: the `i`-th immediate member (0-based, in union
  component order, i.e. `jl_nth_union_component` order restricted to the
  immediate candidates) has tag `2i + 1`, and

      word == (zext(value, 64) << k) | (2i + 1)

  with the bits above `k + bitsizeof` zero.

Values of immediate member types are **always** stored immediate — stores
canonicalize, so word equality is meaningful for immediates (`===` of two
slots compares words first and only recurses into `jl_egal` when both are
references).

The garbage collector and the serializer only ever need the universal
predicate: *a word is traced/relocated iff it is nonzero with bit 0 clear*
(`jl_tagged_word_isptr`).

## Layout metadata

`jl_datatype_layout_t` carries a third trailing table (after the fielddesc
table and the pointer-offset table) listing the word offsets of the tagged
slots of the type, with the count in `ntaggedptrs`; it is composed through
nested inline structs exactly like the pointer table, and is enumerated with
`jl_tagged_offset`. The fielddesc entry of a tagged union field reports
`isptr = 0` and size 8; there is no selector byte. `Memory{U}` uses the
element kind `arrayelem_istagged` (reported as `3` by
`Base.datatype_arrayelem`), with element size 8 and no trailing selector
bytes; memoryrefs are ordinary pointer-based refs.

From Julia, `Base.istaggedunion(U)` reports whether `U` gets this layout, and
`Base.datatype_ntaggedptrs(T)` counts the tagged words embedded in `T`.

Structs holding tagged words are constrained exactly like structs holding
pointers: they are never `pointerfree`, never `isbits`, and cannot be inlined
into contexts that require pointer-free data (such as isbits-union members).

## Notes

* Data races on non-atomic tagged slots are undefined behavior, as for any
  non-atomic Julia field; the two halves of a decoded word (kind and payload)
  come from one atomic word load in the runtime.
* Under conservative stack scanning an immediate word is never mistaken for a
  pointer (it is odd, hence misaligned).
* The encoding assumes little-endian payload extraction, like the rest of the
  runtime.
* `unsafe_wrap` refuses tagged element layouts: a foreign buffer cannot hold
  GC-managed references.
