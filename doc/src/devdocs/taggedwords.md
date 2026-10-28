# Tagged words

On 64-bit platforms, a field or `Memory` element whose type is a `Union` mixing
*reference* members with a few small *primitive* members is stored as a single
64-bit *tagged word*, the representation OCaml uses for `int`. Storing or
loading a small primitive value then involves no allocation and no pointer
chasing; reference members are stored as plain object pointers.

This layout is a documented ABI: C code interoperating with Julia data may rely
on the encoding described here.

## Which unions qualify

The members of a union are the types it is made of (`Base.uniontypes(U)`).
They fall into two kinds:

* *immediate members*: primitive types of at most 63 bits
  (`Core.bitsizeof(T) <= 63`), such as `Int32`, `Char` or `Float32`;
* *reference members*: all other members, stored as a pointer to an object.
  These include mutable and abstract types, singletons such as `Nothing`, and
  immutable structs, which are boxed.

A union `U` is stored as a tagged word exactly when all of the following hold
(checked by `jl_is_tagged_uniontype`):

1. `U` has at least one immediate member, and at least one member that is not
   `isbits`, hence a reference member. A union of `isbits` types only, such as
   `Union{Nothing, Int32}`, keeps the selector-byte layout described in
   [isbits Union Optimizations](isbitsunionarrays.md).
2. `U` has at most 4 immediate members. This limit is a choice: a wider tag
   would admit more, at the cost of payload bits, without changing the
   encoding of unions that qualify today.
3. Every immediate member fits beside the tag: `Core.bitsizeof(T) <= 64 - k`,
   with `k` from the table below.

On 32-bit platforms no union is stored as a tagged word.

`@atomic` fields and `AtomicMemory` elements use the same layout; a slot is one
aligned word, so plain word atomics apply.

## The encoding

One 8-byte, pointer-aligned word per slot. A reference has both low bits
clear and every tag is odd, so the low two bits answer pointer-vs-immediate
without knowing the union type:

| immediates | k (tag bits) | tags       | payload bits |
|:----------:|:------------:|:-----------|:------------:|
| 1          | 1            | 1          | 63           |
| 2          | 2            | 1, 3       | 62           |
| 3–4        | 3            | 1, 3, 5, 7 | 61           |

* `word == 0` — the slot is `#undef` (fields and fresh `Memory` elements start
  undefined, as boxed fields do).
* `word != 0` and low two bits clear — an untagged `jl_value_t*` to a reference
  member. Heap objects are at least 8-byte aligned, so both low bits of a
  reference are clear; the member is identified by the pointee's type tag.
* bit 0 set — an immediate: the `i`-th immediate member (0-based, in union
  component order, i.e. `jl_nth_union_component` order restricted to the
  immediate members) has tag `2i + 1`, and

      word == (zext(value, 64) << k) | (2i + 1)

  with the bits above `k + bitsizeof` zero.
* low bits `10` — reserved for future immediate encodings; no union
  produces such a word, and it is never traced.

Values of immediate member types are **always** stored immediate — stores
canonicalize, so word equality is meaningful for immediates (`===` of two
slots compares words first and only recurses into `jl_egal` when both are
references).

The garbage collector and the serializer only ever need the universal
predicate: *a word is traced/relocated iff it is nonzero with both low bits
clear* (`jl_tagged_word_isptr`). The GC applies the same rule to values rooted
with `JL_GC_PUSH*`, so C code may root a raw tagged word as is.

## Layout metadata

A tagged word is an entry of the layout's pointer table, like any other
reference slot: `npointers` counts it, `first_ptr` may name it, and code that
only asks whether a type holds references needs no change. Code that
dereferences pointer-table entries skips any word that fails
`jl_tagged_word_isptr`; for a plain pointer that test is the usual NULL check.
`ntaggedwords` counts how many of the `npointers` entries are tagged words, so
`ntaggedwords > 0` tells codegen that the type needs runtime handling.

The fielddesc entry of a field stored as a tagged word reports `isptr = 0` and
size 8; there is no selector byte, and `jl_field_istagged` recognizes the field
by its word appearing in the pointer table. `Memory{U}` uses the element kind
`arrayelem_istagged` (reported as `3` by `Base.datatype_arrayelem`), with
element size 8, one pointer per element and no trailing selector bytes;
memoryrefs are ordinary pointer-based refs.

From Julia, `Base.istaggedunion(U)` reports whether `U` gets this layout, and
`Base.datatype_ntaggedwords(T)` counts the tagged words among
`Base.datatype_npointers(T)`.

Since tagged words are pointers in the layout, structs holding them are
constrained exactly like structs holding references: they are never
`pointerfree` or `isbits`, and an inline field of such a type is `#undef` when
its first pointer is the zero word.

## Notes

* Data races on non-atomic tagged words are undefined behavior, as for any
  non-atomic Julia field; the two halves of a decoded word (kind and payload)
  come from one atomic word load in the runtime.
* Under conservative stack scanning an immediate is never mistaken for a
  pointer (it is odd, hence misaligned).
* The encoding assumes little-endian payload extraction, like the rest of the
  runtime.
* `unsafe_wrap` refuses element types holding tagged words: a foreign buffer
  cannot hold GC-managed references.
