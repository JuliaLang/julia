# Tagged word Union Optimizations

A field or `Memory` element whose type is a `Union` mixing *reference*
components with a few small *primitive* components is stored as a single
pointer-sized *tagged word*: a value of a reference component as a plain object
pointer, a value of a primitive component as the value itself plus a few tag
bits. Storing or loading a small primitive value then involves no allocation
and no pointer chasing. This complements the [isbits Union
Optimizations](isbitsunionarrays.md), which cover unions of `isbits` types
only.

This technique has been in use for decades in other language implementations,
among them Lisp dialects, Smalltalk, OCaml and Ruby.

The encoding below is a documented ABI. C code that reads and writes fields
with `jl_get_nth_field` and `jl_set_nth_field`, or reads `Memory` elements with
`jl_genericmemoryref`, never sees a raw tagged word, so only code that accesses
the words directly relies on the encoding. The boxed-element accessors
`jl_genericmemory_ptr_ref`, `jl_array_ptr_ref` and their `_set` counterparts
assert `arrayelem_isboxed`, so they do not apply to such `Memory`, as they do
not apply to isbits union `Memory` either.

## Which unions qualify

The components of a union type `U` are the types it is made of
(`Base.uniontypes(U)`, or `jl_nth_union_component` in C). With `W` the word
size in bits (`Sys.WORD_SIZE`), each component `T` is of one of two kinds:

* an *immediate component*, if `T` is a primitive type of at most `W - 1` bits
  (`Core.bitsizeof(T) < W`), such as `Int16` or `Bool` on all platforms, or
  `Int32` or `Float32` on 64-bit platforms;
* a *reference component* otherwise: mutable and abstract types, singletons
  such as `Nothing`, immutable structs, and wider primitive types such as
  `Int`.

A union `U` is stored as a tagged word exactly when all of the following hold
(checked by `jl_is_tagged_uniontype`):

1. `U` has at least one immediate component, and at least one component that
   is not `isbits`, hence a reference component. A union of `isbits` types
   only, such as `Union{Nothing, Int32}`, keeps the selector-byte layout
   described in [isbits Union Optimizations](isbitsunionarrays.md).
2. `U` has at most 4 immediate components. This limit is an implementation
   choice.
3. Every immediate component `T` satisfies `Core.bitsizeof(T) <= W - k`, with
   `k` the number of tag bits, given in the next section.

`@atomic` fields and `AtomicMemory` elements use the same layout; a tagged word
is one aligned word, so plain word atomics apply.

## The encoding

Each field or element stored as a tagged word occupies one pointer-sized,
pointer-aligned word. The word holds either a *reference*, a pointer to the
object, or an *immediate*, the value itself. The low two bits tell them apart
without knowing the union type: a reference has both low bits clear, and every
tag is odd.

| immediate components | k (tag bits) | tags       | payload bits |
|:--------------------:|:------------:|:-----------|:------------:|
| 1                    | 1            | 1          | W - 1        |
| 2                    | 2            | 1, 3       | W - 2        |
| 3–4                  | 3            | 1, 3, 5, 7 | W - 3        |

* `word == 0` — `#undef` (fields and fresh `Memory` elements start undefined,
  as boxed fields do).
* `word != 0` and low two bits clear — a reference, an untagged `jl_value_t*`.
  Objects are at least 4-byte aligned, so both low bits of a reference are
  clear; the component is identified by the pointee's type tag.
* bit 0 set — an immediate: the `i`-th immediate component (0-based, in
  `jl_nth_union_component` order) has tag `2i + 1`, and

      word == (zext(value, W) << k) | (2i + 1)

  with the bits above `k + bitsizeof` zero.
* low bits `10` — reserved for future immediate encodings; no union
  produces such a word, and it is never traced.

A value whose type is an immediate component is **always** stored as an
immediate. Stores canonicalize, so word equality is meaningful for immediates
(`===` of two tagged words compares the words first and only recurses into
`jl_egal` when both are references).

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
`ntaggedwords > 0` tells codegen that the type needs special handling.

The fielddesc entry of a field stored as a tagged word reports `isptr = 0` and
the size of a pointer; there is no selector byte, and `jl_field_istagged`
recognizes the field by its word appearing in the pointer table. `Memory{U}`
uses the element kind `arrayelem_istagged` (reported as `3` by
`Base.datatype_arrayelem`), with pointer-sized elements, one pointer per
element and no trailing selector bytes; memoryrefs are ordinary pointer-based
refs.

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
