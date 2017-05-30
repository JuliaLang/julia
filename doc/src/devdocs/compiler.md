# High-level Overview of the Native-Code Generation Process

## Representation of Pointers

When emitting code to an object file, pointers will be emitted as relocations.
The deserialization code will ensure any object that pointed to one of these constants
gets recreated and contains the right runtime pointer.

Otherwise, they will be emitted as literal constants.

To emit one of these objects, call `literal_pointer_val`.
It'll handle tracking the Julia value and the LLVM global,
ensuring they are valid both for the current runtime and after deserialization.

When emitted into the object file, these globals are stored as references
in a large `gvals` table. This allows the deserializer to reference them by index,
and implement a custom manual mechanism similar to a Global Offset Table (GOT) to restore them.

Function pointers are handled similarly.
They are stored as values in a large `fvals` table.
Like globals, this allows the deserializer to reference them by index.

Note that `extern` functions are handled separately,
with names, via the usual symbol resolution mechanism in the linker.

Note too that `ccall` functions are also handled separately,
via a manual GOT and Procedure Linkage Table (PLT).


## Representation of Intermediate Values

Values are passed around in a `jl_cgval_t` struct.
This represents an R-value, and includes enough information to
determine how to assign or pass it somewhere.

They are created via one of the helper constructors, usually:
`mark_julia_type` (for immediate values) and `mark_julia_slot` (for pointers to values).

The function `convert_julia_type` can transform between any two types.
It returns an R-value with `cgval.typ` set to `typ`.
It'll cast the object to the requested representation,
making heap boxes, allocating stack copies, and computing tagged unions as
needed to change the representation.

By contrast `update_julia_type` will change `cgval.typ` to `typ`,
only if it can be done at zero-cost (i.e. without emitting any code).


## Union representation

Inferred union types may be stack allocated via a tagged type representation.

The primitive routines that need to be able to handle tagged unions are:
- mark-type
- load-local
- store-local
- isa
- is
- emit_typeof
- emit_sizeof
- boxed
- unbox
- specialized cc-ret

Everything else should be possible to handle in inference by using these
primitives to implement union-splitting.

The representation of the tagged-union is as a pair
of `< void* union, byte selector >`.
The selector is fixed-size as `byte & 0x7f`,
and will union-tag the first 126 isbits.
It records the one-based depth-first count into the type-union of the
isbits objects inside. An index of zero indicates that the `union*` is
actually a tagged heap-allocated `jl_value_t*`,
and needs to be treated as normal for a boxed object rather than as a
tagged union.

The high bit of the selector (`byte & 0x80`) can be tested to determine if the
`void*` is actually a heap-allocated (`jl_value_t*`) box,
thus avoiding the cost of re-allocating a box,
while maintaining the ability to efficiently handle union-splitting based on the low bits.

It is guaranteed that `byte & 0x7f` is an exact test for the type,
if the value can be represented by a tag – it will never be marked `byte = 0x80`.
It is not necessary to also test the type-tag when testing `isa`.

The `union*` memory region may be allocated at *any* size.
The only constraint is that it is big enough to contain the data
currently specified by `selector`.
It might not be big enough to contain the union of all types that
could be stored there according to the associated Union type field.
Use appropriate care when copying.


## Specialized Calling Convention Signature Representation

A `jl_returninfo_t` object describes the calling convention details of any callable.

If any of the arguments or return type of a method can be represented unboxed,
and the method is not varargs, it'll be given an optimized calling convention
signature based on its `specTypes` and `rettype` fields.

The general principles are that:

- Primitive types get passed in int/float registers.
- Tuples of VecElement types get passed in vector registers.
- Structs get passed on the stack.
- Return values are handle similarly to arguments,
  with a size-cutoff at which they will instead be returned via a hidden sret argument.

The total logic for this is implemented by `get_specsig_function` and `deserves_sret`.

Additionally, if the return type is a union, it may be returned as a pair of values (a pointer and a tag).
If the union values can be stack-allocated, then sufficient space to store them will also be passed as a hidden first argument.
It is up to the callee whether the returned pointer will point to this space, a boxed object, or even other constant memory.
