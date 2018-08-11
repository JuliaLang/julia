# Calling Conventions

Julia uses three calling conventions for four distinct purposes:

| Name    | Prefix    | Purpose                          |
|:------- |:--------- |:-------------------------------- |
| Native  | `julia_`  | Speed via specialized signatures |
| JL Call | `jlcall_` | Wrapper for generic calls        |
| JL Call | `jl_`     | Builtins                         |
| C ABI   | `jlcapi_` | Wrapper callable from C          |

## Julia Native Calling Convention

The native calling convention is designed for fast non-generic calls. It usually uses a specialized
signature.

  * LLVM ghosts (zero-length types) are omitted.
  * LLVM scalars and vectors are passed by value.
  * LLVM aggregates (arrays and structs) are passed by reference.

A small return values is returned as LLVM return values. A large return values is returned via
the "structure return" (`sret`) convention, where the caller provides a pointer to a return slot.

An argument or return values that is a homogeneous tuple is sometimes represented as an LLVM vector
instead of an LLVM array.

## JL Call Convention

The JL Call convention is for builtins and generic dispatch. Hand-written functions using this
convention are declared via the macro `JL_CALLABLE`. The convention uses exactly 3 parameters:

  * `F`  - Julia representation of function that is being applied
  * `args` - pointer to array of pointers to boxes
  * `nargs` - length of the array

The return value is a pointer to a box.

## C ABI

C ABI wrappers enable calling Julia from C. The wrapper calls a function using the native calling
convention.

Tuples are always represented as C arrays.
