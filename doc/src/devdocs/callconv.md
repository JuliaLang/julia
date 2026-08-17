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

A small return value is returned as LLVM return values. A large return value is returned via
the "structure return" (`sret`) convention, where the caller provides a pointer to a return slot.

An argument or return value that is a homogeneous tuple is sometimes represented as an LLVM vector
instead of an LLVM array.

### Introspecting the native calling convention

Out-of-tree code generators (GPUCompiler.jl, Enzyme.jl) need to know exactly how a
signature is lowered, and re-deriving these rules is a reliable source of drift.
`jl_get_specsig_layout` in `julia.h` answers the question directly, by running the
same code that emits the signature:

```c
jl_abi_query_t query = { .version = JL_ABI_LAYOUT_VERSION, .ci = codeinst };
jl_abi_layout_t layout;
jl_abi_arginfo_t args[nargs];
jl_get_specsig_layout(&query, &layout, args, nargs);
```

A query names either a `jl_code_instance_t` (whose ABI signature comes from
`jl_get_ci_abi`, which follows a `Core.ABIOverride`) or an explicit
`(sigt, rt)` pair. The answer depends on the `jl_cgparams_t` — `gcstack_arg`
adds or removes a parameter, `prefer_specsig` decides whether the specialized
signature is used at all — and on the target, since the alloca address space
comes from the module's data layout. Pass both if they differ from the host's.

`layout` reports how the value is returned (`jl_abi_retcc_t`, mirroring
`jl_returninfo_t::CallingConv`), and the indices of the leading parameters,
which appear in this order when present:

  1. `sret_return` or `union_bytes_return`, the caller-provided return slot
  2. `return_roots`, an array of the tracked pointers in that slot
  3. `pgcstack_arg`

`args[i]` then describes `jl_tparam(sigt, i)`: whether it is passed by value,
indirectly as a pointer, boxed, or not at all (a ghost type, or a value that is
exactly its own type — see `jl_is_typeegal`), along with its 0-based LLVM
parameter index. An aggregate passed indirectly that contains *some* but not all
tracked pointers additionally takes a `.roots.` shadow parameter, reported as
`roots_idx`; forgetting it shifts every later parameter.

The type-level rules that decide boxing are separately available from libjulia,
without an LLVM context: `jl_deserves_stack`, `jl_deserves_argbox` and
`jl_deserves_retbox`.

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
