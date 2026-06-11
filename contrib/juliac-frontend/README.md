# A JuliaSyntax/JuliaLowering implementation of libjulia-frontend

This directory builds `libjulia-frontend-jl`, a shared library that
implements Julia's frontend interface — the same C ABI as the flisp-based
`libjulia-frontend` — using the pure-Julia frontend (JuliaSyntax for parsing,
JuliaLowering for macro expansion and lowering), compiled ahead of time with
juliac. It contains no flisp.

## What it provides

The library exports the `*_impl` symbols that the runtime's frontend
trampolines bind (see `JL_FRONTEND_EXPORTED_FUNCS` in
`src/jl_exported_funcs.inc`):

| symbol | implementation |
|---|---|
| `jl_frontend_init_impl` | no-op |
| `jl_frontend_parse_impl` | `JuliaSyntax.core_parser_hook` |
| `jl_frontend_lower_impl` | `JuliaLowering.core_lowering_hook` |
| `jl_macroexpand_impl` | `JuliaLowering.macroexpand` (recursive only) |
| `jl_is_operator_impl` etc. (5) | flisp-compatible classification over the JuliaSyntax tokenizer (`JuliaFrontend/src/flisp_ops.jl`), bit-identical to flisp by test |
| `jl_lisp_prompt_impl`, `fl_*_profile_impl` | error (no flisp here) |

The Julia-visible flisp entry points `Base.fl_parse`/`Base.fl_lower` are
overridden inside the image to route to JuliaSyntax/JuliaLowering, so no code
path in this library reaches flisp, even as a fallback.

## Building and testing

```
make            # builds build/libjulia-frontend-jl.so and build/frontend_driver
make check      # ABI smoke test + parity tests against the flisp frontend
```

`frontend_driver` loads the library as its process's Julia image
(`jl_init_with_image_handle`) and calls the `*_impl` entry points through
exactly the C ABI the runtime trampolines use. `make check` validates:

- the operator query functions against flisp over all unicode codepoints
  plus dotted/suffixed/compound-assignment combinations (`test_ops_parity.jl`,
  run against the in-tree julia whose frontend library is flisp);
- parse results through the ABI against `Base.fl_parse` on snippets and on
  real `base/` sources;
- lowering and macro expansion through the ABI by evaluating the lowered
  code (JuliaLowering's IR is intentionally not textually identical to
  flisp's, so these checks are behavioral).

## Status, and the path to bootstrapping without flisp

This library is ABI-compatible with the loader's frontend trampoline
interface, but it cannot yet be dropped in as `libjulia-frontend.so` of a
running julia process: a juliac-built library embeds its own system image,
and the runtime currently supports only one image per process. Loading it
into a process that already has a sysimage (or that is bootstrapping a fresh
`Core` from `boot.jl`) would require either:

1. **Second-image support in the runtime** — restoring the frontend image as
   an auxiliary world that shares the C-defined types (`Expr`, `Symbol`,
   `LineNumberNode`, modules, …) with the host world. The frontend boundary
   was designed so that only such C-owned types cross it, and macro
   invocation already calls back into the host runtime via
   `jl_invoke_julia_macro`, but image restore itself is single-image today.

2. **Out-of-process use during bootstrap** — running this library in its own
   process (as `frontend_driver` does) and feeding parse/lowering results to
   the stage-1 bootstrap, analogous to how the flisp *host* executable is
   used at build time today. This requires a serialization format for
   lowered code in which module references are symbolic rather than
   world-identities.

Until one of those lands, this directory serves as: the definition of the
replacement frontend's ABI surface, a continuously testable parity harness
against flisp, and the build recipe for the eventual swap.
