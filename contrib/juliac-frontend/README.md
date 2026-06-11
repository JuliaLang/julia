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

## The standalone (two-runtimes-in-one-process) library

`make standalone` produces `build/libjulia-frontend-standalone.so`: the
frontend image *plus a complete private copy of the Julia runtime*
(libjulia-internal's objects, libsupport, libuv, libutf8proc, libunwind,
codegen and frontend stubs) linked into one shared library with every symbol
internalized except the 13-function frontend ABI. This is the form intended
to be loadable in-process as a host julia's `libjulia-frontend`, including
by a host of a *different version*:

- The host and guest runtimes share nothing: distinct TLS, GC, heaps and
  type objects. The entry points "switch TLS" by initializing/adopting the
  calling thread into the private runtime (`fe_enter`), leaving host TLS
  untouched.
- Values are converted, not shared, at the boundary (fe2host in
  fe_standalone_entry.c) — the role scm_to_julia played for flisp. Host
  values are only touched through a small dlsym-resolved host C API table
  (`jl_symbol`, `jl_exprn`, `jl_box_*`, ... — the cross-version contract),
  with host-side GC rooting done through manually-built PUSHARGS gcframes.
- The private runtime initializes from the statically-linked image
  (`jl_init_` with a directly-constructed `jl_image_buf_t`; `null_sysimage.o`
  is excluded so the image's `jl_image_unpack` definition binds), with
  signal handling off and a single thread. `jl_export_cli_globals = 0`
  prevents it from exporting its globals to a loader library it does not
  have; the CLI data-pointer slots its image code references are provided
  and filled locally (fe_standalone_globals.c).
- The codegen/frontend trampoline names that libjulia-internal expects from
  the CLI loader are bound at link time to the in-library `*_fallback`
  stubs (`--defsym`); the guest needs no LLVM and contains no flisp.

`make check-standalone` runs `host_driver`, which initializes a normal host
julia, dlopens the standalone library, and exercises parse and the operator
queries through the ABI with host values, comparing against the host's own
frontend.

The library drives a full julia *bootstrap*: with `libjulia-frontend.so`
replaced by the standalone library, `julia --output-ji basecompiler.ji
Base_compiler.jl` cold-boots from `boot.jl` and builds the stage-1 image
(boot, Base_compiler, the whole Compiler), and `julia --sysimage
basecompiler.ji --output-ji sysbase.ji sysimg.jl` builds the full Base +
stdlib stage-2 image on top, with every parse and lower served by the
embedded JuliaSyntax/JuliaLowering (flisp is never invoked); the resulting
image boots and runs julia code. Bootstrap-specific constraints this
required:

- the `Core` runtime-support helpers (`interpolate_ast`,
  `eval_closure_type`, ...) are defined at the top of `boot.jl`, before the
  first quoted syntax / closure / catch-variable in the file, restricted to
  early-bootstrap vocabulary (and the `Typeof` bootstrap definition is
  built with explicit `_expr` calls rather than quoted syntax, since
  defining `interpolate_ast`'s own methods requires `Typeof`);
- the guest's `(top X)` references resolve through the host's
  `jl_base_relative_to(inmodule)`, which falls back to `Core` before `Base`
  exists — and host globals like `jl_base_module` must always be read
  through live functions, never through the CLI loader's data-pointer
  slots, which are snapshots taken before bootstrap populates them;
- `export`/`public` pass through lowering unchanged (the runtime's toplevel
  machinery evaluates them; boot.jl's own `export` runs before `Core.eval`
  exists), and `import`/`using` lower to `(top _eval_import)`/`(top
  _eval_using)` exactly as flisp emits;
- the image build parses and lowers the whole `base/` and `Compiler/`
  source corpus to AOT-compile the full breadth of frontend code paths
  (the guest has no JIT; an interpreted path that reaches a `ccall` fails);
- the C ABI parses with the frontend's own current syntax version and
  strips per-module syntax-version markers (no version parameter exists in
  the ABI, and a `VersionNumber` cannot cross before the host has `Base`);
- closure type names are salted with a host-unique token (derived from the
  host world counter): name uniquification scans shadow modules, which
  start empty in every process, so without it separate sessions whose
  output is serialized into one image chain would collide;
- macro expansions with `toplevel` heads are deferred with each chunk
  wrapped in its own hygienic-scope, so the runtime can re-lower chunks
  one at a time with hygiene intact.

Status: parse, the operator queries, *lowering*, and *macros* work
cross-runtime (`make check-standalone`): host expressions are lowered by the
guest's JuliaLowering, with host modules mirrored by shadow modules, lowered
`CodeInfo` reconstructed host-side field-by-name, and arbitrary host values
carried opaquely. Macros are expanded *before* the boundary by a host-side
port of flisp's `jl_expand_macros` (h_expand_macros in
fe_standalone_entry.c): macro functions are invoked in the host runtime via
`jl_invoke_julia_macro` on host trees, so neither macro arguments nor
expansions ever cross runtimes, and hygiene (`esc`) follows the same
hygienic-scope protocol flisp uses, which JuliaLowering's compat layer
consumes. A julia whose `libjulia-frontend.so` is replaced by the standalone
library boots and evaluates code normally — including macro and method
definitions, closures, generators and quoted syntax — with all lowering
served by the guest runtime.

Constructs whose lowering used to embed run-time objects in the output
(which cannot cross runtimes, and could not be restored from an image) are
now emitted as calls to named `Core` runtime-support functions when the
host runtime provides them: `Core.eval_closure_type` and
`Core.replace_captured_locals!` (boot.jl) for closures, and
`Core.interpolate_ast(Expr, ...)` (defined in early bootstrap) for quoted
syntax with `$` interpolation. On older runtimes without these names,
JuliaLowering falls back to embedding values as before
(`_core_has_lowering_support` in JuliaLowering/src/runtime.jl).

`@generated` functions work in both directions: definitions lowered in
expr-compat mode emit the runtime-recognized `Core.GeneratedFunctionStub`
protocol (rather than JuliaLowering's own stub type, which could not cross),
and when a generator fires, `Base.generated_body_to_codeinfo`'s
`Expr(:lambda/:scope-block/:with-static-parameters ...)` forms are
understood by JuliaLowering's Expr compatibility layer.

Remaining cross-runtime gaps:
- lowering warnings (the `warn` flag) are not forwarded to the host;
- `jl_macroexpand`'s non-recursive flavor leaves hygienic-scope wrappers
  visible (the `expand_scope` post-pass is not implemented);
- JuliaLowering's SyntaxTree-flavored quote interpolation
  (macro_expansion.jl) still embeds its `interpolate_ast` function value;
  that path is only reachable for macros defined *by* the guest world, not
  through the C ABI.
