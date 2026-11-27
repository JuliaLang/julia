# libjulia-frontend: the frontend that bootstraps julia

Julia's parser and lowering are written in Julia (JuliaSyntax and
JuliaLowering, in this repository). Once a system image with `Base` exists,
they parse and lower all code from inside it (`Core._parse` and
`Core._lower`; see `JuliaSyntax.enable_in_core!` and
`JuliaLowering.activate!`). Building that first image needs a frontend that
runs before any of this julia's own Julia code does. That frontend is
`libjulia-frontend`, which this directory builds: the same JuliaSyntax and
JuliaLowering, compiled ahead of time by a published julia release and
linked, together with that release's own runtime, into a self-contained
library.

## How it is built

`deps/julia-bootstrap.mk` downloads the release pinned in
`deps/julia-bootstrap.version` (or uses the installation in
`JULIA_BOOTSTRAP_HOME`). `src/Makefile` then:

1. builds an image with that release (`build_image.jl`, a minimal version of
   JuliaC's build script, running `image.jl`), from this tree's JuliaSyntax and
   JuliaLowering and the `JuliaFrontend` package here, which implements the
   frontend interface on top of them. The image is compiled ahead of time,
   since the library has no JIT: `image.jl` exercises the frontend on all of
   `base/` and `Compiler/` to compile the code paths that bootstrapping takes;
2. compiles `entry.c` against the release's headers;
3. links the two with the release's static runtime (`libjulia-internal.a`)
   into `usr/lib/libjulia-frontend.so`.

The library is only needed to build system images from `boot.jl`, and is not
installed.

## How it works

The runtime reaches its frontend through the trampolines of the
`JL_FRONTEND_EXPORTED_FUNCS` interface (`src/jl_exported_funcs.inc`):
`jl_frontend_init`, `jl_frontend_parse`, `jl_frontend_lower`,
`jl_macroexpand` and the operator queries (`jl_frontend_is_operator` etc.),
which the CLI loader binds to the library's `*_impl` functions (`entry.c`),
or to the stubs in `src/frontend-stubs.c` when the library is absent. The
operator queries, which printing uses, are answered with
`JuliaSyntax.operator_info`; once the host's Base has started, its own
JuliaSyntax answers them instead (`JuliaSyntax.register_operator_queries`).

The library contains a complete julia runtime of its own, of the release's
version, which the host julia (generally a different version) never sees:
the loader opens it with `RTLD_LOCAL | RTLD_DEEPBIND`, and it is linked with
`-Bsymbolic`. Its runtime starts the first time it is used (`fe_enter`), so
loading the library costs little when, as after bootstrapping, it is not.
The two runtimes share no values: `entry.c` converts syntax trees and lowered
code between them at the boundary (`host2fe`, `fe2host`), manipulating host
values only through a small table of host C API functions that it looks up by
name (`fe_resolve_host_api`). The host's modules are mirrored by shadow
modules, and other host values are carried through opaquely.

Macros are expanded on the host side before lowering, by a port of the C
macro expansion driver of the former flisp frontend (`h_expand_macros`): the
macro functions are invoked in the host runtime through
`jl_invoke_julia_macro`, so neither their arguments nor their expansions cross
runtimes. The expansions' hygiene crosses as `hygienic-scope` forms, which
JuliaLowering understands.

Lowering is "portable" (see `JuliaLowering.portable_lowering_hook` and
`DesugaringContext`): the lowered code must not embed values of the lowering
runtime, like functions of its JuliaLowering, so it references support
functions in `Core` by name (`Core.eval_closure_type`,
`Core.replace_captured_locals`, `Core.GeneratedFunctionStub`,
`Base._eval_import`, ...). Since separate frontend sessions see modules
through fresh shadow modules, the names lowering reserves in them carry a
salt derived from the host's world counter, so that the stages of the
bootstrap, whose output goes into one image, do not reuse each other's names.

## Checking it

`check.c` initializes a host julia, opens the library as the loader does and
checks parsing and lowering (including macros, closures, generated functions
and quoted syntax) through its interface, with host values.

## Limitations

- Building the library is only implemented for Linux and FreeBSD so far.
- The frontend interface has no edition parameter: it parses and lowers
  `Core.FLISP_EDITION` syntax, the edition of julia's own bootstrap code.
- Lowering warnings (the `warn` flag) are not forwarded to the host.
- `jl_macroexpand` leaves the `hygienic-scope` wrappers in place (it does not
  resolve them for `legacyscope`). After bootstrap, `Base.macroexpand` uses
  JuliaLowering instead.
