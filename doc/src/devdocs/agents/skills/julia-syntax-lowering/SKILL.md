---
name: julia-syntax-lowering
description: Develop and test JuliaSyntax and JuliaLowering in Julia's source tree. Use when modifying JuliaSyntax/ or JuliaLowering/, running package or targeted tests, or checking JuliaLowering execution.
---

# JuliaSyntax & JuliaLowering development

Use this when modifying `JuliaSyntax/` or `JuliaLowering/` source, tests, or
package metadata.

## Testing JuliaSyntax

Run package tests from the repository root:

```sh
julia --project=JuliaSyntax -e 'using Pkg; Pkg.test()'
```

`JuliaSyntax` is not tied only to the in-tree Julia runtime; if a change may be
version-sensitive, test it with the relevant installed Julia version(s).

## Testing JuliaLowering

Prefer the in-tree Julia runtime because compatibility with the Julia runtime
may matter:

```sh
./usr/bin/julia --project=JuliaLowering -e 'using Pkg; Pkg.test()'
```

If `./usr/bin/julia` is unavailable, note that limitation and use an available
`julia` executable only for preliminary validation.

In this repository, `JuliaLowering/Project.toml` points its `JuliaSyntax`
dependency at the sibling `JuliaSyntax` checkout
(`path = "../JuliaSyntax"`). Therefore, `JuliaSyntax` changes can affect
`JuliaLowering`. When a `JuliaSyntax` change may affect lowering behavior or
syntax data consumed by `JuliaLowering`, run the `JuliaLowering` test command as
well.

### Targeted JuliaLowering tests

For targeted `JuliaLowering` test execution, include
`JuliaLowering/test/utils.jl` before the test file, matching
`JuliaLowering/test/runtests.jl`:

```sh
./usr/bin/julia --project=JuliaLowering -e 'cd("JuliaLowering/test") do; include("utils.jl"); include("scopes.jl"); end'
```

For `_ir.jl` fixtures, call `test_ir_cases` after loading `utils.jl`:

```sh
./usr/bin/julia --project=JuliaLowering -e 'cd("JuliaLowering/test") do; include("utils.jl"); test_ir_cases("scopes_ir.jl"); end'
```

## Trying JuliaLowering execution

For quick execution checks, run with the in-tree Julia runtime and the
`JuliaLowering` project:

```sh
./usr/bin/julia --project=JuliaLowering
```

Then either evaluate code explicitly through `JuliaLowering`:

```julia
using JuliaLowering
JuliaLowering.include_string(Main, "1 + 2")
```

or activate `JuliaLowering` as the process lowerer, then evaluate subsequent
code normally:

```julia
using JuliaLowering
JuliaLowering.activate!()

1 + 2
```

Use `include("path/to/file.jl")` after activation when checking a file. When
using `JuliaLowering.activate!()` from `-e`, make the code under test a separate
top-level statement after activation, or put it in `include(...)`.
