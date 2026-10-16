# The `Compiler` module

This directory maintains the implementation of the Julia compiler.

Through a bootstrapping process, it is bundled into the Julia runtime as `Base.Compiler`.

You can also use this `Compiler` module as the `Compiler` standard library by following the steps below.

## How to use

To utilize this `Compiler.jl` standard library, you need to declare it as a dependency in
your `Project.toml` as follows:
> Project.toml
```toml
[deps]
Compiler = "807dbc54-b67e-4c79-8afb-eafe4df6f2e1"

[compat]
Compiler = "0.1"
```

With the setup above, [the special placeholder version (v0.1)](https://github.com/JuliaLang/BaseCompiler.jl)
will be installed by default.[^1]

[^1]: Currently, only version v0.1 is registered in the [General](https://github.com/JuliaRegistries/General) registry.

If needed, you can switch to a custom implementation of the `Compiler` module by running
```julia-repl
pkg> dev /path/to/Compiler.jl # to use a local implementation
```
or
```julia-repl
pkg> add https://url/of/Compiler/branch # to use a remote implementation
```
This feature is particularly useful for developing or experimenting with alternative compiler implementations.

> [!note]
> The Compiler.jl standard library is available starting from Julia v1.10.
> However, switching to a custom compiler implementation is supported only from
> Julia v1.12 onwards.

> [!warning]
> When using a custom, non-`Base` version of `Compiler` implementation, it may be necessary
> to run `InteractiveUtils.@activate Compiler` to ensure proper functionality of certain
> reflection utilities.

## Developing `Compiler` in the Julia repository

`Base.Compiler` picks up changes to `Compiler/` only when Julia is rebuilt.  To
rebuild and test it, use `make test-Compiler`.  To try changes without
rebuilding, run from the repository root:

```sh
./usr/bin/julia --project=Compiler
```

```julia
using InteractiveUtils
@activate Compiler           # Reflection (`code_typed`, ...) uses the loaded Compiler
@activate Compiler[:codegen] # Make Julia compilation use it
```

Reflection still shares the runtime's inference cache, so a broken compiler can
corrupt runtime state.
