# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Compiler

This experimental standard library module provides an interface to Julia's internal compiler APIs.

By default, `Compiler` exports the same names as `Base.Compiler`, making its usage
functionally equivalent to directly using `Base.Compiler`.
Until proper versioning of the `Compiler.jl` standard library is implemented, this default
fallback behavior is beneficial for maintaining compatibility among the Julia runtime,
the Julia compiler, and external package implementations relying on the compiler internals.

To utilize this `Compiler.jl` standard library, you need to declare it as a dependency in
your `Project.toml` as follows:
> Project.toml
```toml
[deps]
Compiler = "807dbc54-b67e-4c79-8afb-eafe4df6f2e1"

[compat]
Compiler = "0"
```

You can switch to a custom implementation of the `Compiler` module by executing
`pkg> dev /path/to/Compiler.jl` or `pkg> add https://url/of/Compiler/branch`.
This feature is particularly useful for developing or experimenting with alternative
compiler implementations.

!!! note
    The Compiler.jl standard library is available starting from Julia v1.10.
    However, switching to a custom compiler implementation is supported only from
    Julia v1.12 onwards.

!!! warning
    When using a custom, non-`Base` version of `Compiler` implementation, it may be necessary
    to run `InteractiveUtils.@activate Compiler` to ensure proper functionality of certain
    reflection utilities.
"""
baremodule Compiler

using Base: Base
const BaseCompiler = Base.:(>=)(Base.VERSION.minor, 12) ? Base.Compiler : Core.Compiler
let names = Base.:(>=)(Base.VERSION.minor, 12) ?
        Base.names(BaseCompiler; all=true, imported=true, usings=true) :
        Base.names(BaseCompiler; all=true, imported=true)
    for name in names
        if name === :Compiler
            continue
        end
        Core.eval(Compiler, :(using .BaseCompiler: $name))
        Core.eval(Compiler, Expr(:public, name))
    end
end

end
