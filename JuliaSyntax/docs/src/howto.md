# How-To

This section contains brief recipes for particular tasks

## Use JuliaSyntax as the default parser

Since Julia 1.10, JuliaSyntax is Julia's default parser, so no setup is needed.

To instead use the version of the JuliaSyntax package in your environment (for
example a development version, or on Julia versions older than 1.10) for the
REPL, `include()`, `Meta.parse()`, etc, put the following in your startup.jl
file:

```julia
using JuliaSyntax
JuliaSyntax.enable_in_core!()
```

Use `JuliaSyntax.enable_in_core!(false)` to switch back to the built-in parser.
