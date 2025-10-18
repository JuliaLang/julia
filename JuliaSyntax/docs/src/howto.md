# How-To

This section contains brief recipes for particular tasks

## Use JuliaSyntax as the default parser

To use JuliaSyntax as the default Julia parser for the REPL and to `include()`
files, parse code with `Meta.parse()`, etc, put the following in your
startup.jl file:

```julia
using JuliaSyntax
JuliaSyntax.enable_in_core!()
```

This works well in Julia 1.9 but in Julia 1.8 will cause some startup latency.
To reduce that you can create a custom system image by running the code in
`./sysimage/compile.jl` as a Julia script (or directly using the shell, on
unix). Then use `julia -J $resulting_sysimage`.

Using a custom sysimage has the advantage that package precompilation will also
go through the JuliaSyntax parser.

### VSCode

To use JuliaSyntax as the default parser for Julia within VSCode, add the
following to your `startup.jl` file:

```julia
import JuliaSyntax
JuliaSyntax.enable_in_core!()
```

To reduce startup latency you can combine with a custom system as described in
the [Julia VScode docs](https://www.julia-vscode.org/docs/dev/userguide/compilesysimage/#Creating-a-sysimage-for-the-active-environment),
combined with the precompile execution file in `sysimage/precompile_exec.jl` in the source tree.
For additional detail see the discussion in [issue #128](https://github.com/JuliaLang/JuliaSyntax.jl/issues/128).

