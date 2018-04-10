# Package Manager Functions

```@meta
DocTestSetup = :(using Pkg)
```

All package manager functions are defined in the `Pkg` module. None of the `Pkg` module's functions
are exported; to use them, you'll need to prefix each function call with an explicit `Pkg.`, e.g.
[`Pkg.status()`](@ref) or [`Pkg.dir()`](@ref).

Functions for package development (e.g. `tag`, `publish`, etc.) have been moved to the [PkgDev](https://github.com/JuliaLang/PkgDev.jl)
package. See [PkgDev README](https://github.com/JuliaLang/PkgDev.jl/blob/master/README.md) for
the documentation of those functions.

```@docs
Pkg.dir
Pkg.init
Pkg.resolve
Pkg.edit
Pkg.add
Pkg.rm
Pkg.clone
Pkg.setprotocol!
Pkg.available
Pkg.installed
Pkg.status
Pkg.update
Pkg.checkout
Pkg.pin
Pkg.free
Pkg.build
Pkg.test
Pkg.dependents
```

```@meta
DocTestSetup = nothing
```
