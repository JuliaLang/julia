# Package Manager Functions

```@meta
DocTestSetup = :(using Pkg)
```

All package manager functions are defined in the `Pkg` module. None of the `Pkg` module's functions
are exported; to use them, you'll need to prefix each function call with an explicit `OldPkg.`, e.g.
[`OldPkg.status()`](@ref) or [`OldPkg.dir()`](@ref).

Functions for package development (e.g. `tag`, `publish`, etc.) have been moved to the [PkgDev](https://github.com/JuliaLang/PkgDev.jl)
package. See [PkgDev README](https://github.com/JuliaLang/PkgDev.jl/blob/master/README.md) for
the documentation of those functions.

```@docs
OldPkg.dir
OldPkg.init
OldPkg.resolve
OldPkg.edit
OldPkg.add
OldPkg.rm
OldPkg.clone
OldPkg.setprotocol!
OldPkg.available
OldPkg.installed
OldPkg.status
OldPkg.update
OldPkg.checkout
OldPkg.pin
OldPkg.free
OldPkg.build
OldPkg.test
OldPkg.dependents
```

```@meta
DocTestSetup = nothing
```
