# Package Manager Functions

All package manager functions are defined in the `Pkg` module. None of the `Pkg` module's functions
are exported; to use them, you'll need to prefix each function call with an explicit `Pkg.`, e.g.
[`Pkg.status()`](@ref) or [`Pkg.dir()`](@ref).

Functions for package development (e.g. `tag`, `publish`, etc.) have been moved to the [PkgDev](https://github.com/JuliaLang/PkgDev.jl)
package. See [PkgDev README](https://github.com/JuliaLang/PkgDev.jl/blob/master/README.md) for
the documentation of those functions.

```@docs
Base.Pkg.dir
Base.Pkg.init
Base.Pkg.resolve
Base.Pkg.edit
Base.Pkg.add
Base.Pkg.rm
Base.Pkg.clone
Base.Pkg.setprotocol!
Base.Pkg.available
Base.Pkg.installed
Base.Pkg.status
Base.Pkg.update
Base.Pkg.checkout
Base.Pkg.pin
Base.Pkg.free
Base.Pkg.build
Base.Pkg.test
Base.Pkg.dependents
```
