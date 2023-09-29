# Module loading

[`Base.require`](@ref) is responsible for loading modules and it also manages the
precompilation cache. It is the implementation of the `import` statement.

## Experimental features
The features below are experimental and not part of the stable Julia API.
Before building upon them inform yourself about the current thinking and whether they might change soon.

### Package loading callbacks

It is possible to listen to the packages loaded by `Base.require`, by registering a callback.

```julia
loaded_packages = Base.PkgId[]
callback = (pkg::Base.PkgId) -> push!(loaded_packages, pkg)
push!(Base.package_callbacks, callback)
```

Using this would look something like:

```julia-repl
julia> using Example

julia> loaded_packages
1-element Vector{Base.PkgId}:
 Example [7876af07-990d-54b4-ab0e-23690620f79a]
```
