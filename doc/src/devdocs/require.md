# Module loading

[`Base.require`](@ref) is responsible for loading modules and it also manages the
precompilation cache. It is the implementation of the `import` statement.

## Experimental features
The features below are experimental and not part of the stable Julia API.
Before building upon them inform yourself about the current thinking and whether they might change soon.

### Module loading callbacks

It is possible to listen to calls to `Base.require`, by registering a callback.

```julia
loaded_packages = Channel{Base.PkgId}()
callback = (mod::PkgId) -> put!(loaded_packages, mod)
push!(Base.package_callbacks, callback)
```

The callback will fire once per `Base.require` call and it is the users responsibility
to filter calls. As an example one might only be interested in the first time a package
is required.
