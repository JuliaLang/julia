# Module loading

[`Base.require`](@ref) is responsible for loading modules and it also manages the
precompilation cache. It is the implementation of the `import` statement.

## Experimental features
The features below are experimental and not part of the stable Julia API.
Before building upon them inform yourself about the current thinking and whether they might change soon.

### Module loading callbacks

It is possible to listen to the modules loaded by `Base.require`, by registering a callback.

```julia
loaded_packages = Channel{Symbol}()
callback = (mod::Symbol) -> put!(loaded_packages, mod)
push!(Base.package_callbacks, callback)
```

Please note that the symbol given to the callback is a non-unique identifier and
it is the responsibility of the callback provider to walk the module chain to
determine the fully qualified name of the loaded binding.

The callback below is an example of how to do that:

```julia
# Get the fully-qualified name of a module.
function module_fqn(name::Symbol)
    fqn = fullname(Base.root_module(name))
    return join(fqn, '.')
end
```
