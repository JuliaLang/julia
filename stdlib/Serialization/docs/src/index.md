```@meta
EditURL = "https://github.com/JuliaLang/julia/blob/master/stdlib/Serialization/docs/src/index.md"
```

# Serialization

Provides serialization of Julia objects.

```@docs
Serialization.serialize
Serialization.deserialize
Serialization.writeheader
```
## File Format and Conventions

!!! note
    Julia's binary serialization format is not guaranteed to be stable across Julia versions or platforms.
    It is best suited for temporary storage of data within the same Julia version.

### Recommended File Extension

While the Serialization module does not mandate a specific file extension, the Julia community commonly uses the `.jls` extension for serialized Julia files.

Example:

```julia
open("model.jls", "w") do io
    serialize(io, my_model)
end
```
