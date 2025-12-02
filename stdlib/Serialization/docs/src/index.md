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

### Recommended File Extension

While the Serialization module does not mandate a specific file extension, the Julia community commonly uses the `.jls` extension for serialized Julia files.

Example:

```julia
open("model.jls", "w") do io
    serialize(io, my_model)
end
```
