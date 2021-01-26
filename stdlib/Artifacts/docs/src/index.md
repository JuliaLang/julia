# Artifacts

```@meta
DocTestSetup = :(using Artifacts)
```

Starting with Julia 1.6, the artifacts support has moved from `Pkg.jl` to Julia itself.
Until proper documentation can be added here, you can learn more about artifacts in the
`Pkg.jl` manual at <https://julialang.github.io/Pkg.jl/v1/artifacts/>.

!!! compat "Julia 1.6"
    Julia's artifacts API requires at least Julia 1.6. In Julia
    versions 1.3 to 1.5, you can use `Pkg.Artifacts` instead.


```@docs
Artifacts.artifact_meta
Artifacts.artifact_hash
Artifacts.find_artifacts_toml
Artifacts.@artifact_str
```
