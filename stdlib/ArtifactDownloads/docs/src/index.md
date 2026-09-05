```@meta
EditURL = "https://github.com/JuliaLang/julia/blob/master/stdlib/ArtifactDownloads/docs/src/index.md"
```

# ArtifactDownloads

```@meta
DocTestSetup = :(using ArtifactDownloads)
```

`ArtifactDownloads` downloads, verifies, installs and authors artifacts. It is the writing
and networking half of the artifact system: the read-only half (`artifact"name"`,
`artifact_path`, `artifact_meta`, and so on) lives in [`Artifacts`](@ref) and needs no
network. `Pkg` builds its artifact installation on this module, and `LazyArtifacts`
depends on it rather than on `Pkg`, so a package that declares lazy artifacts loads this
module instead of all of Pkg.

!!! compat "Julia 1.14"
    `ArtifactDownloads` was added in Julia 1.14. On earlier versions the same functions are
    available as `Pkg.Artifacts` and `Pkg.PlatformEngines`.

```@docs
ArtifactDownloads.ensure_artifact_installed
ArtifactDownloads.download_artifact
ArtifactDownloads.pkg_server
ArtifactDownloads.create_artifact
ArtifactDownloads.bind_artifact!
ArtifactDownloads.unbind_artifact!
ArtifactDownloads.ArtifactDownloadInfo
ArtifactDownloads.archive_artifact
ArtifactDownloads.verify_artifact
ArtifactDownloads.remove_artifact
ArtifactDownloads.extract_all_hashes
```

## PlatformEngines

The lower level tarball handling used by the functions above.

```@docs
ArtifactDownloads.PlatformEngines.download_verify_unpack
ArtifactDownloads.PlatformEngines.download_verify
ArtifactDownloads.PlatformEngines.verify
ArtifactDownloads.PlatformEngines.package
ArtifactDownloads.PlatformEngines.detect_archive_format
ArtifactDownloads.PlatformEngines.get_extract_cmd
ArtifactDownloads.PlatformEngines.register_auth_error_handler
ArtifactDownloads.PlatformEngines.deregister_auth_error_handler
```

## Git tree hashes

```@docs
ArtifactDownloads.GitTreeHashTools.tree_hash
ArtifactDownloads.GitTreeHashTools.blob_hash
ArtifactDownloads.GitTreeHashTools.contains_files
```
