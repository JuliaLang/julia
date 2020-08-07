Julia can manage and automatically garbage collect scratch spaces of package data.
These spaces can contain datasets, text, binaries, or any other kind of data that would be convenient to store in a location specific to your package.
As compared to [Artifacts](https://julialang.github.io/Pkg.jl/latest/artifacts/), these containers of data are mutable.
Because the scratch space location on disk is not very user-friendly, scratch spaces should, in general, not be used for a storing files that the user must interact with through a file browser.
In that event, packages should simply write out to disk at a location given by the user.
Scratch spaces are designed for data caches that are completely managed by a package and should be removed when the package itself is uninstalled.
In the current implementation, scratch spaces are removed during Pkg garbage collection if the owning package has been removed.
Users can also request a full wipe of all scratch spaces to clean up unused disk space through `clear_scratchspaces!()`, or a more targeted wipe of a particular package through `clear_scratchspaces!(pkg)`

## API overview

Scratch space usage is performed primarily through one function: `get_scratch!()`.
It provides a single interface for creating and getting previously-created spaces, either tied to a package by its UUID, or as a global scratch space that can be accessed by any package.
Here is an example where a package creates a scratch space that is namespaced to its own UUID:

```julia
module ScratchExample
using Scratch

# This will be filled in inside `__init__()`
download_cache = ""

# Downloads a resource, stores it within a scratchspace
function download_dataset(url)
    fname = joinpath(download_cache, basename(url))
    if !isfile(fname)
        download(url, fname)
    end
    return fname
end

function __init__()
    global download_cache = @get_scratch!("downloaded_files")
end

end # module ScratchExample
```

Note that we initialize the `download_cache` within `__init__()` so that our packages are as relocatable as possible; we typically do not want to bake absolute paths into our precompiled files.
This makes use of the `@get_scratch!()` macro, which is identical to the `get_scratch!()` method, except it automatically determines the UUID of the calling module, if possible. The user can manually pass in a `Module` as well for a slightly more verbose incantation:
```julia
function __init__()
    global download_cache = get_scratch!("downloaded_files", @__MODULE__)
end
```

If a user wishes to manually delete a scratch space, the method `delete_scratch!(key; pkg_uuid)` is the natural analog to `get_scratch!()`, however in general users will not need to do so, the scratch spaces will be garbage collected by `Pkg.gc()` automatically.

For a full listing of docstrings and methods, see the [Scratch Space Reference](@ref) section.


## Tips and Tricks

> Can I trigger data regeneration if the scratch space is found to be empty/files are missing?

Yes, this is quite simple; just check the contents of the directory when you first call `get_scratch!()`, and if it's empty, run your generation function:

```julia
using Scratch

function get_dataset_dir()
    dataset_dir = @get_scratch!("dataset")
    if isempty(readdir(dataset_dir))
        perform_expensive_dataset_generation(dataset_dir)
    end
    return dataset_dir
end
```

This ensures your package is resilient against situations such as scratch spaces being deleted by a user that has called `clear_scratchspaces!()` to free up disk space.

> Can I create a scratch space that is not shared across versions of my package?

Yes!  Make use of the `key` parameter and Pkg's ability to look up the current version of your package at compile-time:

```julia
module VersionSpecificExample
using Pkg.TOML, Scratch

# Get the current version at compile-time, that's fine it's not going to change. ;)
function get_version()
    return VersionNumber(TOML.parsefile(joinpath(dirname(@__DIR__), "Project.toml"))["version"])
end
const pkg_version = get_version()

# This will be filled in by `__init__()`; it might change if we get deployed somewhere
version_specific_scratch = Ref{String}()

function __init__()
    # This space will be unique between versions of my package that different major and
    # minor versions, but allows patch releases to share the same.
    scratch_name = "data_for_version-$(pkg_version.major).$(pkg_version.minor)"
    global version_specific_scratch[] = @get_scratch!(scratch_name)
end

end # module
```

> When should I use scratch spaces, and when should I use Artifacts?

Artifacts should, in general, be used when dealing with storing data that is write-once, read-many times.
Because Artifacts are read-only and are content-addressed, this enables very easy transmission of Artifacts from machine to machine, and is why we use them extensively in the package ecosystem.
Scratch spaces, on the other hand, are mutable and not easily distributed, they should generally follow a write-many, read-many access pattern.
Scratch spaces are well-suited for storing machine-specific data, such as compiled objects, results of host introspection, or user-specific data.

> Can I use a scratch space as a temporary workspace, then turn it into an Artifact?

Yes!  Once you're satisfied with your dataset that has been cooking inside a space, and you're ready to share it with the world as an immutable artifact, you can use `create_artifact()` to create an artifact from the space, `archive_artifact()` to get a tarball that you can upload somewhere, and `bind_artifact!()` to write out an `Artifacts.toml` that allows others to download and use it:

```julia
using Pkg, Scratch, Pkg.Artifacts

function export_scratch(scratch_name::String, github_repo::String)
    scratch_dir = @get_scratch!(scratch_name)

    # Copy space directory over to an Artifact
    hash = create_artifact() do artifact_dir
        rm(artifact_dir)
        cp(scratch_dir, artifact_dir)
    end

    # Archive artifact out to a tarball.  Since `upload_tarball()` is not a function that
    # exists, users must either write it themselves (uploading to whatever hosting
    # provider they prefer), or run each line of this `do`-block manually, upload the
    # tarball manually, record its URL, and pass that to `bind_artifact!()`.
    mktempdir() do upload_dir
        tarball_path = joinpath(upload_dir, "$(scratch_name).tar.gz")
        tarball_hash = archive_artifact(hash, tarball_path)

        # Upload tarball to a hosted site somewhere.  Note; this function does not
        # exist, it's put here simply to show the flow of events.
        tarball_url = upload_tarball(tarball_path)

        # Bind artifact to an Artifacts.toml file in the current directory; this file can
        # be used by others to download and use your newly-created Artifact!
        bind_artifact!(
            joinpath(@__DIR__, "./Artifacts.toml"),
            scratch_name,
            hash;
            download_info=[(tarball_url, tarball_hash)],
            force=true,
        )
    end
end
```