# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    ArtifactDownloads

Download, verify, install and author artifacts. The read-only side of the artifact
system (`artifact"name"`, `artifact_path`, `artifact_meta`, ...) lives in `Artifacts`;
this module adds everything that needs the network or writes to the depot:
[`ensure_artifact_installed`](@ref), [`download_artifact`](@ref),
[`create_artifact`](@ref), [`bind_artifact!`](@ref) and friends, and the lower level
[`PlatformEngines`](@ref ArtifactDownloads.PlatformEngines) download, verify and unpack
machinery underneath them. `Pkg` builds on it, and `LazyArtifacts` depends on it rather
than on `Pkg`.
"""
module ArtifactDownloads

using Artifacts, Base.BinaryPlatforms, SHA
using Tar: can_symlink
using FileWatching: FileWatching
using TOML, Dates

import Base: SHA1
import Artifacts: artifact_names, ARTIFACTS_DIR_OVERRIDE, ARTIFACT_OVERRIDES, artifact_paths,
    artifacts_dirs, pack_platform!, unpack_platform, load_artifacts_toml,
    query_override, with_artifacts_directory, load_overrides

export create_artifact, remove_artifact, verify_artifact, archive_artifact,
    ArtifactDownloadInfo, bind_artifact!, unbind_artifact!, download_artifact,
    ensure_artifact_installed, extract_all_hashes

include("utils.jl")
include("MiniProgressBars.jl")
include("GitTreeHashTools.jl")
include("PlatformEngines.jl")

using .MiniProgressBars: print_progress_bottom
using .PlatformEngines: package, download_verify_unpack
using .GitTreeHashTools: GitTreeHashTools

"""
    create_artifact(f::Function)

Creates a new artifact by running `f(artifact_path)`, hashing the result, and moving it
to the artifact store (`~/.julia/artifacts` on a typical installation).  Returns the
identifying tree hash of this artifact.
"""
function create_artifact(f::Function)
    # Ensure the `artifacts` directory exists in our default depot
    artifacts_dir = first(artifacts_dirs())
    mkpath(artifacts_dir)
    create_cachedir_tag(artifacts_dir)

    # Temporary directory where we'll do our creation business
    temp_dir = mktempdir(artifacts_dir)

    try
        # allow the user to do their work inside the temporary directory
        f(temp_dir)

        # Calculate the tree hash for this temporary directory
        artifact_hash = SHA1(GitTreeHashTools.tree_hash(temp_dir))

        # If we created a dupe, just let the temp directory get destroyed. It's got the
        # same contents as whatever already exists after all, so it doesn't matter.  Only
        # move its contents if it actually contains new contents.  Note that we explicitly
        # set `honor_overrides=false` here, as we wouldn't want to drop things into the
        # system directory by accidentally creating something with the same content-hash
        # as something that was foolishly overridden.  This should be virtually impossible
        # unless the user has been very unwise, but let's be cautious.
        new_path = artifact_path(artifact_hash; honor_overrides = false)
        mv_temp_dir_retries(temp_dir, new_path)

        # Give the people what they want
        return artifact_hash
    finally
        # Always attempt to cleanup
        rm(temp_dir; recursive = true, force = true)
    end
end

"""
    remove_artifact(hash::SHA1; honor_overrides::Bool=false)

Removes the given artifact (identified by its SHA1 git tree hash) from disk.  Note that
if an artifact is installed in multiple depots, it will be removed from all of them.  If
an overridden artifact is requested for removal, it will be silently ignored; this method
will never attempt to remove an overridden artifact.

In general, we recommend that you use `Pkg.gc()` to manage artifact installations and do
not use `remove_artifact()` directly, as it can be difficult to know if an artifact is
being used by another package.
"""
function remove_artifact(hash::SHA1)
    if query_override(hash) !== nothing
        # We never remove overridden artifacts.
        return
    end

    # Get all possible paths (rooted in all depots)
    possible_paths = artifacts_dirs(bytes2hex(hash.bytes))
    for path in possible_paths
        if isdir(path)
            rm(path; recursive = true, force = true)
        end
    end
    return
end

"""
    verify_artifact(hash::SHA1; honor_overrides::Bool=false)

Verifies that the given artifact (identified by its SHA1 git tree hash) is installed on-
disk, and retains its integrity.  If the given artifact is overridden, skips the
verification unless `honor_overrides` is set to `true`.
"""
function verify_artifact(hash::SHA1; honor_overrides::Bool = false)
    # Silently skip overridden artifacts unless we really ask for it
    if !honor_overrides
        if query_override(hash) !== nothing
            return true
        end
    end

    # If it doesn't even exist, then skip out
    if !artifact_exists(hash)
        return false
    end

    # Otherwise actually run the verification
    return all(hash.bytes .== GitTreeHashTools.tree_hash(artifact_path(hash)))
end

"""
    archive_artifact(hash::SHA1, tarball_path::String; honor_overrides::Bool=false)

Archive an artifact into a tarball stored at `tarball_path`, returns the SHA256 of the
resultant tarball as a hexadecimal string. Throws an error if the artifact does not
exist.  If the artifact is overridden, throws an error unless `honor_overrides` is set.
"""
function archive_artifact(hash::SHA1, tarball_path::String; honor_overrides::Bool = false)
    if !honor_overrides
        if query_override(hash) !== nothing
            error("Will not archive an overridden artifact unless `honor_overrides` is set!")
        end
    end

    if !artifact_exists(hash)
        error("Unable to archive artifact $(bytes2hex(hash.bytes)): does not exist!")
    end

    # Package it up
    package(artifact_path(hash), tarball_path)

    # Calculate its sha256 and return that
    return open(tarball_path, "r") do io
        return bytes2hex(sha256(io))
    end
end

"""
    ArtifactDownloadInfo

Auxiliary information about an artifact to be used with `bind_artifact!()` to give
a download location for that artifact, as well as the hash and size of that artifact.
"""
struct ArtifactDownloadInfo
    # URL the artifact is available at as a gzip-compressed tarball
    url::String

    # SHA256 hash of the tarball
    hash::Vector{UInt8}

    # Size in bytes of the tarball.  `size <= 0` means unknown.
    size::Int64

    function ArtifactDownloadInfo(url, hash::AbstractVector, size = 0)
        valid_hash_len = SHA.digestlen(SHA256_CTX)
        hash_len = length(hash)
        if hash_len != valid_hash_len
            throw(ArgumentError("Invalid hash length '$(hash_len)', must be $(valid_hash_len)"))
        end
        return new(
            String(url),
            Vector{UInt8}(hash),
            Int64(size),
        )
    end
end

# Convenience constructor for string hashes
ArtifactDownloadInfo(url, hash::AbstractString, args...) = ArtifactDownloadInfo(url, hex2bytes(hash), args...)

# Convenience constructor for legacy Tuple representation
ArtifactDownloadInfo(args::Tuple) = ArtifactDownloadInfo(args...)

ArtifactDownloadInfo(adi::ArtifactDownloadInfo) = adi

# Make the dict that will be embedded in the TOML
function make_dict(adi::ArtifactDownloadInfo)
    ret = Dict{String, Any}(
        "url" => adi.url,
        "sha256" => bytes2hex(adi.hash),
    )
    if adi.size > 0
        ret["size"] = adi.size
    end
    return ret
end

"""
    bind_artifact!(artifacts_toml::String, name::String, hash::SHA1;
                    platform::Union{AbstractPlatform,Nothing} = nothing,
                    download_info::Union{Vector{Tuple},Nothing} = nothing,
                    lazy::Bool = false,
                    force::Bool = false)

Writes a mapping of `name` -> `hash` within the given `(Julia)Artifacts.toml` file. If
`platform` is not `nothing`, this artifact is marked as platform-specific, and will be
a multi-mapping.  It is valid to bind multiple artifacts with the same name, but
different `platform`s and `hash`'es within the same `artifacts_toml`.  If `force` is set
to `true`, this will overwrite a pre-existent mapping, otherwise an error is raised.

`download_info` is an optional vector that contains tuples of URLs and a hash.  These
URLs will be listed as possible locations where this artifact can be obtained.  If `lazy`
is set to `true`, even if download information is available, this artifact will not be
downloaded until it is accessed via the `artifact"name"` syntax, or
`ensure_artifact_installed()` is called upon it.
"""
function bind_artifact!(
        artifacts_toml::String, name::String, hash::SHA1;
        platform::Union{AbstractPlatform, Nothing} = nothing,
        download_info::Union{Vector{<:Tuple}, Vector{<:ArtifactDownloadInfo}, Nothing} = nothing,
        lazy::Bool = false,
        force::Bool = false
    )
    # First, check to see if this artifact is already bound. Parse afresh rather than
    # through Base's TOML cache, since the dict is mutated below.
    if isfile(artifacts_toml)
        artifact_dict = TOML.parsefile(artifacts_toml)

        if !force && haskey(artifact_dict, name)
            meta = artifact_dict[name]
            if !isa(meta, Vector)
                error("Mapping for '$name' within $(artifacts_toml) already exists!")
            elseif any(p -> platforms_match(platform, p), unpack_platform(x, name, artifacts_toml) for x in meta)
                error("Mapping for '$name'/$(triplet(platform)) within $(artifacts_toml) already exists!")
            end
        end
    else
        artifact_dict = Dict{String, Any}()
    end

    # Otherwise, the new piece of data we're going to write out is this dict:
    meta = Dict{String, Any}(
        "git-tree-sha1" => bytes2hex(hash.bytes),
    )

    # If we're set to be lazy, then lazy we shall be
    if lazy
        meta["lazy"] = true
    end

    # Integrate download info, if it is given.  Note that there can be multiple
    # download locations, each with its own tarball with its own hash, but which
    # expands to the same content/treehash.
    if download_info !== nothing
        meta["download"] = make_dict.(ArtifactDownloadInfo.(download_info))
    end

    if platform === nothing
        artifact_dict[name] = meta
    else
        # Add platform-specific keys to our `meta` dict
        pack_platform!(meta, platform)

        # Insert this entry into the list of artifacts
        if !haskey(artifact_dict, name)
            artifact_dict[name] = [meta]
        else
            # Delete any entries that contain identical platforms
            artifact_dict[name] = filter(
                x -> unpack_platform(x, name, artifacts_toml) != platform,
                artifact_dict[name]
            )
            push!(artifact_dict[name], meta)
        end
    end

    # Spit it out onto disk
    atomic_toml_write(artifacts_toml, artifact_dict, sorted = true)

    # Mark that we have used this Artifact.toml
    write_env_usage(artifacts_toml, "artifact_usage.toml")
    return
end

"""
    unbind_artifact!(artifacts_toml::String, name::String; platform = nothing)

Unbind the given `name` from an `(Julia)Artifacts.toml` file.
Silently fails if no such binding exists within the file.
"""
function unbind_artifact!(
        artifacts_toml::String, name::String;
        platform::Union{AbstractPlatform, Nothing} = nothing
    )
    artifact_dict = TOML.parsefile(artifacts_toml)
    if !haskey(artifact_dict, name)
        return
    end

    if platform === nothing
        delete!(artifact_dict, name)
    else
        artifact_dict[name] = filter(
            x -> unpack_platform(x, name, artifacts_toml) != platform,
            artifact_dict[name]
        )
    end

    atomic_toml_write(artifacts_toml, artifact_dict, sorted = true)
    return
end

"""
    download_artifact(tree_hash::SHA1, tarball_url::String, tarball_hash::String;
                        verbose::Bool = false, io::IO=stderr)

Download/install an artifact into the artifact store.  Returns `true` on success,
returns an error object on failure.

!!! compat "Julia 1.8"
    As of Julia 1.8 this function returns the error object rather than `false` when
    failure occurs
"""
function download_artifact(
        tree_hash::SHA1,
        tarball_url::String,
        tarball_hash::Union{String, Nothing} = nothing;
        verbose::Bool = false,
        quiet_download::Bool = false,
        io::IO = stderr_f(),
        progress::Union{Function, Nothing} = nothing,
    )
    _artifact_paths = artifact_paths(tree_hash)
    pidfile = _artifact_paths[1] * ".pid"
    mkpath(dirname(pidfile))
    t_wait_msg = Timer(2) do t
        if progress === nothing
            @info "downloading $tarball_url ($(bytes2hex(tree_hash.bytes))) in another process"
        else
            progress(0, 0; status = "downloading in another process")
        end
    end
    ret = FileWatching.mkpidlock(pidfile, stale_age = 20) do
        close(t_wait_msg)
        if artifact_exists(tree_hash)
            return true
        end

        # Ensure the `artifacts` directory exists in our default depot
        artifacts_dir = first(artifacts_dirs())
        mkpath(artifacts_dir)
        create_cachedir_tag(artifacts_dir)
        # expected artifact path
        dst = joinpath(artifacts_dir, bytes2hex(tree_hash.bytes))

        # We download by using a temporary directory.  We do this because the download may
        # be corrupted or even malicious; we don't want to clobber someone else's artifact
        # by trusting the tree hash that has been given to us; we will instead download it
        # to a temporary directory, calculate the true tree hash, then move it to the proper
        # location only after knowing what it is, and if something goes wrong in the process,
        # everything should be cleaned up.

        # Temporary directory where we'll do our creation business
        temp_dir = mktempdir(artifacts_dir)

        try
            download_verify_unpack(
                tarball_url, tarball_hash, temp_dir;
                ignore_existence = true, verbose, quiet_download, io, progress
            )
            isnothing(progress) || progress(10000, 10000; status = "verifying")
            calc_hash = SHA1(GitTreeHashTools.tree_hash(temp_dir))

            # Did we get what we expected?  If not, freak out.
            if calc_hash.bytes != tree_hash.bytes
                msg = """
                Tree Hash Mismatch!
                Expected git-tree-sha1:   $(bytes2hex(tree_hash.bytes))
                Calculated git-tree-sha1: $(bytes2hex(calc_hash.bytes))
                """
                # Since tree hash calculation is rather fragile and file system dependent,
                # we allow setting JULIA_PKG_IGNORE_HASHES=1 to ignore the error and move
                # the artifact to the expected location and return true
                ignore_hash_env_set = get(ENV, "JULIA_PKG_IGNORE_HASHES", "") != ""
                if ignore_hash_env_set
                    ignore_hash = Base.get_bool_env("JULIA_PKG_IGNORE_HASHES", false)
                    ignore_hash === nothing && @error(
                        "Invalid ENV[\"JULIA_PKG_IGNORE_HASHES\"] value",
                        ENV["JULIA_PKG_IGNORE_HASHES"],
                    )
                    ignore_hash = something(ignore_hash, false)
                else
                    # default: false except Windows users who can't symlink
                    ignore_hash = Sys.iswindows() &&
                        !mktempdir(can_symlink, artifacts_dir)
                end
                if ignore_hash
                    desc = ignore_hash_env_set ?
                        "Environment variable \$JULIA_PKG_IGNORE_HASHES is true" :
                        "System is Windows and user cannot create symlinks"
                    msg *= "\n$desc: \
                    ignoring hash mismatch and moving \
                    artifact to the expected location"
                    @error(msg)
                else
                    error(msg)
                end
            end
            # Move it to the location we expected
            isnothing(progress) || progress(10000, 10000; status = "moving to artifact store")
            mv_temp_dir_retries(temp_dir, dst)
        catch err
            @debug "download_artifact error" tree_hash tarball_url tarball_hash err
            if isa(err, InterruptException)
                rethrow(err)
            end
            # If something went wrong during download, return the error
            return err
        finally
            # Always attempt to cleanup
            try
                rm(temp_dir; recursive = true, force = true)
            catch e
                e isa InterruptException && rethrow()
                @warn("Failed to clean up temporary directory $(repr(temp_dir))", exception = e)
            end
        end
        return true
    end

    return ret
end

"""
    ensure_artifact_installed(name::String, artifacts_toml::String;
                                platform::AbstractPlatform = HostPlatform(),
                                pkg_uuid::Union{Base.UUID,Nothing}=nothing,
                                verbose::Bool = false,
                                quiet_download::Bool = false,
                                io::IO=stderr)

Ensures an artifact is installed, downloading it via the download information stored in
`artifacts_toml` if necessary.  Throws an error if unable to install.

The package server (see [`pkg_server`](@ref)) is tried before the URLs listed in
`artifacts_toml`, unless `pkg_server_eligible` is `false`.
"""
function ensure_artifact_installed(
        name::String, artifacts_toml::String;
        platform::AbstractPlatform = HostPlatform(),
        pkg_uuid::Union{Base.UUID, Nothing} = nothing,
        pkg_server_eligible::Bool = true,
        verbose::Bool = false,
        quiet_download::Bool = false,
        progress::Union{Function, Nothing} = nothing,
        io::IO = stderr_f()
    )
    meta = artifact_meta(name, artifacts_toml; pkg_uuid = pkg_uuid, platform = platform)
    if meta === nothing
        error("Cannot locate artifact '$(name)' in '$(artifacts_toml)'")
    end

    return ensure_artifact_installed(
        name, meta, artifacts_toml;
        pkg_server_eligible, platform, verbose, quiet_download, progress, io
    )
end

function ensure_artifact_installed(
        name::String, meta::Dict, artifacts_toml::String;
        pkg_server_eligible::Bool = true,
        platform::AbstractPlatform = HostPlatform(),
        verbose::Bool = false,
        quiet_download::Bool = false,
        progress::Union{Function, Nothing} = nothing,
        io::IO = stderr_f()
    )
    hash = SHA1(meta["git-tree-sha1"])
    if !artifact_exists(hash)
        if isnothing(progress) || verbose == true
            return try_artifact_download_sources(name, hash, meta, artifacts_toml; pkg_server_eligible, platform, verbose, quiet_download, io)
        else
            # if a custom progress handler is given it is taken to mean the caller wants to handle the download scheduling
            return () -> try_artifact_download_sources(name, hash, meta, artifacts_toml; pkg_server_eligible, platform, quiet_download = true, io, progress)
        end
    else
        return artifact_path(hash)
    end
end

function try_artifact_download_sources(
        name::String, hash::SHA1, meta::Dict, artifacts_toml::String;
        pkg_server_eligible::Bool = true,
        platform::AbstractPlatform = HostPlatform(),
        verbose::Bool = false,
        quiet_download::Bool = false,
        io::IO = stderr_f(),
        progress::Union{Function, Nothing} = nothing
    )

    errors = Any[]
    # first try downloading from Pkg server if the Pkg server knows about this package
    if pkg_server_eligible && (server = pkg_server()) !== nothing
        url = "$server/artifact/$hash"
        download_success = let url = url
            @debug "Downloading artifact from Pkg server" name artifacts_toml platform url
            with_show_download_info(io, name, quiet_download) do
                download_artifact(hash, url; verbose, quiet_download, io, progress)
            end
        end
        # download_success is either `true` or an error object
        if download_success === true
            return artifact_path(hash)
        else
            @debug "Failed to download artifact from Pkg server" download_success
            push!(errors, (url, download_success))
        end
    end

    # If this artifact does not exist on-disk already, ensure it has download
    # information, then download it!
    if !haskey(meta, "download")
        error("Cannot automatically install '$(name)'; no download section in '$(artifacts_toml)'")
    end

    # Attempt to download from all sources
    for entry in meta["download"]
        url = entry["url"]
        tarball_hash = entry["sha256"]
        download_success = let url = url
            @debug "Downloading artifact" name artifacts_toml platform url
            with_show_download_info(io, name, quiet_download) do
                download_artifact(hash, url, tarball_hash; verbose, quiet_download, io, progress)
            end
        end
        # download_success is either `true` or an error object
        if download_success === true
            return artifact_path(hash)
        else
            @debug "Failed to download artifact" download_success
            push!(errors, (url, download_success))
        end
    end
    errmsg = """
    Unable to automatically download/install artifact '$(name)' from sources listed in '$(artifacts_toml)'.
    Sources attempted:
    """
    for (url, err) in errors
        errmsg *= "- $(url)\n"
        errmsg *= "    Error: $(sprint(showerror, err))\n"
    end
    error(errmsg)
end

function with_show_download_info(f, io, name, quiet_download)
    fancyprint = can_fancyprint(io)
    if !quiet_download
        fancyprint && print_progress_bottom(io)
        printpkgstyle(io, :Downloading, "artifact: $name")
    end
    success = false
    try
        result = f()
        success = result === true
        return result
    finally
        if !quiet_download
            fancyprint && print(io, "\033[1A") # move cursor up one line
            fancyprint && print(io, "\033[2K") # clear line
            if success
                fancyprint && printpkgstyle(io, :Downloaded, "artifact: $name")
            else
                printpkgstyle(io, :Failure, "artifact: $name", color = :red)
            end
        end
    end
end

"""
    extract_all_hashes(artifacts_toml::String;
                        platform = HostPlatform(),
                        pkg_uuid = nothing,
                        include_lazy = false)

Extract all hashes from a given `(Julia)Artifacts.toml` file. `package_uuid` must
be provided to properly support overrides from `Overrides.toml` entries in depots.

If `include_lazy` is set to `true`, then lazy packages will be installed as well.
"""
function extract_all_hashes(
        artifacts_toml::String;
        platform::AbstractPlatform = HostPlatform(),
        pkg_uuid::Union{Nothing, Base.UUID} = nothing,
        include_lazy::Bool = false
    )
    hashes = Base.SHA1[]
    if !isfile(artifacts_toml)
        return hashes
    end

    artifact_dict = load_artifacts_toml(artifacts_toml; pkg_uuid = pkg_uuid)

    for name in keys(artifact_dict)
        # Get the metadata about this name for the requested platform
        meta = artifact_meta(name, artifact_dict, artifacts_toml; platform = platform)

        # If there are no instances of this name for the desired platform, skip it
        meta === nothing && continue

        # If it's a lazy one and we aren't including lazy ones, skip
        if get(meta, "lazy", false) && !include_lazy
            continue
        end

        # Otherwise, add it to the list!
        push!(hashes, Base.SHA1(meta["git-tree-sha1"]))
    end

    return hashes
end

# Support `AbstractString`s, but avoid compilers needing to track backedges for callers
# of these functions in case a user defines a new type that is `<: AbstractString`
archive_artifact(hash::SHA1, tarball_path::AbstractString; kwargs...) =
    archive_artifact(hash, string(tarball_path)::String; kwargs...)
bind_artifact!(artifacts_toml::AbstractString, name::AbstractString, hash::SHA1; kwargs...) =
    bind_artifact!(string(artifacts_toml)::String, string(name)::String, hash; kwargs...)
unbind_artifact!(artifacts_toml::AbstractString, name::AbstractString; kwargs...) =
    unbind_artifact!(string(artifacts_toml)::String, string(name)::String; kwargs...)
download_artifact(tree_hash::SHA1, tarball_url::AbstractString, args...; kwargs...) =
    download_artifact(tree_hash, string(tarball_url)::String, args...; kwargs...)
ensure_artifact_installed(name::AbstractString, artifacts_toml::AbstractString; kwargs...) =
    ensure_artifact_installed(string(name)::String, string(artifacts_toml)::String; kwargs...)
ensure_artifact_installed(name::AbstractString, meta::Dict, artifacts_toml::AbstractString; kwargs...) =
    ensure_artifact_installed(string(name)::String, meta, string(artifacts_toml)::String; kwargs...)
extract_all_hashes(artifacts_toml::AbstractString; kwargs...) =
    extract_all_hashes(string(artifacts_toml)::String; kwargs...)

end # module ArtifactDownloads
