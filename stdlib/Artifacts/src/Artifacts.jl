# This file is a part of Julia. License is MIT: https://julialang.org/license

module Artifacts

import Base: get, SHA1
using Base.BinaryPlatforms, Base.TOML

export artifact_exists, artifact_path, artifact_meta, artifact_hash,
       select_downloadable_artifacts, find_artifacts_toml, @artifact_str

"""
    parse_toml(path::String)

Uses Base.TOML to parse a TOML file. Do not mutate the returned dictionary.
"""
function parse_toml(path::String)
    # Uses the caching mechanics for toml files in Base
    Base.parsed_toml(path)
end

# keep in sync with Base.project_names and Base.manifest_names
const artifact_names = ("JuliaArtifacts.toml", "Artifacts.toml")

const ARTIFACTS_DIR_OVERRIDE = Ref{Union{String,Nothing}}(nothing)
"""
    with_artifacts_directory(f::Function, artifacts_dir::String)

Helper function to allow temporarily changing the artifact installation and search
directory.  When this is set, no other directory will be searched for artifacts, and new
artifacts will be installed within this directory.  Similarly, removing an artifact will
only effect the given artifact directory.  To layer artifact installation locations, use
the typical Julia depot path mechanism.
"""
function with_artifacts_directory(f::Function, artifacts_dir::String)
    try
        ARTIFACTS_DIR_OVERRIDE[] = artifacts_dir
        f()
    finally
        ARTIFACTS_DIR_OVERRIDE[] = nothing
    end
end

"""
    artifacts_dirs(args...)

Return a list of paths joined into all possible artifacts directories, as dictated by the
current set of depot paths and the current artifact directory override via the method
`with_artifacts_dir()`.
"""
function artifacts_dirs(args...)
    if ARTIFACTS_DIR_OVERRIDE[] === nothing
        return String[abspath(depot, "artifacts", args...) for depot in Base.DEPOT_PATH]
    else
        # If we've been given an override, use _only_ that directory.
        return String[abspath(ARTIFACTS_DIR_OVERRIDE[], args...)]
    end
end

"""
    ARTIFACT_OVERRIDES

Artifact locations can be overridden by writing `Override.toml` files within the artifact
directories of Pkg depots.  For example, in the default depot `~/.julia`, one may create
a `~/.julia/artifacts/Override.toml` file with the following contents:

    78f35e74ff113f02274ce60dab6e92b4546ef806 = "/path/to/replacement"
    c76f8cda85f83a06d17de6c57aabf9e294eb2537 = "fb886e813a4aed4147d5979fcdf27457d20aa35d"

    [d57dbccd-ca19-4d82-b9b8-9d660942965b]
    c_simple = "/path/to/c_simple_dir"
    libfoo = "fb886e813a4aed4147d5979fcdf27457d20aa35d""

This file defines four overrides; two which override specific artifacts identified
through their content hashes, two which override artifacts based on their bound names
within a particular package's UUID.  In both cases, there are two different targets of
the override: overriding to an on-disk location through an absolutet path, and
overriding to another artifact by its content-hash.
"""
const ARTIFACT_OVERRIDES = Ref{Union{Dict{Symbol,Any},Nothing}}(nothing)
function load_overrides(;force::Bool = false)::Dict{Symbol, Any}
    if ARTIFACT_OVERRIDES[] !== nothing && !force
        return ARTIFACT_OVERRIDES[]
    end

    # We organize our artifact location overrides into two camps:
    #  - overrides per UUID with artifact names mapped to a new location
    #  - overrides per hash, mapped to a new location.
    #
    # Overrides per UUID/bound name are intercepted upon Artifacts.toml load, and new
    # entries within the "hash" overrides are generated on-the-fly.  Thus, all redirects
    # mechanisticly happen through the "hash" overrides.
    overrides = Dict{Symbol,Any}(
        # Overrides by UUID
        :UUID => Dict{Base.UUID,Dict{String,Union{String,SHA1}}}(),

        # Overrides by hash
        :hash => Dict{SHA1,Union{String,SHA1}}(),
    )

    for override_file in reverse(artifacts_dirs("Overrides.toml"))
        !isfile(override_file) && continue

        # Load the toml file
        depot_override_dict = parse_toml(override_file)

        function parse_mapping(mapping::String, name::String)
            if !isabspath(mapping) && !isempty(mapping)
                mapping = tryparse(Base.SHA1, mapping)
                if mapping === nothing
                    @error("Invalid override in '$(override_file)': entry '$(name)' must map to an absolute path or SHA1 hash!")
                end
            end
            return mapping
        end
        function parse_mapping(mapping::Dict, name::String)
            return Dict(k => parse_mapping(v, name) for (k, v) in mapping)
        end
        # Fallthrough for invalid Overrides.toml files
        parse_mapping(mapping, name::String) = nothing

        for (k, mapping) in depot_override_dict
            # First, parse the mapping. Is it an absolute path, a valid SHA1-hash, or neither?
            mapping = parse_mapping(mapping, k)
            if mapping === nothing
                @error("Invalid override in '$(override_file)': failed to parse entry `$(k)`")
                continue
            end

            # Next, determine if this is a hash override or a UUID/name override
            if isa(mapping, String) || isa(mapping, SHA1)
                # if this mapping is a direct mapping (e.g. a String), store it as a hash override
                local hash_str
                hash = tryparse(Base.SHA1, k)
                if hash === nothing
                    @error("Invalid override in '$(override_file)': Invalid SHA1 hash '$(k)'")
                    continue
                end

                # If this mapping is the empty string, un-override it
                if mapping == ""
                    delete!(overrides[:hash], hash)
                else
                    overrides[:hash][hash] = mapping
                end
            elseif isa(mapping, Dict)
                # Convert `k` into a uuid
                uuid = tryparse(Base.UUID, k)
                if uuid === nothing
                    @error("Invalid override in '$(override_file)': Invalid UUID '$(k)'")
                    continue
                end

                # If this mapping is itself a dict, store it as a set of UUID/artifact name overrides
                ovruuid = overrides[:UUID]::Dict{Base.UUID,Dict{String,Union{String,SHA1}}}
                if !haskey(ovruuid, uuid)
                    ovruuid[uuid] = Dict{String,Union{String,SHA1}}()
                end

                # For each name in the mapping, update appropriately
                for (name, override_value) in mapping
                    # If the mapping for this name is the empty string, un-override it
                    if override_value == ""
                        delete!(ovruuid[uuid], name)
                    else
                        # Otherwise, store it!
                        ovruuid[uuid][name] = override_value
                    end
                end
            else
                @error("Invalid override in '$(override_file)': unknown mapping type for '$(k)': $(typeof(mapping))")
            end
        end
    end

    ARTIFACT_OVERRIDES[] = overrides
    return overrides
end

# Helpers to map an override to an actual path
map_override_path(x::AbstractString) = String(x)::String
map_override_path(x::SHA1) = artifact_path(x)
map_override_path(x::Nothing) = nothing

"""
    query_override(hash::SHA1; overrides::Dict = load_overrides())

Query the loaded `<DEPOT>/artifacts/Overrides.toml` settings for artifacts that should be
redirected to a particular path or another content-hash.
"""
function query_override(hash::SHA1; overrides::Dict{Symbol,Any} = load_overrides())
    return map_override_path(get(overrides[:hash], hash, nothing))
end
function query_override(pkg::Base.UUID, artifact_name::String; overrides::Dict{Symbol,Any} = load_overrides())
    if haskey(overrides[:UUID], pkg)
        return map_override_path(get(overrides[:UUID][pkg], artifact_name, nothing))
    end
    return nothing
end

"""
    artifact_paths(hash::SHA1; honor_overrides::Bool=true)

Return all possible paths for an artifact given the current list of depots as returned
by `Pkg.depots()`.  All, some or none of these paths may exist on disk.
"""
function artifact_paths(hash::SHA1; honor_overrides::Bool=true)
    # First, check to see if we've got an override:
    if honor_overrides
        override = query_override(hash)
        if override !== nothing
            return [override]
        end
    end

    return artifacts_dirs(bytes2hex(hash.bytes))
end

"""
    artifact_path(hash::SHA1; honor_overrides::Bool=true)

Given an artifact (identified by SHA1 git tree hash), return its installation path.  If
the artifact does not exist, returns the location it would be installed to.

!!! compat "Julia 1.3"
    This function requires at least Julia 1.3.
"""
function artifact_path(hash::SHA1; honor_overrides::Bool=true)
    # Get all possible paths (rooted in all depots)
    possible_paths = artifact_paths(hash; honor_overrides=honor_overrides)

    # Find the first path that exists and return it
    for p in possible_paths
        if isdir(p)
            return p
        end
    end

    # If none exist, then just return the one that would exist within `depots1()`.
    return first(possible_paths)
end

"""
    artifact_exists(hash::SHA1; honor_overrides::Bool=true)

Returns whether or not the given artifact (identified by its sha1 git tree hash) exists
on-disk.  Note that it is possible that the given artifact exists in multiple locations
(e.g. within multiple depots).

!!! compat "Julia 1.3"
    This function requires at least Julia 1.3.
"""
function artifact_exists(hash::SHA1; honor_overrides::Bool=true)
    return any(isdir, artifact_paths(hash; honor_overrides=honor_overrides))
end

"""
    unpack_platform(entry::Dict, name::String, artifacts_toml::String)

Given an `entry` for the artifact named `name`, located within the file `artifacts_toml`,
returns the `Platform` object that this entry specifies.  Returns `nothing` on error.
"""
function unpack_platform(entry::Dict{String,Any}, name::String,
                         artifacts_toml::String)::Union{Nothing,Platform}
    if !haskey(entry, "os")
        @error("Invalid artifacts file at '$(artifacts_toml)': platform-specific artifact entry '$name' missing 'os' key")
        return nothing
    end

    if !haskey(entry, "arch")
        @error("Invalid artifacts file at '$(artifacts_toml)': platform-specific artifact entrty '$name' missing 'arch' key")
        return nothing
    end

    # Collect all String-valued mappings in `entry` and use them as tags
    tags = Dict{Symbol, String}()
    for (k, v) in entry
        if v isa String
            tags[Symbol(k)] = v
        end
    end
    # Removing some known entries that shouldn't be passed through `tags`
    delete!(tags, :os)
    delete!(tags, :arch)
    delete!(tags, Symbol("git-tree-sha1"))
    return Platform(entry["arch"], entry["os"]; tags...)
end

function pack_platform!(meta::Dict, p::AbstractPlatform)
    for (k, v) in tags(p)
        if v !== nothing
            meta[k] = v
        end
    end
    return meta
end

"""
    load_artifacts_toml(artifacts_toml::String;
                        pkg_uuid::Union{UUID,Nothing}=nothing)

Loads an `(Julia)Artifacts.toml` file from disk.  If `pkg_uuid` is set to the `UUID` of the
owning package, UUID/name overrides stored in a depot `Overrides.toml` will be resolved.
"""
function load_artifacts_toml(artifacts_toml::String;
                             pkg_uuid::Union{Base.UUID,Nothing} = nothing)
    artifact_dict = parse_toml(artifacts_toml)

    # Process overrides for this `pkg_uuid`
    process_overrides(artifact_dict, pkg_uuid)
    return artifact_dict
end

"""
    process_overrides(artifact_dict::Dict, pkg_uuid::Base.UUID)

When loading an `Artifacts.toml` file, we must check `Override.toml` files to see if any
of the artifacts within it have been overridden by UUID.  If they have, we honor the
overrides by inspecting the hashes of the targeted artifacts, then overriding them to
point to the given override, punting the actual redirection off to the hash-based
override system.  This does not modify the `artifact_dict` object, it merely dynamically
adds more hash-based overrides as `Artifacts.toml` files that are overridden are loaded.
"""
function process_overrides(artifact_dict::Dict, pkg_uuid::Base.UUID)
    # Insert just-in-time hash overrides by looking up the names of anything we need to
    # override for this UUID, and inserting new overrides for those hashes.
    overrides = load_overrides()
    if haskey(overrides[:UUID], pkg_uuid)
        pkg_overrides = overrides[:UUID][pkg_uuid]

        for name in keys(artifact_dict)
            # Skip names that we're not overriding
            if !haskey(pkg_overrides, name)
                continue
            end

            # If we've got a platform-specific friend, override all hashes:
            if isa(artifact_dict[name], Array)
                for entry in artifact_dict[name]
                    hash = SHA1(entry["git-tree-sha1"])
                    overrides[:hash][hash] = overrides[:UUID][pkg_uuid][name]
                end
            elseif isa(artifact_dict[name], Dict)
                hash = SHA1(artifact_dict[name]["git-tree-sha1"])
                overrides[:hash][hash] = overrides[:UUID][pkg_uuid][name]
            end
        end
    end
    return artifact_dict
end

# If someone tries to call process_overrides() with `nothing`, do exactly that
process_overrides(artifact_dict::Dict, pkg_uuid::Nothing) = nothing

"""
    artifact_meta(name::String, artifacts_toml::String;
                  platform::AbstractPlatform = HostPlatform(),
                  pkg_uuid::Union{Base.UUID,Nothing}=nothing)

Get metadata about a given artifact (identified by name) stored within the given
`(Julia)Artifacts.toml` file.  If the artifact is platform-specific, use `platform` to choose the
most appropriate mapping.  If none is found, return `nothing`.

!!! compat "Julia 1.3"
    This function requires at least Julia 1.3.
"""
function artifact_meta(name::String, artifacts_toml::String;
                       platform::AbstractPlatform = HostPlatform(),
                       pkg_uuid::Union{Base.UUID,Nothing}=nothing)
    if !isfile(artifacts_toml)
        return nothing
    end

    # Parse the toml of the artifacts_toml file
    artifact_dict = load_artifacts_toml(artifacts_toml; pkg_uuid=pkg_uuid)
    return artifact_meta(name, artifact_dict, artifacts_toml; platform=platform)
end

function artifact_meta(name::String, artifact_dict::Dict, artifacts_toml::String;
                       platform::AbstractPlatform = HostPlatform())
    if !haskey(artifact_dict, name)
        return nothing
    end
    meta = artifact_dict[name]

    # If it's an array, find the entry that best matches our current platform
    if isa(meta, Vector)
        dl_dict = Dict{AbstractPlatform,Dict{String,Any}}()
        for x in meta
            x::Dict{String}
            dl_dict[unpack_platform(x, name, artifacts_toml)] = x
        end
        meta = select_platform(dl_dict, platform)
    # If it's NOT a dict, complain
    elseif !isa(meta, Dict)
        @error("Invalid artifacts file at $(artifacts_toml): artifact '$name' malformed, must be array or dict!")
        return nothing
    end

    # This is such a no-no, we are going to call it out right here, right now.
    if meta !== nothing && !haskey(meta, "git-tree-sha1")
        @error("Invalid artifacts file at $(artifacts_toml): artifact '$name' contains no `git-tree-sha1`!")
        return nothing
    end

    # Return the full meta-dict.
    return meta
end

"""
    artifact_hash(name::String, artifacts_toml::String;
                  platform::AbstractPlatform = HostPlatform())

Thin wrapper around `artifact_meta()` to return the hash of the specified, platform-
collapsed artifact.  Returns `nothing` if no mapping can be found.

!!! compat "Julia 1.3"
    This function requires at least Julia 1.3.
"""
function artifact_hash(name::String, artifacts_toml::String;
                       platform::AbstractPlatform = HostPlatform(),
                       pkg_uuid::Union{Base.UUID,Nothing}=nothing)
    meta = artifact_meta(name, artifacts_toml; platform=platform)
    if meta === nothing
        return nothing
    end

    return SHA1(meta["git-tree-sha1"])
end

function select_downloadable_artifacts(artifact_dict::Dict, artifacts_toml::String;
                                       platform::AbstractPlatform = HostPlatform(),
                                       pkg_uuid::Union{Nothing,Base.UUID} = nothing,
                                       include_lazy::Bool = false)
    artifacts = Dict{String,Any}()
    for name in keys(artifact_dict)
        # Get the metadata about this name for the requested platform
        meta = artifact_meta(name, artifact_dict, artifacts_toml; platform=platform)

        # If there are no instances of this name for the desired platform, skip it
        # Also skip if there's no `download` stanza (e.g. it's only a local artifact)
        # or if it's lazy and we're not explicitly looking for lazy artifacts.
        if meta === nothing || !haskey(meta, "download") || (get(meta, "lazy", false) && !include_lazy)
            continue
        end

        # Else, welcome it into the meta-fold
        artifacts[name] = meta
    end
    return artifacts
end

"""
    select_downloadable_artifacts(artifacts_toml::String;
                                  platform = HostPlatform,
                                  include_lazy = false,
                                  pkg_uuid = nothing)

Returns a dictionary where every entry is an artifact from the given `Artifacts.toml`
that should be downloaded for the requested platform.  Lazy artifacts are included if
`include_lazy` is set.
"""
function select_downloadable_artifacts(artifacts_toml::String;
                                       platform::AbstractPlatform = HostPlatform(),
                                       include_lazy::Bool = false,
                                       pkg_uuid::Union{Nothing,Base.UUID} = nothing)
    if !isfile(artifacts_toml)
        return Dict{String,Any}()
    end
    artifact_dict = load_artifacts_toml(artifacts_toml; pkg_uuid=pkg_uuid)
    return select_downloadable_artifacts(artifact_dict, artifacts_toml; platform, pkg_uuid, include_lazy)
end


"""
    find_artifacts_toml(path::String)

Given the path to a `.jl` file, (such as the one returned by `__source__.file` in a macro
context), find the `(Julia)Artifacts.toml` that is contained within the containing project (if it
exists), otherwise return `nothing`.

!!! compat "Julia 1.3"
    This function requires at least Julia 1.3.
"""
function find_artifacts_toml(path::String)
    if !isdir(path)
        path = dirname(path)
    end

    # Run until we hit the root directory.
    while dirname(path) != path
        for f in artifact_names
            artifacts_toml_path = joinpath(path, f)
            if isfile(artifacts_toml_path)
                return abspath(artifacts_toml_path)
            end
        end

        # Does a `(Julia)Project.toml` file exist here, in the absence of an Artifacts.toml?
        # If so, stop the search as we've probably hit the top-level of this package,
        # and we don't want to escape out into the larger filesystem.
        for f in Base.project_names
            if isfile(joinpath(path, f))
                return nothing
            end
        end

        # Move up a directory
        path = dirname(path)
    end

    # We never found anything, just return `nothing`
    return nothing
end

# We do this to avoid doing the `joinpath()` work if we don't have to, and also to
# avoid a trailing slash due to `joinpath()`'s habit of including one when the last
# argument is the empty string.
function jointail(dir, tail)
    if !isempty(tail)
        return joinpath(dir, tail)
    else
        return dir
    end
end

function _artifact_str(__module__, artifacts_toml, name, path_tail, artifact_dict, hash, platform, @nospecialize(lazyartifacts))
    if haskey(Base.module_keys, __module__)
        # Process overrides for this UUID, if we know what it is
        process_overrides(artifact_dict, Base.module_keys[__module__].uuid)
    end

    # If the artifact exists, we're in the happy path and we can immediately
    # return the path to the artifact:
    for dir in artifact_paths(hash; honor_overrides=true)
        if isdir(dir)
            return jointail(dir, path_tail)
        end
    end

    # If not, try determining what went wrong:
    meta = artifact_meta(name, artifact_dict, artifacts_toml; platform)
    if meta !== nothing && get(meta, "lazy", false)
        if lazyartifacts isa Module && isdefined(lazyartifacts, :ensure_artifact_installed)
            nameof(lazyartifacts) === :Pkg && Base.depwarn("using Pkg instead of using LazyArtifacts is deprecated", :var"@artifact_str", force=true)
            return jointail(lazyartifacts.ensure_artifact_installed(string(name), artifacts_toml; platform), path_tail)
        end
        error("Artifact $(repr(name)) is a lazy artifact; package developers must call `using LazyArtifacts` in $(__module__) before using lazy artifacts.")
    end
    error("Artifact $(repr(name)) was not installed correctly. Try `using Pkg; Pkg.instantiate()` to re-install all missing resources.")
end

"""
    split_artifact_slash(name::String)

Splits an artifact indexing string by path deliminters, isolates the first path element,
returning that and the `joinpath()` of the remaining arguments.  This normalizes all path
separators to the native path separator for the current platform.  Examples:

# Examples
```jldoctest
julia> split_artifact_slash("Foo")
("Foo", "")

julia> ret = split_artifact_slash("Foo/bar/baz.so");

julia> if Sys.iswindows()
            ret == ("Foo", "bar\\baz.so")
       else
            ret == ("Foo", "bar/baz.so")
       end
true

julia> ret = split_artifact_slash("Foo\\bar\\baz.so");

julia> if Sys.iswindows()
            ret == ("Foo", "bar\\baz.so")
       else
            ret == ("Foo", "bar/baz.so")
       end
true
```
"""
function split_artifact_slash(name::String)
    split_name = split(name, r"(/|\\)")
    if length(split_name) == 1
        return (split_name[1], "")
    else
        return (split_name[1], joinpath(split_name[2:end]...))
    end
end

"""
    artifact_slash_lookup(name::String, atifact_dict::Dict,
                          artifacts_toml::String, platform::Platform)

Returns `artifact_name`, `artifact_path_tail`, and `hash` by looking the results up in
the given `artifacts_toml`, first extracting the name and path tail from the given `name`
to support slash-indexing within the given artifact.
"""
function artifact_slash_lookup(name::String, artifact_dict::Dict,
                               artifacts_toml::String, platform::Platform)
    artifact_name, artifact_path_tail = split_artifact_slash(name)

    meta = artifact_meta(artifact_name, artifact_dict, artifacts_toml; platform)
    if meta === nothing
        error("Cannot locate artifact '$(name)' for $(triplet(platform)) in '$(artifacts_toml)'")
    end
    hash = SHA1(meta["git-tree-sha1"])
    return artifact_name, artifact_path_tail, hash
end

"""
    macro artifact_str(name)

Return the on-disk path to an artifact. Automatically looks the artifact up by
name in the project's `(Julia)Artifacts.toml` file. Throws an error on if the
requested artifact is not present. If run in the REPL, searches for the toml
file starting in the current directory, see `find_artifacts_toml()` for more.

If the artifact is marked "lazy" and the package has `using LazyArtifacts`
defined, the artifact will be downloaded on-demand with `Pkg` the first time
this macro tries to compute the path. The files will then be left installed
locally for later.

If `name` contains a forward or backward slash, all elements after the first slash will
be taken to be path names indexing into the artifact, allowing for an easy one-liner to
access a single file/directory within an artifact.  Example:

    ffmpeg_path = @artifact"FFMPEG/bin/ffmpeg"

!!! compat "Julia 1.3"
    This macro requires at least Julia 1.3.

!!! compat "Julia 1.6"
    Slash-indexing requires at least Julia 1.6.
"""
macro artifact_str(name, platform=nothing)
    # Find Artifacts.toml file we're going to load from
    srcfile = string(__source__.file)
    if ((isinteractive() && startswith(srcfile, "REPL[")) || (!isinteractive() && srcfile == "none")) && !isfile(srcfile)
        srcfile = pwd()
    end
    local artifacts_toml = find_artifacts_toml(srcfile)
    if artifacts_toml === nothing
        error(string(
            "Cannot locate '(Julia)Artifacts.toml' file when attempting to use artifact '",
            name,
            "' in '",
            __module__,
            "'",
        ))
    end

    # Load Artifacts.toml at compile time, so that we don't have to use `__source__.file`
    # at runtime, which gets stale if the `.ji` file is relocated.
    local artifact_dict = load_artifacts_toml(artifacts_toml)

    # Invalidate calling .ji file if Artifacts.toml file changes
    Base.include_dependency(artifacts_toml)

    # Check if the user has provided `LazyArtifacts`, and thus supports lazy artifacts
    lazyartifacts = isdefined(__module__, :LazyArtifacts) ? GlobalRef(__module__, :LazyArtifacts) : nothing
    if lazyartifacts === nothing && isdefined(__module__, :Pkg)
        lazyartifacts = GlobalRef(__module__, :Pkg) # deprecated
    end

    # If `name` is a constant, (and we're using the default `Platform`) we can actually load
    # and parse the `Artifacts.toml` file now, saving the work from runtime.
    if isa(name, AbstractString) && platform === nothing
        # To support slash-indexing, we need to split the artifact name from the path tail:
        platform = HostPlatform()
        artifact_name, artifact_path_tail, hash = artifact_slash_lookup(name, artifact_dict, artifacts_toml, platform)
        return quote
            Base.invokelatest(_artifact_str, $(__module__), $(artifacts_toml), $(artifact_name), $(artifact_path_tail), $(artifact_dict), $(hash), $(platform), $(lazyartifacts))::String
        end
    else
        if platform === nothing
            platform = :($(HostPlatform)())
        end
        return quote
            local platform = $(esc(platform))
            local artifact_name, artifact_path_tail, hash = artifact_slash_lookup($(esc(name)), $(artifact_dict), $(artifacts_toml), platform)
            Base.invokelatest(_artifact_str, $(__module__), $(artifacts_toml), artifact_name, artifact_path_tail, $(artifact_dict), hash, platform, $(lazyartifacts))::String
        end
    end
end

# Support `AbstractString`s, but avoid compilers needing to track backedges for callers
# of these functions in case a user defines a new type that is `<: AbstractString`
with_artifacts_directory(f::Function, artifacts_dir::AbstractString) =
    with_artifacts_directory(f, String(artifacts_dir)::String)
query_override(pkg::Base.UUID, artifact_name::AbstractString; overrides::Dict=load_overrides()) =
    query_override(pkg, String(artifact_name)::String; overrides=convert(Dict{Symbol, Any}(overrides)))
unpack_platform(entry::Dict, name::AbstractString, artifacts_toml::AbstractString) =
    unpack_platform(convert(Dict{String, Any}, entry), String(name)::String, String(artifacts_toml)::String)
load_artifacts_toml(artifacts_toml::AbstractString; kwargs...) =
    load_artifacts_toml(String(artifacts_toml)::String; kwargs...)
artifact_meta(name::AbstractString, artifacts_toml::AbstractString; kwargs...) =
    artifact_meta(String(name)::String, String(artifacts_toml)::String; kwargs...)
artifact_meta(name::AbstractString, artifact_dict::Dict, artifacts_toml::AbstractString; kwargs...) =
    artifact_meta(String(name)::String, artifact_dict, String(artifacts_toml)::String; kwargs...)
artifact_hash(name::AbstractString, artifacts_toml::AbstractString; kwargs...) =
    artifact_hash(String(name)::String, String(artifacts_toml)::String; kwargs...)
select_downloadable_artifacts(artifact_dict::Dict, artifacts_toml::AbstractString; kwargs...) =
    select_downloadable_artifacts(artifact_dict, String(artifacts_toml)::String, kwargs...)
select_downloadable_artifacts(artifacts_toml::AbstractString; kwargs...) =
    select_downloadable_artifacts(String(artifacts_toml)::String, kwargs...)
find_artifacts_toml(path::AbstractString) =
    find_artifacts_toml(String(path)::String)
split_artifact_slash(name::AbstractString) =
    split_artifact_slash(String(name)::String)
artifact_slash_lookup(name::AbstractString, artifact_dict::Dict, artifacts_toml::AbstractString) =
    artifact_slash_lookup(String(name)::String, artifact_dict, String(artifacts_toml)::String)

end # module Artifacts
