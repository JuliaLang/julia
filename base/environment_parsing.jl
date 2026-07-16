# This file is a part of Julia. License is MIT: https://julialang.org/license

# Environment representation and lookup for code loading. Each load-path entry becomes an
# `ExplicitEnv` or `ImplicitEnv`, bundled into an `EnvironmentStack`. Explicit environments
# are parsed eagerly; implicit package entries are parsed and memoized on demand.
#
# The manual's "Code Loading" chapter (doc/src/manual/code-loading.md) specifies package
# loading in terms of three maps defined by each environment: `roots`, `graph` and
# `paths`. The types here are the parsed, in-memory form of those maps; comments below
# note which map each structure or lookup corresponds to. `Precompilation` also uses
# `ExplicitEnv` to build its dependency graph.

const require_lock = ReentrantLock()

# Cross-platform case-sensitive path canonicalization

if Sys.isunix() && !Sys.isapple()
    # assume case-sensitive filesystems, don't have to do anything
    isfile_casesensitive(path) = isaccessiblefile(path)
elseif Sys.iswindows()
    # GetLongPathName Win32 function returns the case-preserved filename on NTFS.
    function isfile_casesensitive(path)
        isaccessiblefile(path) || return false  # Fail fast
        basename(Filesystem.longpath(path)) == basename(path)
    end
elseif Sys.isapple()
    # HFS+ filesystem is case-preserving. The getattrlist API returns
    # a case-preserved filename. In the rare event that HFS+ is operating
    # in case-sensitive mode, this will still work but will be redundant.

    # Constants from <sys/attr.h>
    const ATRATTR_BIT_MAP_COUNT = 5
    const ATTR_CMN_NAME = 1
    const BITMAPCOUNT = 1
    const COMMONATTR = 5
    const FSOPT_NOFOLLOW = 1  # Don't follow symbolic links

    const attr_list = zeros(UInt8, 24)
    attr_list[BITMAPCOUNT] = ATRATTR_BIT_MAP_COUNT
    attr_list[COMMONATTR] = ATTR_CMN_NAME

    # This essentially corresponds to the following C code:
    # attrlist attr_list;
    # memset(&attr_list, 0, sizeof(attr_list));
    # attr_list.bitmapcount = ATTR_BIT_MAP_COUNT;
    # attr_list.commonattr = ATTR_CMN_NAME;
    # struct Buffer {
    #    u_int32_t total_length;
    #    u_int32_t filename_offset;
    #    u_int32_t filename_length;
    #    char filename[max_filename_length];
    # };
    # Buffer buf;
    # getattrpath(path, &attr_list, &buf, sizeof(buf), FSOPT_NOFOLLOW);
    function isfile_casesensitive(path)
        isaccessiblefile(path) || return false
        path_basename = String(basename(path))
        local casepreserved_basename
        header_size = 12
        buf = Vector{UInt8}(undef, length(path_basename) + header_size + 1)
        while true
            ret = ccall(:getattrlist, Cint,
                        (Cstring, Ptr{Cvoid}, Ptr{Cvoid}, Csize_t, Culong),
                        path, attr_list, buf, sizeof(buf), FSOPT_NOFOLLOW)
            systemerror(:getattrlist, ret ≠ 0)
            filename_length = GC.@preserve buf unsafe_load(
              convert(Ptr{UInt32}, pointer(buf) + 8))
            if (filename_length + header_size) > length(buf)
                resize!(buf, filename_length + header_size)
                continue
            end
            casepreserved_basename =
              view(buf, (header_size+1):(header_size+filename_length-1))
            break
        end
        # Hack to compensate for inability to create a string from a subarray with no allocations.
        codeunits(path_basename) == casepreserved_basename && return true

        # If there is no match, it's possible that the file does exist but HFS+
        # performed unicode normalization. See  https://developer.apple.com/library/mac/qa/qa1235/_index.html.
        isascii(path_basename) && return false
        codeunits(Unicode.normalize(path_basename, :NFD)) == casepreserved_basename
    end
else
    # Generic fallback that performs a slow directory listing.
    function isfile_casesensitive(path)
        isaccessiblefile(path) || return false
        dir, filename = splitdir(path)
        any(readdir(dir) .== filename)
    end
end

# Check if the file is accessible. If stat fails return `false`

function isaccessibledir(dir)
    return try
        isdir(dir)
    catch err
        err isa IOError || rethrow()
        false
    end
end

function isaccessiblefile(file)
    return try
        isfile(file)
    catch err
        err isa IOError || rethrow()
        false
    end
end

function isaccessiblepath(path)
    return try
        ispath(path)
    catch err
        err isa IOError || rethrow()
        false
    end
end

## SHA1 and SHA256 ##
for (name, namestr, numbytes) in [(:SHA1, "SHA1", 20), (:SHA256, "SHA256", 32)]
    @eval begin
        struct $name
            bytes::NTuple{$numbytes, UInt8}
        end
        function $name(bytes::Vector{UInt8})
            length(bytes) == $numbytes ||
                throw(ArgumentError("wrong number of bytes for " * string($namestr) * ": Expected " * string($numbytes) * " bytes, got $(length(bytes))"))
            return $name(ntuple(i->bytes[i], Val($numbytes)))
        end
        $name(s::AbstractString) = $name(hex2bytes(s))
        parse(::Type{$name}, s::AbstractString) = $name(s)
        function tryparse(::Type{$name}, s::AbstractString)
            try
                return parse($name, s)
            catch e
                if isa(e, ArgumentError)
                    return nothing
                end
                rethrow(e)
            end
        end

        string(hash::$name) = bytes2hex(hash.bytes)
        print(io::IO, hash::$name) = bytes2hex(io, hash.bytes)
        show(io::IO, hash::$name) = print(io, $namestr * "(\"", hash, "\")")

        isless(a::$name, b::$name) = isless(a.bytes, b.bytes)
        hash(a::$name, h::UInt) = hash(($name, a.bytes), h)
        ==(a::$name, b::$name) = a.bytes == b.bytes
    end
end

# fake uuid5 function (for self-assigned UUIDs)
# TODO: delete and use real uuid5 once it's in stdlib

function uuid5(namespace::UUID, key::String)
    u::UInt128 = 0
    h = hash(namespace)
    for _ = 1:sizeof(u)÷sizeof(h)
        u <<= sizeof(h) << 3
        u |= (h = hash(key, h))
    end
    u &= 0xffffffffffff0fff3fffffffffffffff
    u |= 0x00000000000050008000000000000000
    return UUID(u)
end

const ns_dummy_uuid = UUID("fe0723d6-3a44-4c41-8065-ee0f42c8ceab")

function dummy_uuid(project_file::String)
    @lock require_lock begin
    project_path = try
        realpath(project_file)
    catch ex
        ex isa IOError || rethrow()
        project_file
    end
    return uuid5(ns_dummy_uuid, project_path)
    end
end

## package path slugs: turning UUID + SHA1 into a pair of 5-byte "slugs" ##

const slug_chars = String(['A':'Z'; 'a':'z'; '0':'9'])

function slug(x::UInt32, p::Int)
    sprint(sizehint=p) do io
        y = x
        n = UInt32(length(slug_chars))
        for i = 1:p
            y, d = divrem(y, n)
            write(io, slug_chars[1+d])
        end
    end
end

function version_slug(uuid::UUID, sha1::SHA1, p::Int=5)
    crc = _crc32c(uuid)
    crc = _crc32c(sha1.bytes, crc)
    return slug(crc, p)
end

mutable struct CachedTOMLDict
    path::String
    inode::UInt64
    mtime::Float64
    size::Int64
    hash::UInt32
    d::Dict{String, Any}
end

function CachedTOMLDict(p::TOML.Parser, path::String)
    s = stat(path)
    content = read(path)
    crc32 = _crc32c(content)
    TOML.reinit!(p, String(content); filepath=path)
    d = TOML.parse(p)
    return CachedTOMLDict(
        path,
        s.inode,
        s.mtime,
        s.size,
        crc32,
        d,
   )
end

function get_updated_dict(p::TOML.Parser, f::CachedTOMLDict)
    s = stat(f.path)
    # note, this might miss very rapid in-place updates, such that mtime is
    # identical but that is solvable by not doing in-place updates, and not
    # rapidly changing these files
    if s.inode != f.inode || s.mtime != f.mtime || f.size != s.size
        content = read(f.path)
        new_hash = _crc32c(content)
        if new_hash != f.hash
            f.inode = s.inode
            f.mtime = s.mtime
            f.size = s.size
            f.hash = new_hash
            TOML.reinit!(p, String(content); filepath=f.path)
            return f.d = TOML.parse(p)
        end
    end
    return f.d
end

"""
    struct PkgLoadSpec

A PkgLoadSpec is the result of a `locate_package` operation and specifies how
and wherefrom to load a julia package.
"""
struct PkgLoadSpec
    path::String
    julia_syntax_version::VersionNumber
end

struct TOMLCache{Dates}
    p::TOML.Parser{Dates}
    d::Dict{String, CachedTOMLDict}
end
TOMLCache(p::TOML.Parser) = TOMLCache(p, Dict{String, CachedTOMLDict}())
TOMLCache(p::TOML.Parser, d::Dict{String, Dict{String, Any}}) = TOMLCache(p, convert(Dict{String, CachedTOMLDict}, d))

const TOML_CACHE = TOMLCache(TOML.Parser{nothing}())

# Precompile workers have an immutable environment; non-incremental output may not.
_env_frozen() = generating_output(#=incremental=#true)

# TOML files parsed by this process while its environment is frozen.
const _frozen_parsed = Set{String}()
parsed_toml(project_file::AbstractString) = parsed_toml(project_file, TOML_CACHE, require_lock)
function parsed_toml(project_file::AbstractString, toml_cache::TOMLCache, toml_lock::ReentrantLock)
    lock(toml_lock) do
        if !haskey(toml_cache.d, project_file)
            d = CachedTOMLDict(toml_cache.p, project_file)
            toml_cache.d[project_file] = d
            _env_frozen() && push!(_frozen_parsed, project_file)
            return d.d
        else
            d = toml_cache.d[project_file]
            # Frozen files parsed in this process need no freshness `stat`.
            if _env_frozen() && project_file in _frozen_parsed
                return d.d
            end
            dd = get_updated_dict(toml_cache.p, d)
            _env_frozen() && push!(_frozen_parsed, project_file)
            return dd
        end
    end
end

## generic project & manifest API ##

const project_names = ("JuliaProject.toml", "Project.toml")
const manifest_names = (
    "JuliaManifest-v$(VERSION.major).$(VERSION.minor).toml",
    "Manifest-v$(VERSION.major).$(VERSION.minor).toml",
    "JuliaManifest.toml",
    "Manifest.toml",
)
const preferences_names = ("JuliaLocalPreferences.toml", "LocalPreferences.toml")

function locate_project_file(env::String)
    for proj in project_names
        project_file = joinpath(env, proj)
        if isfile_casesensitive(project_file)
            return project_file
        end
    end
    return true
end

# classify the LOAD_PATH entry to be one of:
#  - `false`: nonexistent / nothing to see here
#  - `true`: `env` is an implicit environment
#  - `path`: the path of an explicit project file
# Cache the classification when the environment is frozen.
const _frozen_env_project_file = Dict{String, Union{Bool, String}}()
function env_project_file(env::String)::Union{Bool,String}
    @lock require_lock begin
    if _env_frozen()
        cached = get(_frozen_env_project_file, env, nothing)
        cached === nothing || return cached
    end
    if isdir(env)
        project_file = locate_project_file(env)
    elseif basename(env) in project_names && isfile_casesensitive(env)
        project_file = env
    else
        project_file = false
    end
    _env_frozen() && (_frozen_env_project_file[env] = project_file)
    return project_file
    end
end

function base_project(project_file)
    home_dir = abspath(homedir())
    project_dir = abspath(dirname(project_file))
    current_dir = project_dir
    # Only stop at home boundary if we started under home
    started_in_home = startswith(project_dir, home_dir)

    while true
        parent_dir = dirname(current_dir)
        # Stop if we've reached root
        if parent_dir == current_dir
            return nothing
        end
        # Stop if we started in home and have now left it
        if started_in_home && !startswith(parent_dir, home_dir)
            return nothing
        end

        base_project_file = env_project_file(parent_dir)
        if base_project_file isa String
            d = parsed_toml(base_project_file)
            workspace = get(d, "workspace", nothing)::Union{Dict{String, Any}, Nothing}
            if workspace !== nothing
                projects = get(workspace, "projects", nothing)::Union{Vector{String}, Nothing, String}
                if projects isa Vector
                    # Check if any project in the workspace matches the original project
                    workspace_root = dirname(base_project_file)
                    for project in projects
                        project_path = joinpath(workspace_root, project)
                        if isdir(project_path)
                            if samefile(project_path, project_dir)
                                return base_project_file
                            end
                        end
                    end
                end
            end
        end
        current_dir = parent_dir
    end
end

const NON_VERSIONED_SYNTAX = v"1.13"

# Parse the `julia_version` out of a `[syntax]` table (from a project or manifest entry),
# returning `nothing` when the table is absent or does not specify a `julia_version`.
function syntax_table_version(syntax_table)::Union{Nothing, VersionNumber}
    syntax_table === nothing && return nothing
    jv = get(syntax_table, "julia_version", nothing)
    jv === nothing && return nothing
    return VersionNumber(jv)
end

function project_get_syntax_version(d::Dict)
    # Syntax Evolution. First check syntax.julia_version entry
    ds = get(d, "syntax", nothing)
    sv = syntax_table_version(ds)
    # If not found, default to minimum(compat["julia"])
    if sv === nothing
        cs = get(d, "compat", nothing)
        if cs !== nothing
            jv = get(cs, "julia", nothing)
            if jv !== nothing
                sv = VersionNumber(minimum(semver_spec(jv)).t...)
            end
        end
    end
    # Finally, if neither of those are set, default to the current Julia version.
    # N.B.: This choice is less "compatible" than defaulting to a fixed older version.
    # However, it avoids surprises from moving over scripts and REPL code to packages
    if sv === nothing
        sv = VERSION
    elseif sv <= NON_VERSIONED_SYNTAX
        # Syntax versioning was first introduced in Julia 1.14 - we do not support
        # going back to versions before syntax version 1.13.
        sv = NON_VERSIONED_SYNTAX
    end
    return sv
end

# find project file's corresponding manifest file
const _frozen_manifest_path = Dict{String, Union{Nothing, String}}() # cached in precompile workers
function project_file_manifest_path(project_file::String)::Union{Nothing,String}
    @lock require_lock begin
    if _env_frozen()
        cached = get(_frozen_manifest_path, project_file, missing)
        cached === missing || return cached
    end
    manifest_path = _project_file_manifest_path(project_file)
    _env_frozen() && (_frozen_manifest_path[project_file] = manifest_path)
    return manifest_path
    end
end
function _project_file_manifest_path(project_file::String)::Union{Nothing,String}
    dir = abspath(dirname(project_file))
    isfile_casesensitive(project_file) || return nothing
    d = parsed_toml(project_file)
    base = base_project(project_file)
    base_manifest = base === nothing ? nothing : project_file_manifest_path(base)
    if base_manifest !== nothing
        return base_manifest
    end
    explicit_manifest = get(d, "manifest", nothing)::Union{String, Nothing}
    manifest_path = nothing
    if explicit_manifest !== nothing
        manifest_file = normpath(joinpath(dir, explicit_manifest))
        if isfile_casesensitive(manifest_file)
            manifest_path = manifest_file
        end
    end
    if manifest_path === nothing
        for mfst in manifest_names
            manifest_file = joinpath(dir, mfst)
            if isfile_casesensitive(manifest_file)
                manifest_path = manifest_file
                break
            end
        end
    end
    return manifest_path
end

#########################
# Explicit environments #
#########################

function is_v1_format_manifest(raw_manifest::Dict{String})
    haskey(raw_manifest, "manifest_format") || return true
    manifest_format = raw_manifest["manifest_format"]
    return manifest_format isa Dict{String} && haskey(manifest_format, "uuid")
end

function get_deps(raw_manifest::Dict)
    is_v1_format_manifest(raw_manifest) && return raw_manifest
    return get(Dict{String, Any}, raw_manifest, "deps")::Dict{String, Any}
end

# In terms of the manual's maps: `project_deps` is the environment's "roots" map, `deps`
# (together with `weakdeps`/`extensions`) its "graph", and `lookup_strategy` plus
# `entryfile` the data from which its "paths" map entries are computed.
struct ExplicitEnv
    path::String
    manifest_path::Union{Nothing, String}
    manifest_julia_version::Union{Nothing, String}
    project_name::Union{Nothing, String}
    project_uuid::Union{Nothing, UUID}
    project_deps::Dict{String, UUID}     # [deps] in the active project's Project.toml
    project_weakdeps::Dict{String, UUID} # [weakdeps] in the active project's Project.toml
    project_extras::Dict{String, UUID}   # [extras] in the active project's Project.toml
    project_prefs::Dict{String, Any}     # [preferences] in the active project's Project.toml
                                         # (see "Package/Environment Preferences" in the manual)
    workspace_deps::Dict{String, UUID}   # union of [deps] from all workspace member Project.tomls
    deps::Dict{UUID, Vector{UUID}}       # full dependency graph from Manifest.toml
    weakdeps::Dict{UUID, Vector{UUID}}   # full weak dependency graph from Manifest.toml
    extensions::Dict{UUID, Dict{String, Vector{UUID}}}
    extension_parents::Dict{PkgId, UUID}
    names::Dict{UUID, String}
    lookup_strategy::Dict{UUID, Union{
                                      SHA1,     # `git-tree-sha1` entry
                                      String,   # `path` entry
                                      Nothing,  # stdlib (no `path` nor `git-tree-sha1`)
                                      Missing}} # not present in the manifest
    entryfile::Dict{UUID, Union{Nothing, String}}  # `entryfile`/`path` entry within a package
    syntax_version::Dict{UUID, VersionNumber}      # resolved syntax version for each package
end

ExplicitEnv() = ExplicitEnv(active_project())
function ExplicitEnv(::Nothing, envpath::String="")
    ExplicitEnv(envpath,
        nothing,                  # manifest_path
        nothing,                  # manifest_julia_version
        nothing,                  # project_name
        nothing,                  # project_uuid
        Dict{String, UUID}(),     # project_deps
        Dict{String, UUID}(),     # project_weakdeps
        Dict{String, UUID}(),     # project_extras
        Dict{String, Any}(),      # project_prefs
        Dict{String, UUID}(),     # workspace_deps
        Dict{UUID, Vector{UUID}}(),   # deps
        Dict{UUID, Vector{UUID}}(),   # weakdeps
        Dict{UUID, Dict{String, Vector{UUID}}}(), # extensions
        Dict{PkgId, UUID}(),         # extension_parents
        Dict{UUID, String}(),     # names
        Dict{UUID, Union{SHA1, String, Nothing, Missing}}(), # lookup_strategy
        Dict{UUID, Union{Nothing, String}}(),  # entryfile
        Dict{UUID, VersionNumber}(),           # syntax_version
    )
end
# Only precompilation needs `workspace_deps`, so code loading passes `workspace=false`.
function ExplicitEnv(envpath::String; workspace::Bool=true)
    if !isfile(envpath)
        envpath = abspath(envpath)
        return ExplicitEnv(nothing, envpath)
    end
    envpath = abspath(envpath)
    project_d = parsed_toml(envpath)

    # TODO: Perhaps verify that two packages with the same UUID do not have different names?
    names = Dict{UUID, String}()
    project_name_to_uuid = Dict{String, UUID}()

    project_deps = Dict{String, UUID}()
    project_weakdeps = Dict{String, UUID}()
    project_extras = Dict{String, UUID}()

    for (key, section) in (("deps", project_deps),
                           ("weakdeps", project_weakdeps),
                           ("extras", project_extras))
        for (name, _uuid) in get(Dict{String, Any}, project_d, key)::Dict{String, Any}
            uuid = UUID(_uuid::String)
            section[name] = uuid
            names[uuid] = name
            key == "extras" || (project_name_to_uuid[name] = uuid)
        end
    end

    # A package in both deps and weakdeps is in fact only a weakdep
    for name in keys(project_weakdeps)
        delete!(project_deps, name)
    end

    # A named project is itself a root; without a UUID it gets the usual path-based dummy.
    proj_name = get(project_d, "name", nothing)::Union{String, Nothing}
    _proj_uuid = get(project_d, "uuid", nothing)::Union{String, Nothing}
    proj_uuid = _proj_uuid !== nothing ? UUID(_proj_uuid) :
                proj_name !== nothing ? dummy_uuid(envpath) : nothing

    if proj_name !== nothing
        project_deps[proj_name] = proj_uuid
        names[proj_uuid] = proj_name
    end

    project_extensions = Dict{String, Vector{UUID}}()
    for (name, triggers) in get(Dict{String, Any}, project_d, "extensions")::Dict{String, Any}
        triggers = triggers isa String ? String[triggers] : triggers::Vector{String}
        uuids = UUID[]
        for trigger in triggers
            uuid = get(project_name_to_uuid, trigger, nothing)
            if uuid === nothing
                error("Trigger $trigger for extension $name not found in project")
            end
            push!(uuids, uuid)
        end
        project_extensions[name] = uuids
    end

    project_prefs = get(Dict{String, Any}, project_d, "preferences")::Dict{String, Any}

    manifest = project_file_manifest_path(envpath)
    manifest_d = manifest === nothing ? Dict{String, Any}() : parsed_toml(manifest)
    _manifest_julia_version = get(manifest_d, "julia_version", nothing)
    manifest_julia_version = _manifest_julia_version isa String ? _manifest_julia_version : nothing

    # Dependencies in a manifest can either be stored compressed (when name is unique among all packages)
    # in which case it is a `Vector{String}` or expanded where it is a `name => uuid` mapping.
    deps = Dict{UUID, Union{Vector{String}, Vector{UUID}}}()
    weakdeps = Dict{UUID, Union{Vector{String}, Vector{UUID}}}()
    extensions = Dict{UUID, Dict{String, Vector{String}}}()
    name_to_uuid = Dict{String, UUID}()
    ambiguous_names = Set{String}() # names shared by more than one manifest entry
    lookup_strategy = Dict{UUID, Union{SHA1, String, Nothing, Missing}}()
    entryfile = Dict{UUID, Union{Nothing, String}}()
    syntax_version = Dict{UUID, VersionNumber}()

    sizehint!(deps, length(manifest_d))
    sizehint!(weakdeps, length(manifest_d))
    sizehint!(extensions, length(manifest_d))
    sizehint!(name_to_uuid, length(manifest_d))
    sizehint!(lookup_strategy, length(manifest_d))

    for (name, pkg_infos) in get_deps(manifest_d)
        for pkg_info in pkg_infos::Vector{Any}
            pkg_info = pkg_info::Dict{String, Any}
            m_uuid = UUID(pkg_info["uuid"]::String)

            # If we have multiple packages with the same name we overwrite things here, but
            # that is fine since compressed (name-only) dependency lists are only written by
            # Pkg for names that are unique among all packages. A compressed reference to an
            # ambiguous name is caught when expanding the dependency lists below.
            if haskey(name_to_uuid, name) && name_to_uuid[name] != m_uuid
                push!(ambiguous_names, name)
            end
            names[m_uuid] = name
            name_to_uuid[name] = m_uuid

            for (key, section) in (("deps", deps), ("weakdeps", weakdeps))
                deps_pkg = get(Vector{String}, pkg_info, key)::Union{Vector{String}, Dict{String, Any}}

                if deps_pkg isa Vector{String}
                    section[m_uuid] = deps_pkg
                else
                    uuids = UUID[]
                    for (name_dep, _dep_uuid) in deps_pkg
                        dep_uuid = UUID(_dep_uuid::String)
                        push!(uuids, dep_uuid)
                        names[dep_uuid] = name_dep
                    end
                    section[m_uuid] = uuids
                end
            end

            deps_pkg = get(Dict{String, Any}, pkg_info, "extensions")::Dict{String, Any}
            deps_pkg_concrete = Dict{String, Vector{String}}()
            for (ext, triggers) in deps_pkg
                triggers = triggers isa String ? String[triggers] : triggers::Vector{String}
                deps_pkg_concrete[ext] = triggers
            end
            extensions[m_uuid] = deps_pkg_concrete

            lookup_strat = begin
                if (path = get(pkg_info, "path", nothing)::Union{String, Nothing}) !== nothing
                    path
                elseif (git_tree_sha_str = get(pkg_info, "git-tree-sha1", nothing)::Union{String, Nothing}) !== nothing
                    SHA1(git_tree_sha_str)
                else
                    nothing
                end
            end
            lookup_strategy[m_uuid] = lookup_strat
            entryfile[m_uuid] = get(pkg_info, "entryfile", nothing)::Union{String, Nothing}

            # Old manifests without syntax metadata use NON_VERSIONED_SYNTAX.
            sv = syntax_table_version(get(pkg_info, "syntax", nothing))
            if sv === nothing || sv <= NON_VERSIONED_SYNTAX
                sv = NON_VERSIONED_SYNTAX
            end
            syntax_version[m_uuid] = sv
        end
    end

    # Expand name-only manifest dependency lists to UUIDs.
    deps_expanded = Dict{UUID, Vector{UUID}}()
    weakdeps_expanded = Dict{UUID, Vector{UUID}}()
    extensions_expanded = Dict{UUID, Dict{String, Vector{UUID}}}()
    extension_parents = Dict{PkgId, UUID}()
    sizehint!(deps_expanded, length(deps))
    sizehint!(weakdeps_expanded, length(deps))
    sizehint!(extensions_expanded, length(deps))

    project_is_package = proj_name !== nothing && proj_uuid !== nothing
    if project_is_package
        deps_expanded[proj_uuid] = filter!(!=(proj_uuid), collect(values(project_deps)))
        weakdeps_expanded[proj_uuid] = collect(values(project_weakdeps))
        extensions_expanded[proj_uuid] = project_extensions
        # For the project-as-package, the package root is the directory containing the
        # project file, and the entry file comes from `path` (soft-deprecated) or `entryfile`.
        entryfile_proj = get(project_d, "path", nothing)::Union{String, Nothing}
        if entryfile_proj === nothing
            entryfile_proj = get(project_d, "entryfile", nothing)::Union{String, Nothing}
        end
        lookup_strategy[proj_uuid] = dirname(envpath)
        entryfile[proj_uuid] = entryfile_proj
        syntax_version[proj_uuid] = project_get_syntax_version(project_d)
    end

    for (section, expanded) in ((deps, deps_expanded), (weakdeps, weakdeps_expanded))
        for (pkg, pkg_deps) in section
            # A manifest can contain an entry for the root project itself (recorded with
            # `path = "."` and no dependency list). Its `[deps]`/`[weakdeps]` in the project
            # file are authoritative, so don't let that entry clobber them (set above).
            project_is_package && pkg == proj_uuid && continue
            if pkg_deps isa Vector{UUID}
                expanded[pkg] = pkg_deps
                for dep in pkg_deps
                    name_to_uuid[names[dep]] = dep
                end
            else
                deps_pkg = UUID[]
                sizehint!(deps_pkg, length(pkg_deps))
                for dep in pkg_deps
                    dep in ambiguous_names &&
                        error("expected a single entry for $(repr(dep)) in manifest $(repr(manifest))")
                    push!(deps_pkg, name_to_uuid[dep])
                end
                expanded[pkg] = deps_pkg
            end
        end
    end

    for (pkg, exts) in extensions
        # As above, keep the project file authoritative for the root project's extensions.
        project_is_package && pkg == proj_uuid && continue
        exts_expanded = Dict{String, Vector{UUID}}()
        for (ext, triggers) in exts
            triggers_expanded = UUID[]
            sizehint!(triggers_expanded, length(triggers))
            for trigger in triggers
                trigger_uuid = nothing
                for pkg_deps in (get(Vector{UUID}, deps_expanded, pkg),
                                 get(Vector{UUID}, weakdeps_expanded, pkg))
                    for dep_uuid in pkg_deps
                        if names[dep_uuid] == trigger
                            trigger_uuid = dep_uuid
                            break
                        end
                    end
                    trigger_uuid === nothing || break
                end
                trigger_uuid === nothing &&
                    error("Trigger $trigger for extension $ext not found in package dependencies")
                push!(triggers_expanded, trigger_uuid::UUID)
            end
            exts_expanded[ext] = triggers_expanded
        end
        extensions_expanded[pkg] = exts_expanded
    end
    for (parent, exts) in extensions_expanded
        for ext in keys(exts)
            extension_parents[PkgId(uuid5(parent, ext), ext)] = parent
        end
    end

    # Everything that does not yet have a lookup_strategy is missing from the manifest
    for uuid in values(project_deps)
        get!(lookup_strategy, uuid, missing)
    end

    # See "Workspaces" in the manual.
    workspace_deps = copy(project_deps)
    base = workspace ? base_project(envpath) : nothing
    if base !== nothing
        base_d = parsed_toml(base)

        # Add deps from the workspace root project
        for (name, _uuid) in get(Dict{String, Any}, base_d, "deps")::Dict{String, Any}
            workspace_deps[name] = UUID(_uuid::String)
        end

        # Add deps from each workspace member project
        ws = get(base_d, "workspace", nothing)::Union{Dict{String, Any}, Nothing}
        if ws !== nothing
            ws_projects = get(ws, "projects", nothing)::Union{Vector{String}, Nothing, String}
            if ws_projects isa Vector
                ws_root = dirname(base)
                for ws_proj in ws_projects
                    ws_proj_dir = joinpath(ws_root, ws_proj)
                    ws_proj_file = env_project_file(ws_proj_dir)
                    ws_proj_file isa String || continue
                    ws_d = parsed_toml(ws_proj_file)
                    for (name, _uuid) in get(Dict{String, Any}, ws_d, "deps")::Dict{String, Any}
                        workspace_deps[name] = UUID(_uuid::String)
                    end
                end
            end
        end
    end

    return ExplicitEnv(envpath, manifest, manifest_julia_version, proj_name, proj_uuid,
                       project_deps, project_weakdeps, project_extras,
                       project_prefs, workspace_deps,
                       deps_expanded, weakdeps_expanded, extensions_expanded, extension_parents,
                       names, lookup_strategy, entryfile, syntax_version)
end

#################
# Source layout #
#################

function entry_path(path::String, name::String, entryfile::Union{Nothing,String})::String
    isfile_casesensitive(path) && return normpath(path)
    entryfile === nothing && (entryfile = joinpath("src", "$name.jl"))
    return normpath(joinpath(path, entryfile))
end

function find_ext_path(project_path::String, extname::String)
    path = joinpath(project_path, "ext", extname, extname * ".jl")
    isfile_casesensitive(path) && return path
    return joinpath(project_path, "ext", extname * ".jl")
end

#########################
# Implicit environments #
#########################

function entry_point_and_project_file_inside(dir::String, name::String,
                                             allow_missing_source::Bool=false)::Union{Tuple{Nothing,Nothing},Tuple{String,Nothing},Tuple{String,String}}
    path = normpath(joinpath(dir, "src", "$name.jl"))
    source_exists = isfile_casesensitive(path)
    source_exists || allow_missing_source || return nothing, nothing
    for proj in project_names
        project_file = normpath(joinpath(dir, proj))
        isfile_casesensitive(project_file) || continue
        return path, project_file
    end
    return source_exists ? (path, nothing) : (nothing, nothing)
end

function entry_point_and_project_file(dir::String, name::String,
                                      allow_missing_source::Bool=false)::Union{Tuple{Nothing,Nothing},Tuple{String,Nothing},Tuple{String,String}}
    dir_name = joinpath(dir, name)
    path, project_file = entry_point_and_project_file_inside(dir_name, name)
    path === nothing || return path, project_file
    dir_jl = dir_name * ".jl"
    path, project_file = entry_point_and_project_file_inside(dir_jl, name)
    path === nothing || return path, project_file
    path = normpath(joinpath(dir, "$name.jl"))
    isfile_casesensitive(path) && return path, nothing
    if allow_missing_source
        path, project_file = entry_point_and_project_file_inside(dir_name, name, true)
        path === nothing || return path, project_file
        path, project_file = entry_point_and_project_file_inside(dir_jl, name, true)
        path === nothing || return path, project_file
    end
    return nothing, nothing
end

# A package defined inside an implicit environment (package directory).
struct ImplicitEnvPkg
    uuid::Union{Nothing, UUID}          # nothing for a bare `X.jl` or project-less `X/src/X.jl`
    path::String                        # entry-point file (absolute)
    project_file::Union{Nothing, String}
    # The following are only meaningful when `project_file !== nothing`:
    deps::Dict{String, UUID}            # [deps] of the package's own Project.toml
    weakdeps::Dict{String, UUID}        # [weakdeps]
    extensions::Dict{String, Vector{PkgId}}
    syntax_version::VersionNumber
end

# An implicit environment (or package directory) is a folder in the LOAD_PATH without a project file.
# A package X exists in a package directory if the directory contains one of the following
# "entry point" files: `X.jl`, `X/src/X.jl` or `X.jl/src/X.jl`. See "Package directories"
# in the manual; `implicit_env_pkg` applies its "roots map" UUID rules, with `nothing`
# standing in for the nil UUID of project-less packages.
struct ImplicitEnv
    path::String
    allow_missing_source::Bool
    names::Set{String}
    pkgs::Dict{String, Union{Nothing, ImplicitEnvPkg}}
    extension_parents::Dict{PkgId, String}
end

function implicit_env_pkg(env::ImplicitEnv, name::String)
    name in env.names || return nothing
    haskey(env.pkgs, name) && return env.pkgs[name]
    pkg = implicit_env_pkg(env.path, name, env.allow_missing_source)
    env.pkgs[name] = pkg
    if pkg !== nothing && pkg.uuid !== nothing
        for ext in keys(pkg.extensions)
            env.extension_parents[PkgId(uuid5(pkg.uuid, ext), ext)] = name
        end
    end
    return pkg
end

function implicit_env_pkg(envpath::String, name::String, allow_missing_source::Bool=false)
    path, project_file = entry_point_and_project_file(envpath, name, allow_missing_source)
    path === nothing && return nothing
    if project_file === nothing
        return ImplicitEnvPkg(nothing, path, nothing,
                              Dict{String, UUID}(), Dict{String, UUID}(),
                              Dict{String, Vector{PkgId}}(), VERSION)
    end
    d = parsed_toml(project_file)
    # The entry point file must belong to a package with a matching name
    get(d, "name", name)::String == name || return nothing
    _uuid = get(d, "uuid", nothing)::Union{String, Nothing}
    uuid = _uuid === nothing ? dummy_uuid(project_file) : UUID(_uuid)
    deps = Dict{String, UUID}()
    for (dname, duuid) in get(Dict{String, Any}, d, "deps")::Dict{String, Any}
        deps[dname] = UUID(duuid::String)
    end
    weakdeps = Dict{String, UUID}()
    for (dname, duuid) in get(Dict{String, Any}, d, "weakdeps")::Dict{String, Any}
        weakdeps[dname] = UUID(duuid::String)
    end
    exts = Dict{String, Vector{PkgId}}()
    for (ename, triggers) in get(Dict{String, Any}, d, "extensions")::Dict{String, Any}
        triggers = triggers isa String ? String[triggers] : triggers::Vector{String}
        trigger_ids = PkgId[]
        for trigger in triggers
            trigger_uuid = get(deps, trigger, nothing)
            trigger_uuid === nothing && (trigger_uuid = get(weakdeps, trigger, nothing))
            trigger_uuid === nothing &&
                error("Trigger $trigger for extension $ename not found in package dependencies")
            push!(trigger_ids, PkgId(trigger_uuid, trigger))
        end
        exts[ename] = trigger_ids
    end
    return ImplicitEnvPkg(uuid, path, project_file, deps, weakdeps, exts,
                          project_get_syntax_version(d))
end

function ImplicitEnv(envpath::String; allow_missing_source::Bool=false)
    envpath = abspath(envpath)
    names = Set{String}()
    entries = try
        readdir(envpath; sort=false)
    catch err
        err isa IOError || err isa SystemError || rethrow()
        String[]
    end
    for entry in entries
        name, ext = splitext(entry)
        ext == ".jl" || ext == "" || continue
        push!(names, name)
    end
    return ImplicitEnv(envpath, allow_missing_source, names,
                       Dict{String, Union{Nothing, ImplicitEnvPkg}}(),
                       Dict{PkgId, String}())
end

# Cache the session-constant stdlib directory index and its lazily parsed entries.
const _STDLIB_ENV = Ref{Union{Nothing, ImplicitEnv}}(nothing)
function stdlib_env()
    env = _STDLIB_ENV[]
    if env === nothing || env.path != abspath(Sys.STDLIB)
        env = ImplicitEnv(Sys.STDLIB; allow_missing_source=true)
        _STDLIB_ENV[] = env
    end
    return env
end
reset_stdlib_env() = (_STDLIB_ENV[] = nothing)

####################
# EnvironmentStack #
####################

# Queries walk the expanded `LOAD_PATH` in the order described by "Environment stacks".
struct EnvironmentStack
    load_path::Vector{String}
    roots::Vector{String}                          # raw load_path entry for each parsed env (parallel to `envs`)
    envs::Vector{Union{ImplicitEnv, ExplicitEnv}}
end

function EnvironmentStack(load_path::Vector{String} = load_path())
    roots = String[]
    envs = Union{ImplicitEnv, ExplicitEnv}[]
    for env in load_path
        project_file = env_project_file(env)
        if project_file isa String
            push!(envs, cached_explicit_env(project_file))
            push!(roots, env)
        elseif project_file === true
            if abspath(env) == abspath(Sys.STDLIB)
                push!(envs, stdlib_env())
            else
                push!(envs, ImplicitEnv(env))
            end
            push!(roots, env)
        end
    end
    return EnvironmentStack(load_path, roots, envs)
end

# Set under `require_lock` during `require`; other queries build a stack on demand.
const ENV_STACK = Ref{Union{Nothing, EnvironmentStack}}(nothing)
function current_env_stack()
    s = ENV_STACK[]
    s === nothing && return EnvironmentStack()
    return s
end

# Cache parsed explicit environments by project file. Invalidation uses the same
# `(inode, mtime, size)` signature as `TOML_CACHE`; the manifest path is re-resolved because
# workspace membership can depend on ancestor projects.
struct EnvStatSig
    inode::UInt64
    mtime::Float64
    size::Int64
end
EnvStatSig() = EnvStatSig(0, 0.0, 0)
function EnvStatSig(path::String)
    s = stat(path)
    return EnvStatSig(s.inode, s.mtime, s.size)
end

struct CachedExplicitEnv
    env::ExplicitEnv
    project_sig::EnvStatSig
    manifest_file::Union{Nothing, String}
    manifest_sig::EnvStatSig
end

const EXPLICIT_ENV_CACHE = Dict{String, CachedExplicitEnv}() # guarded by require_lock

function cached_explicit_env(project_file::String)
    assert_havelock(require_lock)
    project_file = abspath(project_file)
    project_sig = EnvStatSig(project_file)
    manifest_file = project_file_manifest_path(project_file)
    manifest_sig = manifest_file === nothing ? EnvStatSig() : EnvStatSig(manifest_file)
    c = get(EXPLICIT_ENV_CACHE, project_file, nothing)
    if c !== nothing && c.project_sig == project_sig &&
            c.manifest_file == manifest_file && c.manifest_sig == manifest_sig
        return c.env
    end
    env = ExplicitEnv(project_file; workspace=false)
    EXPLICIT_ENV_CACHE[project_file] = CachedExplicitEnv(env, project_sig, manifest_file, manifest_sig)
    return env
end

#################
# Lookup logic  #
#################

# The environment is authoritative for `where`, so stop searching.
const STOP = :stop

# Given an env-local dependency list of UUIDs and a target name, return the matching PkgId.
function _find_dep(env::ExplicitEnv, dep_uuids::Vector{UUID}, name::String)::Union{Nothing, PkgId}
    for uuid in dep_uuids
        get(env.names, uuid, nothing) == name && return PkgId(uuid, name)
    end
    return nothing
end

# Return an extension's parent UUID and triggers; see "Package Extensions".
function _extension_parent(env::ExplicitEnv, pkg::PkgId)::Union{Nothing, Tuple{UUID, Vector{UUID}}}
    parent = get(env.extension_parents, pkg, nothing)
    parent === nothing && return nothing
    return (parent, env.extensions[parent][pkg.name])
end

function implicit_extension_parent(env::ImplicitEnv, pname::String, pkg::PkgId)
    _, project_file = entry_point_and_project_file(env.path, pname, env.allow_missing_source)
    project_file === nothing && return false
    project = try
        parsed_toml(project_file)
    catch err
        err isa TOML.ParserError || err isa IOError || err isa SystemError || rethrow()
        return false
    end
    get(project, "name", pname) == pname || return false
    uuid_string = get(project, "uuid", nothing)
    uuid_string isa String || return false
    uuid = tryparse(UUID, uuid_string)
    uuid === nothing && return false
    extensions = get(project, "extensions", nothing)
    extensions isa Dict || return false
    return haskey(extensions, pkg.name) && uuid5(uuid, pkg.name) == pkg.uuid
end

function _extension_parent(env::ImplicitEnv, pkg::PkgId)::Union{Nothing, Tuple{String, ImplicitEnvPkg}}
    pname = get(env.extension_parents, pkg, nothing)
    if pname !== nothing
        return (pname, env.pkgs[pname]::ImplicitEnvPkg)
    end
    for pname in env.names
        implicit_extension_parent(env, pname, pkg) || continue
        parent = implicit_env_pkg(env, pname)
        parent === nothing || return (pname, parent)
    end
    return nothing
end

## Identify ##
#
# Implements the manual's identity resolution: the `(env, name)` methods answer the
# `roots[name]` lookup and the `(env, where, name)` methods answer `graph[where.uuid][name]`.

function _identify_package(env::ExplicitEnv, name::String)::Union{Nothing, PkgId}
    uuid = get(env.project_deps, name, nothing)
    uuid === nothing && return nothing
    return PkgId(uuid, name)
end

function _identify_package(env::ImplicitEnv, name::String)::Union{Nothing, PkgId}
    pkg = implicit_env_pkg(env, name)
    pkg === nothing && return nothing
    return PkgId(pkg.uuid, name)
end

function _identify_package(env::ExplicitEnv, where::PkgId, name::String)::Union{Nothing, PkgId, Symbol}
    where_deps = get(env.deps, where.uuid, nothing)
    if where_deps !== nothing
        # Known packages may load themselves and their declared dependencies.
        where.name == name && return where
        pkg = _find_dep(env, where_deps, name)
        pkg !== nothing && return pkg
        # Found `where` but it does not declare `name` - stop searching.
        return STOP
    end
    ext = _extension_parent(env, where)
    if ext !== nothing
        parent_uuid, triggers = ext

        # Extension loading its parent package
        if get(env.names, parent_uuid, nothing) == name
            return PkgId(parent_uuid, name)
        end

        # Extensions inherit strong deps and their declared trigger weakdeps.
        parent_deps = get(env.deps, parent_uuid, nothing)
        if parent_deps !== nothing
            pkg = _find_dep(env, parent_deps, name)
            pkg !== nothing && return pkg
        end

        # ...and any weakdep of the parent that is a trigger of this extension
        parent_weakdeps = get(env.weakdeps, parent_uuid, nothing)
        if parent_weakdeps !== nothing
            pkg = _find_dep(env, parent_weakdeps, name)
            if pkg !== nothing && pkg.uuid in triggers
                return pkg
            end
        end
        return STOP
    end
    return nothing
end

function _identify_package(env::ImplicitEnv, where::PkgId, name::String)::Union{Nothing, PkgId, Symbol}
    where_pkg = implicit_env_pkg(env, where.name)
    if where_pkg !== nothing && where_pkg.uuid == where.uuid
        where.name == name && return where
        uuid = get(where_pkg.deps, name, nothing)
        uuid !== nothing && return PkgId(uuid, name)
        # An implicit environment containing `where` is authoritative for its dependencies.
        return STOP
    end
    ext = _extension_parent(env, where)
    if ext !== nothing
        pname, ppkg = ext
        pname == name && return PkgId(ppkg.uuid, name)
        uuid = get(ppkg.deps, name, nothing)
        uuid !== nothing && return PkgId(uuid, name)
        triggers = ppkg.extensions[where.name]
        wuuid = get(ppkg.weakdeps, name, nothing)
        wuuid !== nothing && PkgId(wuuid, name) in triggers && return PkgId(wuuid, name)
        return STOP
    end
    return nothing
end

# Also return the load-path root used as `stopenv` by `locate`.
function _identify_package_env(envstack::EnvironmentStack, where::PkgId, name::String)::Union{Nothing, Tuple{PkgId, Union{String, Nothing}}}
    where.name == name && return (where, nothing)
    where.uuid === nothing && return _identify_package_env(envstack, name)
    found = nothing
    for i in eachindex(envstack.envs)
        env = envstack.envs[i]
        pkg = _identify_package(env, where, name)
        if pkg === STOP
            break
        elseif pkg isa PkgId
            found = (pkg, envstack.roots[i])
            break
        end
    end
    # Manifests from another Julia version may omit a current stdlib dependency.
    if found === nothing && is_stdlib(where)
        pkg = _identify_package(stdlib_env(), where, name)
        if pkg isa PkgId
            return (pkg, Sys.STDLIB)
        end
    end
    return found
end

function _identify_package_env(envstack::EnvironmentStack, name::String)::Union{Nothing, Tuple{PkgId, String}}
    for i in eachindex(envstack.envs)
        pkg = _identify_package(envstack.envs[i], name)
        pkg !== nothing && return (pkg, envstack.roots[i])
    end
    return nothing
end

## Locate ##
#
# Implements the manual's `paths[(uuid, name)]` map. `_logical_source` returns the source
# owner (`pkg` or its extension parent), or `nothing` when the environment has no source
# information for it. `reify` interprets the owner's environment metadata, returning
# `missing` when the environment pins a source that is not installed in any depot.

function find_depot_package_root(pkg::PkgId, hash::SHA1)::Union{Nothing, String}
    uuid = pkg.uuid::UUID
    for slug in (version_slug(uuid, hash), version_slug(uuid, hash, 4))
        for depot in DEPOT_PATH
            path = joinpath(depot, "packages", pkg.name, slug)
            ispath(path) && return abspath(path)
        end
    end
    return nothing
end

function _logical_source(env::ExplicitEnv, pkg::PkgId)::Union{Nothing, PkgId}
    source = if get(env.names, pkg.uuid, nothing) == pkg.name &&
            haskey(env.lookup_strategy, pkg.uuid)
        pkg
    else
        ext = _extension_parent(env, pkg)
        ext === nothing && return nothing
        parent_uuid, _ = ext
        PkgId(parent_uuid, env.names[parent_uuid])
    end
    haskey(env.lookup_strategy, source.uuid) || return nothing
    # A `Missing` strategy is a project dep without a manifest entry (e.g. no manifest at
    # all). The environment has no source information for it, but ones further down the
    # stack may, so skip this environment rather than stopping the search.
    env.lookup_strategy[source.uuid] isa Missing && return nothing
    return source
end

function _logical_source(env::ImplicitEnv, pkg::PkgId)::Union{Nothing, PkgId}
    p = implicit_env_pkg(env, pkg.name)
    if p !== nothing && p.uuid == pkg.uuid
        return pkg
    end
    ext = _extension_parent(env, pkg)
    ext === nothing && return nothing
    pname, ppkg = ext
    return PkgId(ppkg.uuid, pname)
end

function reify(env::ExplicitEnv, pkg::PkgId, source::PkgId)::Union{Nothing, Missing, PkgLoadSpec}
    source_uuid = source.uuid::UUID
    strategy = env.lookup_strategy[source_uuid]
    strategy isa Missing && return missing
    if strategy isa Nothing
        # The stdlib environment owns its source and extension metadata.
        return reify(stdlib_env(), pkg, source)
    end

    root = if strategy isa String
        # The project root is absolute; manifest paths are relative to the manifest.
        source_uuid == env.project_uuid ? strategy :
            normpath(abspath(dirname(env.manifest_path::String), strategy))
    else
        strategy = strategy::SHA1
        resolved = find_depot_package_root(source, strategy)
        resolved === nothing && return missing
        resolved
    end

    sv = get(env.syntax_version, source_uuid, VERSION)
    entry = if source == pkg
        entryfile = get(env.entryfile, source_uuid, nothing)
        entry_path(root, pkg.name, entryfile)
    else
        find_ext_path(root, pkg.name)
    end
    return PkgLoadSpec(entry, sv)
end

function reify(env::ImplicitEnv, pkg::PkgId, source::PkgId)::Union{Nothing, PkgLoadSpec}
    p = implicit_env_pkg(env, source.name)
    p === nothing && return nothing
    p.uuid == source.uuid || return nothing
    if source == pkg
        return PkgLoadSpec(p.path, p.syntax_version)
    end
    haskey(p.extensions, pkg.name) || return nothing
    uuid5(source.uuid::UUID, pkg.name) == pkg.uuid || return nothing
    root = dirname(dirname(p.path)) # <root>/src/<name>.jl -> <root>
    return PkgLoadSpec(find_ext_path(root, pkg.name), p.syntax_version)
end

function reify_stdlib(env::ImplicitEnv, pkg::PkgId, parent::PkgId)::PkgLoadSpec
    spec = reify(env, pkg, parent)
    spec isa PkgLoadSpec && return spec
    root = normpath(env.path, parent.name)
    path = pkg == parent ? normpath(root, "src", parent.name * ".jl") :
                           find_ext_path(root, pkg.name)
    return PkgLoadSpec(path, VERSION)
end

function _locate_package(env::Union{ExplicitEnv, ImplicitEnv}, pkg::PkgId)
    source = _logical_source(env, pkg)
    source isa PkgId || return source
    return reify(env, pkg, source)
end

function _locate_package(envstack::EnvironmentStack, pkg::PkgId,
                         stopenv::Union{String, Nothing}=nothing;
                         honor_stopenv::Bool=true)::Union{Nothing, Tuple{PkgLoadSpec, String}}
    specenv = _locate_package_unchecked(envstack, pkg, stopenv, honor_stopenv)
    if specenv !== nothing && !isfile_casesensitive(specenv[1].path)
        return nothing
    end
    return specenv
end

function _locate_package_unchecked(envstack::EnvironmentStack, pkg::PkgId,
                                   stopenv::Union{String, Nothing},
                                   honor_stopenv::Bool)::Union{Nothing, Tuple{PkgLoadSpec, String}}
    if pkg.uuid === nothing
        # Implicit (project-less) packages: only look through implicit environments.
        # N.B.: Implicitly loaded packages do not participate in syntax versioning.
        for i in eachindex(envstack.envs)
            env = envstack.envs[i]
            env isa ImplicitEnv || continue
            spec = _locate_package(env, pkg)
            spec isa PkgLoadSpec && return (spec, envstack.roots[i])
            if honor_stopenv
                stopenv == envstack.roots[i] && return nothing
            end
        end
        return nothing
    end
    stdlib_scanned = false
    for i in eachindex(envstack.envs)
        env = envstack.envs[i]
        spec = _locate_package(env, pkg)
        if spec === missing
            # Stop searching, unless this is a stdlib that may be loadable from its location.
            is_stdlib(pkg) && break
            return nothing
        end
        spec isa PkgLoadSpec && return (spec, envstack.roots[i])
        # `@stdlib` may already put the stdlib env in the stack; note that so the
        # explicit-stdlib fallback below does not repeat the (potentially large) scan.
        env === stdlib_env() && (stdlib_scanned = true)
        if honor_stopenv
            stopenv == envstack.roots[i] && break
        end
    end
    # Explicitly listed stdlibs may still load from the stdlib environment.
    if !stdlib_scanned
        spec = _locate_package(stdlib_env(), pkg)
        spec isa PkgLoadSpec && return (spec, Sys.STDLIB)
    end
    return nothing
end

########################
# Public lookup helpers #
########################

# Used by Pkg but not by loading itself.
function find_package(arg)
    @lock require_lock begin
        stack = current_env_stack()
        pkgenv = _identify_package_env(stack, arg)
        pkgenv === nothing && return nothing
        pkg, env = pkgenv
        specenv = _locate_package(stack, pkg, env)
        specenv === nothing && return nothing
        return specenv[1].path
    end
end

function is_stdlib(pkgid::PkgId)
    @lock require_lock begin
        pkg = implicit_env_pkg(stdlib_env(), pkgid.name)
        return pkg !== nothing && pkg.uuid == pkgid.uuid
    end
end

"""
    Base.identify_package_env(name::String)::Union{Tuple{PkgId, String}, Nothing}
    Base.identify_package_env(where::Union{Module,PkgId}, name::String)::Union{Tuple{PkgId, Union{String, Nothing}}, Nothing}

Same as [`Base.identify_package`](@ref) except that the path to the environment where the package is identified
is also returned, except when the identity is not identified.
"""
identify_package_env(where::Module, name::String) = identify_package_env(PkgId(where), name)
function identify_package_env(where::PkgId, name::String)
    assert_havelock(require_lock)
    return _identify_package_env(current_env_stack(), where, name)
end
identify_package_env(where::Nothing, name::String) = identify_package_env(name)
function identify_package_env(name::String)
    assert_havelock(require_lock)
    return _identify_package_env(current_env_stack(), name)
end

_nothing_or_first(x) = x === nothing ? nothing : first(x)

"""
    Base.identify_package(name::String)::Union{PkgId, Nothing}
    Base.identify_package(where::Union{Module,PkgId}, name::String)::Union{PkgId, Nothing}

Identify the package by its name from the current environment stack, returning
its `PkgId`, or `nothing` if it cannot be found.

If only the `name` argument is provided, it searches each environment in the
stack and its named direct dependencies.

The `where` argument provides the context from where to search for the
package: in this case it first checks if the name matches the context itself,
otherwise it searches all recursive dependencies (from the resolved manifest of
each environment) until it locates the context `where`, and from there
identifies the dependency with the corresponding name.

```jldoctest
julia> Base.identify_package("Pkg") # Pkg is a dependency of the default environment
Pkg [44cfe95a-1eb2-52ea-b672-e2afdf69b78f]

julia> using LinearAlgebra

julia> Base.identify_package(LinearAlgebra, "Pkg") # Pkg is not a dependency of LinearAlgebra
```
"""
identify_package(where::Module, name::String) = @lock require_lock _nothing_or_first(identify_package_env(where, name))
identify_package(where::PkgId, name::String)  = @lock require_lock _nothing_or_first(identify_package_env(where, name))
identify_package(name::String)                = @lock require_lock _nothing_or_first(identify_package_env(name))

"""
    Base.locate_package(pkg::PkgId)::Union{String, Nothing}

The path to the entry-point file for the package corresponding to the identifier
`pkg`, or `nothing` if not found. See also [`identify_package`](@ref).

```julia-repl
julia> pkg = Base.identify_package("Pkg")
Pkg [44cfe95a-1eb2-52ea-b672-e2afdf69b78f]

julia> Base.locate_package(pkg)
"/path/to/julia/stdlib/v$(VERSION.major).$(VERSION.minor)/Pkg/src/Pkg.jl"
```
"""
function locate_package(pkg::PkgId, stopenv::Union{String, Nothing}=nothing)::Union{Nothing,String}
    @lock require_lock begin
        specenv = _locate_package(current_env_stack(), pkg, stopenv)
        specenv === nothing && return nothing
        return specenv[1].path
    end
end

function locate_package_load_spec(pkg::PkgId, stopenv::Union{String, Nothing}=nothing)::Union{Nothing,PkgLoadSpec}
    @lock require_lock begin
        specenv = _locate_package(current_env_stack(), pkg, stopenv)
        specenv === nothing && return nothing
        return specenv[1]
    end
end

######################
# Preference lookup  #
######################

filter_preferences(prefs::Dict{String, Any}, ::Nothing) = prefs
filter_preferences(prefs::Dict{String, Any}, pkg_name::String) =
    get(Dict{String, Any}, prefs, pkg_name)::Dict{String, Any}

function project_uuid_name(project::Dict{String, Any}, uuid::UUID)::Union{Nothing, String}
    name = get(project, "name", nothing)::Union{Nothing, String}
    project_uuid = get(project, "uuid", nothing)::Union{Nothing, String}
    if name !== nothing && project_uuid !== nothing && UUID(project_uuid) == uuid
        return name
    end
    for key in ("deps", "extras", "weakdeps")
        for (name, value) in get(Dict{String, Any}, project, key)::Dict{String, Any}
            UUID(value::String) == uuid && return name
        end
    end
    return nothing
end

function project_uuid_name(env::ExplicitEnv, uuid::UUID)::Union{Nothing, String}
    if env.project_name !== nothing && env.project_uuid == uuid
        return env.project_name
    end
    for section in (env.project_deps, env.project_extras, env.project_weakdeps)
        for (name, candidate) in section
            candidate == uuid && return name
        end
    end
    return nothing
end

# Preferences.jl compatibility.
get_uuid_name(project::Dict{String, Any}, uuid::UUID) = project_uuid_name(project, uuid)
get_uuid_name(project_file::String, uuid::UUID) = project_uuid_name(parsed_toml(project_file), uuid)

function collect_preferences(env::ExplicitEnv, uuid::Union{UUID,Nothing})
    dicts = Dict{String, Any}[]
    pkg_name = uuid === nothing ? nothing : project_uuid_name(env, uuid)
    uuid !== nothing && pkg_name === nothing && return dicts

    push!(dicts, filter_preferences(env.project_prefs, pkg_name))
    for name in preferences_names
        path = joinpath(dirname(env.path), name)
        isfile(path) || continue
        push!(dicts, filter_preferences(parsed_toml(path), pkg_name))
        break
    end
    return dicts
end

function recursive_prefs_merge(base::Dict{String, Any}, overrides::Vector{Dict{String, Any}})
    merged = copy(base)
    for override in overrides
        clear = get(override, "__clear__", nothing)
        if clear isa Vector{String}
            for key in clear
                delete!(merged, key)
            end
        end
        for (key, value) in override
            previous = get(merged, key, nothing)
            if previous isa Dict{String, Any} && value isa Dict{String, Any}
                merged[key] = recursive_prefs_merge(previous, Dict{String,Any}[value])
            else
                merged[key] = value
            end
        end
    end
    return merged
end

function get_projects_workspace_to_root(project_file)
    projects = String[project_file]
    while (project_file = base_project(project_file)) !== nothing
        push!(projects, project_file)
    end
    return projects
end

function get_preferences(uuid::Union{UUID,Nothing}=nothing)
    @lock require_lock begin
        stack = current_env_stack()
        envs = ExplicitEnv[]
        for i in eachindex(stack.envs)
            env = stack.envs[i]
            env isa ExplicitEnv || continue
            if isempty(envs) && stack.roots[i] == first(stack.load_path)
                for project_file in get_projects_workspace_to_root(env.path)
                    push!(envs, project_file == env.path ? env : cached_explicit_env(project_file))
                end
            else
                push!(envs, env)
            end
        end
        merged = Dict{String,Any}()
        for env in Iterators.reverse(envs)
            merged = recursive_prefs_merge(merged, collect_preferences(env, uuid))
        end
        return merged
    end
end
