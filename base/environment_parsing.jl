# This file is a part of Julia. License is MIT: https://julialang.org/license

# Environment lookup for code loading. `ExplicitEnv` and `ImplicitEnv` represent
# load-path entries, and `EnvironmentStack` searches them in order. See the
# "Code Loading" chapter of the manual for the roots, graph, and paths model.

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

_env_frozen() = generating_output(#=incremental=#true)

# Precompile workloads can still mutate the environment that their caches describe.
struct EnvSnapshot
    load_path::Vector{String}
    active_project::Union{Nothing, String}
    depot_path::Vector{String}
end
EnvSnapshot() = EnvSnapshot(copy(LOAD_PATH), ACTIVE_PROJECT[], copy(DEPOT_PATH))
function is_env_snapshot_current(snap::EnvSnapshot)
    return snap.load_path == LOAD_PATH &&
           snap.active_project == ACTIVE_PROJECT[] &&
           snap.depot_path == DEPOT_PATH
end

const _frozen_env_snapshot = Ref{Union{Nothing, EnvSnapshot}}(nothing)

function check_frozen_env()
    assert_havelock(require_lock)
    _env_frozen() || return nothing
    snap = _frozen_env_snapshot[]
    snap !== nothing && is_env_snapshot_current(snap) && return nothing
    snap === nothing || _clear_frozen_env_caches()
    _frozen_env_snapshot[] = EnvSnapshot()
    return nothing
end

const _frozen_toml_files = Set{String}()
parsed_toml(project_file::AbstractString) = parsed_toml(project_file, TOML_CACHE, require_lock)
function parsed_toml(project_file::AbstractString, toml_cache::TOMLCache, toml_lock::ReentrantLock)
    lock(toml_lock) do
        d = get(toml_cache.d, project_file, nothing)
        if d === nothing
            d = CachedTOMLDict(toml_cache.p, project_file)
            toml_cache.d[project_file] = d
        elseif !(_env_frozen() && project_file in _frozen_toml_files)
            get_updated_dict(toml_cache.p, d)
        end
        _env_frozen() && push!(_frozen_toml_files, project_file)
        return d.d
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

# Classify a LOAD_PATH entry as:
#  - `false`: nonexistent / nothing to see here
#  - `true`: `env` is an implicit environment
#  - `path`: the path of an explicit project file
const _frozen_project_files = Dict{String, Union{Bool, String}}()
function env_project_file(env::String)::Union{Bool,String}
    @lock require_lock begin
    if _env_frozen()
        check_frozen_env()
        cached = get(_frozen_project_files, env, nothing)
        cached === nothing || return cached
    end
    if isdir(env)
        project_file = locate_project_file(env)
    elseif basename(env) in project_names && isfile_casesensitive(env)
        project_file = env
    else
        project_file = false
    end
    _env_frozen() && (_frozen_project_files[env] = project_file)
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

function syntax_table_version(syntax_table)::Union{Nothing, VersionNumber}
    syntax_table === nothing && return nothing
    julia_version = get(syntax_table, "julia_version", nothing)
    julia_version === nothing && return nothing
    return VersionNumber(julia_version)
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

const _frozen_manifest_files = Dict{String, Union{Nothing, String}}()
function project_file_manifest_path(project_file::String)::Union{Nothing,String}
    @lock require_lock begin
    if _env_frozen()
        check_frozen_env()
        cached = get(_frozen_manifest_files, project_file, missing)
        cached === missing || return cached
    end
    manifest_path = _project_file_manifest_path(project_file)
    _env_frozen() && (_frozen_manifest_files[project_file] = manifest_path)
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

function _read_uuid_section(project::Dict, key::String)
    section = Dict{String, UUID}()
    for (name, uuid) in get(Dict{String, Any}, project, key)::Dict{String, Any}
        section[name] = UUID(uuid::String)
    end
    return section
end

_toml_string_list(value::String) = String[value]
_toml_string_list(value::Vector{String}) = value

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
        Missing,  # not present in the manifest
    }}
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
function ExplicitEnv(envpath::String; workspace::Bool=true)
    if !isfile(envpath)
        envpath = abspath(envpath)
        return ExplicitEnv(nothing, envpath)
    end
    envpath = abspath(envpath)
    project_d = parsed_toml(envpath)

    names = Dict{UUID, String}()
    project_deps = _read_uuid_section(project_d, "deps")
    project_weakdeps = _read_uuid_section(project_d, "weakdeps")
    project_extras = _read_uuid_section(project_d, "extras")
    for section in (project_deps, project_weakdeps, project_extras)
        for (name, uuid) in section
            names[uuid] = name
        end
    end
    project_trigger_uuids = merge(project_deps, project_weakdeps)

    # Weak dependencies override strong dependencies with the same name.
    for name in keys(project_weakdeps)
        delete!(project_deps, name)
    end

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
        triggers = _toml_string_list(triggers)
        uuids = UUID[]
        for trigger in triggers
            uuid = get(project_trigger_uuids, trigger, nothing)
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

            # Compressed dependency lists are only valid for unique package names.
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
                deps_pkg_concrete[ext] = _toml_string_list(triggers)
            end
            extensions[m_uuid] = deps_pkg_concrete

            path = get(pkg_info, "path", nothing)::Union{String, Nothing}
            hash = get(pkg_info, "git-tree-sha1", nothing)::Union{String, Nothing}
            if path !== nothing
                lookup_strategy[m_uuid] = path
            elseif hash !== nothing
                lookup_strategy[m_uuid] = SHA1(hash)
            else
                lookup_strategy[m_uuid] = nothing
            end
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
            # Project data wins over a duplicate root entry in the manifest.
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

    workspace_deps = copy(project_deps)
    base = workspace ? base_project(envpath) : nothing
    if base !== nothing
        base_d = parsed_toml(base)

        for (name, uuid) in _read_uuid_section(base_d, "deps")
            workspace_deps[name] = uuid
        end

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
                    for (name, uuid) in _read_uuid_section(ws_d, "deps")
                        workspace_deps[name] = uuid
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
    package_dirs = (joinpath(dir, name), joinpath(dir, name * ".jl"))
    for package_dir in package_dirs
        path, project_file = entry_point_and_project_file_inside(package_dir, name)
        path === nothing || return path, project_file
    end
    path = normpath(joinpath(dir, "$name.jl"))
    isfile_casesensitive(path) && return path, nothing
    if allow_missing_source
        for package_dir in package_dirs
            path, project_file = entry_point_and_project_file_inside(package_dir, name, true)
            path === nothing || return path, project_file
        end
    end
    return nothing, nothing
end

struct ImplicitEnvPkg
    uuid::Union{Nothing, UUID} # nothing for a project-less package
    path::String               # absolute entry-point path
    project_file::Union{Nothing, String}
    deps::Dict{String, UUID}
    weakdeps::Dict{String, UUID}
    extensions::Dict{String, Vector{PkgId}}
    syntax_version::VersionNumber
end

# A package directory: a LOAD_PATH entry without a project file.
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
    get(d, "name", name)::String == name || return nothing
    _uuid = get(d, "uuid", nothing)::Union{String, Nothing}
    uuid = _uuid === nothing ? dummy_uuid(project_file) : UUID(_uuid)
    deps = _read_uuid_section(d, "deps")
    weakdeps = _read_uuid_section(d, "weakdeps")
    exts = Dict{String, Vector{PkgId}}()
    for (ename, triggers) in get(Dict{String, Any}, d, "extensions")::Dict{String, Any}
        triggers = _toml_string_list(triggers)
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

struct EnvironmentStack
    load_path::Vector{String}
    roots::Vector{String} # LOAD_PATH entry for each item in `envs`
    envs::Vector{Union{ImplicitEnv, ExplicitEnv}}
    snapshot::EnvSnapshot
    # memoized successful locate results; lives as long as the stack itself
    # (whole process when frozen, one require/precompile operation otherwise)
    locate_memo::Dict{Tuple{PkgId, Union{Nothing, String}, Bool}, Tuple{PkgLoadSpec, String}}
end

function EnvironmentStack(load_path::Vector{String} = load_path())
    roots = String[]
    envs = Union{ImplicitEnv, ExplicitEnv}[]
    for root in load_path
        project_file = env_project_file(root)
        env = if project_file isa String
            cached_explicit_env(project_file)
        elseif project_file === true
            abspath(root) == abspath(Sys.STDLIB) ? stdlib_env() : ImplicitEnv(root)
        else
            continue
        end
        push!(roots, root)
        push!(envs, env)
    end
    return EnvironmentStack(load_path, roots, envs, EnvSnapshot(),
                            Dict{Tuple{PkgId, Union{Nothing, String}, Bool}, Tuple{PkgLoadSpec, String}}())
end

const ENV_STACK = Ref{Union{Nothing, EnvironmentStack}}(nothing)
function current_env_stack()
    check_frozen_env()
    stack = ENV_STACK[]
    if stack === nothing || !is_env_snapshot_current(stack.snapshot)
        return EnvironmentStack()
    end
    return stack
end

struct EnvStatSig
    inode::UInt64
    mtime::Float64
    size::Int64
end
function EnvStatSig(path::String)
    s = stat(path)
    return EnvStatSig(s.inode, s.mtime, s.size)
end

struct CachedExplicitEnv
    env::ExplicitEnv
    project_sig::EnvStatSig
    manifest_file::Union{Nothing, String}
    manifest_sig::Union{Nothing, EnvStatSig}
end

const EXPLICIT_ENV_CACHE = Dict{String, CachedExplicitEnv}() # guarded by require_lock

# Reparse when the project, selected manifest, or manifest contents change.
function cached_explicit_env(project_file::String)
    assert_havelock(require_lock)
    project_file = abspath(project_file)
    project_sig = EnvStatSig(project_file)
    manifest_file = project_file_manifest_path(project_file)
    manifest_sig = manifest_file === nothing ? nothing : EnvStatSig(manifest_file)
    c = get(EXPLICIT_ENV_CACHE, project_file, nothing)
    if c !== nothing && c.project_sig == project_sig &&
            c.manifest_file == manifest_file && c.manifest_sig == manifest_sig
        return c.env
    end
    env = ExplicitEnv(project_file; workspace=false)
    EXPLICIT_ENV_CACHE[project_file] = CachedExplicitEnv(env, project_sig, manifest_file, manifest_sig)
    return env
end

function _clear_frozen_env_caches()
    ENV_STACK[] = nothing
    empty!(_frozen_toml_files)
    empty!(_frozen_project_files)
    empty!(_frozen_manifest_files)
    return nothing
end

#################
# Lookup logic  #
#################

# This environment owns `where`, so later environments must not be searched.
const _STOP_SEARCH = :stop

function _find_dep(env::ExplicitEnv, dep_uuids::Vector{UUID}, name::String)::Union{Nothing, PkgId}
    for uuid in dep_uuids
        get(env.names, uuid, nothing) == name && return PkgId(uuid, name)
    end
    return nothing
end

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
# These methods implement roots and graph lookup from the Code Loading manual.

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
        where.name == name && return where
        pkg = _find_dep(env, where_deps, name)
        pkg !== nothing && return pkg
        return _STOP_SEARCH
    end
    ext = _extension_parent(env, where)
    if ext !== nothing
        parent_uuid, triggers = ext

        if get(env.names, parent_uuid, nothing) == name
            return PkgId(parent_uuid, name)
        end

        parent_deps = get(env.deps, parent_uuid, nothing)
        if parent_deps !== nothing
            pkg = _find_dep(env, parent_deps, name)
            pkg !== nothing && return pkg
        end

        parent_weakdeps = get(env.weakdeps, parent_uuid, nothing)
        if parent_weakdeps !== nothing
            pkg = _find_dep(env, parent_weakdeps, name)
            if pkg !== nothing && pkg.uuid in triggers
                return pkg
            end
        end
        return _STOP_SEARCH
    end
    return nothing
end

function _identify_package(env::ImplicitEnv, where::PkgId, name::String)::Union{Nothing, PkgId, Symbol}
    where_pkg = implicit_env_pkg(env, where.name)
    if where_pkg !== nothing && where_pkg.uuid == where.uuid
        where.name == name && return where
        uuid = get(where_pkg.deps, name, nothing)
        uuid !== nothing && return PkgId(uuid, name)
        return _STOP_SEARCH
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
        return _STOP_SEARCH
    end
    return nothing
end

# Also return the load-path root used as `stopenv` by `locate`.
function _identify_package_env(envstack::EnvironmentStack, where::PkgId, name::String)::Union{Nothing, Tuple{PkgId, Union{String, Nothing}}}
    where.name == name && return (where, nothing)
    where.uuid === nothing && return _identify_package_env(envstack, name)
    for i in eachindex(envstack.envs)
        env = envstack.envs[i]
        pkg = _identify_package(env, where, name)
        if pkg === _STOP_SEARCH
            break
        elseif pkg isa PkgId
            return (pkg, envstack.roots[i])
        end
    end
    # Manifests from another Julia version may omit a current stdlib dependency.
    if is_stdlib(where)
        pkg = _identify_package(stdlib_env(), where, name)
        if pkg isa PkgId
            return (pkg, Sys.STDLIB)
        end
    end
    return nothing
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
# `_logical_source` finds a package or extension's source owner. `reify` turns
# the owner's metadata into a path.

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
    # A dependency without a manifest entry may be found in a later environment.
    strategy isa Missing && return nothing
    if strategy isa Nothing
        return reify(stdlib_env(), pkg, source)
    end

    root = if strategy isa String
        # Project roots are absolute; manifest paths are relative to the manifest.
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
    source === nothing && return nothing
    return reify(env, pkg, source)
end

function _locate_package(envstack::EnvironmentStack, pkg::PkgId,
                         stopenv::Union{String, Nothing}=nothing;
                         honor_stopenv::Bool=true)::Union{Nothing, Tuple{PkgLoadSpec, String}}
    key = (pkg, stopenv, honor_stopenv)
    cached = get(envstack.locate_memo, key, nothing)
    cached === nothing || return cached
    specenv = _locate_package_unchecked(envstack, pkg, stopenv, honor_stopenv)
    if specenv === nothing || !isfile_casesensitive(specenv[1].path)
        # not memoized: a package that is missing now may appear later (e.g. via
        # Pkg.instantiate) without any environment file changing
        return nothing
    end
    envstack.locate_memo[key] = specenv
    return specenv
end

function _locate_package_unchecked(envstack::EnvironmentStack, pkg::PkgId,
                                   stopenv::Union{String, Nothing},
                                   honor_stopenv::Bool)::Union{Nothing, Tuple{PkgLoadSpec, String}}
    if pkg.uuid === nothing
        # Project-less packages only exist in package directories.
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
            # This environment pins the source, but it is not installed.
            is_stdlib(pkg) && break
            return nothing
        end
        spec isa PkgLoadSpec && return (spec, envstack.roots[i])
        # Avoid scanning an `@stdlib` entry again below.
        env === stdlib_env() && (stdlib_scanned = true)
        if honor_stopenv
            stopenv == envstack.roots[i] && break
        end
    end
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
