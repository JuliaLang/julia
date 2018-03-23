# This file is a part of Julia. License is MIT: https://julialang.org/license

# Base.require is the implementation for the `import` statement

# Cross-platform case-sensitive path canonicalization

if Sys.isunix() && !Sys.isapple()
    # assume case-sensitive filesystems, don't have to do anything
    isfile_casesensitive(path) = isfile(path)
elseif Sys.iswindows()
    # GetLongPathName Win32 function returns the case-preserved filename on NTFS.
    function isfile_casesensitive(path)
        isfile(path) || return false  # Fail fast
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
        isfile(path) || return false
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
        isfile(path) || return false
        dir, filename = splitdir(path)
        any(readdir(dir) .== filename)
    end
end

## SHA1 ##

struct SHA1
    bytes::Vector{UInt8}
    function SHA1(bytes::Vector{UInt8})
        length(bytes) == 20 ||
            throw(ArgumentError("wrong number of bytes for SHA1 hash: $(length(bytes))"))
        return new(bytes)
    end
end
SHA1(s::Union{String,SubString{String}}) = SHA1(hex2bytes(s))
string(hash::SHA1) = bytes2hex(hash.bytes)

show(io::IO, hash::SHA1) = print(io, "SHA1(", string(hash), ")")
isless(a::SHA1, b::SHA1) = lexless(a.bytes, b.bytes)
hash(a::SHA1, h::UInt) = hash((SHA1, a.bytes), h)
==(a::SHA1, b::SHA1) = a.bytes == b.bytes

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

dummy_uuid(project_file::String) = isfile_casesensitive(project_file) ?
    uuid5(ns_dummy_uuid, realpath(project_file)) : nothing

## package path slugs: turning UUID + SHA1 into a pair of 4-byte "slugs" ##

const slug_chars = String(['A':'Z'; 'a':'z'; '0':'9'])

function slug(x::UInt32, p::Int)
    sprint(sizehint=p) do io
        n = length(slug_chars)
        for i = 1:p
            x, d = divrem(x, n)
            write(io, slug_chars[1+d])
        end
    end
end

function package_slug(uuid::UUID, p::Int=4)
    crc = _crc32c(uuid)
    return slug(crc, p)
end

function version_slug(uuid::UUID, sha1::SHA1, p::Int=4)
    crc = _crc32c(uuid)
    crc = _crc32c(sha1.bytes, crc)
    return slug(crc, p)
end

## load path expansion: turn LOAD_PATH entries into concrete paths ##

function find_env(envs::Vector)
    for env in envs
        path = find_env(env)
        path != nothing && return path
    end
end

function find_env(env::AbstractString)
    path = abspath(env)
    if isdir(path)
        # directory with a project file?
        for name in project_names
            file = abspath(path, name)
            isfile_casesensitive(file) && return file
        end
    end
    # package dir or path to project file
    return path
end

function find_env(env::NamedEnv)
    # look for named env in each depot
    for depot in DEPOT_PATH
        isdir(depot) || continue
        file = nothing
        for name in project_names
            file = abspath(depot, "environments", env.name, name)
            isfile_casesensitive(file) && return file
        end
        file != nothing && env.create && return file
    end
end

function find_env(env::CurrentEnv, dir::AbstractString = pwd())
    # look for project file in current dir and parents
    home = homedir()
    while true
        for name in project_names
            file = joinpath(dir, name)
            isfile_casesensitive(file) && return file
        end
        # bail at home directory or top of git repo
        (dir == home || ispath(joinpath(dir, ".git"))) && break
        old, dir = dir, dirname(dir)
        dir == old && break
    end
    env.create ? joinpath(pwd(), project_names[end]) : nothing
end

find_env(env::Function) = find_env(env())

load_path() = filter(env -> env ≠ nothing, map(find_env, LOAD_PATH))

## package identification: determine unique identity of package to be loaded ##

find_package(args...) = locate_package(identify_package(args...))

struct PkgId
    uuid::Union{UUID,Nothing}
    name::String

    PkgId(u::UUID, name::AbstractString) = new(UInt128(u) == 0 ? nothing : u, name)
    PkgId(::Nothing, name::AbstractString) = new(nothing, name)
end
PkgId(name::AbstractString) = PkgId(nothing, name)

function PkgId(m::Module, name::String = String(nameof(moduleroot(m))))
    uuid = UUID(ccall(:jl_module_uuid, NTuple{2, UInt64}, (Any,), m))
    UInt128(uuid) == 0 ? PkgId(name) : PkgId(uuid, name)
end

==(a::PkgId, b::PkgId) = a.uuid == b.uuid && a.name == b.name

function hash(pkg::PkgId, h::UInt)
    h += 0xc9f248583a0ca36c % UInt
    h = hash(pkg.uuid, h)
    h = hash(pkg.name, h)
    return h
end

show(io::IO, pkg::PkgId) =
    print(io, pkg.name, " [", pkg.uuid === nothing ? "top-level" : pkg.uuid, "]")

function binpack(pkg::PkgId)
    io = IOBuffer()
    write(io, UInt8(0))
    uuid = pkg.uuid
    write(io, uuid === nothing ? UInt128(0) : UInt128(uuid))
    write(io, pkg.name)
    return String(take!(io))
end

function binunpack(s::String)
    io = IOBuffer(s)
    @assert read(io, UInt8) === 0x00
    uuid = read(io, UInt128)
    name = read(io, String)
    return PkgId(UUID(uuid), name)
end

function identify_package(where::Module, name::String)::Union{Nothing,PkgId}
    identify_package(PkgId(where), name)
end

function identify_package(where::PkgId, name::String)::Union{Nothing,PkgId}
    where.name === name && return where
    where.uuid === nothing && return identify_package(name)
    for env in load_path()
        found_or_uuid = manifest_deps_get(env, where, name)
        found_or_uuid isa UUID && return PkgId(found_or_uuid, name)
        found_or_uuid && return nothing
    end
    return nothing
end

function identify_package(name::String)::Union{Nothing,PkgId}
    for env in load_path()
        found_or_uuid = project_deps_get(env, name)
        found_or_uuid isa UUID && return PkgId(found_or_uuid, name)
        found_or_uuid && return PkgId(name)
    end
    return nothing
end

function identify_package(name::String, names::String...)
    pkg = identify_package(name)
    pkg      === nothing ? nothing :
    pkg.uuid === nothing ? identify_package(names...) :
                           identify_package(pkg, names...)
end

function identify_package(where::PkgId, name::String, names::String...)
    pkg = identify_package(where, name)
    pkg      === nothing ? nothing :
    pkg.uuid === nothing ? identify_package(names...) :
                           identify_package(pkg, names...)
end

## package location: given a package identity find file to load ##

function locate_package(pkg::PkgId)::Union{Nothing,String}
    if pkg.uuid === nothing
        for env in load_path()
            found_or_uuid = project_deps_get(env, pkg.name)
            found_or_uuid isa UUID &&
                return locate_package(PkgId(found_or_uuid, pkg.name))
            found_or_uuid && return implicit_manifest_uuid_path(env, pkg)
        end
    else
        for env in load_path()
            path = manifest_uuid_path(env, pkg)
            path != nothing && return entry_path(path, pkg.name)
        end
    end
end
locate_package(::Nothing) = nothing

## generic project & manifest API ##

const project_names = ("JuliaProject.toml", "Project.toml")
const manifest_names = ("JuliaManifest.toml", "Manifest.toml")

# return means
#  - `false`: nothing to see here
#  - `true`: `env` is an implicit environment
#  - `path`: the path of an explicit project file
function env_project_file(env::String)::Union{Bool,String}
    if isdir(env)
        for proj in project_names
            project_file = joinpath(env, proj)
            isfile_casesensitive(project_file) && return project_file
        end
        return true
    elseif basename(env) in project_names && isfile_casesensitive(env)
        return env
    end
    return false
end

function project_deps_get(env::String, name::String)::Union{Bool,UUID}
    project_file = env_project_file(env)
    if project_file isa String
        return explicit_project_deps_get(project_file, name)
    end
    project_file && implicit_project_deps_get(env, name)
end

function manifest_deps_get(env::String, where::PkgId, name::String)::Union{Bool,UUID}
    @assert where.uuid !== nothing
    project_file = env_project_file(env)
    if project_file isa String
        proj_name, proj_uuid = project_file_name_uuid_path(project_file, where.name)
        if proj_name == where.name && proj_uuid == where.uuid
            # `where` matches the project, use deps as manifest
            found_or_uuid = explicit_project_deps_get(project_file, name)
            return found_or_uuid isa UUID ? found_or_uuid : true
        end
        # look for `where` stanza in manifest file
        manifest_file = project_file_manifest_path(project_file)
        if isfile_casesensitive(manifest_file)
            return explicit_manifest_deps_get(manifest_file, where.uuid, name)
        end
        return false # `where` stanza not found
    end
    project_file && implicit_manifest_deps_get(env, where, name)
end

function manifest_uuid_path(env::String, pkg::PkgId)::Union{Nothing,String}
    project_file = env_project_file(env)
    if project_file isa String
        proj_name, proj_uuid, path = project_file_name_uuid_path(project_file, pkg.name)
        proj_name == pkg.name && proj_uuid == pkg.uuid && return path
        manifest_file = project_file_manifest_path(project_file)
        if isfile_casesensitive(manifest_file)
            return explicit_manifest_uuid_path(manifest_file, pkg)
        end
        return nothing
    end
    project_file ? implicit_manifest_uuid_path(env, pkg) : nothing
end

# regular expressions for scanning project & manifest files

const re_section            = r"^\s*\["
const re_array_of_tables    = r"^\s*\[\s*\["
const re_section_deps       = r"^\s*\[\s*\"?deps\"?\s*\]\s*(?:#|$)"
const re_section_capture    = r"^\s*\[\s*\[\s*\"?(\w+)\"?\s*\]\s*\]\s*(?:#|$)"
const re_subsection_deps    = r"^\s*\[\s*\"?(\w+)\"?\s*\.\s*\"?deps\"?\s*\]\s*(?:#|$)"
const re_key_to_string      = r"^\s*(\w+)\s*=\s*\"(.*)\"\s*(?:#|$)"
const re_uuid_to_string     = r"^\s*uuid\s*=\s*\"(.*)\"\s*(?:#|$)"
const re_name_to_string     = r"^\s*name\s*=\s*\"(.*)\"\s*(?:#|$)"
const re_path_to_string     = r"^\s*path\s*=\s*\"(.*)\"\s*(?:#|$)"
const re_hash_to_string     = r"^\s*git-tree-sha1\s*=\s*\"(.*)\"\s*(?:#|$)"
const re_manifest_to_string = r"^\s*manifest\s*=\s*\"(.*)\"\s*(?:#|$)"
const re_deps_to_any        = r"^\s*deps\s*=\s*(.*?)\s*(?:#|$)"

# find project file's top-level UUID entry (or nothing)
function project_file_name_uuid_path(project_file::String,
    name::String)::Tuple{String,UUID,String}
    open(project_file) do io
        uuid = dummy_uuid(project_file)
        path = joinpath("src", "$name.jl")
        for line in eachline(io)
            occursin(re_section, line) && break
            if (m = match(re_name_to_string, line)) != nothing
                name = String(m.captures[1])
            elseif (m = match(re_uuid_to_string, line)) != nothing
                uuid = UUID(m.captures[1])
            elseif (m = match(re_path_to_string, line)) != nothing
                path = String(m.captures[1])
            end
        end
        path = joinpath(dirname(project_file), path)
        return name, uuid, path
    end
end

# find project file's corresponding manifest file
function project_file_manifest_path(project_file::String)::Union{Nothing,String}
    open(project_file) do io
        dir = abspath(dirname(project_file))
        for line in eachline(io)
            occursin(re_section, line) && break
            if (m = match(re_manifest_to_string, line)) != nothing
                return normpath(joinpath(dir, m.captures[1]))
            end
        end
        local manifest_file
        for mfst in manifest_names
            manifest_file = joinpath(dir, mfst)
            isfile_casesensitive(manifest_file) && return manifest_file
        end
        return manifest_file
    end
end

# find `name` in a manifest file and return its UUID
function manifest_file_name_uuid(manifest_file::String, name::String, io::IO)::Union{Nothing,UUID}
    uuid = name′ = nothing
    for line in eachline(io)
        if (m = match(re_section_capture, line)) != nothing
            name′ == name && break
            name′ = String(m.captures[1])
        elseif (m = match(re_uuid_to_string, line)) != nothing
            uuid = UUID(m.captures[1])
        end
    end
    name′ == name ? uuid : nothing
end

# given package dir and name, find an entry point
# and project file if one exists (or nothing if not)
function entry_point_and_project_file(dir::String, name::String)::Union{Tuple{Nothing,Nothing},Tuple{String,Nothing},Tuple{String,String}}
    for entry in ("", joinpath(name, "src"), joinpath("$name.jl", "src"))
        path = normpath(joinpath(dir, entry, "$name.jl"))
        isfile_casesensitive(path) || continue
        if !isempty(entry)
            for proj in project_names
                project_file = normpath(joinpath(dir, dirname(entry), proj))
                isfile_casesensitive(project_file) || continue
                return path, project_file
            end
        end
        return path, nothing
    end
    return nothing, nothing
end

# given a path and a name, return the entry point
function entry_path(path::String, name::String)::Union{Nothing,String}
    isfile_casesensitive(path) && return normpath(path)
    path = normpath(joinpath(path, "src", "$name.jl"))
    isfile_casesensitive(path) ? path : nothing
end
entry_path(::Nothing, name::String) = nothing

# given a project path (project directory or entry point)
# return the project file
function package_path_to_project_file(path::String)::Union{Nothing,String}
    if !isdir(path)
        dir = dirname(path)
        basename(dir) == "src" || return nothing
        path = dirname(dir)
    end
    for proj in project_names
        project_file = joinpath(path, proj)
        isfile_casesensitive(project_file) && return project_file
    end
end

## explicit project & manifest API ##

# find project file root or deps `name => uuid` mapping
#  - `false` means: did not find `name`
#  - `true` means: found `name` without UUID (can't happen in explicit projects)
#  - `uuid` means: found `name` with `uuid` in project file

function explicit_project_deps_get(project_file::String, name::String)::Union{Bool,UUID}
    open(project_file) do io
        root_name = nothing
        root_uuid = dummy_uuid(project_file)
        state = :top
        for line in eachline(io)
            if state == :top
                if occursin(re_section, line)
                    root_name == name && return root_uuid
                    state = occursin(re_section_deps, line) ? :deps : :other
                elseif (m = match(re_name_to_string, line)) != nothing
                    root_name = String(m.captures[1])
                elseif (m = match(re_uuid_to_string, line)) != nothing
                    root_uuid = UUID(m.captures[1])
                end
            elseif state == :deps
                if (m = match(re_key_to_string, line)) != nothing
                    m.captures[1] == name && return UUID(m.captures[2])
                end
            elseif occursin(re_section, line)
                state = occursin(re_section_deps, line) ? :deps : :other
            end
        end
        return root_name == name && root_uuid
    end
end

# find `where` stanza and `name` in its deps and return its UUID
#  - `false` means: did not find `where`
#  - `true` means: found `where` but `name` not in its deps
#  - `uuid` means: found `where` and `name` mapped to `uuid` in its deps

function explicit_manifest_deps_get(manifest_file::String, where::UUID, name::String)::Union{Bool,UUID}
    open(manifest_file) do io
        uuid = deps = nothing
        state = :other
        for line in eachline(io)
            if occursin(re_array_of_tables, line)
                uuid == where && break
                uuid = deps = nothing
                state = :stanza
            elseif state == :stanza
                if (m = match(re_uuid_to_string, line)) != nothing
                    uuid = UUID(m.captures[1])
                elseif (m = match(re_deps_to_any, line)) != nothing
                    deps = String(m.captures[1])
                elseif occursin(re_subsection_deps, line)
                    state = :deps
                elseif occursin(re_section, line)
                    state = :other
                end
            elseif state == :deps && uuid == where
                if (m = match(re_key_to_string, line)) != nothing
                    m.captures[1] == name && return UUID(m.captures[2])
                end
            end
        end
        uuid == where || return false
        deps == nothing && return true
        # TODO: handle inline table syntax
        if deps[1] != '[' || deps[end] != ']'
            @warn "Unexpected TOML deps format:\n$deps"
            return nothing
        end
        occursin(repr(name), deps) || return true
        seekstart(io) # rewind IO handle
        manifest_file_name_uuid(manifest_file, name, io)
    end
end

# find `uuid` stanza, return the corresponding path
function explicit_manifest_uuid_path(manifest_file::String, pkg::PkgId)::Union{Nothing,String}
    open(manifest_file) do io
        uuid = name = path = hash = nothing
        for line in eachline(io)
            if (m = match(re_section_capture, line)) != nothing
                uuid == pkg.uuid && break
                name = String(m.captures[1])
                path = hash = nothing
            elseif (m = match(re_uuid_to_string, line)) != nothing
                uuid = UUID(m.captures[1])
            elseif (m = match(re_path_to_string, line)) != nothing
                path = String(m.captures[1])
            elseif (m = match(re_hash_to_string, line)) != nothing
                hash = SHA1(m.captures[1])
            end
        end
        uuid == pkg.uuid || return nothing
        name == pkg.name || return nothing # TODO: allow a mismatch?
        if path != nothing
            path = normpath(abspath(dirname(manifest_file), path))
            return entry_path(path, name)
        end
        hash == nothing && return nothing
        slug = joinpath(name, version_slug(uuid, hash))
        for depot in DEPOT_PATH
            path = abspath(depot, "packages", slug)
            ispath(path) && return entry_path(path, name)
        end
    end
end

## implicit project & manifest API ##

# look for an entry point for `name`:
#  - `false` means: did not find `name`
#  - `true` means: found `name` without project file
#  - `uuid` means: found `name` with project file with real or dummy `uuid`
function implicit_project_deps_get(dir::String, name::String)::Union{Bool,UUID}
    path, project_file = entry_point_and_project_file(dir, name)
    project_file == nothing && return path != nothing
    proj_name, proj_uuid = project_file_name_uuid_path(project_file, name)
    proj_name == name && proj_uuid
end

# look for an entry-point for `where` by name, check that UUID matches
# if there's a project file, look up `name` in its deps and return that
#  - `false` means: did not find `where`
#  - `true` means: found `where` but `name` not in its deps
#  - `uuid` means: found `where` and `name` mapped to `uuid` in its deps
function implicit_manifest_deps_get(dir::String, where::PkgId, name::String)::Union{Bool,UUID}
    @assert where.uuid !== nothing
    project_file = entry_point_and_project_file(dir, where.name)[2]
    project_file === nothing && return false
    proj_name, proj_uuid = project_file_name_uuid_path(project_file, where.name)
    proj_name == where.name && proj_uuid == where.uuid || return false
    found_or_uuid = explicit_project_deps_get(project_file, name)
    found_or_uuid isa UUID ? found_or_uuid : true
end

# look for an entry-point for `pkg` and return its path if UUID matches
function implicit_manifest_uuid_path(dir::String, pkg::PkgId)::Union{Nothing,String}
    path, project_file = entry_point_and_project_file(dir, pkg.name)
    pkg.uuid === nothing && project_file === nothing && return path
    pkg.uuid === nothing || project_file === nothing && return nothing
    proj_name, proj_uuid = project_file_name_uuid_path(project_file, pkg.name)
    proj_name == pkg.name && proj_uuid == pkg.uuid ? path : nothing
end

## other code loading functionality ##

function find_source_file(path::AbstractString)
    (isabspath(path) || isfile(path)) && return path
    base_path = joinpath(Sys.BINDIR, DATAROOTDIR, "julia", "base", path)
    return isfile(base_path) ? base_path : nothing
end

cache_file_entry(pkg::PkgId) = joinpath(
    "compiled",
    "v$(VERSION.major).$(VERSION.minor)",
    pkg.uuid === nothing ? "$(pkg.name).ji" : joinpath(pkg.name, "$(package_slug(pkg.uuid)).ji")
)

function find_all_in_cache_path(pkg::PkgId)
    paths = String[]
    entry = cache_file_entry(pkg)
    for depot in DEPOT_PATH
        path = joinpath(depot, entry)
        isfile_casesensitive(path) && push!(paths, path)
    end
    return paths
end

# these return either the array of modules loaded from the path / content given
# or an Exception that describes why it couldn't be loaded
# and it reconnects the Base.Docs.META
function _include_from_serialized(path::String, depmods::Vector{Any})
    restored = ccall(:jl_restore_incremental, Any, (Cstring, Any), path, depmods)
    if !isa(restored, Exception)
        for M in restored::Vector{Any}
            M = M::Module
            if isdefined(M, Base.Docs.META)
                push!(Base.Docs.modules, M)
            end
            if parentmodule(M) === M
                register_root_module(M)
            end
        end
    end
    return restored
end

function _tryrequire_from_serialized(modkey::PkgId, build_id::UInt64, modpath::Union{Nothing, String})
    if root_module_exists(modkey)
        M = root_module(modkey)
        if PkgId(M) == modkey && module_build_id(M) === build_id
            return M
        end
    else
        if modpath === nothing
            modpath = locate_package(modkey)
            modpath === nothing && return nothing
        end
        mod = _require_search_from_serialized(modkey, String(modpath))
        if !isa(mod, Bool)
            for callback in package_callbacks
                invokelatest(callback, modkey)
            end
            for M in mod::Vector{Any}
                if PkgId(M) == modkey && module_build_id(M) === build_id
                    return M
                end
            end
        end
    end
    return nothing
end

function _require_from_serialized(path::String)
    # loads a precompile cache file, ignoring stale_cachfile tests
    # load all of the dependent modules first
    local depmodnames
    io = open(path, "r")
    try
        isvalid_cache_header(io) || return ArgumentError("Invalid header in cache file $path.")
        depmodnames = parse_cache_header(io)[3]
        isvalid_file_crc(io) || return ArgumentError("Invalid checksum in cache file $path.")
    finally
        close(io)
    end
    ndeps = length(depmodnames)
    depmods = Vector{Any}(undef, ndeps)
    for i in 1:ndeps
        modkey, build_id = depmodnames[i]
        dep = _tryrequire_from_serialized(modkey, build_id, nothing)
        dep === nothing && return ErrorException("Required dependency $modkey failed to load from a cache file.")
        depmods[i] = dep::Module
    end
    # then load the file
    return _include_from_serialized(path, depmods)
end

# returns `true` if require found a precompile cache for this sourcepath, but couldn't load it
# returns `false` if the module isn't known to be precompilable
# returns the set of modules restored if the cache load succeeded
function _require_search_from_serialized(pkg::PkgId, sourcepath::String)
    paths = find_all_in_cache_path(pkg)
    for path_to_try in paths::Vector{String}
        staledeps = stale_cachefile(sourcepath, path_to_try)
        if staledeps === true
            continue
        end
        # finish loading module graph into staledeps
        for i in 1:length(staledeps)
            dep = staledeps[i]
            dep isa Module && continue
            modpath, modkey, build_id = dep::Tuple{String, PkgId, UInt64}
            dep = _tryrequire_from_serialized(modkey, build_id, modpath)
            if dep === nothing
                @debug "Required dependency $modname failed to load from cache file for $modpath."
                staledeps = true
                break
            end
            staledeps[i] = dep::Module
        end
        if staledeps === true
            continue
        end
        restored = _include_from_serialized(path_to_try, staledeps)
        if isa(restored, Exception)
            @debug "Deserialization checks failed while attempting to load cache from $path_to_try" exception=restored
        else
            return restored
        end
    end
    return !isempty(paths)
end

# to synchronize multiple tasks trying to import/using something
const package_locks = Dict{PkgId,Condition}()

# to notify downstream consumers that a module was successfully loaded
# Callbacks take the form (mod::Base.PkgId) -> nothing.
# WARNING: This is an experimental feature and might change later, without deprecation.
const package_callbacks = Any[]
# to notify downstream consumers that a file has been included into a particular module
# Callbacks take the form (mod::Module, filename::String) -> nothing
# WARNING: This is an experimental feature and might change later, without deprecation.
const include_callbacks = Any[]

# used to optionally track dependencies when requiring a module:
const _concrete_dependencies = Pair{PkgId,UInt64}[] # these dependency versions are "set in stone", and the process should try to avoid invalidating them
const _require_dependencies = Any[] # a list of (mod, path, mtime) tuples that are the file dependencies of the module currently being precompiled
const _track_dependencies = Ref(false) # set this to true to track the list of file dependencies
function _include_dependency(mod::Module, _path::AbstractString)
    prev = source_path(nothing)
    if prev === nothing
        path = abspath(_path)
    else
        path = joinpath(dirname(prev), _path)
    end
    if _track_dependencies[]
        push!(_require_dependencies, (mod, normpath(path), mtime(path)))
    end
    return path, prev
end

"""
    include_dependency(path::AbstractString)

In a module, declare that the file specified by `path` (relative or absolute) is a
dependency for precompilation; that is, the module will need to be recompiled if this file
changes.

This is only needed if your module depends on a file that is not used via `include`. It has
no effect outside of compilation.
"""
function include_dependency(path::AbstractString)
    _include_dependency(Main, path)
    return nothing
end

# We throw PrecompilableError(true) when a module wants to be precompiled but isn't,
# and PrecompilableError(false) when a module doesn't want to be precompiled but is
struct PrecompilableError <: Exception
    isprecompilable::Bool
end
function show(io::IO, ex::PrecompilableError)
    if ex.isprecompilable
        print(io, "Declaring __precompile__(true) is only allowed in module files being imported.")
    else
        print(io, "Declaring __precompile__(false) is not allowed in files that are being precompiled.")
    end
end
precompilableerror(ex::PrecompilableError, c) = ex.isprecompilable == c
precompilableerror(ex::WrappedException, c) = precompilableerror(ex.error, c)
precompilableerror(ex, c) = false

# Call __precompile__ at the top of a file to force it to be precompiled (true), or
# to be prevent it from being precompiled (false).  __precompile__(true) is
# ignored except within "require" call.
"""
    __precompile__(isprecompilable::Bool=true)

Specify whether the file calling this function is precompilable. If `isprecompilable` is
`true`, then `__precompile__` throws an exception when the file is loaded by
`using`/`import`/`require` *unless* the file is being precompiled, and in a module file it
causes the module to be automatically precompiled when it is imported. Typically,
`__precompile__()` should occur before the `module` declaration in the file.

If a module or file is *not* safely precompilable, it should call `__precompile__(false)` in
order to throw an error if Julia attempts to precompile it.

`__precompile__()` should *not* be used in a module unless all of its dependencies are also
using `__precompile__()`. Failure to do so can result in a runtime error when loading the module.
"""
function __precompile__(isprecompilable::Bool=true)
    if (JLOptions().use_compiled_modules != 0 &&
        isprecompilable != (0 != ccall(:jl_generating_output, Cint, ())) &&
        !(isprecompilable && toplevel_load[]))
        throw(PrecompilableError(isprecompilable))
    end
end

# require always works in Main scope and loads files from node 1
const toplevel_load = Ref(true)

"""
    require(module::Symbol)

This function is part of the implementation of `using` / `import`, if a module is not
already defined in `Main`. It can also be called directly to force reloading a module,
regardless of whether it has been loaded before (for example, when interactively developing
libraries).

Loads a source file, in the context of the `Main` module, on every active node, searching
standard locations for files. `require` is considered a top-level operation, so it sets the
current `include` path but does not use it to search for files (see help for `include`).
This function is typically used to load library code, and is implicitly called by `using` to
load packages.

When searching for files, `require` first looks for package code under `Pkg.dir()`,
then tries paths in the global array `LOAD_PATH`. `require` is case-sensitive on
all platforms, including those with case-insensitive filesystems like macOS and
Windows.
"""
function require(into::Module, mod::Symbol)
    uuidkey = identify_package(into, String(mod))
    # Core.println("require($(PkgId(into)), $mod) -> $uuidkey")
    uuidkey === nothing &&
        throw(ArgumentError("Module $mod not found in current path.\nRun `Pkg.add(\"$mod\")` to install the $mod package."))
    if _track_dependencies[]
        push!(_require_dependencies, (into, binpack(uuidkey), 0.0))
    end
    return require(uuidkey)
end

function require(uuidkey::PkgId)
    if !root_module_exists(uuidkey)
        _require(uuidkey)
        # After successfully loading, notify downstream consumers
        for callback in package_callbacks
            invokelatest(callback, uuidkey)
        end
    end
    return root_module(uuidkey)
end

const loaded_modules = Dict{PkgId,Module}()
const module_keys = IdDict{Module,PkgId}() # the reverse

is_root_module(m::Module) = haskey(module_keys, m)
root_module_key(m::Module) = module_keys[m]

function register_root_module(m::Module)
    key = PkgId(m, String(nameof(m)))
    if haskey(loaded_modules, key)
        oldm = loaded_modules[key]
        if oldm !== m
            @warn "Replacing module `$(key.name)`"
        end
    end
    loaded_modules[key] = m
    module_keys[m] = key
    nothing
end

register_root_module(Core)
register_root_module(Base)
register_root_module(Main)

# This is used as the current module when loading top-level modules.
# It has the special behavior that modules evaluated in it get added
# to the loaded_modules table instead of getting bindings.
baremodule __toplevel__
using Base
end

# get a top-level Module from the given key
root_module(key::PkgId) = loaded_modules[key]
root_module(where::Module, name::Symbol) =
    root_module(identify_package(where, String(name)))

root_module_exists(key::PkgId) = haskey(loaded_modules, key)
loaded_modules_array() = collect(values(loaded_modules))

function unreference_module(key::PkgId)
    if haskey(loaded_modules, key)
        m = pop!(loaded_modules, key)
        # need to ensure all modules are GC rooted; will still be referenced
        # in module_keys
    end
end

function _require(pkg::PkgId)
    # handle recursive calls to require
    loading = get(package_locks, pkg, false)
    if loading !== false
        # load already in progress for this module
        wait(loading)
        return
    end
    package_locks[pkg] = Condition()

    last = toplevel_load[]
    try
        toplevel_load[] = false
        # perform the search operation to select the module file require intends to load
        name = pkg.name
        path = locate_package(pkg)
        if path === nothing
            throw(ArgumentError("Module $name not found in current path.\nRun `Pkg.add(\"$name\")` to install the $name package."))
        end

        # attempt to load the module file via the precompile cache locations
        doneprecompile = false
        if JLOptions().use_compiled_modules != 0
            doneprecompile = _require_search_from_serialized(pkg, path)
            if !isa(doneprecompile, Bool)
                return
            end
        end

        # if the module being required was supposed to have a particular version
        # but it was not handled by the precompile loader, complain
        for (concrete_pkg, concrete_build_id) in _concrete_dependencies
            if pkg == concrete_pkg
                @warn """Module $name with build ID $concrete_build_id is missing from the cache.
                     This may mean module $name does not support precompilation but is imported by a module that does."""
                if JLOptions().incremental != 0
                    # during incremental precompilation, this should be fail-fast
                    throw(PrecompilableError(false))
                end
            end
        end

        if doneprecompile === true || JLOptions().incremental != 0
            # spawn off a new incremental pre-compile task for recursive `require` calls
            # or if the require search declared it was pre-compiled before (and therefore is expected to still be pre-compilable)
            cachefile = compilecache(pkg)
            m = _require_from_serialized(cachefile)
            if isa(m, Exception)
                @warn "The call to compilecache failed to create a usable precompiled cache file for module $name" exception=m
                # fall-through, TODO: disable __precompile__(true) error so that the normal include will succeed
            else
                return
            end
        end

        # just load the file normally via include
        # for unknown dependencies
        uuid = pkg.uuid
        uuid = (uuid === nothing ? (UInt64(0), UInt64(0)) : convert(NTuple{2, UInt64}, uuid))
        old_uuid = ccall(:jl_module_uuid, NTuple{2, UInt64}, (Any,), __toplevel__)
        if uuid !== old_uuid
            ccall(:jl_set_module_uuid, Cvoid, (Any, NTuple{2, UInt64}), __toplevel__, uuid)
        end
        try
            include_relative(__toplevel__, path)
            return
        catch ex
            if uuid !== old_uuid
                ccall(:jl_set_module_uuid, Cvoid, (Any, NTuple{2, UInt64}), __toplevel__, old_uuid)
            end
            if doneprecompile === true || JLOptions().use_compiled_modules == 0 || !precompilableerror(ex, true)
                rethrow() # rethrow non-precompilable=true errors
            end
            # the file requested `__precompile__`, so try to build a cache file and use that
            cachefile = compilecache(pkg)
            m = _require_from_serialized(cachefile)
            if isa(m, Exception)
                @warn """Module `$name` declares `__precompile__(true)` but `require` failed
                         to create a usable precompiled cache file""" exception=m
                # TODO: disable __precompile__(true) error and do normal include instead of error
                error("Module $name declares __precompile__(true) but require failed to create a usable precompiled cache file.")
            end
        finally
            if uuid !== old_uuid
                ccall(:jl_set_module_uuid, Cvoid, (Any, NTuple{2, UInt64}), __toplevel__, old_uuid)
            end
        end
    finally
        toplevel_load[] = last
        loading = pop!(package_locks, pkg)
        notify(loading, all=true)
    end
    nothing
end

# relative-path load

"""
    include_string(m::Module, code::AbstractString, filename::AbstractString="string")

Like `include`, except reads code from the given string rather than from a file.
"""
include_string(m::Module, txt::String, fname::String) =
    ccall(:jl_load_file_string, Any, (Ptr{UInt8}, Csize_t, Cstring, Any),
          txt, sizeof(txt), fname, m)

include_string(m::Module, txt::AbstractString, fname::AbstractString="string") =
    include_string(m, String(txt), String(fname))

function source_path(default::Union{AbstractString,Nothing}="")
    t = current_task()
    while true
        s = t.storage
        if s !== nothing && haskey(s, :SOURCE_PATH)
            return s[:SOURCE_PATH]
        end
        if t === t.parent
            return default
        end
        t = t.parent
    end
end

function source_dir()
    p = source_path(nothing)
    p === nothing ? pwd() : dirname(p)
end

include_relative(mod::Module, path::AbstractString) = include_relative(mod, String(path))
function include_relative(mod::Module, _path::String)
    path, prev = _include_dependency(mod, _path)
    for callback in include_callbacks # to preserve order, must come before Core.include
        invokelatest(callback, mod, path)
    end
    tls = task_local_storage()
    tls[:SOURCE_PATH] = path
    local result
    try
        result = Core.include(mod, path)
    finally
        if prev === nothing
            delete!(tls, :SOURCE_PATH)
        else
            tls[:SOURCE_PATH] = prev
        end
    end
    return result
end

"""
    include(m::Module, path::AbstractString)

Evaluate the contents of the input source file into module `m`. Returns the result
of the last evaluated expression of the input file. During including, a task-local include
path is set to the directory containing the file. Nested calls to `include` will search
relative to that path. This function is typically used to load source
interactively, or to combine files in packages that are broken into multiple source files.
"""
include # defined in sysimg.jl

"""
    evalfile(path::AbstractString, args::Vector{String}=String[])

Load the file using [`include`](@ref), evaluate all expressions,
and return the value of the last one.
"""
function evalfile(path::AbstractString, args::Vector{String}=String[])
    return eval(Module(:__anon__),
                Expr(:toplevel,
                     :(const ARGS = $args),
                     :(eval(x) = $(Expr(:core, :eval))(__anon__, x)),
                     :(eval(m, x) = $(Expr(:core, :eval))(m, x)),
                     :(include(x) = $(Expr(:top, :include))(__anon__, x)),
                     :(include($path))))
end
evalfile(path::AbstractString, args::Vector) = evalfile(path, String[args...])

function create_expr_cache(input::String, output::String, concrete_deps::typeof(_concrete_dependencies), uuid::Union{Nothing,UUID})
    rm(output, force=true)   # Remove file if it exists
    code_object = """
        while !eof(stdin)
            code = readuntil(stdin, '\\0')
            eval(Main, Meta.parse(code))
        end
        """
    io = open(pipeline(detach(`$(julia_cmd()) -O0
                              --output-ji $output --output-incremental=yes
                              --startup-file=no --history-file=no --warn-overwrite=yes
                              --color=$(have_color ? "yes" : "no")
                              --eval $code_object`), stderr=stderr),
              "w", stdout)
    in = io.in
    try
        write(in, """
        begin
        import Pkg
        empty!(Base.LOAD_PATH)
        append!(Base.LOAD_PATH, $(repr(LOAD_PATH, context=:module=>nothing)))
        empty!(Base.DEPOT_PATH)
        append!(Base.DEPOT_PATH, $(repr(DEPOT_PATH)))
        empty!(Base.DL_LOAD_PATH)
        append!(Base.DL_LOAD_PATH, $(repr(DL_LOAD_PATH)))
        Base._track_dependencies[] = true
        empty!(Base._concrete_dependencies)
        """)
        for (pkg, build_id) in concrete_deps
            pkg_str = if pkg.uuid === nothing
                "Base.PkgId($(repr(pkg.name)))"
            else
                "Base.PkgId(Base.UUID(\"$(pkg.uuid)\"), $(repr(pkg.name)))"
            end
            write(in, "push!(Base._concrete_dependencies, $pkg_str => $(repr(build_id)))\n")
        end
        write(io, "end\0")
        uuid_tuple = uuid === nothing ? (0, 0) : convert(NTuple{2, UInt64}, uuid)
        write(in, "ccall(:jl_set_module_uuid, Cvoid, (Any, NTuple{2, UInt64}), Base.__toplevel__, $uuid_tuple)\0")
        source = source_path(nothing)
        if source !== nothing
            write(in, "task_local_storage()[:SOURCE_PATH] = $(repr(source))\0")
        end
        write(in, "Base.include(Base.__toplevel__, $(repr(abspath(input))))\0")
        # TODO: cleanup is probably unnecessary here
        if source !== nothing
            write(in, "delete!(task_local_storage(), :SOURCE_PATH)\0")
        end
        write(in, "ccall(:jl_set_module_uuid, Cvoid, (Any, NTuple{2, UInt64}), Base.__toplevel__, (0, 0))\0")
        close(in)
    catch ex
        close(in)
        process_running(io) && Timer(t -> kill(io), interval = 5.0) # wait a short time before killing the process to give it a chance to clean up on its own first
        rethrow(ex)
    end
    return io
end

"""
    Base.compilecache(module::PkgId)

Creates a precompiled cache file for a module and all of its dependencies.
This can be used to reduce package load times. Cache files are stored in
`DEPOT_PATH[1]/compiled`. See [Module initialization and precompilation](@ref)
for important notes.
"""
function compilecache(pkg::PkgId)
    # decide where to get the source file from
    name = pkg.name
    path = locate_package(pkg)
    path === nothing && throw(ArgumentError("$name not found in path"))
    # decide where to put the resulting cache file
    cachefile = abspath(DEPOT_PATH[1], cache_file_entry(pkg))
    cachepath = dirname(cachefile)
    isdir(cachepath) || mkpath(cachepath)
    # build up the list of modules that we want the precompile process to preserve
    concrete_deps = copy(_concrete_dependencies)
    for (key, mod) in loaded_modules
        if !(mod === Main || mod === Core || mod === Base)
            push!(concrete_deps, key => module_build_id(mod))
        end
    end
    # run the expression and cache the result
    verbosity = isinteractive() ? CoreLogging.Info : CoreLogging.Debug
    if isfile(cachefile)
        @logmsg verbosity "Recompiling stale cache file $cachefile for module $name"
    else
        @logmsg verbosity "Precompiling module $name"
    end
    if success(create_expr_cache(path, cachefile, concrete_deps, pkg.uuid))
        # append checksum to the end of the .ji file:
        open(cachefile, "a+") do f
            write(f, _crc32c(seekstart(f)))
        end
    else
        error("Failed to precompile $name to $cachefile.")
    end
    return cachefile
end

module_build_id(m::Module) = ccall(:jl_module_build_id, UInt64, (Any,), m)

isvalid_cache_header(f::IOStream) = (0 != ccall(:jl_read_verify_header, Cint, (Ptr{Cvoid},), f.ios))
isvalid_file_crc(f::IOStream) = (_crc32c(seekstart(f), filesize(f) - 4) == read(f, UInt32))

function parse_cache_header(f::IO)
    modules = Vector{Pair{PkgId, UInt64}}()
    while true
        n = read(f, Int32)
        n == 0 && break
        sym = String(read(f, n)) # module name
        uuid = UUID((read(f, UInt64), read(f, UInt64))) # pkg UUID
        build_id = read(f, UInt64) # build UUID (mostly just a timestamp)
        push!(modules, PkgId(uuid, sym) => build_id)
    end
    totbytes = read(f, Int64) # total bytes for file dependencies
    # read the list of requirements
    # and split the list into include and requires statements
    includes = Tuple{PkgId, String, Float64}[]
    requires = Pair{PkgId, PkgId}[]
    while true
        n2 = read(f, Int32)
        n2 == 0 && break
        depname = String(read(f, n2))
        mtime = read(f, Float64)
        n1 = read(f, Int32)
        # map ids to keys
        modkey = (n1 == 0) ? PkgId("") : modules[n1].first
        if n1 != 0
            # consume (and ignore) the module path too
            while true
                n1 = read(f, Int32)
                totbytes -= 4
                n1 == 0 && break
                skip(f, n1) # String(read(f, n1))
                totbytes -= n1
            end
        end
        if depname[1] == '\0'
            push!(requires, modkey => binunpack(depname))
        else
            push!(includes, (modkey, depname, mtime))
        end
        totbytes -= 4 + 4 + n2 + 8
    end
    @assert totbytes == 12 "header of cache file appears to be corrupt"
    srctextpos = read(f, Int64)
    # read the list of modules that are required to be present during loading
    required_modules = Vector{Pair{PkgId, UInt64}}()
    while true
        n = read(f, Int32)
        n == 0 && break
        sym = String(read(f, n)) # module name
        uuid = UUID((read(f, UInt64), read(f, UInt64))) # pkg UUID
        build_id = read(f, UInt64) # build id
        push!(required_modules, PkgId(uuid, sym) => build_id)
    end
    return modules, (includes, requires), required_modules, srctextpos
end

function parse_cache_header(cachefile::String)
    io = open(cachefile, "r")
    try
        !isvalid_cache_header(io) && throw(ArgumentError("Invalid header in cache file $cachefile."))
        return parse_cache_header(io)
    finally
        close(io)
    end
end

function cache_dependencies(f::IO)
    defs, (includes, requires), modules = parse_cache_header(f)
    return modules, map(mod_fl_mt -> (mod_fl_mt[2], mod_fl_mt[3]), includes)  # discard the module
end

function cache_dependencies(cachefile::String)
    io = open(cachefile, "r")
    try
        !isvalid_cache_header(io) && throw(ArgumentError("Invalid header in cache file $cachefile."))
        return cache_dependencies(io)
    finally
        close(io)
    end
end

function read_dependency_src(io::IO, filename::AbstractString)
    modules, (includes, requires), required_modules, srctextpos = parse_cache_header(io)
    srctextpos == 0 && error("no source-text stored in cache file")
    seek(io, srctextpos)
    return _read_dependency_src(io, filename)
end

function _read_dependency_src(io::IO, filename::AbstractString)
    while !eof(io)
        filenamelen = read(io, Int32)
        filenamelen == 0 && break
        fn = String(read(io, filenamelen))
        len = read(io, UInt64)
        if fn == filename
            return String(read(io, len))
        end
        seek(io, position(io) + len)
    end
    error(filename, " is not stored in the source-text cache")
end

function read_dependency_src(cachefile::String, filename::AbstractString)
    io = open(cachefile, "r")
    try
        !isvalid_cache_header(io) && throw(ArgumentError("Invalid header in cache file $cachefile."))
        return read_dependency_src(io, filename)
    finally
        close(io)
    end
end

# returns true if it "cachefile.ji" is stale relative to "modpath.jl"
# otherwise returns the list of dependencies to also check
function stale_cachefile(modpath::String, cachefile::String)
    io = open(cachefile, "r")
    try
        if !isvalid_cache_header(io)
            @debug "Rejecting cache file $cachefile due to it containing an invalid cache header"
            return true # invalid cache file
        end
        (modules, (includes, requires), required_modules) = parse_cache_header(io)
        modules = Dict{PkgId, UInt64}(modules)

        # Check if transitive dependencies can be fullfilled
        ndeps = length(required_modules)
        depmods = Vector{Any}(undef, ndeps)
        for i in 1:ndeps
            req_key, req_build_id = required_modules[i]
            # Module is already loaded
            if root_module_exists(req_key)
                M = root_module(req_key)
                if PkgId(M) == req_key && module_build_id(M) === req_build_id
                    depmods[i] = M
                else
                    @debug "Rejecting cache file $cachefile because module $req_key is already loaded and incompatible."
                    return true # Won't be able to fulfill dependency
                end
            else
                path = locate_package(req_key)
                if path === nothing
                    @debug "Rejecting cache file $cachefile because dependency $req_key not found."
                    return true # Won't be able to fulfill dependency
                end
                depmods[i] = (path, req_key, req_build_id)
            end
        end

        # check if this file is going to provide one of our concrete dependencies
        # or if it provides a version that conflicts with our concrete dependencies
        # or neither
        skip_timecheck = false
        for (req_key, req_build_id) in _concrete_dependencies
            build_id = get(modules, req_key, UInt64(0))
            if build_id !== UInt64(0)
                if build_id === req_build_id
                    skip_timecheck = true
                    break
                end
                @debug "Rejecting cache file $cachefile because it provides the wrong uuid (got $build_id) for $mod (want $req_build_id)"
                return true # cachefile doesn't provide the required version of the dependency
            end
        end

        # now check if this file is fresh relative to its source files
        if !skip_timecheck
            if !samefile(includes[1][2], modpath)
                @debug "Rejecting cache file $cachefile because it is for file $(includes[1][2])) not file $modpath"
                return true # cache file was compiled from a different path
            end
            for (modkey, req_modkey) in requires
                # verify that `require(modkey, name(req_modkey))` ==> `req_modkey`
                if identify_package(modkey, req_modkey.name) != req_modkey
                    @debug "Rejecting cache file $cachefile because uuid mapping for $modkey => $req_modkey has changed"
                    return true
                end
            end
            for (_, f, ftime_req) in includes
                # Issue #13606: compensate for Docker images rounding mtimes
                # Issue #20837: compensate for GlusterFS truncating mtimes to microseconds
                ftime = mtime(f)
                if ftime != ftime_req && ftime != floor(ftime_req) && ftime != trunc(ftime_req, 6)
                    @debug "Rejecting stale cache file $cachefile (mtime $ftime_req) because file $f (mtime $ftime) has changed"
                    return true
                end
            end
        end

        if !isvalid_file_crc(io)
            @debug "Rejecting cache file $cachefile because it has an invalid checksum"
            return true
        end

        return depmods # fresh cachefile
    finally
        close(io)
    end
end

"""
    @__LINE__ -> Int

`@__LINE__` expands to the line number of the location of the macrocall.
Returns `0` if the line number could not be determined.
"""
macro __LINE__()
    return __source__.line
end

"""
    @__FILE__ -> AbstractString

`@__FILE__` expands to a string with the path to the file containing the
macrocall, or an empty string if evaluated by `julia -e <expr>`.
Returns `nothing` if the macro was missing parser source information.
Alternatively see [`PROGRAM_FILE`](@ref).
"""
macro __FILE__()
    __source__.file === nothing && return nothing
    return String(__source__.file)
end

"""
    @__DIR__ -> AbstractString

`@__DIR__` expands to a string with the absolute path to the directory of the file
containing the macrocall.
Returns the current working directory if run from a REPL or if evaluated by `julia -e <expr>`.
"""
macro __DIR__()
    __source__.file === nothing && return nothing
    return abspath(dirname(String(__source__.file)))
end
