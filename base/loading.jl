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
SHA1(s::AbstractString) = SHA1(hex2bytes(s))

string(hash::SHA1) = bytes2hex(hash.bytes)
print(io::IO, hash::SHA1) = bytes2hex(io, hash.bytes)
show(io::IO, hash::SHA1) = print(io, "SHA1(\"", hash, "\")")

isless(a::SHA1, b::SHA1) = isless(a.bytes, b.bytes)
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

function dummy_uuid(project_file::String)
    project_path = try
        realpath(project_file)
    catch
        project_file
    end
    return uuid5(ns_dummy_uuid, project_path)
end

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

function package_slug(uuid::UUID, p::Int=5)
    crc = _crc32c(uuid)
    return slug(crc, p)
end

function version_slug(uuid::UUID, sha1::SHA1, p::Int=5)
    crc = _crc32c(uuid)
    crc = _crc32c(sha1.bytes, crc)
    return slug(crc, p)
end

## package identification: determine unique identity of package to be loaded ##

function find_package(args...)
    pkg = identify_package(args...)
    pkg === nothing && return nothing
    return locate_package(pkg)
end

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

## package identity: given a package name and a context, try to return its identity ##

identify_package(where::Module, name::String) = identify_package(PkgId(where), name)

# identify_package computes the PkgId for `name` from the context of `where`
# or return `nothing` if no mapping exists for it
function identify_package(where::PkgId, name::String)::Union{Nothing,PkgId}
    where.name === name && return where
    where.uuid === nothing && return identify_package(name) # ignore `where`
    for env in load_path()
        uuid = manifest_deps_get(env, where, name)
        uuid === nothing && continue # not found--keep looking
        uuid.uuid === nothing || return uuid # found in explicit environment--use it
        return nothing # found in implicit environment--return "not found"
    end
    return nothing
end

# identify_package computes the PkgId for `name` from toplevel context
# by looking through the Project.toml files and directories
function identify_package(name::String)::Union{Nothing,PkgId}
    for env in load_path()
        uuid = project_deps_get(env, name)
        uuid === nothing || return uuid # found--return it
    end
    return nothing
end

function identify_package(name::String, names::String...)
    pkg = identify_package(name)
    pkg === nothing && return nothing
    return identify_package(pkg, names...)
end

# locate `tail(names)` package by following the search path graph through `names` starting from `where`
function identify_package(where::PkgId, name::String, names::String...)
    pkg = identify_package(where, name)
    pkg === nothing && return nothing
    return identify_package(pkg, names...)
end

## package location: given a package identity, find file to load ##

function locate_package(pkg::PkgId)::Union{Nothing,String}
    if pkg.uuid === nothing
        for env in load_path()
            # look for the toplevel pkg `pkg.name` in this entry
            found = project_deps_get(env, pkg.name)
            found === nothing && continue
            if pkg == found
                # pkg.name is present in this directory or project file,
                # return the path the entry point for the code, if it could be found
                # otherwise, signal failure
                return implicit_manifest_uuid_path(env, pkg)
            end
            @assert found.uuid !== nothing
            return locate_package(found) # restart search now that we know the uuid for pkg
        end
    else
        for env in load_path()
            path = manifest_uuid_path(env, pkg)
            path === nothing || return entry_path(path, pkg.name)
        end
    end
    return nothing
end

"""
    pathof(m::Module)

Return the path of the `m.jl` file that was used to `import` module `m`,
or `nothing` if `m` was not imported from a package.

Use [`dirname`](@ref) to get the directory part and [`basename`](@ref)
to get the file name part of the path.
"""
function pathof(m::Module)
    pkgid = get(Base.module_keys, m, nothing)
    pkgid === nothing && return nothing
    return Base.locate_package(pkgid)
end

"""
    pkgdir(m::Module)

 Return the root directory of the package that imported module `m`,
 or `nothing` if `m` was not imported from a package.
 """
function pkgdir(m::Module)
    rootmodule = Base.moduleroot(m)
    path = pathof(rootmodule)
    path === nothing && return nothing
    return dirname(dirname(path))
end

## generic project & manifest API ##

const project_names = ("JuliaProject.toml", "Project.toml")
const manifest_names = ("JuliaManifest.toml", "Manifest.toml")

# classify the LOAD_PATH entry to be one of:
#  - `false`: nonexistant / nothing to see here
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

function project_deps_get(env::String, name::String)::Union{Nothing,PkgId}
    project_file = env_project_file(env)
    if project_file isa String
        pkg_uuid = explicit_project_deps_get(project_file, name)
        pkg_uuid === nothing || return PkgId(pkg_uuid, name)
    elseif project_file
        return implicit_project_deps_get(env, name)
    end
    return nothing
end

function manifest_deps_get(env::String, where::PkgId, name::String)::Union{Nothing,PkgId}
    @assert where.uuid !== nothing
    project_file = env_project_file(env)
    if project_file isa String
        # first check if `where` names the Project itself
        proj = project_file_name_uuid(project_file, where.name)
        if proj == where
            # if `where` matches the project, use [deps] section as manifest, and stop searching
            pkg_uuid = explicit_project_deps_get(project_file, name)
            return PkgId(pkg_uuid, name)
        end
        # look for manifest file and `where` stanza
        return explicit_manifest_deps_get(project_file, where.uuid, name)
    elseif project_file
        # if env names a directory, search it
        return implicit_manifest_deps_get(env, where, name)
    end
    return nothing
end

function manifest_uuid_path(env::String, pkg::PkgId)::Union{Nothing,String}
    project_file = env_project_file(env)
    if project_file isa String
        proj = project_file_name_uuid(project_file, pkg.name)
        if proj == pkg
            # if `pkg` matches the project, return the project itself
            return project_file_path(project_file, pkg.name)
        end
        # look for manifest file and `where` stanza
        return explicit_manifest_uuid_path(project_file, pkg)
    elseif project_file
        # if env names a directory, search it
        return implicit_manifest_uuid_path(env, pkg)
    end
    return nothing
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
function project_file_name_uuid(project_file::String, name::String)::PkgId
    pkg = open(project_file) do io
        uuid = dummy_uuid(project_file)
        for line in eachline(io)
            occursin(re_section, line) && break
            if (m = match(re_name_to_string, line)) !== nothing
                name = String(m.captures[1])
            elseif (m = match(re_uuid_to_string, line)) !== nothing
                uuid = UUID(m.captures[1])
            end
        end
        return PkgId(uuid, name)
    end
    return pkg
end

function project_file_path(project_file::String, name::String)::String
    path = open(project_file) do io
        for line in eachline(io)
            occursin(re_section, line) && break
            if (m = match(re_path_to_string, line)) !== nothing
                return String(m.captures[1])
            end
        end
        return ""
    end
    return joinpath(dirname(project_file), path)
end


# find project file's corresponding manifest file
function project_file_manifest_path(project_file::String)::Union{Nothing,String}
    open(project_file) do io
        dir = abspath(dirname(project_file))
        for line in eachline(io)
            occursin(re_section, line) && break
            if (m = match(re_manifest_to_string, line)) !== nothing
                manifest_file = normpath(joinpath(dir, m.captures[1]))
                isfile_casesensitive(manifest_file) && return manifest_file
                return nothing # silently stop if the explicitly listed manifest file is not present
            end
        end
        for mfst in manifest_names
            manifest_file = joinpath(dir, mfst)
            isfile_casesensitive(manifest_file) && return manifest_file
        end
        return nothing
    end
end

# find `name` in a manifest file and return its UUID
# return `nothing` on failure
function manifest_file_name_uuid(manifest_file::IO, name::String)::Union{Nothing,UUID}
    name_section = false
    uuid = nothing
    for line in eachline(manifest_file)
        if (m = match(re_section_capture, line)) !== nothing
            name_section && break
            name_section = (m.captures[1] == name)
        elseif name_section
            if (m = match(re_uuid_to_string, line)) !== nothing
                uuid = UUID(m.captures[1])
            end
        end
    end
    return uuid
end

# given a directory (implicit env from LOAD_PATH) and a name,
# check if it is an implicit package
function entry_point_and_project_file_inside(dir::String, name::String)::Union{Tuple{Nothing,Nothing},Tuple{String,Nothing},Tuple{String,String}}
    path = normpath(joinpath(dir, "src", "$name.jl"))
    isfile_casesensitive(path) || return nothing, nothing
    for proj in project_names
        project_file = normpath(joinpath(dir, proj))
        isfile_casesensitive(project_file) || continue
        return path, project_file
    end
    return path, nothing
end

# given a project directory (implicit env from LOAD_PATH) and a name,
# find an entry point for `name`, and see if it has an associated project file
function entry_point_and_project_file(dir::String, name::String)::Union{Tuple{Nothing,Nothing},Tuple{String,Nothing},Tuple{String,String}}
    path = normpath(joinpath(dir, "$name.jl"))
    isfile_casesensitive(path) && return path, nothing
    dir = joinpath(dir, name)
    path, project_file = entry_point_and_project_file_inside(dir, name)
    path === nothing || return path, project_file
    dir = dir * ".jl"
    path, project_file = entry_point_and_project_file_inside(dir, name)
    path === nothing || return path, project_file
    return nothing, nothing
end

# given a path and a name, return the entry point
function entry_path(path::String, name::String)::Union{Nothing,String}
    isfile_casesensitive(path) && return normpath(path)
    path = normpath(joinpath(path, "src", "$name.jl"))
    isfile_casesensitive(path) && return path
    return nothing # source not found
end

## explicit project & manifest API ##

# find project file root or deps `name => uuid` mapping
# return `nothing` if `name` is not found
function explicit_project_deps_get(project_file::String, name::String)::Union{Nothing,UUID}
    pkg_uuid = open(project_file) do io
        root_name = nothing
        root_uuid = dummy_uuid(project_file)
        state = :top
        for line in eachline(io)
            if occursin(re_section, line)
                state === :top && root_name == name && return root_uuid
                state = occursin(re_section_deps, line) ? :deps : :other
            elseif state === :top
                if (m = match(re_name_to_string, line)) !== nothing
                    root_name = String(m.captures[1])
                elseif (m = match(re_uuid_to_string, line)) !== nothing
                    root_uuid = UUID(m.captures[1])
                end
            elseif state === :deps
                if (m = match(re_key_to_string, line)) !== nothing
                    m.captures[1] == name && return UUID(m.captures[2])
                end
            end
        end
        return root_name == name ? root_uuid : nothing
    end
    return pkg_uuid
end

# find `where` stanza and return the PkgId for `name`
# return `nothing` if it did not find `where` (indicating caller should continue searching)
function explicit_manifest_deps_get(project_file::String, where::UUID, name::String)::Union{Nothing,PkgId}
    manifest_file = project_file_manifest_path(project_file)
    manifest_file === nothing && return nothing # manifest not found--keep searching LOAD_PATH
    found_or_uuid = open(manifest_file) do io
        uuid = deps = nothing
        state = :other
        # first search the manifest for the deps section associated with `where` (by uuid)
        for line in eachline(io)
            if occursin(re_array_of_tables, line)
                uuid == where && break
                uuid = deps = nothing
                state = :stanza
            elseif state === :stanza
                if (m = match(re_uuid_to_string, line)) !== nothing
                    uuid = UUID(m.captures[1])
                elseif (m = match(re_deps_to_any, line)) !== nothing
                    deps = String(m.captures[1])
                elseif occursin(re_subsection_deps, line)
                    state = :deps
                elseif occursin(re_section, line)
                    state = :other
                end
            elseif state === :deps && uuid == where
                # [deps] section format gives both name and uuid
                if (m = match(re_key_to_string, line)) !== nothing
                    m.captures[1] == name && return UUID(m.captures[2])
                end
            end
        end
        # now search through `deps = []` string to see if we have an entry for `name`
        uuid == where || return false
        deps === nothing && return true
        # TODO: handle inline table syntax
        if deps[1] != '[' || deps[end] != ']'
            @warn "Unexpected TOML deps format:\n$deps"
            return false
        end
        occursin(repr(name), deps) || return true
        seekstart(io) # rewind IO handle
        # finally, find out the `uuid` associated with `name`
        return something(manifest_file_name_uuid(io, name), false)
    end
    found_or_uuid isa UUID && return PkgId(found_or_uuid, name)
    found_or_uuid && return PkgId(name)
    return nothing
end

# find `uuid` stanza, return the corresponding path
function explicit_manifest_uuid_path(project_file::String, pkg::PkgId)::Union{Nothing,String}
    manifest_file = project_file_manifest_path(project_file)
    manifest_file === nothing && return nothing # no manifest, skip env
    open(manifest_file) do io
        uuid = name = path = hash = nothing
        for line in eachline(io)
            if (m = match(re_section_capture, line)) !== nothing
                uuid == pkg.uuid && break
                name = String(m.captures[1])
                path = hash = nothing
            elseif (m = match(re_uuid_to_string, line)) !== nothing
                uuid = UUID(m.captures[1])
            elseif (m = match(re_path_to_string, line)) !== nothing
                path = String(m.captures[1])
            elseif (m = match(re_hash_to_string, line)) !== nothing
                hash = SHA1(m.captures[1])
            end
        end
        uuid == pkg.uuid || return nothing
        name == pkg.name || return nothing # TODO: allow a mismatch?
        if path !== nothing
            path = normpath(abspath(dirname(manifest_file), path))
            return path
        end
        hash === nothing && return nothing
        # Keep the 4 since it used to be the default
        for slug in (version_slug(uuid, hash, 4), version_slug(uuid, hash))
            for depot in DEPOT_PATH
                path = abspath(depot, "packages", name, slug)
                ispath(path) && return path
            end
        end
        return nothing
    end
end

## implicit project & manifest API ##

# look for an entry point for `name` from a top-level package (no environment)
# otherwise return `nothing` to indicate the caller should keep searching
function implicit_project_deps_get(dir::String, name::String)::Union{Nothing,PkgId}
    path, project_file = entry_point_and_project_file(dir, name)
    if project_file === nothing
        path === nothing && return nothing
        return PkgId(name)
    end
    proj = project_file_name_uuid(project_file, name)
    proj.name == name || return nothing
    return proj
end

# look for an entry-point for `name`, check that UUID matches
# if there's a project file, look up `name` in its deps and return that
# otherwise return `nothing` to indicate the caller should keep searching
function implicit_manifest_deps_get(dir::String, where::PkgId, name::String)::Union{Nothing,PkgId}
    @assert where.uuid !== nothing
    project_file = entry_point_and_project_file(dir, where.name)[2]
    project_file === nothing && return nothing # a project file is mandatory for a package with a uuid
    proj = project_file_name_uuid(project_file, where.name)
    proj == where || return nothing # verify that this is the correct project file
    # this is the correct project, so stop searching here
    pkg_uuid = explicit_project_deps_get(project_file, name)
    return PkgId(pkg_uuid, name)
end

# look for an entry-point for `pkg` and return its path if UUID matches
function implicit_manifest_uuid_path(dir::String, pkg::PkgId)::Union{Nothing,String}
    path, project_file = entry_point_and_project_file(dir, pkg.name)
    if project_file === nothing
        pkg.uuid === nothing || return nothing
        return path
    end
    proj = project_file_name_uuid(project_file, pkg.name)
    proj == pkg || return nothing
    return path
end

## other code loading functionality ##

function find_source_file(path::AbstractString)
    (isabspath(path) || isfile(path)) && return path
    base_path = joinpath(Sys.BINDIR::String, DATAROOTDIR, "julia", "base", path)
    return isfile(base_path) ? base_path : nothing
end

cache_file_entry(pkg::PkgId) = joinpath(
    "compiled",
    "v$(VERSION.major).$(VERSION.minor)",
    pkg.uuid === nothing ? ""       : pkg.name),
    pkg.uuid === nothing ? pkg.name : package_slug(pkg.uuid)

function find_all_in_cache_path(pkg::PkgId)
    paths = String[]
    entrypath, entryfile = cache_file_entry(pkg)
    for path in joinpath.(DEPOT_PATH, entrypath)
        isdir(path) || continue
        for file in readdir(path)
            if !((pkg.uuid === nothing && file == entryfile * ".ji") ||
                 (pkg.uuid !== nothing && startswith(file, entryfile * "_")))
                 continue
            end
            filepath = joinpath(path, file)
            isfile_casesensitive(filepath) && push!(paths, filepath)
        end
    end
    return paths
end

# these return either the array of modules loaded from the path / content given
# or an Exception that describes why it couldn't be loaded
# and it reconnects the Base.Docs.META
function _include_from_serialized(path::String, depmods::Vector{Any})
    sv = ccall(:jl_restore_incremental, Any, (Cstring, Any), path, depmods)
    if isa(sv, Exception)
        return sv
    end
    restored = sv[1]
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
    isassigned(sv, 2) && ccall(:jl_init_restored_modules, Cvoid, (Any,), sv[2])
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
                M = M::Module
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
        try
            touch(path_to_try) # update timestamp of precompilation file
        catch # file might be read-only and then we fail to update timestamp, which is fine
        end
        # finish loading module graph into staledeps
        for i in 1:length(staledeps)
            dep = staledeps[i]
            dep isa Module && continue
            modpath, modkey, build_id = dep::Tuple{String, PkgId, UInt64}
            dep = _tryrequire_from_serialized(modkey, build_id, modpath)
            if dep === nothing
                @debug "Required dependency $modkey failed to load from cache file for $modpath."
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
        path = normpath(joinpath(dirname(prev), _path))
    end
    if _track_dependencies[]
        push!(_require_dependencies, (mod, path, mtime(path)))
    end
    return path, prev
end

"""
    include_dependency(path::AbstractString)

In a module, declare that the file specified by `path` (relative or absolute) is a
dependency for precompilation; that is, the module will need to be recompiled if this file
changes.

This is only needed if your module depends on a file that is not used via [`include`](@ref). It has
no effect outside of compilation.
"""
function include_dependency(path::AbstractString)
    _include_dependency(Main, path)
    return nothing
end

# we throw PrecompilableError when a module doesn't want to be precompiled
struct PrecompilableError <: Exception end
function show(io::IO, ex::PrecompilableError)
    print(io, "Declaring __precompile__(false) is not allowed in files that are being precompiled.")
end
precompilableerror(ex::PrecompilableError) = true
precompilableerror(ex::WrappedException) = precompilableerror(ex.error)
precompilableerror(@nospecialize ex) = false

# Call __precompile__(false) at the top of a tile prevent it from being precompiled (false)
"""
    __precompile__(isprecompilable::Bool)

Specify whether the file calling this function is precompilable, defaulting to `true`.
If a module or file is *not* safely precompilable, it should call `__precompile__(false)` in
order to throw an error if Julia attempts to precompile it.
"""
@noinline function __precompile__(isprecompilable::Bool=true)
    if !isprecompilable && ccall(:jl_generating_output, Cint, ()) != 0
        throw(PrecompilableError())
    end
    nothing
end

# require always works in Main scope and loads files from node 1
const toplevel_load = Ref(true)

const full_warning_showed = Ref(false)
const modules_warned_for = Set{PkgId}()

"""
    require(into::Module, module::Symbol)

This function is part of the implementation of [`using`](@ref) / [`import`](@ref), if a module is not
already defined in `Main`. It can also be called directly to force reloading a module,
regardless of whether it has been loaded before (for example, when interactively developing
libraries).

Loads a source file, in the context of the `Main` module, on every active node, searching
standard locations for files. `require` is considered a top-level operation, so it sets the
current `include` path but does not use it to search for files (see help for [`include`](@ref)).
This function is typically used to load library code, and is implicitly called by `using` to
load packages.

When searching for files, `require` first looks for package code in the global array
[`LOAD_PATH`](@ref). `require` is case-sensitive on all platforms, including those with
case-insensitive filesystems like macOS and Windows.

For more details regarding code loading, see the manual sections on [modules](@ref modules) and
[parallel computing](@ref code-availability).
"""
function require(into::Module, mod::Symbol)
    uuidkey = identify_package(into, String(mod))
    # Core.println("require($(PkgId(into)), $mod) -> $uuidkey")
    if uuidkey === nothing
        where = PkgId(into)
        if where.uuid === nothing
            throw(ArgumentError("""
                Package $mod not found in current path:
                - Run `import Pkg; Pkg.add($(repr(String(mod))))` to install the $mod package.
                """))
        else
            s = """
            Package $(where.name) does not have $mod in its dependencies:
            - If you have $(where.name) checked out for development and have
              added $mod as a dependency but haven't updated your primary
              environment's manifest file, try `Pkg.resolve()`.
            - Otherwise you may need to report an issue with $(where.name)"""

            uuidkey = identify_package(PkgId(string(into)), String(mod))
            uuidkey === nothing && throw(ArgumentError(s))

            # fall back to toplevel loading with a warning
            if !(where in modules_warned_for)
                @warn string(
                    full_warning_showed[] ? "" : s, "\n",
                    string("Loading $(mod) into $(where.name) from project dependency, ",
                           "future warnings for $(where.name) are suppressed.")
                ) _module = nothing _file = nothing _group = nothing
                push!(modules_warned_for, where)
            end
            full_warning_showed[] = true
        end
    end
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
        path = locate_package(pkg)
        if path === nothing
            throw(ArgumentError("""
                Package $pkg is required but does not seem to be installed:
                 - Run `Pkg.instantiate()` to install all recorded dependencies.
                """))
        end

        # attempt to load the module file via the precompile cache locations
        if JLOptions().use_compiled_modules != 0
            m = _require_search_from_serialized(pkg, path)
            if !isa(m, Bool)
                return
            end
        end

        # if the module being required was supposed to have a particular version
        # but it was not handled by the precompile loader, complain
        for (concrete_pkg, concrete_build_id) in _concrete_dependencies
            if pkg == concrete_pkg
                @warn """Module $(pkg.name) with build ID $concrete_build_id is missing from the cache.
                     This may mean $pkg does not support precompilation but is imported by a module that does."""
                if JLOptions().incremental != 0
                    # during incremental precompilation, this should be fail-fast
                    throw(PrecompilableError())
                end
            end
        end

        if JLOptions().use_compiled_modules != 0
            if (0 == ccall(:jl_generating_output, Cint, ())) || (JLOptions().incremental != 0)
                # spawn off a new incremental pre-compile task for recursive `require` calls
                # or if the require search declared it was pre-compiled before (and therefore is expected to still be pre-compilable)
                cachefile = compilecache(pkg, path)
                if isa(cachefile, Exception)
                    if precompilableerror(cachefile)
                        verbosity = isinteractive() ? CoreLogging.Info : CoreLogging.Debug
                        @logmsg verbosity "Skipping precompilation since __precompile__(false). Importing $pkg."
                    else
                        @warn "The call to compilecache failed to create a usable precompiled cache file for $pkg" exception=m
                    end
                    # fall-through to loading the file locally
                else
                    m = _require_from_serialized(cachefile)
                    if isa(m, Exception)
                        @warn "The call to compilecache failed to create a usable precompiled cache file for $pkg" exception=m
                    else
                        return
                    end
                end
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
            include(__toplevel__, path)
            return
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
    include_string([mapexpr::Function,] m::Module, code::AbstractString, filename::AbstractString="string")

Like [`include`](@ref), except reads code from the given string rather than from a file.

The optional first argument `mapexpr` can be used to transform the included code before
it is evaluated: for each parsed expression `expr` in `code`, the `include_string` function
actually evaluates `mapexpr(expr)`.  If it is omitted, `mapexpr` defaults to [`identity`](@ref).
"""
function include_string(mapexpr::Function, mod::Module, code::AbstractString,
                        filename::AbstractString="string")
    loc = LineNumberNode(1, Symbol(filename))
    try
        ast = Meta.parseall(code, filename=filename)
        @assert Meta.isexpr(ast, :toplevel)
        result = nothing
        line_and_ex = Expr(:toplevel, loc, nothing)
        for ex in ast.args
            if ex isa LineNumberNode
                loc = ex
                line_and_ex.args[1] = ex
                continue
            end
            ex = mapexpr(ex)
            # Wrap things to be eval'd in a :toplevel expr to carry line
            # information as part of the expr.
            line_and_ex.args[2] = ex
            result = Core.eval(mod, line_and_ex)
        end
        return result
    catch exc
        # TODO: Now that stacktraces are more reliable we should remove
        # LoadError and expose the real error type directly.
        rethrow(LoadError(filename, loc.line, exc))
    end
end

include_string(m::Module, txt::AbstractString, fname::AbstractString="string") =
    include_string(identity, m, txt, fname)

function source_path(default::Union{AbstractString,Nothing}="")
    s = current_task().storage
    if s !== nothing && haskey(s::IdDict{Any,Any}, :SOURCE_PATH)
        return s[:SOURCE_PATH]::Union{Nothing,String}
    end
    return default
end

function source_dir()
    p = source_path(nothing)
    return p === nothing ? pwd() : dirname(p)
end

"""
    Base.include([mapexpr::Function,] [m::Module,] path::AbstractString)

Evaluate the contents of the input source file in the global scope of module `m`.
Every module (except those defined with [`baremodule`](@ref)) has its own
definition of `include` omitting the `m` argument, which evaluates the file in that module.
Returns the result of the last evaluated expression of the input file. During including,
a task-local include path is set to the directory containing the file. Nested calls to
`include` will search relative to that path. This function is typically used to load source
interactively, or to combine files in packages that are broken into multiple source files.

The optional first argument `mapexpr` can be used to transform the included code before
it is evaluated: for each parsed expression `expr` in `path`, the `include` function
actually evaluates `mapexpr(expr)`.  If it is omitted, `mapexpr` defaults to [`identity`](@ref).
"""
Base.include # defined in Base.jl

# Full include() implementation which is used after bootstrap
function _include(mapexpr::Function, mod::Module, _path::AbstractString)
    @_noinline_meta # Workaround for module availability in _simplify_include_frames
    path, prev = _include_dependency(mod, _path)
    for callback in include_callbacks # to preserve order, must come before eval in include_string
        invokelatest(callback, mod, path)
    end
    code = read(path, String)
    tls = task_local_storage()
    tls[:SOURCE_PATH] = path
    try
        return include_string(mapexpr, mod, code, path)
    finally
        if prev === nothing
            delete!(tls, :SOURCE_PATH)
        else
            tls[:SOURCE_PATH] = prev
        end
    end
end

"""
    evalfile(path::AbstractString, args::Vector{String}=String[])

Load the file using [`include`](@ref), evaluate all expressions,
and return the value of the last one.
"""
function evalfile(path::AbstractString, args::Vector{String}=String[])
    return Core.eval(Module(:__anon__),
        Expr(:toplevel,
             :(const ARGS = $args),
             :(eval(x) = $(Expr(:core, :eval))(__anon__, x)),
             :(include(x) = $(Expr(:top, :include))(__anon__, x)),
             :(include(mapexpr::Function, x) = $(Expr(:top, :include))(mapexpr, __anon__, x)),
             :(include($path))))
end
evalfile(path::AbstractString, args::Vector) = evalfile(path, String[args...])

function load_path_setup_code(load_path::Bool=true)
    code = """
    append!(empty!(Base.DEPOT_PATH), $(repr(map(abspath, DEPOT_PATH))))
    append!(empty!(Base.DL_LOAD_PATH), $(repr(map(abspath, DL_LOAD_PATH))))
    """
    if load_path
        load_path = map(abspath, Base.load_path())
        path_sep = Sys.iswindows() ? ';' : ':'
        any(path -> path_sep in path, load_path) &&
            error("LOAD_PATH entries cannot contain $(repr(path_sep))")
        code *= """
        append!(empty!(Base.LOAD_PATH), $(repr(load_path)))
        ENV["JULIA_LOAD_PATH"] = $(repr(join(load_path, Sys.iswindows() ? ';' : ':')))
        Base.ACTIVE_PROJECT[] = nothing
        """
    end
    return code
end

# this is called in the external process that generates precompiled package files
function include_package_for_output(input::String, depot_path::Vector{String}, dl_load_path::Vector{String}, load_path::Vector{String}, concrete_deps::typeof(_concrete_dependencies), uuid_tuple::NTuple{2,UInt64}, source::Union{Nothing,String})
    append!(empty!(Base.DEPOT_PATH), depot_path)
    append!(empty!(Base.DL_LOAD_PATH), dl_load_path)
    append!(empty!(Base.LOAD_PATH), load_path)
    ENV["JULIA_LOAD_PATH"] = join(load_path, Sys.iswindows() ? ';' : ':')
    Base.ACTIVE_PROJECT[] = nothing
    Base._track_dependencies[] = true
    append!(empty!(Base._concrete_dependencies), concrete_deps)

    ccall(:jl_set_module_uuid, Cvoid, (Any, NTuple{2, UInt64}), Base.__toplevel__, uuid_tuple)
    if source !== nothing
        task_local_storage()[:SOURCE_PATH] = source
    end

    try
        Base.include(Base.__toplevel__, input)
    catch ex
        precompilableerror(ex) || rethrow()
        @debug "Aborting `create_expr_cache'" exception=(ErrorException("Declaration of __precompile__(false) not allowed"), catch_backtrace())
        exit(125) # we define status = 125 means PrecompileableError
    end
end

@assert precompile(include_package_for_output, (String,Vector{String},Vector{String},Vector{String},typeof(_concrete_dependencies),NTuple{2,UInt64},Nothing))
@assert precompile(include_package_for_output, (String,Vector{String},Vector{String},Vector{String},typeof(_concrete_dependencies),NTuple{2,UInt64},String))

function create_expr_cache(input::String, output::String, concrete_deps::typeof(_concrete_dependencies), uuid::Union{Nothing,UUID})
    rm(output, force=true)   # Remove file if it exists
    depot_path = map(abspath, DEPOT_PATH)
    dl_load_path = map(abspath, DL_LOAD_PATH)
    load_path = map(abspath, Base.load_path())
    path_sep = Sys.iswindows() ? ';' : ':'
    any(path -> path_sep in path, load_path) &&
        error("LOAD_PATH entries cannot contain $(repr(path_sep))")

    deps_strs = String[]
    for (pkg, build_id) in concrete_deps
        pkg_str = if pkg.uuid === nothing
            "Base.PkgId($(repr(pkg.name)))"
        else
            "Base.PkgId(Base.UUID(\"$(pkg.uuid)\"), $(repr(pkg.name)))"
        end
        push!(deps_strs, "$pkg_str => $(repr(build_id))")
    end
    deps = repr(eltype(concrete_deps)) * "[" * join(deps_strs, ",") * "]"

    uuid_tuple = uuid === nothing ? (UInt64(0), UInt64(0)) : convert(NTuple{2, UInt64}, uuid)

    io = open(pipeline(`$(julia_cmd()) -O0
                       --output-ji $output --output-incremental=yes
                       --startup-file=no --history-file=no --warn-overwrite=yes
                       --color=$(have_color === nothing ? "auto" : have_color ? "yes" : "no")
                       --eval 'eval(Meta.parse(read(stdin,String)))'`, stderr=stderr),
              "w", stdout)
    # write data over stdin to avoid the (unlikely) case of exceeding max command line size
    write(io.in, """
        Base.include_package_for_output($(repr(abspath(input))), $(repr(depot_path)), $(repr(dl_load_path)),
            $(repr(load_path)), $deps, $(repr(uuid_tuple)), $(repr(source_path(nothing))))
        """)
    close(io.in)
    return io
end

@assert precompile(create_expr_cache, (String, String, typeof(_concrete_dependencies), Nothing))
@assert precompile(create_expr_cache, (String, String, typeof(_concrete_dependencies), UUID))

function compilecache_path(pkg::PkgId)::String
    entrypath, entryfile = cache_file_entry(pkg)
    cachepath = joinpath(DEPOT_PATH[1], entrypath)
    isdir(cachepath) || mkpath(cachepath)
    if pkg.uuid === nothing
        abspath(cachepath, entryfile) * ".ji"
    else
        crc = _crc32c(something(Base.active_project(), ""))
        crc = _crc32c(unsafe_string(JLOptions().image_file), crc)
        crc = _crc32c(unsafe_string(JLOptions().julia_bin), crc)
        project_precompile_slug = slug(crc, 5)
        abspath(cachepath, string(entryfile, "_", project_precompile_slug, ".ji"))
    end
end

"""
    Base.compilecache(module::PkgId)

Creates a precompiled cache file for a module and all of its dependencies.
This can be used to reduce package load times. Cache files are stored in
`DEPOT_PATH[1]/compiled`. See [Module initialization and precompilation](@ref)
for important notes.
"""
function compilecache(pkg::PkgId)
    path = locate_package(pkg)
    path === nothing && throw(ArgumentError("$pkg not found during precompilation"))
    return compilecache(pkg, path)
end

const MAX_NUM_PRECOMPILE_FILES = 10

function compilecache(pkg::PkgId, path::String)
    # decide where to put the resulting cache file
    cachefile = compilecache_path(pkg)
    cachepath = dirname(cachefile)
    # prune the directory with cache files
    if pkg.uuid !== nothing
        entrypath, entryfile = cache_file_entry(pkg)
        cachefiles = filter!(x -> startswith(x, entryfile * "_"), readdir(cachepath))
        if length(cachefiles) >= MAX_NUM_PRECOMPILE_FILES
            idx = findmin(mtime.(joinpath.(cachepath, cachefiles)))[2]
            rm(joinpath(cachepath, cachefiles[idx]))
        end
    end
    # build up the list of modules that we want the precompile process to preserve
    concrete_deps = copy(_concrete_dependencies)
    for (key, mod) in loaded_modules
        if !(mod === Main || mod === Core || mod === Base)
            push!(concrete_deps, key => module_build_id(mod))
        end
    end
    # run the expression and cache the result
    verbosity = isinteractive() ? CoreLogging.Info : CoreLogging.Debug
    @logmsg verbosity "Precompiling $pkg"

    # create a temporary file in `cachepath` directory, write the cache in it,
    # write the checksum, _and then_ atomically move the file to `cachefile`.
    tmppath, tmpio = mktemp(cachepath)
    local p
    try
        close(tmpio)
        p = create_expr_cache(path, tmppath, concrete_deps, pkg.uuid)
        if success(p)
            # append checksum to the end of the .ji file:
            open(tmppath, "a+") do f
                write(f, _crc32c(seekstart(f)))
            end
            # inherit permission from the source file
            chmod(tmppath, filemode(path) & 0o777)

            # this is atomic according to POSIX:
            rename(tmppath, cachefile)
            return cachefile
        end
    finally
        rm(tmppath, force=true)
    end
    if p.exitcode == 125
        return PrecompilableError()
    else
        error("Failed to precompile $pkg to $cachefile.")
    end
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

        # Check if transitive dependencies can be fulfilled
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
                if ftime != ftime_req && ftime != floor(ftime_req) && ftime != trunc(ftime_req, digits=6)
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
    @__FILE__ -> AbstractString

Expand to a string with the path to the file containing the
macrocall, or an empty string if evaluated by `julia -e <expr>`.
Return `nothing` if the macro was missing parser source information.
Alternatively see [`PROGRAM_FILE`](@ref).
"""
macro __FILE__()
    __source__.file === nothing && return nothing
    return String(__source__.file::Symbol)
end

"""
    @__DIR__ -> AbstractString

Expand to a string with the absolute path to the directory of the file
containing the macrocall.
Return the current working directory if run from a REPL or if evaluated by `julia -e <expr>`.
"""
macro __DIR__()
    __source__.file === nothing && return nothing
    _dirname = dirname(String(__source__.file::Symbol))
    return isempty(_dirname) ? pwd() : abspath(_dirname)
end
