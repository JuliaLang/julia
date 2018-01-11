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
        buf = Vector{UInt8}(uninitialized, length(path_basename) + header_size + 1)
        while true
            ret = ccall(:getattrlist, Cint,
                        (Cstring, Ptr{Cvoid}, Ptr{Cvoid}, Csize_t, Culong),
                        path, attr_list, buf, sizeof(buf), FSOPT_NOFOLLOW)
            systemerror(:getattrlist, ret ≠ 0)
            filename_length = @gc_preserve buf unsafe_load(
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

convert(::Type{String}, hash::SHA1) = bytes2hex(convert(Vector{UInt8}, hash))
convert(::Type{Vector{UInt8}}, hash::SHA1) = hash.bytes

string(hash::SHA1) = String(hash)
show(io::IO, hash::SHA1) = print(io, "SHA1(", convert(String, hash), ")")
isless(a::SHA1, b::SHA1) = lexless(a.bytes, b.bytes)
hash(a::SHA1, h::UInt) = hash((SHA1, a.bytes), h)
==(a::SHA1, b::SHA1) = a.bytes == b.bytes

## package path slugs ##

import Base.Random: UUID

const SlugInt = UInt32 # max p = 4
const chars = String(['A':'Z'; 'a':'z'; '0':'9'])
const nchars = SlugInt(length(chars))
const max_p = floor(Int, log(nchars, typemax(SlugInt) >>> 8))

function slug(x::SlugInt, p::Int)
    1 ≤ p ≤ max_p || # otherwise previous steps are wrong
        error("invalid slug size: $p (need 1 ≤ p ≤ $max_p)")
    return sprint() do io
        for i = 1:p
            x, d = divrem(x, nchars)
            write(io, chars[1+d])
        end
    end
end
slug(x::Integer, p::Int) = slug(SlugInt(x), p)

function slug(bytes::Vector{UInt8}, p::Int)
    n = nchars^p
    x = zero(SlugInt)
    for (i, b) in enumerate(bytes)
        x = (x + b*powermod(2, 8(i-1), n)) % n
    end
    slug(x, p)
end

slug(uuid::UUID, p::Int=4) = slug(uuid.value % nchars^p, p)
slug(sha1::SHA1, p::Int=4) = slug(sha1.bytes, p)

version_slug(uuid::UUID, sha1::SHA1) = joinpath(slug(uuid), slug(sha1))

## finding packages ##

const uuid_sym = Symbol("#uuid")

const project_names = ["JuliaProject.toml", "Project.toml"]
const manifest_names = ["JuliaManifest.toml", "Manifest.toml"]

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
end

find_env(env::Function) = find_env(env())

load_path() = filter(env -> env ≠ nothing, map(find_env, LOAD_PATH))

# find `import name` inside of `into` module
function find_package(into::Module, name::String)
    isdefined(into, uuid_sym) || return find_package(name)
    into_uuid = getfield(into, uuid_sym)::UUID
    into_name = String(module_name(into))
    find_package(into_name => into_uuid, name)
end

# find top-level `import name`
function find_package(name::String)
    path = nothing
    for env in (envs = load_path())
        what = project_uuid_or_path(env, name)
        what isa UUID && return (manifest_path(what, name, envs), what)
        path == nothing && (path = what)
    end
    return path, nothing
end

# find `import name` with respect to `into_uuid` + `into_name`
function find_package(into::Pair{String,UUID}, name::String)
    # look for `into_uuid` + `into_name` in each manifest
    # look up `name` in corresponding deps section --> uuid | path
    # if the result is a UUID scan all manifests for it
    # if the result is a path just return it
    for env in (envs = load_path())
        if isdir(env) # package directory
            # look for `into` entry point & project file
            for dir in [into[1], "$(into[1]).jl"]
                dir = abspath(env, dir)
                path = joinpath(dir, "src", "$(into[1]).jl")
                isfile_casesensitive(path) || continue
                for proj in project_names
                    project_file = joinpath(dir, proj)
                    isfile_casesensitive(project_file) || continue
                    # have both entry point and project file
                    uuid = project_file_uuid(project_file)
                    uuid == into[2] || break # next env
                    what = project_file_deps_uuid_or_path(project_file, name)
                    if what isa UUID
                        # now look UUID + name up in envs as manifests
                        return manifest_path(what, name, envs), what
                    else
                        # just return the given entry point
                        return entry_path(what, name), nothing
                    end
                end
                break
            end
        elseif basename(env) in project_names && isfile_casesensitive(env)
            manifest_file = project_file_manifest_path(env)
            isfile_casesensitive(manifest_file) || continue
            io = open(manifest_file)
            try
                deps = manifest_file_deps(manifest_file, into[2], io)
                deps == nothing && continue
                if deps isa Vector
                    name in deps || return nothing, nothing
                    what = name
                elseif deps isa Dict
                    haskey(deps, name) || return nothing, nothing
                    what = deps[name]::UUID
                end
                seekstart(io) # rewind IO handle
                path_and_uuid = manifest_file_path_and_uuid(manifest_file, what, io)
                path_and_uuid == nothing && return nothing, nothing
                path = entry_path(path_and_uuid[1], name)
                return path, path_and_uuid[2]
            finally
                close(io)
            end
        end
    end
end

function find_package(name::String, names::String...)
    path, uuid = find_package(name)
    path === nothing && return nothing, nothing
    uuid === nothing && return find_package(names...)
    return find_package(name => uuid, names...)
end

function find_package(into::Pair{String,UUID}, name::String, names::String...)
    path, uuid = find_package(into, name)
    path === nothing && return nothing, nothing
    uuid === nothing && return find_package(names...)
    return find_package(name => uuid, names...)
end

## helper functions for finding packages ##

function entry_path(path::String, name::String)
    isfile_casesensitive(path) && return path
    path = joinpath(path, "src", "$name.jl")
    isfile_casesensitive(path) ? path : nothing
end
entry_path(::Nothing, name::String) = nothing

# search `env` as a project (implicit or explicit):
#  - return `uuid` of `name` if it exists
#  - return `path` of `name` if it exists otherwise
#  - return `nothing` otherwise
function project_uuid_or_path(env::String, name::String)
    if isdir(env) # package directory
        for dir in ["", joinpath(name, "src"), joinpath("$name.jl", "src")]
            dir = joinpath(env, dir)
            path = joinpath(dir, "$name.jl")
            isfile_casesensitive(path) || continue
            if basename(dir) == "src"
                for proj in project_names
                    project_file = joinpath(dirname(dir), proj)
                    isfile_casesensitive(project_file) || continue
                    return project_file_uuid(project_file)
                end
            end
            return path
        end
    elseif basename(env) in project_names && isfile_casesensitive(env)
        what = project_file_deps_uuid_or_path(env, name)
        return what isa UUID ? what : entry_path(what, name)
    end
    return nothing
end

# search each `env` as a manifest (implicit or explicit) for `uuid`:
#  - implicit: look for `name` entry point with matching `uuid`
#  - explicit: look for `uuid` stanza and entry point for `name`
function manifest_path(uuid::UUID, name::String, envs::Vector{String})
    for env in envs # load_path() from caller
        if isdir(env) # package directory
            for dir in [name, "$name.jl"]
                dir = joinpath(env, dir)
                path = joinpath(dir, "src", "$name.jl")
                isfile_casesensitive(path) || continue
                uuid′ = nothing
                for proj in project_names
                    project_file = joinpath(dir, proj)
                    isfile_casesensitive(project_file) || continue
                    # have both entry point and project file
                    uuid′ = project_file_uuid(project_file)
                    break
                end
                uuid′ == uuid && return path
                break # found but uuid didn't match
            end
        elseif basename(env) in project_names && isfile_casesensitive(env)
            manifest_file = project_file_manifest_path(env)
            isfile_casesensitive(manifest_file) || continue
            path_and_uuid = manifest_file_path_and_uuid(manifest_file, uuid)
            path_and_uuid == nothing && return nothing
            return entry_path(path_and_uuid[1], name)
        end
    end
end

## TOML file parsing helpers ##

const re_section            = r"^\s*\["
const re_array_of_tables    = r"^\s*\[\s*\["
const re_section_deps       = r"^\s*\[\s*\"?deps\"?\s*\]\s*(?:#|$)"
const re_section_capture    = r"^\s*\[\s*\[\s*\"?(\w+)\"?\s*\]\s*\]\s*(?:#|$)"
const re_subsection_deps    = r"^\s*\[\s*\"?(\w+)\"?\s*\.\s*\"?deps\"?\s*\]\s*(?:#|$)"
const re_key_to_string      = r"^\s*(\w+)\s*=\s*\"(.*)\"\s*(?:#|$)"
const re_uuid_to_string     = r"^\s*uuid\s*=\s*\"(.*)\"\s*(?:#|$)"
const re_path_to_string     = r"^\s*path\s*=\s*\"(.*)\"\s*(?:#|$)"
const re_hash_to_string     = r"^\s*hash-sha1\s*=\s*\"(.*)\"\s*(?:#|$)"
const re_manifest_to_string = r"^\s*manifest\s*=\s*\"(.*)\"\s*(?:#|$)"
const re_deps_to_any        = r"^\s*deps\s*=\s*(.*?)\s*(?:#|$)"

# find project file's top-level UUID entry (or nothing)
function project_file_uuid(project_file::String)
    open(project_file) do io
        for line in eachline(io)
            ismatch(re_section, line) && break
            if (m = match(re_uuid_to_string, line)) != nothing
                return UUID(m.captures[1])
            end
        end
    end
end

# find project file's corresponding manifest file
function project_file_manifest_path(project_file::String)
    open(project_file) do io
        dir = abspath(dirname(project_file))
        for line in eachline(io)
            ismatch(re_section, line) && break
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

# find project file deps section's `name => uuid | path` mapping
function project_file_deps_uuid_or_path(project_file::String, name::String)
    open(project_file) do io
        for line in eachline(io)
            ismatch(re_section_deps, line) && break
        end
        for line in eachline(io)
            ismatch(re_section, line) && break
            m = match(re_key_to_string, line)
            m.captures[1] != name && continue
            what = m.captures[2]
            if '/' in what || '\\' in what
                return normpath(joinpath(dirname(project_file), what))
            else
                return UUID(what)
            end
        end
    end
end

function manifest_file_deps(manifest_file::AbstractString, uuid::UUID)
    open(manifest_file) do io
        manifest_file_deps(manifest_file, uuid, io)
    end
end

# find manifest file's `into` stanza's deps
function manifest_file_deps(manifest_file::AbstractString, into::UUID, io::IO)
    deps = nothing
    found = in_deps = false
    for line in eachline(io)
        if !in_deps
            if ismatch(re_array_of_tables, line)
                found && break
                deps = nothing
                found = false
            elseif (m = match(re_uuid_to_string, line)) != nothing
                found = (into == UUID(m.captures[1]))
            elseif (m = match(re_deps_to_any, line)) != nothing
                deps = String(m.captures[1])
            elseif ismatch(re_subsection_deps, line)
                deps = Dict{String,UUID}()
                in_deps = true
            end
        else # in_deps
            if (m = match(re_key_to_string, line)) != nothing
                deps[m.captures[1]] = UUID(m.captures[2])
            elseif ismatch(re_section, line)
                found && break
                in_deps = false
            end
        end
    end
    found || return nothing
    deps isa String || return deps
    # TODO: handle inline table syntax
    if deps[1] != '[' || deps[end] != ']'
        @warn "Unexpected TOML deps format:\n$deps"
        return nothing
    end
    return map(m->m.captures[1], eachmatch(r"\"(.*?)\"", deps))
end

function manifest_file_path_and_uuid(manifest_file::AbstractString, what::Union{UUID,String})
    open(manifest_file) do io
        manifest_file_path_and_uuid(manifest_file, what, io)
    end
end

function manifest_file_path_and_uuid(manifest_file::AbstractString, uuid::UUID, io::IO)
    uuid′ = name = path = hash = nothing
    for line in eachline(io)
        if (m = match(re_section_capture, line)) != nothing
            uuid′ == uuid && break
            name = String(m.captures[1])
            path = hash = nothing
        elseif (m = match(re_uuid_to_string, line)) != nothing
            uuid′ = UUID(m.captures[1])
        elseif (m = match(re_path_to_string, line)) != nothing
            path = String(m.captures[1])
        elseif (m = match(re_hash_to_string, line)) != nothing
            hash = SHA1(m.captures[1])
        end
    end
    uuid′ == uuid || return nothing
    path != nothing && return normpath(abspath(dirname(manifest_file), path)), uuid
    hash == nothing && return nothing
    slug = version_slug(uuid, hash)
    for depot in DEPOT_PATH
        path = abspath(depot, "packages", slug)
        ispath(path) && return path, uuid
    end
end

function manifest_file_path_and_uuid(manifest_file::String, name::String, io::IO)
    uuid = name′ = path = hash = nothing
    for line in eachline(io)
        if (m = match(re_section_capture, line)) != nothing
            name′ == name && break
            name′ = String(m.captures[1])
            path = hash = nothing
        elseif (m = match(re_uuid_to_string, line)) != nothing
            uuid = UUID(m.captures[1])
        elseif (m = match(re_path_to_string, line)) != nothing
            path = String(m.captures[1])
        elseif (m = match(re_hash_to_string, line)) != nothing
            hash = SHA1(m.captures[1])
        end
    end
    name′ == name || return nothing
    path != nothing && return normpath(abspath(dirname(manifest_file), path)), uuid
    uuid == nothing && return nothing
    hash == nothing && return nothing
    slug = version_slug(uuid, hash)
    for depot in DEPOT_PATH
        path = abspath(depot, "packages", slug)
        ispath(path) && return path, uuid
    end
end

## other code loading functionality ##

function find_source_file(path::AbstractString)
    (isabspath(path) || isfile(path)) && return path
    base_path = joinpath(Sys.BINDIR, DATAROOTDIR, "julia", "base", path)
    return isfile(base_path) ? base_path : nothing
end

function find_all_in_cache_path(mod::Symbol)
    name = string(mod)
    paths = String[]
    for prefix in LOAD_CACHE_PATH
        path = joinpath(prefix, name*".ji")
        if isfile_casesensitive(path)
            push!(paths, path)
        end
    end
    return paths
end

# these return either the array of modules loaded from the path / content given
# or an Exception that describes why it couldn't be loaded
_include_from_serialized(into::Module, content::Vector{UInt8}, depmods::Vector{Module}) =
    ccall(:jl_restore_incremental_from_buf, Any,
          (Any, Ptr{UInt8}, Int, Any),
          into, content, sizeof(content), depmods)

_include_from_serialized(into::Module, path::String, depmods::Vector{Module}) =
    ccall(:jl_restore_incremental, Any,
          (Any, Cstring, Any),
          into, path, depmods)

# returns an array of modules loaded, or an Exception that describes why it failed
# and it reconnects the Base.Docs.META
function _require_from_serialized(into::Module, mod::Symbol, path_to_try::String)
    return _require_from_serialized(into, mod, path_to_try, parse_cache_header(path_to_try)[3])
end
function _require_from_serialized(into::Module, mod::Symbol, path_to_try::String, depmodnames::Vector{Pair{Symbol, UInt64}})
    # load all of the dependent modules
    ndeps = length(depmodnames)
    depmods = Vector{Module}(uninitialized, ndeps)
    for i in 1:ndeps
        modname, uuid = depmodnames[i]
        if root_module_exists(modname)
            M = root_module(modname)
            if module_name(M) === modname && module_uuid(M) === uuid
                depmods[i] = M
            end
        else
            info = find_package(into, string(modname))
            info === nothing && return ErrorException("Required dependency $modname not found in current path.")
            modpath = info isa String ? info : info[1]
            mod = _require_search_from_serialized(into, modname, String(modpath))
            if !isa(mod, Bool)
                for M in mod::Vector{Any}
                    if module_name(M) === modname && module_uuid(M) === uuid
                        depmods[i] = M
                        break
                    end
                end
                for callback in package_callbacks
                    invokelatest(callback, modname)
                end
            end
        end
        isassigned(depmods, i) || return ErrorException("Required dependency $modname failed to load from a cache file.")
    end
    # now load the path_to_try.ji file
    restored = _include_from_serialized(into, path_to_try, depmods)
    if !isa(restored, Exception)
        for M in restored::Vector{Any}
            M = M::Module
            if isdefined(M, Base.Docs.META)
                push!(Base.Docs.modules, M)
            end
            if module_parent(M) === M
                register_root_module(module_name(M), M)
            end
        end
    end
    return restored
end

# returns `true` if require found a precompile cache for this mod, but couldn't load it
# returns `false` if the module isn't known to be precompilable
# returns the set of modules restored if the cache load succeeded
function _require_search_from_serialized(into::Module, mod::Symbol, sourcepath::String)
    paths = find_all_in_cache_path(mod)
    for path_to_try in paths::Vector{String}
        deps = stale_cachefile(into, sourcepath, path_to_try)
        if deps === true
            continue
        end
        restored = _require_from_serialized(into, mod, path_to_try, deps)
        if isa(restored, Exception)
            if isa(restored, ErrorException)
                # can't use this cache due to a module uuid mismatch,
                # defer reporting error until after trying all of the possible matches
                @debug "Failed to load $path_to_try because $(restored.msg)"
                continue
            end
            @warn "Deserialization checks failed while attempting to load cache from $path_to_try"
            throw(restored)
        else
            return restored
        end
    end
    return !isempty(paths)
end

# to synchronize multiple tasks trying to import/using something
const package_locks = Dict{Symbol,Condition}()

# to notify downstream consumers that a module was successfully loaded
# Callbacks take the form (mod::Symbol) -> nothing.
# WARNING: This is an experimental feature and might change later, without deprecation.
const package_callbacks = Any[]
# to notify downstream consumers that a file has been included into a particular module
# Callbacks take the form (mod::Module, filename::String) -> nothing
# WARNING: This is an experimental feature and might change later, without deprecation.
const include_callbacks = Any[]

# used to optionally track dependencies when requiring a module:
const _concrete_dependencies = Pair{Symbol, UInt64}[] # these dependency versions are "set in stone", and the process should try to avoid invalidating them
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
    require(into::Module, module::Symbol)

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
    if _track_dependencies[]
        push!(_require_dependencies, (into, "\0$mod", 0.0))
    end
    info = nothing
    if !root_module_exists(mod)
        info = _require(into, mod)
        for callback in package_callbacks
            invokelatest(callback, mod)
        end
    end
    m = root_module(mod)
    if info isa Tuple
        uuid = info[2]
        if !isdefined(m, uuid_sym)
            ccall(:jl_set_const, Cvoid, (Any, Any, Any), m, uuid_sym, uuid)
        else
            uuid == (uuid′ = getfield(m, uuid_sym)) ||
                @warn """
                require($into, $mod) UUID mismatch:
                 - old = $(uuid′)
                 - new = $(uuid)
                """
        end
    end
    return m
end

const loaded_modules = ObjectIdDict()
const module_keys = ObjectIdDict()

function register_root_module(key, m::Module)
    if haskey(loaded_modules, key)
        oldm = loaded_modules[key]
        if oldm !== m
            name = module_name(oldm)
            @warn "Replacing module `$name`"
        end
    end
    loaded_modules[key] = m
    module_keys[m] = key
    nothing
end

register_root_module(:Core, Core)
register_root_module(:Base, Base)
register_root_module(:Main, Main)

is_root_module(m::Module) = haskey(module_keys, m)

root_module_key(m::Module) = module_keys[m]

# This is used as the current module when loading top-level modules.
# It has the special behavior that modules evaluated in it get added
# to the loaded_modules table instead of getting bindings.
baremodule __toplevel__
using Base
end

# get a top-level Module from the given key
# for now keys can only be Symbols, but that will change
root_module(key::Symbol) = loaded_modules[key]

root_module_exists(key::Symbol) = haskey(loaded_modules, key)

loaded_modules_array() = collect(values(loaded_modules))

function unreference_module(key)
    if haskey(loaded_modules, key)
        m = pop!(loaded_modules, key)
        # need to ensure all modules are GC rooted; will still be referenced
        # in module_keys
    end
end

function _require(into::Module, mod::Symbol)
    # handle recursive calls to require
    loading = get(package_locks, mod, false)
    if loading !== false
        # load already in progress for this module
        wait(loading)
        return
    end
    package_locks[mod] = Condition()

    last = toplevel_load[]
    try
        toplevel_load[] = false
        # perform the search operation to select the module file require intends to load
        name = string(mod)
        info = find_package(into, name)
        if info === nothing
            # TODO: package autoloading hooks go here
            throw(ArgumentError("Module $name not found in current path.\nRun `Pkg.add(\"$name\")` to install the $name package."))
        end
        path = info isa String ? info : String(info[1])

        # attempt to load the module file via the precompile cache locations
        doneprecompile = false
        if JLOptions().use_compiled_modules != 0
            doneprecompile = _require_search_from_serialized(into, mod, path)
            if !isa(doneprecompile, Bool)
                return info
            end
        end

        # if the module being required was supposed to have a particular version
        # but it was not handled by the precompile loader, complain
        for (concrete_mod, concrete_uuid) in _concrete_dependencies
            if mod === concrete_mod
                @warn """Module $mod with uuid $concrete_uuid is missing from the cache.
                     This may mean module $mod does not support precompilation but is imported by a module that does."""
                if JLOptions().incremental != 0
                    # during incremental precompilation, this should be fail-fast
                    throw(PrecompilableError(false))
                end
            end
        end

        if doneprecompile === true || JLOptions().incremental != 0
            # spawn off a new incremental pre-compile task for recursive `require` calls
            # or if the require search declared it was pre-compiled before (and therefore is expected to still be pre-compilable)
            cachefile = compilecache(into, mod)
            m = _require_from_serialized(into, mod, cachefile)
            if isa(m, Exception)
                @warn "The call to compilecache failed to create a usable precompiled cache file for module $name" exception=m
                # fall-through, TODO: disable __precompile__(true) error so that the normal include will succeed
            else
                return info
            end
        end

        # just load the file normally via include
        # for unknown dependencies
        if info isa Tuple
            # horrible hack, but since we don't control module creation, ¯\_(ツ)_/¯
            ccall(:jl_set_global, Cvoid, (Any, Any, Any), __toplevel__, uuid_sym, info[2])
        end
        try
            Base.include_relative(__toplevel__, path)
            return info
        catch ex
            if doneprecompile === true || JLOptions().use_compiled_modules == 0 || !precompilableerror(ex, true)
                rethrow() # rethrow non-precompilable=true errors
            end
            # the file requested `__precompile__`, so try to build a cache file and use that
            cachefile = compilecache(into, mod)
            m = _require_from_serialized(into, mod, cachefile)
            if isa(m, Exception)
                @warn """Module `$mod` declares `__precompile__(true)` but `require` failed
                         to create a usable precompiled cache file""" exception=m
                # TODO: disable __precompile__(true) error and do normal include instead of error
                error("Module $mod declares __precompile__(true) but require failed to create a usable precompiled cache file.")
            end
        finally
            if info isa Tuple
                ccall(:jl_set_global, Cvoid, (Any, Any, Any), __toplevel__, uuid_sym, nothing)
            end
        end
    finally
        toplevel_load[] = last
        loading = pop!(package_locks, mod)
        notify(loading, all=true)
    end
    return info
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

function tls_recurse(key::Symbol, default=nothing)
    t = current_task()
    while true
        s = t.storage
        s !== nothing && haskey(s, key) && return s[key]
        t === t.parent && return default
        t = t.parent
    end
end

source_path(default="") = tls_recurse(:SOURCE_PATH, default)

function source_dir()
    p = source_path(nothing)
    p === nothing ? pwd() : dirname(p)
end

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
include_relative(mod::Module, path::AbstractString) = include_relative(mod, String(path))

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

function create_expr_cache(
    input::String, output::String,
    concrete_deps::typeof(_concrete_dependencies),
    uuid::Union{Nothing,UUID})
    rm(output, force=true)   # Remove file if it exists
    code_object = """
        while !eof(STDIN)
            eval(Main, deserialize(STDIN))
        end
        """
    io = open(pipeline(detach(`$(julia_cmd()) -O0
                              --output-ji $output --output-incremental=yes
                              --startup-file=no --history-file=no --warn-overwrite=yes
                              --color=$(have_color ? "yes" : "no")
                              --eval $code_object`), stderr=STDERR),
              "w", STDOUT)
    in = io.in
    try
        serialize(in, quote
                  empty!(Base.LOAD_PATH)
                  append!(Base.LOAD_PATH, $LOAD_PATH)
                  empty!(Base.LOAD_CACHE_PATH)
                  append!(Base.LOAD_CACHE_PATH, $LOAD_CACHE_PATH)
                  empty!(Base.DL_LOAD_PATH)
                  append!(Base.DL_LOAD_PATH, $DL_LOAD_PATH)
                  empty!(Base._concrete_dependencies)
                  append!(Base._concrete_dependencies, $concrete_deps)
                  Base._track_dependencies[] = true
                  const $uuid_sym = $uuid
                  # same horrible hack as in _require
                  ccall(:jl_set_global, Cvoid, (Any, Any, Any),
                        Base.__toplevel__, $(Meta.quot(uuid_sym)), $uuid)
                  end)
        source = source_path(nothing)
        if source !== nothing
            serialize(in, quote
                      task_local_storage()[:SOURCE_PATH] = $(source)
                      end)
        end
        serialize(in, :(Base.include(Base.__toplevel__, $(abspath(input)))))
        if source !== nothing
            serialize(in, :(delete!(task_local_storage(), :SOURCE_PATH)))
        end
        close(in)
    catch ex
        close(in)
        process_running(io) && Timer(t -> kill(io), 5.0) # wait a short time before killing the process to give it a chance to clean up on its own first
        rethrow(ex)
    end
    return io
end

compilecache(into::Module, mod::Symbol) = compilecache(into, string(mod))

"""
    Base.compilecache(into::Module, module::String)

Creates a precompiled cache file for
a module and all of its dependencies.
This can be used to reduce package load times. Cache files are stored in
`LOAD_CACHE_PATH[1]`, which defaults to `~/.julia/lib/VERSION`. See
[Module initialization and precompilation](@ref)
for important notes.
"""
function compilecache(into::Module, name::String)
    # decide where to get the source file from
    info = find_package(into, name)
    info === nothing && throw(ArgumentError("$name not found in path"))
    path = info isa String ? info : String(info[1])
    # decide where to put the resulting cache file
    cachepath = LOAD_CACHE_PATH[1]
    if !isdir(cachepath)
        mkpath(cachepath)
    end
    cachefile::String = abspath(cachepath, "$name.ji")
    # build up the list of modules that we want the precompile process to preserve
    concrete_deps = copy(_concrete_dependencies)
    for (key, mod) in loaded_modules
        if !(mod === Main || mod === Core || mod === Base)
            push!(concrete_deps, key => module_uuid(mod))
        end
    end
    # run the expression and cache the result
    verbosity = isinteractive() ? CoreLogging.Info : CoreLogging.Debug
    if isfile(cachefile)
        @logmsg verbosity "Recompiling stale cache file $cachefile for module $name"
    else
        @logmsg verbosity "Precompiling module $name"
    end
    uuid = info isa Tuple ? info[2] : nothing
    if success(create_expr_cache(path, cachefile, concrete_deps, uuid))
        # append checksum to the end of the .ji file:
        open(cachefile, "a+") do f
            write(f, hton(_crc32c(seekstart(f))))
        end
    else
        error("Failed to precompile $name to $cachefile.")
    end
    return cachefile
end

module_uuid(m::Module) = ccall(:jl_module_uuid, UInt64, (Any,), m)

isvalid_cache_header(f::IOStream) = 0 != ccall(:jl_read_verify_header, Cint, (Ptr{Cvoid},), f.ios)

function parse_cache_header(f::IO)
    modules = Vector{Pair{Symbol, UInt64}}()
    while true
        n = ntoh(read(f, Int32))
        n == 0 && break
        sym = Symbol(read(f, n)) # module symbol
        uuid = ntoh(read(f, UInt64)) # module UUID (mostly just a timestamp)
        push!(modules, sym => uuid)
    end
    totbytes = ntoh(read(f, Int64)) # total bytes for file dependencies
    # read the list of requirements
    # and split the list into include and requires statements
    includes = Tuple{String, String, Float64}[]
    requires = Pair{String, String}[]
    while true
        n2 = ntoh(read(f, Int32))
        n2 == 0 && break
        depname = String(read(f, n2))
        mtime = ntoh(read(f, Float64))
        n1 = ntoh(read(f, Int32))
        if n1 == 0
            modname = "Main" # remap anything loaded outside this cache files modules to have occurred in `Main`
        else
            modname = String(modules[n1][1])
            while true
                n1 = ntoh(read(f, Int32))
                totbytes -= 4
                n1 == 0 && break
                modname = string(modname, ".", String(read(f, n1)))
                totbytes -= n1
            end
        end
        if depname[1] == '\0'
            push!(requires, modname => depname[2:end])
        else
            push!(includes, (modname, depname, mtime))
        end
        totbytes -= 4 + 4 + n2 + 8
    end
    @show includes
    @show requires
    @assert totbytes == 12 "header of cache file appears to be corrupt"
    srctextpos = ntoh(read(f, Int64))
    # read the list of modules that are required to be present during loading
    required_modules = Vector{Pair{Symbol, UInt64}}()
    while true
        n = ntoh(read(f, Int32))
        n == 0 && break
        sym = Symbol(read(f, n)) # module symbol
        uuid = ntoh(read(f, UInt64)) # module UUID
        push!(required_modules, sym => uuid)
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
        filenamelen = ntoh(read(io, Int32))
        filenamelen == 0 && break
        fn = String(read(io, filenamelen))
        len = ntoh(read(io, UInt64))
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
function stale_cachefile(into::Module, modpath::String, cachefile::String)
    io = open(cachefile, "r")
    try
        if !isvalid_cache_header(io)
            @debug "Rejecting cache file $cachefile due to it containing an invalid cache header"
            return true # invalid cache file
        end
        (modules, (includes, requires), required_modules) = parse_cache_header(io)
        modules = Dict{Symbol, UInt64}(modules)

        # Check if transitive dependencies can be fullfilled
        for (mod, uuid_req) in required_modules
            # Module is already loaded
            if root_module_exists(mod)
                continue
            end
            name = string(mod)
            info = find_package(into, name)
            if info === nothing
                @debug "Rejecting cache file $cachefile because dependency $name not found."
                return true # Won't be able to fullfill dependency
            end
        end

        # check if this file is going to provide one of our concrete dependencies
        # or if it provides a version that conflicts with our concrete dependencies
        # or neither
        for (mod, uuid_req) in _concrete_dependencies
            uuid = get(modules, mod, UInt64(0))
            if uuid !== UInt64(0)
                if uuid === uuid_req
                    return required_modules # this is the file we want
                end
                @debug "Rejecting cache file $cachefile because it provides the wrong uuid (got $uuid) for $mod (want $uuid_req)"
                return true # cachefile doesn't provide the required version of the dependency
            end
        end

        # now check if this file is fresh relative to its source files
        if !samefile(includes[1][2], modpath)
            @debug "Rejecting cache file $cachefile because it is for file $(includes[1][2])) not file $modpath"
            return true # cache file was compiled from a different path
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

        # finally, verify that the cache file has a valid checksum
        crc = _crc32c(seekstart(io), filesize(io)-4)
        if crc != ntoh(read(io, UInt32))
            @debug "Rejecting cache file $cachefile because it has an invalid checksum"
            return true
        end

        return required_modules # fresh cachefile
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
