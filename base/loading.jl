# This file is a part of Julia. License is MIT: https://julialang.org/license

# Base.require is the implementation for the `import` statement
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

## SHA1 ##

struct SHA1
    bytes::NTuple{20, UInt8}
end
function SHA1(bytes::Vector{UInt8})
    length(bytes) == 20 ||
        throw(ArgumentError("wrong number of bytes for SHA1 hash: $(length(bytes))"))
    return SHA1(ntuple(i->bytes[i], Val(20)))
end
SHA1(s::AbstractString) = SHA1(hex2bytes(s))
parse(::Type{SHA1}, s::AbstractString) = SHA1(s)
function tryparse(::Type{SHA1}, s::AbstractString)
    try
        return parse(SHA1, s)
    catch e
        if isa(e, ArgumentError)
            return nothing
        end
        rethrow(e)
    end
end

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
    @lock require_lock begin
    cache = LOADING_CACHE[]
    if cache !== nothing
        uuid = get(cache.dummy_uuid, project_file, nothing)
        uuid === nothing || return uuid
    end
    project_path = try
        realpath(project_file)
    catch ex
        ex isa IOError || rethrow()
        project_file
    end
    uuid = uuid5(ns_dummy_uuid, project_path)
    if cache !== nothing
        cache.dummy_uuid[project_file] = uuid
    end
    return uuid
    end
end

## package path slugs: turning UUID + SHA1 into a pair of 4-byte "slugs" ##

const slug_chars = String(['A':'Z'; 'a':'z'; '0':'9'])

function slug(x::UInt32, p::Int)
    y::UInt32 = x
    sprint(sizehint=p) do io
        n = length(slug_chars)
        for i = 1:p
            y, d = divrem(y, n)
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

struct LoadingCache
    load_path::Vector{String}
    dummy_uuid::Dict{String, UUID}
    env_project_file::Dict{String, Union{Bool, String}}
    project_file_manifest_path::Dict{String, Union{Nothing, String}}
    require_parsed::Set{String}
    identified_where::Dict{Tuple{PkgId, String}, Union{Nothing, Tuple{PkgId, Union{Nothing, String}}}}
    identified::Dict{String, Union{Nothing, Tuple{PkgId, Union{Nothing, String}}}}
    located::Dict{Tuple{PkgId, Union{String, Nothing}}, Union{Tuple{Union{String, Nothing}, Union{String, Nothing}}, Nothing}}
end
const LOADING_CACHE = Ref{Union{LoadingCache, Nothing}}(nothing)
LoadingCache() = LoadingCache(load_path(), Dict(), Dict(), Dict(), Set(), Dict(), Dict(), Dict())


struct TOMLCache
    p::TOML.Parser
    d::Dict{String, CachedTOMLDict}
end
const TOML_CACHE = TOMLCache(TOML.Parser(), Dict{String, Dict{String, Any}}())

parsed_toml(project_file::AbstractString) = parsed_toml(project_file, TOML_CACHE, require_lock)
function parsed_toml(project_file::AbstractString, toml_cache::TOMLCache, toml_lock::ReentrantLock)
    lock(toml_lock) do
        cache = LOADING_CACHE[]
        dd = if !haskey(toml_cache.d, project_file)
            d = CachedTOMLDict(toml_cache.p, project_file)
            toml_cache.d[project_file] = d
            d.d
        else
            d = toml_cache.d[project_file]
            # We are in a require call and have already parsed this TOML file
            # assume that it is unchanged to avoid hitting disk
            if cache !== nothing && project_file in cache.require_parsed
                d.d
            else
                get_updated_dict(toml_cache.p, d)
            end
        end
        if cache !== nothing
            push!(cache.require_parsed, project_file)
        end
        return dd
    end
end

## package identification: determine unique identity of package to be loaded ##

# Used by Pkg but not used in loading itself
function find_package(arg)
    pkgenv = identify_package_env(arg)
    pkgenv === nothing && return nothing
    pkg, env = pkgenv
    return locate_package(pkg, env)
end

"""
    Base.identify_package_env(name::String)::Union{Tuple{PkgId, String}, Nothing}
    Base.identify_package_env(where::Union{Module,PkgId}, name::String)::Union{Tuple{PkgId, String} Nothing}

Same as [`Base.identify_package`](@ref) except that the path to the environment where the package is identified
is also returned.
"""
identify_package_env(where::Module, name::String) = identify_package_env(PkgId(where), name)
function identify_package_env(where::PkgId, name::String)
    cache = LOADING_CACHE[]
    if cache !== nothing
        pkg_env = get(cache.identified_where, (where, name), nothing)
        pkg_env === nothing || return pkg_env
    end
    pkg_env = nothing
    if where.name === name
        pkg_env = where, nothing
    elseif where.uuid === nothing
        pkg_env = identify_package_env(name) # ignore `where`
    else
        for env in load_path()
            pkgid = manifest_deps_get(env, where, name)
            pkgid === nothing && continue # not found--keep looking
            if pkgid.uuid !== nothing
                pkg_env = pkgid, env # found in explicit environment--use it
            end
            break # found in implicit environment--return "not found"
        end
    end
    if cache !== nothing
        cache.identified_where[(where, name)] = pkg_env
    end
    return pkg_env
end
function identify_package_env(name::String)
    cache = LOADING_CACHE[]
    if cache !== nothing
        pkg_env = get(cache.identified, name, nothing)
        pkg_env === nothing || return pkg_env
    end
    pkg_env = nothing
    for env in load_path()
        pkg = project_deps_get(env, name)
        if pkg !== nothing
            pkg_env = pkg, env # found--return it
            break
        end
    end
    if cache !== nothing
        cache.identified[name] = pkg_env
    end
    return pkg_env
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

```julia-repl
julia> Base.identify_package("Pkg") # Pkg is a dependency of the default environment
Pkg [44cfe95a-1eb2-52ea-b672-e2afdf69b78f]

julia> using LinearAlgebra

julia> Base.identify_package(LinearAlgebra, "Pkg") # Pkg is not a dependency of LinearAlgebra
```
"""
identify_package(where::Module, name::String) = _nothing_or_first(identify_package_env(where, name))
identify_package(where::PkgId, name::String)  = _nothing_or_first(identify_package_env(where, name))
identify_package(name::String)                = _nothing_or_first(identify_package_env(name))

function locate_package_env(pkg::PkgId, stopenv::Union{String, Nothing}=nothing)
    cache = LOADING_CACHE[]
    if cache !== nothing
        pathenv = get(cache.located, (pkg, stopenv), nothing)
        pathenv === nothing || return pathenv
    end
    path = nothing
    env′ = nothing
    if pkg.uuid === nothing
        for env in load_path()
            env′ = env
            # look for the toplevel pkg `pkg.name` in this entry
            found = project_deps_get(env, pkg.name)
            if found !== nothing
                @assert found.name == pkg.name
                if found.uuid === nothing
                    # pkg.name is present in this directory or project file,
                    # return the path the entry point for the code, if it could be found
                    # otherwise, signal failure
                    path = implicit_manifest_uuid_path(env, pkg)
                    @goto done
                end
            end
            if !(loading_extension || precompiling_extension)
                stopenv == env && @goto done
            end
        end
    else
        for env in load_path()
            env′ = env
            path = manifest_uuid_path(env, pkg)
            # missing is used as a sentinel to stop looking further down in envs
            if path === missing
                path = nothing
                @goto done
            end
            if path !== nothing
                path = entry_path(path, pkg.name)
                @goto done
            end
            if !(loading_extension || precompiling_extension)
                stopenv == env && break
            end
        end
        # Allow loading of stdlibs if the name/uuid are given
        # e.g. if they have been explicitly added to the project/manifest
        mbypath = manifest_uuid_path(Sys.STDLIB, pkg)
        if mbypath isa String
            path = entry_path(mbypath, pkg.name)
            @goto done
        end
    end
    @label done
    if cache !== nothing
        cache.located[(pkg, stopenv)] = path, env′
    end
    return path, env′
end

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
    _nothing_or_first(locate_package_env(pkg, stopenv))
end

"""
    pathof(m::Module)

Return the path of the `m.jl` file that was used to `import` module `m`,
or `nothing` if `m` was not imported from a package.

Use [`dirname`](@ref) to get the directory part and [`basename`](@ref)
to get the file name part of the path.

See also [`pkgdir`](@ref).
"""
function pathof(m::Module)
    @lock require_lock begin
    pkgid = get(module_keys, m, nothing)
    pkgid === nothing && return nothing
    origin = get(pkgorigins, pkgid, nothing)
    origin === nothing && return nothing
    path = origin.path
    path === nothing && return nothing
    return fixup_stdlib_path(path)
    end
end

"""
    pkgdir(m::Module[, paths::String...])

Return the root directory of the package that declared module `m`,
or `nothing` if `m` was not declared in a package. Optionally further
path component strings can be provided to construct a path within the
package root.

To get the root directory of the package that implements the current module
the form `pkgdir(@__MODULE__)` can be used.

```julia-repl
julia> pkgdir(Foo)
"/path/to/Foo.jl"

julia> pkgdir(Foo, "src", "file.jl")
"/path/to/Foo.jl/src/file.jl"
```

See also [`pathof`](@ref).

!!! compat "Julia 1.7"
    The optional argument `paths` requires at least Julia 1.7.
"""
function pkgdir(m::Module, paths::String...)
    rootmodule = moduleroot(m)
    path = pathof(rootmodule)
    path === nothing && return nothing
    return joinpath(dirname(dirname(path)), paths...)
end

function get_pkgversion_from_path(path)
    project_file = locate_project_file(path)
    if project_file isa String
        d = parsed_toml(project_file)
        v = get(d, "version", nothing)
        if v !== nothing
            return VersionNumber(v::String)
        end
    end
    return nothing
end

"""
    pkgversion(m::Module)

Return the version of the package that imported module `m`,
or `nothing` if `m` was not imported from a package, or imported
from a package without a version field set.

The version is read from the package's Project.toml during package
load.

To get the version of the package that imported the current module
the form `pkgversion(@__MODULE__)` can be used.

!!! compat "Julia 1.9"
    This function was introduced in Julia 1.9.
"""
function pkgversion(m::Module)
    path = pkgdir(m)
    path === nothing && return nothing
    @lock require_lock begin
        v = get_pkgversion_from_path(path)
        pkgorigin = get(pkgorigins, PkgId(moduleroot(m)), nothing)
        # Cache the version
        if pkgorigin !== nothing && pkgorigin.version === nothing
            pkgorigin.version = v
        end
        return v
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
function env_project_file(env::String)::Union{Bool,String}
    @lock require_lock begin
    cache = LOADING_CACHE[]
    if cache !== nothing
        project_file = get(cache.env_project_file, env, nothing)
        project_file === nothing || return project_file
    end
    if isdir(env)
        project_file = locate_project_file(env)
    elseif basename(env) in project_names && isfile_casesensitive(env)
        project_file = env
    else
        project_file = false
    end
    if cache !== nothing
        cache.env_project_file[env] = project_file
    end
    return project_file
    end
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
    uuid = where.uuid
    @assert uuid !== nothing
    project_file = env_project_file(env)
    if project_file isa String
        # first check if `where` names the Project itself
        proj = project_file_name_uuid(project_file, where.name)
        if proj == where
            # if `where` matches the project, use [deps] section as manifest, and stop searching
            pkg_uuid = explicit_project_deps_get(project_file, name)
            return PkgId(pkg_uuid, name)
        end
        d = parsed_toml(project_file)
        exts = get(d, "extensions", nothing)::Union{Dict{String, Any}, Nothing}
        if exts !== nothing
            # Check if `where` is an extension of the project
            if where.name in keys(exts) && where.uuid == uuid5(proj.uuid::UUID, where.name)
                # Extensions can load weak deps...
                weakdeps = get(d, "weakdeps", nothing)::Union{Dict{String, Any}, Nothing}
                if weakdeps !== nothing
                    wuuid = get(weakdeps, name, nothing)::Union{String, Nothing}
                    if wuuid !== nothing
                        return PkgId(UUID(wuuid), name)
                    end
                end
                # ... and they can load same deps as the project itself
                mby_uuid = explicit_project_deps_get(project_file, name)
                mby_uuid === nothing || return PkgId(mby_uuid, name)
            end
        end
        # look for manifest file and `where` stanza
        return explicit_manifest_deps_get(project_file, where, name)
    elseif project_file
        # if env names a directory, search it
        return implicit_manifest_deps_get(env, where, name)
    end
    return nothing
end

function manifest_uuid_path(env::String, pkg::PkgId)::Union{Nothing,String,Missing}
    project_file = env_project_file(env)
    if project_file isa String
        proj = project_file_name_uuid(project_file, pkg.name)
        if proj == pkg
            # if `pkg` matches the project, return the project itself
            return project_file_path(project_file)
        end
        mby_ext = project_file_ext_path(project_file, pkg.name)
        mby_ext === nothing || return mby_ext
        # look for manifest file and `where` stanza
        return explicit_manifest_uuid_path(project_file, pkg)
    elseif project_file
        # if env names a directory, search it
        proj = implicit_manifest_uuid_path(env, pkg)
        proj === nothing || return proj
        # if not found
        parentid = get(EXT_PRIMED, pkg, nothing)
        if parentid !== nothing
            _, parent_project_file = entry_point_and_project_file(env, parentid.name)
            if parent_project_file !== nothing
                parentproj = project_file_name_uuid(parent_project_file, parentid.name)
                if parentproj == parentid
                    mby_ext = project_file_ext_path(parent_project_file, pkg.name)
                    mby_ext === nothing || return mby_ext
                end
            end
        end
    end
    return nothing
end


function find_ext_path(project_path::String, extname::String)
    extfiledir = joinpath(project_path, "ext", extname, extname * ".jl")
    isfile(extfiledir) && return extfiledir
    return joinpath(project_path, "ext", extname * ".jl")
end

function project_file_ext_path(project_file::String, name::String)
    d = parsed_toml(project_file)
    p = project_file_path(project_file)
    exts = get(d, "extensions", nothing)::Union{Dict{String, Any}, Nothing}
    if exts !== nothing
        if name in keys(exts)
            return find_ext_path(p, name)
        end
    end
    return nothing
end

# find project file's top-level UUID entry (or nothing)
function project_file_name_uuid(project_file::String, name::String)::PkgId
    d = parsed_toml(project_file)
    uuid′ = get(d, "uuid", nothing)::Union{String, Nothing}
    uuid = uuid′ === nothing ? dummy_uuid(project_file) : UUID(uuid′)
    name = get(d, "name", name)::String
    return PkgId(uuid, name)
end

function project_file_path(project_file::String)
    d = parsed_toml(project_file)
    joinpath(dirname(project_file), get(d, "path", "")::String)
end

# find project file's corresponding manifest file
function project_file_manifest_path(project_file::String)::Union{Nothing,String}
    @lock require_lock begin
    cache = LOADING_CACHE[]
    if cache !== nothing
        manifest_path = get(cache.project_file_manifest_path, project_file, missing)
        manifest_path === missing || return manifest_path
    end
    dir = abspath(dirname(project_file))
    d = parsed_toml(project_file)
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
    if cache !== nothing
        cache.project_file_manifest_path[project_file] = manifest_path
    end
    return manifest_path
    end
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
    dir_name = joinpath(dir, name)
    path, project_file = entry_point_and_project_file_inside(dir_name, name)
    path === nothing || return path, project_file
    dir_jl = dir_name * ".jl"
    path, project_file = entry_point_and_project_file_inside(dir_jl, name)
    path === nothing || return path, project_file
    return nothing, nothing
end

# Find the project file for the extension `ext` in the implicit env `dir``
function implicit_env_project_file_extension(dir::String, ext::PkgId)
    for pkg in readdir(dir; join=true)
        project_file = env_project_file(pkg)
        project_file isa String || continue
        proj = project_file_name_uuid(project_file, "")
        uuid5(proj.uuid, ext.name) == ext.uuid || continue
        path = project_file_ext_path(project_file, ext.name)
        if path !== nothing
            return path, project_file
        end
    end
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
# `ext` is the name of the extension if `name` is loaded from one
# return `nothing` if `name` is not found
function explicit_project_deps_get(project_file::String, name::String, ext::Union{String,Nothing}=nothing)::Union{Nothing,UUID}
    d = parsed_toml(project_file)
    if get(d, "name", nothing)::Union{String, Nothing} === name
        root_uuid = dummy_uuid(project_file)
        uuid = get(d, "uuid", nothing)::Union{String, Nothing}
        return uuid === nothing ? root_uuid : UUID(uuid)
    end
    deps = get(d, "deps", nothing)::Union{Dict{String, Any}, Nothing}
    if deps !== nothing
        uuid = get(deps, name, nothing)::Union{String, Nothing}
        uuid === nothing || return UUID(uuid)
    end
    if ext !== nothing
        extensions = get(d, "extensions", nothing)
        extensions === nothing && return nothing
        ext_data = get(extensions, ext, nothing)
        ext_data === nothing && return nothing
        if (ext_data isa String && name == ext_data) || (ext_data isa Vector{String} && name in ext_data)
            weakdeps = get(d, "weakdeps", nothing)::Union{Dict{String, Any}, Nothing}
            weakdeps === nothing && return nothing
            wuuid = get(weakdeps, name, nothing)::Union{String, Nothing}
            wuuid === nothing && return nothing
            return UUID(wuuid)
        end
    end
    return nothing
end

function is_v1_format_manifest(raw_manifest::Dict{String})
    if haskey(raw_manifest, "manifest_format")
        mf = raw_manifest["manifest_format"]
        if mf isa Dict{String} && haskey(mf, "uuid")
            # the off-chance where an old format manifest has a dep called "manifest_format"
            return true
        end
        return false
    else
        return true
    end
end

# returns a deps list for both old and new manifest formats
function get_deps(raw_manifest::Dict)
    if is_v1_format_manifest(raw_manifest)
        return raw_manifest
    else
        # if the manifest has no deps, there won't be a `deps` field
        return get(Dict{String, Any}, raw_manifest, "deps")::Dict{String, Any}
    end
end

# find `where` stanza and return the PkgId for `name`
# return `nothing` if it did not find `where` (indicating caller should continue searching)
function explicit_manifest_deps_get(project_file::String, where::PkgId, name::String)::Union{Nothing,PkgId}
    manifest_file = project_file_manifest_path(project_file)
    manifest_file === nothing && return nothing # manifest not found--keep searching LOAD_PATH
    d = get_deps(parsed_toml(manifest_file))
    found_where = false
    found_name = false
    for (dep_name, entries) in d
        entries::Vector{Any}
        for entry in entries
            entry = entry::Dict{String, Any}
            uuid = get(entry, "uuid", nothing)::Union{String, Nothing}
            uuid === nothing && continue
            if UUID(uuid) === where.uuid
                found_where = true
                # deps is either a list of names (deps = ["DepA", "DepB"]) or
                # a table of entries (deps = {"DepA" = "6ea...", "DepB" = "55d..."}
                deps = get(entry, "deps", nothing)::Union{Vector{String}, Dict{String, Any}, Nothing}
                if deps isa Vector{String}
                    found_name = name in deps
                    break
                elseif deps isa Dict{String, Any}
                    deps = deps::Dict{String, Any}
                    for (dep, uuid) in deps
                        uuid::String
                        if dep === name
                            return PkgId(UUID(uuid), name)
                        end
                    end
                end
            else # Check for extensions
                extensions = get(entry, "extensions", nothing)
                if extensions !== nothing
                    if haskey(extensions, where.name) && where.uuid == uuid5(UUID(uuid), where.name)
                        found_where = true
                        if name == dep_name
                            return PkgId(UUID(uuid), name)
                        end
                        exts = extensions[where.name]::Union{String, Vector{String}}
                        if (exts isa String && name == exts) || (exts isa Vector{String} && name in exts)
                            weakdeps = get(entry, "weakdeps", nothing)::Union{Vector{String}, Dict{String, Any}, Nothing}
                            if weakdeps !== nothing
                                if weakdeps isa Vector{String}
                                    found_name = name in weakdeps
                                    break
                                elseif weakdeps isa Dict{String, Any}
                                    weakdeps = weakdeps::Dict{String, Any}
                                    for (dep, uuid) in weakdeps
                                        uuid::String
                                        if dep === name
                                            return PkgId(UUID(uuid), name)
                                        end
                                    end
                                end
                            end
                        end
                        # `name` is not an ext, do standard lookup as if this was the parent
                        return identify_package(PkgId(UUID(uuid), dep_name), name)
                    end
                end
            end
        end
    end
    found_where || return nothing
    found_name || return PkgId(name)
    # Only reach here if deps was not a dict which mean we have a unique name for the dep
    name_deps = get(d, name, nothing)::Union{Nothing, Vector{Any}}
    if name_deps === nothing || length(name_deps) != 1
        error("expected a single entry for $(repr(name)) in $(repr(project_file))")
    end
    entry = first(name_deps::Vector{Any})::Dict{String, Any}
    uuid = get(entry, "uuid", nothing)::Union{String, Nothing}
    uuid === nothing && return nothing
    return PkgId(UUID(uuid), name)
end

# find `uuid` stanza, return the corresponding path
function explicit_manifest_uuid_path(project_file::String, pkg::PkgId)::Union{Nothing,String,Missing}
    manifest_file = project_file_manifest_path(project_file)
    manifest_file === nothing && return nothing # no manifest, skip env

    d = get_deps(parsed_toml(manifest_file))
    entries = get(d, pkg.name, nothing)::Union{Nothing, Vector{Any}}
    if entries !== nothing
        for entry in entries
            entry = entry::Dict{String, Any}
            uuid = get(entry, "uuid", nothing)::Union{Nothing, String}
            uuid === nothing && continue
            if UUID(uuid) === pkg.uuid
                return explicit_manifest_entry_path(manifest_file, pkg, entry)
            end
        end
    end
    # Extensions
    for (name, entries) in d
        entries = entries::Vector{Any}
        for entry in entries
            uuid = get(entry, "uuid", nothing)::Union{Nothing, String}
            extensions = get(entry, "extensions", nothing)::Union{Nothing, Dict{String, Any}}
            if extensions !== nothing && haskey(extensions, pkg.name) && uuid !== nothing && uuid5(UUID(uuid), pkg.name) == pkg.uuid
                parent_path = locate_package(PkgId(UUID(uuid), name))
                if parent_path === nothing
                    error("failed to find source of parent package: \"$name\"")
                end
                p = normpath(dirname(parent_path), "..")
                return find_ext_path(p, pkg.name)
            end
        end
    end
    return nothing
end

function explicit_manifest_entry_path(manifest_file::String, pkg::PkgId, entry::Dict{String,Any})
    path = get(entry, "path", nothing)::Union{Nothing, String}
    if path !== nothing
        path = normpath(abspath(dirname(manifest_file), path))
        return path
    end
    hash = get(entry, "git-tree-sha1", nothing)::Union{Nothing, String}
    if hash === nothing
        return joinpath(Sys.STDLIB, pkg.name)
    end
    hash = SHA1(hash)
    # Keep the 4 since it used to be the default
    uuid = pkg.uuid::UUID # checked within `explicit_manifest_uuid_path`
    for slug in (version_slug(uuid, hash), version_slug(uuid, hash, 4))
        for depot in DEPOT_PATH
            path = joinpath(depot, "packages", pkg.name, slug)
            ispath(path) && return abspath(path)
        end
    end
    # no depot contains the package, return missing to stop looking
    return missing
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
    if project_file === nothing
        # `where` could be an extension
        project_file = implicit_env_project_file_extension(dir, where)[2]
        project_file === nothing && return nothing
    end
    proj = project_file_name_uuid(project_file, where.name)
    ext = nothing
    if proj !== where
        # `where` could be an extension in `proj`
        d = parsed_toml(project_file)
        exts = get(d, "extensions", nothing)::Union{Dict{String, Any}, Nothing}
        if exts !== nothing && where.name in keys(exts)
            if where.uuid !== uuid5(proj.uuid, where.name)
                return nothing
            end
            ext = where.name
        else
            return nothing
        end
    end
    # this is the correct project, so stop searching here
    pkg_uuid = explicit_project_deps_get(project_file, name, ext)
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
    base_path = joinpath(Sys.BINDIR, DATAROOTDIR, "julia", "base", path)
    return isfile(base_path) ? normpath(base_path) : nothing
end

function cache_file_entry(pkg::PkgId)
    uuid = pkg.uuid
    return joinpath(
        "compiled",
        "v$(VERSION.major).$(VERSION.minor)",
        uuid === nothing ? ""       : pkg.name),
        uuid === nothing ? pkg.name : package_slug(uuid)
end

# for use during running the REPL precompilation subprocess script, given we don't
# want it to pick up caches that already exist for other optimization levels
const ignore_compiled_cache = PkgId[]

function find_all_in_cache_path(pkg::PkgId)
    paths = String[]
    pkg in ignore_compiled_cache && return paths
    entrypath, entryfile = cache_file_entry(pkg)
    for path in joinpath.(DEPOT_PATH, entrypath)
        isdir(path) || continue
        for file in readdir(path, sort = false) # no sort given we sort later
            if !((pkg.uuid === nothing && file == entryfile * ".ji") ||
                 (pkg.uuid !== nothing && startswith(file, entryfile * "_") &&
                  endswith(file, ".ji")))
                 continue
            end
            filepath = joinpath(path, file)
            isfile_casesensitive(filepath) && push!(paths, filepath)
        end
    end
    if length(paths) > 1
        function sort_by(path)
            # when using pkgimages, consider those cache files first
            pkgimage = if JLOptions().use_pkgimages != 0
                io = open(path, "r")
                try
                    if iszero(isvalid_cache_header(io))
                        false
                    else
                        _, _, _, _, _, _, _, flags = parse_cache_header(io, path)
                        CacheFlags(flags).use_pkgimages
                    end
                finally
                    close(io)
                end
            else
                false
            end
            (; pkgimage, mtime=mtime(path))
        end
        function sort_lt(a, b)
            if a.pkgimage != b.pkgimage
                return a.pkgimage < b.pkgimage
            end
            return a.mtime < b.mtime
        end

        # allocating the sort vector is less expensive than using sort!(.. by=sort_by),
        # which would call the relatively slow mtime multiple times per path
        p = sortperm(sort_by.(paths), lt=sort_lt, rev=true)
        return paths[p]
    else
        return paths
    end
end

ocachefile_from_cachefile(cachefile) = string(chopsuffix(cachefile, ".ji"), ".", Libc.Libdl.dlext)
cachefile_from_ocachefile(cachefile) = string(chopsuffix(cachefile, ".$(Libc.Libdl.dlext)"), ".ji")


# use an Int counter so that nested @time_imports calls all remain open
const TIMING_IMPORTS = Threads.Atomic{Int}(0)

# these return either the array of modules loaded from the path / content given
# or an Exception that describes why it couldn't be loaded
# and it reconnects the Base.Docs.META
function _include_from_serialized(pkg::PkgId, path::String, ocachepath::Union{Nothing, String}, depmods::Vector{Any}, ignore_native::Union{Nothing,Bool}=nothing)
    if isnothing(ignore_native)
        if JLOptions().code_coverage == 0 && JLOptions().malloc_log == 0
            ignore_native = false
        else
            io = open(path, "r")
            try
                iszero(isvalid_cache_header(io)) && return ArgumentError("Invalid header in cache file $path.")
                _, (includes, _, _), _, _, _, _, _, _ = parse_cache_header(io, path)
                ignore_native = pkg_tracked(includes)
            finally
                close(io)
            end
        end
    end
    assert_havelock(require_lock)
    timing_imports = TIMING_IMPORTS[] > 0
    try
    if timing_imports
        t_before = time_ns()
        cumulative_compile_timing(true)
        t_comp_before = cumulative_compile_time_ns()
    end

    if ocachepath !== nothing
        @debug "Loading object cache file $ocachepath for $(repr("text/plain", pkg))"
        sv = ccall(:jl_restore_package_image_from_file, Any, (Cstring, Any, Cint, Cstring, Cint), ocachepath, depmods, false, pkg.name, ignore_native)
    else
        @debug "Loading cache file $path for $(repr("text/plain", pkg))"
        sv = ccall(:jl_restore_incremental, Any, (Cstring, Any, Cint, Cstring), path, depmods, false, pkg.name)
    end
    if isa(sv, Exception)
        return sv
    end

    restored = register_restored_modules(sv, pkg, path)

    for M in restored
        M = M::Module
        if parentmodule(M) === M && PkgId(M) == pkg
            if timing_imports
                elapsed = round((time_ns() - t_before) / 1e6, digits = 1)
                comp_time, recomp_time = cumulative_compile_time_ns() .- t_comp_before
                print(lpad(elapsed, 9), " ms  ")
                parentid = get(EXT_PRIMED, pkg, nothing)
                if parentid !== nothing
                    print(parentid.name, " → ")
                end
                print(pkg.name)
                if comp_time > 0
                    printstyled(" ", Ryu.writefixed(Float64(100 * comp_time / (elapsed * 1e6)), 2), "% compilation time", color = Base.info_color())
                end
                if recomp_time > 0
                    perc = Float64(100 * recomp_time / comp_time)
                    printstyled(" (", perc < 1 ? "<1" : Ryu.writefixed(perc, 0), "% recompilation)", color = Base.warn_color())
                end
                println()
            end
            return M
        end
    end
    return ErrorException("Required dependency $(repr("text/plain", pkg)) failed to load from a cache file.")

    finally
        timing_imports && cumulative_compile_timing(false)
    end
end

function register_restored_modules(sv::SimpleVector, pkg::PkgId, path::String)
    # This function is also used by PkgCacheInspector.jl
    restored = sv[1]::Vector{Any}
    for M in restored
        M = M::Module
        if isdefined(M, Base.Docs.META) && getfield(M, Base.Docs.META) !== nothing
            push!(Base.Docs.modules, M)
        end
        if parentmodule(M) === M
            register_root_module(M)
        end
    end

    # Register this cache path now - If Requires.jl is loaded, Revise may end
    # up looking at the cache path during the init callback.
    get!(PkgOrigin, pkgorigins, pkg).cachepath = path

    inits = sv[2]::Vector{Any}
    if !isempty(inits)
        unlock(require_lock) # temporarily _unlock_ during these callbacks
        try
            for (i, mod) in pairs(inits)
                run_module_init(mod, i)
            end
        finally
            lock(require_lock)
        end
    end
    return restored
end

function run_module_init(mod::Module, i::Int=1)
    # `i` informs ordering for the `@time_imports` report formatting
    if TIMING_IMPORTS[] == 0
        ccall(:jl_init_restored_module, Cvoid, (Any,), mod)
    else
        if isdefined(mod, :__init__)
            connector = i > 1 ? "├" : "┌"
            printstyled("               $connector ", color = :light_black)

            elapsedtime = time_ns()
            cumulative_compile_timing(true)
            compile_elapsedtimes = cumulative_compile_time_ns()

            ccall(:jl_init_restored_module, Cvoid, (Any,), mod)

            elapsedtime = (time_ns() - elapsedtime) / 1e6
            cumulative_compile_timing(false);
            comp_time, recomp_time = (cumulative_compile_time_ns() .- compile_elapsedtimes) ./ 1e6

            print("$(round(elapsedtime, digits=1)) ms $mod.__init__() ")
            if comp_time > 0
                printstyled(Ryu.writefixed(Float64(100 * comp_time / elapsedtime), 2), "% compilation time", color = Base.info_color())
            end
            if recomp_time > 0
                perc = Float64(100 * recomp_time / comp_time)
                printstyled(" ($(perc < 1 ? "<1" : Ryu.writefixed(perc, 0))% recompilation)", color = Base.warn_color())
            end
            println()
        end
    end
end

function run_package_callbacks(modkey::PkgId)
    run_extension_callbacks(modkey)
    assert_havelock(require_lock)
    unlock(require_lock)
    try
        for callback in package_callbacks
            invokelatest(callback, modkey)
        end
    catch
        # Try to continue loading if a callback errors
        errs = current_exceptions()
        @error "Error during package callback" exception=errs
    finally
        lock(require_lock)
    end
    nothing
end


##############
# Extensions #
##############

mutable struct ExtensionId
    const id::PkgId
    const parentid::PkgId # just need the name, for printing
    ntriggers::Int # how many more packages must be defined until this is loaded
end

const EXT_PRIMED = Dict{PkgId, PkgId}() # Extension -> Parent
const EXT_DORMITORY = Dict{PkgId,Vector{ExtensionId}}() # Trigger -> Extensions that can be triggered by it
const EXT_DORMITORY_FAILED = ExtensionId[]

function insert_extension_triggers(pkg::PkgId)
    pkg.uuid === nothing && return
    path_env_loc = locate_package_env(pkg)
    path_env_loc === nothing && return
    path, env_loc = path_env_loc
    if path === nothing || env_loc === nothing
        return
    end
    insert_extension_triggers(env_loc, pkg)
end

function insert_extension_triggers(env::String, pkg::PkgId)::Union{Nothing,Missing}
    project_file = env_project_file(env)
    if project_file isa String || project_file
        implicit_project_file = project_file
        if !(implicit_project_file isa String)
            # if env names a directory, search it for an implicit project file (for stdlibs)
            path, implicit_project_file = entry_point_and_project_file(env, pkg.name)
            if !(implicit_project_file isa String)
                return nothing
            end
        end
        # Look in project for extensions to insert
        proj_pkg = project_file_name_uuid(implicit_project_file, pkg.name)
        if pkg == proj_pkg
            d_proj = parsed_toml(implicit_project_file)
            weakdeps = get(d_proj, "weakdeps", nothing)::Union{Nothing, Vector{String}, Dict{String,Any}}
            extensions = get(d_proj, "extensions", nothing)::Union{Nothing, Dict{String, Any}}
            extensions === nothing && return
            weakdeps === nothing && return
            if weakdeps isa Dict{String, Any}
                return _insert_extension_triggers(pkg, extensions, weakdeps)
            end
        end

        # Now look in manifest
        project_file isa String || return nothing
        manifest_file = project_file_manifest_path(project_file)
        manifest_file === nothing && return
        d = get_deps(parsed_toml(manifest_file))
        for (dep_name, entries) in d
            entries::Vector{Any}
            for entry in entries
                entry = entry::Dict{String, Any}
                uuid = get(entry, "uuid", nothing)::Union{String, Nothing}
                uuid === nothing && continue
                if UUID(uuid) == pkg.uuid
                    weakdeps = get(entry, "weakdeps", nothing)::Union{Nothing, Vector{String}, Dict{String,Any}}
                    extensions = get(entry, "extensions", nothing)::Union{Nothing, Dict{String, Any}}
                    extensions === nothing && return
                    weakdeps === nothing && return
                    if weakdeps isa Dict{String, Any}
                        return _insert_extension_triggers(pkg, extensions, weakdeps)
                    end

                    d_weakdeps = Dict{String, Any}()
                    for (dep_name, entries) in d
                        dep_name in weakdeps || continue
                        entries::Vector{Any}
                        if length(entries) != 1
                            error("expected a single entry for $(repr(dep_name)) in $(repr(project_file))")
                        end
                        entry = first(entries)::Dict{String, Any}
                        uuid = entry["uuid"]::String
                        d_weakdeps[dep_name] = uuid
                    end
                    @assert length(d_weakdeps) == length(weakdeps)
                    return _insert_extension_triggers(pkg, extensions, d_weakdeps)
                end
            end
        end
    end
    return nothing
end

function _insert_extension_triggers(parent::PkgId, extensions::Dict{String, Any}, weakdeps::Dict{String, Any})
    for (ext, triggers) in extensions
        triggers = triggers::Union{String, Vector{String}}
        triggers isa String && (triggers = [triggers])
        id = PkgId(uuid5(parent.uuid::UUID, ext), ext)
        if id in keys(EXT_PRIMED) || haskey(Base.loaded_modules, id)
            continue  # extension is already primed or loaded, don't add it again
        end
        EXT_PRIMED[id] = parent
        gid = ExtensionId(id, parent, 1 + length(triggers))
        trigger1 = get!(Vector{ExtensionId}, EXT_DORMITORY, parent)
        push!(trigger1, gid)
        for trigger in triggers
            # TODO: Better error message if this lookup fails?
            uuid_trigger = UUID(weakdeps[trigger]::String)
            trigger_id = PkgId(uuid_trigger, trigger)
            if !haskey(explicit_loaded_modules, trigger_id) || haskey(package_locks, trigger_id)
                trigger1 = get!(Vector{ExtensionId}, EXT_DORMITORY, trigger_id)
                push!(trigger1, gid)
            else
                gid.ntriggers -= 1
            end
        end
    end
end

loading_extension::Bool = false
precompiling_extension::Bool = false
function run_extension_callbacks(extid::ExtensionId)
    assert_havelock(require_lock)
    succeeded = try
        # Used by Distributed to now load extensions in the package callback
        global loading_extension = true
        _require_prelocked(extid.id)
        @debug "Extension $(extid.id.name) of $(extid.parentid.name) loaded"
        true
    catch
        # Try to continue loading if loading an extension errors
        errs = current_exceptions()
        @error "Error during loading of extension $(extid.id.name) of $(extid.parentid.name), \
                use `Base.retry_load_extensions()` to retry." exception=errs
        false
    finally
        global loading_extension = false
    end
    return succeeded
end

function run_extension_callbacks(pkgid::PkgId)
    assert_havelock(require_lock)
    # take ownership of extids that depend on this pkgid
    extids = pop!(EXT_DORMITORY, pkgid, nothing)
    extids === nothing && return
    for extid in extids
        if extid.ntriggers > 0
            # indicate pkgid is loaded
            extid.ntriggers -= 1
        end
        if extid.ntriggers < 0
            # indicate pkgid is loaded
            extid.ntriggers += 1
            succeeded = false
        else
            succeeded = true
        end
        if extid.ntriggers == 0
            # actually load extid, now that all dependencies are met,
            # and record the result
            succeeded = succeeded && run_extension_callbacks(extid)
            succeeded || push!(EXT_DORMITORY_FAILED, extid)
        end
    end
    return
end

"""
    retry_load_extensions()

Loads all the (not yet loaded) extensions that have their extension-dependencies loaded.
This is used in cases where the automatic loading of an extension failed
due to some problem with the extension. Instead of restarting the Julia session,
the extension can be fixed, and this function run.
"""
function retry_load_extensions()
    @lock require_lock begin
    # this copy is desired since run_extension_callbacks will release this lock
    # so this can still mutate the list to drop successful ones
    failed = copy(EXT_DORMITORY_FAILED)
    empty!(EXT_DORMITORY_FAILED)
    filter!(failed) do extid
        return !run_extension_callbacks(extid)
    end
    prepend!(EXT_DORMITORY_FAILED, failed)
    end
    return
end

"""
    get_extension(parent::Module, extension::Symbol)

Return the module for `extension` of `parent` or return `nothing` if the extension is not loaded.
"""
get_extension(parent::Module, ext::Symbol) = get_extension(PkgId(parent), ext)
function get_extension(parentid::PkgId, ext::Symbol)
    parentid.uuid === nothing && return nothing
    extid = PkgId(uuid5(parentid.uuid, string(ext)), string(ext))
    return get(loaded_modules, extid, nothing)
end

# End extensions


struct CacheFlags
    # OOICCDDP - see jl_cache_flags
    use_pkgimages::Bool
    debug_level::Int
    check_bounds::Int
    inline::Bool
    opt_level::Int
end
function CacheFlags(f::UInt8)
    use_pkgimages = Bool(f & 1)
    debug_level = Int((f >> 1) & 3)
    check_bounds = Int((f >> 3) & 3)
    inline = Bool((f >> 5) & 1)
    opt_level = Int((f >> 6) & 3) # define OPT_LEVEL in statiddata_utils
    CacheFlags(use_pkgimages, debug_level, check_bounds, inline, opt_level)
end
CacheFlags(f::Int) = CacheFlags(UInt8(f))
CacheFlags() = CacheFlags(ccall(:jl_cache_flags, UInt8, ()))

function _cacheflag_to_uint8(cf::CacheFlags)::UInt8
    f = UInt8(0)
    f |= cf.use_pkgimages << 0
    f |= cf.debug_level << 1
    f |= cf.check_bounds << 3
    f |= cf.inline << 5
    f |= cf.opt_level << 6
    return f
end

function show(io::IO, cf::CacheFlags)
    print(io, "use_pkgimages = ", cf.use_pkgimages)
    print(io, ", debug_level = ", cf.debug_level)
    print(io, ", check_bounds = ", cf.check_bounds)
    print(io, ", inline = ", cf.inline)
    print(io, ", opt_level = ", cf.opt_level)
end

struct ImageTarget
    name::String
    flags::Int32
    ext_features::String
    features_en::Vector{UInt8}
    features_dis::Vector{UInt8}
end

function parse_image_target(io::IO)
    flags = read(io, Int32)
    nfeature = read(io, Int32)
    feature_en = read(io, 4*nfeature)
    feature_dis = read(io, 4*nfeature)
    name_len = read(io, Int32)
    name = String(read(io, name_len))
    ext_features_len = read(io, Int32)
    ext_features = String(read(io, ext_features_len))
    ImageTarget(name, flags, ext_features, feature_en, feature_dis)
end

function parse_image_targets(targets::Vector{UInt8})
    io = IOBuffer(targets)
    ntargets = read(io, Int32)
    targets = Vector{ImageTarget}(undef, ntargets)
    for i in 1:ntargets
        targets[i] = parse_image_target(io)
    end
    return targets
end

function current_image_targets()
    targets = @ccall jl_reflect_clone_targets()::Vector{UInt8}
    return parse_image_targets(targets)
end

struct FeatureName
    name::Cstring
    bit::UInt32 # bit index into a `uint32_t` array;
    llvmver::UInt32 # 0 if it is available on the oldest LLVM version we support
end

function feature_names()
    fnames = Ref{Ptr{FeatureName}}()
    nf = Ref{Csize_t}()
    @ccall jl_reflect_feature_names(fnames::Ptr{Ptr{FeatureName}}, nf::Ptr{Csize_t})::Cvoid
    if fnames[] == C_NULL
        @assert nf[] == 0
        return Vector{FeatureName}(undef, 0)
    end
    Base.unsafe_wrap(Array, fnames[], nf[], own=false)
end

function test_feature(features::Vector{UInt8}, feat::FeatureName)
    bitidx = feat.bit
    u8idx = div(bitidx, 8) + 1
    bit = bitidx % 8
    return (features[u8idx] & (1 << bit)) != 0
end

function show(io::IO, it::ImageTarget)
    print(io, it.name)
    if !isempty(it.ext_features)
        print(io, ",", it.ext_features)
    end
    print(io, "; flags=", it.flags)
    print(io, "; features_en=(")
    first = true
    for feat in feature_names()
        if test_feature(it.features_en, feat)
            name = Base.unsafe_string(feat.name)
            if first
                first = false
                print(io, name)
            else
                print(io, ", ", name)
            end
        end
    end
    print(io, ")")
    # Is feature_dis useful?
end

# should sync with the types of arguments of `stale_cachefile`
const StaleCacheKey = Tuple{Base.PkgId, UInt128, String, String}

"""
    Base.isprecompiled(pkg::PkgId; ignore_loaded::Bool=false)

Returns whether a given PkgId within the active project is precompiled.

By default this check observes the same approach that code loading takes
with respect to when different versions of dependencies are currently loaded
to that which is expected. To ignore loaded modules and answer as if in a
fresh julia session specify `ignore_loaded=true`.

!!! compat "Julia 1.10"
    This function requires at least Julia 1.10.
"""
function isprecompiled(pkg::PkgId;
        ignore_loaded::Bool=false,
        stale_cache::Dict{StaleCacheKey,Bool}=Dict{StaleCacheKey, Bool}(),
        cachepaths::Vector{String}=Base.find_all_in_cache_path(pkg),
        sourcepath::Union{String,Nothing}=Base.locate_package(pkg),
        flags::CacheFlags=CacheFlags())
    isnothing(sourcepath) && error("Cannot locate source for $(repr(pkg))")
    for path_to_try in cachepaths
        staledeps = stale_cachefile(sourcepath, path_to_try, ignore_loaded = true, requested_flags=flags)
        if staledeps === true
            continue
        end
        staledeps, _ = staledeps::Tuple{Vector{Any}, Union{Nothing, String}}
        # finish checking staledeps module graph
        for i in 1:length(staledeps)
            dep = staledeps[i]
            dep isa Module && continue
            modpath, modkey, modbuild_id = dep::Tuple{String, PkgId, UInt128}
            modpaths = find_all_in_cache_path(modkey)
            for modpath_to_try in modpaths::Vector{String}
                stale_cache_key = (modkey, modbuild_id, modpath, modpath_to_try)::StaleCacheKey
                if get!(() -> stale_cachefile(stale_cache_key...; ignore_loaded, requested_flags=flags) === true,
                        stale_cache, stale_cache_key)
                    continue
                end
                @goto check_next_dep
            end
            @goto check_next_path
            @label check_next_dep
        end
        try
            # update timestamp of precompilation file so that it is the first to be tried by code loading
            touch(path_to_try)
        catch ex
            # file might be read-only and then we fail to update timestamp, which is fine
            ex isa IOError || rethrow()
        end
        return true
        @label check_next_path
    end
    return false
end

# loads a precompile cache file, after checking stale_cachefile tests
function _tryrequire_from_serialized(modkey::PkgId, build_id::UInt128)
    assert_havelock(require_lock)
    loaded = nothing
    if root_module_exists(modkey)
        loaded = root_module(modkey)
    else
        loaded = start_loading(modkey)
        if loaded === nothing
            try
                modpath = locate_package(modkey)
                modpath === nothing && return nothing
                set_pkgorigin_version_path(modkey, String(modpath))
                loaded = _require_search_from_serialized(modkey, String(modpath), build_id)
            finally
                end_loading(modkey, loaded)
            end
            if loaded isa Module
                insert_extension_triggers(modkey)
                run_package_callbacks(modkey)
            end
        end
    end
    if !(loaded isa Module) || PkgId(loaded) != modkey
        return ErrorException("Required dependency $modkey failed to load from a cache file.")
    end
    return loaded
end

# loads a precompile cache file, ignoring stale_cachefile tests
# assuming all depmods are already loaded and everything is valid
function _tryrequire_from_serialized(modkey::PkgId, path::String, ocachepath::Union{Nothing, String}, sourcepath::String, depmods::Vector{Any})
    assert_havelock(require_lock)
    loaded = nothing
    if root_module_exists(modkey)
        loaded = root_module(modkey)
    else
        loaded = start_loading(modkey)
        if loaded === nothing
            try
                for i in 1:length(depmods)
                    dep = depmods[i]
                    dep isa Module && continue
                    _, depkey, depbuild_id = dep::Tuple{String, PkgId, UInt128}
                    @assert root_module_exists(depkey)
                    dep = root_module(depkey)
                    depmods[i] = dep
                end
                set_pkgorigin_version_path(modkey, sourcepath)
                loaded = _include_from_serialized(modkey, path, ocachepath, depmods)
            finally
                end_loading(modkey, loaded)
            end
            if loaded isa Module
                insert_extension_triggers(modkey)
                run_package_callbacks(modkey)
            end
        end
    end
    if !(loaded isa Module) || PkgId(loaded) != modkey
        return ErrorException("Required dependency $modkey failed to load from a cache file.")
    end
    return loaded
end

# returns whether the package is tracked in coverage or malloc tracking based on
# JLOptions and includes
function pkg_tracked(includes)
    if JLOptions().code_coverage == 0 && JLOptions().malloc_log == 0
        return false
    elseif JLOptions().code_coverage == 1 || JLOptions().malloc_log == 1 # user
        # Just say true. Pkgimages aren't in Base
        return true
    elseif JLOptions().code_coverage == 2 || JLOptions().malloc_log == 2 # all
        return true
    elseif JLOptions().code_coverage == 3 || JLOptions().malloc_log == 3 # tracked path
        if JLOptions().tracked_path == C_NULL
            return false
        else
            tracked_path = unsafe_string(JLOptions().tracked_path)
            if isempty(tracked_path)
                return false
            else
                return any(includes) do inc
                    startswith(inc.filename, tracked_path)
                end
            end
        end
    end
end

# loads a precompile cache file, ignoring stale_cachefile tests
# load the best available (non-stale) version of all dependent modules first
function _tryrequire_from_serialized(pkg::PkgId, path::String, ocachepath::Union{Nothing, String})
    assert_havelock(require_lock)
    local depmodnames
    io = open(path, "r")
    ignore_native = false
    try
        iszero(isvalid_cache_header(io)) && return ArgumentError("Invalid header in cache file $path.")
        _, (includes, _, _), depmodnames, _, _, _, clone_targets, _ = parse_cache_header(io, path)

        ignore_native = pkg_tracked(includes)

        pkgimage = !isempty(clone_targets)
        if pkgimage
            ocachepath !== nothing || return ArgumentError("Expected ocachepath to be provided")
            isfile(ocachepath) || return ArgumentError("Ocachepath $ocachepath is not a file.")
            ocachepath == ocachefile_from_cachefile(path) || return ArgumentError("$ocachepath is not the expected ocachefile")
            # TODO: Check for valid clone_targets?
            isvalid_pkgimage_crc(io, ocachepath) || return ArgumentError("Invalid checksum in cache file $ocachepath.")
        else
            @assert ocachepath === nothing
        end
        isvalid_file_crc(io) || return ArgumentError("Invalid checksum in cache file $path.")
    finally
        close(io)
    end
    ndeps = length(depmodnames)
    depmods = Vector{Any}(undef, ndeps)
    for i in 1:ndeps
        modkey, build_id = depmodnames[i]
        dep = _tryrequire_from_serialized(modkey, build_id)
        if !isa(dep, Module)
            return dep
        end
        depmods[i] = dep
    end
    # then load the file
    return _include_from_serialized(pkg, path, ocachepath, depmods, ignore_native)
end

# returns `nothing` if require found a precompile cache for this sourcepath, but couldn't load it
# returns the set of modules restored if the cache load succeeded
@constprop :none function _require_search_from_serialized(pkg::PkgId, sourcepath::String, build_id::UInt128; reasons=nothing)
    assert_havelock(require_lock)
    paths = find_all_in_cache_path(pkg)
    for path_to_try in paths::Vector{String}
        staledeps = stale_cachefile(pkg, build_id, sourcepath, path_to_try; reasons)
        if staledeps === true
            continue
        end
        staledeps, ocachefile = staledeps::Tuple{Vector{Any}, Union{Nothing, String}}
        # finish checking staledeps module graph
        for i in 1:length(staledeps)
            dep = staledeps[i]
            dep isa Module && continue
            modpath, modkey, modbuild_id = dep::Tuple{String, PkgId, UInt128}
            modpaths = find_all_in_cache_path(modkey)
            for modpath_to_try in modpaths
                modstaledeps = stale_cachefile(modkey, modbuild_id, modpath, modpath_to_try)
                if modstaledeps === true
                    continue
                end
                modstaledeps, modocachepath = modstaledeps::Tuple{Vector{Any}, Union{Nothing, String}}
                staledeps[i] = (modpath, modkey, modpath_to_try, modstaledeps, modocachepath)
                @goto check_next_dep
            end
            @debug "Rejecting cache file $path_to_try because required dependency $modkey with build ID $(UUID(modbuild_id)) is missing from the cache."
            @goto check_next_path
            @label check_next_dep
        end
        try
            touch(path_to_try) # update timestamp of precompilation file
        catch ex # file might be read-only and then we fail to update timestamp, which is fine
            ex isa IOError || rethrow()
        end
        # finish loading module graph into staledeps
        for i in 1:length(staledeps)
            dep = staledeps[i]
            dep isa Module && continue
            modpath, modkey, modcachepath, modstaledeps, modocachepath = dep::Tuple{String, PkgId, String, Vector{Any}, Union{Nothing, String}}
            dep = _tryrequire_from_serialized(modkey, modcachepath, modocachepath, modpath, modstaledeps)
            if !isa(dep, Module)
                @debug "Rejecting cache file $path_to_try because required dependency $modkey failed to load from cache file for $modcachepath." exception=dep
                @goto check_next_path
            end
            staledeps[i] = dep
        end
        restored = _include_from_serialized(pkg, path_to_try, ocachefile, staledeps)
        isa(restored, Module) && return restored
        @debug "Deserialization checks failed while attempting to load cache from $path_to_try" exception=restored
        continue
        @label check_next_path
    end
    return nothing
end

# to synchronize multiple tasks trying to import/using something
const package_locks = Dict{PkgId,Pair{Task,Threads.Condition}}()

debug_loading_deadlocks::Bool = true # Enable a slightly more expensive, but more complete algorithm that can handle simultaneous tasks.
                               # This only triggers if you have multiple tasks trying to load the same package at the same time,
                               # so it is unlikely to make a difference normally.
function start_loading(modkey::PkgId)
    # handle recursive calls to require
    assert_havelock(require_lock)
    loading = get(package_locks, modkey, nothing)
    if loading !== nothing
        # load already in progress for this module on the task
        task, cond = loading
        deps = String[modkey.name]
        pkgid = modkey
        assert_havelock(cond.lock)
        if debug_loading_deadlocks && current_task() !== task
            waiters = Dict{Task,Pair{Task,PkgId}}() # invert to track waiting tasks => loading tasks
            for each in package_locks
                cond2 = each[2][2]
                assert_havelock(cond2.lock)
                for waiting in cond2.waitq
                    push!(waiters, waiting => (each[2][1] => each[1]))
                end
            end
            while true
                running = get(waiters, task, nothing)
                running === nothing && break
                task, pkgid = running
                push!(deps, pkgid.name)
                task === current_task() && break
            end
        end
        if current_task() === task
            others = String[modkey.name] # repeat this to emphasize the cycle here
            for each in package_locks # list the rest of the packages being loaded too
                if each[2][1] === task
                    other = each[1].name
                    other == modkey.name || other == pkgid.name || push!(others, other)
                end
            end
            msg = sprint(deps, others) do io, deps, others
                print(io, "deadlock detected in loading ")
                join(io, deps, " -> ")
                print(io, " -> ")
                join(io, others, " && ")
            end
            throw(ConcurrencyViolationError(msg))
        end
        return wait(cond)
    end
    package_locks[modkey] = current_task() => Threads.Condition(require_lock)
    return
end

function end_loading(modkey::PkgId, @nospecialize loaded)
    loading = pop!(package_locks, modkey)
    notify(loading[2], loaded, all=true)
    nothing
end

# to notify downstream consumers that a module was successfully loaded
# Callbacks take the form (mod::Base.PkgId) -> nothing.
# WARNING: This is an experimental feature and might change later, without deprecation.
const package_callbacks = Any[]
# to notify downstream consumers that a file has been included into a particular module
# Callbacks take the form (mod::Module, filename::String) -> nothing
# WARNING: This is an experimental feature and might change later, without deprecation.
const include_callbacks = Any[]

# used to optionally track dependencies when requiring a module:
const _concrete_dependencies = Pair{PkgId,UInt128}[] # these dependency versions are "set in stone", and the process should try to avoid invalidating them
const _require_dependencies = Any[] # a list of (mod, abspath, fsize, hash, mtime) tuples that are the file dependencies of the module currently being precompiled
const _track_dependencies = Ref(false) # set this to true to track the list of file dependencies
function _include_dependency(mod::Module, _path::AbstractString; track_content=true)
    prev = source_path(nothing)
    if prev === nothing
        path = abspath(_path)
    else
        path = normpath(joinpath(dirname(prev), _path))
    end
    if _track_dependencies[]
        @lock require_lock begin
            if track_content
                hash = isdir(path) ? _crc32c(join(readdir(path))) : open(_crc32c, path, "r")
                # use mtime=-1.0 here so that fsize==0 && mtime==0.0 corresponds to a missing include_dependency
                push!(_require_dependencies, (mod, path, filesize(path), hash, -1.0))
            else
                push!(_require_dependencies, (mod, path, UInt64(0), UInt32(0), mtime(path)))
            end
        end
    end
    return path, prev
end

"""
    include_dependency(path::AbstractString; track_content::Bool=false)

In a module, declare that the file, directory, or symbolic link specified by `path`
(relative or absolute) is a dependency for precompilation; that is, the module will need
to be recompiled if the modification time `mtime` of `path` changes.
If `track_content=true` recompilation is triggered when the content of `path` changes
(if `path` is a directory the content equals `join(readdir(path))`).

This is only needed if your module depends on a path that is not used via [`include`](@ref). It has
no effect outside of compilation.

!!! compat "Julia 1.11"
    Keyword argument `track_content` requires at least Julia 1.11.
"""
function include_dependency(path::AbstractString; track_content::Bool=false)
    _include_dependency(Main, path, track_content=track_content)
    return nothing
end

# we throw PrecompilableError when a module doesn't want to be precompiled
import Core: PrecompilableError
function show(io::IO, ex::PrecompilableError)
    print(io, "Error when precompiling module, potentially caused by a __precompile__(false) declaration in the module.")
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
    if !isprecompilable && generating_output()
        throw(PrecompilableError())
    end
    nothing
end

# require always works in Main scope and loads files from node 1
const toplevel_load = Ref(true)

const _require_world_age = Ref{UInt}(typemax(UInt))

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
    if _require_world_age[] != typemax(UInt)
        Base.invoke_in_world(_require_world_age[], __require, into, mod)
    else
        @invokelatest __require(into, mod)
    end
end

function __require(into::Module, mod::Symbol)
    @lock require_lock begin
    LOADING_CACHE[] = LoadingCache()
    try
        uuidkey_env = identify_package_env(into, String(mod))
        # Core.println("require($(PkgId(into)), $mod) -> $uuidkey_env")
        if uuidkey_env === nothing
            where = PkgId(into)
            if where.uuid === nothing
                hint, dots = begin
                    if isdefined(into, mod) && getfield(into, mod) isa Module
                        true, "."
                    elseif isdefined(parentmodule(into), mod) && getfield(parentmodule(into), mod) isa Module
                        true, ".."
                    else
                        false, ""
                    end
                end
                hint_message = hint ? ", maybe you meant `import/using $(dots)$(mod)`" : ""
                install_message = if mod != :Pkg
                    start_sentence = hint ? "Otherwise, run" : "Run"
                    "\n- $start_sentence `import Pkg; Pkg.add($(repr(String(mod))))` to install the $mod package."
                else  # for some reason Pkg itself isn't availability so do not tell them to use Pkg to install it.
                    ""
                end

                throw(ArgumentError("Package $mod not found in current path$hint_message.$install_message"))
            else
                manifest_warnings = collect_manifest_warnings()
                throw(ArgumentError("""
                Package $(where.name) does not have $mod in its dependencies:
                $manifest_warnings- You may have a partially installed environment. Try `Pkg.instantiate()`
                  to ensure all packages in the environment are installed.
                - Or, if you have $(where.name) checked out for development and have
                  added $mod as a dependency but haven't updated your primary
                  environment's manifest file, try `Pkg.resolve()`.
                - Otherwise you may need to report an issue with $(where.name)"""))
            end
        end
        uuidkey, env = uuidkey_env
        if _track_dependencies[]
            path = binpack(uuidkey)
            push!(_require_dependencies, (into, path, UInt64(0), UInt32(0), 0.0))
        end
        return _require_prelocked(uuidkey, env)
    finally
        LOADING_CACHE[] = nothing
    end
    end
end

function find_unsuitable_manifests_versions()
    unsuitable_manifests = String[]
    dev_manifests = String[]
    for env in load_path()
        project_file = env_project_file(env)
        project_file isa String || continue # no project file
        manifest_file = project_file_manifest_path(project_file)
        manifest_file isa String || continue # no manifest file
        m = parsed_toml(manifest_file)
        man_julia_version = get(m, "julia_version", nothing)
        man_julia_version isa String || @goto mark
        man_julia_version = VersionNumber(man_julia_version)
        thispatch(man_julia_version) != thispatch(VERSION) && @goto mark
        isempty(man_julia_version.prerelease) != isempty(VERSION.prerelease) && @goto mark
        isempty(man_julia_version.prerelease) && continue
        man_julia_version.prerelease[1] != VERSION.prerelease[1] && @goto mark
        if VERSION.prerelease[1] == "DEV"
            # manifests don't store the 2nd part of prerelease, so cannot check further
            # so treat them specially in the warning
            push!(dev_manifests, manifest_file)
        end
        continue
        @label mark
        push!(unsuitable_manifests, string(manifest_file, " (v", man_julia_version, ")"))
    end
    return unsuitable_manifests, dev_manifests
end

function collect_manifest_warnings()
    unsuitable_manifests, dev_manifests = find_unsuitable_manifests_versions()
    msg = ""
    if !isempty(unsuitable_manifests)
        msg *= """
        - Note that the following manifests in the load path were resolved with a different
          julia version, which may be the cause of the error:
            $(join(unsuitable_manifests, "\n    "))
        """
    end
    if !isempty(dev_manifests)
        msg *= """
        - Note that the following manifests in the load path were resolved a potentially
          different DEV version of the current version, which may be the cause
          of the error:
            $(join(dev_manifests, "\n    "))
        """
    end
    return msg
end

require(uuidkey::PkgId) = @lock require_lock _require_prelocked(uuidkey)

const REPL_PKGID = PkgId(UUID("3fa0cd96-eef1-5676-8a61-b3b8758bbffb"), "REPL")

function _require_prelocked(uuidkey::PkgId, env=nothing)
    if _require_world_age[] != typemax(UInt)
        Base.invoke_in_world(_require_world_age[], __require_prelocked, uuidkey, env)
    else
        @invokelatest __require_prelocked(uuidkey, env)
    end
end

function __require_prelocked(uuidkey::PkgId, env=nothing)
    assert_havelock(require_lock)
    if !root_module_exists(uuidkey)
        newm = _require(uuidkey, env)
        if newm === nothing
            error("package `$(uuidkey.name)` did not define the expected \
                  module `$(uuidkey.name)`, check for typos in package module name")
        end
        insert_extension_triggers(uuidkey)
        # After successfully loading, notify downstream consumers
        run_package_callbacks(uuidkey)
    else
        m = get(loaded_modules, uuidkey, nothing)
        if m !== nothing
            explicit_loaded_modules[uuidkey] = m
            run_package_callbacks(uuidkey)
        end
        newm = root_module(uuidkey)
    end
    return newm
end

mutable struct PkgOrigin
    path::Union{String,Nothing}
    cachepath::Union{String,Nothing}
    version::Union{VersionNumber,Nothing}
end
PkgOrigin() = PkgOrigin(nothing, nothing, nothing)
const pkgorigins = Dict{PkgId,PkgOrigin}()

const loaded_modules = Dict{PkgId,Module}()
# Emptied on Julia start
const explicit_loaded_modules = Dict{PkgId,Module}()
const loaded_modules_order = Vector{Module}()
const module_keys = IdDict{Module,PkgId}() # the reverse

is_root_module(m::Module) = @lock require_lock haskey(module_keys, m)
root_module_key(m::Module) = @lock require_lock module_keys[m]

@constprop :none function register_root_module(m::Module)
    # n.b. This is called from C after creating a new module in `Base.__toplevel__`,
    # instead of adding them to the binding table there.
    @lock require_lock begin
    key = PkgId(m, String(nameof(m)))
    if haskey(loaded_modules, key)
        oldm = loaded_modules[key]
        if oldm !== m
            if generating_output(#=incremental=#true)
                error("Replacing module `$(key.name)`")
            else
                @warn "Replacing module `$(key.name)`"
            end
        end
    end
    push!(loaded_modules_order, m)
    loaded_modules[key] = m
    explicit_loaded_modules[key] = m
    module_keys[m] = key
    end
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
root_module(key::PkgId) = @lock require_lock loaded_modules[key]
function root_module(where::Module, name::Symbol)
    key = identify_package(where, String(name))
    key isa PkgId || throw(KeyError(name))
    return root_module(key)
end
maybe_root_module(key::PkgId) = @lock require_lock get(loaded_modules, key, nothing)

root_module_exists(key::PkgId) = @lock require_lock haskey(loaded_modules, key)
loaded_modules_array() = @lock require_lock copy(loaded_modules_order)

function unreference_module(key::PkgId)
    if haskey(loaded_modules, key)
        m = pop!(loaded_modules, key)
        # need to ensure all modules are GC rooted; will still be referenced
        # in module_keys
    end
end

# whoever takes the package_locks[pkg] must call this function immediately
function set_pkgorigin_version_path(pkg::PkgId, path::Union{String,Nothing})
    assert_havelock(require_lock)
    pkgorigin = get!(PkgOrigin, pkgorigins, pkg)
    if path !== nothing
        # Pkg needs access to the version of packages in the sysimage.
        if generating_output(#=incremental=#false)
            pkgorigin.version = get_pkgversion_from_path(joinpath(dirname(path), ".."))
        end
    end
    pkgorigin.path = path
    nothing
end

# A hook to allow code load to use Pkg.precompile
const PKG_PRECOMPILE_HOOK = Ref{Function}()

# Returns `nothing` or the new(ish) module
function _require(pkg::PkgId, env=nothing)
    assert_havelock(require_lock)
    loaded = start_loading(pkg)
    loaded === nothing || return loaded

    last = toplevel_load[]
    try
        toplevel_load[] = false
        # perform the search operation to select the module file require intends to load
        path = locate_package(pkg, env)
        if path === nothing
            throw(ArgumentError("""
                Package $(repr("text/plain", pkg)) is required but does not seem to be installed:
                 - Run `Pkg.instantiate()` to install all recorded dependencies.
                """))
        end
        set_pkgorigin_version_path(pkg, path)

        pkg_precompile_attempted = false # being safe to avoid getting stuck in a Pkg.precompile loop
        reasons = Dict{String,Int}()
        # attempt to load the module file via the precompile cache locations
        if JLOptions().use_compiled_modules != 0
            @label load_from_cache
            m = _require_search_from_serialized(pkg, path, UInt128(0); reasons)
            if m isa Module
                return m
            end
        end

        if JLOptions().use_compiled_modules == 3
            error("Precompiled image $pkg not available with flags $(CacheFlags())")
        end

        # if the module being required was supposed to have a particular version
        # but it was not handled by the precompile loader, complain
        for (concrete_pkg, concrete_build_id) in _concrete_dependencies
            if pkg == concrete_pkg
                @warn """Module $(pkg.name) with build ID $((UUID(concrete_build_id))) is missing from the cache.
                     This may mean $(repr("text/plain", pkg)) does not support precompilation but is imported by a module that does."""
                if JLOptions().incremental != 0
                    # during incremental precompilation, this should be fail-fast
                    throw(PrecompilableError())
                end
            end
        end

        if JLOptions().use_compiled_modules == 1
            if !generating_output(#=incremental=#false)
                if !pkg_precompile_attempted && isinteractive() && isassigned(PKG_PRECOMPILE_HOOK)
                    pkg_precompile_attempted = true
                    unlock(require_lock)
                    try
                        @invokelatest PKG_PRECOMPILE_HOOK[](pkg.name, _from_loading = true)
                    finally
                        lock(require_lock)
                    end
                    @goto load_from_cache
                end
                # spawn off a new incremental pre-compile task for recursive `require` calls
                cachefile_or_module = maybe_cachefile_lock(pkg, path) do
                    # double-check now that we have lock
                    m = _require_search_from_serialized(pkg, path, UInt128(0))
                    m isa Module && return m
                    compilecache(pkg, path; reasons)
                end
                cachefile_or_module isa Module && return cachefile_or_module::Module
                cachefile = cachefile_or_module
                if isnothing(cachefile) # maybe_cachefile_lock returns nothing if it had to wait for another process
                    @goto load_from_cache # the new cachefile will have the newest mtime so will come first in the search
                elseif isa(cachefile, Exception)
                    if precompilableerror(cachefile)
                        verbosity = isinteractive() ? CoreLogging.Info : CoreLogging.Debug
                        @logmsg verbosity "Skipping precompilation due to precompilable error. Importing $(repr("text/plain", pkg))." exception=m
                    else
                        @warn "The call to compilecache failed to create a usable precompiled cache file for $(repr("text/plain", pkg))" exception=m
                    end
                    # fall-through to loading the file locally if not incremental
                else
                    cachefile, ocachefile = cachefile::Tuple{String, Union{Nothing, String}}
                    m = _tryrequire_from_serialized(pkg, cachefile, ocachefile)
                    if !isa(m, Module)
                        @warn "The call to compilecache failed to create a usable precompiled cache file for $(repr("text/plain", pkg))" exception=m
                    else
                        return m
                    end
                end
                if JLOptions().incremental != 0
                    # during incremental precompilation, this should be fail-fast
                    throw(PrecompilableError())
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
        unlock(require_lock)
        try
            include(__toplevel__, path)
            loaded = get(loaded_modules, pkg, nothing)
        finally
            lock(require_lock)
            if uuid !== old_uuid
                ccall(:jl_set_module_uuid, Cvoid, (Any, NTuple{2, UInt64}), __toplevel__, old_uuid)
            end
        end
    finally
        toplevel_load[] = last
        end_loading(pkg, loaded)
    end
    return loaded
end

# Only used from test/precompile.jl
function _require_from_serialized(uuidkey::PkgId, path::String, ocachepath::Union{String, Nothing})
    @lock require_lock begin
    set_pkgorigin_version_path(uuidkey, nothing)
    newm = _tryrequire_from_serialized(uuidkey, path, ocachepath)
    newm isa Module || throw(newm)
    insert_extension_triggers(uuidkey)
    # After successfully loading, notify downstream consumers
    run_package_callbacks(uuidkey)
    return newm
    end
end



# relative-path load

"""
    include_string([mapexpr::Function,] m::Module, code::AbstractString, filename::AbstractString="string")

Like [`include`](@ref), except reads code from the given string rather than from a file.

The optional first argument `mapexpr` can be used to transform the included code before
it is evaluated: for each parsed expression `expr` in `code`, the `include_string` function
actually evaluates `mapexpr(expr)`.  If it is omitted, `mapexpr` defaults to [`identity`](@ref).

!!! compat "Julia 1.5"
    Julia 1.5 is required for passing the `mapexpr` argument.
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
    if s !== nothing
        s = s::IdDict{Any,Any}
        if haskey(s, :SOURCE_PATH)
            return s[:SOURCE_PATH]::Union{Nothing,String}
        end
    end
    return default
end

function source_dir()
    p = source_path(nothing)
    return p === nothing ? pwd() : dirname(p)
end

"""
    Base.include([mapexpr::Function,] m::Module, path::AbstractString)

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

!!! compat "Julia 1.5"
    Julia 1.5 is required for passing the `mapexpr` argument.
"""
Base.include # defined in Base.jl

# Full include() implementation which is used after bootstrap
function _include(mapexpr::Function, mod::Module, _path::AbstractString)
    @noinline # Workaround for module availability in _simplify_include_frames
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

Load the file into an anonymous module using [`include`](@ref), evaluate all expressions,
and return the value of the last expression.
The optional `args` argument can be used to set the input arguments of the script (i.e. the global `ARGS` variable).
Note that definitions (e.g. methods, globals) are evaluated in the anonymous module and do not affect the current module.

# Examples

```jldoctest
julia> write("testfile.jl", \"\"\"
           @show ARGS
           1 + 1
       \"\"\");

julia> x = evalfile("testfile.jl", ["ARG1", "ARG2"]);
ARGS = ["ARG1", "ARG2"]

julia> x
2

julia> rm("testfile.jl")
```
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
        Base.set_active_project(nothing)
        """
    end
    return code
end

"""
    check_src_module_wrap(srcpath::String)

Checks that a package entry file `srcpath` has a module declaration, and that it is before any using/import statements.
"""
function check_src_module_wrap(pkg::PkgId, srcpath::String)
    module_rgx = r"^(|end |\"\"\" )\s*(?:@)*(?:bare)?module\s"
    load_rgx = r"\b(?:using|import)\s"
    load_seen = false
    inside_string = false
    for s in eachline(srcpath)
        if count("\"\"\"", s) == 1
            # ignore module docstrings
            inside_string = !inside_string
        end
        inside_string && continue
        if contains(s, module_rgx)
            if load_seen
                throw(ErrorException("Package $(repr("text/plain", pkg)) source file $srcpath has a using/import before a module declaration."))
            end
            return true
        end
        if startswith(s, load_rgx)
            load_seen = true
        end
    end
    throw(ErrorException("Package $(repr("text/plain", pkg)) source file $srcpath does not contain a module declaration."))
end

# this is called in the external process that generates precompiled package files
function include_package_for_output(pkg::PkgId, input::String, depot_path::Vector{String}, dl_load_path::Vector{String}, load_path::Vector{String},
                                    concrete_deps::typeof(_concrete_dependencies), source::Union{Nothing,String})

    check_src_module_wrap(pkg, input)

    append!(empty!(Base.DEPOT_PATH), depot_path)
    append!(empty!(Base.DL_LOAD_PATH), dl_load_path)
    append!(empty!(Base.LOAD_PATH), load_path)
    ENV["JULIA_LOAD_PATH"] = join(load_path, Sys.iswindows() ? ';' : ':')
    set_active_project(nothing)
    Base._track_dependencies[] = true
    get!(Base.PkgOrigin, Base.pkgorigins, pkg).path = input
    append!(empty!(Base._concrete_dependencies), concrete_deps)
    uuid_tuple = pkg.uuid === nothing ? (UInt64(0), UInt64(0)) : convert(NTuple{2, UInt64}, pkg.uuid)

    ccall(:jl_set_module_uuid, Cvoid, (Any, NTuple{2, UInt64}), Base.__toplevel__, uuid_tuple)
    if source !== nothing
        task_local_storage()[:SOURCE_PATH] = source
    end

    ccall(:jl_set_newly_inferred, Cvoid, (Any,), Core.Compiler.newly_inferred)
    Core.Compiler.track_newly_inferred.x = true
    try
        Base.include(Base.__toplevel__, input)
    catch ex
        precompilableerror(ex) || rethrow()
        @debug "Aborting `create_expr_cache'" exception=(ErrorException("Declaration of __precompile__(false) not allowed"), catch_backtrace())
        exit(125) # we define status = 125 means PrecompileableError
    finally
        Core.Compiler.track_newly_inferred.x = false
    end
end

const PRECOMPILE_TRACE_COMPILE = Ref{String}()
function create_expr_cache(pkg::PkgId, input::String, output::String, output_o::Union{Nothing, String},
                           concrete_deps::typeof(_concrete_dependencies), flags::Cmd=``, internal_stderr::IO = stderr, internal_stdout::IO = stdout)
    @nospecialize internal_stderr internal_stdout
    rm(output, force=true)   # Remove file if it exists
    output_o === nothing || rm(output_o, force=true)
    depot_path = map(abspath, DEPOT_PATH)
    dl_load_path = map(abspath, DL_LOAD_PATH)
    load_path = map(abspath, Base.load_path())
    # if pkg is a stdlib, append its parent Project.toml to the load path
    parentid = get(EXT_PRIMED, pkg, nothing)
    if parentid !== nothing
        for env in load_path
            project_file = env_project_file(env)
            if project_file === true
                _, parent_project_file = entry_point_and_project_file(env, parentid.name)
                if parent_project_file !== nothing
                    parentproj = project_file_name_uuid(parent_project_file, parentid.name)
                    if parentproj == parentid
                        push!(load_path, parent_project_file)
                    end
                end
            end
        end
    end
    path_sep = Sys.iswindows() ? ';' : ':'
    any(path -> path_sep in path, load_path) &&
        error("LOAD_PATH entries cannot contain $(repr(path_sep))")

    deps_strs = String[]
    function pkg_str(_pkg::PkgId)
        if _pkg.uuid === nothing
            "Base.PkgId($(repr(_pkg.name)))"
        else
            "Base.PkgId(Base.UUID(\"$(_pkg.uuid)\"), $(repr(_pkg.name)))"
        end
    end
    for (pkg, build_id) in concrete_deps
        push!(deps_strs, "$(pkg_str(pkg)) => $(repr(build_id))")
    end

    if output_o !== nothing
        @debug "Generating object cache file for $(repr("text/plain", pkg))"
        cpu_target = get(ENV, "JULIA_CPU_TARGET", nothing)
        opts = `--output-o $(output_o) --output-ji $(output) --output-incremental=yes`
    else
        @debug "Generating cache file for $(repr("text/plain", pkg))"
        cpu_target = nothing
        opts = `-O0 --output-ji $(output) --output-incremental=yes`
    end

    deps_eltype = sprint(show, eltype(concrete_deps); context = :module=>nothing)
    deps = deps_eltype * "[" * join(deps_strs, ",") * "]"
    trace = isassigned(PRECOMPILE_TRACE_COMPILE) ? `--trace-compile=$(PRECOMPILE_TRACE_COMPILE[])` : ``
    io = open(pipeline(addenv(`$(julia_cmd(;cpu_target)::Cmd)
                             $(flags)
                             $(opts)
                              --startup-file=no --history-file=no --warn-overwrite=yes
                              --color=$(have_color === nothing ? "auto" : have_color ? "yes" : "no")
                              $trace
                              -`,
                              "OPENBLAS_NUM_THREADS" => 1,
                              "JULIA_NUM_THREADS" => 1),
                       stderr = internal_stderr, stdout = internal_stdout),
              "w", stdout)
    # write data over stdin to avoid the (unlikely) case of exceeding max command line size
    write(io.in, """
        empty!(Base.EXT_DORMITORY) # If we have a custom sysimage with `EXT_DORMITORY` prepopulated
        Base.track_nested_precomp($(vcat(Base.precompilation_stack, pkg)))
        Base.precompiling_extension = $(loading_extension)
        Base.include_package_for_output($(pkg_str(pkg)), $(repr(abspath(input))), $(repr(depot_path)), $(repr(dl_load_path)),
            $(repr(load_path)), $deps, $(repr(source_path(nothing))))
        """)
    close(io.in)
    return io
end

const precompilation_stack = Vector{PkgId}()
# Helpful for debugging when precompilation is unexpectedly nested.
# Enable with `JULIA_DEBUG=nested_precomp`. Note that it expected to be nested in classical code-load precompilation
# TODO: Add detection if extension precompilation is nested and error / return early?
function track_nested_precomp(pkgs::Vector{PkgId})
    append!(precompilation_stack, pkgs)
    if length(precompilation_stack) > 1
        list() = join(map(p->p.name, precompilation_stack), " > ")
        @debug "Nested precompilation: $(list())" _group=:nested_precomp
    end
end

function compilecache_dir(pkg::PkgId)
    entrypath, entryfile = cache_file_entry(pkg)
    return joinpath(DEPOT_PATH[1], entrypath)
end

function compilecache_path(pkg::PkgId, prefs_hash::UInt64; project::String=something(Base.active_project(), ""))::String
    entrypath, entryfile = cache_file_entry(pkg)
    cachepath = joinpath(DEPOT_PATH[1], entrypath)
    isdir(cachepath) || mkpath(cachepath)
    if pkg.uuid === nothing
        abspath(cachepath, entryfile) * ".ji"
    else
        crc = _crc32c(project)
        crc = _crc32c(unsafe_string(JLOptions().image_file), crc)
        crc = _crc32c(unsafe_string(JLOptions().julia_bin), crc)
        crc = _crc32c(ccall(:jl_cache_flags, UInt8, ()), crc)

        cpu_target = get(ENV, "JULIA_CPU_TARGET", nothing)
        if cpu_target === nothing
            cpu_target = unsafe_string(JLOptions().cpu_target)
        end
        crc = _crc32c(cpu_target, crc)

        crc = _crc32c(prefs_hash, crc)
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
function compilecache(pkg::PkgId, internal_stderr::IO = stderr, internal_stdout::IO = stdout; flags::Cmd=``, reasons::Union{Dict{String,Int},Nothing}=Dict{String,Int}())
    @nospecialize internal_stderr internal_stdout
    path = locate_package(pkg)
    path === nothing && throw(ArgumentError("$(repr("text/plain", pkg)) not found during precompilation"))
    return compilecache(pkg, path, internal_stderr, internal_stdout; flags, reasons)
end

const MAX_NUM_PRECOMPILE_FILES = Ref(10)

function compilecache(pkg::PkgId, path::String, internal_stderr::IO = stderr, internal_stdout::IO = stdout,
                      keep_loaded_modules::Bool = true; flags::Cmd=``, reasons::Union{Dict{String,Int},Nothing}=Dict{String,Int}())

    @nospecialize internal_stderr internal_stdout
    # decide where to put the resulting cache file
    cachepath = compilecache_dir(pkg)

    # build up the list of modules that we want the precompile process to preserve
    concrete_deps = copy(_concrete_dependencies)
    if keep_loaded_modules
        for mod in loaded_modules_array()
            if !(mod === Main || mod === Core || mod === Base)
                push!(concrete_deps, PkgId(mod) => module_build_id(mod))
            end
        end
    end
    # run the expression and cache the result
    verbosity = isinteractive() ? CoreLogging.Info : CoreLogging.Debug
    @logmsg verbosity "Precompiling $(repr("text/plain", pkg)) $(list_reasons(reasons))"

    # create a temporary file in `cachepath` directory, write the cache in it,
    # write the checksum, _and then_ atomically move the file to `cachefile`.
    mkpath(cachepath)
    cache_objects = JLOptions().use_pkgimages == 1
    tmppath, tmpio = mktemp(cachepath)

    if cache_objects
        tmppath_o, tmpio_o = mktemp(cachepath)
        tmppath_so, tmpio_so = mktemp(cachepath)
    else
        tmppath_o = nothing
    end
    local p
    try
        close(tmpio)
        if cache_objects
            close(tmpio_o)
            close(tmpio_so)
        end
        p = create_expr_cache(pkg, path, tmppath, tmppath_o, concrete_deps, flags, internal_stderr, internal_stdout)

        if success(p)
            if cache_objects
                # Run linker over tmppath_o
                Linking.link_image(tmppath_o, tmppath_so)
            end

            # Read preferences hash back from .ji file (we can't precompute because
            # we don't actually know what the list of compile-time preferences are without compiling)
            prefs_hash = preferences_hash(tmppath)
            cachefile = compilecache_path(pkg, prefs_hash)
            ocachefile = cache_objects ? ocachefile_from_cachefile(cachefile) : nothing

            # append checksum for so to the end of the .ji file:
            crc_so = UInt32(0)
            if cache_objects
                crc_so = open(_crc32c, tmppath_so, "r")
            end

            # append extra crc to the end of the .ji file:
            open(tmppath, "r+") do f
                if iszero(isvalid_cache_header(f))
                    error("Invalid header for $(repr("text/plain", pkg)) in new cache file $(repr(tmppath)).")
                end
                seekend(f)
                write(f, crc_so)
                seekstart(f)
                write(f, _crc32c(f))
            end

            # inherit permission from the source file (and make them writable)
            chmod(tmppath, filemode(path) & 0o777 | 0o200)

            # prune the directory with cache files
            if pkg.uuid !== nothing
                entrypath, entryfile = cache_file_entry(pkg)
                cachefiles = filter!(x -> startswith(x, entryfile * "_") && endswith(x, ".ji"), readdir(cachepath))
                if length(cachefiles) >= MAX_NUM_PRECOMPILE_FILES[]
                    idx = findmin(mtime.(joinpath.(cachepath, cachefiles)))[2]
                    evicted_cachefile = joinpath(cachepath, cachefiles[idx])
                    @debug "Evicting file from cache" evicted_cachefile
                    rm(evicted_cachefile; force=true)
                    try
                        rm(ocachefile_from_cachefile(evicted_cachefile); force=true)
                        @static if Sys.isapple()
                            rm(ocachefile_from_cachefile(evicted_cachefile) * ".dSYM"; force=true, recursive=true)
                        end
                    catch e
                        e isa IOError || rethrow()
                    end
                end
            end

            if cache_objects
                try
                    rename(tmppath_so, ocachefile::String; force=true)
                catch e
                    e isa IOError || rethrow()
                    isfile(ocachefile::String) || rethrow()
                    # Windows prevents renaming a file that is in use so if there is a Julia session started
                    # with a package image loaded, we cannot rename that file.
                    # The code belows append a `_i` to the name of the cache file where `i` is the smallest number such that
                    # that cache file does not exist.
                    ocachename, ocacheext = splitext(ocachefile::String)
                    old_cachefiles = Set(readdir(cachepath))
                    num = 1
                    while true
                        ocachefile = ocachename * "_$num" * ocacheext
                        in(basename(ocachefile), old_cachefiles) || break
                        num += 1
                    end
                    # TODO: Risk for a race here if some other process grabs this name before us
                    cachefile = cachefile_from_ocachefile(ocachefile)
                    rename(tmppath_so, ocachefile::String; force=true)
                end
                @static if Sys.isapple()
                    run(`$(Linking.dsymutil()) $ocachefile`, Base.DevNull(), Base.DevNull(), Base.DevNull())
                end
            end
            # this is atomic according to POSIX (not Win32):
            rename(tmppath, cachefile; force=true)
            return cachefile, ocachefile
        end
    finally
        rm(tmppath, force=true)
        if cache_objects
            rm(tmppath_o::String, force=true)
            rm(tmppath_so, force=true)
        end
    end
    if p.exitcode == 125
        return PrecompilableError()
    else
        error("Failed to precompile $(repr("text/plain", pkg)) to $(repr(tmppath)).")
    end
end

function module_build_id(m::Module)
    hi, lo = ccall(:jl_module_build_id, NTuple{2,UInt64}, (Any,), m)
    return (UInt128(hi) << 64) | lo
end

function isvalid_cache_header(f::IOStream)
    pkgimage = Ref{UInt8}()
    checksum = ccall(:jl_read_verify_header, UInt64, (Ptr{Cvoid}, Ptr{UInt8}, Ptr{Int64}, Ptr{Int64}), f.ios, pkgimage, Ref{Int64}(), Ref{Int64}()) # returns checksum id or zero

    if !iszero(checksum) && pkgimage[] != 0
        @debug "Cache header was for pkgimage"
        return UInt64(0) # We somehow read the header for a pkgimage and not a ji
    end
    return checksum
end
isvalid_file_crc(f::IOStream) = (_crc32c(seekstart(f), filesize(f) - 4) == read(f, UInt32))

function isvalid_pkgimage_crc(f::IOStream, ocachefile::String)
    seekstart(f) # TODO necessary
    seek(f, filesize(f) - 8)
    expected_crc_so = read(f, UInt32)
    crc_so = open(_crc32c, ocachefile, "r")
    expected_crc_so == crc_so
end

mutable struct CacheHeaderIncludes
    const id::PkgId
    filename::String
    const fsize::UInt64
    const hash::UInt32
    const mtime::Float64
    const modpath::Vector{String}   # seemingly not needed in Base, but used by Revise
end

function replace_depot_path(path::AbstractString)
    for depot in DEPOT_PATH
        !isdir(depot) && continue

        # Strip extraneous pathseps through normalization.
        if isdirpath(depot)
            depot = dirname(depot)
        end

        if startswith(path, depot)
            path = replace(path, depot => "@depot"; count=1)
            break
        end
    end
    return path
end

function restore_depot_path(path::AbstractString, depot::AbstractString)
    replace(path, r"^@depot" => depot; count=1)
end

function resolve_depot(inc::AbstractString)
    startswith(inc, string("@depot", Filesystem.pathsep())) || return :not_relocatable
    for depot in DEPOT_PATH
        ispath(restore_depot_path(inc, depot)) && return depot
    end
    return :no_depot_found
end


function parse_cache_header(f::IO, cachefile::AbstractString)
    flags = read(f, UInt8)
    modules = Vector{Pair{PkgId, UInt64}}()
    while true
        n = read(f, Int32)
        n == 0 && break
        sym = String(read(f, n)) # module name
        uuid = UUID((read(f, UInt64), read(f, UInt64))) # pkg UUID
        build_id = read(f, UInt64) # build UUID (mostly just a timestamp)
        push!(modules, PkgId(uuid, sym) => build_id)
    end
    totbytes = Int64(read(f, UInt64)) # total bytes for file dependencies + preferences
    # read the list of requirements
    # and split the list into include and requires statements
    includes = CacheHeaderIncludes[]
    requires = Pair{PkgId, PkgId}[]
    while true
        n2 = read(f, Int32)
        totbytes -= 4
        if n2 == 0
            break
        end
        depname = String(read(f, n2))
        totbytes -= n2
        fsize = read(f, UInt64)
        totbytes -= 8
        hash = read(f, UInt32)
        totbytes -= 4
        mtime = read(f, Float64)
        totbytes -= 8
        n1 = read(f, Int32)
        totbytes -= 4
        # map ids to keys
        modkey = (n1 == 0) ? PkgId("") : modules[n1].first
        modpath = String[]
        if n1 != 0
            # determine the complete module path
            while true
                n1 = read(f, Int32)
                totbytes -= 4
                if n1 == 0
                    break
                end
                push!(modpath, String(read(f, n1)))
                totbytes -= n1
            end
        end
        if depname[1] == '\0'
            push!(requires, modkey => binunpack(depname))
        else
            push!(includes, CacheHeaderIncludes(modkey, depname, fsize, hash, mtime, modpath))
        end
    end
    prefs = String[]
    while true
        n2 = read(f, Int32)
        totbytes -= 4
        if n2 == 0
            break
        end
        push!(prefs, String(read(f, n2)))
        totbytes -= n2
    end
    prefs_hash = read(f, UInt64)
    totbytes -= 8
    srctextpos = read(f, Int64)
    totbytes -= 8
    @assert totbytes == 0 "header of cache file appears to be corrupt (totbytes == $(totbytes))"
    # read the list of modules that are required to be present during loading
    required_modules = Vector{Pair{PkgId, UInt128}}()
    while true
        n = read(f, Int32)
        n == 0 && break
        sym = String(read(f, n)) # module name
        uuid = UUID((read(f, UInt64), read(f, UInt64))) # pkg UUID
        build_id = UInt128(read(f, UInt64)) << 64
        build_id |= read(f, UInt64)
        push!(required_modules, PkgId(uuid, sym) => build_id)
    end
    l = read(f, Int32)
    clone_targets = read(f, l)

    srcfiles = srctext_files(f, srctextpos, includes)

    includes_srcfiles = CacheHeaderIncludes[]
    includes_depfiles = CacheHeaderIncludes[]
    for (i, inc) in enumerate(includes)
        if inc.filename ∈ srcfiles
            push!(includes_srcfiles, inc)
        else
            push!(includes_depfiles, inc)
        end
    end

    # determine depot for @depot replacement for include() files and include_dependency() files separately
    srcfiles_depot = resolve_depot(first(srcfiles))
    if srcfiles_depot === :no_depot_found
        @debug("Unable to resolve @depot tag include() files from cache file $cachefile", srcfiles)
    elseif srcfiles_depot === :not_relocatable
        @debug("include() files from $cachefile are not relocatable", srcfiles)
    else
        for inc in includes_srcfiles
            inc.filename = restore_depot_path(inc.filename, srcfiles_depot)
        end
    end
    for inc in includes_depfiles
        depot = resolve_depot(inc.filename)
        if depot === :no_depot_found
            @debug("Unable to resolve @depot tag for include_dependency() file $(inc.filename) from cache file $cachefile", srcfiles)
        elseif depot === :not_relocatable
            @debug("include_dependency() file $(inc.filename) from $cachefile is not relocatable", srcfiles)
        else
            inc.filename = restore_depot_path(inc.filename, depot)
        end
    end

    return modules, (includes, includes_srcfiles, requires), required_modules, srctextpos, prefs, prefs_hash, clone_targets, flags
end

function parse_cache_header(cachefile::String)
    io = open(cachefile, "r")
    try
        iszero(isvalid_cache_header(io)) && throw(ArgumentError("Invalid header in cache file $cachefile."))
        ret = parse_cache_header(io, cachefile)
        return ret
    finally
        close(io)
    end
end

preferences_hash(f::IO, cachefile::AbstractString) = parse_cache_header(f, cachefile)[6]
function preferences_hash(cachefile::String)
    io = open(cachefile, "r")
    try
        if iszero(isvalid_cache_header(io))
            throw(ArgumentError("Invalid header in cache file $cachefile."))
        end
        return preferences_hash(io, cachefile)
    finally
        close(io)
    end
end

function cache_dependencies(f::IO, cachefile::AbstractString)
    _, (includes, _, _), modules, _... = parse_cache_header(f, cachefile)
    return modules, map(chi -> chi.filename, includes)  # return just filename
end

function cache_dependencies(cachefile::String)
    io = open(cachefile, "r")
    try
        iszero(isvalid_cache_header(io)) && throw(ArgumentError("Invalid header in cache file $cachefile."))
        return cache_dependencies(io, cachefile)
    finally
        close(io)
    end
end

function read_dependency_src(io::IO, cachefile::AbstractString, filename::AbstractString)
    _, (includes, _, _), _, srctextpos, _, _, _, _ = parse_cache_header(io, cachefile)
    srctextpos == 0 && error("no source-text stored in cache file")
    seek(io, srctextpos)
    return _read_dependency_src(io, filename, includes)
end

function _read_dependency_src(io::IO, filename::AbstractString, includes::Vector{CacheHeaderIncludes}=CacheHeaderIncludes[])
    while !eof(io)
        filenamelen = read(io, Int32)
        filenamelen == 0 && break
        depotfn = String(read(io, filenamelen))
        len = read(io, UInt64)
        fn = if !startswith(depotfn, string("@depot", Filesystem.pathsep()))
            depotfn
        else
            basefn = restore_depot_path(depotfn, "")
            idx = findfirst(includes) do inc
                endswith(inc.filename, basefn)
            end
            isnothing(idx) ? depotfn : includes[idx].filename
        end
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
        iszero(isvalid_cache_header(io)) && throw(ArgumentError("Invalid header in cache file $cachefile."))
        return read_dependency_src(io, cachefile, filename)
    finally
        close(io)
    end
end

function srctext_files(f::IO, srctextpos::Int64, includes::Vector{CacheHeaderIncludes})
    files = Set{String}()
    srctextpos == 0 && return files
    seek(f, srctextpos)
    while !eof(f)
        filenamelen = read(f, Int32)
        filenamelen == 0 && break
        filename = String(read(f, filenamelen))
        len = read(f, UInt64)
        push!(files, filename)
        seek(f, position(f) + len)
    end
    return files
end

# Test to see if this UUID is mentioned in this `Project.toml`; either as
# the top-level UUID (e.g. that of the project itself), as a dependency,
# or as an extra/weakdep for Preferences.
function get_uuid_name(project::Dict{String, Any}, uuid::UUID)
    uuid_p = get(project, "uuid", nothing)::Union{Nothing, String}
    name = get(project, "name", nothing)::Union{Nothing, String}
    if name !== nothing && uuid_p !== nothing && UUID(uuid_p) == uuid
        return name
    end
    deps = get(project, "deps", nothing)::Union{Nothing, Dict{String, Any}}
    if deps !== nothing
        for (k, v) in deps
            if uuid == UUID(v::String)
                return k
            end
        end
    end
    for subkey in ("deps", "extras", "weakdeps")
        subsection = get(project, subkey, nothing)::Union{Nothing, Dict{String, Any}}
        if subsection !== nothing
            for (k, v) in subsection
                if uuid == UUID(v::String)
                    return k
                end
            end
        end
    end
    return nothing
end

function get_uuid_name(project_toml::String, uuid::UUID)
    project = parsed_toml(project_toml)
    return get_uuid_name(project, uuid)
end

# If we've asked for a specific UUID, this function will extract the prefs
# for that particular UUID.  Otherwise, it returns all preferences.
function filter_preferences(prefs::Dict{String, Any}, pkg_name)
    if pkg_name === nothing
        return prefs
    else
        return get(Dict{String, Any}, prefs, pkg_name)::Dict{String, Any}
    end
end

function collect_preferences(project_toml::String, uuid::Union{UUID,Nothing})
    # We'll return a list of dicts to be merged
    dicts = Dict{String, Any}[]

    project = parsed_toml(project_toml)
    pkg_name = nothing
    if uuid !== nothing
        # If we've been given a UUID, map that to the name of the package as
        # recorded in the preferences section.  If we can't find that mapping,
        # exit out, as it means there's no way preferences can be set for that
        # UUID, as we only allow actual dependencies to have preferences set.
        pkg_name = get_uuid_name(project, uuid)
        if pkg_name === nothing
            return dicts
        end
    end

    # Look first inside of `Project.toml` to see we have preferences embedded within there
    proj_preferences = get(Dict{String, Any}, project, "preferences")::Dict{String, Any}
    push!(dicts, filter_preferences(proj_preferences, pkg_name))

    # Next, look for `(Julia)LocalPreferences.toml` files next to this `Project.toml`
    project_dir = dirname(project_toml)
    for name in preferences_names
        toml_path = joinpath(project_dir, name)
        if isfile(toml_path)
            prefs = parsed_toml(toml_path)
            push!(dicts, filter_preferences(prefs, pkg_name))

            # If we find `JuliaLocalPreferences.toml`, don't look for `LocalPreferences.toml`
            break
        end
    end

    return dicts
end

"""
    recursive_prefs_merge(base::Dict, overrides::Dict...)

Helper function to merge preference dicts recursively, honoring overrides in nested
dictionaries properly.
"""
function recursive_prefs_merge(base::Dict{String, Any}, overrides::Dict{String, Any}...)
    new_base = Base._typeddict(base, overrides...)

    for override in overrides
        # Clear entries are keys that should be deleted from any previous setting.
        override_clear = get(override, "__clear__", nothing)
        if override_clear isa Vector{String}
            for k in override_clear
                delete!(new_base, k)
            end
        end

        for (k, override_k) in override
            # Note that if `base` has a mapping that is _not_ a `Dict`, and `override`
            new_base_k = get(new_base, k, nothing)
            if new_base_k isa Dict{String, Any} && override_k isa Dict{String, Any}
                new_base[k] = recursive_prefs_merge(new_base_k, override_k)
            else
                new_base[k] = override_k
            end
        end
    end
    return new_base
end

function get_preferences(uuid::Union{UUID,Nothing} = nothing)
    merged_prefs = Dict{String,Any}()
    for env in reverse(load_path())
        project_toml = env_project_file(env)
        if !isa(project_toml, String)
            continue
        end

        # Collect all dictionaries from the current point in the load path, then merge them in
        dicts = collect_preferences(project_toml, uuid)
        merged_prefs = recursive_prefs_merge(merged_prefs, dicts...)
    end
    return merged_prefs
end

function get_preferences_hash(uuid::Union{UUID, Nothing}, prefs_list::Vector{String})
    # Start from a predictable hash point to ensure that the same preferences always
    # hash to the same value, modulo changes in how Dictionaries are hashed.
    h = UInt(0)
    uuid === nothing && return UInt64(h)

    # Load the preferences
    prefs = get_preferences(uuid)

    # Walk through each name that's called out as a compile-time preference
    for name in prefs_list
        prefs_value = get(prefs, name, nothing)
        if prefs_value !== nothing
            h = hash(prefs_value, h)::UInt
        end
    end
    # We always return a `UInt64` so that our serialization format is stable
    return UInt64(h)
end

get_preferences_hash(m::Module, prefs_list::Vector{String}) = get_preferences_hash(PkgId(m).uuid, prefs_list)

# This is how we keep track of who is using what preferences at compile-time
const COMPILETIME_PREFERENCES = Dict{UUID,Set{String}}()

# In `Preferences.jl`, if someone calls `load_preference(@__MODULE__, key)` while we're precompiling,
# we mark that usage as a usage at compile-time and call this method, so that at the end of `.ji` generation,
# we can record the list of compile-time preferences and embed that into the `.ji` header
function record_compiletime_preference(uuid::UUID, key::String)
    pref = get!(Set{String}, COMPILETIME_PREFERENCES, uuid)
    push!(pref, key)
    return nothing
end
get_compiletime_preferences(uuid::UUID) = collect(get(Vector{String}, COMPILETIME_PREFERENCES, uuid))
get_compiletime_preferences(m::Module) = get_compiletime_preferences(PkgId(m).uuid)
get_compiletime_preferences(::Nothing) = String[]

function check_clone_targets(clone_targets)
    rejection_reason = ccall(:jl_check_pkgimage_clones, Any, (Ptr{Cchar},), clone_targets)
    if rejection_reason !== nothing
        return rejection_reason
    end
end

# Set by FileWatching.__init__()
global mkpidlock_hook
global trymkpidlock_hook
global parse_pidfile_hook

# The preferences hash is only known after precompilation so just assume no preferences.
# Also ignore the active project, which means that if all other conditions are equal,
# the same package cannot be precompiled from different projects and/or different preferences at the same time.
compilecache_pidfile_path(pkg::PkgId) = compilecache_path(pkg, UInt64(0); project="") * ".pidfile"

const compilecache_pidlock_stale_age = 10

# Allows processes to wait if another process is precompiling a given source already.
# The lock file mtime will be updated when held at most every `stale_age/2` seconds, with expected
# variance of 10 seconds or more being infrequent but not unusual.
# After `stale_age` seconds beyond the mtime of the lock file, the lock file is deleted and
# precompilation will proceed if the locking process no longer exists or after `stale_age * 5`
# seconds if the process does still exist.
# If the lock is held by another host, it will conservatively wait `stale_age * 5`
# seconds since processes cannot be checked remotely
function maybe_cachefile_lock(f, pkg::PkgId, srcpath::String; stale_age=compilecache_pidlock_stale_age)
    if @isdefined(mkpidlock_hook) && @isdefined(trymkpidlock_hook) && @isdefined(parse_pidfile_hook)
        pidfile = compilecache_pidfile_path(pkg)
        cachefile = invokelatest(trymkpidlock_hook, f, pidfile; stale_age)
        if cachefile === false
            pid, hostname, age = invokelatest(parse_pidfile_hook, pidfile)
            verbosity = isinteractive() ? CoreLogging.Info : CoreLogging.Debug
            if isempty(hostname) || hostname == gethostname()
                @logmsg verbosity "Waiting for another process (pid: $pid) to finish precompiling $(repr("text/plain", pkg)). Pidfile: $pidfile"
            else
                @logmsg verbosity "Waiting for another machine (hostname: $hostname, pid: $pid) to finish precompiling $(repr("text/plain", pkg)). Pidfile: $pidfile"
            end
            # wait until the lock is available, but don't actually acquire it
            # returning nothing indicates a process waited for another
            return invokelatest(mkpidlock_hook, Returns(nothing), pidfile; stale_age)
        end
        return cachefile
    else
        # for packages loaded before FileWatching.__init__()
        f()
    end
end

function record_reason(reasons::Dict{String,Int}, reason::String)
    reasons[reason] = get(reasons, reason, 0) + 1
end
record_reason(::Nothing, ::String) = nothing
function list_reasons(reasons::Dict{String,Int})
    isempty(reasons) && return ""
    return "(cache misses: $(join(("$k ($v)" for (k,v) in reasons), ", ")))"
end
list_reasons(::Nothing) = ""

# returns true if it "cachefile.ji" is stale relative to "modpath.jl" and build_id for modkey
# otherwise returns the list of dependencies to also check
@constprop :none function stale_cachefile(modpath::String, cachefile::String; ignore_loaded::Bool = false, requested_flags::CacheFlags=CacheFlags(), reasons=nothing)
    return stale_cachefile(PkgId(""), UInt128(0), modpath, cachefile; ignore_loaded, requested_flags, reasons)
end
@constprop :none function stale_cachefile(modkey::PkgId, build_id::UInt128, modpath::String, cachefile::String;
                                            ignore_loaded::Bool = false, requested_flags::CacheFlags=CacheFlags(), reasons::Union{Dict{String,Int},Nothing}=nothing)
    io = open(cachefile, "r")
    try
        checksum = isvalid_cache_header(io)
        if iszero(checksum)
            @debug "Rejecting cache file $cachefile due to it containing an invalid cache header"
            record_reason(reasons, "invalid header")
            return true # invalid cache file
        end
        modules, (includes, _, requires), required_modules, srctextpos, prefs, prefs_hash, clone_targets, actual_flags = parse_cache_header(io, cachefile)
        if isempty(modules)
            return true # ignore empty file
        end
        if @ccall(jl_match_cache_flags(_cacheflag_to_uint8(requested_flags)::UInt8, actual_flags::UInt8)::UInt8) == 0
            @debug """
            Rejecting cache file $cachefile for $modkey since the flags are mismatched
              requested flags: $(requested_flags) [$(_cacheflag_to_uint8(requested_flags))]
              cache file:      $(CacheFlags(actual_flags)) [$actual_flags]
            """
            record_reason(reasons, "mismatched flags")
            return true
        end
        pkgimage = !isempty(clone_targets)
        if pkgimage
            ocachefile = ocachefile_from_cachefile(cachefile)
            if JLOptions().use_pkgimages == 0
                # presence of clone_targets means native code cache
                @debug "Rejecting cache file $cachefile for $modkey since it would require usage of pkgimage"
                record_reason(reasons, "requires pkgimages")
                return true
            end
            rejection_reasons = check_clone_targets(clone_targets)
            if !isnothing(rejection_reasons)
                @debug("Rejecting cache file $cachefile for $modkey:",
                    Reasons=rejection_reasons,
                    var"Image Targets"=parse_image_targets(clone_targets),
                    var"Current Targets"=current_image_targets())
                record_reason(reasons, "target mismatch")
                return true
            end
            if !isfile(ocachefile)
                @debug "Rejecting cache file $cachefile for $modkey since pkgimage $ocachefile was not found"
                record_reason(reasons, "missing ocachefile")
                return true
            end
        else
            ocachefile = nothing
        end
        id = first(modules)
        if id.first != modkey && modkey != PkgId("")
            @debug "Rejecting cache file $cachefile for $modkey since it is for $id instead"
            record_reason(reasons, "for different pkgid")
            return true
        end
        if build_id != UInt128(0)
            id_build = (UInt128(checksum) << 64) | id.second
            if id_build != build_id
                @debug "Ignoring cache file $cachefile for $modkey ($((UUID(id_build)))) since it does not provide desired build_id ($((UUID(build_id))))"
                record_reason(reasons, "for different buildid")
                return true
            end
        end
        id = id.first
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
                elseif ignore_loaded
                    # Used by Pkg.precompile given that there it's ok to precompile different versions of loaded packages
                    @goto locate_branch
                else
                    @debug "Rejecting cache file $cachefile because module $req_key is already loaded and incompatible."
                    record_reason(reasons, req_key == PkgId(Core) ? "wrong julia version" : "wrong dep version loaded")
                    return true # Won't be able to fulfill dependency
                end
            else
                @label locate_branch
                path = locate_package(req_key)
                if path === nothing
                    @debug "Rejecting cache file $cachefile because dependency $req_key not found."
                    record_reason(reasons, "dep missing source")
                    return true # Won't be able to fulfill dependency
                end
                depmods[i] = (path, req_key, req_build_id)
            end
        end

        # check if this file is going to provide one of our concrete dependencies
        # or if it provides a version that conflicts with our concrete dependencies
        # or neither
        skip_check = false
        for (req_key, req_build_id) in _concrete_dependencies
            build_id = get(modules, req_key, UInt64(0))
            if build_id !== UInt64(0)
                build_id |= UInt128(checksum) << 64
                if build_id === req_build_id
                    skip_check = true
                    break
                end
                @debug "Rejecting cache file $cachefile because it provides the wrong build_id (got $((UUID(build_id)))) for $req_key (want $(UUID(req_build_id)))"
                record_reason(reasons, "wrong dep buildid")
                return true # cachefile doesn't provide the required version of the dependency
            end
        end

        # now check if this file's content hash has changed relative to its source files
        if !skip_check
            if !samefile(includes[1].filename, modpath) && !samefile(fixup_stdlib_path(includes[1].filename), modpath)
                @debug "Rejecting cache file $cachefile because it is for file $(includes[1].filename) not file $modpath"
                record_reason(reasons, "wrong source")
                return true # cache file was compiled from a different path
            end
            for (modkey, req_modkey) in requires
                # verify that `require(modkey, name(req_modkey))` ==> `req_modkey`
                pkg = identify_package(modkey, req_modkey.name)
                if pkg != req_modkey
                    @debug "Rejecting cache file $cachefile because uuid mapping for $modkey => $req_modkey has changed, expected $modkey => $(repr("text/plain", pkg))"
                    record_reason(reasons, "dep uuid changed")
                    return true
                end
            end
            for chi in includes
                f, fsize_req, hash_req, ftime_req = chi.filename, chi.fsize, chi.hash, chi.mtime
                if startswith(f, string("@depot", Filesystem.pathsep()))
                    @debug("Rejecting stale cache file $cachefile because its depot could not be resolved")
                    record_reason(reasons, "nonresolveable depot")
                    return true
                end
                if !ispath(f)
                    _f = fixup_stdlib_path(f)
                    if isfile(_f) && startswith(_f, Sys.STDLIB)
                        continue
                    end
                    @debug "Rejecting stale cache file $cachefile because file $f does not exist"
                    record_reason(reasons, "missing sourcefile")
                    return true
                end
                if ftime_req >= 0.0
                    # this is an include_dependency for which we only recorded the mtime
                    ftime = mtime(f)
                    is_stale = ( ftime != ftime_req ) &&
                               ( ftime != floor(ftime_req) ) &&           # Issue #13606, PR #13613: compensate for Docker images rounding mtimes
                               ( ftime != ceil(ftime_req) ) &&            # PR: #47433 Compensate for CirceCI's truncating of timestamps in its caching
                               ( ftime != trunc(ftime_req, digits=6) ) && # Issue #20837, PR #20840: compensate for GlusterFS truncating mtimes to microseconds
                               ( ftime != 1.0 )  &&                       # PR #43090: provide compatibility with Nix mtime.
                               !( 0 < (ftime_req - ftime) < 1e-6 )        # PR #45552: Compensate for Windows tar giving mtimes that may be incorrect by up to one microsecond
                    if is_stale
                        @debug "Rejecting stale cache file $cachefile because mtime of include_dependency $f has changed (mtime $ftime, before $ftime_req)"
                        record_reason(reasons, "include_dependency mtime change")
                        return true
                    end
                else
                    fsize = filesize(f)
                    if fsize != fsize_req
                        @debug "Rejecting stale cache file $cachefile because file size of $f has changed (file size $fsize, before $fsize_req)"
                        record_reason(reasons, "include_dependency fsize change")
                        return true
                    end
                    hash = isdir(f) ? _crc32c(join(readdir(f))) : open(_crc32c, f, "r")
                    if hash != hash_req
                        @debug "Rejecting stale cache file $cachefile because hash of $f has changed (hash $hash, before $hash_req)"
                        record_reason(reasons, "include_dependency fhash change")
                        return true
                    end
                end
            end
        end

        if !isvalid_file_crc(io)
            @debug "Rejecting cache file $cachefile because it has an invalid checksum"
            record_reason(reasons, "invalid checksum")
            return true
        end

        if pkgimage
            if !isvalid_pkgimage_crc(io, ocachefile::String)
                @debug "Rejecting cache file $cachefile because $ocachefile has an invalid checksum"
                record_reason(reasons, "ocachefile invalid checksum")
                return true
            end
        end

        curr_prefs_hash = get_preferences_hash(id.uuid, prefs)
        if prefs_hash != curr_prefs_hash
            @debug "Rejecting cache file $cachefile because preferences hash does not match 0x$(string(prefs_hash, base=16)) != 0x$(string(curr_prefs_hash, base=16))"
            record_reason(reasons, "preferences hash mismatch")
            return true
        end

        return depmods, ocachefile # fresh cachefile
    finally
        close(io)
    end
end

"""
    @__FILE__ -> String

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
    @__DIR__ -> String

Macro to obtain the absolute path of the current directory as a string.

If in a script, returns the directory of the script containing the `@__DIR__` macrocall. If run from a
REPL or if evaluated by `julia -e <expr>`, returns the current working directory.

# Examples

The example illustrates the difference in the behaviors of `@__DIR__` and `pwd()`, by creating
a simple script in a different directory than the current working one and executing both commands:

```julia-repl
julia> cd("/home/JuliaUser") # working directory

julia> # create script at /home/JuliaUser/Projects
       open("/home/JuliaUser/Projects/test.jl","w") do io
           print(io, \"\"\"
               println("@__DIR__ = ", @__DIR__)
               println("pwd() = ", pwd())
           \"\"\")
       end

julia> # outputs script directory and current working directory
       include("/home/JuliaUser/Projects/test.jl")
@__DIR__ = /home/JuliaUser/Projects
pwd() = /home/JuliaUser
```
"""
macro __DIR__()
    __source__.file === nothing && return nothing
    _dirname = dirname(String(__source__.file::Symbol))
    return isempty(_dirname) ? pwd() : abspath(_dirname)
end

"""
    precompile(f, argtypes::Tuple{Vararg{Any}})

Compile the given function `f` for the argument tuple (of types) `argtypes`, but do not execute it.
"""
function precompile(@nospecialize(f), @nospecialize(argtypes::Tuple))
    precompile(Tuple{Core.Typeof(f), argtypes...})
end

const ENABLE_PRECOMPILE_WARNINGS = Ref(false)
function precompile(@nospecialize(argt::Type))
    ret = ccall(:jl_compile_hint, Int32, (Any,), argt) != 0
    if !ret && ENABLE_PRECOMPILE_WARNINGS[]
        @warn "Inactive precompile statement" maxlog=100 form=argt _module=nothing _file=nothing _line=0
    end
    return ret
end

# Variants that work for `invoke`d calls for which the signature may not be sufficient
precompile(mi::Core.MethodInstance, world::UInt=get_world_counter()) =
    (ccall(:jl_compile_method_instance, Cvoid, (Any, Any, UInt), mi, C_NULL, world); return true)

"""
    precompile(f, argtypes::Tuple{Vararg{Any}}, m::Method)

Precompile a specific method for the given argument types. This may be used to precompile
a different method than the one that would ordinarily be chosen by dispatch, thus
mimicking `invoke`.
"""
function precompile(@nospecialize(f), @nospecialize(argtypes::Tuple), m::Method)
    precompile(Tuple{Core.Typeof(f), argtypes...}, m)
end

function precompile(@nospecialize(argt::Type), m::Method)
    atype, sparams = ccall(:jl_type_intersection_with_env, Any, (Any, Any), argt, m.sig)::SimpleVector
    mi = Core.Compiler.specialize_method(m, atype, sparams)
    return precompile(mi)
end

@assert precompile(include_package_for_output, (PkgId, String, Vector{String}, Vector{String}, Vector{String}, typeof(_concrete_dependencies), Nothing))
@assert precompile(include_package_for_output, (PkgId, String, Vector{String}, Vector{String}, Vector{String}, typeof(_concrete_dependencies), String))
@assert precompile(create_expr_cache, (PkgId, String, String, String, typeof(_concrete_dependencies), Cmd, IO, IO))
@assert precompile(create_expr_cache, (PkgId, String, String, Nothing, typeof(_concrete_dependencies), Cmd, IO, IO))
