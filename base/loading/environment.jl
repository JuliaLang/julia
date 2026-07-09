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

"""
    struct PkgLoadSpec

A PkgLoadSpec is the result of a `locate_package` operation and specifies how
and wherefrom to load a julia package.
"""
struct PkgLoadSpec
    path::String
    julia_syntax_version::VersionNumber
end

struct LoadingCache
    load_path::Vector{String}
    dummy_uuid::Dict{String, UUID}
    env_project_file::Dict{String, Union{Bool, String}}
    project_file_manifest_path::Dict{String, Union{Nothing, String}}
    require_parsed::Set{String}
    identified_where::Dict{Tuple{PkgId, String}, Union{Nothing, Tuple{PkgId, String}}}
    identified::Dict{String, Union{Nothing, Tuple{PkgId, String}}}
    located::Dict{Tuple{PkgId, Union{String, Nothing}}, Union{Tuple{PkgLoadSpec, String}, Nothing}}
end
const LOADING_CACHE = Ref{Union{LoadingCache, Nothing}}(nothing) # n.b.: all access to and through this are protected by require_lock
LoadingCache() = LoadingCache(
    load_path(),
    Dict{String, UUID}(),
    Dict{String, Union{Bool, String}}(),
    Dict{String, Union{Nothing, String}}(),
    Set{String}(),
    Dict{Tuple{PkgId, String}, Union{Nothing, Tuple{PkgId, String}}}(),
    Dict{String, Union{Nothing, Tuple{PkgId, String}}}(),
    Dict{Tuple{PkgId, Union{String, Nothing}}, Union{Tuple{PkgLoadSpec, String}, Nothing}}()
)


struct TOMLCache{Dates}
    p::TOML.Parser{Dates}
    d::Dict{String, CachedTOMLDict}
end
TOMLCache(p::TOML.Parser) = TOMLCache(p, Dict{String, CachedTOMLDict}())
TOMLCache(p::TOML.Parser, d::Dict{String, Dict{String, Any}}) = TOMLCache(p, convert(Dict{String, CachedTOMLDict}, d))

const TOML_CACHE = TOMLCache(TOML.Parser{nothing}())

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
function find_package(arg) # ::Union{Nothing,String}
    @lock require_lock begin
    pkgenv = identify_package_env(arg)
    pkgenv === nothing && return nothing
    pkg, env = pkgenv
    return locate_package(pkg, env)
    end
end

# is there a better/faster ground truth?
function is_stdlib(pkgid::PkgId)
    pkgid.name in readdir(Sys.STDLIB) || return false
    stdlib_root = joinpath(Sys.STDLIB, pkgid.name)
    project_file = locate_project_file(stdlib_root)
    if project_file isa String
        d = parsed_toml(project_file)
        uuid = get(d, "uuid", nothing)
        if uuid !== nothing
            return UUID(uuid) == pkgid.uuid
        end
    end
    return false
end

"""
    Base.identify_package_env(name::String)::Union{Tuple{PkgId, String}, Nothing}
    Base.identify_package_env(where::Union{Module,PkgId}, name::String)::Union{Tuple{PkgId, Union{String, Nothing}}, Nothing}

Same as [`Base.identify_package`](@ref) except that the path to the environment where the package is identified
is also returned, except when the identity is not identified.
"""
identify_package_env(where::Module, name::String) = identify_package_env(PkgId(where), name)
function identify_package_env(where::PkgId, name::String)
    # Special cases
    if where.name === name
        # Project tries to load itself
        return (where, nothing)
    elseif where.uuid === nothing
        # Project without Project.toml - treat as toplevel load
        return identify_package_env(nothing, name)
    end

    # Check if we have a cached answer for this
    assert_havelock(require_lock)
    cache = LOADING_CACHE[]
    cache_key = (where, name)
    if cache !== nothing
        pkg_env = get(cache.identified_where, cache_key, missing)
        pkg_env === missing || return pkg_env
    end

    # Main part: Search through all environments in the load path to see if we have
    # a matching entry.
    pkg_env = nothing
    for env in load_path()
        pkgid = environment_deps_get(env, where, name)
        # If we didn't find `where` at all, keep looking through the environment stack
        pkgid === nothing && continue
        if pkgid.uuid !== nothing
            pkg_env = (pkgid, env)
        end
        # If we don't have pkgid.uuid, still break here - this is a sentinel that indicates
        # that we've found `where` but it did not have the required dependency. We terminate the search.
        break
    end
    if pkg_env === nothing && is_stdlib(where)
        # if not found it could be that manifests are from a different julia version/commit
        # where stdlib dependencies have changed, so look up deps based on the stdlib Project.toml
        # as a fallback
        pkg_env = identify_stdlib_project_dep(where, name)
    end

    # Cache the result
    if cache !== nothing
        cache.identified_where[cache_key] = pkg_env
    end
    return pkg_env
end
function identify_package_env(where::Nothing, name::String)
    # Check if we have a cached answer for this
    assert_havelock(require_lock)
    cache = LOADING_CACHE[]
    if cache !== nothing
        pkg_env = get(cache.identified, name, missing)
        pkg_env === missing || return pkg_env
    end

    # Main part: Search through all environments in the load path to see if we have
    # a matching entry.
    pkg_env = nothing
    for env in load_path()
        pkgid = environment_deps_get(env, nothing, name)
        # If we didn't find `where` at all, keep looking through the environment stack
        pkgid === nothing && continue
        pkg_env = (pkgid, env)
        break
    end

    # Cache the result
    if cache !== nothing
        cache.identified[name] = pkg_env
    end
    return pkg_env
end
identify_package_env(name::String) = identify_package_env(nothing, name)

function identify_stdlib_project_dep(stdlib::PkgId, depname::String)
    @debug """
    Stdlib $(repr("text/plain", stdlib)) is trying to load `$depname`
    which is not listed as a dep in the load path manifests, so resorting to search
    in the stdlib Project.tomls for true deps"""
    stdlib_projfile = locate_project_file(joinpath(Sys.STDLIB, stdlib.name))
    stdlib_projfile === nothing && return nothing
    found = explicit_project_deps_get(stdlib_projfile, depname)
    if found !== nothing
        @debug "$(repr("text/plain", stdlib)) indeed depends on $depname in project $stdlib_projfile"
        pkgid = PkgId(found, depname)
        return pkgid, stdlib_projfile
    end
    return nothing
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

function locate_package_env(pkg::PkgId, stopenv::Union{String, Nothing}=nothing)::Union{Nothing,Tuple{PkgLoadSpec, String}}
    assert_havelock(require_lock)
    cache = LOADING_CACHE[]
    if cache !== nothing
        specenv = get(cache.located, (pkg, stopenv), missing)
        specenv === missing || return specenv
    end
    (env′, spec) = @label found begin
        if pkg.uuid === nothing
            # The project we're looking for does not have a Project.toml (n.b. - present
            # `Project.toml` without UUID gets a path-based dummy UUID). It must have
            # come from an implicit manifest environment, so go through those only.
            # N.B.: Implicitly loaded packages do not participate in syntax versioning.
            for env in load_path()
                project_file = env_project_file(env)
                (project_file isa Bool && project_file) || continue
                found = implicit_manifest_pkgid(env, pkg.name)
                if found !== nothing && found.uuid === nothing
                    @assert found.name == pkg.name
                    break found (env, implicit_manifest_uuid_load_spec(env, pkg))
                end
                if !(loading_extension || precompiling_extension)
                    stopenv == env && break found (nothing, nothing)
                end
            end
        else
            for env in load_path()
                spec = manifest_uuid_load_spec(env, pkg)
                # missing is used as a sentinel to stop looking further down in envs
                if spec === missing
                    is_stdlib(pkg) && break
                    break found (nothing, nothing)
                end
                if spec !== nothing
                    break found (env, spec)
                end
                if !(loading_extension || precompiling_extension)
                    stopenv == env && break
                end
            end
            # Allow loading of stdlibs if the name/uuid are given
            # e.g. if they have been explicitly added to the project/manifest
            mbyspec = manifest_uuid_load_spec(Sys.STDLIB, pkg)
            if mbyspec isa PkgLoadSpec
                break found (Sys.STDLIB, mbyspec)
            end
        end
        (nothing, nothing)
    end
    if spec !== nothing && !isfile_casesensitive(spec.path)
        spec = nothing
    end
    if cache !== nothing
        cache.located[(pkg, stopenv)] = spec === nothing ? nothing : (spec, something(env′))
    end
    spec === nothing && return nothing
    return spec, something(env′)
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
    @lock require_lock begin
        specenv = locate_package_env(pkg, stopenv)
        specenv === nothing && return nothing
        specenv[1].path
    end
end

function locate_package_load_spec(pkg::PkgId, stopenv::Union{String, Nothing}=nothing)::Union{Nothing,PkgLoadSpec}
    @lock require_lock begin
        specenv = locate_package_env(pkg, stopenv)
        specenv === nothing && return nothing
        specenv[1]
    end
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
    pkgid = PkgId(m)
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

If an extension module is given, the root of the parent package is returned.

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
    original = path
    path, base = splitdir(dirname(path))
    if base == "src"
        # package source in `../src/Foo.jl`
    elseif base == "ext"
        # extension source in `../ext/FooExt.jl`
    elseif basename(path) == "ext"
        # extension source in `../ext/FooExt/FooExt.jl`
        path = dirname(path)
    else
        error("Unexpected path structure for module source: $original")
    end
    return joinpath(path, paths...)
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

If the module `m` belongs to a versioned package, return the
version number of that package. Otherwise return `nothing`.

The version is read from the package's Project.toml during package
load.

To get the version of the package that imported the current module
the form `pkgversion(@__MODULE__)` can be used.

!!! compat "Julia 1.9"
    This function was introduced in Julia 1.9.
"""
function pkgversion(m::Module)
    @lock require_lock begin
        pkgorigin = get(pkgorigins, PkgId(moduleroot(m)), nothing)
        if pkgorigin !== nothing && pkgorigin.version !== nothing
            return pkgorigin.version
        end
        path = pkgdir(m)
        path === nothing && return nothing
        v = get_pkgversion_from_path(path)
        if pkgorigin !== nothing
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

function package_get_here(project_file, name::String)
    # if `where` matches the project, use [deps] section as manifest, and stop searching
    pkg_uuid = explicit_project_deps_get(project_file, name)
    pkg_uuid === nothing && return PkgId(name)
    return PkgId(pkg_uuid, name)
end

function package_get(project_file, where::Union{Nothing, PkgId}, name::String)
    if where !== nothing
        proj = project_file_name_uuid(project_file, where.name)
        proj != where && return nothing
    end
    return package_get_here(project_file, name)
end

ext_may_load_weakdep(exts::String, name::String) = exts == name
ext_may_load_weakdep(exts::Vector{String}, name::String) = name in exts

function package_extension_get(project_file, where::PkgId, name::String)
    d = parsed_toml(project_file)
    exts = get(d, "extensions", nothing)::Union{Dict{String, Any}, Nothing}
    if exts !== nothing
        proj = project_file_name_uuid(project_file, where.name)
        # Check if `where` is an extension of the project
        if where.name in keys(exts) && where.uuid == uuid5(proj.uuid::UUID, where.name)
            # Extensions can load weak deps if they are an extension trigger
            if ext_may_load_weakdep(exts[where.name]::Union{String, Vector{String}}, name)
                weakdeps = get(d, "weakdeps", nothing)::Union{Dict{String, Any}, Nothing}
                if weakdeps !== nothing
                    wuuid = get(weakdeps, name, nothing)::Union{String, Nothing}
                    if wuuid !== nothing
                        return PkgId(UUID(wuuid), name)
                    end
                end
            end
            # ... and they can load same deps as the project itself
            return package_get_here(project_file, name)
        end
    end
    return nothing
end

function environment_deps_get(env::String, where::Union{Nothing,PkgId}, name::String)::Union{Nothing,PkgId}
    @assert where === nothing || where.uuid !== nothing
    project_file = env_project_file(env)
    implicit_manifest = !(project_file isa String)
    if implicit_manifest
        project_file || return nothing
        if where === nothing
            # Toplevel load with a directory (implicit manifest) - all we look for is the
            # existence of the package name in the directory.
            pkg = implicit_manifest_pkgid(env, name)
            return pkg
        end
        project_file = implicit_manifest_project(env, where)
        project_file === nothing && return nothing
    end

    # Are we
    #    a) loading into a top-level project itself
    #    b) loading into a non-top-level project that was part of an implicit
    #       manifest environment (and for which we found the project file above)
    #    c) performing a top-level load (where === nothing) - i.e. we're looking
    #       at an environment's project file.
    #
    # If so, we may load either:
    #   I: the project itself (if name matches where)
    #   II: a dependency from [deps] section of the project file
    #
    # N.B.: Here "top-level" includes package loaded from an implicit manifest, which
    #       uses the same code path. Otherwise this is the active project.
    pkg = package_get(project_file, where, name)
    if pkg !== nothing
        if where === nothing && pkg.uuid === nothing
            # This is a top-level load - even though we didn't find the dependency
            # here, we still want to keep looking through the top-level environment stack.
            return nothing
        end
        return pkg
    end

    @assert where !== nothing

    # Are we an extension of a project from cases a), b) above
    # If so, in addition to I, II above, we get:
    #   III: A dependency from [weakdeps] section of the project file as long
    #        as it is an extension trigger for `where` in the `extensions` section.
    pkg = package_extension_get(project_file, where, name)
    pkg === nothing || return pkg

    if implicit_manifest
        # With an implicit manifest, getting here means that our (implicit) environment
        # *has* the package `where`. If we don't find it, it just means that `where` doesn't
        # have `name` as a dependency - c.f. the analogous case in `explicit_manifest_deps_get`.
        return PkgId(name)
    end

    # All other cases, dependencies come from the (top-level) manifest
    return explicit_manifest_deps_get(project_file, where, name)
end

function manifest_uuid_load_spec(env::String, pkg::PkgId)::Union{Nothing,PkgLoadSpec,Missing}
    project_file = env_project_file(env)
    if project_file isa String
        proj = project_file_name_uuid(project_file, pkg.name)
        if proj == pkg
            # if `pkg` matches the project, return the project itself
            return project_file_load_spec(project_file, pkg.name)
        end
        mby_ext = project_file_ext_load_spec(project_file, pkg)
        mby_ext === nothing || return mby_ext
        # look for manifest file and `where` stanza
        return explicit_manifest_uuid_load_spec(project_file, pkg)
    elseif project_file
        # if env names a directory, search it
        # Implicit environments do not participate in syntax versioning
        proj = implicit_manifest_uuid_load_spec(env, pkg)
        proj === nothing || return proj
        # if not found, this might be an extension - first we fast path needing
        # to scan the whole directory for a matching extension by peeking at
        # EXT_PRIMED. However, this only works if the parent package was loaded.
        # This is usually the case, but not always, e.g. in precompilation.
        triggers = get(EXT_PRIMED, pkg, nothing)
        if triggers !== nothing
            parentid = triggers[1]
            _, parent_project_file = entry_point_and_project_file(env, parentid.name)
            if parent_project_file !== nothing
                parentproj = project_file_name_uuid(parent_project_file, parentid.name)
                if parentproj == parentid
                    mby_ext = project_file_ext_load_spec(parent_project_file, pkg)
                    mby_ext === nothing || return mby_ext
                end
            end
        else
            # We still need to scan the whole directory for extensions.
            ext_ls, ext_proj = implicit_env_project_file_extension(env, pkg)
            ext_ls === nothing || return ext_ls
        end
    end
    return nothing
end


function find_ext_path(project_path::String, extname::String)
    extfiledir = joinpath(project_path, "ext", extname, extname * ".jl")
    isfile(extfiledir) && return extfiledir
    return joinpath(project_path, "ext", extname * ".jl")
end

function project_file_ext_load_spec(project_file::String, ext::PkgId)
    d = parsed_toml(project_file)
    p = dirname(project_file)
    exts = get(d, "extensions", nothing)::Union{Dict{String, Any}, Nothing}
    if exts !== nothing
        if ext.name in keys(exts) && ext.uuid == uuid5(UUID(d["uuid"]::String), ext.name)
            # Syntax version of the main package applies to its extensions
            return PkgLoadSpec(find_ext_path(p, ext.name), project_get_syntax_version(d))
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

const NON_VERSIONED_SYNTAX = v"1.13"

function project_get_syntax_version(d::Dict)
    # Syntax Evolution. First check syntax.julia_version entry
    sv = nothing
    ds = get(d, "syntax", nothing)
    if ds !== nothing
        sv = VersionNumber(get(ds, "julia_version", nothing))
    end
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

function project_file_load_spec(project_file::String, name::String)
    d = parsed_toml(project_file)
    entryfile = get(d, "path", nothing)::Union{String, Nothing}
    # "path" entry in project file is soft deprecated
    if entryfile === nothing
        entryfile = get(d, "entryfile", nothing)::Union{String, Nothing}
    end
    sv = project_get_syntax_version(d)
    return PkgLoadSpec(entry_path(dirname(project_file), name, entryfile), sv)
end

function workspace_manifest(project_file)
    base = base_project(project_file)
    if base !== nothing
        return project_file_manifest_path(base)
    end
    return nothing
end

struct VersionedParse
    ver::VersionNumber
end

function (vp::VersionedParse)(code, filename::String, lineno::Int, offset::Int, options::Symbol)
    if !isdefined(Base, :JuliaSyntax)
        if vp.ver === VERSION
            return Core._parse
        end
        error("JuliaSyntax module is required for syntax version $(vp.ver), but it is not loaded.")
    end
    Base.JuliaSyntax.core_parser_hook(code, filename, lineno, offset, options; syntax_version=vp.ver)
end

function parser_for_active_project()
    project = active_project()
    sv = VERSION
    if project !== nothing && isfile(project)
        try
            sv = project_get_syntax_version(parsed_toml(project))
        catch e
            @warn "Failed to read project $project - defaulting to latest syntax. err=$e"
        end
    end
    VersionedParse(sv)
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
    isfile_casesensitive(project_file) || return nothing
    d = parsed_toml(project_file)
    base_manifest = workspace_manifest(project_file)
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
    dir_name = joinpath(dir, name)
    path, project_file = entry_point_and_project_file_inside(dir_name, name)
    path === nothing || return path, project_file
    dir_jl = dir_name * ".jl"
    path, project_file = entry_point_and_project_file_inside(dir_jl, name)
    path === nothing || return path, project_file
    # check for less likely case with a bare file and no src directory last to minimize stat calls
    path = normpath(joinpath(dir, "$name.jl"))
    isfile_casesensitive(path) && return path, nothing
    return nothing, nothing
end

# Find the project file for the extension `ext` in the implicit env `dir``
function implicit_env_project_file_extension(dir::String, ext::PkgId)
    for pkg in readdir(dir; join=true)
        project_file = env_project_file(pkg)
        project_file isa String || continue
        ls = project_file_ext_load_spec(project_file, ext)
        if ls !== nothing
            return ls, project_file
        end
    end
    return nothing, nothing
end

# given a path, name, and possibly an entryfile, return the entry point
function entry_path(path::String, name::String, entryfile::Union{Nothing,String})::String
    isfile_casesensitive(path) && return normpath(path)
    entrypoint = entryfile === nothing ? joinpath("src", "$name.jl") : entryfile
    return normpath(joinpath(path, entrypoint))
end

## explicit project & manifest API ##

# find project file root or deps `name => uuid` mapping
# `ext` is the name of the extension if `name` is loaded from one
# return `nothing` if `name` is not found
function explicit_project_deps_get(project_file::String, name::String)::Union{Nothing,UUID}
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

function dep_stanza_get(stanza::Dict{String, Any}, name::String)::Union{Nothing, PkgId}
    for (dep, uuid) in stanza
        uuid::String
        if dep === name
            return PkgId(UUID(uuid), name)
        end
    end
    return nothing
end

function dep_stanza_get(stanza::Vector{String}, name::String)::Union{Nothing, PkgId}
    name in stanza && return PkgId(name)
    return nothing
end

dep_stanza_get(stanza::Nothing, name::String) = nothing

function explicit_manifest_deps_get(project_file::String, where::PkgId, name::String)::Union{Nothing,PkgId}
    manifest_file = project_file_manifest_path(project_file)
    manifest_file === nothing && return nothing # manifest not found--keep searching LOAD_PATH
    d = get_deps(parsed_toml(manifest_file))
    for (dep_name, entries) in d
        entries::Vector{Any}
        for entry in entries
            entry = entry::Dict{String, Any}
            uuid = get(entry, "uuid", nothing)::Union{String, Nothing}
            uuid === nothing && continue
            # deps is either a list of names (deps = ["DepA", "DepB"]) or
            # a table of entries (deps = {"DepA" = "6ea...", "DepB" = "55d..."}
            deps = get(entry, "deps", nothing)::Union{Vector{String}, Dict{String, Any}, Nothing}
            local dep::Union{Nothing, PkgId}
            @label resolved begin
                if UUID(uuid) === where.uuid
                    dep = dep_stanza_get(deps, name)

                    # We found `where` in this environment, but it did not have a deps entry for
                    # `name`. This is likely because the dependency was modified without a corresponding
                    # change to dependency's Project or our Manifest. Return a sentinel here indicating
                    # that we know the package, but do not know its UUID. The caller will terminate the
                    # search and provide an appropriate error to the user.
                    dep === nothing && return PkgId(name)
                else
                    # Check if we're trying to load into an extension of this package
                    extensions = get(entry, "extensions", nothing)
                    if extensions !== nothing
                        if haskey(extensions, where.name) && where.uuid == uuid5(UUID(uuid), where.name)
                            if name == dep_name
                                # Extension loads its base package
                                return PkgId(UUID(uuid), name)
                            end
                            exts = extensions[where.name]::Union{String, Vector{String}}
                            # Extensions are allowed to load:
                            # 1. Any ordinary dep of the parent package
                            # 2. Any weakdep of the parent package declared as an extension trigger
                            for deps′ in (ext_may_load_weakdep(exts, name) ?
                                    (get(entry, "weakdeps", nothing)::Union{Vector{String}, Dict{String, Any}, Nothing}, deps) :
                                    (deps,))
                                dep = dep_stanza_get(deps′, name)
                                dep === nothing && continue
                                break resolved
                            end
                            return PkgId(name)
                        end
                    end
                    continue
                end
            end

            dep.uuid !== nothing && return dep

            # We have the dep, but it did not specify a UUID. In this case,
            # it must be that the name is unique in the manifest - so lookup
            # the UUID at the top level by name
            name_deps = get(d, name, nothing)::Union{Nothing, Vector{Any}}
            if name_deps === nothing || length(name_deps) != 1
                error("expected a single entry for $(repr(name)) in $(repr(project_file))")
            end
            entry = first(name_deps::Vector{Any})::Dict{String, Any}
            uuid = get(entry, "uuid", nothing)::Union{String, Nothing}
            uuid === nothing && return PkgId(name)
            return PkgId(UUID(uuid), name)
        end
    end

    # We did not find `where` in this environment, either as a package or as an extension.
    # The caller should continue searching the environment stack.
    return nothing
end

# find `uuid` stanza, return the corresponding path
function explicit_manifest_uuid_load_spec(project_file::String, pkg::PkgId)::Union{Nothing,PkgLoadSpec,Missing}
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
                return explicit_manifest_entry_load_spec(manifest_file, pkg, entry)
            end
        end
    end
    # Extensions
    for (name, entries) in d
        entries = entries::Vector{Any}
        for entry in entries
            entry = entry::Dict{String, Any}
            uuid = get(entry, "uuid", nothing)::Union{Nothing, String}
            extensions = get(entry, "extensions", nothing)::Union{Nothing, Dict{String, Any}}
            if extensions !== nothing && haskey(extensions, pkg.name) && uuid !== nothing && uuid5(UUID(uuid), pkg.name) == pkg.uuid
                parent_load_spec = explicit_manifest_entry_load_spec(manifest_file, PkgId(UUID(uuid), name), entry)
                if parent_load_spec === nothing || parent_load_spec === missing
                    error("failed to find source of parent package: \"$name\"")
                end
                parent_path = parent_load_spec.path
                p = normpath(dirname(parent_path), "..")
                return PkgLoadSpec(find_ext_path(p, pkg.name), parent_load_spec.julia_syntax_version)
            end
        end
    end
    return nothing
end

function explicit_manifest_entry_load_spec(manifest_file::String, pkg::PkgId, entry::Dict{String,Any})::Union{Nothing, Missing, PkgLoadSpec}
    # Resolve syntax version. N.B.: Unlike in project files, an absent syntax.julia_version
    # entry in manifest files means defaulting to 1.13. This is because we assume the
    # manifest was created by an older version of julia that did not support syntax versioning.
    # Newer versions of Pkg will provide syntax version information in the manifest,
    # even if absent from the project file.
    syntax_version = NON_VERSIONED_SYNTAX
    syntax_table = get(entry, "syntax", nothing)
    if syntax_table !== nothing
        syntax_version = VersionNumber(get(syntax_table, "julia_version", nothing))
        # Clamp to minimum supported syntax version
        if syntax_version <= NON_VERSIONED_SYNTAX
            syntax_version = NON_VERSIONED_SYNTAX
        end
    end

    # Resolve path
    path = get(entry, "path", nothing)::Union{Nothing, String}
    entryfile = get(entry, "entryfile", nothing)::Union{Nothing, String}
    if path !== nothing
        path = entry_path(normpath(abspath(dirname(manifest_file), path)), pkg.name, entryfile)
        return PkgLoadSpec(path, syntax_version)
    end
    hash = get(entry, "git-tree-sha1", nothing)::Union{Nothing, String}
    if hash === nothing
        # stdlibs do not have a git-hash so cannot be loaded from depots. As
        # a special case, we allow loading these directly from the stdlib location
        # (treated as an implicit environment).
        mbyspec = manifest_uuid_load_spec(Sys.STDLIB, pkg)
        if mbyspec isa PkgLoadSpec && isfile(mbyspec.path)
            return mbyspec
        end
        return nothing
    end
    hash = SHA1(hash)
    # Keep the 4 since it used to be the default
    uuid = pkg.uuid::UUID # checked within `explicit_manifest_uuid_path`
    for slug in (version_slug(uuid, hash), version_slug(uuid, hash, 4))
        for depot in DEPOT_PATH
            path = joinpath(depot, "packages", pkg.name, slug)
            ispath(path) && return PkgLoadSpec(entry_path(abspath(path), pkg.name, entryfile), syntax_version)
        end
    end
    # no depot contains the package, return missing to stop looking
    return missing
end

## implicit project & manifest API ##
function implicit_manifest_pkgid(dir::String, name::String)::Union{Nothing,PkgId}
    path, project_file = entry_point_and_project_file(dir, name)
    if project_file === nothing
        path === nothing && return nothing
        return PkgId(name)
    end
    proj = project_file_name_uuid(project_file, name)
    proj.name == name || return nothing
    return proj
end

function implicit_manifest_project(dir::String, pkg::PkgId)::Union{Nothing, String}
    @assert pkg.uuid !== nothing
    project_file = entry_point_and_project_file(dir, pkg.name)[2]
    if project_file === nothing
        # `where` could be an extension
        return implicit_env_project_file_extension(dir, pkg)[2]
    end
    proj = project_file_name_uuid(project_file, pkg.name)
    proj == pkg || return nothing
    return project_file
end

# look for an entry-point for `pkg` and return its path if UUID matches
function implicit_manifest_uuid_load_spec(dir::String, pkg::PkgId)::Union{Nothing, PkgLoadSpec}
    path, project_file = entry_point_and_project_file(dir, pkg.name)
    if project_file === nothing
        pkg.uuid === nothing || return nothing
        # Without a project file, treat as empty - which defaults to VERSION
        return PkgLoadSpec(path, VERSION)
    end
    proj = project_file_name_uuid(project_file, pkg.name)
    proj == pkg || return nothing
    return PkgLoadSpec(path, project_get_syntax_version(parsed_toml(project_file)))
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
    recursive_prefs_merge(base::Dict{String, Any}, overrides::Vector{Dict{String, Any}})

Helper function to merge preference dicts recursively, honoring overrides in nested
dictionaries properly.
"""
function recursive_prefs_merge(base::Dict{String, Any}, overrides::Vector{Dict{String, Any}})
    merged = copy(base)

    for override in overrides
        # Clear entries are keys that should be deleted from any previous setting.
        override_clear = get(override, "__clear__", nothing)
        if override_clear isa Vector{String}
            for k in override_clear
                delete!(merged, k)
            end
        end

        for (k, override_k) in override
            # Note that if `base` has a mapping that is _not_ a `Dict`, and `override`
            merged_k = get(merged, k, nothing)
            if merged_k isa Dict{String, Any} && override_k isa Dict{String, Any}
                merged[k] = recursive_prefs_merge(merged_k, Dict{String,Any}[override_k])
            else
                merged[k] = override_k
            end
        end
    end
    return merged
end

function get_projects_workspace_to_root(project_file)
    projects = String[project_file]
    while true
        project_file = base_project(project_file)
        if project_file === nothing
            return projects
        end
        push!(projects, project_file)
    end
end

function get_preferences(uuid::Union{UUID,Nothing} = nothing)
    merged_prefs = Dict{String,Any}()
    loadpath = load_path()
    projects_to_merge_prefs = String[]
    append!(projects_to_merge_prefs, Iterators.drop(loadpath, 1))
    if length(loadpath) >= 1
        prepend!(projects_to_merge_prefs, get_projects_workspace_to_root(first(loadpath)))
    end

    for env in reverse(projects_to_merge_prefs)
        project_toml = env_project_file(env)
        if !isa(project_toml, String)
            continue
        end

        # Collect all dictionaries from the current point in the load path, then merge them in
        dicts = collect_preferences(project_toml, uuid)
        merged_prefs = recursive_prefs_merge(merged_prefs, dicts)
    end
    return merged_prefs
end
