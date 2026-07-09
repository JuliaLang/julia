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
        if !haskey(toml_cache.d, project_file)
            d = CachedTOMLDict(toml_cache.p, project_file)
            toml_cache.d[project_file] = d
            return d.d
        else
            d = toml_cache.d[project_file]
            return get_updated_dict(toml_cache.p, d)
        end
    end
end

## package identification: determine unique identity of package to be loaded ##

# Used by Pkg but not used in loading itself
function find_package(arg) # ::Union{Nothing,String}
    @lock require_lock begin
    stack = current_env_stack()
    pkgenv = identify_package_env(stack, arg)
    pkgenv === nothing && return nothing
    pkg, env = pkgenv
    specenv = locate_package_spec(stack, pkg, env)
    specenv === nothing && return nothing
    return specenv[1].path
    end
end

# Answered from the cached stdlib environment (`stdlib_env`), which already records the
# name -> uuid mapping for every stdlib, avoiding a `readdir` + TOML parse per call.
function is_stdlib(pkgid::PkgId)
    p = get(stdlib_env().pkgs, pkgid.name, nothing)
    p === nothing && return false
    return p.uuid == pkgid.uuid
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
    return identify_package_env(current_env_stack(), where, name)
end
identify_package_env(where::Nothing, name::String) = identify_package_env(name)
function identify_package_env(name::String)
    assert_havelock(require_lock)
    return identify_package_env(current_env_stack(), name)
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
    return locate_package_spec(current_env_stack(), pkg, stopenv)
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
    if isdir(env)
        project_file = locate_project_file(env)
    elseif basename(env) in project_names && isfile_casesensitive(env)
        project_file = env
    else
        project_file = false
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

ext_may_load_weakdep(exts::String, name::String) = exts == name
ext_may_load_weakdep(exts::Vector{String}, name::String) = name in exts


function find_ext_path(project_path::String, extname::String)
    extfiledir = joinpath(project_path, "ext", extname, extname * ".jl")
    isfile(extfiledir) && return extfiledir
    return joinpath(project_path, "ext", extname * ".jl")
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

# find `uuid` stanza, return the corresponding path

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
        mbyspec = _locate_package(stdlib_env(), pkg)
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

# Structured environment representation for code loading.
#
# The lookup functions above that resolve a package identity (`identify_package`)
# or a package location (`locate_package`) traditionally worked by repeatedly
# re-parsing the `Project.toml`/`Manifest.toml` files of each environment in the
# load path, using an ad-hoc per-`require` cache (`LOADING_CACHE`) to avoid the
# worst of the redundant work.
#
# The machinery below instead parses each environment in the load path exactly once into a
# structured, queryable object (`ExplicitEnv` for environments with a project file,
# `ImplicitEnv` for package directories) and bundles them into an `EnvironmentStack`.
# All identity/location queries are then answered from these in-memory structures.
#
# The manual's "Code Loading" chapter (doc/src/manual/code-loading.md) specifies package
# loading in terms of three maps defined by each environment: `roots`, `graph` and
# `paths`. The types here are the parsed, in-memory form of those maps; comments below
# note which map each structure or lookup corresponds to.
#
# The `ExplicitEnv` type is also used by `Precompilation` (see `precompilation.jl`)
# to build the dependency graph for parallel precompilation.

#########################
# Explicit environments #
#########################

# An explicit environment is a folder with a `Project.toml` file and (most often)
# a `Manifest.toml` file. The `Project.toml` file describes what can be loaded at
# top-level and the `Manifest.toml` describes what packages can be loaded in other
# packages as well as how the path is looked up for a package.
# In terms of the manual's maps: `project_deps` is the environment's "roots" map, `deps`
# (together with `weakdeps`/`extensions`) its "graph", and `lookup_strategy` plus
# `entryfile` the data from which its "paths" map entries are computed.
struct ExplicitEnv
    path::String
    manifest_path::Union{Nothing, String}
    project_name::Union{Nothing, String}
    project_uuid::Union{Nothing, UUID}
    project_deps::Dict{String, UUID}     # [deps] in the active project's Project.toml
    project_weakdeps::Dict{String, UUID} # [weakdeps] in the active project's Project.toml
    project_extras::Dict{String, UUID}   # [extras] in the active project's Project.toml
    project_extensions::Dict{String, Vector{UUID}} # [extensions] in the active project's Project.toml
    workspace_deps::Dict{String, UUID}   # union of [deps] from all workspace member Project.tomls
    deps::Dict{UUID, Vector{UUID}}       # full dependency graph from Manifest.toml
    weakdeps::Dict{UUID, Vector{UUID}}   # full weak dependency graph from Manifest.toml
    extensions::Dict{UUID, Dict{String, Vector{UUID}}}
    # Lookup name for a UUID
    names::Dict{UUID, String}
    lookup_strategy::Dict{UUID, Union{
                                      SHA1,     # `git-tree-sha1` entry
                                      String,   # `path` entry
                                      Nothing,  # stdlib (no `path` nor `git-tree-sha1`)
                                      Missing}} # not present in the manifest
    # Loading-specific per-package information used to build a `PkgLoadSpec`:
    entryfile::Dict{UUID, Union{Nothing, String}}  # `entryfile`/`path` entry within a package
    syntax_version::Dict{UUID, VersionNumber}      # resolved syntax version for each package
end

ExplicitEnv() = ExplicitEnv(active_project())
function ExplicitEnv(::Nothing, envpath::String="")
    ExplicitEnv(envpath,
        nothing,                  # manifest_path
        nothing,                  # project_name
        nothing,                  # project_uuid
        Dict{String, UUID}(),     # project_deps
        Dict{String, UUID}(),     # project_weakdeps
        Dict{String, UUID}(),     # project_extras
        Dict{String, Vector{UUID}}(), # project_extensions
        Dict{String, UUID}(),     # workspace_deps
        Dict{UUID, Vector{UUID}}(),   # deps
        Dict{UUID, Vector{UUID}}(),   # weakdeps
        Dict{UUID, Dict{String, Vector{UUID}}}(), # extensions
        Dict{UUID, String}(),     # names
        Dict{UUID, Union{SHA1, String, Nothing, Missing}}(), # lookup_strategy
        Dict{UUID, Union{Nothing, String}}(),  # entryfile
        Dict{UUID, VersionNumber}(),           # syntax_version
    )
end
# `workspace=true` additionally collects `workspace_deps` (the union of `[deps]` across all
# workspace members). That requires walking parent directories and parsing the member
# projects, which only `Precompilation` needs - the code-loading lookup never consults
# `workspace_deps`, so the `EnvironmentStack` builder passes `workspace=false`.
function ExplicitEnv(envpath::String; workspace::Bool=true)
    # Handle missing project file by creating an empty environment
    if !isfile(envpath)
        envpath = abspath(envpath)
        return ExplicitEnv(nothing, envpath)
    end
    envpath = abspath(envpath)
    project_d = parsed_toml(envpath)

    # TODO: Perhaps verify that two packages with the same UUID do not have different names?
    names = Dict{UUID, String}()
    project_uuid_to_name = Dict{String, UUID}()

    project_deps = Dict{String, UUID}()
    project_weakdeps = Dict{String, UUID}()
    project_extras = Dict{String, UUID}()

    # Collect all direct dependencies of the project
    for key in ("deps", "weakdeps", "extras")
        for (name, _uuid) in get(Dict{String, Any}, project_d, key)::Dict{String, Any}
            v = key == "deps" ? project_deps :
                key == "weakdeps" ? project_weakdeps :
                key == "extras" ? project_extras :
                error()
            uuid = UUID(_uuid::String)
            v[name] = uuid
            names[uuid] = name
            project_uuid_to_name[name] = uuid
        end
    end

    # A package in both deps and weakdeps is in fact only a weakdep
    for (name, _) in project_weakdeps
        delete!(project_deps, name)
    end

    # This project might be a package, in that case, that is also a "dependency"
    # of the project. A named project without an explicit `uuid` is still loadable as a
    # package: it is given a path-based dummy UUID (matching `project_file_name_uuid`).
    proj_name = get(project_d, "name", nothing)::Union{String, Nothing}
    _proj_uuid = get(project_d, "uuid", nothing)::Union{String, Nothing}
    proj_uuid = _proj_uuid !== nothing ? UUID(_proj_uuid) :
                proj_name !== nothing ? dummy_uuid(envpath) : nothing

    project_is_package = proj_name !== nothing
    if project_is_package
        project_deps[proj_name] = proj_uuid
        names[proj_uuid] = proj_name
    end

    project_extensions = Dict{String, Vector{UUID}}()
    # Collect all extensions of the project
    for (name, triggers) in get(Dict{String, Any}, project_d, "extensions")::Dict{String, Any}
        if triggers isa String
            triggers = [triggers]
        else
            triggers = triggers::Vector{String}
        end
        uuids = UUID[]
        for trigger in triggers
            uuid = get(project_uuid_to_name, trigger, nothing)
            if uuid === nothing
                error("Trigger $trigger for extension $name not found in project")
            end
            push!(uuids, uuid)
        end
        project_extensions[name] = uuids
    end

    manifest = project_file_manifest_path(envpath)
    manifest_d = manifest === nothing ? Dict{String, Any}() : parsed_toml(manifest)

    # Dependencies in a manifest can either be stored compressed (when name is unique among all packages)
    # in which case it is a `Vector{String}` or expanded where it is a `name => uuid` mapping.
    deps = Dict{UUID, Union{Vector{String}, Vector{UUID}}}()
    weakdeps = Dict{UUID, Union{Vector{String}, Vector{UUID}}}()
    extensions = Dict{UUID, Dict{String, Vector{String}}}()
    name_to_uuid = Dict{String, UUID}()
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

            # If we have multiple packages with the same name we will overwrite things here
            # but that is fine since we will only use the information in here for packages
            # with unique names
            names[m_uuid] = name
            name_to_uuid[name] = m_uuid

            for key in ("deps", "weakdeps")
                deps_pkg = get(Vector{String}, pkg_info, key)::Union{Vector{String}, Dict{String, Any}}
                d = key == "deps" ? deps :
                    key == "weakdeps" ? weakdeps :
                    error()

                # Compressed format with unique names:
                if deps_pkg isa Vector{String}
                    d[m_uuid] = deps_pkg
                # Expanded format:
                else
                    uuids = UUID[]
                    for (name_dep, _dep_uuid) in deps_pkg
                        dep_uuid = UUID(_dep_uuid::String)
                        push!(uuids, dep_uuid)
                        names[dep_uuid] = name_dep
                    end
                    d[m_uuid] = uuids
                end
            end

            # Extensions
            deps_pkg = get(Dict{String, Any}, pkg_info, "extensions")::Dict{String, Any}
            deps_pkg_concrete = Dict{String, Vector{String}}()
            for (ext, triggers) in deps_pkg
                if triggers isa String
                    triggers = [triggers]
                else
                    triggers = triggers::Vector{String}
                end
                deps_pkg_concrete[ext] = triggers
            end
            extensions[m_uuid] = deps_pkg_concrete

            # Determine strategy to find package
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

            # Resolve syntax version. N.B.: Unlike in project files, an absent
            # syntax.julia_version entry in manifest files means defaulting to
            # NON_VERSIONED_SYNTAX, because we assume the manifest was created by
            # an older version of julia that did not support syntax versioning.
            sv = NON_VERSIONED_SYNTAX
            syntax_table = get(pkg_info, "syntax", nothing)
            if syntax_table !== nothing
                sv = VersionNumber(get(syntax_table, "julia_version", nothing))
                if sv <= NON_VERSIONED_SYNTAX
                    sv = NON_VERSIONED_SYNTAX
                end
            end
            syntax_version[m_uuid] = sv
        end
    end

    # No matter if the deps were stored compressed or not in the manifest,
    # we internally store them expanded
    deps_expanded = Dict{UUID, Vector{UUID}}()
    weakdeps_expanded = Dict{UUID, Vector{UUID}}()
    extensions_expanded = Dict{UUID, Dict{String, Vector{UUID}}}()
    sizehint!(deps_expanded, length(deps))
    sizehint!(weakdeps_expanded, length(deps))
    sizehint!(extensions_expanded, length(deps))

    if proj_name !== nothing && proj_uuid !== nothing
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

    for key in ("deps", "weakdeps")
        d = key == "deps" ? deps :
            key == "weakdeps" ? weakdeps :
            error()
        d_expanded = key == "deps" ? deps_expanded :
                     key == "weakdeps" ? weakdeps_expanded :
                     error()
        for (pkg, deps) in d
            # dependencies was already expanded so use it directly:
            if deps isa Vector{UUID}
                d_expanded[pkg] = deps
                for dep in deps
                    name_to_uuid[names[dep]] = dep
                end
            # find the (unique) UUID associated with the name
            else
                deps_pkg = UUID[]
                sizehint!(deps_pkg, length(deps))
                for dep in deps
                    push!(deps_pkg, name_to_uuid[dep])
                end
                d_expanded[pkg] = deps_pkg
            end
        end
    end

    for (pkg, exts) in extensions
        exts_expanded = Dict{String, Vector{UUID}}()
        for (ext, triggers) in exts
            triggers_expanded = UUID[]
            sizehint!(triggers_expanded, length(triggers))
            for trigger in triggers
                push!(triggers_expanded, name_to_uuid[trigger])
            end
            exts_expanded[ext] = triggers_expanded
        end
        extensions_expanded[pkg] = exts_expanded
    end

    # Everything that does not yet have a lookup_strategy is missing from the manifest
    for (_, uuid) in project_deps
        get!(lookup_strategy, uuid, missing)
    end

    # Collect the union of [deps] from all workspace member projects
    # (see "Workspaces" in the manual). For non-workspace projects, this
    # is the same as project_deps.
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

    return ExplicitEnv(envpath, manifest, proj_name, proj_uuid,
                       project_deps, project_weakdeps, project_extras,
                       project_extensions, workspace_deps,
                       deps_expanded, weakdeps_expanded, extensions_expanded,
                       names, lookup_strategy, entryfile, syntax_version)
end

#########################
# Implicit environments #
#########################

# A package defined inside an implicit environment (package directory).
struct ImplicitEnvPkg
    uuid::Union{Nothing, UUID}          # nothing for a bare `X.jl` or project-less `X/src/X.jl`
    path::String                        # entry-point file (absolute)
    project_file::Union{Nothing, String}
    # The following are only meaningful when `project_file !== nothing`:
    deps::Dict{String, UUID}            # [deps] of the package's own Project.toml
    weakdeps::Dict{String, UUID}        # [weakdeps]
    extensions::Dict{String, Union{String, Vector{String}}} # extname => triggers
    syntax_version::VersionNumber
end

# An implicit environment (or package directory) is a folder in the LOAD_PATH without a project file.
# A package X exists in a package directory if the directory contains one of the following
# "entry point" files: `X.jl`, `X/src/X.jl` or `X.jl/src/X.jl`. See "Package directories"
# in the manual; `implicit_env_pkg` applies its "roots map" UUID rules, with `nothing`
# standing in for the nil UUID of project-less packages.
struct ImplicitEnv
    path::String
    pkgs::Dict{String, ImplicitEnvPkg}
end

function implicit_env_pkg(envpath::String, name::String)
    path, project_file = entry_point_and_project_file(envpath, name)
    path === nothing && return nothing
    if project_file === nothing
        return ImplicitEnvPkg(nothing, path, nothing,
                              Dict{String, UUID}(), Dict{String, UUID}(),
                              Dict{String, Union{String, Vector{String}}}(), VERSION)
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
    exts = Dict{String, Union{String, Vector{String}}}()
    for (ename, triggers) in get(Dict{String, Any}, d, "extensions")::Dict{String, Any}
        exts[ename] = triggers::Union{String, Vector{String}}
    end
    return ImplicitEnvPkg(uuid, path, project_file, deps, weakdeps, exts,
                          project_get_syntax_version(d))
end

function ImplicitEnv(envpath::String)
    envpath = abspath(envpath)
    pkgs = Dict{String, ImplicitEnvPkg}()
    for entry in readdir(envpath; sort=false)
        name, ext = splitext(entry)
        # Fast rejection: only bare `X.jl` files or directories `X`/`X.jl` can be packages
        if ext == ".jl"
            # could be a bare `X.jl` or a package folder `X.jl`
        elseif ext != ""
            continue
        end
        pkg = implicit_env_pkg(envpath, name)
        pkg === nothing && continue
        # Prefer a package with a project file / folder over a bare file with the same name
        existing = get(pkgs, name, nothing)
        if existing === nothing || (existing.project_file === nothing && pkg.project_file !== nothing)
            pkgs[name] = pkg
        end
    end
    return ImplicitEnv(envpath, pkgs)
end

# The stdlib environment is an implicit environment which is constant during a session,
# so cache it rather than re-`readdir`-ing and re-parsing ~60 Project.toml files per require.
const _STDLIB_ENV = Ref{Union{Nothing, ImplicitEnv}}(nothing)
function stdlib_env()
    env = _STDLIB_ENV[]
    if env === nothing || env.path != abspath(Sys.STDLIB)
        env = ImplicitEnv(Sys.STDLIB)
        _STDLIB_ENV[] = env
    end
    return env
end
reset_stdlib_env() = (_STDLIB_ENV[] = nothing)

####################
# EnvironmentStack #
####################

# An environment stack is the stack of environments formed via load_path() (the expanded LOAD_PATH).
# Queries walk `envs` in order, which realizes the merge favoring earlier entries described
# in "Environment stacks" in the manual without materializing merged maps.
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

# During a `require` call the environment stack is parsed exactly once and cached here
# (protected by `require_lock`), replacing the old per-`require` `LOADING_CACHE`.
# Outside of `require`, queries build a fresh stack on demand.
const ENV_STACK = Ref{Union{Nothing, EnvironmentStack}}(nothing)
function current_env_stack()
    s = ENV_STACK[]
    s === nothing && return EnvironmentStack()
    return s
end

# Cache of parsed `ExplicitEnv`s keyed by (absolute) project file, so that repeated
# programmatic one-off queries (where `ENV_STACK` is not set) are not dominated by the
# cost of rebuilding the whole environment from the manifest each time.
#
# Invalidation mirrors the `TOML_CACHE` (`CachedTOMLDict`): an entry is reused only while
# the project file and its resolved manifest file are unchanged, judged by the same
# `(inode, mtime, size)` signature. The manifest path is re-resolved on every call because
# for workspace members it depends on ancestor directories, not just the project file
# itself (`ExplicitEnv`'s content is otherwise a pure function of the two files, and
# `locate`'s depot/stdlib lookups happen at query time against the current `DEPOT_PATH`).
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

mutable struct CachedExplicitEnv
    const env::ExplicitEnv
    const project_sig::EnvStatSig
    const manifest_file::Union{Nothing, String}
    const manifest_sig::EnvStatSig
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

# Marker returned when we found the context package `where` (or determined that the
# active project's manifest is authoritative) but could not resolve the requested
# package: at that point we should stop searching further environments.
const STOP = :stop

# Given an env-local dependency list of UUIDs and a target name, return the matching PkgId.
function _find_dep(env::ExplicitEnv, dep_uuids::Vector{UUID}, name::String)::Union{Nothing, PkgId}
    for uuid in dep_uuids
        get(env.names, uuid, nothing) == name && return PkgId(uuid, name)
    end
    return nothing
end

# Is `pkg` an extension of some parent package recorded in `env.extensions`?
# (See "Package Extensions" in the manual.)
# Returns the parent UUID and the trigger UUIDs of that extension, or nothing.
function _extension_parent(env::ExplicitEnv, pkg::PkgId)::Union{Nothing, Tuple{UUID, Vector{UUID}}}
    for (parent_uuid, exts) in env.extensions
        for (extname, triggers) in exts
            if extname == pkg.name && uuid5(parent_uuid, extname) == pkg.uuid
                return (parent_uuid, triggers)
            end
        end
    end
    return nothing
end

## Identify ##
#
# Implements the manual's identity resolution: the `(env, name)` methods answer the
# `roots[name]` lookup and the `(env, where, name)` methods answer `graph[where.uuid][name]`.

# Top-level identification: does this environment's project expose `name`?
function _identify_package(env::ExplicitEnv, name::String)::Union{Nothing, PkgId}
    uuid = get(env.project_deps, name, nothing)
    uuid === nothing && return nothing
    return PkgId(uuid, name)
end

function _identify_package(env::ImplicitEnv, name::String)::Union{Nothing, PkgId}
    pkg = get(env.pkgs, name, nothing)
    pkg === nothing && return nothing
    return PkgId(pkg.uuid, name)
end

# Contextual identification: `where` (a package with a uuid) wants to load `name`.
function _identify_package(env::ExplicitEnv, where::PkgId, name::String)::Union{Nothing, PkgId, Symbol}
    where_deps = get(env.deps, where.uuid, nothing)
    if where_deps !== nothing
        # `where` is a package (or the project) known to this environment.
        # It may load itself, or any of its declared dependencies.
        where.name == name && return where
        pkg = _find_dep(env, where_deps, name)
        pkg !== nothing && return pkg
        # Found `where` but it does not declare `name` - stop searching.
        return STOP
    end
    # `where` might be an extension of a package known to this environment.
    ext = _extension_parent(env, where)
    if ext !== nothing
        parent_uuid, triggers = ext
        # Extension loading its parent package
        if get(env.names, parent_uuid, nothing) == name
            return PkgId(parent_uuid, name)
        end
        # Extensions can load any ordinary dep of the parent package...
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
    # `where` is not part of this environment - keep searching.
    return nothing
end

function _identify_package(env::ImplicitEnv, where::PkgId, name::String)::Union{Nothing, PkgId, Symbol}
    where_pkg = get(env.pkgs, where.name, nothing)
    if where_pkg !== nothing && where_pkg.uuid == where.uuid
        # A package with a uuid in an implicit environment must have a project file.
        where.name == name && return where
        uuid = get(where_pkg.deps, name, nothing)
        uuid !== nothing && return PkgId(uuid, name)
        # `where` exists here but does not declare `name`: with an implicit manifest this
        # terminates the search (analogous to the explicit-manifest case).
        return STOP
    end
    # `where` might be an extension of a package in this implicit environment.
    for (pname, ppkg) in env.pkgs
        ppkg.uuid === nothing && continue
        haskey(ppkg.extensions, where.name) || continue
        uuid5(ppkg.uuid, where.name) == where.uuid || continue
        # Found the parent
        pname == name && return PkgId(ppkg.uuid, name)
        uuid = get(ppkg.deps, name, nothing)
        uuid !== nothing && return PkgId(uuid, name)
        triggers = ppkg.extensions[where.name]
        if ext_may_load_weakdep(triggers, name)
            wuuid = get(ppkg.weakdeps, name, nothing)
            wuuid !== nothing && return PkgId(wuuid, name)
        end
        return STOP
    end
    return nothing
end

# Contextual identification returning also the load_path root where the identity was
# established (used as `stopenv` by `locate`). Mirrors the old `identify_package_env`.
function identify_package_env(envstack::EnvironmentStack, where::PkgId, name::String)::Union{Nothing, Tuple{PkgId, Union{String, Nothing}}}
    where.name == name && return (where, nothing)
    where.uuid === nothing && return identify_package_env(envstack, name)
    found = nothing
    for i in eachindex(envstack.envs)
        env = envstack.envs[i]
        pkg = _identify_package(env, where, name)
        if pkg === STOP
            # Found `where` but it does not declare `name`: terminate the search.
            break
        elseif pkg isa PkgId
            found = (pkg, envstack.roots[i])
            break
        end
        # nothing: keep looking
    end
    # Fallback: `where` is a stdlib but its dependency was not found in any manifest in the
    # load path (e.g. the manifests are from a different julia version). Resort to the
    # stdlib's own Project.toml.
    if found === nothing && is_stdlib(where)
        pkg = _identify_package(stdlib_env(), where, name)
        if pkg isa PkgId
            return (pkg, locate_project_file(joinpath(Sys.STDLIB::String, where.name)))
        end
    end
    return found
end

function identify_package_env(envstack::EnvironmentStack, name::String)::Union{Nothing, Tuple{PkgId, String}}
    for i in eachindex(envstack.envs)
        pkg = _identify_package(envstack.envs[i], name)
        pkg !== nothing && return (pkg, envstack.roots[i])
    end
    return nothing
end

identify_package(envstack::EnvironmentStack, where::PkgId, name::String) = _nothing_or_first(identify_package_env(envstack, where, name))
identify_package(envstack::EnvironmentStack, name::String) = _nothing_or_first(identify_package_env(envstack, name))

## Locate ##

function _locate_package(env::ExplicitEnv, pkg::PkgId)::Union{Nothing, PkgLoadSpec, Missing}
    strategy = get(env.lookup_strategy, pkg.uuid, missing)
    entryfile = get(env.entryfile, pkg.uuid, nothing)
    if !(strategy isa Missing) || haskey(env.lookup_strategy, pkg.uuid)
        sv = get(env.syntax_version, pkg.uuid, VERSION)
        if strategy isa Missing
            # In project_deps but not in the manifest - stop searching.
            return missing
        elseif strategy isa Nothing
            # stdlib (no `path` nor `git-tree-sha1`): load it from the stdlib location.
            spec = _locate_package(stdlib_env(), pkg)
            spec isa PkgLoadSpec && return spec
            return nothing
        elseif strategy isa String
            if pkg.uuid == env.project_uuid
                # The project-as-package: root is the project dir (already absolute).
                return PkgLoadSpec(entry_path(strategy, pkg.name, entryfile), sv)
            end
            # `path` entries in a manifest are relative to the manifest directory.
            root = normpath(abspath(dirname(env.manifest_path::String), strategy))
            return PkgLoadSpec(entry_path(root, pkg.name, entryfile), sv)
        elseif strategy isa SHA1
            for slug in (version_slug(pkg.uuid, strategy), version_slug(pkg.uuid, strategy, 4))
                for depot in DEPOT_PATH
                    path = joinpath(depot, "packages", pkg.name, slug)
                    ispath(path) && return PkgLoadSpec(entry_path(abspath(path), pkg.name, entryfile), sv)
                end
            end
            # No depot contains the package - stop searching.
            return missing
        end
    end
    # The package might be an extension of a manifest package.
    ext = _extension_parent(env, pkg)
    if ext !== nothing
        parent_uuid, _ = ext
        parent_spec = _locate_package(env, PkgId(parent_uuid, env.names[parent_uuid]))
        if parent_spec isa PkgLoadSpec
            p = normpath(dirname(parent_spec.path), "..")
            return PkgLoadSpec(find_ext_path(p, pkg.name), parent_spec.julia_syntax_version)
        end
    end
    return nothing
end

function _locate_package(env::ImplicitEnv, pkg::PkgId)::Union{Nothing, PkgLoadSpec}
    p = get(env.pkgs, pkg.name, nothing)
    if p !== nothing && p.uuid == pkg.uuid
        return PkgLoadSpec(p.path, p.syntax_version)
    end
    # The package might be an extension of a package in this implicit environment.
    for (pname, ppkg) in env.pkgs
        ppkg.uuid === nothing && continue
        haskey(ppkg.extensions, pkg.name) || continue
        uuid5(ppkg.uuid, pkg.name) == pkg.uuid || continue
        root = dirname(dirname(ppkg.path)) # <root>/src/<name>.jl -> <root>
        return PkgLoadSpec(find_ext_path(root, pkg.name), ppkg.syntax_version)
    end
    return nothing
end

function locate_package_spec(envstack::EnvironmentStack, pkg::PkgId, stopenv::Union{String, Nothing}=nothing)::Union{Nothing, Tuple{PkgLoadSpec, String}}
    specenv = _locate_package_spec(envstack, pkg, stopenv)
    if specenv !== nothing && !isfile_casesensitive(specenv[1].path)
        return nothing
    end
    return specenv
end

function _locate_package_spec(envstack::EnvironmentStack, pkg::PkgId, stopenv::Union{String, Nothing})::Union{Nothing, Tuple{PkgLoadSpec, String}}
    if pkg.uuid === nothing
        # Implicit (project-less) packages: only look through implicit environments.
        # N.B.: Implicitly loaded packages do not participate in syntax versioning.
        for i in eachindex(envstack.envs)
            env = envstack.envs[i]
            env isa ImplicitEnv || continue
            spec = _locate_package(env, pkg)
            spec isa PkgLoadSpec && return (spec, envstack.roots[i])
            if !(loading_extension || precompiling_extension)
                stopenv == envstack.roots[i] && return nothing
            end
        end
        return nothing
    end
    for i in eachindex(envstack.envs)
        env = envstack.envs[i]
        spec = _locate_package(env, pkg)
        if spec === missing
            # Stop searching, unless this is a stdlib that may be loadable from its location.
            is_stdlib(pkg) && break
            return nothing
        end
        spec isa PkgLoadSpec && return (spec, envstack.roots[i])
        if !(loading_extension || precompiling_extension)
            stopenv == envstack.roots[i] && break
        end
    end
    # Allow loading of stdlibs if the name/uuid are given e.g. if they have been
    # explicitly added to the project/manifest.
    spec = _locate_package(stdlib_env(), pkg)
    spec isa PkgLoadSpec && return (spec, Sys.STDLIB)
    return nothing
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
