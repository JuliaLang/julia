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
    project_path = try
        realpath(project_file)
    catch ex
        ex isa IOError || rethrow()
        project_file
    end
    uuid = uuid5(ns_dummy_uuid, project_path)
    return uuid
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

struct TOMLCache
    p::TOML.Parser
    d::Dict{String, CachedTOMLDict}
end
const TOML_CACHE = TOMLCache(TOML.Parser(), Dict{String, Dict{String, Any}}())

parsed_toml(project_file::AbstractString) = parsed_toml(project_file, TOML_CACHE, require_lock)
function parsed_toml(project_file::AbstractString, toml_cache::TOMLCache, toml_lock::ReentrantLock)
    lock(toml_lock) do
        if !haskey(toml_cache.d, project_file)
            d = CachedTOMLDict(toml_cache.p, project_file)
            toml_cache.d[project_file] = d
            return d.d
        else
            # We are in a require call and have already parsed this TOML file
            # assume that it is unchanged to avoid hitting disk
            if CACHED_ENV_STACK[] !== nothing
                return d.d
            end
            d = toml_cache.d[project_file]
            return get_updated_dict(toml_cache.p, d)
        end
    end
end


const project_names = ("JuliaProject.toml", "Project.toml")
const manifest_names = ("JuliaManifest.toml", "Manifest.toml")
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

function project_file_uuid(d::Dict, project_file::String)::UUID
    uuid′ = get(d, "uuid", nothing)::Union{String, Nothing}
    return uuid′ === nothing ? dummy_uuid(project_file) : UUID(uuid′)
end

function project_file_manifest_path(project_file::String)::Union{Nothing,String}
    dir = dirname(project_file)
    for mfst in manifest_names
        manifest_file = joinpath(dir, mfst)
        isfile_casesensitive(manifest_file) && return manifest_file
    end
    return nothing
end
function is_v1_format_manifest(raw_manifest::Dict)
    if haskey(raw_manifest, "manifest_format")
        mf = raw_manifest["manifest_format"]
        if mf isa Dict && haskey(mf, "uuid")
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
        return get(Dict{String, Any}, raw_manifest, "deps")
    end
end

include("codeloading2.jl")


## other code loading functionality ##


"""
    pathof(m::Module)

Return the path of the `m.jl` file that was used to `import` module `m`,
or `nothing` if `m` was not imported from a package.

Use [`dirname`](@ref) to get the directory part and [`basename`](@ref)
to get the file name part of the path.
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

Return the root directory of the package that imported module `m`,
or `nothing` if `m` was not imported from a package. Optionally further
path component strings can be provided to construct a path within the
package root.

To get the root directory of the package that imported the current module
the form `pkgdir(@__MODULE__)` can be used.

```julia-repl
julia> pkgdir(Foo)
"/path/to/Foo.jl"

julia> pkgdir(Foo, "src", "file.jl")
"/path/to/Foo.jl/src/file.jl"
```

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

function find_source_file(path::AbstractString)
    (isabspath(path) || isfile(path)) && return path
    base_path = joinpath(Sys.BINDIR, DATAROOTDIR, "julia", "base", path)
    return isfile(base_path) ? normpath(base_path) : nothing
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
        isaccessibledir(path) || continue
        for file in readdir(path, sort = false) # no sort given we sort later
            if !((pkg.uuid === nothing && file == entryfile * ".ji") ||
                 (pkg.uuid !== nothing && startswith(file, entryfile * "_")))
                 continue
            end
            filepath = joinpath(path, file)
            isfile_casesensitive(filepath) && push!(paths, filepath)
        end
    end
    if length(paths) > 1
        # allocating the sort vector is less expensive than using sort!(.. by=mtime), which would
        # call the relatively slow mtime multiple times per path
        p = sortperm(mtime.(paths), rev = true)
        return paths[p]
    else
        return paths
    end
end

# use an Int counter so that nested @time_imports calls all remain open
const TIMING_IMPORTS = Threads.Atomic{Int}(0)

# these return either the array of modules loaded from the path / content given
# or an Exception that describes why it couldn't be loaded
# and it reconnects the Base.Docs.META
function _include_from_serialized(pkg::PkgId, path::String, depmods::Vector{Any})
    assert_havelock(require_lock)
    timing_imports = TIMING_IMPORTS[] > 0
    try
    if timing_imports
        t_before = time_ns()
        cumulative_compile_timing(true)
        t_comp_before = cumulative_compile_time_ns()
    end

    @debug "Loading cache file $path for $pkg"
    sv = ccall(:jl_restore_incremental, Any, (Cstring, Any), path, depmods)
    if isa(sv, Exception)
        return sv
    end
    sv = sv::SimpleVector
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
            ccall(:jl_init_restored_modules, Cvoid, (Any,), inits)
        finally
            lock(require_lock)
        end
    end

    for M in restored
        M = M::Module
        if parentmodule(M) === M && PkgId(M) == pkg
            if timing_imports
                elapsed = round((time_ns() - t_before) / 1e6, digits = 1)
                comp_time, recomp_time = cumulative_compile_time_ns() .- t_comp_before
                print(lpad(elapsed, 9), " ms  ")
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
    return ErrorException("Required dependency $pkg failed to load from a cache file.")

    finally
        timing_imports && cumulative_compile_timing(false)
    end
end

function run_package_callbacks(modkey::PkgId)
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

# loads a precompile cache file, after checking stale_cachefile tests
function _tryrequire_from_serialized(modkey::PkgId, build_id::UInt64)
    assert_havelock(require_lock)
    loaded = nothing
    if root_module_exists(modkey)
        loaded = root_module(modkey)
    else
        loading = get(package_locks, modkey, false)
        if loading !== false
            # load already in progress for this module
            loaded = wait(loading)
        else
            package_locks[modkey] = Threads.Condition(require_lock)
            try
                modpath = locate_package(modkey)
                modpath === nothing && return nothing
                set_pkgorigin_version_path(modkey, String(modpath))
                loaded = _require_search_from_serialized(modkey, String(modpath), build_id)
            finally
                loading = pop!(package_locks, modkey)
                notify(loading, loaded, all=true)
            end
            if loaded isa Module
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
function _tryrequire_from_serialized(modkey::PkgId, path::String, sourcepath::String, depmods::Vector{Any})
    assert_havelock(require_lock)
    loaded = nothing
    if root_module_exists(modkey)
        loaded = root_module(modkey)
    else
        loading = get(package_locks, modkey, false)
        if loading !== false
            # load already in progress for this module
            loaded = wait(loading)
        else
            for i in 1:length(depmods)
                dep = depmods[i]
                dep isa Module && continue
                _, depkey, depbuild_id = dep::Tuple{String, PkgId, UInt64}
                @assert root_module_exists(depkey)
                dep = root_module(depkey)
                depmods[i] = dep
            end
            package_locks[modkey] = Threads.Condition(require_lock)
            try
                set_pkgorigin_version_path(modkey, sourcepath)
                loaded = _include_from_serialized(modkey, path, depmods)
            finally
                loading = pop!(package_locks, modkey)
                notify(loading, loaded, all=true)
            end
            if loaded isa Module
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
# load the best available (non-stale) version of all dependent modules first
function _tryrequire_from_serialized(pkg::PkgId, path::String)
    assert_havelock(require_lock)
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
        dep = _tryrequire_from_serialized(modkey, build_id)
        if !isa(dep, Module)
            return dep
        end
        depmods[i] = dep
    end
    # then load the file
    return _include_from_serialized(pkg, path, depmods)
end

# returns `nothing` if require found a precompile cache for this sourcepath, but couldn't load it
# returns the set of modules restored if the cache load succeeded
@constprop :none function _require_search_from_serialized(pkg::PkgId, sourcepath::String, build_id::UInt64)
    assert_havelock(require_lock)
    paths = find_all_in_cache_path(pkg)
    for path_to_try in paths::Vector{String}
        staledeps = stale_cachefile(pkg, build_id, sourcepath, path_to_try)
        if staledeps === true
            continue
        end
        staledeps = staledeps::Vector{Any}
        # finish checking staledeps module graph
        for i in 1:length(staledeps)
            dep = staledeps[i]
            dep isa Module && continue
            modpath, modkey, modbuild_id = dep::Tuple{String, PkgId, UInt64}
            modpaths = find_all_in_cache_path(modkey)
            modfound = false
            for modpath_to_try in modpaths::Vector{String}
                modstaledeps = stale_cachefile(modkey, modbuild_id, modpath, modpath_to_try)
                if modstaledeps === true
                    continue
                end
                modstaledeps = modstaledeps::Vector{Any}
                staledeps[i] = (modpath, modkey, modpath_to_try, modstaledeps)
                modfound = true
                break
            end
            if !modfound
                @debug "Rejecting cache file $path_to_try because required dependency $modkey with build ID $modbuild_id is missing from the cache."
                staledeps = true
                break
            end
        end
        if staledeps === true
            continue
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
            modpath, modkey, modpath_to_try, modstaledeps = dep::Tuple{String, PkgId, String, Vector{Any}}
            dep = _tryrequire_from_serialized(modkey, modpath_to_try, modpath, modstaledeps)
            if !isa(dep, Module)
                @debug "Rejecting cache file $path_to_try because required dependency $modkey failed to load from cache file for $modpath." exception=dep
                staledeps = true
                break
            end
            staledeps[i] = dep
        end
        if staledeps === true
            continue
        end
        restored = _include_from_serialized(pkg, path_to_try, staledeps)
        if !isa(restored, Module)
            @debug "Deserialization checks failed while attempting to load cache from $path_to_try" exception=restored
        else
            return restored
        end
    end
    return
end

# to synchronize multiple tasks trying to import/using something
const package_locks = Dict{PkgId,Threads.Condition}()

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
        @lock require_lock begin
        push!(_require_dependencies, (mod, path, mtime(path)))
        end
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
    @lock require_lock begin
    no_initial_cache = CACHED_ENV_STACK[] === nothing
    try
    no_initial_cache && (CACHED_ENV_STACK[] = EnvironmentStack())
    uuidkey = identify_package(into, String(mod))
    # Core.println("require($(PkgId(into)), $mod) -> $uuidkey from env \"$env\"")
    if uuidkey === nothing
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
            start_sentence = hint ? "Otherwise, run" : "Run"
            throw(ArgumentError("""
                Package $mod not found in current path$hint_message.
                - $start_sentence `import Pkg; Pkg.add($(repr(String(mod))))` to install the $mod package."""))
        else
            throw(ArgumentError("""
            Package $(where.name) does not have $mod in its dependencies:
            - You may have a partially installed environment. Try `Pkg.instantiate()`
                to ensure all packages in the environment are installed.
            - Or, if you have $(where.name) checked out for development and have
                added $mod as a dependency but haven't updated your primary
                environment's manifest file, try `Pkg.resolve()`.
            - Otherwise you may need to report an issue with $(where.name)"""))
        end
        full_warning_showed[] = true
    end
    if _track_dependencies[]
        push!(_require_dependencies, (into, binpack(uuidkey), 0.0))
    end
    return _require_prelocked(uuidkey)
    finally
        no_initial_cache && (CACHED_ENV_STACK[] = nothing)
    end # try
    end # @lock
end

mutable struct PkgOrigin
    path::Union{String,Nothing}
    cachepath::Union{String,Nothing}
    version::Union{VersionNumber,Nothing}
end
PkgOrigin() = PkgOrigin(nothing, nothing, nothing)
const pkgorigins = Dict{PkgId,PkgOrigin}()

require(uuidkey::PkgId) = @lock require_lock _require_prelocked(uuidkey)

function _require_prelocked(uuidkey::PkgId)
    assert_havelock(require_lock)
    if !root_module_exists(uuidkey)
        newm = _require(uuidkey)
        if newm === nothing
            error("package `$(uuidkey.name)` did not define the expected \
                  module `$(uuidkey.name)`, check for typos in package module name")
        end
        # After successfully loading, notify downstream consumers
        run_package_callbacks(uuidkey)
    else
        newm = root_module(uuidkey)
    end
    return newm
end

const loaded_modules = Dict{PkgId,Module}()
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
            if (0 != ccall(:jl_generating_output, Cint, ())) && (JLOptions().incremental != 0)
                error("Replacing module `$(key.name)`")
            else
                @warn "Replacing module `$(key.name)`"
            end
        end
    end
    push!(loaded_modules_order, m)
    loaded_modules[key] = m
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
        if Core.Compiler.generating_sysimg()
            pkgorigin.version = get_pkgversion_from_path(joinpath(dirname(path), ".."))
        end
    end
    pkgorigin.path = path
    nothing
end

# Returns `nothing` or the new(ish) module
function _require(pkg::PkgId)
    assert_havelock(require_lock)
    # handle recursive calls to require
    loading = get(package_locks, pkg, false)
    if loading !== false
        # load already in progress for this module
        return wait(loading)
    end
    package_locks[pkg] = Threads.Condition(require_lock)

    last = toplevel_load[]
    loaded = nothing
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
        set_pkgorigin_version_path(pkg, path)

        # attempt to load the module file via the precompile cache locations
        if JLOptions().use_compiled_modules != 0
            m = _require_search_from_serialized(pkg, path, UInt64(0))
            if m isa Module
                return m
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
                    m = _tryrequire_from_serialized(pkg, cachefile)
                    if !isa(m, Module)
                        @warn "The call to compilecache failed to create a usable precompiled cache file for $pkg" exception=m
                    else
                        return m
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
        loading = pop!(package_locks, pkg)
        notify(loading, loaded, all=true)
    end
    return loaded
end

function _require_from_serialized(uuidkey::PkgId, path::String)
    @lock require_lock begin
    set_pkgorigin_version_path(uuidkey, nothing)
    newm = _tryrequire_from_serialized(uuidkey, path)
    newm isa Module || throw(newm)
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

# Example

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

# this is called in the external process that generates precompiled package files
function include_package_for_output(pkg::PkgId, input::String, depot_path::Vector{String}, dl_load_path::Vector{String}, load_path::Vector{String},
                                    concrete_deps::typeof(_concrete_dependencies), source::Union{Nothing,String})
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
    ccall(:jl_set_newly_inferred, Cvoid, (Any,), Core.Compiler.newly_inferred)
end

const PRECOMPILE_TRACE_COMPILE = Ref{String}()
const ENV_SERIALIZATION_FILE = Ref{Union{Nothing,String}}(nothing)
function create_expr_cache(pkg::PkgId, input::String, output::String, concrete_deps::typeof(_concrete_dependencies), internal_stderr::IO = stderr, internal_stdout::IO = stdout)
    @nospecialize internal_stderr internal_stdout
    rm(output, force=true)   # Remove file if it exists
    depot_path = map(abspath, DEPOT_PATH)
    dl_load_path = map(abspath, DL_LOAD_PATH)
    load_path = map(abspath, Base.load_path())
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
    deps_eltype = sprint(show, eltype(concrete_deps); context = :module=>nothing)
    deps = deps_eltype * "[" * join(deps_strs, ",") * "]"
    trace = isassigned(PRECOMPILE_TRACE_COMPILE) ? `--trace-compile=$(PRECOMPILE_TRACE_COMPILE[])` : ``
    io = open(pipeline(addenv(`$(julia_cmd()::Cmd) -O0
                              --output-ji $output --output-incremental=yes
                              --startup-file=no --history-file=no --warn-overwrite=yes
                              --color=$(have_color === nothing ? "auto" : have_color ? "yes" : "no")
                              $trace
                              -`,
                              "OPENBLAS_NUM_THREADS" => 1),
                       stderr = internal_stderr, stdout = internal_stdout),
              "w", stdout)
    # write data over stdin to avoid the (unlikely) case of exceeding max command line size
    serialization_pkgid = PkgId(UUID("9e88b42a-f829-5b0c-bbe9-9e923198166b"), "Serialization")
    if haskey(loaded_modules, serialization_pkgid)
        local ftmp
        if ENV_SERIALIZATION_FILE[] === nothing || !isfile(ENV_SERIALIZATION_FILE[])
            Serialization = loaded_modules[serialization_pkgid]
            ftmp, iotmp = mktemp()
            Serialization.serialize(iotmp, EnvironmentStack())
            close(iotmp)
        else
            ftmp = ENV_SERIALIZATION_FILE[]
        end
        write(io.in, """
            Base.ENV_SERIALIZATION_FILE[] = $(repr(ftmp))
            Base.CACHED_ENV_STACK[] = (Base.loaded_modules[Base.PkgId(Base.UUID("9e88b42a-f829-5b0c-bbe9-9e923198166b"), "Serialization")].deserialize(IOBuffer(read($(repr(ftmp))))))
        """)
    end
    write(io.in, """
        Base.include_package_for_output($(pkg_str(pkg)), $(repr(abspath(input))), $(repr(depot_path)), $(repr(dl_load_path)),
            $(repr(load_path)), $deps, $(repr(source_path(nothing))))
        """)
    close(io.in)
    return io
end

function compilecache_dir(pkg::PkgId)
    entrypath, entryfile = cache_file_entry(pkg)
    return joinpath(DEPOT_PATH[1], entrypath)
end

function compilecache_path(pkg::PkgId, prefs_hash::UInt64)::String
    entrypath, entryfile = cache_file_entry(pkg)
    cachepath = joinpath(DEPOT_PATH[1], entrypath)
    isaccessibledir(cachepath) || mkpath(cachepath)
    if pkg.uuid === nothing
        abspath(cachepath, entryfile) * ".ji"
    else
        crc = _crc32c(something(Base.active_project(), ""))
        crc = _crc32c(unsafe_string(JLOptions().image_file), crc)
        crc = _crc32c(unsafe_string(JLOptions().julia_bin), crc)
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
function compilecache(pkg::PkgId, internal_stderr::IO = stderr, internal_stdout::IO = stdout)
    @nospecialize internal_stderr internal_stdout
    path = locate_package(pkg)
    path === nothing && throw(ArgumentError("$pkg not found during precompilation"))
    return compilecache(pkg, path, internal_stderr, internal_stdout)
end

const MAX_NUM_PRECOMPILE_FILES = Ref(10)

function compilecache(pkg::PkgId, path::String, internal_stderr::IO = stderr, internal_stdout::IO = stdout,
                      keep_loaded_modules::Bool = true)

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
    @logmsg verbosity "Precompiling $pkg"

    # create a temporary file in `cachepath` directory, write the cache in it,
    # write the checksum, _and then_ atomically move the file to `cachefile`.
    mkpath(cachepath)
    tmppath, tmpio = mktemp(cachepath)
    local p
    try
        close(tmpio)
        p = create_expr_cache(pkg, path, tmppath, concrete_deps, internal_stderr, internal_stdout)
        if success(p)
            # append checksum to the end of the .ji file:
            open(tmppath, "a+") do f
                write(f, _crc32c(seekstart(f)))
            end
            # inherit permission from the source file (and make them writable)
            chmod(tmppath, filemode(path) & 0o777 | 0o200)

            # Read preferences hash back from .ji file (we can't precompute because
            # we don't actually know what the list of compile-time preferences are without compiling)
            prefs_hash = preferences_hash(tmppath)
            cachefile = compilecache_path(pkg, prefs_hash)

            # prune the directory with cache files
            if pkg.uuid !== nothing
                entrypath, entryfile = cache_file_entry(pkg)
                cachefiles = filter!(x -> startswith(x, entryfile * "_"), readdir(cachepath))
                if length(cachefiles) >= MAX_NUM_PRECOMPILE_FILES[]
                    idx = findmin(mtime.(joinpath.(cachepath, cachefiles)))[2]
                    rm(joinpath(cachepath, cachefiles[idx]); force=true)
                end
            end

            # this is atomic according to POSIX:
            rename(tmppath, cachefile; force=true)
            return cachefile
        end
    finally
        rm(tmppath, force=true)
    end
    if p.exitcode == 125
        return PrecompilableError()
    else
        error("Failed to precompile $pkg to $tmppath.")
    end
end

module_build_id(m::Module) = ccall(:jl_module_build_id, UInt64, (Any,), m)

isvalid_cache_header(f::IOStream) = (0 != ccall(:jl_read_verify_header, Cint, (Ptr{Cvoid},), f.ios))
isvalid_file_crc(f::IOStream) = (_crc32c(seekstart(f), filesize(f) - 4) == read(f, UInt32))

struct CacheHeaderIncludes
    id::PkgId
    filename::String
    mtime::Float64
    modpath::Vector{String}   # seemingly not needed in Base, but used by Revise
end

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
    totbytes = read(f, Int64) # total bytes for file dependencies + preferences
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
            push!(includes, CacheHeaderIncludes(modkey, depname, mtime, modpath))
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
    required_modules = Vector{Pair{PkgId, UInt64}}()
    while true
        n = read(f, Int32)
        n == 0 && break
        sym = String(read(f, n)) # module name
        uuid = UUID((read(f, UInt64), read(f, UInt64))) # pkg UUID
        build_id = read(f, UInt64) # build id
        push!(required_modules, PkgId(uuid, sym) => build_id)
    end
    return modules, (includes, requires), required_modules, srctextpos, prefs, prefs_hash
end

function parse_cache_header(cachefile::String; srcfiles_only::Bool=false)
    io = open(cachefile, "r")
    try
        !isvalid_cache_header(io) && throw(ArgumentError("Invalid header in cache file $cachefile."))
        ret = parse_cache_header(io)
        srcfiles_only || return ret
        modules, (includes, requires), required_modules, srctextpos, prefs, prefs_hash = ret
        srcfiles = srctext_files(io, srctextpos)
        delidx = Int[]
        for (i, chi) in enumerate(includes)
            chi.filename ∈ srcfiles || push!(delidx, i)
        end
        deleteat!(includes, delidx)
        return modules, (includes, requires), required_modules, srctextpos, prefs, prefs_hash
    finally
        close(io)
    end
end



preferences_hash(f::IO) = parse_cache_header(f)[end]
function preferences_hash(cachefile::String)
    io = open(cachefile, "r")
    try
        if !isvalid_cache_header(io)
            throw(ArgumentError("Invalid header in cache file $cachefile."))
        end
        return preferences_hash(io)
    finally
        close(io)
    end
end


function cache_dependencies(f::IO)
    defs, (includes, requires), modules, srctextpos, prefs, prefs_hash = parse_cache_header(f)
    return modules, map(chi -> (chi.filename, chi.mtime), includes)  # return just filename and mtime
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
    modules, (includes, requires), required_modules, srctextpos, prefs, prefs_hash = parse_cache_header(io)
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

function srctext_files(f::IO, srctextpos::Int64)
    files = Set{String}()
    srctextpos == 0 && return files
    seek(f, srctextpos)
    while !eof(f)
        filenamelen = read(f, Int32)
        filenamelen == 0 && break
        fn = String(read(f, filenamelen))
        len = read(f, UInt64)
        push!(files, fn)
        seek(f, position(f) + len)
    end
    return files
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

###############
# Preferences #
###############

function collect_preferences(env::ExplicitEnv, uuid::Union{UUID,Nothing})
    # We'll return a list of dicts to be merged
    dicts = Dict{String, Any}[]

    # If we've been given a UUID, map that to the name of the package as
    # recorded in the preferences section.  If we can't find that mapping,
    # exit out, as it means there's no way preferences can be set for that
    # UUID, as we only allow actual dependencies to have preferences set.
    pkg_name = nothing
    if uuid !== nothing
        for (name, uuid′) in env.project_deps
            uuid == uuid′ && (pkg_name = name)
        end
        if pkg_name === nothing
            for (name, uuid′) in env.project_extras
                uuid == uuid′ && (pkg_name = name)
            end
        end
        pkg_name === nothing && return dicts
    end

    # Look first to see if we have preferences embedded within there
    if env.prefs !== nothing
        push!(dicts, filter_preferences(env.prefs, pkg_name))
    end

    if env.local_prefs !== nothing
        push!(dicts, filter_preferences(env.local_prefs, pkg_name))
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

function get_preferences(uuid::Union{UUID,Nothing} = nothing, envstack=EnvironmentStack())
    merged_prefs = Dict{String,Any}()
    for env in reverse(envstack.envs)
        env isa ExplicitEnv || continue

        # Collect all dictionaries from the current point in the load path, then merge them in
        dicts = collect_preferences(env, uuid)
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

# returns true if it "cachefile.ji" is stale relative to "modpath.jl" and build_id for modkey
# otherwise returns the list of dependencies to also check
@constprop :none function stale_cachefile(modpath::String, cachefile::String; ignore_loaded::Bool = false)
    return stale_cachefile(PkgId(""), UInt64(0), modpath, cachefile; ignore_loaded)
end
@constprop :none function stale_cachefile(modkey::PkgId, build_id::UInt64, modpath::String, cachefile::String; ignore_loaded::Bool = false)
    io = open(cachefile, "r")
    try
        if !isvalid_cache_header(io)
            @debug "Rejecting cache file $cachefile due to it containing an invalid cache header"
            return true # invalid cache file
        end
        modules, (includes, requires), required_modules, srctextpos, prefs, prefs_hash = parse_cache_header(io)
        if isempty(modules)
            return true # ignore empty file
        end
        id = first(modules)
        if id.first != modkey && modkey != PkgId("")
            @debug "Rejecting cache file $cachefile for $modkey since it is for $id instead"
            return true
        end
        if build_id != UInt64(0) && id.second != build_id
            @debug "Ignoring cache file $cachefile for $modkey since it is does not provide desired build_id"
            return true
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
                    return true # Won't be able to fulfill dependency
                end
            else
                @label locate_branch
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
                @debug "Rejecting cache file $cachefile because it provides the wrong build_id (got $build_id) for $req_key (want $req_build_id)"
                return true # cachefile doesn't provide the required version of the dependency
            end
        end

        # now check if this file is fresh relative to its source files
        if !skip_timecheck
            if !samefile(includes[1].filename, modpath)
                @debug "Rejecting cache file $cachefile because it is for file $(includes[1].filename) not file $modpath"
                return true # cache file was compiled from a different path
            end
            for (modkey, req_modkey) in requires
                # verify that `require(modkey, name(req_modkey))` ==> `req_modkey`
                if identify_package(modkey, req_modkey.name) != req_modkey
                    @debug "Rejecting cache file $cachefile because uuid mapping for $modkey => $req_modkey has changed"
                    return true
                end
            end
            for chi in includes
                f, ftime_req = chi.filename, chi.mtime
                ftime = mtime(f)
                is_stale = ( ftime != ftime_req ) &&
                           ( ftime != floor(ftime_req) ) &&           # Issue #13606, PR #13613: compensate for Docker images rounding mtimes
                           ( ftime != ceil(ftime_req) ) &&            # PR: #47433 Compensate for CirceCI's truncating of timestamps in its caching
                           ( ftime != trunc(ftime_req, digits=6) ) && # Issue #20837, PR #20840: compensate for GlusterFS truncating mtimes to microseconds
                           ( ftime != 1.0 )  &&                       # PR #43090: provide compatibility with Nix mtime.
                           !( 0 < (ftime_req - ftime) < 1e-6 )        # PR #45552: Compensate for Windows tar giving mtimes that may be incorrect by up to one microsecond
                if is_stale
                    @debug "Rejecting stale cache file $cachefile (mtime $ftime_req) because file $f (mtime $ftime) has changed"
                    return true
                end
            end
        end

        if !isvalid_file_crc(io)
            @debug "Rejecting cache file $cachefile because it has an invalid checksum"
            return true
        end

        curr_prefs_hash = get_preferences_hash(id.uuid, prefs)
        if prefs_hash != curr_prefs_hash
            @debug "Rejecting cache file $cachefile because preferences hash does not match 0x$(string(prefs_hash, base=16)) != 0x$(string(curr_prefs_hash, base=16))"
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

precompile(include_package_for_output, (PkgId, String, Vector{String}, Vector{String}, Vector{String}, typeof(_concrete_dependencies), Nothing))
precompile(include_package_for_output, (PkgId, String, Vector{String}, Vector{String}, Vector{String}, typeof(_concrete_dependencies), String))
precompile(create_expr_cache, (PkgId, String, String, typeof(_concrete_dependencies), IO, IO))
