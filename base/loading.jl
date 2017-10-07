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
        buf = Vector{UInt8}(length(path_basename) + header_size + 1)
        while true
            ret = ccall(:getattrlist, Cint,
                        (Cstring, Ptr{Void}, Ptr{Void}, Csize_t, Culong),
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
        Vector{UInt8}(path_basename) == casepreserved_basename && return true

        # If there is no match, it's possible that the file does exist but HFS+
        # performed unicode normalization. See  https://developer.apple.com/library/mac/qa/qa1235/_index.html.
        isascii(path_basename) && return false
        Vector{UInt8}(normalize_string(path_basename, :NFD)) == casepreserved_basename
    end
else
    # Generic fallback that performs a slow directory listing.
    function isfile_casesensitive(path)
        isfile(path) || return false
        dir, filename = splitdir(path)
        any(readdir(dir) .== filename)
    end
end

function load_hook(prefix::String, name::String, ::Void)
    name_jl = "$name.jl"
    path = joinpath(prefix, name_jl)
    isfile_casesensitive(path) && return abspath(path)
    path = joinpath(prefix, name_jl, "src", name_jl)
    isfile_casesensitive(path) && return abspath(path)
    path = joinpath(prefix, name, "src", name_jl)
    isfile_casesensitive(path) && return abspath(path)
    return nothing
end
load_hook(prefix::String, name::String, path::String) = path
load_hook(prefix, name::String, ::Any) =
    throw(ArgumentError("unrecognized custom loader in LOAD_PATH: $prefix"))

_str(x::AbstractString) = String(x)
_str(x) = x

# `wd` is a working directory to search. defaults to current working directory.
# if `wd === nothing`, no extra path is searched.
function find_in_path(name::String, wd::Union{Void,String})
    isabspath(name) && return name
    base = name
    if endswith(name,".jl")
        base = name[1:prevind(name, end-2)]
    else
        name = string(base,".jl")
    end
    if wd !== nothing
        isfile_casesensitive(joinpath(wd,name)) && return joinpath(wd,name)
    end
    path = nothing
    path = _str(load_hook(_str(Pkg.dir()), base, path))
    for dir in LOAD_PATH
        path = _str(load_hook(_str(dir), base, path))
    end
    return path
end
find_in_path(name::AbstractString, wd::AbstractString = pwd()) =
    find_in_path(String(name), String(wd))

function find_source_file(file::String)
    (isabspath(file) || isfile(file)) && return file
    file2 = find_in_path(file)
    file2 !== nothing && return file2
    file2 = joinpath(JULIA_HOME, DATAROOTDIR, "julia", "base", file)
    return isfile(file2) ? file2 : nothing
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
function _include_from_serialized(content::Vector{UInt8})
    return ccall(:jl_restore_incremental_from_buf, Any, (Ptr{UInt8}, Int), content, sizeof(content))
end
function _include_from_serialized(path::String)
    return ccall(:jl_restore_incremental, Any, (Cstring,), path)
end

# returns an array of modules loaded, or an Exception that describes why it failed
# and it reconnects the Base.Docs.META
function _require_from_serialized(mod::Symbol, path_to_try::String)
    restored = _include_from_serialized(path_to_try)
    if !isa(restored, Exception)
        for M in restored::Vector{Any}
            if isdefined(M, Base.Docs.META)
                push!(Base.Docs.modules, M)
            end
        end
    end
    return restored
end

# returns `true` if require found a precompile cache for this mod, but couldn't load it
# returns `false` if the module isn't known to be precompilable
# returns the set of modules restored if the cache load succeeded
function _require_search_from_serialized(mod::Symbol, sourcepath::String)
    paths = find_all_in_cache_path(mod)
    for path_to_try in paths::Vector{String}
        if stale_cachefile(sourcepath, path_to_try)
            continue
        end
        restored = _require_from_serialized(mod, path_to_try)
        if isa(restored, Exception)
            if isa(restored, ErrorException) && endswith(restored.msg, " uuid did not match cache file.")
                # can't use this cache due to a module uuid mismatch,
                # defer reporting error until after trying all of the possible matches
                DEBUG_LOADING[] && info("JL_DEBUG_LOADING: Failed to load $path_to_try because $(restored.msg)")
                continue
            end
            warn("Deserialization checks failed while attempting to load cache from $path_to_try.")
            throw(restored)
        else
            return restored
        end
    end
    return !isempty(paths)
end

# this value is set by `require` based on whether JULIA_DEBUG_LOADING
# is presently defined as an environment variable
# and makes the logic in this file noisier about what it is doing and why
const DEBUG_LOADING = Ref(false)

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
const _concrete_dependencies = Any[] # these dependency versions are "set in stone", and the process should try to avoid invalidating them
const _require_dependencies = Any[] # a list of (mod, path, mtime) tuples that are the file dependencies of the module currently being precompiled
const _track_dependencies = Ref(false) # set this to true to track the list of file dependencies
function _include_dependency(modstring::AbstractString, _path::AbstractString)
    prev = source_path(nothing)
    if prev === nothing
        path = abspath(_path)
    else
        path = joinpath(dirname(prev), _path)
    end
    if _track_dependencies[]
        push!(_require_dependencies, (modstring, path, mtime(path)))
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
    _include_dependency("#__external__", path)
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

"""
    reload(name::AbstractString)

Force reloading of a package, even if it has been loaded before. This is intended for use
during package development as code is modified.
"""
function reload(name::AbstractString)
    if contains(name, Filesystem.path_separator) || contains(name, ".")
        # for reload("path/file.jl") just ask for include instead
        error("use `include` instead of `reload` to load source files")
    else
        # reload("Package") is ok
        unreference_module(Symbol(name))
        require(Symbol(name))
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
function require(mod::Symbol)
    if !root_module_exists(mod)
        _require(mod)
        # After successfully loading, notify downstream consumers
        if toplevel_load[] && myid() == 1 && nprocs() > 1
            # broadcast top-level import/using from node 1 (only)
            @sync for p in procs()
                p == 1 && continue
                @async remotecall_wait(p) do
                    require(mod)
                    nothing
                end
            end
        end
        for callback in package_callbacks
            invokelatest(callback, mod)
        end
    end
    return root_module(mod)
end

const loaded_modules = ObjectIdDict()
const module_keys = ObjectIdDict()

function register_root_module(key, m::Module)
    if haskey(loaded_modules, key)
        oldm = loaded_modules[key]
        if oldm !== m
            name = module_name(oldm)
            warn("replacing module $name.")
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

function register_all(a)
    for m in a
        if module_parent(m) === m
            register_root_module(module_name(m), m)
        end
    end
end

function _require(mod::Symbol)
    # dependency-tracking is only used for one top-level include(path),
    # and is not applied recursively to imported modules:
    old_track_dependencies = _track_dependencies[]
    _track_dependencies[] = false
    DEBUG_LOADING[] = haskey(ENV, "JULIA_DEBUG_LOADING")

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
        path = find_in_path(name, nothing)
        if path === nothing
            throw(ArgumentError("Module $name not found in current path.\nRun `Pkg.add(\"$name\")` to install the $name package."))
        end

        # attempt to load the module file via the precompile cache locations
        doneprecompile = false
        if JLOptions().use_compiled_modules != 0
            doneprecompile = _require_search_from_serialized(mod, path)
            if !isa(doneprecompile, Bool)
                register_all(doneprecompile)
                return
            end
        end

        # if the module being required was supposed to have a particular version
        # but it was not handled by the precompile loader, complain
        for (concrete_mod, concrete_uuid) in _concrete_dependencies
            if mod === concrete_mod
                warn("""Module $mod with uuid $concrete_uuid is missing from the cache.
                     This may mean module $mod does not support precompilation but is imported by a module that does.""")
                if JLOptions().incremental != 0
                    # during incremental precompilation, this should be fail-fast
                    throw(PrecompilableError(false))
                end
            end
        end

        if doneprecompile === true || JLOptions().incremental != 0
            # spawn off a new incremental pre-compile task for recursive `require` calls
            # or if the require search declared it was pre-compiled before (and therefore is expected to still be pre-compilable)
            cachefile = compilecache(mod)
            m = _require_from_serialized(mod, cachefile)
            if isa(m, Exception)
                warn("The call to compilecache failed to create a usable precompiled cache file for module $name. Got:")
                warn(m, prefix="WARNING: ")
                # fall-through, TODO: disable __precompile__(true) error so that the normal include will succeed
            else
                register_all(m)
                return
            end
        end

        # just load the file normally via include
        # for unknown dependencies
        try
            Base.include_relative(__toplevel__, path)
            return
        catch ex
            if doneprecompile === true || JLOptions().use_compiled_modules == 0 || !precompilableerror(ex, true)
                rethrow() # rethrow non-precompilable=true errors
            end
            # the file requested `__precompile__`, so try to build a cache file and use that
            cachefile = compilecache(mod)
            m = _require_from_serialized(mod, cachefile)
            if isa(m, Exception)
                warn(m, prefix="WARNING: ")
                # TODO: disable __precompile__(true) error and do normal include instead of error
                error("Module $mod declares __precompile__(true) but require failed to create a usable precompiled cache file.")
            end
            register_all(m)
        end
    finally
        toplevel_load[] = last
        loading = pop!(package_locks, mod)
        notify(loading, all=true)
        _track_dependencies[] = old_track_dependencies
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

function source_path(default::Union{AbstractString,Void}="")
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
    path, prev = _include_dependency(string(mod), _path)
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

function create_expr_cache(input::String, output::String, concrete_deps::Vector{Any})
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

compilecache(mod::Symbol) = compilecache(string(mod))

"""
    Base.compilecache(module::String)

Creates a precompiled cache file for
a module and all of its dependencies.
This can be used to reduce package load times. Cache files are stored in
`LOAD_CACHE_PATH[1]`, which defaults to `~/.julia/lib/VERSION`. See
[Module initialization and precompilation](@ref)
for important notes.
"""
function compilecache(name::String)
    # decide where to get the source file from
    path = find_in_path(name, nothing)
    path === nothing && throw(ArgumentError("$name not found in path"))
    path = String(path)
    # decide where to put the resulting cache file
    cachepath = LOAD_CACHE_PATH[1]
    if !isdir(cachepath)
        mkpath(cachepath)
    end
    cachefile::String = abspath(cachepath, name*".ji")
    # build up the list of modules that we want the precompile process to preserve
    concrete_deps = copy(_concrete_dependencies)
    for (key,mod) in loaded_modules
        if !(mod === Main || mod === Core || mod === Base)
            push!(concrete_deps, (key, module_uuid(mod)))
        end
    end
    # run the expression and cache the result
    if isinteractive() || DEBUG_LOADING[]
        if isfile(cachefile)
            info("Recompiling stale cache file $cachefile for module $name.")
        else
            info("Precompiling module $name.")
        end
    end
    if success(create_expr_cache(path, cachefile, concrete_deps))
        # append checksum to the end of the .ji file:
        open(cachefile, "a+") do f
            write(f, hton(crc32c(seekstart(f))))
        end
    else
        error("Failed to precompile $name to $cachefile.")
    end
    return cachefile
end

module_uuid(m::Module) = ccall(:jl_module_uuid, UInt64, (Any,), m)

isvalid_cache_header(f::IOStream) = 0 != ccall(:jl_read_verify_header, Cint, (Ptr{Void},), f.ios)

function parse_cache_header(f::IO)
    modules = Dict{Symbol,UInt64}()
    while true
        n = ntoh(read(f, Int32))
        n == 0 && break
        sym = Symbol(read(f, n)) # module symbol
        uuid = ntoh(read(f, UInt64)) # module UUID (mostly just a timestamp)
        modules[sym] = uuid
    end
    totbytes = ntoh(read(f, Int64)) # total bytes for file dependencies
    # read the list of files
    files = Tuple{String,String,Float64}[]
    while true
        n1 = ntoh(read(f, Int32))
        n1 == 0 && break
        @assert n1 >= 0 "EOF while reading cache header" # probably means this wasn't a valid file to be read by Base.parse_cache_header
        modname = String(read(f, n1))
        n2 = ntoh(read(f, Int32))
        @assert n2 >= 0 "EOF while reading cache header" # probably means this wasn't a valid file to be read by Base.parse_cache_header
        filename = String(read(f, n2))
        push!(files, (modname, filename, ntoh(read(f, Float64))))
        totbytes -= 8 + n1 + n2 + 8
    end
    @assert totbytes == 12 "header of cache file appears to be corrupt"
    srctextpos = ntoh(read(f, Int64))
    # read the list of modules that are required to be present during loading
    required_modules = Dict{Symbol,UInt64}()
    while true
        n = ntoh(read(f, Int32))
        n == 0 && break
        sym = Symbol(read(f, n)) # module symbol
        uuid = ntoh(read(f, UInt64)) # module UUID
        required_modules[sym] = uuid
    end
    return modules, files, required_modules, srctextpos
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
    defs, files, modules = parse_cache_header(f)
    return modules, map(mod_fl_mt -> (mod_fl_mt[2], mod_fl_mt[3]), files)  # discard the module
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
    modules, files, required_modules, srctextpos = parse_cache_header(io)
    srctextpos == 0 && error("no source-text stored in cache file")
    seek(io, srctextpos)
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

function stale_cachefile(modpath::String, cachefile::String)
    io = open(cachefile, "r")
    try
        if !isvalid_cache_header(io)
            DEBUG_LOADING[] && info("JL_DEBUG_LOADING: Rejecting cache file $cachefile due to it containing an invalid cache header.")
            return true # invalid cache file
        end
        modules, files, required_modules = parse_cache_header(io)

        # Check if transitive dependencies can be fullfilled
        for mod in keys(required_modules)
            if mod == :Main || mod == :Core || mod == :Base
                continue
            # Module is already loaded
            elseif root_module_exists(mod)
                continue
            end
            name = string(mod)
            path = find_in_path(name, nothing)
            if path === nothing
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
                    return false # this is the file we want
                end
                DEBUG_LOADING[] && info("JL_DEBUG_LOADING: Rejecting cache file $cachefile because it provides the wrong uuid (got $uuid) for $mod (want $uuid_req).")
                return true # cachefile doesn't provide the required version of the dependency
            end
        end

        # now check if this file is fresh relative to its source files
        if !samefile(files[1][2], modpath)
            DEBUG_LOADING[] && info("JL_DEBUG_LOADING: Rejecting cache file $cachefile because it is for file $(files[1][2])) not file $modpath.")
            return true # cache file was compiled from a different path
        end
        for (_, f, ftime_req) in files
            # Issue #13606: compensate for Docker images rounding mtimes
            # Issue #20837: compensate for GlusterFS truncating mtimes to microseconds
            ftime = mtime(f)
            if ftime != ftime_req && ftime != floor(ftime_req) && ftime != trunc(ftime_req, 6)
                DEBUG_LOADING[] && info("JL_DEBUG_LOADING: Rejecting stale cache file $cachefile (mtime $ftime_req) because file $f (mtime $ftime) has changed.")
                return true
            end
        end

        # finally, verify that the cache file has a valid checksum
        crc = crc32c(seekstart(io), filesize(io)-4)
        if crc != ntoh(read(io, UInt32))
            DEBUG_LOADING[] && info("JL_DEBUG_LOADING: Rejecting cache file $cachefile because it has an invalid checksum.")
            return true
        end

        return false # fresh cachefile
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
