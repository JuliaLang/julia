# This file is a part of Julia. License is MIT: http://julialang.org/license

# Base.require is the implementation for the `import` statement

# Cross-platform case-sensitive path canonicalization

if is_unix() && !is_apple()
    # assume case-sensitive filesystems, don't have to do anything
    isfile_casesensitive(path) = isfile(path)
elseif is_windows()
    # GetLongPathName Win32 function returns the case-preserved filename on NTFS.
    function isfile_casesensitive(path)
        isfile(path) || return false  # Fail fast
        basename(Filesystem.longpath(path)) == basename(path)
    end
elseif is_apple()
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
        const header_size = 12
        buf = Array{UInt8}(length(path_basename) + header_size + 1)
        while true
            ret = ccall(:getattrlist, Cint,
                        (Cstring, Ptr{Void}, Ptr{Void}, Csize_t, Culong),
                        path, attr_list, buf, sizeof(buf), FSOPT_NOFOLLOW)
            systemerror(:getattrlist, ret ≠ 0)
            filename_length = unsafe_load(
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
        base = name[1:end-3]
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

function find_in_node_path(name::String, srcpath, node::Int=1)
    if myid() == node
        return find_in_path(name, srcpath)
    else
        return remotecall_fetch(find_in_path, node, name, srcpath)
    end
end

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
# and also attempts to load the same file across all nodes (if toplevel_node and myid() == master)
# and it reconnects the Base.Docs.META
function _require_from_serialized(node::Int, mod::Symbol, path_to_try::String, toplevel_load::Bool)
    local restored = nothing
    local content::Vector{UInt8}
    if toplevel_load && myid() == 1 && nprocs() > 1
        # broadcast top-level import/using from node 1 (only)
        if node == myid()
            content = open(read, path_to_try)
        else
            content = remotecall_fetch(open, node, read, path_to_try)
        end
        restored = _include_from_serialized(content)
        isa(restored, Exception) && return restored
        others = filter(x -> x != myid(), procs())
        refs = Any[
            (p, @spawnat(p,
                let m = try
                            _include_from_serialized(content)
                        catch ex
                            isa(ex, Exception) ? ex : ErrorException(string(ex))
                        end
                    isa(m, Exception) ? m : nothing
                end))
            for p in others ]
        for (id, ref) in refs
            m = fetch(ref)
            if m !== nothing
                warn("Node state is inconsistent: node $id failed to load cache from $path_to_try. Got:")
                warn(m, prefix="WARNING: ")
            end
        end
    elseif node == myid()
        restored = _include_from_serialized(path_to_try)
    else
        content = remotecall_fetch(open, node, read, path_to_try)
        restored = _include_from_serialized(content)
    end

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
function _require_search_from_serialized(node::Int, mod::Symbol, sourcepath::String, toplevel_load::Bool)
    if node == myid()
        paths = find_all_in_cache_path(mod)
    else
        paths = @fetchfrom node find_all_in_cache_path(mod)
    end

    for path_to_try in paths::Vector{String}
        if stale_cachefile(sourcepath, path_to_try)
            continue
        end
        restored = _require_from_serialized(node, mod, path_to_try, toplevel_load)
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

# used to optionally track dependencies when requiring a module:
const _concrete_dependencies = Any[] # these dependency versions are "set in stone", and the process should try to avoid invalidating them
const _require_dependencies = Any[] # a list of (path, mtime) tuples that are the file dependencies of the module currently being precompiled
const _track_dependencies = Ref(false) # set this to true to track the list of file dependencies
function _include_dependency(_path::AbstractString)
    prev = source_path(nothing)
    path = (prev === nothing) ? abspath(_path) : joinpath(dirname(prev), _path)
    if myid() == 1 && _track_dependencies[]
        apath = abspath(path)
        push!(_require_dependencies, (apath, mtime(apath)))
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
    _include_dependency(path)
    return nothing
end

# We throw PrecompilableError(true) when a module wants to be precompiled but isn't,
# and PrecompilableError(false) when a module doesn't want to be precompiled but is
immutable PrecompilableError <: Exception
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
`__precompile__()` should occur before the `module` declaration in the file, or better yet
`VERSION >= v"0.4" && __precompile__()` in order to be backward-compatible with Julia 0.3.

If a module or file is *not* safely precompilable, it should call `__precompile__(false)` in
order to throw an error if Julia attempts to precompile it.

`__precompile__()` should *not* be used in a module unless all of its dependencies are also
using `__precompile__()`. Failure to do so can result in a runtime error when loading the module.
"""
function __precompile__(isprecompilable::Bool=true)
    if (myid() == 1 &&
        JLOptions().use_compilecache != 0 &&
        isprecompilable != (0 != ccall(:jl_generating_output, Cint, ())) &&
        !(isprecompilable && toplevel_load::Bool))
        throw(PrecompilableError(isprecompilable))
    end
end

function require_modname(name::AbstractString)
    # This function can be deleted when the deprecation for `require`
    # is deleted.
    # While we could also strip off the absolute path, the user may be
    # deliberately directing to a different file than what got
    # cached. So this takes a conservative approach.
    if Bool(JLOptions().use_compilecache)
        if endswith(name, ".jl")
            tmp = name[1:end-3]
            for prefix in LOAD_CACHE_PATH
                path = joinpath(prefix, tmp*".ji")
                if isfile(path)
                    return tmp
                end
            end
        end
    end
    return name
end

"""
    reload(name::AbstractString)

Force reloading of a package, even if it has been loaded before. This is intended for use
during package development as code is modified.
"""
function reload(name::AbstractString)
    if isfile(name) || contains(name,Filesystem.path_separator)
        # for reload("path/file.jl") just ask for include instead
        error("use `include` instead of `reload` to load source files")
    else
        # reload("Package") is ok
        require(Symbol(require_modname(name)))
    end
end

# require always works in Main scope and loads files from node 1
toplevel_load = true

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
    # dependency-tracking is only used for one top-level include(path),
    # and is not applied recursively to imported modules:
    old_track_dependencies = _track_dependencies[]
    _track_dependencies[] = false
    DEBUG_LOADING[] = haskey(ENV, "JULIA_DEBUG_LOADING")

    global toplevel_load
    loading = get(package_locks, mod, false)
    if loading !== false
        # load already in progress for this module
        wait(loading)
        return
    end
    package_locks[mod] = Condition()

    last = toplevel_load::Bool
    try
        toplevel_load = false
        # perform the search operation to select the module file require intends to load
        name = string(mod)
        path = find_in_node_path(name, nothing, 1)
        if path === nothing
            throw(ArgumentError("Module $name not found in current path.\nRun `Pkg.add(\"$name\")` to install the $name package."))
        end

        # attempt to load the module file via the precompile cache locations
        doneprecompile = false
        if JLOptions().use_compilecache != 0
            doneprecompile = _require_search_from_serialized(1, mod, path, last)
            if !isa(doneprecompile, Bool)
                return # success
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
            # spawn off a new incremental pre-compile task from node 1 for recursive `require` calls
            # or if the require search declared it was pre-compiled before (and therefore is expected to still be pre-compilable)
            cachefile = compilecache(mod)
            m = _require_from_serialized(1, mod, cachefile, last)
            if isa(m, Exception)
                warn("The call to compilecache failed to create a usable precompiled cache file for module $name. Got:")
                warn(m, prefix="WARNING: ")
                # fall-through, TODO: disable __precompile__(true) error so that the normal include will succeed
            else
                return # success
            end
        end

        # just load the file normally via include
        # for unknown dependencies
        try
            if last && myid() == 1 && nprocs() > 1
                # include on node 1 first to check for PrecompilableErrors
                eval(Main, :(Base.include_from_node1($path)))

                # broadcast top-level import/using from node 1 (only)
                refs = Any[ @spawnat p eval(Main, :(Base.include_from_node1($path))) for p in filter(x -> x != 1, procs()) ]
                for r in refs; wait(r); end
            else
                eval(Main, :(Base.include_from_node1($path)))
            end
        catch ex
            if doneprecompile === true || JLOptions().use_compilecache == 0 || !precompilableerror(ex, true)
                rethrow() # rethrow non-precompilable=true errors
            end
            # the file requested `__precompile__`, so try to build a cache file and use that
            cachefile = compilecache(mod)
            m = _require_from_serialized(1, mod, cachefile, last)
            if isa(m, Exception)
                warn(m, prefix="WARNING: ")
                # TODO: disable __precompile__(true) error and do normal include instead of error
                error("Module $mod declares __precompile__(true) but require failed to create a usable precompiled cache file.")
            end
        end
    finally
        toplevel_load = last
        loading = pop!(package_locks, mod)
        notify(loading, all=true)
        _track_dependencies[] = old_track_dependencies
    end
    nothing
end

# remote/parallel load

"""
    include_string(code::AbstractString, filename::AbstractString="string")

Like `include`, except reads code from the given string rather than from a file. Since there
is no file path involved, no path processing or fetching from node 1 is done.
"""
include_string(txt::String, fname::String) =
    ccall(:jl_load_file_string, Any, (Ptr{UInt8},Csize_t,Cstring),
          txt, sizeof(txt), fname)

include_string(txt::AbstractString, fname::AbstractString="string") =
    include_string(String(txt), String(fname))

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
    p === nothing ? p : dirname(p)
end

"""
    @__FILE__ -> AbstractString

`@__FILE__` expands to a string with the absolute file path of the file containing the
macro. Returns `nothing` if run from a REPL or an empty string if evaluated by
`julia -e <expr>`. Alternatively see [`PROGRAM_FILE`](@ref).
"""
macro __FILE__() source_path() end

"""
    @__DIR__ -> AbstractString

`@__DIR__` expands to a string with the directory part of the absolute path of the file
containing the macro. Returns `nothing` if run from a REPL or an empty string if
evaluated by `julia -e <expr>`.
"""
macro __DIR__() source_dir() end

include_from_node1(path::AbstractString) = include_from_node1(String(path))
function include_from_node1(_path::String)
    path, prev = _include_dependency(_path)
    tls = task_local_storage()
    tls[:SOURCE_PATH] = path
    local result
    try
        if myid()==1
            # sleep a bit to process file requests from other nodes
            nprocs()>1 && sleep(0.005)
            result = Core.include(path)
            nprocs()>1 && sleep(0.005)
        else
            result = include_string(remotecall_fetch(readstring, 1, path), path)
        end
    finally
        if prev === nothing
            delete!(tls, :SOURCE_PATH)
        else
            tls[:SOURCE_PATH] = prev
        end
    end
    result
end

"""
    include(path::AbstractString)

Evaluate the contents of the input source file in the current context. Returns the result
of the last evaluated expression of the input file. During including, a task-local include
path is set to the directory containing the file. Nested calls to `include` will search
relative to that path. All paths refer to files on node 1 when running in parallel, and
files will be fetched from node 1. This function is typically used to load source
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
                     :(eval(x) = Main.Core.eval(__anon__,x)),
                     :(eval(m,x) = Main.Core.eval(m,x)),
                     :(Main.Base.include($path))))
end
evalfile(path::AbstractString, args::Vector) = evalfile(path, String[args...])

function create_expr_cache(input::String, output::String, concrete_deps::Vector{Any})
    rm(output, force=true)   # Remove file if it exists
    code_object = """
        while !eof(STDIN)
            eval(Main, deserialize(STDIN))
        end
        """
    io, pobj = open(pipeline(detach(`$(julia_cmd()) -O0
                                    --output-ji $output --output-incremental=yes
                                    --startup-file=no --history-file=no
                                    --color=$(have_color ? "yes" : "no")
                                    --eval $code_object`), stderr=STDERR),
                    "w", STDOUT)
    try
        serialize(io, quote
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
            serialize(io, quote
                      task_local_storage()[:SOURCE_PATH] = $(source)
                      end)
        end
        serialize(io, :(Base.include($(abspath(input)))))
        if source !== nothing
            serialize(io, :(delete!(task_local_storage(), :SOURCE_PATH)))
        end
        close(io)
        wait(pobj)
        return pobj
    catch
        kill(pobj)
        close(io)
        rethrow()
    end
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
    myid() == 1 || error("can only precompile from node 1")
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
    for existing in names(Main)
        if isdefined(Main, existing)
            mod = getfield(Main, existing)
            if isa(mod, Module) && !(mod === Main || mod === Core || mod === Base)
                mod = mod::Module
                if module_parent(mod) === Main && module_name(mod) === existing
                    push!(concrete_deps, (existing, module_uuid(mod)))
                end
            end
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
    if !success(create_expr_cache(path, cachefile, concrete_deps))
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
    files = Tuple{String,Float64}[]
    while true
        n = ntoh(read(f, Int32))
        n == 0 && break
        totbytes -= 4 + n + 8
        @assert n >= 0 "EOF while reading cache header" # probably means this wasn't a valid file to be read by Base.parse_cache_header
        push!(files, (String(read(f, n)), ntoh(read(f, Float64))))
    end
    @assert totbytes == 4 "header of cache file appears to be corrupt"
    return modules, files
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
    defs, files = parse_cache_header(f)
    modules = []
    while true
        n = ntoh(read(f, Int32))
        n == 0 && break
        sym = Symbol(read(f, n)) # module symbol
        uuid = ntoh(read(f, UInt64)) # module UUID (mostly just a timestamp)
        push!(modules, (sym, uuid))
    end
    return modules, files
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

function stale_cachefile(modpath::String, cachefile::String)
    io = open(cachefile, "r")
    try
        if !isvalid_cache_header(io)
            DEBUG_LOADING[] && info("JL_DEBUG_LOADING: Rejecting cache file $cachefile due to it containing an invalid cache header.")
            return true # invalid cache file
        end
        modules, files = parse_cache_header(io)

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
        if !samefile(files[1][1], modpath)
            DEBUG_LOADING[] && info("JL_DEBUG_LOADING: Rejecting cache file $cachefile because it is for file $(files[1][1])) not file $modpath.")
            return true # cache file was compiled from a different path
        end
        for (f, ftime_req) in files
            # Issue #13606: compensate for Docker images rounding mtimes
            ftime = mtime(f)
            if ftime != ftime_req && ftime != floor(ftime_req)
                DEBUG_LOADING[] && info("JL_DEBUG_LOADING: Rejecting stale cache file $cachefile (mtime $ftime_req) because file $f (mtime $ftime) has changed.")
                return true
            end
        end
        return false # fresh cachefile
    finally
        close(io)
    end
end
