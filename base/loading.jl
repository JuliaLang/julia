# This file is a part of Julia. License is MIT: http://julialang.org/license

# Base.require is the implementation for the `import` statement

# Cross-platform case-sensitive path canonicalization

if OS_NAME ∈ (:Linux, :FreeBSD)
    # Case-sensitive filesystems, don't have to do anything
    isfile_casesensitive(path) = isfile(path)
elseif OS_NAME == :Windows
    # GetLongPathName Win32 function returns the case-preserved filename on NTFS.
    function isfile_casesensitive(path)
        isfile(path) || return false  # Fail fast
        Filesystem.longpath(path) == path
    end
elseif OS_NAME == :Darwin
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
        path_basename = bytestring(basename(path))
        local casepreserved_basename
        const header_size = 12
        buf = Array(UInt8, length(path_basename) + header_size + 1)
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
              sub(buf, (header_size+1):(header_size+filename_length-1))
            break
        end
        # Hack to compensate for inability to create a string from a subarray with no allocations.
        path_basename.data == casepreserved_basename && return true

        # If there is no match, it's possible that the file does exist but HFS+
        # performed unicode normalization. See  https://developer.apple.com/library/mac/qa/qa1235/_index.html.
        isascii(path_basename) && return false
        normalize_string(path_basename, :NFD).data == casepreserved_basename
    end
else
    # Generic fallback that performs a slow directory listing.
    function isfile_casesensitive(path)
        isfile(path) || return false
        dir, filename = splitdir(path)
        any(readdir(dir) .== filename)
    end
end

# `wd` is a working directory to search. defaults to current working directory.
# if `wd === nothing`, no extra path is searched.
function find_in_path(name::AbstractString, wd = pwd())
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
    for prefix in [Pkg.dir(); LOAD_PATH]
        path = joinpath(prefix, name)
        isfile_casesensitive(path) && return abspath(path)
        path = joinpath(prefix, base, "src", name)
        isfile_casesensitive(path) && return abspath(path)
        path = joinpath(prefix, name, "src", name)
        isfile_casesensitive(path) && return abspath(path)
    end
    return nothing
end

function find_in_node_path(name, srcpath, node::Int=1)
    if myid() == node
        find_in_path(name, srcpath)
    else
        remotecall_fetch(find_in_path, node, name, srcpath)
    end
end

function find_source_file(file)
    (isabspath(file) || isfile(file)) && return file
    file2 = find_in_path(file)
    file2 !== nothing && return file2
    file2 = joinpath(JULIA_HOME, DATAROOTDIR, "julia", "base", file)
    isfile(file2) ? file2 : nothing
end

function find_all_in_cache_path(mod::Symbol)
    name = string(mod)
    paths = AbstractString[]
    for prefix in LOAD_CACHE_PATH
        path = joinpath(prefix, name*".ji")
        if isfile_casesensitive(path)
            push!(paths, path)
        end
    end
    paths
end

function _include_from_serialized(content::Vector{UInt8})
    return ccall(:jl_restore_incremental_from_buf, Any, (Ptr{UInt8},Int), content, sizeof(content))
end

# returns an array of modules loaded, or nothing if failed
function _require_from_serialized(node::Int, mod::Symbol, path_to_try::ByteString, toplevel_load::Bool)
    if JLOptions().use_compilecache == 0
        return nothing
    end
    restored = nothing
    if toplevel_load && myid() == 1 && nprocs() > 1
        recompile_stale(mod, path_to_try)
        # broadcast top-level import/using from node 1 (only)
        if node == myid()
            content = open(readbytes, path_to_try)
        else
            content = remotecall_fetch(open, node, readbytes, path_to_try)
        end
        restored = _include_from_serialized(content)
        if restored !== nothing
            others = filter(x -> x != myid(), procs())
            refs = Any[ @spawnat p (nothing !== _include_from_serialized(content)) for p in others]
            for (id, ref) in zip(others, refs)
                if !fetch(ref)
                    warn("node state is inconsistent: node $id failed to load cache from $path_to_try")
                end
            end
        end
    elseif node == myid()
        myid() == 1 && recompile_stale(mod, path_to_try)
        restored = ccall(:jl_restore_incremental, Any, (Ptr{UInt8},), path_to_try)
    else
        content = remotecall_fetch(open, node, readbytes, path_to_try)
        restored = _include_from_serialized(content)
    end
    # otherwise, continue search

    if restored !== nothing
        for M in restored
            if isdefined(M, :__META__)
                push!(Base.Docs.modules, M)
            end
        end
    end
    return restored
end

function _require_from_serialized(node::Int, mod::Symbol, toplevel_load::Bool)
    if JLOptions().use_compilecache == 0
        return nothing
    end
    if node == myid()
        paths = find_all_in_cache_path(mod)
    else
        paths = @fetchfrom node find_all_in_cache_path(mod)
    end
    sort!(paths, by=mtime, rev=true) # try newest cachefiles first
    for path_to_try in paths
        restored = _require_from_serialized(node, mod, path_to_try, toplevel_load)
        if restored === nothing
            warn("deserialization checks failed while attempting to load cache from $path_to_try")
        else
            return restored
        end
    end
    return nothing
end

# to synchronize multiple tasks trying to import/using something
const package_locks = Dict{Symbol,Condition}()

# used to optionally track dependencies when requiring a module:
const _require_dependencies = Tuple{ByteString,Float64}[]
const _track_dependencies = [false]
function _include_dependency(_path::AbstractString)
    prev = source_path(nothing)
    path = (prev === nothing) ? abspath(_path) : joinpath(dirname(prev),_path)
    if myid() == 1 && _track_dependencies[1]
        apath = abspath(path)
        push!(_require_dependencies, (apath, mtime(apath)))
    end
    return path, prev
end
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
        print(io, "__precompile__(true) is only allowed in module files being imported")
    else
        print(io, "__precompile__(false) is not allowed in files that are being precompiled")
    end
end
precompilableerror(ex::PrecompilableError, c) = ex.isprecompilable == c
precompilableerror(ex::WrappedException, c) = precompilableerror(ex.error, c)
precompilableerror(ex, c) = false

# Call __precompile__ at the top of a file to force it to be precompiled (true), or
# to be prevent it from being precompiled (false).  __precompile__(true) is
# ignored except within "require" call.
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
    if isfile(name) || contains(name,path_separator)
        # for reload("path/file.jl") just ask for include instead
        error("use `include` instead of `reload` to load source files")
    else
        # reload("Package") is ok
        require(symbol(require_modname(name)))
    end
end

# require always works in Main scope and loads files from node 1
toplevel_load = true
function require(mod::Symbol)
    # dependency-tracking is only used for one top-level include(path),
    # and is not applied recursively to imported modules:
    old_track_dependencies = _track_dependencies[1]
    _track_dependencies[1] = false

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
        if nothing !== _require_from_serialized(1, mod, last)
            return
        end
        if JLOptions().incremental != 0
            # spawn off a new incremental precompile task from node 1 for recursive `require` calls
            cachefile = compilecache(mod)
            if nothing === _require_from_serialized(1, mod, cachefile, last)
                warn("require failed to create a precompiled cache file")
            end
            return
        end
        name = string(mod)
        path = find_in_node_path(name, nothing, 1)
        if path === nothing
            throw(ArgumentError("$name not found in path.\nRun Pkg.add(\"$name\") to install the $name package"))
        end
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
            if !precompilableerror(ex, true)
                rethrow() # rethrow non-precompilable=true errors
            end
            isinteractive() && info("Precompiling module $mod...")
            cachefile = compilecache(mod)
            if nothing === _require_from_serialized(1, mod, cachefile, last)
                error("__precompile__(true) but require failed to create a precompiled cache file")
            end
        end
    finally
        toplevel_load = last
        loading = pop!(package_locks, mod)
        notify(loading, all=true)
        _track_dependencies[1] = old_track_dependencies
    end
    nothing
end

# remote/parallel load

include_string(txt::ByteString, fname::ByteString) =
    ccall(:jl_load_file_string, Any, (Ptr{UInt8},Csize_t,Ptr{UInt8},Csize_t),
          txt, sizeof(txt), fname, sizeof(fname))

include_string(txt::AbstractString, fname::AbstractString="string") =
    include_string(bytestring(txt), bytestring(fname))

function source_path(default::Union{AbstractString,Void}="")
    t = current_task()
    while true
        s = t.storage
        if !is(s, nothing) && haskey(s, :SOURCE_PATH)
            return s[:SOURCE_PATH]
        end
        if is(t, t.parent)
            return default
        end
        t = t.parent
    end
end

function source_dir()
    p = source_path(nothing)
    p === nothing ? p : dirname(p)
end

macro __FILE__() source_path() end

function include_from_node1(_path::AbstractString)
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
            result = include_string(remotecall_fetch(readall, 1, path), path)
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

function evalfile(path::AbstractString, args::Vector{UTF8String}=UTF8String[])
    return eval(Module(:__anon__),
                Expr(:toplevel,
                     :(const ARGS = $args),
                     :(eval(x) = Main.Core.eval(__anon__,x)),
                     :(eval(m,x) = Main.Core.eval(m,x)),
                     :(Main.Base.include($path))))
end
evalfile(path::AbstractString, args::Vector) = evalfile(path, UTF8String[args...])

function create_expr_cache(input::AbstractString, output::AbstractString)
    isfile(output) && rm(output)
    code_object = """
        while !eof(STDIN)
            eval(Main, deserialize(STDIN))
        end
        """
    io, pobj = open(pipeline(detach(`$(julia_cmd())
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
                  end)
        source = source_path(nothing)
        if source !== nothing
            serialize(io, quote
                      task_local_storage()[:SOURCE_PATH] = $(source)
                      end)
        end
        serialize(io, :(Base._track_dependencies[1] = true))
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
function compilecache(name::ByteString)
    myid() == 1 || error("can only precompile from node 1")
    path = find_in_path(name, nothing)
    path === nothing && throw(ArgumentError("$name not found in path"))
    cachepath = LOAD_CACHE_PATH[1]
    if !isdir(cachepath)
        mkpath(cachepath)
    end
    cachefile = abspath(cachepath, name*".ji")
    if !success(create_expr_cache(path, cachefile))
        error("Failed to precompile $name to $cachefile")
    end
    return cachefile
end

module_uuid(m::Module) = ccall(:jl_module_uuid, UInt64, (Any,), m)

isvalid_cache_header(f::IOStream) = 0 != ccall(:jl_deserialize_verify_header, Cint, (Ptr{Void},), f.ios)

function cache_dependencies(f::IO)
    modules = Tuple{Symbol,UInt64}[]
    files = Tuple{ByteString,Float64}[]
    while true
        n = ntoh(read(f, Int32))
        n == 0 && break
        push!(modules,
              (symbol(readbytes(f, n)), # module symbol
               ntoh(read(f, UInt64)))) # module UUID (timestamp)
    end
    read(f, Int64) # total bytes for file dependencies
    while true
        n = ntoh(read(f, Int32))
        n == 0 && break
        push!(files, (bytestring(readbytes(f, n)), ntoh(read(f, Float64))))
    end
    return modules, files
end

function cache_dependencies(cachefile::AbstractString)
    io = open(cachefile, "r")
    try
        !isvalid_cache_header(io) && throw(ArgumentError("invalid cache file $cachefile"))
        return cache_dependencies(io)
    finally
        close(io)
    end
end

function stale_cachefile(modpath, cachefile)
    io = open(cachefile, "r")
    try
        if !isvalid_cache_header(io)
            return true # invalid cache file
        end
        modules, files = cache_dependencies(io)
        if files[1][1] != modpath
            return true # cache file was compiled from a different path
        end
        for (f,ftime) in files
            # Issue #13606: compensate for Docker images rounding mtimes
            if mtime(f) ∉ (ftime, floor(ftime))
                return true
            end
        end
        # files are not stale, so module list is valid and needs checking
        for (M,uuid) in modules
            if !isdefined(Main, M)
                require(M) # should recursively recompile module M if stale
            end
            if module_uuid(Main.(M)) != uuid
                return true
            end
        end
        return false # fresh cachefile
    finally
        close(io)
    end
end

function recompile_stale(mod, cachefile)
    path = find_in_path(string(mod), nothing)
    if path === nothing
        rm(cachefile)
        error("module $mod not found in current path; removed orphaned cache file $cachefile")
    end
    if stale_cachefile(path, cachefile)
        info("Recompiling stale cache file $cachefile for module $mod.")
        if !success(create_expr_cache(path, cachefile))
            error("Failed to precompile $mod to $cachefile")
        end
    end
end
