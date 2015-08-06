# This file is a part of Julia. License is MIT: http://julialang.org/license

# Base.require is the implementation for the `import` statement

# `wd` is a working directory to search. from the top level (e.g the prompt)
# it's just the cwd, otherwise it will be set to source_dir().
function find_in_path(name::AbstractString, wd = pwd())
    isabspath(name) && return name
    if wd === nothing; wd = pwd(); end
    base = name
    if endswith(name,".jl")
        base = name[1:end-3]
    else
        name = string(base,".jl")
    end
    isfile(joinpath(wd,name)) && return joinpath(wd,name)
    for prefix in [Pkg.dir(); LOAD_PATH]
        path = joinpath(prefix, name)
        isfile(path) && return abspath(path)
        path = joinpath(prefix, base, "src", name)
        isfile(path) && return abspath(path)
        path = joinpath(prefix, name, "src", name)
        isfile(path) && return abspath(path)
    end
    return nothing
end

function find_in_node_path(name, srcpath, node::Int=1)
    if myid() == node
        find_in_path(name, srcpath)
    else
        remotecall_fetch(node, find_in_path, name, srcpath)
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
    paths = String[]
    for prefix in LOAD_CACHE_PATH
        path = joinpath(prefix, name*".ji")
        if isfile(path)
            push!(paths, path)
        end
    end
    paths
end

function _include_from_serialized(content::Vector{UInt8})
    return ccall(:jl_restore_incremental_from_buf, Any, (Ptr{Uint8},Int), content, sizeof(content))
end

# returns an array of modules loaded, or nothing if failed
function _require_from_serialized(node::Int, mod::Symbol, path_to_try::ByteString, toplevel_load::Bool)
    restored = nothing
    if toplevel_load && myid() == 1 && nprocs() > 1
        recompile_stale(mod, path_to_try)
        # broadcast top-level import/using from node 1 (only)
        if node == myid()
            content = open(readbytes, path_to_try)
        else
            content = remotecall_fetch(node, open, readbytes, path_to_try)
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
        restored = ccall(:jl_restore_incremental, Any, (Ptr{Uint8},), path_to_try)
    else
        content = remotecall_fetch(node, open, readbytes, path_to_try)
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
    paths = @fetchfrom node find_all_in_cache_path(mod)
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
const package_loaded = Set{Symbol}()

# used to optionally track dependencies when requiring a module:
const _require_dependencies = ByteString[]
const _track_dependencies = [false]
function _include_dependency(_path::AbstractString)
    prev = source_path(nothing)
    path = (prev === nothing) ? abspath(_path) : joinpath(dirname(prev),_path)
    if _track_dependencies[1]
        push!(_require_dependencies, abspath(path))
    end
    return path, prev
end
function include_dependency(path::AbstractString)
    _include_dependency(path)
    return nothing
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
            # spawn off a new incremental compile task from node 1 for recursive `require` calls
            cachefile = compile(mod)
            if nothing === _require_from_serialized(1, mod, cachefile, last)
                warn("require failed to create a precompiled cache file")
            end
            return
        end

        name = string(mod)
        path = find_in_node_path(name, source_dir(), 1)
        path === nothing && throw(ArgumentError("$name not found in path"))
        if last && myid() == 1 && nprocs() > 1
            # broadcast top-level import/using from node 1 (only)
            content = open(readall, path)
            refs = Any[ @spawnat p eval(Main, :(Base.include_from_node1($path))) for p in procs() ]
            for r in refs; wait(r); end
        else
            eval(Main, :(Base.include_from_node1($path)))
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

include_string(txt::AbstractString, fname::AbstractString) = include_string(bytestring(txt), bytestring(fname))

include_string(txt::AbstractString) = include_string(txt, "string")

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
            result = include_string(remotecall_fetch(1, readall, path), path)
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
    code_object = """
        while !eof(STDIN)
            eval(Main, deserialize(STDIN))
        end
        """
    io, pobj = open(detach(`$(julia_cmd())
            --output-ji $output --output-incremental=yes
            --startup-file=no --history-file=no
            --eval $code_object`), "w", STDOUT)
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
        serialize(io, quote
            delete!(task_local_storage(), :SOURCE_PATH)
        end)
    end
    close(io)
    wait(pobj)
    return pobj
end

compile(mod::Symbol) = compile(string(mod))
function compile(name::ByteString)
    myid() == 1 || error("can only compile from node 1")
    path = find_in_path(name)
    path === nothing && throw(ArgumentError("$name not found in path"))
    cachepath = LOAD_CACHE_PATH[1]
    if !isdir(cachepath)
        mkpath(cachepath)
    end
    cachefile = abspath(cachepath, name*".ji")
    create_expr_cache(path, cachefile)
    return cachefile
end

module_uuid(m::Module) = ccall(:jl_module_uuid, UInt64, (Any,), m)

isvalid_cache_header(f::IOStream) = 0 != ccall(:jl_deserialize_verify_header, Cint, (Ptr{Void},), f.ios)

function cache_dependencies(f::IO)
    modules = Tuple{Symbol,UInt64}[]
    files = ByteString[]
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
        push!(files, bytestring(readbytes(f, n)))
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

function stale_cachefile(cachefile::AbstractString, cachefile_mtime::Real=mtime(cachefile))
    io = open(cachefile, "r")
    try
        if !isvalid_cache_header(io)
            return true # invalid cache file
        end
        modules, files = cache_dependencies(io)
        for f in files
            if mtime(f) > cachefile_mtime
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
    cachestat = stat(cachefile)
    if iswritable(cachestat) && stale_cachefile(cachefile, cachestat.mtime)
        info("Recompiling stale cache file $cachefile for module $mod.")
        create_expr_cache(find_in_path(string(mod)), cachefile)
    end
end
