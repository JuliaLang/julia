# This file is a part of Julia. License is MIT: http://julialang.org/license

# Base.require is the implementation for the `import` statement

function find_in_path(name::AbstractString)
    isabspath(name) && return name
    isfile(name) && return abspath(name)
    base = name
    if endswith(name,".jl")
        base = name[1:end-3]
    else
        name = string(base,".jl")
        isfile(name) && return abspath(name)
    end
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

find_in_node_path(name, node::Int=1) = myid() == node ?
    find_in_path(name) : remotecall_fetch(node, find_in_path, name)

function find_source_file(file)
    (isabspath(file) || isfile(file)) && return file
    file2 = find_in_path(file)
    file2 != nothing && return file2
    file2 = joinpath(JULIA_HOME, DATAROOTDIR, "julia", "base", file)
    isfile(file2) ? file2 : nothing
end

function find_in_cache_path(mod::Symbol)
    name = string(mod)
    for prefix in LOAD_CACHE_PATH
        path = joinpath(prefix, name*".ji")
        if isfile(path)
            produce(path)
        end
    end
    nothing
end

function _include_from_serialized(content::Vector{UInt8})
    m = ccall(:jl_restore_incremental_from_buf, UInt, (Ptr{Uint8},Int), content, sizeof(content))
    return m != 0
end

function _require_from_serialized(node::Int, path_to_try::ByteString)
    if myid() == 1 && current_module() === Main && nprocs() > 1
        # broadcast top-level import/using from node 1 (only)
        if node == myid()
            content = open(readbytes, path_to_try)
        else
            content = remotecall_fetch(node, open, readbytes, path_to_try)
        end
        if _include_from_serialized(content)
            others = filter(x -> x != myid(), procs())
            refs = Any[ @spawnat p _include_from_serialized(content) for p in others]
            successes = Any[ fetch(ref) for ref in refs ]
            for (id, ref) in zip(others, refs)
                if fetch(ref)
                    warn("node state is inconsistent: node $id failed to load serialized cache from :node $node:$path_to_try.")
                end
            end
            return true
        end
    elseif node == myid()
        if ccall(:jl_restore_incremental, UInt, (Ptr{Uint8},), path_to_try) != 0
            return true
        end
    else
        content = remotecall_fetch(node, open, readbytes, path_to_try)
        if _include_from_serialized(content)
            return true
        end
    end
    # otherwise, continue search
    return false
end

function _require_from_serialized(node::Int, mod::Symbol)
    name = string(mod)
    finder = @spawnat node @task find_in_cache_path(mod) # TODO: switch this to an explicit Channel
    while true
        path_to_try = remotecall_fetch(node, finder->consume(fetch(finder)), finder)
        path_to_try === nothing && return false
        if _require_from_serialized(node, path_to_try)
            return true
        end
    end
end

# to synchronize multiple tasks trying to import/using something
package_locks = Dict{Symbol,Condition}()

# setting cachecompile=true will force a compile of a cache file
# require always works in Main scope and loads files from node 1
function require(mod::Symbol, cachecompile::Bool=false)
    loading = get(package_locks, mod, false)
    if loading !== false
        # load already in progress for this module
        wait(loading)
        return
    end
    package_locks[mod] = Condition()

    try
        if !cachecompile && _require_from_serialized(1, mod)
            return
        end

        name = string(mod)
        path = find_in_node_path(name, 1)
        path === nothing && throw(ArgumentError("$name not found in path"))

        if cachecompile || JLOptions().incremental != 0
            # spawn off a new incremental compile task from node 1 for recursive `require` calls
            cachefile = remotecall_fetch(1, create_expr_cache, path, name)
            if !_require_from_serialized(1, cachefile)
                error("Warning: require failed to create a precompiled cache file")
            end
            return
        end

        if myid() == 1 && current_module() === Main && nprocs() > 1
            # broadcast top-level import/using from node 1 (only)
            content = open(readall, path)
            refs = Any[ @spawnat p eval(Main, :(Base.base_include($content, $path, (1, "")))) for p in procs() ]
            for r in refs; wait(r); end
        else
            eval(Main, :(Base.base_include($path, (1, ""))))
        end
    finally
        loading = pop!(package_locks, mod)
        notify(loading, all=true)
    end
    nothing
end
const reload = require

# remote/parallel load

function source_path(default::Union{AbstractString,Void}=nothing)
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

macro __FILE__()
    sp = source_path()
    if !isa(sp, Tuple{Int,Any})
        return ":unknown:\0$sp"
    elseif sp[1] === 0
        return ":string:\0$(sp[2])"
    elseif sp[1] !== myid()
        return ":node $(sp[1]):\0$(sp[2])"
    else
        return sp[2]
    end
end

# base_include is the implementation for Base.include
# if relative_to::Void, it uses the working directory and node of the previous (recursive) include to find the file
# relative_to is a tuple of (node, cwd), where node can be 0 iff the content is from a string (sans a real path)
function base_include(content::Nullable{ByteString}, path::AbstractString, relative_to=nothing)
    prev = source_path(nothing)
    local bytepath::ByteString, node::Int
    if !isnull(content)
        # already have content, so no need for file-system operations
        if relative_to === nothing
            node = 0
            bytepath = bytestring(path)
        else
            node = relative_to[1]
            bytepath = bytestring(joinpath(relative_to[2],path))
        end
    else
        if prev !== nothing && relative_to === nothing
            # if this is a recursive include without explicit relative_to location
            # pick up previous location for last include
            relative_to = (prev[1], dirname(prev[2]))
        end
        if relative_to === nothing || relative_to[1] === 0
            # no previous path, pick up location from host
            node = myid()
            bytepath = bytestring(abspath(path))
        else
            node = relative_to[1]
            bytepath = bytestring(joinpath(relative_to[2],path))
        end
    end

    tls = task_local_storage()
    tls[:SOURCE_PATH] = (node, bytepath)
    try
        if isnull(content)
            if myid() === node
                # sleep a bit to process requests from other nodes
                nprocs()>1 && yield()
                result = Core.include(bytepath)
                nprocs()>1 && yield()
                return result
            else
                content = Nullable{ByteString}(remotecall_fetch(node, readall, bytepath))
                # fall-through now that content is assigned
            end
        end
        txt = get(content)
        return ccall(:jl_load_file_string, Any, (Ptr{UInt8},Csize_t,Ptr{UInt8},Csize_t),
                     txt, sizeof(txt), bytepath, sizeof(bytepath))
    finally
        if prev === nothing
            delete!(tls, :SOURCE_PATH)
        else
            tls[:SOURCE_PATH] = prev
        end
    end
end
base_include(content::AbstractString, path::AbstractString, relative_to=nothing) = base_include(Nullable{ByteString}(content), path, relative_to)
base_include(path::AbstractString, relative_to=nothing) = base_include(Nullable{ByteString}(), path, relative_to)
include_string(txt::AbstractString, fname::AbstractString="none") = base_include(Nullable{ByteString}(bytestring(txt)), bytestring(fname))

function evalfile(path::AbstractString, args::Vector{UTF8String}=UTF8String[])
    return eval(Module(:__anon__),
                Expr(:toplevel,
                     :(const ARGS = $args),
                     :(eval(x) = Main.Core.eval(__anon__,x)),
                     :(eval(m,x) = Main.Core.eval(m,x)),
                     :(Main.Base.include($path))))
end
evalfile(path::AbstractString, args::Vector) = evalfile(path, UTF8String[args...])

function create_expr_cache(m::Expr, name)
    cachepath = LOAD_CACHE_PATH[1]
    if !isdir(cachepath)
        mkdir(cachepath)
    end
    cachefile = abspath(cachepath, name*".ji")
    code_object = """
        while !eof(STDIN)
            eval(Main, deserialize(STDIN))
        end
        """
    io, pobj = open(detach(setenv(`$(julia_cmd())
            --output-ji $cachefile --output-incremental=yes
            --startup-file=no --history-file=no
            --eval $code_object`,
        ["JULIA_HOME=$JULIA_HOME", "HOME=$(homedir())"])), "w")
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
    serialize(io, m)
    if source !== nothing
        serialize(io, quote
            delete!(task_local_storage(), :SOURCE_PATH)
        end)
    end
    close(io)
    wait(pobj)
    return cachefile
end

function create_expr_cache(file::AbstractString, name)
    return create_expr_cache(:(Base.include($(abspath(file)))), name)
end
