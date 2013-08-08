# require

function is_file_readable(path)
    s = stat(bytestring(path))
    return isfile(s) && isreadable(s)
end

function find_in_path(name::String)
    isabspath(name) && return name
    isfile(name) && return abspath(name)
    base = name
    if endswith(name,".jl")
        base = name[1:end-3]
    else
        name = string(base,".jl")
    end
    for prefix in [Pkg.dir(), LOAD_PATH]
        path = joinpath(prefix, name)
        is_file_readable(path) && return abspath(path)
        path = joinpath(prefix, base, "src", name)
        is_file_readable(path) && return abspath(path)
        path = joinpath(prefix, name, "src", name)
        is_file_readable(path) && return abspath(path)
    end
    return abspath(name)
end

find_in_node1_path(name) = myid()==1 ?
    find_in_path(name) : remotecall_fetch(1, find_in_path, name)

# Store list of files and their load time
package_list = (ByteString=>Float64)[]
# to synchronize multiple tasks trying to require something
package_locks = (ByteString=>Any)[]
require(fname::String) = require(bytestring(fname))
require(f::String, fs::String...) = (require(f); for x in fs require(x); end)

function require(name::ByteString)
    if myid() == 1 
        @sync for p in filter(x -> x != 1, procs())
            @spawnat p require(name)
        end
    end
    path = find_in_node1_path(name)
    if haskey(package_list,path)
        wait(package_locks[path])
    else
        reload_path(path)
    end
    nothing
end

reload(name) = reload(string(name))

function reload(name::String)
    if myid() == 1
        @sync for p in filter(x -> x != 1, procs())
            @spawnat p reload(name)
        end
    end
    reload_path(find_in_node1_path(name))
end

# remote/parallel load

include_string(txt::ByteString, fname::ByteString) =
    ccall(:jl_load_file_string, Any, (Ptr{Uint8},Ptr{Uint8}), txt, fname)

include_string(txt::ByteString) = include_string(txt, "string")

function source_path(default)
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
source_path() = source_path("")

function include_from_node1(path)
    prev = source_path(nothing)
    path = (prev == nothing) ? abspath(path) : joinpath(dirname(prev),path)
    tls = task_local_storage()
    tls[:SOURCE_PATH] = path
    local result
    try
        if myid()==1
            result = Core.include(path)
        else
            include_string(remotecall_fetch(1, readall, path), path)
            # don't bother sending last value for remote include
            result = nothing
        end
    finally
        if prev == nothing
            delete!(tls, :SOURCE_PATH)
        else
            tls[:SOURCE_PATH] = prev
        end
    end
    result
end

function reload_path(path)
    had = haskey(package_list, path)
    if !had
        package_locks[path] = RemoteRef()
    end
    package_list[path] = time()
    tls = task_local_storage()
    prev = delete!(tls, :SOURCE_PATH, nothing)
    try
        eval(Main, :(Base.include_from_node1($path)))
    catch e
        had || delete!(package_list, path)
        rethrow(e)
    finally
        if prev != nothing
            tls[:SOURCE_PATH] = prev
        end
    end
    if !isready(package_locks[path])
        put(package_locks[path],nothing)
    end
    nothing
end

function evalfile(path::String, args::Array = {})
    return eval(Module(:__anon__),
                Expr(:toplevel,:(ARGS=$args),:(include($path))))
end
