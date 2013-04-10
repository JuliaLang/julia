# require

function is_file_readable(path)
    s = stat(bytestring(path))
    return isfile(s) && isreadable(s)
end

function find_in_path(name::String)
    name[1] == '/' && return abspath(name)
    isfile(name) && return abspath(name)
    base = name
    if endswith(name,".jl")
        base = match(r"^(.*)\.jl$",name).captures[1]
    else
        name = string(base,".jl")
    end
    for prefix in LOAD_PATH
        path = string(prefix,"/",base,"/src/",name)
        is_file_readable(path) && return abspath(path)
        path = string(prefix,"/",name)
        is_file_readable(path) && return abspath(path)
    end
    return abspath(name)
end

find_in_node1_path(name) = myid()==1 ?
    find_in_path(name) : remote_call_fetch(1, find_in_path, name)

# Store list of files and their load time
package_list = (ByteString=>Float64)[]
require(fname::String) = require(bytestring(fname))
require(f::String, fs::String...) = (require(f); for x in fs require(x); end)

function require(name::ByteString)
    if myid() == 1
        @sync for p = 2:nprocs()
            @spawnat p require(name)
        end
    end
    path = find_in_node1_path(name)
    has(package_list,path) || reload_path(path)
end

function reload(name::String)
    if myid() == 1
        @sync for p = 2:nprocs()
            @spawnat p reload(name)
        end
    end
    reload_path(find_in_node1_path(name))
end

# remote/parallel load

include_string(txt::ByteString, fname::ByteString) =
    ccall(:jl_load_file_string, Void, (Ptr{Uint8},Ptr{Uint8}), txt, fname)

include_string(txt::ByteString) = include_string(txt, "string")

function source_path(default)
    t = current_task()
    while true
        s = t.storage
        if !is(s, nothing) && has(s, :SOURCE_PATH)
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
    try
        if myid()==1
            Core.include(path)
        else
            include_string(remote_call_fetch(1, readall, path), path)
        end
    finally
        if prev == nothing
            delete!(tls, :SOURCE_PATH)
        else
            tls[:SOURCE_PATH] = prev
        end
    end
    nothing
end

function reload_path(path)
    had = has(package_list, path)
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
    nothing
end

function evalfile(path::String)
    s = readall(path)
    m = Module(:__anon__)
    body = Expr(:toplevel)
    i = 1
    while !done(s,i)
        ex, i = parse(s,i,true,false)
        if isa(ex,Expr) && ex.head === :error
            if ex.args[1] == "end of input"
                break
            else
                throw(ParseError(ex.args[1]))
            end
        end
        push!(body.args, ex)
    end
    return eval(m, body)
end
