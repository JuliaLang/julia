# This file is a part of Julia. License is MIT: http://julialang.org/license

# require

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

find_in_node1_path(name) = myid()==1 ?
    find_in_path(name) : remotecall_fetch(1, find_in_path, name)

function find_source_file(file)
    (isabspath(file) || isfile(file)) && return file
    file2 = find_in_path(file)
    file2 != nothing && return file2
    file2 = joinpath(JULIA_HOME, DATAROOTDIR, "julia", "base", file)
    isfile(file2) ? file2 : nothing
end

# Store list of files and their load time
package_list = Dict{ByteString,Float64}()
# to synchronize multiple tasks trying to require something
package_locks = Dict{ByteString,Any}()
# List of files associated with each module
package_files = Dict{Symbol,Set{ByteString}}()
current_package = Array(Symbol, 0)
require(fname::AbstractString) = require(bytestring(fname))
require(f::AbstractString, fs::AbstractString...) = (require(f); for x in fs require(x); end)

# only broadcast top-level (not nested) requires and reloads
toplevel_load = true

function _require_from_serialized(content)
    m = ccall(:jl_restore_new_module_from_buf, UInt, (Ptr{Uint8},Int), content, sizeof(content))
    #package_list[path] = time()
    return m
end

function _require_from_cache(name::AbstractString)
    for prefix in LOAD_CACHE_PATH
        path = joinpath(prefix, name*".ji")
        if isfile(path)
            mt = mtime(path)
            if nprocs() == 1
                if ccall(:jl_restore_new_module, UInt, (Ptr{Uint8},), path) != 0
                    @show symbol(basename(name))
                    #package_list[path] = time()
                    try
                        module_files = eval(symbol(basename(name))).__module_files__
                        @show module_files
                    end
                    return true
#                        for f in module_files
#                            if mtime(f) > mt
#                                return false
#                            end
#                        end
#                        return true
#                    end
#                    return false
                end
            else
                content = open(readbytes, path)
                if _require_from_serialized(content) != 0
                    refs = Any[ @spawnat p _require_from_serialized(content) for p in filter(x->x!=1, procs()) ]
                    for r in refs; wait(r); end
                    return true
                end
            end
        end
    end
    return false
end

function require(sname::Symbol)
    name = string(sname)
    if !_require_from_cache(name)
        require(name)
    end
end

function require(name::AbstractString)
    path = find_in_node1_path(name)
    path == nothing && throw(ArgumentError("$name not found in path"))
    push!(current_package, symbol(name))

    if myid() == 1 && toplevel_load
        refs = Any[ @spawnat p _require(path) for p in filter(x->x!=1, procs()) ]
        _require(path)
        for r in refs; wait(r); end
    else
        _require(path)
    end
    pop!(current_package)
    nothing
end

function _require(path)
    global toplevel_load
    if haskey(package_list,path)
        loaded, c = package_locks[path]
        !loaded && wait(c)
    else
        last = toplevel_load
        toplevel_load = false
        try
            reload_path(path)
        finally
            toplevel_load = last
        end
    end
end

function reload(name::AbstractString)
    global toplevel_load
    path = find_in_node1_path(name)
    path == nothing && throw(ArgumentError("$name not found in path"))
    refs = nothing
    if myid() == 1 && toplevel_load
        refs = Any[ @spawnat p reload_path(path) for p in filter(x->x!=1, procs()) ]
    end
    last = toplevel_load
    toplevel_load = false
    try
        reload_path(path)
    finally
        toplevel_load = last
    end
    if refs !== nothing
        for r in refs; wait(r); end
    end
    nothing
end

# remote/parallel load

include_string(txt::ByteString, fname::ByteString) =
    ccall(:jl_load_file_string, Any, (Ptr{UInt8},Csize_t,Ptr{UInt8},Csize_t),
          txt, sizeof(txt), fname, sizeof(fname))

include_string(txt::AbstractString, fname::AbstractString) = include_string(bytestring(txt), bytestring(fname))

include_string(txt::AbstractString) = include_string(txt, "string")

function source_path(default::Union(AbstractString,Void)="")
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

macro __FILE__() source_path() end

function include_from_node1(path::AbstractString)
    prev = source_path(nothing)
    path = (prev == nothing) ? abspath(path) : joinpath(dirname(prev),path)
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
        if prev == nothing
            delete!(tls, :SOURCE_PATH)
        else
            tls[:SOURCE_PATH] = prev
        end
    end
    if !isempty(Base.current_package)
        mod = Base.current_package[end]
        if !haskey(Base.package_files, mod)
            Base.package_files[mod] = Set{ByteString}()
        end
        push!(Base.package_files[mod], path)
    end
    result
end

function reload_path(path::AbstractString)
    had = haskey(package_list, path)
    if !had
        package_locks[path] = (false, Condition())
    end
    package_list[path] = time()
    tls = task_local_storage()
    prev = pop!(tls, :SOURCE_PATH, nothing)
    try
        eval(Main, :(Base.include_from_node1($path)))
    catch e
        had || delete!(package_list, path)
        rethrow(e)
    finally
        if prev !== nothing
            tls[:SOURCE_PATH] = prev
        end
    end
    reloaded, c = package_locks[path]
    if !reloaded
        package_locks[path] = (true, c)
        notify(c, all=true)
    end
    nothing
end

function evalfile(path::AbstractString, args::Vector{UTF8String}=UTF8String[])
    return eval(Module(:__anon__),
                Expr(:toplevel,
                     :(const ARGS = $args),
                     :(eval(x) = Core.eval(__anon__,x)),
                     :(eval(m,x) = Core.eval(m,x)),
                     :(include($path))))
end
evalfile(path::AbstractString, args::Vector) = evalfile(path, UTF8String[args...])

create_expr_cache(m::Box) = create_expr_cache(m.contents::Expr)
function create_expr_cache(m::Expr)
    assert(m.head === :module)
    modname = m.args[2]
    typeassert(modname, Symbol)
    cachepath = LOAD_CACHE_PATH[1]
    if !isdir(cachepath)
        mkdir(cachepath; recursive=true)
    end
    code_object = """
        while !eof(STDIN)
            eval(Main, deserialize(STDIN))
        end
        """
    io, pobj = open(detach(setenv(`$(julia_cmd()) --build $cachepath --startup-file=no --history-file=no -E $code_object`,["JULIA_HOME=$JULIA_HOME","HOME=$(homedir())"])), "w")
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
    Base.package_files[modname] = Set{ByteString}()
    serialize(io, m)
    if source !== nothing
        serialize(io, quote
            delete!(task_local_storage(), :SOURCE_PATH)
        end)
    end
    pkg_files = Base.package_files[modname]
    serialize(io, quote
        eval(eval($modname), :(__module_files__ = Base.package_files[$$modname]))
    end)
    close(io)
    wait(pobj)
    if !_require_from_cache(string(modname))
        error("Warning: @cacheable module $modname failed to define a module")
    end
end

macro cacheable(m)
    :(create_expr_cache($(Box(m))))
end
