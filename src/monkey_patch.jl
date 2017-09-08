function Base.create_expr_cache(input::String, output::String, concrete_deps::Vector{Any})
    rm(output, force=true)   # Remove file if it exists
    _, sockserver = listenany(ip"127.0.0.1", 0)
    _, port = getsockname(sockserver) # port is wrong on 0.6
    code_object = """
        while !eof(STDIN)
            eval(Main, deserialize(STDIN))
        end
        """
    cmd = `$(Base.julia_cmd()) -O0
            --output-ji $output --output-incremental=yes
            --startup-file=no --history-file=no
            --color=$(Base.have_color ? "yes" : "no")
            --eval $code_object`
    in, io = open(pipeline(detach(cmd), stderr=STDERR), "w", STDOUT)
    try
        serialize(in, quote
            empty!(Base.LOAD_PATH)
            let pathserver = connect(ip"127.0.0.1", $port)
                eval(Main, quote
                    function Base.load_hook(::Void, name::String, ::Any)
                        serialize($pathserver, name)
                        p = deserialize($pathserver)
                        isa(p, Exception) && throw(p)
                        return p
                    end
                end)
            end
            push!(Base.LOAD_PATH, nothing)
            empty!(Base.LOAD_CACHE_PATH)
            append!(Base.LOAD_CACHE_PATH, $(Base.LOAD_CACHE_PATH))
            empty!(Base.DL_LOAD_PATH)
            append!(Base.DL_LOAD_PATH, $(Base.DL_LOAD_PATH))
            empty!(Base._concrete_dependencies)
            append!(Base._concrete_dependencies, $concrete_deps)
            Base._track_dependencies[] = true
            Base.include($(@__FILE__))
        end)
        let pathclient = accept(sockserver)
            @async try
                while isopen(pathclient)
                    let r, p = deserialize(pathclient)::String
                        try
                            r = Base.find_in_path(p, nothing)
                        catch ex
                            r = ErrorException(sprint(Base.showerror, ex, catch_backtrace()))
                        end
                        serialize(pathclient, r)
                    end
                end
            catch ex
                if isopen(pathclient)
                    close(pathclient)
                    rethrow(ex)
                end
            end
        end
        close(sockserver)
        source = Base.source_path(nothing)
        if source !== nothing
            serialize(in, quote
                      task_local_storage()[:SOURCE_PATH] = $(source)
                  end)
        end
        serialize(in, :(Base.include($(abspath(input)))))
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
