let user_data_dir
    ENV["PATH"] = JULIA_HOME*";"*joinpath(JULIA_HOME,"..","Git","bin")*";"*ENV["PATH"]
    haskey(ENV,"JULIA_EDITOR") || (ENV["JULIA_EDITOR"] = "start")
    user_data_dir = abspath(ENV["AppData"],"julia")
    isdir(user_data_dir) || mkdir(user_data_dir)
    ispty(s::Base.AsyncStream) = bool(ccall(:jl_ispty, Cint, (Ptr{Void},), s))
    ispty(s) = false
    if ispty(STDIN)
        function Base.Terminals.raw!(t::Base.Terminals.TTYTerminal,raw::Bool)
            if raw
                run(`stty raw -echo onlcr -ocrnl opost`,t.in_stream,t.out_stream,t.err_stream)
            else
                run(`stty sane`,t.in_stream,t.out_stream,t.err_stream)
            end
            true
        end
    end
end
