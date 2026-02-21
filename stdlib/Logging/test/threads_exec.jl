using Logging

function test_threads_exec(n)
    Threads.@threads for i in 1:n
        @debug "iteration" maxlog=1 _id=Symbol("$(i)_debug") i Threads.threadid()
        @info "iteration" maxlog=1 _id=Symbol("$(i)_info") i Threads.threadid()
        @warn "iteration" maxlog=1 _id=Symbol("$(i)_warn") i Threads.threadid()
        @error "iteration" maxlog=1 _id=Symbol("$(i)_error") i Threads.threadid()
    end
end

n = parse(Int, ARGS[1])
test_threads_exec(n)
