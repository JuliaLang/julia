using Logging

function test_threads_exec(n)
    Threads.@threads for i in 1:n
        x = rand()
        @debug "iteration" i x Threads.threadid()
        @info "iteration" i x Threads.threadid()
        @warn "iteration" i x Threads.threadid()
        @error "iteration" i x Threads.threadid()
    end
end

n = parse(Int, ARGS[1])
test_threads_exec(n)
