# This file is a part of Julia. License is MIT: https://julialang.org/license

# Drives the coverage counters from several threads at once (#59355).

function hot(n)
    s = 0
    for i in 1:n
        s += i
    end
    return s
end

function spawn_all(n)
    tasks = [Threads.@spawn hot(n) for _ in 1:(4 * Threads.nthreads())]
    return all(t -> fetch(t) == hot(n), tasks)
end

exit(spawn_all(1000) ? 0 : 1)
