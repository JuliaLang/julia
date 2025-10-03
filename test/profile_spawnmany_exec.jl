# This file is a part of Julia. License is MIT: https://julialang.org/license

using Profile

function spawnmany(n)
    if n > 2
        m = n ÷ 2
        t = Threads.@spawn spawnmany(m)
        spawnmany(m)
        wait(t)
    end
end

@profile spawnmany(parse(Int, get(ENV, "NTASKS", "2000000")))
