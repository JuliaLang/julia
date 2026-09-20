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

# test/threads.jl kills this run after a fixed timeout, so leave evidence of
# what stalled behind. The sample count is the profiler's own progress: the
# Windows sampler suspends a thread and then unwinds it, and if that deadlocks
# (the suspended thread holds a lock the unwinder needs) the count freezes at
# the moment of the hang, while a scheduler-side hang leaves it climbing until
# the buffer is full. A healthy run finishes inside the first interval and
# prints nothing.
const REPORT_INTERVAL_S = 30

t0 = time_ns()
watchdog = Timer(REPORT_INTERVAL_S; interval = REPORT_INTERVAL_S) do _
    elapsed = round((time_ns() - t0) / 1e9, digits = 1)
    println(stderr, "  spawnmany still running after $(elapsed)s: ",
            "$(Profile.len_data()) of $(Profile.maxlen_data()) profile slots used")
end

try
    @profile spawnmany(parse(Int, get(ENV, "NTASKS", "2000000")))
finally
    close(watchdog)
end
