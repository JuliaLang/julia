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

if haskey(ENV, "SPAWN_TRIAL")
    stat = split(read("/proc/self/stat", String)[findlast(')', read("/proc/self/stat", String))+2:end])
    println(stderr, "SPAWN_TRIAL_STATS arm=", ENV["SPAWN_TRIAL"],
            " gc_s=", round(Base.gc_time_ns() / 1e9; digits=1),
            " minflt=", stat[8], " utime_ticks=", stat[12], " stime_ticks=", stat[13],
            " stack_mappings=", @ccall(jl_get_num_stack_mappings()::Cint),
            " glibc=", unsafe_string(@ccall(gnu_get_libc_version()::Cstring)),
            " rss_mb=", Sys.maxrss() >> 20)
    @ccall malloc_stats()::Cvoid
end
