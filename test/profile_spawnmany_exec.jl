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

const NTASKS = parse(Int, get(ENV, "NTASKS", "2000000"))
if haskey(ENV, "SPAWN_NOPROFILE")
    spawnmany(NTASKS)
else
    @profile spawnmany(NTASKS)
end

if haskey(ENV, "SPAWN_TRIAL")
    stat = let s = read("/proc/self/stat", String); split(s[findlast(')', s)+2:end]); end
    # Per thread: nanoseconds on CPU, nanoseconds waiting on a run queue, timeslices
    sched = [parse.(Int, split(read("/proc/self/task/$t/schedstat", String))) for t in readdir("/proc/self/task")]
    status = read("/proc/self/status", String)
    field(k) = (m = match(Regex("$k:\\s*(\\S+)"), status); m === nothing ? "?" : m[1])
    println(stderr, "SPAWN_TRIAL_STATS arm=", ENV["SPAWN_TRIAL"],
            " gc_s=", round(Base.gc_time_ns() / 1e9; digits=1),
            " minflt=", stat[8], " majflt=", stat[10], " utime_ticks=", stat[12], " stime_ticks=", stat[13],
            " oncpu_s=", round(sum(x[1] for x in sched) / 1e9; digits=1),
            " runq_wait_s=", round(sum(x[2] for x in sched) / 1e9; digits=1),
            " nthreads_os=", length(sched), " vol_ctx=", field("voluntary_ctxt_switches"), " nonvol_ctx=", field("nonvoluntary_ctxt_switches"),
            " cpus=", field("Cpus_allowed_list"), " profile_len=", Profile.len_data(), " rss_mb=", Sys.maxrss() >> 20)
end
