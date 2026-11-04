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

# A fixed amount of work on a shallow stack with no extra tasks
@noinline function spin(depth)
    depth > 0 && return spin(depth - 1) + 1
    x = UInt(1)
    for _ in 1:3_000_000_000
        x ⊻= x << 13
        x ⊻= x >> 7
        x ⊻= x << 17
    end
    return x
end
const sink = Ref{UInt}(0)

function iostats()
    d = Dict(first(p) => parse(Int, last(p)) for p in (split(l, ": ") for l in eachline("/proc/self/io")))
    return (d["rchar"], d["syscr"], d["syscw"])
end

# The unwinder on its own: no signals, and the same libunwind calls as a profile sample
function backtrace_cost(label, N)
    a = iostats()
    t = time_ns()
    local bt
    for _ in 1:N
        bt = backtrace()
    end
    t = time_ns() - t
    b = iostats()
    println(stderr, "SPAWN_TRIAL_BT ", label, " nmaps=", countlines("/proc/self/maps"), " N=", N, " us_per_bt=", round(t / 1e3 / N; digits=1),
            " nptrs=", length(bt), " rchar_per_bt=", (b[1] - a[1]) ÷ N, " syscr_per_bt=", round((b[2] - a[2]) / N; digits=1),
            " syscw_per_bt=", round((b[3] - a[3]) / N; digits=1))
    return bt
end

if get(ENV, "SPAWN_WORKLOAD", "spawn") == "backtrace"
    N = parse(Int, get(ENV, "SPAWN_BT_N", "2000"))
    backtrace_cost("main-few-maps", N)
    bt = fetch(Threads.@spawn backtrace_cost("task-few-maps", N))
    println(stderr, "SPAWN_TRIAL_FRAMES ", join(("$(f.func)@$(basename(string(f.file))):$(f.from_c ? 'C' : 'J')" for f in stacktrace(bt, true)), " < "))
    spawnmany(200000)
    println(stderr, "SPAWN_TRIAL_MAPPINGS stacks=", @ccall(jl_get_num_stack_mappings()::Cint))
    backtrace_cost("main-many-maps", N)
    fetch(Threads.@spawn backtrace_cost("task-many-maps", N))
    exit(0)
end

const NTASKS = parse(Int, get(ENV, "NTASKS", "2000000"))
work() = get(ENV, "SPAWN_WORKLOAD", "spawn") == "spin" ? (sink[] = spin(20); nothing) : spawnmany(NTASKS)
if haskey(ENV, "SPAWN_NOPROFILE")
    work()
else
    @profile work()
end

if haskey(ENV, "SPAWN_TRIAL")
    stat = let s = read("/proc/self/stat", String); split(s[findlast(')', s)+2:end]); end
    # Read before anything below adds to these counters
    rchar, syscr, syscw = iostats()
    nmaps = countlines("/proc/self/maps")
    data = haskey(ENV, "SPAWN_NOPROFILE") ? UInt[] : Profile.fetch(include_meta = true, limitwarn = false)
    samples = count(i -> data[i] == 0 && data[i+1] == 0, 1:length(data)-1)
    println(stderr, "SPAWN_TRIAL_STATS arm=", ENV["SPAWN_TRIAL"],
            " utime_ticks=", stat[12], " stime_ticks=", stat[13], " gc_s=", round(Base.gc_time_ns() / 1e9; digits=1),
            " samples=", samples, " entries=", length(data), " nmaps=", nmaps,
            " rchar=", rchar, " syscr=", syscr, " syscw=", syscw)
end
