# GCBenchmarks' serial/linked/list, as a region showcase. The list is
# built by prepending - every store is young-to-old, legal - and the
# whole 134-million-node list is garbage the moment the function
# returns: one window, one O(1) reset, against the stock collector
# tracing it to death.
# Run:  julia showcase_linkedlist.jl [stock|region] [mb]
region_set(n)   = ccall(:jl_gc_region_set, Cint, (Cint,), n)
unsafe_region_reset(n) = ccall(:jl_gc_region_unsafe_reset, UInt64, (Cint,), n)
const EVENT = 2

# When REGIONS_TSV names a file, append one row of numbers to it; the first
# row into a new file is the header. results/run_all.sh collects every
# measurement this way and results/plot.py draws from the files.
function tsv_row(names, values)
    path = get(ENV, "REGIONS_TSV", "")
    isempty(path) && return nothing
    header = !isfile(path) || filesize(path) == 0
    open(path, "a") do io
        header && println(io, "# ", join(names, '\t'))
        println(io, join(values, '\t'))
    end
    return nothing
end

mutable struct ListNode
    key::Int64
    next::ListNode
    ListNode(x) = new(x)
    ListNode(x, y) = new(x, y)
end

@noinline function list(n)
    start = ListNode(1)
    current = start
    for i in 2:(n * 1024^2)
        current = ListNode(i, current)
    end
    return current.key
end

function run(mode, n)
    if mode == "region"
        # The discipline this workload embodies: the region replaces the
        # collector. With coexistence the stock heuristics would otherwise
        # trace the growing multi-GB region mid-window for exact liveness -
        # legal now, but pure waste when everything dies wholesale at the
        # reset.
        GC.enable(false)
        region_set(EVENT)
        k = list(n)
        region_set(0)
        unsafe_region_reset(EVENT)
        GC.enable(true)
        return k
    end
    return list(n)
end

mode = length(ARGS) >= 1 ? ARGS[1] : "stock"
n = length(ARGS) >= 2 ? parse(Int, ARGS[2]) : 64
run(mode, 1)                                   # warm
GC.gc()
g0 = Base.gc_num()
t0 = time_ns()
k = run(mode, n)
t1 = time_ns()
g1 = Base.gc_num()
println("key                ", k)
println("mode               ", mode)
println("wall time          ", round((t1 - t0) / 1e9; digits = 3), " s")
println("collections        ", g1.pause - g0.pause)
println("gc time            ", round(Int, (g1.total_time - g0.total_time) / 1e6), " ms")
println("peak RSS           ", round(Sys.maxrss() / 1e6; digits = 1), " MB")

tsv_row(("showcase", "mode", "param", "wall_s", "collections", "gc_ms", "peak_rss_mb"),
        ("linkedlist", mode, n, (t1 - t0) / 1e9, g1.pause - g0.pause,
         (g1.total_time - g0.total_time) / 1e6, Sys.maxrss() / 1e6))
