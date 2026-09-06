# The classic GCBench binary tree, as a region showcase. The temporary
# trees die wholesale - one @with_region window per batch, one O(1)
# reset - and the long-lived tree stays in region 0. Same-region edges
# are legal in both directions, so make() needs no change of any kind.
# Run:  julia showcase_binarytree.jl [stock|region] [depth]
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

struct Node
    l::Union{Nothing, Node}
    r::Union{Nothing, Node}
end
make(n::Int) = n === 0 ? Node(nothing, nothing) : Node(make(n - 1), make(n - 1))
check(node::Node) = 1 + (node.l === nothing ? 0 : check(node.l) + check(node.r))

function run(mode, n)
    minDepth = 4
    io = IOBuffer()
    println(io, "stretch tree of depth ", n + 1, "\t check: ", check(make(n + 1)))
    long_tree = make(n)
    for depth in minDepth:2:n
        c = 0
        niter = 1 << (n - depth + minDepth)
        if mode == "region"
            for _ in 1:niter
                region_set(EVENT)
                c += check(make(depth))
                region_set(0)
                unsafe_region_reset(EVENT)
            end
        else
            for _ in 1:niter
                c += check(make(depth))
            end
        end
        println(io, niter, "\t trees of depth ", depth, "\t check: ", c)
    end
    println(io, "long lived tree of depth ", n, "\t check: ", check(long_tree))
    return take!(io)
end

mode = length(ARGS) >= 1 ? ARGS[1] : "stock"
n = length(ARGS) >= 2 ? parse(Int, ARGS[2]) : 18
run(mode, 6)                                   # warm
GC.gc()
g0 = Base.gc_num()
t0 = time_ns()
out = run(mode, n)
t1 = time_ns()
g1 = Base.gc_num()
print(String(out))
println("mode               ", mode)
println("wall time          ", round((t1 - t0) / 1e9; digits = 3), " s")
println("collections        ", g1.pause - g0.pause)
println("gc time            ", round(Int, (g1.total_time - g0.total_time) / 1e6), " ms")
println("peak RSS           ", round(Sys.maxrss() / 1e6; digits = 1), " MB")

tsv_row(("showcase", "mode", "param", "wall_s", "collections", "gc_ms", "peak_rss_mb"),
        ("binarytree", mode, n, (t1 - t0) / 1e9, g1.pause - g0.pause,
         (g1.total_time - g0.total_time) / 1e6, Sys.maxrss() / 1e6))
