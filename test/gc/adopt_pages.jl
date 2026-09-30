# This file is a part of Julia. License is MIT: https://julialang.org/license

# Leave pages that are only partially free behind on every thread, then allocate from a
# single task so that it has to adopt the pages the other threads swept.
#
# Without arguments (as `test/gc.jl` runs it) this is a quick self-checking pass. With
# `--measure [N1 N2 [KEEP]]` the two phases are scaled up and the pool pages that hold
# live objects and the physical footprint are reported after each phase, e.g.
#     julia -t 8 test/gc/adopt_pages.jl --measure

mutable struct Obj
    a::Int
    b::Int
    c::Int
end

function work(n, keep)
    kept = Obj[]
    sizehint!(kept, n ÷ keep + 1)
    for i in 1:n
        o = Obj(i, i, i)
        i % keep == 0 && push!(kept, o)
    end
    return kept
end

function check(kept1, kept2, n1, n2, keep)
    nt = length(kept1)
    @assert sum(length, kept1) + length(kept2) == n1 ÷ nt ÷ keep * nt + n2 ÷ keep
    for kept in (kept1..., kept2)
        for (i, o) in enumerate(kept)
            @assert o.a == o.b == o.c == i * keep
        end
    end
end

# pages that hold live objects, summed over the size classes; only meaningful right after
# a full collection (assumes the default 16 KiB GC pages)
function pages_in_use()
    stats = cglobal(:jl_gc_page_fragmentation_stats, Csize_t)
    npools = Sys.WORD_SIZE == 64 ? 49 : 50 # `JL_GC_N_POOLS`
    return sum(Int(unsafe_load(stats, 2i)) for i in 1:npools) * 16384
end

function footprint()
    if Sys.isapple()
        buf = zeros(UInt64, 64)
        @ccall proc_pid_rusage(getpid()::Cint, 4::Cint, buf::Ptr{UInt64})::Cint
        return "footprint" => Int(buf[10]) # `ri_phys_footprint` of `rusage_info_v4`
    end
    return "maxrss" => Sys.maxrss()
end

mib(x) = round(Int, x / 2^20)

function (@main)(args::Vector{String})
    measure = "--measure" in args
    sizes = [parse(Int, arg) for arg in args if !startswith(arg, "--")]
    n1 = get(sizes, 1, measure ? 40_000_000 : 4_000_000)
    n2 = get(sizes, 2, measure ? 8_000_000 : 1_000_000)
    keep = get(sizes, 3, 16)
    nt = Threads.nthreads()

    kept1 = fetch.([Threads.@spawn work(n1 ÷ nt, keep) for _ in 1:nt])
    GC.gc(true); GC.gc(true)
    live1, pages1 = Base.gc_live_bytes(), pages_in_use()
    kept2 = work(n2, keep)
    GC.gc(true); GC.gc(true)
    live2, pages2 = Base.gc_live_bytes(), pages_in_use()
    check(kept1, kept2, n1, n2, keep)

    if measure
        label, bytes = footprint()
        println("threads=$nt  phase 1: live=$(mib(live1)) MB pages_in_use=$(mib(pages1)) MB  ",
                "phase 2: live=+$(mib(live2 - live1)) MB pages_in_use=+$(mib(pages2 - pages1)) MB  ",
                "$label=$(mib(bytes)) MB")
    end
    return 0
end
