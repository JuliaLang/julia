# Demonstrator D: optimistic parallel Delaunay mesh refinement, regions vs the
# stock collector, on the SAME code. The canonical irregular-parallel benchmark
# (Galois / Lonestar). A shared triangle mesh; each round, N workers each pick a
# bad triangle and SPECULATIVELY compute its Bowyer-Watson cavity and a quality
# analysis (read-only on the mesh, allocation-heavy, in the worker's own leaf);
# then a deterministic sequential commit applies the non-conflicting cavities in
# seed-id order -- a cavity that overlaps one already committed this round
# ABORTS and its whole speculation is discarded. Deterministic, so region and
# stock produce the identical mesh; only the abort reclamation differs -- the
# region resets a lost speculation in O(1), the stock collector traces it.
#   run with: julia -t4 dmr.jl
include(joinpath(@__DIR__, "dmr_core.jl"))
include(joinpath(@__DIR__, "demo_common.jl"))
using .DMRCore, .DemoCommon
using Printf

@noinline region_set(n)        = ccall(:jl_gc_region_set, Cint, (Cint,), n)
@noinline unsafe_region_reset(n)      = UInt64(ccall(:jl_gc_region_unsafe_reset, UInt64, (Cint,), n))
@noinline region_parent!(c, p) = ccall(:jl_gc_region_declare_parent, Cint, (Cint, Cint), c, p)
@noinline quarantined(n)       = Int(ccall(:jl_gc_region_quarantined, Cint, (Cint,), n)) != 0
leaf_of(tid) = tid

# A committed plan carries only VALUES (ids, a point, boundary edges) -- no leaf
# object -- so producing it in region 0 from a leaf speculation is not an escape.
struct Plan
    seed::Int
    cc::DMRCore.P
    cav::Vector{Int}
    bnd::Vector{Tuple{Int,Int}}
    ok::Bool
end

# Speculate on `seed`: the heavy read-only work (cavity BFS + `work` throwaway
# quality samples) runs in `leaf`; the small plan is copied to region 0 as
# values. A lost round resets the leaf and discards all of it.
function speculate(m, seed::Int, leaf::Int, work::Int)
    t = m.tris[seed]
    (t.alive && tri_area(m, t) > m.area_thresh) || return Plan(seed, DMRCore.P(0,0), Int[], Tuple{Int,Int}[], false)
    cc = centroid(m, t)                            # always inside the domain: terminates
    leaf != 0 && region_set(leaf)
    cav, incav, bnd = cavity(m, seed, cc)          # heavy: BFS, Set + Vectors, in the leaf
    # A quality analysis a real refiner does: `work` throwaway samples per cavity.
    if work > 0
        junk = Vector{DMRCore.P}(undef, work)
        @inbounds for w in 1:work; junk[w] = DMRCore.P(cc.x*w, cc.y*w); end
        s = 0.0; @inbounds for w in 1:work; s += junk[w].x; end
        cc = DMRCore.P(cc.x + (s*0.0), cc.y)       # data-depend, no effect
    end
    leaf != 0 && region_set(0)
    Plan(seed, cc, copy(cav), copy(bnd), true)     # the small plan, in region 0
end

# Apply a plan to the shared mesh (region 0): add the point, kill the cavity,
# fan from the point to the boundary. Returns the new triangle count added.
function commit_plan!(m, pl::Plan)
    pid = (push!(m.pts, pl.cc); length(m.pts))
    for id in pl.cav
        t = DMRCore.byid(m, id); t.alive = false
        DMRCore.del_edge!(m, t.a, t.b, id); DMRCore.del_edge!(m, t.b, t.c, id); DMRCore.del_edge!(m, t.c, t.a, id)
    end
    for (u, v) in pl.bnd
        DMRCore.newtri!(m, u, v, pid)
    end
    length(pl.bnd)
end

# The round-based optimistic refinement. Returns (final tri count, aborts).
function refine_optimistic!(m, nworkers, tree::Bool, work::Int; maxtris=200000)
    aborts = 0
    plans = Vector{Plan}(undef, nworkers)
    while ntris(m) < maxtris
        # gather up to nworkers distinct bad seeds
        batch = Int[]
        for (i, t) in enumerate(m.tris)
            t.alive && tri_area(m, t) > m.area_thresh && (push!(batch, i); length(batch) == nworkers && break)
        end
        isempty(batch) && break
        # PHASE 1: speculate in parallel (read-only on m)
        Threads.@threads for k in 1:length(batch)
            leaf = tree ? leaf_of(Threads.threadid()) : 0
            plans[k] = speculate(m, batch[k], leaf, work)
            leaf != 0 && unsafe_region_reset(leaf)          # the speculation's leaf garbage, dropped
        end
        # PHASE 2: commit in deterministic seed order; overlapping cavities abort
        consumed = Set{Int}()
        for k in 1:length(batch)
            pl = plans[k]
            if !pl.ok || any(id -> id in consumed, pl.cav)
                aborts += 1; continue                # conflict or stale: discard the plan
            end
            for id in pl.cav; push!(consumed, id); end
            commit_plan!(m, pl)
        end
    end
    (ntris(m), aborts)
end

# One sibling leaf per thread.
for t in 1:Threads.nthreads()
    region_parent!(leaf_of(t), 0)
end

function run_scale(label, grid, thresh, work)
    build() = build_square(grid, thresh)
    r_cs = Ref(0); s_cs = Ref(0); r_ab = Ref(0); s_ab = Ref(0)
    reg, sto, _ = DemoCommon.ab(
        () -> (m = build(); a = refine_optimistic!(m, Threads.nthreads(), true,  work); r_cs[]=checksum(m); r_ab[]=a[2]; a[1]),
        () -> (m = build(); a = refine_optimistic!(m, Threads.nthreads(), false, work); s_cs[]=checksum(m); s_ab[]=a[2]; a[1]); reps = 2)
    report_table("D", "$label  (grid $grid, work=$work, $(Threads.nthreads()) threads)", reg, sto)
    anyq = any(quarantined(leaf_of(t)) for t in 1:Threads.nthreads())
    @printf("  region aborts %d   stock aborts %d   same mesh: %s   quarantine: %s\n",
            r_ab[], s_ab[], string(r_cs[] == s_cs[]), string(anyq))
    r_cs[] == s_cs[] && !anyq
end

# Warm STOCK first so the geometry compiles at region 0.
let m = build_square(8, 0.01); refine_optimistic!(m, Threads.nthreads(), false, 32); end
let m = build_square(8, 0.01); refine_optimistic!(m, Threads.nthreads(), true, 32); end
# A small mesh and a mild threshold keep the O(n) per-round rescan from
# dominating; the work sweep supplies the allocation pressure instead of a
# huge mesh. (A worklist would remove the rescan; kept simple here.)
const PROBE = get(ENV, "DMR_PROBE", "") == "1"
println("Demonstrator D -- optimistic parallel Delaunay mesh refinement, regions vs stock\n")
allok = true
if PROBE
    # One single refinement, timed, so the per-refinement cost is known before
    # any sweep. Prints the mesh size, rounds' worth of aborts, and the seconds.
    let m = build_square(16, 2.0e-4)
        t0 = time_ns()
        n, ab = refine_optimistic!(m, Threads.nthreads(), false, 512)
        dt = (time_ns() - t0) / 1e9
        @printf("probe: %d tris, %d aborts, %.3f s for one refinement\n", n, ab, dt)
    end
else
    allok &= run_scale("work=0   ", 16, 2.0e-4, 0)
    allok &= run_scale("work=512 ", 16, 2.0e-4, 512)
    allok &= run_scale("work=2048", 16, 2.0e-4, 2048)
end
println()
println(allok ? "DEMO D: consistent (same mesh, no escape)" : "DEMO D: CHECK (mesh or escape)")
