#!/usr/bin/env julia
# Demo for the tiered compilation PR.
#
# Run with the in-tree julia from the repo root:
#
#     ./julia contrib/tier_demo.jl
#
# Spawns child processes that execute an identical LinearAlgebra TTFX
# workload under three configurations:
#
#   (1) baseline -O2  — every function compiled at -O2 up front
#   (2) baseline -O0  — every function compiled at -O0, no promotion
#   (3) tiered T0=-O0 — cold code at -O0, hot code promoted to -O3 by the
#                       background tier worker
#
# Reports median wall time per config and the tier worker's enqueue / queue
# / swap counts so you can see promotion actually happen.

using Printf

const JULIA = joinpath(@__DIR__, "..", "julia")

const WORKLOAD = raw"""
    # Simulate a PrecompileTools-style workload: many Val{N}-parameterized
    # specializations. Body is intentionally small + @noinline so each
    # (T, Val{N}) survives as a distinct CI that the tier probe can see;
    # bigger inlinable bodies caused inference/concrete-eval to fold most
    # call sites away, leaving nothing to promote. LLVM at -O2 still
    # unrolls/vectorizes the inner muladd loop versus -O0, which is what
    # makes the hot pass show a clear post-promotion win.
    @noinline function loopwork(x::T, ::Val{N}) where {T, N}
        s = zero(T); t = zero(T)
        for i in 1:N
            y = T(i)
            s = muladd(y, x, s)
            t = muladd(y, y, t)
        end
        s - t
    end

    leaf_types = (Float64, Float32, Float16, Int32, Int64, UInt32, Int16, UInt64)

    # Run the parameterized loop once for each (T, Val{N}). `mult` scales
    # how much work this pass does — the cold pass uses mult=1 (just enough
    # to trip tier promotion), the hot pass uses a big mult so steady-state
    # codegen quality shows up in the wall time.
    function run_pass(mult)
        s = 0.0
        for N in 1:80
            v = Val(N)
            for T in leaf_types
                x = T(N % 100 + 1)
                for _ in 1:(40 * mult)
                    s += Float64(loopwork(x, v))
                end
            end
        end
        s
    end

    # --- COLD pass: T0 codegen/interp (TTFX). Enqueues hot CIs; with the
    # worker on its own thread (--threads=1,1) it begins promoting them to -O3
    # in the BACKGROUND, overlapping the work below.
    cold = @timed run_pass(1)

    # Kick the scheduler once so the :interactive tier worker actually wakes and
    # starts promoting. Without ANY yield between enqueue and the hot loop the
    # worker stays asleep and all promotion falls into the synchronous tail
    # (tiering then loses badly). Real programs yield constantly (allocations,
    # I/O, task switches, GC), so this models the common case; a pure
    # allocation-free compute path would NOT get this for free.
    yield()

    # --- HOT pass: NO synchronous drain first, so promotion OVERLAPS this
    # pass — hot ramps from T0 speed to -O3 as CIs are swapped in. This is the
    # realistic steady-state path for a long-running process (vs. forcing a
    # blocking drain, which only happens at e.g. sysimage save).
    hot = @timed run_pass(200)

    # Tail drain: finish any CIs the worker didn't promote during the hot pass.
    # ~0 when the workload outlived promotion; the residual blocking cost.
    tail = @elapsed ccall(:jl_tier_drain, Cvoid, ())

    calls = Ref{UInt64}(0); wins = Ref{UInt64}(0)
    pushes = Ref{UInt64}(0); pops = Ref{UInt64}(0); drops = Ref{UInt64}(0)
    ccall(:jl_tier_get_stats, Cvoid, (Ptr{UInt64}, Ptr{UInt64}), calls, wins)
    ccall(:jl_tier_get_queue_stats, Cvoid,
          (Ptr{UInt64}, Ptr{UInt64}, Ptr{UInt64}), pushes, pops, drops)
    swaps = ccall(:jl_tier_get_swaps, UInt64, ())
    # Background -O3 CPU spent (overlapped + tail); a real cost even though it
    # is mostly hidden from wall-clock by the worker thread.
    cn = Ref{UInt64}(0); pn = Ref{UInt64}(0)
    ccall(:jl_tier_get_timing, Cvoid, (Ptr{UInt64}, Ptr{UInt64}), cn, pn)
    println("COLD_WALL=",    round(cold.time,         digits=4))
    println("COLD_COMPILE=", round(cold.compile_time, digits=4))
    println("HOT_WALL=",     round(hot.time,          digits=4))
    println("TAIL_WALL=",    round(tail,              digits=4))
    println("PROMOTE_CPU=",  round(pn[] / 1e9,        digits=4))
    println("CALLS=", calls[])
    println("WINS=", wins[])
    println("PUSHES=", pushes[])
    println("POPS=", pops[])
    println("SWAPS=", swaps[])
"""

function run_config(label, env; optlevel=2, compile=nothing, reps=3)
    cold_walls = Float64[]; cold_comps = Float64[]; hot_walls = Float64[]
    tail_walls = Float64[]; promote_cpus = Float64[]
    calls = wins = pushes = pops = swaps = 0
    for _ in 1:reps
        fullenv = copy(ENV)
        merge!(fullenv, env)
        io = IOBuffer()
        # --pkgimages=no forces fresh JIT of the package code so there's
        # actually compile work for the tier system to operate on. With
        # pkgimages on, `using LinearAlgebra` just loads cached native code
        # and tier_calls is 0 — accurate but uninteresting.
        extra = compile === nothing ? `` : `--compile=$compile`
        # --threads=1,1: one compute thread + one :interactive thread so the
        # tier worker promotes in parallel with the workload (matches how Pkg
        # launches precompile children), rather than time-slicing one thread.
        cmd = `$JULIA --startup-file=no --pkgimages=no --threads=1,1 -O$optlevel $extra -e $WORKLOAD`
        run(pipeline(setenv(cmd, fullenv); stdout=io, stderr=stderr))
        out = String(take!(io))
        parse_line(key) = parse(Float64,
            split(only(filter(startswith(key * "="), split(out, '\n'))), '=')[2])
        push!(cold_walls, parse_line("COLD_WALL"))
        push!(cold_comps, parse_line("COLD_COMPILE"))
        push!(hot_walls,  parse_line("HOT_WALL"))
        push!(tail_walls, parse_line("TAIL_WALL"))
        push!(promote_cpus, parse_line("PROMOTE_CPU"))
        calls  = Int(parse_line("CALLS"))
        wins   = Int(parse_line("WINS"))
        pushes = Int(parse_line("PUSHES"))
        pops   = Int(parse_line("POPS"))
        swaps  = Int(parse_line("SWAPS"))
    end
    med(xs) = (sort!(xs); xs[(end + 1) ÷ 2])
    (; label,
       cold_wall=med(cold_walls), cold_comp=med(cold_comps),
       hot_wall=med(hot_walls), tail_wall=med(tail_walls), promote_cpu=med(promote_cpus),
       calls, wins, pushes, pops, swaps)
end

# One throwaway run to settle pkg precompile state and disk cache.
print("warming up... "); flush(stdout)
run_config("warmup", Dict("JULIA_TIER_ENABLE" => "0"); reps=1)
println("done")

results = [
    run_config("baseline -O2", Dict("JULIA_TIER_ENABLE" => "0"); optlevel=2),
    run_config("baseline -O0", Dict("JULIA_TIER_ENABLE" => "0"); optlevel=0),
    run_config("tiered interp, thr=10", Dict("JULIA_TIER_ENABLE" => "1"); optlevel=2),
    run_config("tiered interp, thr=1",
               Dict("JULIA_TIER_ENABLE" => "1",
                    "JULIA_TIER_THRESHOLD" => "1"); optlevel=2),
]

tot(r) = r.cold_wall + r.hot_wall + r.tail_wall
base_cold  = results[1].cold_wall
base_hot   = results[1].hot_wall
base_total = tot(results[1])
println()
println("Tiered compilation demo (median of 3)")
println("=" ^ 123)
@printf("  %-24s %8s %8s %8s %8s %8s %8s %8s %8s   %6s\n",
        "config", "cold(s)", "hot(s)", "tail(s)", "total(s)", "promCPU",
        "Δ cold", "Δ hot", "Δ total", "swaps")
for r in results
    total = tot(r)
    dcold  = round(100 * (r.cold_wall - base_cold)  / base_cold,  digits=1)
    dhot   = round(100 * (r.hot_wall  - base_hot)   / base_hot,   digits=1)
    dtotal = round(100 * (total       - base_total) / base_total, digits=1)
    @printf("  %-24s %8.3f %8.3f %8.3f %8.3f %8.3f %+7.1f%% %+7.1f%% %+7.1f%%   %6d\n",
            r.label, r.cold_wall, r.hot_wall, r.tail_wall, total, r.promote_cpu,
            dcold, dhot, dtotal, r.swaps)
end
println()
println("Legend:")
println("  cold    — first-run wall (T0 JIT/interp); 25 600 loopwork calls")
println("  hot     — steady-state wall; 5 120 000 calls. For tiered configs the")
println("            background -O3 promotion OVERLAPS this, so it ramps T0→-O3.")
println("  tail    — residual blocking jl_tier_drain after hot (~0 if hot")
println("            outlived promotion; would be the full -O3 cost if forced")
println("            synchronous, e.g. at sysimage save with no overlap)")
println("  total   — cold + hot + tail (wall-clock, user-perceived)")
println("  promCPU — background -O3 recompile CPU spent (overlapped + tail);")
println("            mostly hidden from wall-clock but a real extra CPU cost")
println("  swaps   — CIs re-compiled at -O3 and atomically swapped in")
println()
println("Notes:")
println(" - Children run --threads=1,1 so promotion overlaps the workload; the win")
println("   depends on that overlap. Forced synchronous (no spare thread / sysimage")
println("   save) the -O3 cost moves into 'tail' and tiering loses end-to-end.")
println(" - 'total' is wall-clock; 'promCPU' is the extra background -O3 CPU it costs.")
