# This file is a part of Julia. License is MIT: https://julialang.org/license
#
# Runnable demo for the tiered-compilation prototype.
#
#   JULIA_TIER=1 JULIA_TIER_LOG=1 ./julia tiered/demo.jl
#
# Without JULIA_TIER=1 at startup the per-CodeInstance call counters are not
# emitted, so the demo still runs but reports callcount == 0.

include(joinpath(@__DIR__, "TieredCompilation.jl"))
using .TieredCompilation

# A small workload function (tier-1 target).
function work(x::Int)
    s = 0
    for i in 1:x
        s += i % 7
    end
    return s
end

# Dynamic-dispatch driver: with `@nospecialize`, calls to `g` route through
# `CodeInstance.invoke` at run time, so a tier swap on the callee is observable.
function drive(@nospecialize(g), n::Int)
    s = 0
    for i in 1:n
        s += g(i % 100 + 1)::Int
    end
    return s
end

const N = 200_000

work(10)            # force native (tier-1) compilation
drive(work, N)      # accrue calls on the native CodeInstance

println("Before promotion: ", tier_stats(work, (Int,)))
before = work(50)

opt_ci = promote_tier!(work, (Int,))
println("Promoted: optimized CodeInstance owner = ", opt_ci.owner)

drive(work, N)      # these calls now route through the optimized code

println("After promotion (native CI):    ", tier_stats(work, (Int,)))
println("After promotion (optimized CI): callcount = ",
        ccall(:jl_tier_callcount, UInt64, (Any,), opt_ci))
after = work(50)

@assert before == after "tiered promotion changed the result ($before != $after)"
println("Result preserved across promotion: work(50) = ", after)

if get(ENV, "JULIA_TIER", "") != "1"
    @warn "JULIA_TIER was not 1 at startup; call counts read 0. " *
          "Re-run: JULIA_TIER=1 JULIA_TIER_LOG=1 ./julia tiered/demo.jl"
end
