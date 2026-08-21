# This file is a part of Julia. License is MIT: https://julialang.org/license

# Regression test for #62753: recomputing MPFR's shared constant cache
# allocates through the GMP hooks while holding the cache lock; if that
# allocation safepoints while other threads wait on the lock in GC-unsafe
# ccall regions, the collection can never start. Here a task spamming GC.gc
# keeps stop-the-world windows open while workers call mpfr_acos at
# ever-rising precision, forcing recomputation under the cache's write lock;
# unfixed builds hang within a few iterations.

using Base.MPFR: libmpfr, MPFRRoundingMode as RM, MPFRRoundNearest as RNDN

function acos_at(prec::Int)
    x = BigFloat(1 - 1e-9, precision=prec)
    z = BigFloat(precision=prec)
    ccall((:mpfr_acos, libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, RM), z, x, RNDN)
    return z
end

function main(iters)
    done = Threads.Atomic{Bool}(false)
    gcspam = Threads.@spawn while !done[]
        GC.gc(false)
        yield()
    end
    nworkers = max(2, Threads.nthreads() - 1)
    for i in 1:iters
        prec = 256 + 32 * i
        tasks = [Threads.@spawn acos_at(prec) for _ in 1:nworkers]
        foreach(wait, tasks)
    end
    done[] = true
    wait(gcspam)
end

main(parse(Int, get(ENV, "MPFR_GC_ITERS", "150")))
