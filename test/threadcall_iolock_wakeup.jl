# This file is a part of Julia. License is MIT: https://julialang.org/license

# Run with 2 default threads and no interactive thread.
#
# Regression test: finalizers that take the iolock run on a libuv worker thread during a
# `@threads` loop of `@threadcall`s. The `@threadcall` results must still be delivered.

@assert Threads.nthreads(:interactive) == 0 && Threads.nthreads(:default) == 2

takes_iolock(_) = (Base.iolock_begin(); Base.iolock_end())

function threadcalls()
    Threads.@threads for i in 1:8
        # GC on the libuv worker, which runs the pending finalizers there
        i % 4 == 0 && @threadcall(:jl_gc_collect, Cvoid, (Cint,), 0)
        @threadcall(:jl_getpagesize, Clong, ())
    end
end

for _ in 1:20
    for j in 1:50
        finalizer(takes_iolock, Ref(j))
    end
    threadcalls()
end
