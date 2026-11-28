# Test that runtime-free callbacks (`@cfunction` with `runtime_free=true`) are compiled into
# a trimmed image, which has no compiler to do that when their expressions are evaluated:
# each with all the code that it calls, which is not code that the image calls otherwise
module RuntimeFreeCallback

@noinline factorial_(n::Cint) = n <= Cint(1) ? Cint(1) : n * factorial_(n - Cint(1))
@noinline shared(x::Cint) = x * Cint(3)
callback(x::Cint) = factorial_(x) + shared(x)
callback2(x::Cint) = shared(x) + Cint(1)

# one that C calls where Julia code cannot run: while the GC is running
const ngc = Ref(0)
count_gc(full::Cint) = (setfield!(ngc, :x, getfield(ngc, :x) + 1); nothing)

function @main(args::Vector{String})::Cint
    println(Core.stdout, ccall(@cfunction(callback, Cint, (Cint,), runtime_free=true), Cint, (Cint,), 4))
    println(Core.stdout, ccall(@cfunction(callback2, Cint, (Cint,), runtime_free=true), Cint, (Cint,), 4))
    count = @cfunction(count_gc, Cvoid, (Cint,), runtime_free=true)
    ccall(:jl_gc_set_cb_pre_gc, Cvoid, (Ptr{Cvoid}, Cint), count, 1)
    GC.gc()
    ccall(:jl_gc_set_cb_pre_gc, Cvoid, (Ptr{Cvoid}, Cint), count, 0)
    println(Core.stdout, ngc[] > 0)
    return 0
end

end
