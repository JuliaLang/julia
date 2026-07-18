# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, OpenBLAS_jll

if Base.USE_BLAS64
    macro blasfunc(x)
        return Expr(:quote, Symbol(x, "64_"))
    end
else
    macro blasfunc(x)
        return Expr(:quote, x)
    end
end

@testset "OpenBLAS_jll" begin
    @test dlsym(OpenBLAS_jll.libopenblas, @blasfunc(openblas_set_num_threads); throw_error=false) !== nothing
end
