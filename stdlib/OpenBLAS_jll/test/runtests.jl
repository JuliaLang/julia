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
    @test dlsym(OpenBLAS_jll.libopenblas_handle, @blasfunc(openblas_set_num_threads); throw_error=false) != nothing
    @testset "deterministic mul!" begin
        # mul! should be deterministic, see #53054
        function tester_53054()
            C = ComplexF32
            mat = zeros(C, 1, 1)
            for _ in 1:100
                v = [C(1-0.2im) C(2+0.3im)]
                mul!(mat, v, v', C(1+im), 1)
            end
            return mat
        end
        @test allequal(tester_53054() for _ in 1:10000)
    end
end
