# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, libblastrampoline_jll

@testset "libblastrampoline_jll" begin
    @test isa(Libdl.dlsym(libblastrampoline_jll.libblastrampoline, :dgemm_64_), Ptr{Nothing})

    # Preserve the JLLWrappers path compatibility accessor used by packages.
    @test libblastrampoline_jll.get_libblastrampoline_path() == libblastrampoline_jll.libblastrampoline_path
end

# A `ccall` naming LBT by string must still get a forwarded LBT (issue #63432).
# Each check runs in a fresh process so nothing has loaded LBT beforehand.
@testset "ccall into LBT by library name" begin
    ilaver, BlasInt = Base.USE_BLAS64 ? (:ilaver_64_, Int64) : (:ilaver_, Int32)
    for lib in (Base.liblapack_name, "libblastrampoline")
        script = """
        using LinearAlgebra
        a, b, c = Ref{$BlasInt}(0), Ref{$BlasInt}(0), Ref{$BlasInt}(0)
        ccall(($(repr(ilaver)), $(repr(lib))), Cvoid, (Ref{$BlasInt}, Ref{$BlasInt}, Ref{$BlasInt}), a, b, c)
        print(a[])
        """
        @test parse(Int, readchomp(`$(Base.julia_cmd()) -e $script`)) >= 3
    end
end
