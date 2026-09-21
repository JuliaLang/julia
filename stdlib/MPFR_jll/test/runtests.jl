# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, MPFR_jll

@testset "MPFR_jll" begin
    vn = VersionNumber(unsafe_string(ccall((:mpfr_get_version,libmpfr), Cstring, ())))
    @test vn == v"4.2.2"

    # Preserve the JLLWrappers path compatibility accessor used by packages.
    @test MPFR_jll.get_libmpfr_path() == MPFR_jll.libmpfr_path
end
