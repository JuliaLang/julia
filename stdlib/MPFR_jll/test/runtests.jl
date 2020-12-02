# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, MPFR_jll

@testset "MPFR_jll" begin
    vn = VersionNumber(unsafe_string(ccall((:mpfr_get_version,libmpfr), Cstring, ())))
    @test vn == v"4.1.0"
end
