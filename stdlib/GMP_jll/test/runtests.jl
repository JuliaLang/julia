# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, GMP_jll

@testset "GMP_jll" begin
    vn = VersionNumber(unsafe_string(unsafe_load(cglobal(dlsym(libgmp, :__gmp_version), Ptr{Cchar}))))
    @test vn == v"6.3.0"

    # Preserve the JLLWrappers path compatibility accessors used by packages.
    @test GMP_jll.get_libgmp_path() == GMP_jll.libgmp_path
    @test GMP_jll.get_libgmpxx_path() == GMP_jll.libgmpxx_path
end
