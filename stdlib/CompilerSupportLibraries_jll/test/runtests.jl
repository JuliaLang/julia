# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, CompilerSupportLibraries_jll, Base.BinaryPlatforms

@testset "CompilerSupportLibraries_jll" begin
    @test isfile(CompilerSupportLibraries_jll.libgcc_s_path)
    @test isfile(CompilerSupportLibraries_jll.libgfortran_path)
    @test isfile(CompilerSupportLibraries_jll.libstdcxx_path)
    @test isfile(CompilerSupportLibraries_jll.libgomp_path)
    if libc(HostPlatform()) != "musl"
        @test isfile(CompilerSupportLibraries_jll.libssp_path)
    end
end
