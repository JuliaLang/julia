# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, CompilerSupportLibraries_jll

@testset "CompilerSupportLibraries_jll" begin
    @test isfile(CompilerSupportLibraries_jll.libgcc_s_path)
    @test isfile(CompilerSupportLibraries_jll.libgfortran_path)
    @test isfile(CompilerSupportLibraries_jll.libstdcxx_path)
    @test isfile(CompilerSupportLibraries_jll.libgomp_path)

    # Preserve the JLLWrappers path compatibility accessors used by packages.
    for product in (:libgfortran, :libstdcxx, :libgomp)
        path = getfield(CompilerSupportLibraries_jll, Symbol(product, "_path"))
        get_path = getfield(CompilerSupportLibraries_jll, Symbol("get_", product, "_path"))
        @test get_path() == path
    end
end
