# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, libblastrampoline_jll

@testset "libblastrampoline_jll" begin
    @test isa(Libdl.dlsym(libblastrampoline_jll.libblastrampoline, :dgemm_64_), Ptr{Nothing})

    # Preserve the JLLWrappers path compatibility accessor used by packages.
    @test libblastrampoline_jll.get_libblastrampoline_path() == libblastrampoline_jll.libblastrampoline_path
end
