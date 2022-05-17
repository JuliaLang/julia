# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, libblastrampoline_jll

@testset "libblastrampoline_jll" begin
    @test isa(Libdl.dlsym(Libdl.dlopen(:libblastrampoline), :dgemm_64_), Ptr{Nothing})
end
