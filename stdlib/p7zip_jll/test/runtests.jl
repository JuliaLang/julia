# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, p7zip_jll

@testset "p7zip_jll" begin
    @test isfile(p7zip_jll.p7zip_path)
end
