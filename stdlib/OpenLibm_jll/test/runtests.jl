# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, OpenLibm_jll

@testset "OpenLibm_jll" begin
    @test ccall((:isopenlibm, libopenlibm), Cint, ()) == 1
end
