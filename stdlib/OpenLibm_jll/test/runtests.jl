# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, OpenLibm_jll

@testset "OpenLibm_jll" begin
    @test ccall((:isopenlibm, libopenlibm), Cint, ()) == 1

    # Preserve the JLLWrappers path compatibility accessor used by packages.
    @test OpenLibm_jll.get_libopenlibm_path() == OpenLibm_jll.libopenlibm_path
end
