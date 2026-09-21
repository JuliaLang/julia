# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, dSFMT_jll

@testset "dSFMT_jll" begin
    idstring = ccall((:dsfmt_get_idstring, libdSFMT), Ptr{UInt8}, ())
    @test startswith(unsafe_string(idstring), "dSFMT2-")

    # Preserve the JLLWrappers path compatibility accessor used by packages.
    @test dSFMT_jll.get_libdSFMT_path() == dSFMT_jll.libdSFMT_path
end
