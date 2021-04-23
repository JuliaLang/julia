# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, PCRE2_jll

@testset "PCRE2_jll" begin
    vstr = zeros(UInt8, 32)
    @test ccall((:pcre2_config_8, libpcre2_8), Cint, (UInt32, Ref{UInt8}), 11, vstr) > 0
    vn = VersionNumber(split(unsafe_string(pointer(vstr)), " ")[1])
    @test vn == v"10.36.0"
end
