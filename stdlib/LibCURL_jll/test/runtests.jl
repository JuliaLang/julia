# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using LibCURL_jll

@testset "LibCURL_jll" begin
    v = unsafe_string(ccall((:curl_version, libcurl), Cstring, ()))
    @test startswith(v, "libcurl/")
end
