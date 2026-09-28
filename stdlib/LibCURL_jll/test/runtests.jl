# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using LibCURL_jll

@testset "LibCURL_jll" begin
    v = unsafe_string(ccall((:curl_version, libcurl), Cstring, ()))
    @test startswith(v, "libcurl/")

    # Preserve the JLLWrappers path compatibility accessor used by packages.
    @test LibCURL_jll.get_libcurl_path() == LibCURL_jll.libcurl_path
end
