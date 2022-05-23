# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, nghttp2_jll

struct nghttp2_info
    age::Cint
    version_num::Cint
    version_str::Cstring
    proto_str::Cstring
end

@testset "nghttp2_jll" begin
    info = unsafe_load(ccall((:nghttp2_version,libnghttp2), Ptr{nghttp2_info}, (Cint,), 0))
    @test VersionNumber(unsafe_string(info.version_str)) == v"1.47.0"
end
