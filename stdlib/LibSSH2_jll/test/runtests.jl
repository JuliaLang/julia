# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, LibSSH2_jll

@testset "LibSSH2_jll" begin
    @test VersionNumber(unsafe_string(ccall((:libssh2_version, libssh2), Cstring, (Cint,), 0))) == v"1.11.1"
end
