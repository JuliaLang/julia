# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, LibSSH2_jll

@testset "LibSSH2_jll" begin
    #TODO # We use a `startswith()` here because when built from source, this returns "1.9.0_DEV"
    #TODO vn = startswith(unsafe_string(ccall((:libssh2_version, libssh2), Cstring, (Cint,), 0)), "1.9.0")
    @test VersionNumber(major[], minor[], patch[]) == v"1.11.1"
end
