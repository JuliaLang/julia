# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, LibSSH2_jll

@testset "LibSSH2_jll" begin
    vn = unsafe_string(ccall((:libssh2_version, libssh2), Cstring, (Cint,), 0))
    # Depending on how LibSSH2_jll was installed (downloaded from
    # BinaryBuilder or built from source here), the version number is
    # either "1.11.1" or "1.11.1_DEV", respectively.
    @test startswith(vn, "1.11.1")
end
