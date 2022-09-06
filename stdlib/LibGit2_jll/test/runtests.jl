# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, LibGit2_jll

@testset "LibGit2_jll" begin
    major = Ref{Cint}(0)
    minor = Ref{Cint}(0)
    patch = Ref{Cint}(0)
    @test ccall((:git_libgit2_version, libgit2), Cint, (Ref{Cint}, Ref{Cint}, Ref{Cint}), major, minor, patch) == 0
    @test VersionNumber(major[], minor[], patch[]) == v"1.4.3"
end
