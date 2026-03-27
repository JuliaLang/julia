# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, LMDB_jll

@testset "LMDB_jll" begin
    let (major, minor, patch) = (Ref{Cint}(0), Ref{Cint}(0), Ref{Cint}(0))
        ccall((:mdb_version, liblmdb), Cstring, (Ptr{Cint}, Ptr{Cint}, Ptr{Cint}),
              major, minor, patch)
        @test (major[], minor[], patch[]) == (0, 9, 33)
    end
end
