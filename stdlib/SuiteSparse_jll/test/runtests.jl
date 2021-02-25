# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, SuiteSparse_jll

@testset "SuiteSparse_jll" begin
    @test ccall((:SuiteSparse_version, libsuitesparseconfig), Cint, (Ptr{Cint},), C_NULL) == 5008
end
