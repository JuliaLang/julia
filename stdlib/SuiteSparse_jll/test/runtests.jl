# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, SuiteSparse_jll

# SuiteSparse only uses SUITESPARSE_MAIN_VERSION and SUITESPARSE_SUB_VERSION to compute its version
# The SUITESPARSE_SUBSUB_VERSION is not used
@testset "SuiteSparse_jll" begin
    @test ccall((:SuiteSparse_version, libsuitesparseconfig), Cint, (Ptr{Cint},), C_NULL) == 7000
end
