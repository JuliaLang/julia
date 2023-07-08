# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, SuiteSparse_jll

# SuiteSparse only uses SUITESPARSE_MAIN_VERSION and SUITESPARSE_SUB_VERSION to compute its version
# The SUITESPARSE_SUBSUB_VERSION is not used
# TODO before release: update to 7020 or above when upstreamed.
# This should be safe and unecessary since we specify exact version of the BB JLL.
@testset "SuiteSparse_jll" begin
    @test ccall((:SuiteSparse_version, libsuitesparseconfig), Cint, (Ptr{Cint},), C_NULL) > 7000
end
