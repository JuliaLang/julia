# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, SuiteSparse_jll

# SuiteSparse only uses SUITESPARSE_MAIN_VERSION and SUITESPARSE_SUB_VERSION to compute its version
# The SUITESPARSE_SUBSUB_VERSION is not used
# TODO before release: update to 7020 or above when upstreamed.
# This should be safe and unnecessary since we specify exact version of the BB JLL.
@testset "SuiteSparse_jll" begin
    @test ccall((:SuiteSparse_version, libsuitesparseconfig), Cint, (Ptr{Cint},), C_NULL) > 7000

    # Preserve the JLLWrappers path compatibility accessors used by packages.
    for product in (
        :libamd,
        :libbtf,
        :libcamd,
        :libccolamd,
        :libcholmod,
        :libcolamd,
        :libklu,
        :libldl,
        :librbio,
        :libspqr,
        :libsuitesparseconfig,
        :libumfpack,
    )
        path = getfield(SuiteSparse_jll, Symbol(product, "_path"))
        get_path = getfield(SuiteSparse_jll, Symbol("get_", product, "_path"))
        @test get_path() == path
    end
end
