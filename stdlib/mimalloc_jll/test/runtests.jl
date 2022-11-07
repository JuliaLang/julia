# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, mimalloc_jll

@testset "mimalloc_jll" begin
    ptr = ccall((:mi_malloc, mimalloc), Ptr{Cvoid}, (Int,), 4)
    @test ptr != C_NULL
    ccall((:mi_free, mimalloc), Cvoid, (Ptr{Cvoid},), ptr)
end
