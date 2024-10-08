using Base.Experimental.SIMD
using Test
using InteractiveUtils

function vcopyto!(a::Array{T}, b::Array{T}) where T
    stride = natural_vecwidth(T)
    VT = Vec{stride, T}
    @assert length(a) == length(b)
    @assert length(a) % stride == 0
    @inbounds for i in 1:stride:length(a)
        vec = vload(VT, a, i)
        vstore!(b, vec, i)
    end
end

@testset "load/store" begin
    A = rand(64)
    B = zeros(64)

    vcopyto!(A, B)
    @test A == B

    @test_throws BoundsError vload(Vec{4, Float64}, A, 62)
    vec = vload(Vec{4, Float64}, A, 1)
    @test_throws BoundsError vstore!(A, vec, 62)

    load(A, i) = @inbounds vload(Vec{4, Float64}, A, i)
    store(A,v,i) = @inbounds vstore!(A, v, i)

    ir = sprint(io->code_llvm(io, vload, (Type{Vec{4, Float64}}, Vector{Float64}, Int)))
    @test contains(ir, "call void @j_throw_boundserror")

    ir = sprint(io->code_llvm(io, load, (Vector{Float64}, Int)))
    @test contains(ir, "load <4 x double>")
    @test !contains(ir, "call void @j_throw_boundserror")
end
