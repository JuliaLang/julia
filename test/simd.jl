using Base.Experimental.SIMD
using Test
using InteractiveUtils

function vcopyto!(a::Array{T}, b::Array{T}) where T
    VT = preferred_vector(T)
    stride = width(VT)
    @assert length(a) == length(b)
    @assert length(a) % stride == 0
    @inbounds for i in 1:stride:length(a)
        vec = vload(VT, a, i)
        vstore!(b, vec, i)
    end
end

# todo: noninline/mutable types?
primitive type I256 256 end
primitive type I512 512 end

@testset "preferred_vector_width" begin
    for T in (Int8, Int16, Int32, Int64, Int128, I256)
        max_width = 32 # avx2
        @test width(preferred_vector(T)) == max_width ÷ sizeof(T)
    end
    @test_throws ErrorException preferred_vector(I526)
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

@testset "basic floating-point arithmetic" begin
    A = rand(64)
    v = vload(Vec{4, Float64}, A, 1)

    @test v+v isa Vec{4, Float64}
    ir = sprint(io->code_llvm(io, +, (Vec{4, Float64}, Vec{4, Float64})))
    @test contains(ir, "fadd <4 x double>")
    
    @test v-v isa Vec{4, Float64}
    ir = sprint(io->code_llvm(io, -, (Vec{4, Float64}, Vec{4, Float64})))
    @test contains(ir, "fsub <4 x double>")

    @test v*v isa Vec{4, Float64}
    ir = sprint(io->code_llvm(io, *, (Vec{4, Float64}, Vec{4, Float64})))
    @test contains(ir, "fmul <4 x double>")

    @test v/v isa Vec{4, Float64}
    ir = sprint(io->code_llvm(io, /, (Vec{4, Float64}, Vec{4, Float64})))
    @test contains(ir, "fdiv <4 x double>")

    @test muladd(v, v, v) isa Vec{4, Float64}
    ir = sprint(io->code_llvm(io, muladd, (Vec{4, Float64}, Vec{4, Float64}, Vec{4, Float64})))
    @test contains(ir, "fmul contract <4 x double>")
    @test contains(ir, "fadd contract <4 x double>")

    @test -v isa Vec{4, Float64}
    ir = sprint(io->code_llvm(io, -, (Vec{4, Float64},)))
    @test contains(ir, "fneg <4 x double>")

    # TODO: Way to test Intrinsics directly?
    #`-v` -> ERROR: neg_float_withtype: value is not a primitive type
end

@testset "select" begin
    ir = sprint(io->code_llvm(io, select, (Vec{4, Bool}, Vec{4, Float64}, Vec{4, Float64})))
    @test contains(ir, "icmp eq <4 x i8>")
    @test contains(ir, "select <4 x i1>")
end

@test "basic integer arithmetic" begin
end

@test "basic logical operations" begin
end
