# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestDiagonal

using Test, LinearAlgebra, Random
using LinearAlgebra: BlasFloat, BlasComplex
using Base: rtoldefault

const BASE_TEST_PATH = joinpath(Sys.BINDIR, "..", "share", "julia", "test")
isdefined(Main, :Furlongs) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "Furlongs.jl"))
using .Main.Furlongs

isdefined(Main, :OffsetArrays) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "OffsetArrays.jl"))
using .Main.OffsetArrays

const TNumber = Union{Float64,ComplexF64,Rational{Int},Complex{Rational{Int}}}
struct MockUnitful{T<:TNumber} <: Number
    data::T
    MockUnitful(data::T) where T<:TNumber = new{T}(data)
end
MockUnitful{T}(i::Integer) where {T<:TNumber} = MockUnitful(T(i))
Base.rtoldefault(::Type{MockUnitful{T}}) where T<:TNumber = rtoldefault(T)
function Base.rtoldefault(::Type{MockUnitful{T1}}, ::Type{MockUnitful{T2}}, atol::Real) where {T1<:TNumber,T2<:Number}
    rtoldefault(T1, T2, atol)
end
Base.:(==)(a::MockUnitful{T}, b::MockUnitful{T}) where T<:TNumber = a.data == b.data
Base.isless(a::MockUnitful{T}, b::MockUnitful{T}) where T<:TNumber = isless(a.data, b.data)
# Base.isapprox(a::MockUnitful{T}, b::MockUnitful{T}; kwargs...) where T<:TNumber = isapprox(a.data, b.data; kwargs...)
Base.zero(::Type{MockUnitful{T}}) where T<:TNumber = MockUnitful(zero(T))
Base.iszero(a::MockUnitful{T}) where T<:TNumber = iszero(a.data)
Base.:+(a::MockUnitful{T}) where T<:TNumber = MockUnitful(+a.data)
Base.:+(a::MockUnitful{T}, b::MockUnitful{T}) where T<:TNumber = MockUnitful(a.data + b.data)
Base.:-(a::MockUnitful{T}) where T<:TNumber = MockUnitful(-a.data)
Base.:-(a::MockUnitful{T}, b::MockUnitful{T}) where T<:TNumber = MockUnitful(a.data - b.data)
Base.abs(a::MockUnitful{T}) where T<:TNumber = MockUnitful(abs(a.data))
Base.real(a::MockUnitful{T}) where T<:TNumber = MockUnitful(real(a.data))
Base.imag(a::MockUnitful{T}) where T<:TNumber = MockUnitful(imag(a.data))
Base.one(::Type{<:MockUnitful{T}}) where T = one(T)
Base.oneunit(::Type{<:MockUnitful{T}}) where T = MockUnitful(one(T))
Base.isone(a::MockUnitful{T}) where T<:TNumber = isone(a.data)
Base.:*(a::MockUnitful{T}, b::T) where T<:TNumber = MockUnitful(a.data * b)
Base.:*(a::T, b::MockUnitful{T}) where T<:TNumber = MockUnitful(a * b.data)
Base.:*(a::MockUnitful{T}, b::MockUnitful{T}) where T<:TNumber = MockUnitful(a.data * b.data)
Base.inv(a::MockUnitful{T}) where T<:TNumber = MockUnitful(inv(a.data))
Base.:/(a::MockUnitful{T}, b::T) where T<:TNumber = MockUnitful(a.data / b)
Base.:/(a::MockUnitful{T}, b::MockUnitful{T}) where T<:TNumber = MockUnitful(a.data / b.data)
Base.:\(a::T, b::MockUnitful{T}) where T<:TNumber = MockUnitful(a \ b.data)
Base.:\(a::MockUnitful{T}, b::MockUnitful{T}) where T<:TNumber = MockUnitful(a.data \ b.data)

const n=12 # Size of matrix problem to test
Random.seed!(1)

#TODO @testset for relty in (Float32, Float64, BigFloat), elty in (relty, Complex{relty})
#TODO     dd=convert(Vector{elty}, randn(n))
#TODO     vv=convert(Vector{elty}, randn(n))
#TODO     UU=convert(Matrix{elty}, randn(n,n))
#TODO     if elty <: Complex
#TODO         dd+=im*convert(Vector{elty}, randn(n))
#TODO         vv+=im*convert(Vector{elty}, randn(n))
#TODO         UU+=im*convert(Matrix{elty}, randn(n,n))
#TODO     end
#TODO     D = Diagonal(dd)
#TODO     DM = Matrix(Diagonal(dd))
#TODO 
#TODO     @testset "constructor" begin
#TODO         for x in (dd, GenericArray(dd))
#TODO             @test Diagonal(x)::Diagonal{elty,typeof(x)} == DM
#TODO             @test Diagonal(x).diag === x
#TODO             @test Diagonal{elty}(x)::Diagonal{elty,typeof(x)} == DM
#TODO             @test Diagonal{elty}(x).diag === x
#TODO             @test Diagonal{elty}(D) === D
#TODO         end
#TODO         @test eltype(Diagonal{elty}([1,2,3,4])) == elty
#TODO         @test isa(Diagonal{elty,Vector{elty}}(GenericArray([1,2,3,4])), Diagonal{elty,Vector{elty}})
#TODO         DI = Diagonal([1,2,3,4])
#TODO         @test Diagonal(DI) === DI
#TODO         @test isa(Diagonal{elty}(DI), Diagonal{elty})
#TODO         # issue #26178
#TODO         @test_throws MethodError convert(Diagonal, [1, 2, 3, 4])
#TODO     end
#TODO 
#TODO     @testset "Basic properties" begin
#TODO         @test_throws ArgumentError size(D,0)
#TODO         @test typeof(convert(Diagonal{ComplexF32},D)) <: Diagonal{ComplexF32}
#TODO         @test typeof(convert(AbstractMatrix{ComplexF32},D)) <: Diagonal{ComplexF32}
#TODO 
#TODO         @test Array(real(D)) == real(DM)
#TODO         @test Array(abs.(D)) == abs.(DM)
#TODO         @test Array(imag(D)) == imag(DM)
#TODO 
#TODO         @test parent(D) == dd
#TODO         @test D[1,1] == dd[1]
#TODO         @test D[1,2] == 0
#TODO 
#TODO         @test issymmetric(D)
#TODO         @test isdiag(D)
#TODO         @test isdiag(Diagonal([[1 0; 0 1], [1 0; 0 1]]))
#TODO         @test !isdiag(Diagonal([[1 0; 0 1], [1 0; 1 1]]))
#TODO         @test istriu(D)
#TODO         @test istriu(D, -1)
#TODO         @test !istriu(D, 1)
#TODO         @test istriu(Diagonal(zero(diag(D))), 1)
#TODO         @test istril(D)
#TODO         @test !istril(D, -1)
#TODO         @test istril(D, 1)
#TODO         @test istril(Diagonal(zero(diag(D))), -1)
#TODO         if elty <: Real
#TODO             @test ishermitian(D)
#TODO         end
#TODO     end
#TODO 
#TODO     @testset "diag" begin
#TODO         @test_throws ArgumentError diag(D,  n+1)
#TODO         @test_throws ArgumentError diag(D, -n-1)
#TODO         @test (@inferred diag(D))::typeof(dd) == dd
#TODO         @test (@inferred diag(D, 0))::typeof(dd) == dd
#TODO         @test (@inferred diag(D, 1))::typeof(dd) == zeros(elty, n-1)
#TODO         DG = Diagonal(GenericArray(dd))
#TODO         @test (@inferred diag(DG))::typeof(GenericArray(dd)) == GenericArray(dd)
#TODO         @test (@inferred diag(DG, 1))::typeof(GenericArray(dd)) == GenericArray(zeros(elty, n-1))
#TODO     end
#TODO 
#TODO 
#TODO     @testset "Simple unary functions" begin
#TODO         for op in (-,)
#TODO             @test op(D)==op(DM)
#TODO         end
#TODO 
#TODO         for func in (det, tr)
#TODO             @test func(D) ≈ func(DM) atol=n^2*eps(relty)*(1+(elty<:Complex))
#TODO         end
#TODO         if relty <: BlasFloat
#TODO             for func in (exp, cis, sinh, cosh, tanh, sech, csch, coth)
#TODO                 @test func(D) ≈ func(DM) atol=n^3*eps(relty)
#TODO             end
#TODO             @test log(Diagonal(abs.(D.diag))) ≈ log(abs.(DM)) atol=n^3*eps(relty)
#TODO         end
#TODO         if elty <: BlasComplex
#TODO             for func in (logdet, sqrt, sin, cos, tan, sec, csc, cot,
#TODO                          asin, acos, atan, asec, acsc, acot,
#TODO                          asinh, acosh, atanh, asech, acsch, acoth)
#TODO                 @test func(D) ≈ func(DM) atol=n^2*eps(relty)*2
#TODO             end
#TODO         end
#TODO     end
#TODO 
#TODO     @testset "Two-dimensional Euler formula for Diagonal" begin
#TODO         @test cis(Diagonal([π, π])) ≈ -I
#TODO     end
#TODO 
#TODO     @testset "Linear solve" begin
#TODO         for (v, U) in ((vv, UU), (view(vv, 1:n), view(UU, 1:n, 1:2)))
#TODO             @test D*v ≈ DM*v atol=n*eps(relty)*(1+(elty<:Complex))
#TODO             @test D*U ≈ DM*U atol=n^2*eps(relty)*(1+(elty<:Complex))
#TODO 
#TODO             @test transpose(U)*D ≈ transpose(U)*Array(D)
#TODO             @test U'*D ≈ U'*Array(D)
#TODO 
#TODO             if relty != BigFloat
#TODO                 atol_two = 2n^2 * eps(relty) * (1 + (elty <: Complex))
#TODO                 atol_three = 2n^3 * eps(relty) * (1 + (elty <: Complex))
#TODO                 @test D\v ≈ DM\v atol=atol_two
#TODO                 @test D\U ≈ DM\U atol=atol_three
#TODO                 @test ldiv!(D, copy(v)) ≈ DM\v atol=atol_two
#TODO                 @test ldiv!(transpose(D), copy(v)) ≈ DM\v atol=atol_two
#TODO                 @test ldiv!(adjoint(conj(D)), copy(v)) ≈ DM\v atol=atol_two
#TODO                 @test ldiv!(D, copy(U)) ≈ DM\U atol=atol_three
#TODO                 @test ldiv!(transpose(D), copy(U)) ≈ DM\U atol=atol_three
#TODO                 @test ldiv!(adjoint(conj(D)), copy(U)) ≈ DM\U atol=atol_three
#TODO                 # this method tests AbstractMatrix/AbstractVec for second arg
#TODO                 Usym_bad = Symmetric(ones(elty, n+1, n+1))
#TODO                 @test_throws DimensionMismatch ldiv!(D, copy(Usym_bad))
#TODO 
#TODO                 @test ldiv!(zero(v), D, copy(v)) ≈ DM\v atol=atol_two
#TODO                 @test ldiv!(zero(v), transpose(D), copy(v)) ≈ DM\v atol=atol_two
#TODO                 @test ldiv!(zero(v), adjoint(conj(D)), copy(v)) ≈ DM\v atol=atol_two
#TODO                 @test ldiv!(zero(U), D, copy(U)) ≈ DM\U atol=atol_three
#TODO                 @test ldiv!(zero(U), transpose(D), copy(U)) ≈ DM\U atol=atol_three
#TODO                 @test ldiv!(zero(U), adjoint(conj(D)), copy(U)) ≈ DM\U atol=atol_three
#TODO 
#TODO                 Uc = copy(U')
#TODO                 target = rmul!(Uc, Diagonal(inv.(D.diag)))
#TODO                 @test rdiv!(Uc, D) ≈ target atol=atol_three
#TODO                 @test_throws DimensionMismatch rdiv!(Matrix{elty}(I, n-1, n-1), D)
#TODO                 @test_throws SingularException rdiv!(Uc, Diagonal(fill!(similar(D.diag), 0)))
#TODO                 @test rdiv!(Uc, transpose(D)) ≈ target atol=atol_three
#TODO                 @test rdiv!(Uc, adjoint(conj(D))) ≈ target atol=atol_three
#TODO                 @test ldiv!(D, Matrix{eltype(D)}(I, size(D))) ≈ D \ Matrix{eltype(D)}(I, size(D)) atol=atol_three
#TODO                 @test_throws DimensionMismatch ldiv!(D, fill(elty(1), n + 1))
#TODO                 @test_throws SingularException ldiv!(Diagonal(zeros(relty, n)), copy(v))
#TODO                 b = rand(elty, n, n)
#TODO                 @test ldiv!(D, copy(b)) ≈ Array(D)\Array(b)
#TODO                 @test_throws SingularException ldiv!(Diagonal(zeros(elty, n)), copy(b))
#TODO                 b = view(rand(elty, n), Vector(1:n))
#TODO                 b2 = copy(b)
#TODO                 c = ldiv!(D, b)
#TODO                 d = Array(D)\b2
#TODO                 @test c ≈ d
#TODO                 @test_throws SingularException ldiv!(Diagonal(zeros(elty, n)), b)
#TODO                 b = rand(elty, n+1, n+1)
#TODO                 @test_throws DimensionMismatch ldiv!(D, copy(b))
#TODO                 b = view(rand(elty, n+1), Vector(1:n+1))
#TODO                 @test_throws DimensionMismatch ldiv!(D, b)
#TODO             end
#TODO         end
#TODO     end
#TODO     d = convert(Vector{elty}, randn(n))
#TODO     D2 = Diagonal(d)
#TODO     DM2= Matrix(Diagonal(d))
#TODO     @testset "Binary operations" begin
#TODO         for op in (+, -, *)
#TODO             @test Array(op(D, D2)) ≈ op(DM, DM2)
#TODO         end
#TODO         @testset "with plain numbers" begin
#TODO             a = rand()
#TODO             @test Array(a*D) ≈ a*DM
#TODO             @test Array(D*a) ≈ DM*a
#TODO             @test Array(D/a) ≈ DM/a
#TODO             if elty <: Real
#TODO                 @test Array(abs.(D)^a) ≈ abs.(DM)^a
#TODO             else
#TODO                 @test Array(D^a) ≈ DM^a
#TODO             end
#TODO             @test Diagonal(1:100)^2 == Diagonal((1:100).^2)
#TODO             p = 3
#TODO             @test Diagonal(1:100)^p == Diagonal((1:100).^p)
#TODO             @test Diagonal(1:100)^(-1) == Diagonal(inv.(1:100))
#TODO             @test Diagonal(1:100)^2.0 == Diagonal((1:100).^2.0)
#TODO             @test Diagonal(1:100)^(2.0+0im) == Diagonal((1:100).^(2.0+0im))
#TODO         end
#TODO 
#TODO         if relty <: BlasFloat
#TODO             for b in (rand(elty,n,n), rand(elty,n))
#TODO                 @test lmul!(copy(D), copy(b)) ≈ Array(D)*Array(b)
#TODO                 @test lmul!(transpose(copy(D)), copy(b)) ≈ transpose(Array(D))*Array(b)
#TODO                 @test lmul!(adjoint(copy(D)), copy(b)) ≈ Array(D)'*Array(b)
#TODO             end
#TODO         end
#TODO 
#TODO         #a few missing mults
#TODO         bd = Bidiagonal(D2)
#TODO         @test D*transpose(D2) ≈ Array(D)*transpose(Array(D2))
#TODO         @test D2*transpose(D) ≈ Array(D2)*transpose(Array(D))
#TODO         @test D2*D' ≈ Array(D2)*Array(D)'
#TODO 
#TODO         #division of two Diagonals
#TODO         @test D/D2 ≈ Diagonal(D.diag./D2.diag)
#TODO         @test D\D2 ≈ Diagonal(D2.diag./D.diag)
#TODO 
#TODO         # QR \ Diagonal
#TODO         A = rand(elty, n, n)
#TODO         qrA = qr(A)
#TODO         @test qrA \ D ≈ A \ D
#TODO 
#TODO         # HermOrSym
#TODO         A     = rand(elty, n, n)
#TODO         Asym  = Symmetric(A + transpose(A), :U)
#TODO         Aherm = Hermitian(A + adjoint(A), :U)
#TODO         for op in (+, -)
#TODO             @test op(Asym, D) isa Symmetric
#TODO             @test Array(op(Asym, D)) ≈ Array(Symmetric(op(Array(Asym), Array(D))))
#TODO             @test op(D, Asym) isa Symmetric
#TODO             @test Array(op(D, Asym)) ≈ Array(Symmetric(op(Array(D), Array(Asym))))
#TODO             if !(elty <: Real)
#TODO                 Dr = real(D)
#TODO                 @test op(Aherm, Dr) isa Hermitian
#TODO                 @test Array(op(Aherm, Dr)) ≈ Array(Hermitian(op(Array(Aherm), Array(Dr))))
#TODO                 @test op(Dr, Aherm) isa Hermitian
#TODO                 @test Array(op(Dr, Aherm)) ≈ Array(Hermitian(op(Array(Dr), Array(Aherm))))
#TODO             end
#TODO         end
#TODO         @test Array(D*transpose(Asym)) ≈ Array(D) * Array(transpose(Asym))
#TODO         @test Array(D*adjoint(Asym)) ≈ Array(D) * Array(adjoint(Asym))
#TODO         @test Array(D*transpose(Aherm)) ≈ Array(D) * Array(transpose(Aherm))
#TODO         @test Array(D*adjoint(Aherm)) ≈ Array(D) * Array(adjoint(Aherm))
#TODO         @test Array(transpose(Asym)*transpose(D)) ≈ Array(transpose(Asym)) * Array(transpose(D))
#TODO         @test Array(transpose(D)*transpose(Asym)) ≈ Array(transpose(D)) * Array(transpose(Asym))
#TODO         @test Array(adjoint(Aherm)*adjoint(D)) ≈ Array(adjoint(Aherm)) * Array(adjoint(D))
#TODO         @test Array(adjoint(D)*adjoint(Aherm)) ≈ Array(adjoint(D)) * Array(adjoint(Aherm))
#TODO 
#TODO         # Performance specialisations for A*_mul_B!
#TODO         vvv = similar(vv)
#TODO         @test (r = Matrix(D) * vv   ; mul!(vvv, D, vv)  ≈ r ≈ vvv)
#TODO         @test (r = Matrix(D)' * vv  ; mul!(vvv, adjoint(D), vv) ≈ r ≈ vvv)
#TODO         @test (r = transpose(Matrix(D)) * vv ; mul!(vvv, transpose(D), vv) ≈ r ≈ vvv)
#TODO 
#TODO         UUU = similar(UU)
#TODO         for transformA in (identity, adjoint, transpose)
#TODO             for transformD in (identity, adjoint, transpose)
#TODO                 @test mul!(UUU, transformA(UU), transformD(D)) ≈  transformA(UU) * Matrix(transformD(D))
#TODO                 @test mul!(UUU, transformD(D), transformA(UU)) ≈  Matrix(transformD(D)) * transformA(UU)
#TODO             end
#TODO         end
#TODO 
#TODO         alpha = elty(randn())  # randn(elty) does not work with BigFloat
#TODO         beta = elty(randn())
#TODO         @test begin
#TODO             vvv = similar(vv)
#TODO             vvv .= randn(size(vvv))  # randn!(vvv) does not work with BigFloat
#TODO             r = alpha * Matrix(D) * vv + beta * vvv
#TODO             mul!(vvv, D, vv, alpha, beta)  ≈ r ≈ vvv
#TODO         end
#TODO         @test begin
#TODO             vvv = similar(vv)
#TODO             vvv .= randn(size(vvv))  # randn!(vvv) does not work with BigFloat
#TODO             r = alpha * Matrix(D)' * vv + beta * vvv
#TODO             mul!(vvv, adjoint(D), vv, alpha, beta) ≈ r ≈ vvv
#TODO         end
#TODO         @test begin
#TODO             vvv = similar(vv)
#TODO             vvv .= randn(size(vvv))  # randn!(vvv) does not work with BigFloat
#TODO             r = alpha * transpose(Matrix(D)) * vv + beta * vvv
#TODO             mul!(vvv, transpose(D), vv, alpha, beta) ≈ r ≈ vvv
#TODO         end
#TODO 
#TODO         @test begin
#TODO             UUU = similar(UU)
#TODO             UUU .= randn(size(UUU))  # randn!(UUU) does not work with BigFloat
#TODO             r = alpha * Matrix(D) * UU + beta * UUU
#TODO             mul!(UUU, D, UU, alpha, beta) ≈ r ≈ UUU
#TODO         end
#TODO         @test begin
#TODO             UUU = similar(UU)
#TODO             UUU .= randn(size(UUU))  # randn!(UUU) does not work with BigFloat
#TODO             r = alpha * Matrix(D)' * UU + beta * UUU
#TODO             mul!(UUU, adjoint(D), UU, alpha, beta) ≈ r ≈ UUU
#TODO         end
#TODO         @test begin
#TODO             UUU = similar(UU)
#TODO             UUU .= randn(size(UUU))  # randn!(UUU) does not work with BigFloat
#TODO             r = alpha * transpose(Matrix(D)) * UU + beta * UUU
#TODO             mul!(UUU, transpose(D), UU, alpha, beta) ≈ r ≈ UUU
#TODO         end
#TODO 
#TODO         # make sure that mul!(A, {Adj|Trans}(B)) works with B as a Diagonal
#TODO         VV = Array(D)
#TODO         DD = copy(D)
#TODO         r  = VV * Matrix(D)
#TODO         @test Array(rmul!(VV, DD)) ≈ r ≈ Array(D)*Array(D)
#TODO         DD = copy(D)
#TODO         r  = VV * transpose(Array(D))
#TODO         @test Array(rmul!(VV, transpose(DD))) ≈ r
#TODO         DD = copy(D)
#TODO         r  = VV * Array(D)'
#TODO         @test Array(rmul!(VV, adjoint(DD))) ≈ r
#TODO 
#TODO         # kron
#TODO         D3 = Diagonal(convert(Vector{elty}, rand(n÷2)))
#TODO         DM3= Matrix(D3)
#TODO         @test Matrix(kron(D, D3)) ≈ kron(DM, DM3)
#TODO         M4 = rand(elty, n÷2, n÷2)
#TODO         @test kron(D3, M4) ≈ kron(DM3, M4)
#TODO         @test kron(M4, D3) ≈ kron(M4, DM3)
#TODO         X = [ones(1,1) for i in 1:2, j in 1:2]
#TODO         @test kron(I(2), X)[1,3] == zeros(1,1)
#TODO         X = [ones(2,2) for i in 1:2, j in 1:2]
#TODO         @test kron(I(2), X)[1,3] == zeros(2,2)
#TODO     end
#TODO     @testset "iszero, isone, triu, tril" begin
#TODO         Dzero = Diagonal(zeros(elty, 10))
#TODO         Done = Diagonal(ones(elty, 10))
#TODO         Dmix = Diagonal(zeros(elty, 10))
#TODO         Dmix[end,end] = one(elty)
#TODO         @test iszero(Dzero)
#TODO         @test !isone(Dzero)
#TODO         @test !iszero(Done)
#TODO         @test isone(Done)
#TODO         @test !iszero(Dmix)
#TODO         @test !isone(Dmix)
#TODO         @test istriu(D)
#TODO         @test istril(D)
#TODO         @test iszero(triu(D,1))
#TODO         @test triu(D,0)  == D
#TODO         @test triu(D,-1) == D
#TODO         @test tril(D,1)  == D
#TODO         @test iszero(tril(D,-1))
#TODO         @test tril(D,0)  == D
#TODO         @test_throws ArgumentError tril(D, -n - 2)
#TODO         @test_throws ArgumentError tril(D, n)
#TODO         @test_throws ArgumentError triu(D, -n)
#TODO         @test_throws ArgumentError triu(D, n + 2)
#TODO     end
#TODO 
#TODO     # factorize
#TODO     @test factorize(D) == D
#TODO 
#TODO     @testset "Eigensystem" begin
#TODO         eigD = eigen(D)
#TODO         @test Diagonal(eigD.values) == D
#TODO         @test eigD.vectors == Matrix(I, size(D))
#TODO         eigsortD = eigen(D, sortby=LinearAlgebra.eigsortby)
#TODO         @test eigsortD.values !== D.diag
#TODO         @test eigsortD.values == sort(D.diag, by=LinearAlgebra.eigsortby)
#TODO         @test Matrix(eigsortD) == D
#TODO     end
#TODO 
#TODO     @testset "ldiv" begin
#TODO         v = rand(n + 1)
#TODO         @test_throws DimensionMismatch D\v
#TODO         v = rand(n)
#TODO         @test D\v ≈ DM\v
#TODO         V = rand(n + 1, n)
#TODO         @test_throws DimensionMismatch D\V
#TODO         V = rand(n, n)
#TODO         @test D\V ≈ DM\V
#TODO     end
#TODO 
#TODO     @testset "conj and transpose" begin
#TODO         @test transpose(D) == D
#TODO         if elty <: BlasComplex
#TODO             @test Array(conj(D)) ≈ conj(DM)
#TODO             @test adjoint(D) == conj(D)
#TODO         end
#TODO         # Translates to Ac/t_mul_B, which is specialized after issue 21286
#TODO         @test(D' * vv == conj(D) * vv)
#TODO         @test(transpose(D) * vv == D * vv)
#TODO     end
#TODO 
#TODO     # logdet and logabsdet
#TODO     if relty <: Real
#TODO         lD = Diagonal(convert(Vector{relty}, rand(n)))
#TODO         lM = Matrix(lD)
#TODO         @test logdet(lD) ≈ logdet(lM)
#TODO         d1, s1 = @inferred logabsdet(lD)
#TODO         d2, s2 = logabsdet(lM)
#TODO         @test d1 ≈ d2
#TODO         @test s1 == s2
#TODO         @test logdet(Diagonal(relty[-1,-2])) ≈ log(2)
#TODO         @test_throws DomainError logdet(Diagonal(relty[-1,-2,-3]))
#TODO     end
#TODO 
#TODO     @testset "similar" begin
#TODO         @test isa(similar(D), Diagonal{elty})
#TODO         @test isa(similar(D, Int), Diagonal{Int})
#TODO         @test isa(similar(D, (3,2)), Matrix{elty})
#TODO         @test isa(similar(D, Int, (3,2)), Matrix{Int})
#TODO     end
#TODO 
#TODO     # Issue number 10036
#TODO     # make sure issymmetric/ishermitian work for
#TODO     # non-real diagonal matrices
#TODO     @testset "issymmetric/hermitian for complex Diagonal" begin
#TODO         @test issymmetric(D2)
#TODO         @test ishermitian(D2)
#TODO         if elty <: Complex
#TODO             dc = d .+ elty(1im)
#TODO             D3 = Diagonal(dc)
#TODO             @test issymmetric(D3)
#TODO             @test !ishermitian(D3)
#TODO         end
#TODO     end
#TODO 
#TODO     @testset "svd (#11120/#11247)" begin
#TODO         U, s, V = svd(D)
#TODO         @test (U*Diagonal(s))*V' ≈ D
#TODO         @test svdvals(D) == s
#TODO         @test svd(D).V == V
#TODO     end
#TODO 
#TODO     @testset "svd/eigen with Diagonal{Furlong}" begin
#TODO         Du = Furlong.(D)
#TODO         @test Du isa Diagonal{<:Furlong{1}}
#TODO         F = svd(Du)
#TODO         U, s, V = F
#TODO         @test map(x -> x.val, Matrix(F)) ≈ map(x -> x.val, Du)
#TODO         @test svdvals(Du) == s
#TODO         @test U isa AbstractMatrix{<:Furlong{0}}
#TODO         @test V isa AbstractMatrix{<:Furlong{0}}
#TODO         @test s isa AbstractVector{<:Furlong{1}}
#TODO         E = eigen(Du)
#TODO         vals, vecs = E
#TODO         @test Matrix(E) == Du
#TODO         @test vals isa AbstractVector{<:Furlong{1}}
#TODO         @test vecs isa AbstractMatrix{<:Furlong{0}}
#TODO     end
#TODO end
#TODO 
#TODO @testset "rdiv! (#40887)" begin
#TODO     @test rdiv!(Matrix(Diagonal([2.0, 3.0])), Diagonal(2:3)) == Diagonal([1.0, 1.0])
#TODO     @test rdiv!(fill(3.0, 3, 3), 3.0I(3)) == ones(3,3)
#TODO end
#TODO 
#TODO @testset "kron (issue #40595)" begin
#TODO     # custom array type to test that kron on Diagonal matrices preserves types of the parents if possible
#TODO     struct KronTestArray{T, N, AT} <: AbstractArray{T, N}
#TODO         data::AT
#TODO     end
#TODO     KronTestArray(data::AbstractArray) = KronTestArray{eltype(data), ndims(data), typeof(data)}(data)
#TODO     Base.size(A::KronTestArray) = size(A.data)
#TODO     LinearAlgebra.kron(A::KronTestArray, B::KronTestArray) = KronTestArray(kron(A.data, B.data))
#TODO     Base.getindex(K::KronTestArray{<:Any,N}, i::Vararg{Int,N}) where {N} = K.data[i...]
#TODO 
#TODO     A = KronTestArray([1, 2, 3]);
#TODO     @test kron(A, A) isa KronTestArray
#TODO     Ad = Diagonal(A);
#TODO     @test kron(Ad, Ad).diag isa KronTestArray
#TODO     @test kron(Ad, Ad).diag == kron([1, 2, 3], [1, 2, 3])
#TODO end
#TODO 
#TODO @testset "svdvals and eigvals (#11120/#11247)" begin
#TODO     D = Diagonal(Matrix{Float64}[randn(3,3), randn(2,2)])
#TODO     @test sort([svdvals(D)...;], rev = true) ≈ svdvals([D.diag[1] zeros(3,2); zeros(2,3) D.diag[2]])
#TODO     @test sort([eigvals(D)...;], by=LinearAlgebra.eigsortby) ≈ eigvals([D.diag[1] zeros(3,2); zeros(2,3) D.diag[2]])
#TODO end
#TODO 
#TODO @testset "eigvals should return a copy of the diagonal" begin
#TODO     D = Diagonal([1, 2, 3])
#TODO     lam = eigvals(D)
#TODO     D[3,3] = 4 # should not affect lam
#TODO     @test lam == [1, 2, 3]
#TODO end
#TODO 
#TODO @testset "eigmin (#27847)" begin
#TODO     for _ in 1:100
#TODO         d = randn(rand(1:10))
#TODO         D = Diagonal(d)
#TODO         @test eigmin(D) == minimum(d)
#TODO     end
#TODO end
#TODO 
#TODO @testset "isposdef" begin
#TODO     @test isposdef(Diagonal(1.0 .+ rand(n)))
#TODO     @test !isposdef(Diagonal(-1.0 * rand(n)))
#TODO     @test isposdef(Diagonal(complex(1.0, 0.0) .+ rand(n)))
#TODO     @test !isposdef(Diagonal(complex(1.0, 1.0) .+ rand(n)))
#TODO     @test isposdef(Diagonal([[1 0; 0 1], [1 0; 0 1]]))
#TODO     @test !isposdef(Diagonal([[1 0; 0 1], [1 0; 1 1]]))
#TODO end
#TODO 
#TODO @testset "getindex" begin
#TODO     d = randn(n)
#TODO     D = Diagonal(d)
#TODO     # getindex bounds checking
#TODO     @test_throws BoundsError D[0, 0]
#TODO     @test_throws BoundsError D[-1, -2]
#TODO     @test_throws BoundsError D[n, n + 1]
#TODO     @test_throws BoundsError D[n + 1, n]
#TODO     @test_throws BoundsError D[n + 1, n + 1]
#TODO     # getindex on and off the diagonal
#TODO     for i in 1:n, j in 1:n
#TODO         @test D[i, j] == (i == j ? d[i] : 0)
#TODO     end
#TODO end
#TODO 
#TODO @testset "setindex!" begin
#TODO     d = randn(n)
#TODO     D = Diagonal(d)
#TODO     # setindex! bounds checking
#TODO     @test_throws BoundsError D[0, 0] = 0
#TODO     @test_throws BoundsError D[-1 , -2] = 0
#TODO     @test_throws BoundsError D[n, n + 1] = 0
#TODO     @test_throws BoundsError D[n + 1, n] = 0
#TODO     @test_throws BoundsError D[n + 1, n + 1] = 0
#TODO     for i in 1:n, j in 1:n
#TODO         if i == j
#TODO             # setindex on! the diagonal
#TODO             @test ((D[i, j] = i) == i; D[i, j] == i)
#TODO         else
#TODO             # setindex! off the diagonal
#TODO             @test ((D[i, j] = 0) == 0; iszero(D[i, j]))
#TODO             @test_throws ArgumentError D[i, j] = 1
#TODO         end
#TODO     end
#TODO end

@testset "inverse" begin
    for d in [randn(n), Int[], [1, 2, 3], [1im, 2im, 3im], [1//1, 2//1, 3//1], [1+1im//1, 2//1, 3im//1],
              MockUnitful.(randn(n))]
        D = Diagonal(d)
        @test inv(D) ≈ inv(Array(D))
    end
    @test_throws SingularException inv(Diagonal(zeros(n)))
    @test_throws SingularException inv(Diagonal([0, 1, 2]))
    @test_throws SingularException inv(Diagonal([0im, 1im, 2im]))
end

@testset "pseudoinverse" begin
    for d in [randn(n), zeros(n), Int[], [0, 2, 0.003], [0im, 1+2im, 0.003im],
              [0//1, 2//1, 3//100], [0//1, 1//1+2im, 3im//100], MockUnitful.(randn(n))]
        D = Diagonal(d)
        @test pinv(D) ≈ pinv(Array(D))
        @test pinv(D, 1.0e-2) ≈ pinv(Array(D), 1.0e-2)
    end
end

#TODO # allow construct from range
#TODO @test all(Diagonal(range(1, stop=3, length=3)) .== Diagonal([1.0,2.0,3.0]))
#TODO 
#TODO # Issue 12803
#TODO for t in (Float32, Float64, Int, ComplexF64, Rational{Int})
#TODO     @test Diagonal(Matrix{t}[fill(t(1), 2, 2), fill(t(1), 3, 3)])[2,1] == zeros(t, 3, 2)
#TODO end
#TODO 
#TODO # Issue 15401
#TODO @test Matrix(1.0I, 5, 5) \ Diagonal(fill(1.,5)) == Matrix(I, 5, 5)
#TODO 
#TODO @testset "Triangular and Diagonal" begin
#TODO     function _test_matrix(type)
#TODO         if type == Int
#TODO             return rand(1:9, 5, 5)
#TODO         else
#TODO             return randn(type, 5, 5)
#TODO         end
#TODO     end
#TODO     types = (Float64, Int, ComplexF64)
#TODO     for ta in types
#TODO         D = Diagonal(_test_matrix(ta))
#TODO         for tb in types
#TODO             B = _test_matrix(tb)
#TODO             Tmats = (LowerTriangular(B), UnitLowerTriangular(B), UpperTriangular(B), UnitUpperTriangular(B))
#TODO             restypes = (LowerTriangular, LowerTriangular, UpperTriangular, UpperTriangular)
#TODO             for (T, rtype) in zip(Tmats, restypes)
#TODO                 adjtype = (rtype == LowerTriangular) ? UpperTriangular : LowerTriangular
#TODO 
#TODO                 # Triangular * Diagonal
#TODO                 R = T * D
#TODO                 @test R ≈ Array(T) * Array(D)
#TODO                 @test isa(R, rtype)
#TODO 
#TODO                 # Diagonal * Triangular
#TODO                 R = D * T
#TODO                 @test R ≈ Array(D) * Array(T)
#TODO                 @test isa(R, rtype)
#TODO 
#TODO                 # Adjoint of Triangular * Diagonal
#TODO                 R = T' * D
#TODO                 @test R ≈ Array(T)' * Array(D)
#TODO                 @test isa(R, adjtype)
#TODO 
#TODO                 # Diagonal * Adjoint of Triangular
#TODO                 R = D * T'
#TODO                 @test R ≈ Array(D) * Array(T)'
#TODO                 @test isa(R, adjtype)
#TODO 
#TODO                 # Transpose of Triangular * Diagonal
#TODO                 R = transpose(T) * D
#TODO                 @test R ≈ transpose(Array(T)) * Array(D)
#TODO                 @test isa(R, adjtype)
#TODO 
#TODO                 # Diagonal * Transpose of Triangular
#TODO                 R = D * transpose(T)
#TODO                 @test R ≈ Array(D) * transpose(Array(T))
#TODO                 @test isa(R, adjtype)
#TODO             end
#TODO         end
#TODO     end
#TODO end
#TODO 
#TODO let D1 = Diagonal(rand(5)), D2 = Diagonal(rand(5))
#TODO     @test LinearAlgebra.rmul!(copy(D1),D2) == D1*D2
#TODO     @test LinearAlgebra.lmul!(D1,copy(D2)) == D1*D2
#TODO     @test LinearAlgebra.rmul!(copy(D1),transpose(D2)) == D1*transpose(D2)
#TODO     @test LinearAlgebra.lmul!(transpose(D1),copy(D2)) == transpose(D1)*D2
#TODO     @test LinearAlgebra.rmul!(copy(D1),adjoint(D2)) == D1*adjoint(D2)
#TODO     @test LinearAlgebra.lmul!(adjoint(D1),copy(D2)) == adjoint(D1)*D2
#TODO end
#TODO 
#TODO @testset "multiplication of a Diagonal with a Matrix" begin
#TODO     A = collect(reshape(1:8, 4, 2));
#TODO     B = BigFloat.(A);
#TODO     DL = Diagonal(collect(axes(A, 1)));
#TODO     DR = Diagonal(Float16.(collect(axes(A, 2))));
#TODO 
#TODO     @test DL * A == collect(DL) * A
#TODO     @test A * DR == A * collect(DR)
#TODO     @test DL * B == collect(DL) * B
#TODO     @test B * DR == B * collect(DR)
#TODO 
#TODO     A = reshape([ones(2,2), ones(2,2)*2, ones(2,2)*3, ones(2,2)*4], 2, 2)
#TODO     Ac = collect(A)
#TODO     D = Diagonal([collect(reshape(1:4, 2, 2)), collect(reshape(5:8, 2, 2))])
#TODO     Dc = collect(D)
#TODO     @test A * D == Ac * Dc
#TODO     @test D * A == Dc * Ac
#TODO     @test D * D == Dc * Dc
#TODO 
#TODO     AS = similar(A)
#TODO     mul!(AS, A, D, true, false)
#TODO     @test AS == A * D
#TODO 
#TODO     D2 = similar(D)
#TODO     mul!(D2, D, D)
#TODO     @test D2 == D * D
#TODO 
#TODO     copyto!(D2, D)
#TODO     lmul!(D, D2)
#TODO     @test D2 == D * D
#TODO     copyto!(D2, D)
#TODO     rmul!(D2, D)
#TODO     @test D2 == D * D
#TODO end
#TODO 
#TODO @testset "multiplication of QR Q-factor and Diagonal (#16615 spot test)" begin
#TODO     D = Diagonal(randn(5))
#TODO     Q = qr(randn(5, 5)).Q
#TODO     @test D * Q' == Array(D) * Q'
#TODO     Q = qr(randn(5, 5), ColumnNorm()).Q
#TODO     @test_throws ArgumentError lmul!(Q, D)
#TODO end
#TODO 
#TODO @testset "block diagonal matrices" begin
#TODO     D = Diagonal([[1 2; 3 4], [1 2; 3 4]])
#TODO     Dherm = Diagonal([[1 1+im; 1-im 1], [1 1+im; 1-im 1]])
#TODO     Dsym = Diagonal([[1 1+im; 1+im 1], [1 1+im; 1+im 1]])
#TODO     @test adjoint(D) == Diagonal([[1 3; 2 4], [1 3; 2 4]])
#TODO     @test transpose(D) == Diagonal([[1 3; 2 4], [1 3; 2 4]])
#TODO     @test adjoint(Dherm) == Dherm
#TODO     @test transpose(Dherm) == Diagonal([[1 1-im; 1+im 1], [1 1-im; 1+im 1]])
#TODO     @test adjoint(Dsym) == Diagonal([[1 1-im; 1-im 1], [1 1-im; 1-im 1]])
#TODO     @test transpose(Dsym) == Dsym
#TODO 
#TODO     v = [[1, 2], [3, 4]]
#TODO     @test Dherm' * v == Dherm * v
#TODO     @test transpose(D) * v == [[7, 10], [15, 22]]
#TODO 
#TODO     @test issymmetric(D) == false
#TODO     @test issymmetric(Dherm) == false
#TODO     @test issymmetric(Dsym) == true
#TODO 
#TODO     @test ishermitian(D) == false
#TODO     @test ishermitian(Dherm) == true
#TODO     @test ishermitian(Dsym) == false
#TODO 
#TODO     @test exp(D) == Diagonal([exp([1 2; 3 4]), exp([1 2; 3 4])])
#TODO     @test cis(D) == Diagonal([cis([1 2; 3 4]), cis([1 2; 3 4])])
#TODO     @test log(D) == Diagonal([log([1 2; 3 4]), log([1 2; 3 4])])
#TODO     @test sqrt(D) == Diagonal([sqrt([1 2; 3 4]), sqrt([1 2; 3 4])])
#TODO 
#TODO     @test tr(D) == 10
#TODO     @test det(D) == 4
#TODO end
#TODO 
#TODO @testset "linear solve for block diagonal matrices" begin
#TODO     D = Diagonal([rand(2,2) for _ in 1:5])
#TODO     b = [rand(2,2) for _ in 1:5]
#TODO     B = [rand(2,2) for _ in 1:5, _ in 1:5]
#TODO     @test ldiv!(D, copy(b)) ≈ Diagonal(inv.(D.diag)) * b
#TODO     @test ldiv!(D, copy(B)) ≈ Diagonal(inv.(D.diag)) * B
#TODO     @test rdiv!(copy(B), D) ≈ B * Diagonal(inv.(D.diag))
#TODO end
#TODO 
#TODO @testset "multiplication with Symmetric/Hermitian" begin
#TODO     for T in (Float64, ComplexF64)
#TODO         D = Diagonal(randn(T, n))
#TODO         A = randn(T, n, n); A = A'A
#TODO         S = Symmetric(A)
#TODO         H = Hermitian(A)
#TODO         for (transform1, transform2) in ((identity,  identity),
#TODO                 (identity,  adjoint  ), (adjoint,   identity ), (adjoint,   adjoint  ),
#TODO                 (identity,  transpose), (transpose, identity ), (transpose, transpose) )
#TODO             @test *(transform1(D), transform2(S)) ≈ *(transform1(Matrix(D)), transform2(Matrix(S)))
#TODO             @test *(transform1(D), transform2(H)) ≈ *(transform1(Matrix(D)), transform2(Matrix(H)))
#TODO             @test *(transform1(S), transform2(D)) ≈ *(transform1(Matrix(S)), transform2(Matrix(D)))
#TODO             @test *(transform1(S), transform2(H)) ≈ *(transform1(Matrix(S)), transform2(Matrix(H)))
#TODO         end
#TODO     end
#TODO end
#TODO 
#TODO @testset "multiplication of transposes of Diagonal (#22428)" begin
#TODO     for T in (Float64, ComplexF64)
#TODO         D = Diagonal(randn(T, 5, 5))
#TODO         B = Diagonal(randn(T, 5, 5))
#TODO         DD = Diagonal([randn(T, 2, 2), rand(T, 2, 2)])
#TODO         BB = Diagonal([randn(T, 2, 2), rand(T, 2, 2)])
#TODO         fullDD = copyto!(Matrix{Matrix{T}}(undef, 2, 2), DD)
#TODO         fullBB = copyto!(Matrix{Matrix{T}}(undef, 2, 2), BB)
#TODO         for (transform1, transform2) in ((identity,  identity),
#TODO                 (identity,  adjoint  ), (adjoint,   identity ), (adjoint,   adjoint  ),
#TODO                 (identity,  transpose), (transpose, identity ), (transpose, transpose))
#TODO             @test *(transform1(D), transform2(B))::typeof(D) ≈ *(transform1(Matrix(D)), transform2(Matrix(B))) atol=2 * eps()
#TODO             @test *(transform1(DD), transform2(BB))::typeof(DD) == *(transform1(fullDD), transform2(fullBB))
#TODO         end
#TODO         M = randn(T, 5, 5)
#TODO         MM = [randn(T, 2, 2) for _ in 1:2, _ in 1:2]
#TODO         for transform in (identity, adjoint, transpose)
#TODO             @test lmul!(transform(D), copy(M)) ≈ *(transform(Matrix(D)), M)
#TODO             @test rmul!(copy(M), transform(D)) ≈ *(M, transform(Matrix(D)))
#TODO             @test lmul!(transform(DD), copy(MM)) ≈ *(transform(fullDD), MM)
#TODO             @test rmul!(copy(MM), transform(DD)) ≈ *(MM, transform(fullDD))
#TODO         end
#TODO     end
#TODO end
#TODO 
#TODO @testset "Diagonal of adjoint/transpose vectors (#23649)" begin
#TODO     @test Diagonal(adjoint([1, 2, 3])) == Diagonal([1 2 3])
#TODO     @test Diagonal(transpose([1, 2, 3])) == Diagonal([1 2 3])
#TODO end
#TODO 
#TODO @testset "Multiplication with adjoint and transpose vectors (#26863)" begin
#TODO     x = collect(1:2)
#TODO     xt = transpose(x)
#TODO     A = reshape([[1 2; 3 4], zeros(Int,2,2), zeros(Int, 2, 2), [5 6; 7 8]], 2, 2)
#TODO     D = Diagonal(A)
#TODO     @test x'*D == x'*A == collect(x')*D == collect(x')*A
#TODO     @test xt*D == xt*A == collect(xt)*D == collect(xt)*A
#TODO     outadjxD = similar(x'*D); outtrxD = similar(xt*D);
#TODO     mul!(outadjxD, x', D)
#TODO     @test outadjxD == x'*D
#TODO     mul!(outtrxD, xt, D)
#TODO     @test outtrxD == xt*D
#TODO 
#TODO     D1 = Diagonal([[1 2; 3 4]])
#TODO     @test D1 * x' == D1 * collect(x') == collect(D1) * collect(x')
#TODO     @test D1 * xt == D1 * collect(xt) == collect(D1) * collect(xt)
#TODO     outD1adjx = similar(D1 * x'); outD1trx = similar(D1 * xt);
#TODO     mul!(outadjxD, D1, x')
#TODO     @test outadjxD == D1*x'
#TODO     mul!(outtrxD, D1, xt)
#TODO     @test outtrxD == D1*xt
#TODO 
#TODO     y = [x, x]
#TODO     yt = transpose(y)
#TODO     @test y'*D*y == (y'*D)*y == (y'*A)*y
#TODO     @test yt*D*y == (yt*D)*y == (yt*A)*y
#TODO     outadjyD = similar(y'*D); outtryD = similar(yt*D);
#TODO     outadjyD2 = similar(collect(y'*D)); outtryD2 = similar(collect(yt*D));
#TODO     mul!(outadjyD, y', D)
#TODO     mul!(outadjyD2, y', D)
#TODO     @test outadjyD == outadjyD2 == y'*D
#TODO     mul!(outtryD, yt, D)
#TODO     mul!(outtryD2, yt, D)
#TODO     @test outtryD == outtryD2 == yt*D
#TODO end
#TODO 
#TODO @testset "Multiplication of single element Diagonal (#36746, #40726)" begin
#TODO     @test_throws DimensionMismatch Diagonal(randn(1)) * randn(5)
#TODO     @test_throws DimensionMismatch Diagonal(randn(1)) * Diagonal(randn(3, 3))
#TODO     A = [1 0; 0 2]
#TODO     v = [3, 4]
#TODO     @test Diagonal(A) * v == A * v
#TODO     @test Diagonal(A) * Diagonal(A) == A * A
#TODO     @test_throws DimensionMismatch [1 0;0 1] * Diagonal([2 3])   # Issue #40726
#TODO     @test_throws DimensionMismatch lmul!(Diagonal([1]), [1,2,3]) # nearby
#TODO end
#TODO 
#TODO @testset "Multiplication of a Diagonal with an OffsetArray" begin
#TODO     # Offset indices should throw
#TODO     D = Diagonal(1:4)
#TODO     A = OffsetArray(rand(4,4), 2, 2)
#TODO     @test_throws ArgumentError D * A
#TODO     @test_throws ArgumentError A * D
#TODO     @test_throws ArgumentError mul!(similar(A, size(A)), A, D)
#TODO     @test_throws ArgumentError mul!(similar(A, size(A)), D, A)
#TODO end
#TODO 
#TODO @testset "Triangular division by Diagonal #27989" begin
#TODO     K = 5
#TODO     for elty in (Float32, Float64, ComplexF32, ComplexF64)
#TODO         U = UpperTriangular(randn(elty, K, K))
#TODO         L = LowerTriangular(randn(elty, K, K))
#TODO         D = Diagonal(randn(elty, K))
#TODO         @test (U / D)::UpperTriangular{elty} == UpperTriangular(Matrix(U) / Matrix(D))
#TODO         @test (L / D)::LowerTriangular{elty} == LowerTriangular(Matrix(L) / Matrix(D))
#TODO         @test (D \ U)::UpperTriangular{elty} == UpperTriangular(Matrix(D) \ Matrix(U))
#TODO         @test (D \ L)::LowerTriangular{elty} == LowerTriangular(Matrix(D) \ Matrix(L))
#TODO     end
#TODO end
#TODO 
#TODO @testset "(Sym)Tridiagonal division by Diagonal" begin
#TODO     for K in (5, 1), elty in (Float64, ComplexF32), overlength in (1, 0)
#TODO         S = SymTridiagonal(randn(elty, K), randn(elty, K-overlength))
#TODO         T = Tridiagonal(randn(elty, K-1), randn(elty, K), randn(elty, K-1))
#TODO         D = Diagonal(randn(elty, K))
#TODO         D0 = Diagonal(zeros(elty, K))
#TODO         @test (D \ S)::Tridiagonal{elty} == Tridiagonal(Matrix(D) \ Matrix(S))
#TODO         @test (D \ T)::Tridiagonal{elty} == Tridiagonal(Matrix(D) \ Matrix(T))
#TODO         @test (S / D)::Tridiagonal{elty} == Tridiagonal(Matrix(S) / Matrix(D))
#TODO         @test (T / D)::Tridiagonal{elty} == Tridiagonal(Matrix(T) / Matrix(D))
#TODO         @test_throws SingularException D0 \ S
#TODO         @test_throws SingularException D0 \ T
#TODO         @test_throws SingularException S / D0
#TODO         @test_throws SingularException T / D0
#TODO     end
#TODO     # 0-length case
#TODO     S = SymTridiagonal(Float64[], Float64[])
#TODO     T = Tridiagonal(Float64[], Float64[], Float64[])
#TODO     D = Diagonal(Float64[])
#TODO     @test (D \ S)::Tridiagonal{Float64} == T
#TODO     @test (D \ T)::Tridiagonal{Float64} == T
#TODO     @test (S / D)::Tridiagonal{Float64} == T
#TODO     @test (T / D)::Tridiagonal{Float64} == T
#TODO     # matrix eltype case
#TODO     K = 5
#TODO     for elty in (Float64, ComplexF32), overlength in (1, 0)
#TODO         S = SymTridiagonal([rand(elty, 2, 2) for _ in 1:K], [rand(elty, 2, 2) for _ in 1:K-overlength])
#TODO         T = Tridiagonal([rand(elty, 2, 2) for _ in 1:K-1], [rand(elty, 2, 2) for _ in 1:K], [rand(elty, 2, 2) for _ in 1:K-1])
#TODO         D = Diagonal(randn(elty, K))
#TODO         SM = fill(zeros(elty, 2, 2), K, K)
#TODO         TM = copy(SM)
#TODO         SM[1,1] = S[1,1]; TM[1,1] = T[1,1]
#TODO         for j in 2:K
#TODO             SM[j,j-1] = S[j,j-1]; SM[j,j] = S[j,j]; SM[j-1,j] = S[j-1,j]
#TODO             TM[j,j-1] = T[j,j-1]; TM[j,j] = T[j,j]; TM[j-1,j] = T[j-1,j]
#TODO         end
#TODO         for (M, Mm) in ((S, SM), (T, TM))
#TODO             DS = D \ M
#TODO             @test DS isa Tridiagonal
#TODO             DM = D \ Mm
#TODO             for i in -1:1; @test diag(DS, i) ≈ diag(DM, i) end
#TODO             DS = M / D
#TODO             @test DS isa Tridiagonal
#TODO             DM = Mm / D
#TODO             for i in -1:1; @test diag(DS, i) ≈ diag(DM, i) end
#TODO         end
#TODO     end
#TODO     # eltype promotion case
#TODO     S = SymTridiagonal(rand(-20:20, K), rand(-20:20, K-1))
#TODO     T = Tridiagonal(rand(-20:20, K-1), rand(-20:20, K), rand(-20:20, K-1))
#TODO     D = Diagonal(rand(1:20, K))
#TODO     @test (D \ S)::Tridiagonal{Float64} == Tridiagonal(Matrix(D) \ Matrix(S))
#TODO     @test (D \ T)::Tridiagonal{Float64} == Tridiagonal(Matrix(D) \ Matrix(T))
#TODO     @test (S / D)::Tridiagonal{Float64} == Tridiagonal(Matrix(S) / Matrix(D))
#TODO     @test (T / D)::Tridiagonal{Float64} == Tridiagonal(Matrix(T) / Matrix(D))
#TODO end
#TODO 
#TODO @testset "eigenvalue sorting" begin
#TODO     D = Diagonal([0.4, 0.2, -1.3])
#TODO     @test eigvals(D) == eigen(D).values == [0.4, 0.2, -1.3] # not sorted by default
#TODO     @test eigvals(Matrix(D)) == eigen(Matrix(D)).values == [-1.3, 0.2, 0.4] # sorted even if diagonal special case is detected
#TODO     E = eigen(D, sortby=abs) # sortby keyword supported for eigen(::Diagonal)
#TODO     @test E.values == [0.2, 0.4, -1.3]
#TODO     @test E.vectors == [0 1 0; 1 0 0; 0 0 1]
#TODO end
#TODO 
#TODO @testset "sum, mapreduce" begin
#TODO     D = Diagonal([1,2,3])
#TODO     Ddense = Matrix(D)
#TODO     @test sum(D) == 6
#TODO     @test_throws ArgumentError sum(D, dims=0)
#TODO     @test sum(D, dims=1) == sum(Ddense, dims=1)
#TODO     @test sum(D, dims=2) == sum(Ddense, dims=2)
#TODO     @test sum(D, dims=3) == sum(Ddense, dims=3)
#TODO     @test typeof(sum(D, dims=1)) == typeof(sum(Ddense, dims=1))
#TODO     @test mapreduce(one, min, D, dims=1) == mapreduce(one, min, Ddense, dims=1)
#TODO     @test mapreduce(one, min, D, dims=2) == mapreduce(one, min, Ddense, dims=2)
#TODO     @test mapreduce(one, min, D, dims=3) == mapreduce(one, min, Ddense, dims=3)
#TODO     @test typeof(mapreduce(one, min, D, dims=1)) == typeof(mapreduce(one, min, Ddense, dims=1))
#TODO     @test mapreduce(zero, max, D, dims=1) == mapreduce(zero, max, Ddense, dims=1)
#TODO     @test mapreduce(zero, max, D, dims=2) == mapreduce(zero, max, Ddense, dims=2)
#TODO     @test mapreduce(zero, max, D, dims=3) == mapreduce(zero, max, Ddense, dims=3)
#TODO     @test typeof(mapreduce(zero, max, D, dims=1)) == typeof(mapreduce(zero, max, Ddense, dims=1))
#TODO 
#TODO     D = Diagonal(Int[])
#TODO     Ddense = Matrix(D)
#TODO     @test sum(D) == 0
#TODO     @test_throws ArgumentError sum(D, dims=0)
#TODO     @test sum(D, dims=1) == sum(Ddense, dims=1)
#TODO     @test sum(D, dims=2) == sum(Ddense, dims=2)
#TODO     @test sum(D, dims=3) == sum(Ddense, dims=3)
#TODO     @test typeof(sum(D, dims=1)) == typeof(sum(Ddense, dims=1))
#TODO 
#TODO     D = Diagonal(Int[2])
#TODO     Ddense = Matrix(D)
#TODO     @test sum(D) == 2
#TODO     @test_throws ArgumentError sum(D, dims=0)
#TODO     @test sum(D, dims=1) == sum(Ddense, dims=1)
#TODO     @test sum(D, dims=2) == sum(Ddense, dims=2)
#TODO     @test sum(D, dims=3) == sum(Ddense, dims=3)
#TODO     @test typeof(sum(D, dims=1)) == typeof(sum(Ddense, dims=1))
#TODO end
#TODO 
#TODO @testset "logabsdet for generic eltype" begin
#TODO     d = Any[1, -2.0, -3.0]
#TODO     D = Diagonal(d)
#TODO     d1, s1 = logabsdet(D)
#TODO     @test d1 ≈ sum(log ∘ abs, d)
#TODO     @test s1 == prod(sign, d)
#TODO end
#TODO 
#TODO @testset "Empty (#35424)" begin
#TODO     @test zeros(0)'*Diagonal(zeros(0))*zeros(0) === 0.0
#TODO     @test transpose(zeros(0))*Diagonal(zeros(Complex{Int}, 0))*zeros(0) === 0.0 + 0.0im
#TODO     @test dot(zeros(Int32, 0), Diagonal(zeros(Int, 0)), zeros(Int16, 0)) === 0
#TODO end
#TODO 
#TODO @testset "Diagonal(undef)" begin
#TODO     d = Diagonal{Float32}(undef, 2)
#TODO     @test length(d.diag) == 2
#TODO end
#TODO 
#TODO @testset "permutedims (#39447)" begin
#TODO     for D in (Diagonal(zeros(5)), Diagonal(zeros(5) .+ 1im), Diagonal([[1,2],[3,4]]))
#TODO         @test permutedims(D) === permutedims(D,(1,2)) === permutedims(D,(2,1)) === D
#TODO         @test_throws ArgumentError permutedims(D,(1,3))
#TODO     end
#TODO end
#TODO 
#TODO @testset "Inner product" begin
#TODO     A = Diagonal(rand(10) .+ im)
#TODO     B = Diagonal(rand(10) .+ im)
#TODO     @test dot(A, B) ≈ dot(Matrix(A), B)
#TODO     @test dot(A, B) ≈ dot(A, Matrix(B))
#TODO     @test dot(A, B) ≈ dot(Matrix(A), Matrix(B))
#TODO     @test dot(A, B) ≈ conj(dot(B, A))
#TODO end
#TODO 
#TODO @testset "eltype relaxation(#41015)" begin
#TODO     A = rand(3,3)
#TODO     for trans in (identity, adjoint, transpose)
#TODO         @test ldiv!(trans(I(3)), A) == A
#TODO         @test rdiv!(A, trans(I(3))) == A
#TODO     end
#TODO end
#TODO 
#TODO const BASE_TEST_PATH = joinpath(Sys.BINDIR, "..", "share", "julia", "test")
#TODO isdefined(Main, :ImmutableArrays) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "ImmutableArrays.jl"))
#TODO using .Main.ImmutableArrays
#TODO 
#TODO @testset "Conversion to AbstractArray" begin
#TODO     # tests corresponding to #34995
#TODO     d = ImmutableArray([1, 2, 3, 4])
#TODO     D = Diagonal(d)
#TODO 
#TODO     @test convert(AbstractArray{Float64}, D)::Diagonal{Float64,ImmutableArray{Float64,1,Array{Float64,1}}} == D
#TODO     @test convert(AbstractMatrix{Float64}, D)::Diagonal{Float64,ImmutableArray{Float64,1,Array{Float64,1}}} == D
#TODO end
#TODO 
#TODO @testset "divisions functionality" for elty in (Int, Float64, ComplexF64)
#TODO     B = Diagonal(rand(elty,5,5))
#TODO     x = rand(elty)
#TODO     @test \(x, B) == /(B, x)
#TODO end
#TODO 
#TODO @testset "promotion" begin
#TODO     for (v1, v2) in (([true], [1]), ([zeros(2,2)], [zeros(Int, 2,2)]))
#TODO         T = promote_type(eltype(v1), eltype(v2))
#TODO         V = promote_type(typeof(v1), typeof(v2))
#TODO         d1 = Diagonal(v1)
#TODO         d2 = Diagonal(v2)
#TODO         v = [d1, d2]
#TODO         @test (@inferred eltype(v)) == Diagonal{T, V}
#TODO     end
#TODO     # test for a type for which promote_type doesn't lead to a concrete eltype
#TODO     struct MyArrayWrapper{T,N,A<:AbstractArray{T,N}} <: AbstractArray{T,N}
#TODO        a :: A
#TODO     end
#TODO     Base.size(M::MyArrayWrapper) = size(M.a)
#TODO     Base.axes(M::MyArrayWrapper) = axes(M.a)
#TODO     Base.length(M::MyArrayWrapper) = length(M.a)
#TODO     Base.getindex(M::MyArrayWrapper, i::Int...) = M.a[i...]
#TODO     Base.setindex!(M::MyArrayWrapper, v, i::Int...) = M.a[i...] = v
#TODO     d1 = Diagonal(MyArrayWrapper(1:3))
#TODO     d2 = Diagonal(MyArrayWrapper(1.0:3.0))
#TODO     c = [d1, d2]
#TODO     @test c[1] == d1
#TODO     @test c[2] == d2
#TODO end
#TODO 
#TODO @testset "zero and one" begin
#TODO     D1 = Diagonal(rand(3))
#TODO     @test D1 + zero(D1) == D1
#TODO     @test D1 * one(D1) == D1
#TODO     @test D1 * oneunit(D1) == D1
#TODO     @test oneunit(D1) isa typeof(D1)
#TODO     D2 = Diagonal([collect(reshape(1:4, 2, 2)), collect(reshape(5:8, 2, 2))])
#TODO     @test D2 + zero(D2) == D2
#TODO     @test D2 * one(D2) == D2
#TODO     @test D2 * oneunit(D2) == D2
#TODO     @test oneunit(D2) isa typeof(D2)
#TODO     D3 = Diagonal([D2, D2]);
#TODO     @test D3 + zero(D3) == D3
#TODO     @test D3 * one(D3) == D3
#TODO     @test D3 * oneunit(D3) == D3
#TODO     @test oneunit(D3) isa typeof(D3)
#TODO end
#TODO 
#TODO @testset "AbstractTriangular" for (Tri, UTri) in ((UpperTriangular, UnitUpperTriangular), (LowerTriangular, UnitLowerTriangular))
#TODO     A = randn(4, 4)
#TODO     TriA = Tri(A)
#TODO     UTriA = UTri(A)
#TODO     D = Diagonal(1.0:4.0)
#TODO     DM = Matrix(D)
#TODO     DMF = factorize(DM)
#TODO     outTri = similar(TriA)
#TODO     out = similar(A)
#TODO     # 2 args
#TODO     for fun in (*, rmul!, rdiv!, /)
#TODO         @test fun(copy(TriA), D)::Tri == fun(Matrix(TriA), D)
#TODO         @test fun(copy(UTriA), D)::Tri == fun(Matrix(UTriA), D)
#TODO     end
#TODO     for fun in (*, lmul!, ldiv!, \)
#TODO         @test fun(D, copy(TriA))::Tri == fun(D, Matrix(TriA))
#TODO         @test fun(D, copy(UTriA))::Tri == fun(D, Matrix(UTriA))
#TODO     end
#TODO     # 3 args
#TODO     @test outTri === ldiv!(outTri, D, TriA)::Tri == ldiv!(out, D, Matrix(TriA))
#TODO     @test outTri === ldiv!(outTri, D, UTriA)::Tri == ldiv!(out, D, Matrix(UTriA))
#TODO     @test outTri === mul!(outTri, D, TriA)::Tri == mul!(out, D, Matrix(TriA))
#TODO     @test outTri === mul!(outTri, D, UTriA)::Tri == mul!(out, D, Matrix(UTriA))
#TODO     @test outTri === mul!(outTri, TriA, D)::Tri == mul!(out, Matrix(TriA), D)
#TODO     @test outTri === mul!(outTri, UTriA, D)::Tri == mul!(out, Matrix(UTriA), D)
#TODO     # 5 args
#TODO     @test outTri === mul!(outTri, D, TriA, 2, 1)::Tri == mul!(out, D, Matrix(TriA), 2, 1)
#TODO     @test outTri === mul!(outTri, D, UTriA, 2, 1)::Tri == mul!(out, D, Matrix(UTriA), 2, 1)
#TODO     @test outTri === mul!(outTri, TriA, D, 2, 1)::Tri == mul!(out, Matrix(TriA), D, 2, 1)
#TODO     @test outTri === mul!(outTri, UTriA, D, 2, 1)::Tri == mul!(out, Matrix(UTriA), D, 2, 1)
#TODO end

end # module TestDiagonal
