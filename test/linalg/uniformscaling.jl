# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

srand(123)

# Uniform scaling
@test I[1,1] == 1 # getindex
@test I[1,2] == 0 # getindex
@test I === I' # transpose
@test one(UniformScaling{Float32}) == UniformScaling(one(Float32))
@test zero(UniformScaling{Float32}) == UniformScaling(zero(Float32))
@test zero(UniformScaling(rand(Complex128))) == zero(UniformScaling{Complex128})
@test one(UniformScaling(rand(Complex128))) == one(UniformScaling{Complex128})
@test eltype(one(UniformScaling(rand(Complex128)))) == Complex128
@test -one(UniformScaling(2)) == UniformScaling(-1)

α = randn()
@test α .* UniformScaling(1.0) == UniformScaling(1.0) .* α
@test UniformScaling(α)./α == UniformScaling(1.0)
@test α + UniformScaling(1.0) == UniformScaling(1.0) + α
@test α - UniformScaling(1.0) == -(UniformScaling(1.0) - α)
@test copy(UniformScaling(one(Float64))) == UniformScaling(one(Float64))
@test sprint(show,UniformScaling(one(Float32))) == "UniformScaling{Float32}\n1.0*I"

λ = complex(randn(),randn())
J = UniformScaling(λ)
@test ndims(J) == 2
@test transpose(J) == J
@test J*eye(2) == conj(J'eye(2)) # ctranpose (and A(c)_mul_B)
@test I + I === UniformScaling(2) # +
@test inv(I) == I
@test inv(J) == UniformScaling(inv(λ))

B = bitrand(2,2)
@test B + I == B + eye(B)
@test I + B == B + eye(B)

A = randn(2,2)
@test A + I == A + eye(A)
@test I + A == A + eye(A)
@test I - I === UniformScaling(0)
@test B - I == B - eye(B)
@test I - B == eye(B) - B
@test A - I == A - eye(A)
@test I - A == eye(A) - A
@test I*J === UniformScaling(λ)
@test B*J == B*λ
@test J*B == B*λ

S = sprandn(3,3,0.5)
@test S*J == S*λ
@test J*S == S*λ
@test A*J == A*λ
@test J*A == A*λ
@test J*ones(3) == ones(3)*λ
@test λ*J === UniformScaling(λ*J.λ)
@test J*λ === UniformScaling(λ*J.λ)
@test J/I === J
@test I/A == inv(A)
@test A/I == A
@test I/λ === UniformScaling(1/λ)
@test I\J === J

T = LowerTriangular(randn(3,3))
@test T + J == full(T) + J
@test J + T == J + full(T)
@test T - J == full(T) - J
@test J - T == J - full(T)
@test T\I == inv(T)
T = LinAlg.UnitLowerTriangular(randn(3,3))
@test T + J == full(T) + J
@test J + T == J + full(T)
@test T - J == full(T) - J
@test J - T == J - full(T)
@test T\I == inv(T)
T = UpperTriangular(randn(3,3))
@test T + J == full(T) + J
@test J + T == J + full(T)
@test T - J == full(T) - J
@test J - T == J - full(T)
@test T\I == inv(T)
T = LinAlg.UnitUpperTriangular(randn(3,3))
@test T + J == full(T) + J
@test J + T == J + full(T)
@test T - J == full(T) - J
@test J - T == J - full(T)
@test T\I == inv(T)

@test I\A == A
@test A\I == inv(A)
@test λ\I === UniformScaling(1/λ)