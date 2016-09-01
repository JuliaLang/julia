# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

srand(123)

# Uniform scaling
@test I[1,1] == 1 # getindex
@test I[1,2] == 0 # getindex
@test I === I' # transpose
@test ndims(I) == 2
@test one(UniformScaling{Float32}) == UniformScaling(one(Float32))
@test zero(UniformScaling{Float32}) == UniformScaling(zero(Float32))
@test eltype(one(UniformScaling{Float32})) == Float32
@test zero(UniformScaling(rand(Complex128))) == zero(UniformScaling{Complex128})
@test one(UniformScaling(rand(Complex128))) == one(UniformScaling{Complex128})
@test eltype(one(UniformScaling(rand(Complex128)))) == Complex128
@test -one(UniformScaling(2)) == UniformScaling(-1)
@test istriu(I)
@test istril(I)
@test issymmetric(I)
@test issymmetric(UniformScaling(complex(1.0,1.0)))
@test ishermitian(I)
@test !ishermitian(UniformScaling(complex(1.0,1.0)))

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

let AA = randn(2, 2)
    for SS in (sprandn(3,3, 0.5), speye(Int, 3))
        for atype in ("Array", "SubArray")
            if atype == "Array"
                A = AA
                S = SS
            else
                A = view(AA, 1:2, 1:2)
                S = view(SS, 1:3, 1:3)
            end

            @test @inferred(A + I) == A + eye(A)
            @test @inferred(I + A) == A + eye(A)
            @test @inferred(I - I) === UniformScaling(0)
            @test @inferred(B - I) == B - eye(B)
            @test @inferred(I - B) == eye(B) - B
            @test @inferred(A - I) == A - eye(A)
            @test @inferred(I - A) == eye(A) - A
            @test @inferred(I*J) === UniformScaling(λ)
            @test @inferred(B*J) == B*λ
            @test @inferred(J*B) == B*λ
            @test @inferred(I*A) !== A # Don't alias
            @test @inferred(I*S) !== S # Don't alias
            @test @inferred(A*I) !== A # Don't alias
            @test @inferred(S*I) !== S # Don't alias

            @test @inferred(S*J) == S*λ
            @test @inferred(J*S) == S*λ
            @test @inferred(A*J) == A*λ
            @test @inferred(J*A) == A*λ
            @test @inferred(J*ones(3)) == ones(3)*λ
            @test @inferred(λ*J) === UniformScaling(λ*J.λ)
            @test @inferred(J*λ) === UniformScaling(λ*J.λ)
            @test @inferred(J/I) === J
            @test @inferred(I/A) == inv(A)
            @test @inferred(A/I) == A
            @test @inferred(I/λ) === UniformScaling(1/λ)
            @test @inferred(I\J) === J

            if atype == "Array"
                T = LowerTriangular(randn(3,3))
            else
                T = LowerTriangular(view(randn(3,3), 1:3, 1:3))
            end
            @test @inferred(T + J) == full(T) + J
            @test @inferred(J + T) == J + full(T)
            @test @inferred(T - J) == full(T) - J
            @test @inferred(J - T) == J - full(T)
            @test @inferred(T\I) == inv(T)

            if atype == "Array"
                T = LinAlg.UnitLowerTriangular(randn(3,3))
            else
                T = LinAlg.UnitLowerTriangular(view(randn(3,3), 1:3, 1:3))
            end
            @test @inferred(T + J) == full(T) + J
            @test @inferred(J + T) == J + full(T)
            @test @inferred(T - J) == full(T) - J
            @test @inferred(J - T) == J - full(T)
            @test @inferred(T\I) == inv(T)

            if atype == "Array"
                T = UpperTriangular(randn(3,3))
            else
                T = UpperTriangular(view(randn(3,3), 1:3, 1:3))
            end
            @test @inferred(T + J) == full(T) + J
            @test @inferred(J + T) == J + full(T)
            @test @inferred(T - J) == full(T) - J
            @test @inferred(J - T) == J - full(T)
            @test @inferred(T\I) == inv(T)

            if atype == "Array"
                T = LinAlg.UnitUpperTriangular(randn(3,3))
            else
                T = LinAlg.UnitUpperTriangular(view(randn(3,3), 1:3, 1:3))
            end
            @test @inferred(T + J) == full(T) + J
            @test @inferred(J + T) == J + full(T)
            @test @inferred(T - J) == full(T) - J
            @test @inferred(J - T) == J - full(T)
            @test @inferred(T\I) == inv(T)

            @test @inferred(I\A) == A
            @test @inferred(A\I) == inv(A)
            @test @inferred(λ\I) === UniformScaling(1/λ)
        end
    end
end
