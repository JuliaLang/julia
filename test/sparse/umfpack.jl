# This file is a part of Julia. License is MIT: http://julialang.org/license

se33 = speye(3)
do33 = ones(3)
@test isequal(se33 \ do33, do33)

# based on deps/Suitesparse-4.0.2/UMFPACK/Demo/umfpack_di_demo.c

using Base.SparseArrays.UMFPACK.increment!

A0 = sparse(increment!([0,4,1,1,2,2,0,1,2,3,4,4]),
           increment!([0,4,0,2,1,2,1,4,3,2,1,2]),
           [2.,1.,3.,4.,-1.,-3.,3.,6.,2.,1.,4.,2.], 5, 5)

for Tv in (Float64, Complex128)
    for Ti in Base.uniontypes(Base.SparseArrays.UMFPACK.UMFITypes)
        A = convert(SparseMatrixCSC{Tv,Ti}, A0)
        lua = lufact(A)
        @test nnz(lua) == 18
        @test_throws KeyError lua[:Z]
        L,U,p,q,Rs = lua[:(:)]
        @test (Diagonal(Rs) * A)[p,q] ≈ L * U

        det(lua) ≈ det(Array(A))

        b = [8., 45., -3., 3., 19.]
        x = lua\b
        @test x ≈ float([1:5;])

        @test norm(A*x-b,1) < eps(1e4)
        z = complex.(b,zeros(b))
        x = Base.SparseArrays.A_ldiv_B!(lua, z)
        @test x ≈ float([1:5;])
        @test z === x
        y = similar(z)
        A_ldiv_B!(y, lua, complex.(b,zeros(b)))
        @test y ≈ x

        @test norm(A*x-b,1) < eps(1e4)

        b = [8., 20., 13., 6., 17.]
        x = lua'\b
        @test x ≈ float([1:5;])

        @test norm(A'*x-b,1) < eps(1e4)
        z = complex.(b,zeros(b))
        x = Base.SparseArrays.Ac_ldiv_B!(lua, z)
        @test x ≈ float([1:5;])
        @test x === z
        y = similar(x)
        Base.SparseArrays.Ac_ldiv_B!(y, lua, complex.(b,zeros(b)))
        @test y ≈ x

        @test norm(A'*x-b,1) < eps(1e4)
        x = lua.'\b
        @test x ≈ float([1:5;])

        @test norm(A.'*x-b,1) < eps(1e4)
        x = Base.SparseArrays.At_ldiv_B!(lua,complex.(b,zeros(b)))
        @test x ≈ float([1:5;])
        y = similar(x)
        Base.SparseArrays.At_ldiv_B!(y, lua,complex.(b,zeros(b)))
        @test y ≈ x

        @test norm(A.'*x-b,1) < eps(1e4)

        # Element promotion and type inference
        @inferred lua\ones(Int, size(A, 2))
    end
end

Ac0 = complex.(A0,A0)
for Ti in Base.uniontypes(Base.SparseArrays.UMFPACK.UMFITypes)
    Ac = convert(SparseMatrixCSC{Complex128,Ti}, Ac0)
    lua = lufact(Ac)
    L,U,p,q,Rs = lua[:(:)]
    @test (Diagonal(Rs) * Ac)[p,q] ≈ L * U
end

for elty in (Float64, Complex128)
    for (m, n) in ((10,5), (5, 10))
        A = sparse([1:min(m,n); rand(1:m, 10)], [1:min(m,n); rand(1:n, 10)], elty == Float64 ? randn(min(m, n) + 10) : complex.(randn(min(m, n) + 10), randn(min(m, n) + 10)))
        F = lufact(A)
        L, U, p, q, Rs = F[:(:)]
        @test (Diagonal(Rs) * A)[p,q] ≈ L * U
    end
end

#4523 - complex sparse \
x = speye(2) + im * speye(2)
@test (x*(lufact(x) \ ones(2))) ≈ ones(2)

@test det(sparse([1,3,3,1], [1,1,3,3], [1,1,1,1])) == 0

# UMFPACK_ERROR_n_nonpositive
@test_throws ArgumentError lufact(sparse(Int[], Int[], Float64[], 5, 0))

#15099
for (Tin, Tout) in (
        (Complex32, Complex128),
        (Complex64, Complex128),
        (Complex128, Complex128),
        (Float16, Float64),
        (Float32, Float64),
        (Float64, Float64),
        (Int, Float64),
    )

    F = lufact(sparse(ones(Tin, 1, 1)))
    L = sparse(ones(Tout, 1, 1))
    @test F[:p] == F[:q] == [1]
    @test F[:Rs] == [1.0]
    @test F[:L] == F[:U] == L
    @test F[:(:)] == (L, L, [1], [1], [1.0])
end

for T in (BigFloat, Complex{BigFloat})
    @test_throws ArgumentError lufact(sparse(ones(T, 1, 1)))
end

#size(::UmfpackLU)
let
    m = n = 1
    F = lufact(sparse(ones(m, n)))
    @test size(F) == (m, n)
    @test size(F, 1) == m
    @test size(F, 2) == n
    @test size(F, 3) == 1
    @test_throws ArgumentError size(F,-1)
end

let
    a = rand(5)
    @test_throws ArgumentError Base.SparseArrays.UMFPACK.solve!(a, lufact(speye(5,5)), a, Base.SparseArrays.UMFPACK.UMFPACK_A)
    aa = complex(a)
    @test_throws ArgumentError Base.SparseArrays.UMFPACK.solve!(aa, lufact(complex(speye(5,5))), aa, Base.SparseArrays.UMFPACK.UMFPACK_A)
end

#18246,18244-lufact sparse pivot
let A = speye(4)
    A[1:2,1:2] = [-.01 -200; 200 .001]
    F = lufact(A)
    @test F[:p] == [3 ; 4 ; 2 ; 1]
end

# Test that A[c|t]_ldiv_B!{T<:Complex}(X::StridedMatrix{T}, lu::UmfpackLU{Float64},
# B::StridedMatrix{T}) works as expected.
let N = 10, p = 0.5
    A = N*speye(N) + sprand(N, N, p)
    X = zeros(Complex{Float64}, N, N)
    B = complex.(rand(N, N), rand(N, N))
    luA, lufA = lufact(A), lufact(Array(A))
    @test A_ldiv_B!(copy(X), luA, B) ≈ A_ldiv_B!(copy(X), lufA, B)
    @test At_ldiv_B!(copy(X), luA, B) ≈ At_ldiv_B!(copy(X), lufA, B)
    @test Ac_ldiv_B!(copy(X), luA, B) ≈ Ac_ldiv_B!(copy(X), lufA, B)
end
