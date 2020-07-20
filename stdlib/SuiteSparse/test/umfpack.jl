# This file is a part of Julia. License is MIT: https://julialang.org/license

using SuiteSparse: increment!
using Serialization
using LinearAlgebra: Adjoint, Transpose, SingularException

@testset "UMFPACK wrappers" begin
    se33 = sparse(1.0I, 3, 3)
    do33 = fill(1., 3)
    @test isequal(se33 \ do33, do33)

    # based on deps/Suitesparse-4.0.2/UMFPACK/Demo/umfpack_di_demo.c

    A0 = sparse(increment!([0,4,1,1,2,2,0,1,2,3,4,4]),
                increment!([0,4,0,2,1,2,1,4,3,2,1,2]),
                [2.,1.,3.,4.,-1.,-3.,3.,6.,2.,1.,4.,2.], 5, 5)

    @testset "Core functionality for $Tv elements" for Tv in (Float64, ComplexF64)
        # We might be able to support two index sizes one day
        for Ti in Base.uniontypes(SuiteSparse.UMFPACK.UMFITypes)
            A = convert(SparseMatrixCSC{Tv,Ti}, A0)
            lua = lu(A)
            @test nnz(lua) == 18
            @test_throws ErrorException lua.Z
            L,U,p,q,Rs = lua.:(:)
            @test (Diagonal(Rs) * A)[p,q] ≈ L * U

            det(lua) ≈ det(Array(A))

            b = [8., 45., -3., 3., 19.]
            x = lua\b
            @test x ≈ float([1:5;])

            @test A*x ≈ b
            z = complex.(b)
            x = LinearAlgebra.ldiv!(lua, z)
            @test x ≈ float([1:5;])
            @test z === x
            y = similar(z)
            LinearAlgebra.ldiv!(y, lua, complex.(b))
            @test y ≈ x

            @test A*x ≈ b

            b = [8., 20., 13., 6., 17.]
            x = lua'\b
            @test x ≈ float([1:5;])

            @test A'*x ≈ b
            z = complex.(b)
            x = LinearAlgebra.ldiv!(adjoint(lua), z)
            @test x ≈ float([1:5;])
            @test x === z
            y = similar(x)
            LinearAlgebra.ldiv!(y, adjoint(lua), complex.(b))
            @test y ≈ x

            @test A'*x ≈ b
            x = transpose(lua) \ b
            @test x ≈ float([1:5;])

            @test transpose(A) * x ≈ b
            x = LinearAlgebra.ldiv!(transpose(lua), complex.(b))
            @test x ≈ float([1:5;])
            y = similar(x)
            LinearAlgebra.ldiv!(y, transpose(lua), complex.(b))
            @test y ≈ x

            @test transpose(A) * x ≈ b

            # Element promotion and type inference
            @inferred lua\fill(1, size(A, 2))
        end
    end

    @testset "More tests for complex cases" begin
        Ac0 = complex.(A0,A0)
        for Ti in Base.uniontypes(SuiteSparse.UMFPACK.UMFITypes)
            Ac = convert(SparseMatrixCSC{ComplexF64,Ti}, Ac0)
            x  = fill(1.0 + im, size(Ac,1))
            lua = lu(Ac)
            L,U,p,q,Rs = lua.:(:)
            @test (Diagonal(Rs) * Ac)[p,q] ≈ L * U
            b  = Ac*x
            @test Ac\b ≈ x
            b  = Ac'*x
            @test Ac'\b ≈ x
            b  = transpose(Ac)*x
            @test transpose(Ac)\b ≈ x
        end
    end

    @testset "Rectangular cases. elty=$elty, m=$m, n=$n" for
        elty in (Float64, ComplexF64),
            (m, n) in ((10,5), (5, 10))

        Random.seed!(30072018)
        A = sparse([1:min(m,n); rand(1:m, 10)], [1:min(m,n); rand(1:n, 10)], elty == Float64 ? randn(min(m, n) + 10) : complex.(randn(min(m, n) + 10), randn(min(m, n) + 10)))
        F = lu(A)
        L, U, p, q, Rs = F.:(:)
        @test (Diagonal(Rs) * A)[p,q] ≈ L * U
    end

    @testset "Issue #4523 - complex sparse \\" begin
        A, b = sparse((1.0 + im)I, 2, 2), fill(1., 2)
        @test A * (lu(A)\b) ≈ b

        @test det(sparse([1,3,3,1], [1,1,3,3], [1,1,1,1])) == 0
    end

    @testset "UMFPACK_ERROR_n_nonpositive" begin
        @test_throws ArgumentError lu(sparse(Int[], Int[], Float64[], 5, 0))
    end

    @testset "Issue #15099" for (Tin, Tout) in (
            (ComplexF16, ComplexF64),
            (ComplexF32, ComplexF64),
            (ComplexF64, ComplexF64),
            (Float16, Float64),
            (Float32, Float64),
            (Float64, Float64),
            (Int, Float64),
        )

        F = lu(sparse(fill(Tin(1), 1, 1)))
        L = sparse(fill(Tout(1), 1, 1))
        @test F.p == F.q == [1]
        @test F.Rs == [1.0]
        @test F.L == F.U == L
        @test F.:(:) == (L, L, [1], [1], [1.0])
    end

    @testset "BigFloat not supported" for T in (BigFloat, Complex{BigFloat})
        @test_throws ArgumentError lu(sparse(fill(T(1), 1, 1)))
    end

    @testset "size(::UmfpackLU)" begin
        m = n = 1
        F = lu(sparse(fill(1., m, n)))
        @test size(F) == (m, n)
        @test size(F, 1) == m
        @test size(F, 2) == n
        @test size(F, 3) == 1
        @test_throws ArgumentError size(F,-1)
    end

    @testset "Test aliasing" begin
        a = rand(5)
        @test_throws ArgumentError SuiteSparse.UMFPACK.solve!(a, lu(sparse(1.0I, 5, 5)), a, SuiteSparse.UMFPACK.UMFPACK_A)
        aa = complex(a)
        @test_throws ArgumentError SuiteSparse.UMFPACK.solve!(aa, lu(sparse((1.0im)I, 5, 5)), aa, SuiteSparse.UMFPACK.UMFPACK_A)
    end

    @testset "Issues #18246,18244 - lu sparse pivot" begin
        A = sparse(1.0I, 4, 4)
        A[1:2,1:2] = [-.01 -200; 200 .001]
        F = lu(A)
        @test F.p == [3 ; 4 ; 2 ; 1]
    end

    @testset "Test that A[c|t]_ldiv_B!{T<:Complex}(X::StridedMatrix{T}, lu::UmfpackLU{Float64}, B::StridedMatrix{T}) works as expected." begin
        N = 10
        p = 0.5
        A = N*I + sprand(N, N, p)
        X = zeros(ComplexF64, N, N)
        B = complex.(rand(N, N), rand(N, N))
        luA, lufA = lu(A), lu(Array(A))
        @test LinearAlgebra.ldiv!(copy(X), luA, B) ≈ LinearAlgebra.ldiv!(copy(X), lufA, B)
        @test LinearAlgebra.ldiv!(copy(X), adjoint(luA), B) ≈ LinearAlgebra.ldiv!(copy(X), adjoint(lufA), B)
        @test LinearAlgebra.ldiv!(copy(X), transpose(luA), B) ≈ LinearAlgebra.ldiv!(copy(X), transpose(lufA), B)
    end

    @testset "singular matrix" begin
        for A in sparse.((Float64[1 2; 0 0], ComplexF64[1 2; 0 0]))
            @test_throws SingularException lu(A)
            @test !issuccess(lu(A; check = false))
        end
    end

    @testset "deserialization" begin
        A  = 10*I + sprandn(10, 10, 0.4)
        F1 = lu(A)
        b  = IOBuffer()
        serialize(b, F1)
        seekstart(b)
        F2 = deserialize(b)
        for nm in (:colptr, :m, :n, :nzval, :rowval, :status)
            @test getfield(F1, nm) == getfield(F2, nm)
        end
    end

    @testset "Reuse symbolic LU factorization" begin
        A1 = sparse(increment!([0,4,1,1,2,2,0,1,2,3,4,4]),
                    increment!([0,4,0,2,1,2,1,4,3,2,1,2]),
                    [2.,1.,3.,4.,-1.,-3.,3.,9.,2.,1.,4.,2.], 5, 5)
        for Tv in (Float64, ComplexF64, Float16, Float32, ComplexF16, ComplexF32)
            for Ti in Base.uniontypes(SuiteSparse.UMFPACK.UMFITypes)
                A = convert(SparseMatrixCSC{Tv,Ti}, A0)
                B = convert(SparseMatrixCSC{Tv,Ti}, A1)
                b = Tv[8., 45., -3., 3., 19.]
                F = lu(A)
                lu!(F, B)
                @test F\b ≈ B\b ≈ Matrix(B)\b

                # singular matrix
                C = copy(B)
                C[4, 3] = Tv(0)
                F = lu(A)
                @test_throws SingularException lu!(F, C)

                # change of nonzero pattern
                D = copy(B)
                D[5, 1] = Tv(1.0)
                F = lu(A)
                @test_throws ArgumentError lu!(F, D)
            end
        end
    end

end

@testset "REPL printing of UmfpackLU" begin
    # regular matrix
    A = sparse([1, 2], [1, 2], Float64[1.0, 1.0])
    F = lu(A)
    facstring = sprint((t, s) -> show(t, "text/plain", s), F)
    lstring = sprint((t, s) -> show(t, "text/plain", s), F.L)
    ustring = sprint((t, s) -> show(t, "text/plain", s), F.U)
    @test facstring == "$(summary(F))\nL factor:\n$lstring\nU factor:\n$ustring"

    # singular matrix
    B = sparse(zeros(Float64, 2, 2))
    F = lu(B; check=false)
    facstring = sprint((t, s) -> show(t, "text/plain", s), F)
    @test facstring == "Failed factorization of type $(summary(F))"
end
