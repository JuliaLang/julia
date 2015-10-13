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
    for Ti in Base.SparseArrays.UMFPACK.UMFITypes.types
        A = convert(SparseMatrixCSC{Tv,Ti}, A0)
        lua = lufact(A)
        @test nnz(lua) == 18
        L,U,p,q,Rs = lua[:(:)]
        @test_approx_eq scale(Rs,A)[p,q] L*U

        @test_approx_eq det(lua) det(full(A))

        b = [8., 45., -3., 3., 19.]
        x = lua\b
        @test_approx_eq x float([1:5;])

        @test norm(A*x-b,1) < eps(1e4)

        b = [8., 20., 13., 6., 17.]
        x = lua'\b
        @test_approx_eq x float([1:5;])

        @test norm(A'*x-b,1) < eps(1e4)

        # Element promotion and type inference
        @inferred lua\ones(Int, size(A, 2))
    end
end

Ac0 = complex(A0,A0)
for Ti in Base.SparseArrays.UMFPACK.UMFITypes.types
    Ac = convert(SparseMatrixCSC{Complex128,Ti}, Ac0)
    lua = lufact(Ac)
    L,U,p,q,Rs = lua[:(:)]
    @test_approx_eq scale(Rs,Ac)[p,q] L*U
end

for elty in (Float64, Complex128)
    for (m, n) in ((10,5), (5, 10))
        A = sparse([1:min(m,n); rand(1:m, 10)], [1:min(m,n); rand(1:n, 10)], elty == Float64 ? randn(min(m, n) + 10) : complex(randn(min(m, n) + 10), randn(min(m, n) + 10)))
        F = lufact(A)
        L, U, p, q, Rs = F[:(:)]
        @test_approx_eq scale(Rs,A)[p,q] L*U
    end
end

#4523 - complex sparse \
x = speye(2) + im * speye(2)
@test_approx_eq (x*(lufact(x) \ ones(2))) ones(2)
