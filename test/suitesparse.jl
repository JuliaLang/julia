se33 = speye(3)
do33 = ones(3)
@test isequal(se33 \ do33, do33)

using Base.SuiteSparse

# based on deps/Suitesparse-4.0.2/UMFPACK/Demo/umfpack_di_demo.c

A = sparse(increment!([0,4,1,1,2,2,0,1,2,3,4,4]),
           increment!([0,4,0,2,1,2,1,4,3,2,1,2]),
           [2.,1.,3.,4.,-1.,-3.,3.,6.,2.,1.,4.,2.], 5, 5)
lua = lu(A)
umf_lunz(lua)
@test_approx_eq det(lua) det(full(A))

b = [8., 45., -3., 3., 19.]
x = lua\b
@test_approx_eq x float([1:5])

@test norm(A*x-b,1) < eps(1e4)

L,U,P,Q,Rs = umf_extract(lua)
@test_approx_eq diagmm(Rs,A)[P,Q] L*U

