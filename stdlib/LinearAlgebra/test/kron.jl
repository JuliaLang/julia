using LinearAlgebra,Test
@testset " issue ##### " begin
	x = collect(0:50)
	A = [ 0.1, x[1:5],reshape(x[1:6],2,3),reshape(x[1:6],1,6) ]
	@test kron(A...) == kron(kron(kron(A[1],A[2]),A[3]),A[4])
	A = A[[2,3,4,1]]
	@test kron(A...) == kron(kron(kron(A[1],A[2]),A[3]),A[4])
end
