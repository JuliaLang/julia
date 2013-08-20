# Real
function realeigtest(n, counts)
	A = rand(n,n)
	d = Vector{eltype(A)}
	v = similar(A)
	for ct = 1:counts
		d,v = eig(A)
	end
	d,v
end

# Symmetric
function symeigtest(n, counts)
	A = rand(n,n)
	A = A + A'
	d = Vector{eltype(A)}
	v = similar(A)
	for ct = 1:counts
		d,v = eig(A)
	end
	d,v
end

# Hermitian
function hermitianeigtest(n, counts)
	A = rand(n,n) + im*rand(n,n)
	A = A + A'
	d = Vector{eltype(A)}
	v = similar(A)
	for ct = 1:counts
		d,v = eig(A)
	end
	d,v
end

@timeit realeigtest(2, 10_000) "realeig_tiny" "Tiny real eig test"
@timeit realeigtest(16, 1_000) "realeig_small" "Small real eig test"
@timeit realeigtest(64, 100) "realeig_medium" "Medium real eig test"
@timeit realeigtest(256, 5) "realeig_large" "Large real eig test"
@timeit realeigtest(1024, 1) "realeig_huge" "Huge real eig test"

@timeit symeigtest(2, 10_000) "symeig_tiny" "Tiny symmetric eig test"
@timeit symeigtest(16, 1_000) "symeig_small" "Small symmetric eig test"
@timeit symeigtest(64, 100) "symeig_medium" "Medium symmetric eig test"
@timeit symeigtest(256, 5) "symeig_large" "Large symmetric eig test"
@timeit symeigtest(1024, 1) "symeig_huge" "Huge symmetric eig test"

@timeit hermitianeigtest(2, 10_000) "hermitianeig_tiny" "Tiny Hermitian eig test"
@timeit hermitianeigtest(16, 1_000) "hermitianeig_small" "Small Hermitian eig test"
@timeit hermitianeigtest(64, 100) "hermitianeig_medium" "Medium Hermitian eig test"
@timeit hermitianeigtest(256, 5) "hermitianeig_large" "Large Hermitian eig test"
@timeit hermitianeigtest(1024, 1) "hermitianeig_huge" "Huge Hermitian eig test"
