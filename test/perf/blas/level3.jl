# gemm on various size matrices

function matmultest(n, repeat)
	a = rand(n,n)
	b = similar(a)
	for ct=1:repeat
		A_mul_B(b, a, a)
	end
	b
end

@timeit matmultest(2, 1_000_000) "matmul_tiny" "Tiny matrix-matrix multiplication test"
@timeit matmultest(16, 100_000) "matmul_small" "Small matrix-matrix multiplication test"
@timeit matmultest(64, 10_000) "matmul_medium" "Medium matrix-matrix multiplication test"
@timeit matmultest(256, 100) "matmul_large" "Large matrix-matrix multiplication test"
