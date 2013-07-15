# gemm on various size matrices

function matmultest(n, repeats)
	a = rand(n,n)
	b = similar(a)
	for ct=1:repeats
		b = a * a
	end
	b
end

@timeit matmultest(2, 1_000_000) "matmul_tiny"
@timeit matmultest(16, 100_000) "matmul_small"
@timeit matmultest(64, 10_000) "matmul_medium"
@timeit matmultest(256, 100) "matmul_large"