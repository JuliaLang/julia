function gemvtest(n, repeat)
	A = rand(n,n)
	x = rand(n)
	z = similar(x)

	for ct = 1:repeat
		z = A * x
	end
	z
end


@timeit gemvtest(2, 100_000) "gemv_tiny" "Tiny matrix-vector multiplication test"
@timeit gemvtest(16, 100_000) "gemv_small" "Small matrix-vector multiplication test"
@timeit gemvtest(64, 10_000) "gemv_medium" "Medium matrix-vector multiplication test"
@timeit gemvtest(256, 1000) "gemv_large" "Large matrix-vector multiplication test"
@timeit gemvtest(1024, 100) "gemv_huge" "Huge matrix-vector multiplication test"
