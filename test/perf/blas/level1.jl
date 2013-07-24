function dottest(n, repeat)
	a = rand(n)
	b = rand(n)
	c = similar(a)
	for ct = 1:repeat
		c = dot(a,b)
	end
	c
end

function axpytest(n, repeat)
	a = rand(1)[1]
	x = rand(n)
	y = zeros(n)
	for ct = 1:repeat
		LinAlg.BLAS.axpy!(a, x, y)
	end
	y
end

@timeit dottest(2, 1_000_000) "dot_tiny" "Tiny dotproduct test"
@timeit dottest(16, 1_000_000) "dot_small" "Small dotproduct test"
@timeit dottest(64, 1_000_000) "dot_medium" "Medium dotproduct test"
@timeit dottest(256, 100_000) "dot_large" "Large dotproduct test"
@timeit dottest(1024, 100_000) "dot_huge" "Huge dotproduct test"

@timeit axpytest(2, 1_000_000) "axpy_tiny" "Tiny BLAS axpy test"
@timeit axpytest(16, 1_000_000) "axpy_small" "Small BLAS axpy test"
@timeit axpytest(64, 1_000_000) "axpy_medium" "Medium BLAS axpy test"
@timeit axpytest(256, 100_000) "axpy_large" "Large BLAS axpy test"
@timeit axpytest(1024, 100_000) "axpy_huge" "Huge BLAS axpy test"
