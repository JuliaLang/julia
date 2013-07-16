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

@timeit dottest(2, 1_000_000) "dot_tiny"
@timeit dottest(16, 1_000_000) "dot_small"
@timeit dottest(64, 1_000_000) "dot_medium"
@timeit dottest(256, 100_000) "dot_large"
@timeit dottest(1024, 100_000) "dot_huge"

@timeit axpytest(2, 1_000_000) "axpy_tiny"
@timeit axpytest(16, 1_000_000) "axpy_small"
@timeit axpytest(64, 1_000_000) "axpy_medium"
@timeit axpytest(256, 100_000) "axpy_large"
@timeit axpytest(1024, 100_000) "axpy_huge"