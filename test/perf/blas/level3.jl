# gemm on various size matrices

function matmultest(n, repeat)
	a = rand(n,n)
	b = similar(a)
	for ct=1:repeat
	  A_mul_B!(b, a, a)
	end
	b
end

for (testfunc, testname, longtestname) in [(gemvtest, "matmul", "matrix-matrix multiplication")]
  for (n, t, size) in [(2  , 10^6, "tiny"),
                       (2^4, 10^5, "small"),
                       (2^6, 10^4, "medium"),
                       (2^8, 10^3, "large")]
    @timeit apply(testfunc, n, t) string(testname, "_", size) string(uppercase(size[1]), size[2:end], " ", longtestname, " test")
  end
end

