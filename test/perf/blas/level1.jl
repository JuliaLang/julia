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
    Base.LinAlg.BLAS.axpy!(a, x, y)
  end
  y
end

for (testfunc, testname, longtestname) in [(dottest, "dot", "dot product"),
                                           (axpytest, "axpy", "axpy")]
  for (n, t, size) in [(2, 10^6, "tiny"),
                       (2^4, 10^6, "small"),
                       (2^6, 10^6, "medium"),
                       (2^8, 10^5, "large"),
                       (2^10, 10^5, "huge")]
    @timeit apply(testfunc, n, t) string(testname, "_", size) string(uppercase(size[1]), size[2:end], " ", longtestname, " test")
  end
end

