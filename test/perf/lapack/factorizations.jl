function svdtest(n, iter)
    A = rand(n,n)
    B = svdfact(A)
    for i = 1:iter-1
        B = svdfact(A)
    end
    B
end

function schurtest(n, iter)
    A = rand(n,n)
    B = schurfact(A)
    for i = 1:iter-1
        B = schurfact(A)
    end
    B
end

function choleskytest(n, iter)
    A = rand(n,n)
    A = A'*A
    B = cholfact(A)
    for i = 1:iter-1
        B = cholfact(A)
    end
    B
end

function qrtest(n, iter)
    A = rand(n,n)
    B = qrfact(A)
    for i = 1:iter-1
        B = qrfact(A)
    end
    B
end

function lutest(n, iter)
    A = rand(n,n)
    B = lufact(A)
    for i = 1:iter-1
        B = lufact(A)
    end
    B
end

for (testfunc, testname, longtestname) in [
        (choleskytest, "choleskytest", "Cholesky factorization"),
        (schurtest, "schurtest", "Schur factorization"),
        (svdtest, "svdtest", "Singular value decomposition")]
    for (n, t, size) in [(2  , 20000, "tiny"),
                         (2^4,  1000, "small"),
                         (2^6,   100, "medium"),
                         (2^8,     5, "large"),
                         (2^10,    1, "huge")]
      @timeit apply(testfunc, n, t) string(testname, "_", size) string(uppercase(size[1]), size[2:end], " ", longtestname, " test")
    end
end

for (testfunc, testname, longtestname) in [
        (qrtest, "qrtest", "QR factorization")]
    for (n, t, size) in [(2  ,100000, "tiny"),
                         (2^4,  5000, "small"),
                         (2^6,   500, "medium"),
                         (2^8,     5, "large"),
                         (2^10,    1, "huge")]
      @timeit apply(testfunc, n, t) string(testname, "_", size) string(uppercase(size[1]), size[2:end], " ", longtestname, " test")
    end
end

for (testfunc, testname, longtestname) in [
        (lutest, "lutest", "LU factorization")]
    for (n, t, size) in [(2  ,100000, "tiny"),
                         (2^4, 10000, "small"),
                         (2^6,  1000, "medium"),
                         (2^8,   100, "large"),
                         (2^10,    2, "huge")]
      @timeit apply(testfunc, n, t) string(testname, "_", size) string(uppercase(size[1]), size[2:end], " ", longtestname, " test")
    end
end

