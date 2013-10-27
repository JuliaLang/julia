# Real
function realeigtest(n, iter)
    A = rand(n,n)
    d = Vector{eltype(A)}
    v = similar(A)
    for i = 1:iter
        d,v = eig(A)
    end
    d,v
end

# Symmetric
function symeigtest(n, iter)
    A = rand(n,n)
    A = A + A'
    d = Vector{eltype(A)}
    v = similar(A)
    for i = 1:iter
        d,v = eig(A)
    end
    d,v
end

# Hermitian
function hermitianeigtest(n, iter)
    A = rand(n,n) + im*rand(n,n)
    A = A + A'
    d = Vector{eltype(A)}
    v = similar(A)
    for i = 1:iter
        d,v = eig(A)
    end
    d,v
end

for (testfunc, testname, longtestname) in [
        (realeigtest, "realeig", "Real matrix eigenfactorization"),
        (symeigtest, "symeig", "Symmetric matrix eigenfactorization"),
        (hermitianeigtest, "hermitianeig", "Hermitian matrix eigenfactorization")]
    for (n, t, size) in [(2  , 10^4, "tiny"),
                         (2^4, 10^3, "small"),
                         (2^6, 10^2, "medium"),
                         (2^8, 5   , "large"),
                         (2^10,1   , "huge")]
      @timeit apply(testfunc, n, t) string(testname, "_", size) string(uppercase(size[1]), size[2:end], " ", longtestname, " test")
    end
end

