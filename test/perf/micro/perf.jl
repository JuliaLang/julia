using Base.Test

include("../perfutil.jl")

## recursive fib ##

fib(n) = n < 2 ? n : fib(n-1) + fib(n-2)

@test fib(20) == 6765
@timeit fib(20) "fib" "Recursive fibonacci"

## parse integer ##

function parseintperf(t)
    local n, m
    for i=1:t
        n = rand(UInt32)
        s = hex(n)
        m = uint32(parseint(Int64,s,16))
    end
    @test m == n
    return n
end

@timeit parseintperf(1000) "parse_int" "Integer parsing"

## array constructors ##

@test all(ones(200,200) .== 1)
# @timeit ones(200,200) "ones" "description"

## matmul and transpose ##

A = ones(200,200)
@test all(A*A' .== 200)
# @timeit A*A' "AtA" "description"

## mandelbrot set: complex arithmetic and comprehensions ##

function mandel(z)
    c = z
    maxiter = 80
    for n = 1:maxiter
        if abs(z) > 2
            return n-1
        end
        z = z^2 + c
    end
    return maxiter
end

mandelperf() = [ mandel(complex(r,i)) for i=-1.:.1:1., r=-2.0:.1:0.5 ]
@test sum(mandelperf()) == 14791
@timeit mandelperf() "mandel" "Calculation of mandelbrot set"

## numeric vector sort ##

function qsort!(a,lo,hi)
    i, j = lo, hi
    while i < hi
        pivot = a[(lo+hi)>>>1]
        while i <= j
            while a[i] < pivot; i += 1; end
            while a[j] > pivot; j -= 1; end
            if i <= j
                a[i], a[j] = a[j], a[i]
                i, j = i+1, j-1
            end
        end
        if lo < j; qsort!(a,lo,j); end
        lo, j = i, hi
    end
    return a
end

sortperf(n) = qsort!(rand(n), 1, n)
@test issorted(sortperf(5000))
@timeit sortperf(5000) "quicksort" "Sorting of random numbers using quicksort"

## slow pi series ##

function pisum()
    sum = 0.0
    for j = 1:500
        sum = 0.0
        for k = 1:10000
            sum += 1.0/(k*k)
        end
    end
    sum
end

@test abs(pisum()-1.644834071848065) < 1e-12
@timeit pisum() "pi_sum" "Summation of a power series"

## slow pi series, vectorized ##

function pisumvec()
    s = 0.0
    a = [1:10000]
    for j = 1:500
        s = sum(1./(a.^2))
    end
    s
end

#@test abs(pisumvec()-1.644834071848065) < 1e-12
#@timeit pisumvec() "pi_sum_vec"

## random matrix statistics ##

function randmatstat(t)
    n = 5
    v = zeros(t)
    w = zeros(t)
    for i=1:t
        a = randn(n,n)
        b = randn(n,n)
        c = randn(n,n)
        d = randn(n,n)
        P = [a b c d]
        Q = [a b; c d]
        v[i] = trace((P.'*P)^4)
        w[i] = trace((Q.'*Q)^4)
    end
    return (std(v)/mean(v), std(w)/mean(w))
end

(s1, s2) = randmatstat(1000)
@test 0.5 < s1 < 1.0 && 0.5 < s2 < 1.0
@timeit randmatstat(1000) "rand_mat_stat" "Statistics on a random matrix"

## largish random number gen & matmul ##

@timeit rand(1000,1000)*rand(1000,1000) "rand_mat_mul" "Multiplication of random matrices"

## printfd ##

@unix_only begin
    function printfd(n)
        open("/dev/null","w") do io
            for i = 1:n
                @printf(io,"%d %d\n",i,i+1)
            end
        end
    end

    printfd(1)
    @timeit printfd(100000) "printfd" "Printing to a file descriptor"
end
