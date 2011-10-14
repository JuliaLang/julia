println("*** Julia ***")

# simple performance tests

macro timeit(ex,name)
    quote
        t = Inf
        for i=1:5
            t = min(t, @elapsed $ex)
        end
        println(rpad(strcat($name,":"), 20), t)
    end
end

## recursive fib ##

fib(n) = n < 2 ? n : fib(n-1) + fib(n-2)

@assert fib(20) == 6765
@timeit fib(20) "recursive fib"

## parse int ##

function parseintperf()
    local n
    for i=1:1000
        n=parse_bin("1111000011110000111100001111")
    end
    return n
end

@assert parseintperf() == 252645135
@timeit parseintperf() "parse_int"

## array constructors ##

@assert ones(200,200) == 1
@timeit ones(200,200) "ones"

## matmul and transpose ##

A = ones(200,200)
@assert A*A' == 200
@timeit A*A' "A*A'"

## mandelbrot set: complex arithmetic and comprehensions ##

function mandel(z::Complex)
    n = 0
    c = z
    for n=0:79
        if abs(z)>2
            n -= 1
            break
        end
        z = z^2+c
    end
    n+1
end

mandelperf() = [ mandel(complex(r,i)) | r=-2.0:.1:0.5, i=-1.:.1:1. ]
@assert sum(mandelperf()) == 14791
@timeit mandelperf() "mandelbrot"

## numeric vector sort ##

function sortperf(n)
  v = rand(n)
  v = sort(v)
end
@assert issorted(sortperf(5000))
@timeit sortperf(5000) "sort"

## slow pi series ##

function pisum()
    sum = 0.0
    for j=1:500
        sum = 0.0
        for k=1:10000
            sum += 1.0/(k*k)
        end
    end
    sum
end

@assert abs(pisum()-1.644834071848065) < 1e-12
@timeit pisum() "pi sum"

## Random matrix statistics ##

function randmatstat(t)
    n = 5
    v = zeros(t)
    w = zeros(t)
    for i=1:t
        a = randn(n, n)
        b = randn(n, n)
        c = randn(n, n)
        d = randn(n, n)
        P = [a b c d]
        Q = [a b; c d]
        v[i] = trace((P.'*P)^4)
        w[i] = trace((Q.'*Q)^4)
    end
    return (std(v)/mean(v), std(w)/mean(w))
end

(s1, s2) = randmatstat(1000)
@assert s1 > 0.5 && s1 < 1.0
@timeit randmatstat(1000) "random matrix"
