# simple performance tests

nl() = print("\n")

function timeit(func, args...)
    nexpt = 5
    times = zeros(nexpt)

    for i=1:nexpt
        tic(); func(args...); times[i] = qtoc();
    end

    times = sort(times)
    print (times[1])
    nl()
    nl()
end

nl()

## recursive fib ##

fib(n) = n < 2 ? n : fib(n-1) + fib(n-2)

print("recursive fib(20): ")
f = fib(20)
@assert f == 6765
timeit(fib, 20)

## parse int ##

print("parse_int: ")

function parseintperf()
    local n
    for i=1:1000
        n=bin_parse("1111000011110000111100001111")
    end
    n
end

@assert parseintperf() == 252645135
timeit(parseintperf)

## array constructors ##

print("ones: ")
o = ones(200,200)
@assert o == 1
timeit(ones, 200, 200)

## matmul and transpose ##

print("A * transpose(A): ")
matmul(o) = o * o.'
@assert all(matmul(o)==200)
timeit(matmul, o)

## mandelbrot set: complex arithmetic and comprehensions ##

function mandel(z::Complex)
    n = 0
    c = z
    for n=0:79
        if abs(z)>2
            break
        end
        z = z^2+c
    end
    n
end

print("mandelbrot: ")
mandelperf() = [ mandel(Complex(r,i)) | r = -2.0:.1:0.5, i = -1.:.1:1. ]
@assert sum(mandelperf()) == 14791
timeit(mandelperf)

## numeric vector sort ##

print("sort: ")
function sortperf(n)
  v = rand(n)
  v = sort(v)
end
@assert issorted(sortperf(5000))
timeit(sortperf, 5000)

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

print("pi sum: ")
s = pisum()
@assert abs(s-1.644834071848065) < 1e-12
timeit(pisum)

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

print("random matrix statistics: ")
(s1, s2) = randmatstat(1000)
@assert s1 > 0.5 && s1 < 1.0
timeit(randmatstat, 1000)
