# simple performance tests

nl() = print("\n")

## recursive fib ##

fib(n) = n < 2 ? n : fib(n-1) + fib(n-2)

print("recursive fib(20): ")
fib(5)  # warm up: make sure fib is compiled
tic(); f = fib(20); toc()
assert(f == 6765)
nl()

## parse int ##

print("parse_int: ")
bin("10")
tic()
for i=1:1000
    global n
    n=bin("1111000011110000111100001111")
end
toc()
assert(n == 252645135)
nl()

## array constructors ##

print("ones: ")
small=ones(2,2)
tic(); o = ones(200,200); toc()
assert(all(o==1))
nl()

## matmul and transpose ##

print("A * A': ")
small*small'
tic(); oo = o * o'; toc()
assert(all(oo==200))
nl()

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
mandel(Complex(-.53,.68))
tic()
M = [ mandel(Complex(r,i)) | r = -2.0:.1:0.5, i = -1.:.1:1. ]
toc()
assert(sum(M) == 14791)
nl()

## numeric vector quicksort ##

print("quicksort: ")
small=rand(3)
sort(small)
n = 5000
v = rand(n)
tic(); v = sort(v); toc()
assert(issorted(v))
nl()

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
tic(); s = pisum(); toc()
assert(abs(s-1.644834071848065) < 1e-12)
nl()

## Random matrix statistics ##

function randmatstat(t)
    n=5
    v = zeros(t)
    w = zeros(t)
    for i=1:t
        a = randn(n, n)
        b = randn(n, n)
        c = randn(n, n)
        d = randn(n, n)
        P = [a b c d]
        Q = [a b;c d]
        v[i] = trace((P'*P)^4)
        w[i] = trace((Q'*Q)^4)
    end
    return (std(v)/mean(v), std(w)/mean(w))
end

print("random matrix statistics: ")
randmatstat(5)
tic(); (s1, s2) = randmatstat(1000); toc()
#assert(round(10*s1) == 7);
nl()
