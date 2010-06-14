# simple performance tests

nl() = print("\n")

## recursive fib ##

fib(n) = n < 2 ? n : fib(n-1) + fib(n-2)

print("recursive fib(33): ")
fib(5)  # warm up: make sure fib is compiled
tic(); f = fib(33); toc()
assert(f == 3524578)
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
