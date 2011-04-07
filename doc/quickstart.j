# load a source file
load("file.j")

# defining and changing scalar and array variables
x = 3
x = [1 2 3]     # set x to a 1x3 row vector
x = [1, 2, 3]   # set x to a 1d vector (implicitly treated as a column)
A = [1 2 3 4; 5 6 7 8; 9 10 11 12];
    # set A to a 3x4 matrix, semicolon hides output at the prompt.
    # inside a source file no output is the default.
x[2] = 7    # change x to [1 7 3]
A[2,1] = 0  # chage A[2,1] to 0

# matrix constructors
rand(n)     # uniform random n-element 1d vector
randn(m,n)  # normal random mxn matrix
# same for ones, zeros
diag(A)     # diagonal of matrix
diagm(x)    # construct a diagonal matrix from a vector

# cell arrays
c = {1, 2, 3}   # 1d
c = {1 2; A x}  # 2d

# solving linear equations
(L,U,P) = lu(A)

# function definitions, loops
# 1. long form (for multiple statements)
function f1(x, y, n)
    s = 0
    for i=1:n
        s += x
    end
    while n < y < x   # (n<y) && (y<x)
        n *= 2
    end
    s+n    # value of last expression is returned; "return s+n" also works
end

# example of returning multiple values
# return (gcd(a,b),x,y) such that ax+by == gcd(a,b)
function gcdx(a, b)
    if b == 0
        (a, 1, 0)
    else
        m = a % b
        k = div((a-m), b)   # div is integer division
        (g, x, y) = gcdx(b, m)
        (g, y, x-k*y)
    end
end

# 2. short form (for a single expression)
f(x) = 2x-1

# 3. anonymous functions
map(x->x+1, {1,2,3})     # gives {2,3,4}. "x->x+1" is a 1-argument function

()->2        # zero-argument anonymous function
(x,y)->3y-x  # two-argument anonymous function

# conditional within an expression
fib(n) = n < 2 ? n : fib(n-1) + fib(n-2)

# multiple statements on a line
x = 1; a = 2; b = x

"string"
'A'  # character

# pascal's triangle symmetric matrix using a comprehension
S(n) = [nCr(i+j-2,i-1) | i=1:n,j=1:n]
