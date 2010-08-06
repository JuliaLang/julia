# basic booleans
assert(true)
assert(!false)
assert(true == true)
assert(false == false)
assert(true != false)

# the bool operator
assert(bool(false) == false)
assert(bool(true) == true)
assert(bool(0) == false)
assert(bool(1) == true)
assert(bool(-1) == true)
assert(bool(0.0) == false)
assert(bool(1.0) == true)
assert(bool(0.1) == true)
assert(bool(-1.0) == true)
assert(bool(Complex(0,0)) == false)
assert(bool(Complex(1,0)) == true)
assert(bool(Complex(0,1)) == true)
assert(bool(0//1) == false)
assert(bool(1//1) == true)
assert(bool(1//2) == true)

# basic type relationships
assert(Int8 <: Int)
assert(Int32 <: Int)
assert((Int8,Int8) <: (Int,Int))
assert(!(Tensor{Float64,2} <: Tensor{Scalar,2}))
assert(!(Tensor{Float64,1} <: Tensor{Float64,2}))
assert((Int,Int...) <: (Int,Scalar...))
assert((Int,Float64,Int...) <: (Int,Scalar...))
assert((Int,Float64) <: (Int,Scalar...))
assert((Int32,) <: (Scalar...))
assert(() <: (Scalar...))
assert(!((Int32...) <: (Int32,)))
assert(!((Int32...) <: (Scalar,Int)))
assert(!((Int...,) <: (Int,Int,Int...)))
assert(!(Array{Int8,1} <: Array{Any,1}))
assert(!(Array{Any,1} <: Array{Int8,1}))
assert(Array{Int8,1} <: Array{Int8,1})

# ntuples
nttest1{n}(x::NTuple{n,Int32}) = n
assert(nttest1(()) == 0)
assert(nttest1((1,2)) == 2)
assert(NTuple <: Tuple)
assert(NTuple{typevar(`T),Int32} <: (Int32...))
assert(!(NTuple{typevar(`T),Int32} <: (Int32,Int32...)))
assert((Int32...) <: NTuple{typevar(`T),Int32})
assert((Int32,Int32...) <: NTuple{typevar(`T),Int32})

assert(2+3 == 5)
assert(2.+3. == 5.)
assert(2*3 == 6)
assert(2. * 3. == 6.)
assert(min(1.0,1) == 1)

a = ones(4)
b = a+a
assert(b[1]==2. && b[2]==2. && b[3]==2. && b[4]==2.)

assert(length((1,)) == 1)
assert(length((1,2)) == 2)

assert(1+[1,2,3] == [2,3,4])
assert([1,2,3]+1 == [2,3,4])
assert(1-[1,2,3] == [0,-1,-2])
assert([1,2,3]-1 == [0,1,2])

assert(5*[1,2,3] == [5,10,15])
assert([1,2,3]*5 == [5,10,15])
assert(1/[1,2,5] == [1.0,0.5,0.2])
assert([1,2,3]/5 == [0.2,0.4,0.6])

l = dequeue(1,2,3)
push(l,8)
assert(l[1]==1 && l[2]==2 && l[3]==3 && l[4]==8)
v = pop(l)
assert(v == 8)
v = pop(l)
assert(v == 3)
assert(length(l)==2)

a = ones(2,2)
a[1,1] = 1
a[1,2] = 2
a[2,1] = 3
a[2,2] = 4
b = a'
assert(a[1,1] == 1. && a[1,2] == 2. &&
       a[2,1] == 3. && a[2,2] == 4.)
assert(b[1,1] == 1. && b[2,1] == 2. &&
       b[1,2] == 3. && b[2,2] == 4.)

x = (2,3)
assert((+)(x...) == 5)

a = rand()
b = rand()
assert(a != b)

assert(sign(1) == 1)
assert(sign(-1) == -1)
assert(sign(0) == 0)
assert(sign(1.0) == 1)
assert(sign(-1.0) == -1)
assert(sign(0.0) == 0)
assert(sign(-0.0) == 0)
assert(sign( 1.0/0.0) == 1)
assert(sign(-1.0/0.0) == -1)
assert(sign(2//3) == 1)
assert(sign(-2//3) == -1)
assert(sign(0//1) == 0)
assert(sign(-0//1) == 0)

assert(signbit(1) == 1)
assert(signbit(-1) == -1)
assert(signbit(0) == 1)
assert(signbit(1.0) == 1)
assert(signbit(-1.0) == -1)
assert(signbit(0.0) == 1)
assert(signbit(-0.0) == -1)
assert(signbit(1.0/0.0) == 1)
assert(signbit(-1.0/0.0) == -1)

assert(1//1 == 1)
assert(2//2 == 1)
assert(1//1 == 1//1)
assert(2//2 == 1//1)
assert(2//4 == 3//6)
assert(1//2 + 1//2 == 1)
assert((-1)//3 == -(1//3))
assert(1//2 + 3//4 == 5//4)
assert(1//3 * 3//4 == 1//4)
# assert(1//2 / 3//4 == 2//3)

assert(1+1.5 == 2.5)
assert(1.5+1 == 2.5)
assert(1+1.5+2 == 4.5)
assert(is(typeof(convert(Complex{Int16},1)),Complex{Int16}))
assert(Complex(1,2)+1 == Complex(2,2))
assert(Complex(1,2)+1.5 == Complex(2.5,2.0))
assert(1/Complex(2,2) == Complex(.25,-.25))
assert(Complex(1.5,1.0) + 1//2 == Complex(2.0,1.0))
assert(real(Complex(1//2,2//3)) == 1//2)
assert(imag(Complex(1//2,2//3)) == 2//3)
assert(Complex(1,2) + 1//2 == Complex(3//2,2//1))
assert(Complex(1,2) + 1//2 * 0.5 == Complex(1.25,2.0))
assert((Complex(1,2) + 1//2) * 0.5 == Complex(0.75,1.0))
assert((Complex(1,2)/Complex(2.5,3.0))*Complex(2.5,3.0) == Complex(1,2))
assert(0.7 < real(sqrt(Complex(0,1))) < 0.707107)

# integer parsing
assert(digit("0"[1]) == 0)
assert(digit("1"[1]) == 1)
assert(digit("9"[1]) == 9)
assert(digit("A"[1]) == 10)
assert(digit("a"[1]) == 10)
assert(digit("B"[1]) == 11)
assert(digit("b"[1]) == 11)
assert(digit("F"[1]) == 15)
assert(digit("f"[1]) == 15)
assert(digit("Z"[1]) == 35)
assert(digit("z"[1]) == 35)

assert(bin("0") == 0)
assert(bin("1") == 1)
assert(bin("10") == 2)
assert(bin("11") == 3)
assert(bin("1111000011110000111100001111") == 252645135)

assert(oct("0") == 0)
assert(oct("1") == 1)
assert(oct("7") == 7)
assert(oct("10") == 8)
assert(oct("11") == 9)
assert(oct("72") == 58)
assert(oct("3172207320") == 434704080)

assert(dec("0") == 0)
assert(dec("1") == 1)
assert(dec("9") == 9)
assert(dec("10") == 10)
assert(dec("3830974272") == 3830974272)

assert(hex("0") == 0)
assert(hex("1") == 1)
assert(hex("9") == 9)
assert(hex("a") == 10)
assert(hex("f") == 15)
assert(hex("10") == 16)
assert(hex("0BADF00D") == 195948557)
assert(hex("BADCAB1E") == 3135023902)
assert(hex("CafeBabe") == 3405691582)
assert(hex("DeadBeef") == 3735928559)

# conversions
function fooo()
    local x::Int8
    x = 1000
    x
end
assert(int32(fooo()) == -24)
function foo()
    local x::Int8
    function bar()
        x = 1000
    end
    bar()
    x
end
assert(int32(foo()) == -24)

function bar{T}(x::T)
    local z::Complex{T}
    z = x
    z
end
assert(bar(3.0) == Complex(3.0,0.0))

z = convert(Complex{Float64},2)
assert(z == Complex(2.0,0.0))

# misc
fib(n) = n < 2 ? n : fib(n-1) + fib(n-2)
assert(fib(20) == 6765)

# static parameters
sptest1{T}(x::T, y::T) = 42
sptest1{T,S}(x::T, y::S) = 43
assert(sptest1(1,2) == 42)
assert(sptest1(1,"b") == 43)

sptest2{T}(x::T) = T
assert(is(sptest2(`a),Symbol))

sptest3{T}(x::T) = y->T
m = sptest3(`a)
assert(is(m(0),Symbol))

# closures
function clotest()
    c = 0
    function inc()
        c += 1
    end
    function dec()
        c -= 1
    end
    inc(); inc()
    assert(c == 2)
    dec()
    assert(c == 1)
    assert((()->c)() == 1)
    return (n->(c+=n), ()->c)
end
(inc, C) = clotest()
inc(11)
assert(C() == 12)

Y(f) = (h->f(x->h(h)(x)))(h->f(x->h(h)(x)))
yfib = Y(fib->(n->(n < 2 ? n : fib(n-1) + fib(n-2))))
assert(yfib(20) == 6765)

# variable scope, globals
glob_x = 23
function glotest()
    global glob_x
    glob_x = 24
    loc_x = 8
    function inner()
        global loc_x = 10
        glob_x = 88  # glob_x is not global in here
    end
    inner()
    assert(loc_x == 8)
end
glotest()
assert(glob_x == 24)
assert(loc_x == 10)

# comprehensions
X = [ i+2j | i=1:5, j=1:5 ]
assert(X[2,3] == 8)
assert(X[4,5] == 14)
assert(ones(2,3) * ones(2,3)' == [3.,3.; 3.,3.])
assert([ [1,2] | i=1:2, : ] == [1, 2; 1, 2])

# concatenation
assert( [ ones(2,2), 2*ones(2,1) ] == [ 1, 1, 2; 1, 1, 2 ] )
assert( [ ones(2,2); 2*ones(1,2) ] == [ 1, 1; 1, 1; 2, 2 ] )

# syntax
assert((true ? 1 : false ? 2 : 3) == 1)

# blas, lapack
n = 10
a = rand(n,n)
asym = a+a'+n*eye(n)
b = rand(n)
r = chol(asym)
assert(sum(r'*r - asym) < 1e-8)
(l,u,p) = lu(a)
assert(sum(l[p,:]*u - a) < 1e-8)
(q,r,p) = qr(a)
assert(sum(q*r[:,p] - a) < 1e-8)
(v,d) = eig(asym)
assert(sum(asym*v[:,1]-d[1]*v[:,1]) < 1e-8)
(u,s,vt) = svd(a)
assert(sum(u*s*vt - a) < 1e-8)
x = a \ b
assert(sum(a*x-b) < 1e-8)
x = triu(a) \ b
assert(sum(triu(a)*x-b) < 1e-8)
x = tril(a) \ b
assert(sum(tril(a)*x-b) < 1e-8)
