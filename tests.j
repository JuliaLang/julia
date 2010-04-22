assert(true)
assert(!false)
assert(1)
assert(!!1)
assert(0)
assert(!!0)

# basic type relationships
assert(subtype(Int8, Int))
assert(subtype(Int32, Int))
assert(subtype((Int8,Int8), (Int,Int)))
assert(subtype(Tensor{Float64,2}, Tensor{Scalar,2}))
assert(!subtype(Tensor{Float64,1}, Tensor{Scalar,2}))
assert(subtype((Int,Int...), (Int, Scalar...)))
assert(subtype((Int,Float64,Int...), (Int, Scalar...)))
assert(subtype((Int,Float64), (Int, Scalar...)))
assert(subtype((Int32,), (Scalar...)))
assert(subtype((), (Scalar...)))
assert(!subtype((Int32...), (Int32,)))
assert(!subtype((Int32...), (Scalar, Int)))
assert(!subtype((Int...,), (Int, Int, Int...)))
assert(!subtype(Array{Int8,1}, Array{Any,1}))
assert(!subtype(Array{Any,1}, Array{Int8,1}))
assert(subtype(Array{Int8,1}, Array{Int8,1}))

# ntuples
nttest1{n}(x::NTuple{n,Int32}) = n
assert(nttest1(()) == 0)
assert(nttest1((1,2)) == 2)
assert(subtype(NTuple,Tuple))
assert(subtype(NTuple{typevar(`t),Int32}, (Int32...)))
assert(!subtype(NTuple{typevar(`t),Int32}, (Int32, Int32...)))
assert(subtype((Int32...), NTuple{typevar(`t),Int32}))
assert(subtype((Int32, Int32...), NTuple{typevar(`t),Int32}))

assert(2+3 == 5)
assert(2.+3. == 5.)
assert(2*3 == 6)
assert(2. * 3. == 6.)

a = ones(4)
b = a+a
assert(b[1]==2. && b[2]==2. && b[3]==2. && b[4]==2.)

assert(length((1,)) == 1)
assert(length((1,2)) == 2)

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

assert(signbit(1) == 1)
assert(signbit(-1) == -1)
assert(signbit(0) == 1)
assert(signbit(1.0) == 1)
assert(signbit(-1.0) == -1)
assert(signbit(0.0) == 1)
assert(signbit(-0.0) == -1)
assert(signbit( 1.0/0.0) == 1)
assert(signbit(-1.0/0.0) == -1)

assert(1+rational(1,2) == rational(3,2))
assert(1./Complex(2.,2.) == Complex(.25, -.25))

# conversions
function foo()
    local x::Int8
    function bar()
        x = 1000
    end
    bar()
    x
end
assert(int32(foo()) == -24)

z = Complex{Float64}.convert(2)
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
    return (n->do(c+=n), ()->c)
end
(inc, C) = clotest()
inc(11)
assert(C() == 12)

Y(f) = (h->f(x->h(h)(x)))(h->f(x->h(h)(x)))
yfib = Y(fib->(n->(n < 2 ? n : fib(n-1) + fib(n-2))))
assert(yfib(20) == 6765)

# comprehensions
X = [ i+2j | i=1:5, j=1:5 ]
assert(X[2,3] == 8)
assert(X[4,5] == 14)
