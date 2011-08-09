# basic booleans
@assert true
@assert !false

# the bool operator
@assert bool(false) == false
@assert bool(true) == true

# basic type relationships
@assert Int8 <: Int
@assert Int32 <: Int
@assert (Int8,Int8) <: (Int,Int)
@assert !(AbstractArray{Float64,2} <: AbstractArray{Number,2})
@assert !(AbstractArray{Float64,1} <: AbstractArray{Float64,2})
@assert (Int,Int...) <: (Int,Real...)
@assert (Int,Float64,Int...) <: (Int,Number...)
@assert (Int,Float64) <: (Int,Number...)
@assert (Int32,) <: (Number...)
@assert () <: (Number...)
@assert !((Int32...) <: (Int32,))
@assert !((Int32...) <: (Number,Int))
@assert !((Int...,) <: (Int,Int,Int...))
@assert !(Array{Int8,1} <: Array{Any,1})
@assert !(Array{Any,1} <: Array{Int8,1})
@assert Array{Int8,1} <: Array{Int8,1}
@assert !subtype(Type{None}, Type{Int32})
@assert !subtype(Vector{Float64},Vector{Union(Float64,Float32)})
@assert is(None, tintersect(Vector{Float64},Vector{Union(Float64,Float32)}))

@assert !isa(Array,Type{Any})
@assert subtype(Type{ComplexPair},CompositeKind)
@assert isa(ComplexPair,Type{ComplexPair})
@assert subtype(Type{Ptr{None}},Type{Ptr})
let T = typevar(:T)
    @assert !is(None, tintersect(Array{None},AbstractArray{T}))
    @assert  is(None, tintersect((Type{Ptr{Uint8}},Ptr{None}),
                                 (Type{Ptr{T}},Ptr{T})))
end
let N = typevar(:N)
    @assert isequal(tintersect((NTuple{N,Int},NTuple{N,Int}),
                               ((Int,Int), (Int...))),
                    ((Int,Int), (Int,Int)))
    @assert isequal(tintersect((NTuple{N,Int},NTuple{N,Int}),
                               ((Int...), (Int,Int))),
                    ((Int,Int), (Int,Int)))
end
@assert is(None, tintersect(Type{Any},Type{ComplexPair}))
@assert is(None, tintersect(Type{Any},Type{typevar(:T,Real)}))
@assert !subtype(Type{Array{Int}},Type{AbstractArray{Int}})
@assert subtype(Type{Array{Int}},Type{Array{typevar(:T,Int)}})

# ntuples
nttest1{n}(x::NTuple{n,Size}) = n
@assert nttest1(()) == 0
@assert nttest1((1,2)) == 2
@assert NTuple <: Tuple
@assert NTuple{typevar(:T),Int32} <: (Int32...)
@assert !(NTuple{typevar(:T),Int32} <: (Int32,Int32...))
@assert (Int32...) <: NTuple{typevar(:T),Int32}
@assert (Int32,Int32...) <: NTuple{typevar(:T),Int32}

# basic arithmetic and indexing
@assert 2+3 == 5
@assert 2.+3. == 5.
@assert 2*3 == 6
@assert 2.*3 == 6
@assert 2. * 3. == 6.
@assert min(1.0,1) == 1

a = ones(4)
b = a+a
@assert b[1]==2. && b[2]==2. && b[3]==2. && b[4]==2.

@assert length((1,)) == 1
@assert length((1,2)) == 2

@assert 1+[1,2,3] == [2,3,4]
@assert [1,2,3]*5 == [5,10,15]

a = [1 2; 3 4]
@assert a' == [1 3; 2 4]

x = (2,3)
@assert +(x...) == 5

a = rand()
b = rand()
@assert a != b

@assert sign(-1) == -1
@assert sign(0) == 0
@assert isequal(sign(-NaN), NaN)
@assert signbit(-NaN) == -1

@assert isnan(NaN)   == true
@assert isnan(1//2)  == false

@assert isinf(-1.0)  == false
@assert isinf(Inf)   == true
@assert isinf(-Inf)  == true
@assert isinf(NaN)   == false

@assert isfinite(Inf)   == false
@assert isfinite(-Inf)  == false
@assert isfinite(NaN)   == false
@assert isfinite(1//2)  == true

@assert sqrt(2) == 1.4142135623730951

@assert 1+1.5 == 2.5
@assert is(typeof(convert(ComplexPair{Int16},1)),ComplexPair{Int16})
@assert ComplexPair(1,2)+1 == ComplexPair(2,2)
@assert 0.7 < real(sqrt(ComplexPair(0,1))) < 0.707107

@assert parse_int(Int32,"z",36) == 35
@assert parse_bin("0") == 0
@assert parse_oct("7") == 7
@assert parse_dec("3830974272") == 3830974272
@assert parse_hex("0BADF00D") == 195948557

function fooo()
    local x::Int8
    x = 1000
    x
end
@assert int32(fooo()) == -24

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
    @assert c == 2
    dec()
    @assert c == 1
    @assert (()->c)() == 1
    return (n->(c+=n), ()->c)
end
let T = clotest()
    (inc, C) = T
    inc(11)
    @assert C() == 12
end

Yc(f) = (h->f(x->h(h)(x)))(h->f(x->h(h)(x)))
yfib = Yc(fib->(n->(n < 2 ? n : fib(n-1) + fib(n-2))))
@assert yfib(20) == 6765

@assert map((x,y)->x+y,(1,2,3),(4,5,6)) == (5,7,9)
@assert map((x,y)->x+y,
            (100001,100002,100003),
            (100004,100005,100006)) == (200005,200007,200009)

# variable scope, globals
glob_x = 23
function glotest()
    global glob_x
    glob_x = 24
    loc_x = 8
    function inner()
        global loc_x = 10
        glob_x = 88
    end
    function inner2()
        local glob_x  # override
        global loc_x
        glob_x = 2
        @assert glob_x == 2
        @assert loc_x == 10
    end
    inner()
    inner2()
    @assert glob_x == 88
    @assert loc_x == 8
end
glotest()
@assert glob_x == 88
@assert loc_x == 10

# ranges
@assert size(10:1:0) == (0,)
@assert length(1:.2:2) == 6
@assert length(Range(2.,.2,1.)) == 0

# Arrays 
@assert [ones(2,2)  2*ones(2,1)] == [1 1 2; 1 1 2]

# blas, lapack
n = 10
a = rand(n,n)
(l,u,p) = lu(a)
@assert sum(l[p,:]*u - a) < 1e-8

# arpack
(d,v) = eigs(a, 3)
@assert abs(sum(a*v[:,1]-d[1,1]*v[:,1])) < 1e-8

# hash table
h = HashTable()
for i=1:100
    h[i] = i+1
end
for i=1:100
    @assert (h[i] == i+1)
end
h[77] = 100
@assert h[77]==100

# fft
a = rand(8) + im*rand(8)
@assert norm((1/length(a))*ifft(fft(a)) - a) < 1e-8

# Success
println("Julia unit tests pass")
