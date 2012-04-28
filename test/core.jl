# test core language features

# basic type relationships
@assert Int8 <: Integer
@assert Int32 <: Integer
@assert (Int8,Int8) <: (Integer,Integer)
@assert !(AbstractArray{Float64,2} <: AbstractArray{Number,2})
@assert !(AbstractArray{Float64,1} <: AbstractArray{Float64,2})
@assert (Integer,Integer...) <: (Integer,Real...)
@assert (Integer,Float64,Integer...) <: (Integer,Number...)
@assert (Integer,Float64) <: (Integer,Number...)
@assert (Int32,) <: (Number...)
@assert () <: (Number...)
@assert !((Int32...) <: (Int32,))
@assert !((Int32...) <: (Number,Integer))
@assert !((Integer...,) <: (Integer,Integer,Integer...))
@assert !(Array{Int8,1} <: Array{Any,1})
@assert !(Array{Any,1} <: Array{Int8,1})
@assert Array{Int8,1} <: Array{Int8,1}
@assert !subtype(Type{None}, Type{Int32})
@assert !subtype(Vector{Float64},Vector{Union(Float64,Float32)})
@assert is(None, tintersect(Vector{Float64},Vector{Union(Float64,Float32)}))

@assert !isa(Array,Type{Any})
@assert subtype(Type{ComplexPair},CompositeKind)
@assert isa(ComplexPair,Type{ComplexPair})
@assert !subtype(Type{Ptr{None}},Type{Ptr})
@assert !subtype(Type{Rational{Int}}, Type{Rational})
let T = typevar(:T,true)
    @assert !is(None, tintersect(Array{None},AbstractArray{T}))
    @assert  is(None, tintersect((Type{Ptr{Uint8}},Ptr{None}),
                                 (Type{Ptr{T}},Ptr{T})))
    @assert !subtype(Type{T},TypeVar)

    @assert isequal(tintersect((Range{Int},(Int,Int)),(AbstractArray{T},Dims)),
                    (Range{Int},(Int,Int)))

    @assert isequal(tintersect((T, AbstractArray{T}),(Number, Array{Int,1})),
                    (Int, Array{Int,1}))

    @assert isequal(tintersect((T, AbstractArray{T}),(Int, Array{Number,1})),
                    None)

    @assert isequal(tintersect((T, AbstractArray{T}),(Any, Array{Number,1})),
                    (Number, Array{Number,1}))
    @assert !is(None, tintersect((Array{T}, Array{T}), (Array, Array{Any})))
end
let N = typevar(:N,true)
    @assert isequal(tintersect((NTuple{N,Integer},NTuple{N,Integer}),
                               ((Integer,Integer), (Integer...))),
                    ((Integer,Integer), (Integer,Integer)))
    @assert isequal(tintersect((NTuple{N,Integer},NTuple{N,Integer}),
                               ((Integer...), (Integer,Integer))),
                    ((Integer,Integer), (Integer,Integer)))
end
@assert is(None, tintersect(Type{Any},Type{ComplexPair}))
@assert is(None, tintersect(Type{Any},Type{typevar(:T,Real)}))
@assert !subtype(Type{Array{Integer}},Type{AbstractArray{Integer}})
@assert !subtype(Type{Array{Integer}},Type{Array{typevar(:T,Integer)}})
@assert is(None, tintersect(Type{Function},BitsKind))
@assert is(Type{Int32}, tintersect(Type{Int32},BitsKind))
@assert !subtype(Type,TypeVar)

# ntuples
nttest1{n}(x::NTuple{n,Int}) = n
@assert nttest1(()) == 0
@assert nttest1((1,2)) == 2
@assert NTuple <: Tuple
@assert NTuple{typevar(:T),Int32} <: (Int32...)
@assert !(NTuple{typevar(:T),Int32} <: (Int32,Int32...))
@assert (Int32...) <: NTuple{typevar(:T),Int32}
@assert (Int32,Int32...) <: NTuple{typevar(:T),Int32}

# type declarations

abstract Sup_{A,B}
abstract Qux_{T} <: Sup_{Qux_{Int},T}

@assert subtype(Qux_{Int}.super, Sup_)
@assert is(Qux_{Int}, Qux_{Int}.super.parameters[1])
@assert is(Qux_{Int}.super.parameters[2], Int)
@assert subtype(Qux_{Char}.super, Sup_)
@assert is(Qux_{Int}, Qux_{Char}.super.parameters[1])
@assert is(Qux_{Char}.super.parameters[2], Char)

@assert subtype(Qux_.super.parameters[1].super, Sup_)
@assert is(Qux_{Int}, Qux_.super.parameters[1].super.parameters[1])
@assert is(Int, Qux_.super.parameters[1].super.parameters[2])

type Foo_{T} x::Foo_{Int} end

@assert is(Foo_.types[1], Foo_{Int})
@assert is(Foo_.types[1].types[1], Foo_{Int})

type Circ_{T} x::Circ_{T} end
# this does not necessarily have to be true, but it's nice
@assert is(Circ_, Circ_.types[1])

x = (2,3)
@assert +(x...) == 5

# bits types
if WORD_SIZE == 64
    @assert isa((()->box(Ptr{Int8},unbox64(0)))(), Ptr{Int8})
else
    @assert isa((()->box(Ptr{Int8},unbox32(0)))(), Ptr{Int8})
end
@assert isa(convert(Char,65), Char)

# conversions
function fooo()
    local x::Int8
    x = 1000
    x
end
@assert int32(fooo()) == -24
function foo()
    local x::Int8
    function bar()
        x = 1000
    end
    bar()
    x
end
@assert int32(foo()) == -24

function bar{T}(x::T)
    local z::ComplexPair{T}
    z = x
    z
end
@assert bar(3.0) == ComplexPair(3.0,0.0)

z = convert(ComplexPair{Float64},2)
@assert z == ComplexPair(2.0,0.0)

# misc
fib(n) = n < 2 ? n : fib(n-1) + fib(n-2)
@assert fib(20) == 6765

# static parameters
sptest1{T}(x::T, y::T) = 42
sptest1{T,S}(x::T, y::S) = 43
@assert sptest1(1,2) == 42
@assert sptest1(1,"b") == 43

sptest2{T}(x::T) = T
@assert is(sptest2(:a),Symbol)

sptest3{T}(x::T) = y->T
let m = sptest3(:a)
    @assert is(m(0),Symbol)
end

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

    fibb(n) = n < 2 ? n : fibb(n-1)+fibb(n-2)
    assert(fibb(10)==55)

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

# dispatch
begin
    local foo, bar, baz
    foo(x::(Any...))=0
    foo(x::(Integer...))=1
    @assert foo((:a,))==0
    @assert foo(( 2,))==1

    bar{T}(x::(T,T,T,T))=1
    bar(x::(Any,Any,Any,Any))=2
    @assert bar((1,1,1,1)) == 1
    @assert bar((1,1,1,"a")) == 2
    @assert bar((:a,:a,:a,:a)) == 1

    baz(::Type{Rational}) = 1
    baz{T}(::Type{Rational{T}}) = 2
    @assert baz(Rational) == 1
    @assert baz(Rational{Int}) == 2
end

# syntax
@assert (true ? 1 : false ? 2 : 3) == 1

begin
    local mytype
    function mytype(vec)
        convert(Vector{(ASCIIString, BitsKind)}, vec)
    end
    some_data = {("a", Int32), ("b", Int32)}
    @assert isa(mytype(some_data),Vector{(ASCIIString, BitsKind)})
end

type MyArray{N} <: AbstractArray{Int, N}
end
begin
    local x
    x = MyArray{1}()
    foob(x::AbstractArray)=0
    foob{T}(x::AbstractVector{T})=1
    @assert foob(x) == 1
end

begin
    local f, g, a
    f{T}(a::Vector{Vector{T}}) = a
    g{T}(a::Vector{Vector{T}}) = a
    a = Vector{Int}[]
    @assert is(f(a), a)
    @assert is(g(a), a)
end

type _AA{T}; a::T; end
typealias _AoA{T} _AA{_AA{T}}
begin
    local g, a
    g{T}(a::_AA{_AA{T}}) = a
    a = _AA(_AA(1))
    @assert is(g(a),a)
end
