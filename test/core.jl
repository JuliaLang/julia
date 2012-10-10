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
let T = TypeVar(:T,true)
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
    @assert is(None, tintersect((Vector{Vector{Int}},Vector{Vector}),
                                (Vector{Vector{T}},Vector{Vector{T}})))
end
let N = TypeVar(:N,true)
    @assert isequal(tintersect((NTuple{N,Integer},NTuple{N,Integer}),
                               ((Integer,Integer), (Integer...))),
                    ((Integer,Integer), (Integer,Integer)))
    @assert isequal(tintersect((NTuple{N,Integer},NTuple{N,Integer}),
                               ((Integer...), (Integer,Integer))),
                    ((Integer,Integer), (Integer,Integer)))
    local A = tintersect((NTuple{N,Any},Array{Int,N}),
                         ((Int,Int...),Array))
    local B = ((Int,Int...),Array{Int,N})
    @assert A<:B && B<:A
    @assert isequal(tintersect((NTuple{N,Any},Array{Int,N}),
                               ((Int,Int...),Array{Int,2})),
                    ((Int,Int), Array{Int,2}))
end
@assert is(None, tintersect(Type{Any},Type{ComplexPair}))
@assert is(None, tintersect(Type{Any},Type{TypeVar(:T,Real)}))
@assert !subtype(Type{Array{Integer}},Type{AbstractArray{Integer}})
@assert !subtype(Type{Array{Integer}},Type{Array{TypeVar(:T,Integer)}})
@assert is(None, tintersect(Type{Function},BitsKind))
@assert is(Type{Int32}, tintersect(Type{Int32},BitsKind))
@assert !subtype(Type,TypeVar)
@assert !is(None, tintersect(BitsKind, Type))
@assert !is(None, tintersect(BitsKind, Type{Int}))
@assert is(None, tintersect(BitsKind, Type{Integer}))
@assert !is(None, tintersect(BitsKind, Type{TypeVar(:T,Int)}))
@assert !is(None, tintersect(BitsKind, Type{TypeVar(:T,Integer)}))

# ntuples
nttest1{n}(x::NTuple{n,Int}) = n
@assert nttest1(()) == 0
@assert nttest1((1,2)) == 2
@assert NTuple <: Tuple
@assert NTuple{TypeVar(:T),Int32} <: (Int32...)
@assert !(NTuple{TypeVar(:T),Int32} <: (Int32,Int32...))
@assert (Int32...) <: NTuple{TypeVar(:T),Int32}
@assert (Int32,Int32...) <: NTuple{TypeVar(:T),Int32}

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
@assert is(Circ_{Int}, Circ_{Int}.types[1])

# issue #786
type Node{T}
    v::Vector{Node}
end

@assert is(Node{Int}.types[1].parameters[1], Node)

type Node2{T}
    v::Vector{Node2{T}}
end

@assert is(Node2{Int}.types[1].parameters[1], Node2{Int})

type FooFoo{A,B} y::FooFoo{A} end

@assert FooFoo{Int} <: FooFoo{Int,String}.types[1]


x = (2,3)
@assert +(x...) == 5

# bits types
if WORD_SIZE == 64
    @assert isa((()->box(Ptr{Int8},unbox(Int64,0)))(), Ptr{Int8})
else
    @assert isa((()->box(Ptr{Int8},unbox(Int32,0)))(), Ptr{Int8})
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

# syntax
@assert (true ? 1 : false ? 2 : 3) == 1

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

# allow typevar in Union to match as long as the arguments contain
# sufficient information
# issue #814
begin
    local MatOrNothing, my_func, M
    typealias MatOrNothing{T} Union(AbstractMatrix{T}, Vector{None})
    my_func{T<:Real}(A::MatOrNothing{T}, B::MatOrNothing{T},
                     C::MatOrNothing{T}) = 0
    M = [ 2. 1. ; 1. 1. ]
    @assert my_func([], M, M) == 0
end

begin
    local my_func, a, c
    my_func{T}(P::Vector{T}, Q::Vector{T}) = 0
    my_func{T}(x::T, P::Vector{T}) = 1
    # todo: this gives an ambiguity warning
    #my_func{T}(P::Vector{T}, x::T) = 2
    a = Int[3]
    c = Vector[a]

    @assert my_func(c,c)==0
    @assert_fails my_func(a,c)
end

begin
    local baar, foor, boor
    # issue #1131
    baar(x::CompositeKind) = 0
    baar(x::UnionKind) = 1
    baar(x::TypeConstructor) = 2
    @assert baar(StridedArray) == 2
    @assert baar(StridedArray.body) == 1
    @assert baar(Vector) == 2
    @assert baar(Vector.body) == 0

    boor(x) = 0
    boor(x::UnionKind) = 1
    @assert boor(StridedArray) == 0
    @assert boor(StridedArray.body) == 1

    # issue #1202
    foor(x::UnionKind) = 1
    @assert_fails foor(StridedArray)
    @assert foor(StridedArray.body) == 1
    @assert_fails foor(StridedArray)
end

# issue #1153
type SI{m, s, kg}
    value::FloatingPoint
end

*{m1, m2, s1, s2, kg1, kg2}(x::SI{m1, s1, kg1}, y::SI{m2, s2, kg2}) = SI{m1 + m2, s1 + s2, kg1 + kg2}(x.value * y.value)

begin
    local a,b
    a = SI{0,0,1}(1.0) * SI{1,2,0}(2.0)
    b = SI{0,0,1}(1.0) * SI{1,-2,0}(2.0)
    @assert typeof(a) === SI{1,2,1}
    @assert typeof(b) === SI{1,-2,1}
end

# pull request 1270
begin
    local a,p, a2,p2
    a = [11,12,13]
    p = pointer(a)
    @assert unsafe_ref(p, 1) == 11
    unsafe_assign(p, 99, 2)
    @assert a == [11,99,13]
    a2 = Any[101,102,103]
    p2 = pointer(a2)
    @assert unsafe_ref(p2) == 101
    unsafe_assign(p2, 909, 3)
    @assert a2 == [101,102,909]
end

# issue #1287, combinations of try, catch, return
begin
    local f, g

    function f()
        try
            return 1
        end
    end
    @assert f() == 1

    function g()
        try
            error("badness")
        catch
            return 2
        end
    end
    @assert g() == 2
end
