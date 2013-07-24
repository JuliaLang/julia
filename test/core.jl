# test core language features

# basic type relationships
@test Int8 <: Integer
@test Int32 <: Integer
@test (Int8,Int8) <: (Integer,Integer)
@test !(AbstractArray{Float64,2} <: AbstractArray{Number,2})
@test !(AbstractArray{Float64,1} <: AbstractArray{Float64,2})
@test (Integer,Integer...) <: (Integer,Real...)
@test (Integer,Float64,Integer...) <: (Integer,Number...)
@test (Integer,Float64) <: (Integer,Number...)
@test (Int32,) <: (Number...)
@test () <: (Number...)
@test !((Int32...) <: (Int32,))
@test !((Int32...) <: (Number,Integer))
@test !((Integer...,) <: (Integer,Integer,Integer...))
@test !(Array{Int8,1} <: Array{Any,1})
@test !(Array{Any,1} <: Array{Int8,1})
@test Array{Int8,1} <: Array{Int8,1}
@test !subtype(Type{None}, Type{Int32})
@test !subtype(Vector{Float64},Vector{Union(Float64,Float32)})
@test is(None, typeintersect(Vector{Float64},Vector{Union(Float64,Float32)}))

@test !isa(Array,Type{Any})
@test subtype(Type{Complex},DataType)
@test isa(Complex,Type{Complex})
@test !subtype(Type{Ptr{None}},Type{Ptr})
@test !subtype(Type{Rational{Int}}, Type{Rational})
let T = TypeVar(:T,true)
    @test !is(None, typeintersect(Array{None},AbstractArray{T}))
    @test  is(None, typeintersect((Type{Ptr{Uint8}},Ptr{None}),
                                  (Type{Ptr{T}},Ptr{T})))
    @test !subtype(Type{T},TypeVar)

    @test isequal(typeintersect((Range{Int},(Int,Int)),(AbstractArray{T},Dims)),
                  (Range{Int},(Int,Int)))

    @test isequal(typeintersect((T, AbstractArray{T}),(Number, Array{Int,1})),
                  (Int, Array{Int,1}))

    @test isequal(typeintersect((T, AbstractArray{T}),(Int, Array{Number,1})),
                  None)

    @test isequal(typeintersect((T, AbstractArray{T}),(Any, Array{Number,1})),
                  (Number, Array{Number,1}))
    @test !is(None, typeintersect((Array{T}, Array{T}), (Array, Array{Any})))
    f47{T}(x::Vector{Vector{T}}) = 0
    @test_throws f47(Array(Vector,0))
    @test f47(Array(Vector{Int},0)) == 0
end
let N = TypeVar(:N,true)
    @test isequal(typeintersect((NTuple{N,Integer},NTuple{N,Integer}),
                                ((Integer,Integer), (Integer...))),
                  ((Integer,Integer), (Integer,Integer)))
    @test isequal(typeintersect((NTuple{N,Integer},NTuple{N,Integer}),
                                ((Integer...), (Integer,Integer))),
                  ((Integer,Integer), (Integer,Integer)))
    local A = typeintersect((NTuple{N,Any},Array{Int,N}),
                            ((Int,Int...),Array))
    local B = ((Int,Int...),Array{Int,N})
    @test A<:B && B<:A
    @test isequal(typeintersect((NTuple{N,Any},Array{Int,N}),
                                ((Int,Int...),Array{Int,2})),
                  ((Int,Int), Array{Int,2}))
end
@test is(None, typeintersect(Type{Any},Type{Complex}))
@test is(None, typeintersect(Type{Any},Type{TypeVar(:T,Real)}))
@test !subtype(Type{Array{Integer}},Type{AbstractArray{Integer}})
@test !subtype(Type{Array{Integer}},Type{Array{TypeVar(:T,Integer)}})
@test is(None, typeintersect(Type{Function},UnionType))
@test is(Type{Int32}, typeintersect(Type{Int32},DataType))
@test !subtype(Type,TypeVar)
@test !is(None, typeintersect(DataType, Type))
@test !is(None, typeintersect(UnionType, Type))
@test !is(None, typeintersect(DataType, Type{Int}))
@test !is(None, typeintersect(DataType, Type{TypeVar(:T,Int)}))
@test !is(None, typeintersect(DataType, Type{TypeVar(:T,Integer)}))

@test isa(Int,Type{TypeVar(:T,Number)})
@test !isa(DataType,Type{TypeVar(:T,Number)})
@test subtype(DataType,Type{TypeVar(:T,Type)})

@test isa((),Type{()})
@test subtype((DataType,),Type{TypeVar(:T,Tuple)})
@test !subtype((Int,),Type{TypeVar(:T,Tuple)})
@test isa((Int,),Type{TypeVar(:T,Tuple)})

@test !isa(Type{(Int,Int)},Tuple)
@test subtype(Type{(Int,Int)},Tuple)
@test subtype(Type{(Int,)}, (DataType,))

# this is fancy: know that any type T<:Number must be either a DataType or a UnionType
@test subtype(Type{TypeVar(:T,Number)},Union(DataType,UnionType))
@test !subtype(Type{TypeVar(:T,Number)},DataType)
@test subtype(Type{TypeVar(:T,Tuple)},Union(Tuple,UnionType))
@test !subtype(Type{TypeVar(:T,Tuple)},Union(DataType,UnionType))

@test !is(None, typeintersect((DataType,DataType),Type{TypeVar(:T,(Number,Number))}))
@test !is(None, typeintersect((DataType,UnionType),Type{(Number,None)}))

# issue #2997
let T = TypeVar(:T,Union(Float64,Array{Float64,1}),true)
    @test typeintersect(T,Real) === Float64
end

# join
@test typejoin(Int8,Int16) === Signed
@test typejoin(Int,String) === Any
@test typejoin(Array{Float64},BitArray) <: AbstractArray
@test typejoin(Array{Bool},BitArray) <: AbstractArray{Bool}
@test typejoin((Int,Int8),(Int8,Float64)) === (Signed,Real)
@test Base.typeseq(typejoin((ASCIIString,ASCIIString),(UTF8String,ASCIIString),
                            (ASCIIString,UTF8String),(Int,ASCIIString,Int)),
                   (Any,String,Int...))
@test Base.typeseq(typejoin((Int8,Int...),(Int8,Int8)),
                   (Int8,Signed...))
@test Base.typeseq(typejoin((Int8,Int...),(Int8,Int8...)),
                   (Int8,Signed...))
@test Base.typeseq(typejoin((Int8,Uint8,Int...),(Int8,Int8...)),
                   (Int8,Integer...))
@test Base.typeseq(typejoin(Union(Int,String),Int), Union(Int,String))
@test Base.typeseq(typejoin(Union(Int,String),Int8), Any)

@test promote_type(Bool,None) === Bool

# ntuples
nttest1{n}(x::NTuple{n,Int}) = n
@test nttest1(()) == 0
@test nttest1((1,2)) == 2
@test NTuple <: Tuple
@test NTuple{TypeVar(:T),Int32} <: (Int32...)
@test !(NTuple{TypeVar(:T),Int32} <: (Int32,Int32...))
@test (Int32...) <: NTuple{TypeVar(:T),Int32}
@test (Int32,Int32...) <: NTuple{TypeVar(:T),Int32}

# type declarations

abstract Sup_{A,B}
abstract Qux_{T} <: Sup_{Qux_{Int},T}

@test subtype(Qux_{Int}.super, Sup_)
@test is(Qux_{Int}, Qux_{Int}.super.parameters[1])
@test is(Qux_{Int}.super.parameters[2], Int)
@test subtype(Qux_{Char}.super, Sup_)
@test is(Qux_{Int}, Qux_{Char}.super.parameters[1])
@test is(Qux_{Char}.super.parameters[2], Char)

@test subtype(Qux_.super.parameters[1].super, Sup_)
@test is(Qux_{Int}, Qux_.super.parameters[1].super.parameters[1])
@test is(Int, Qux_.super.parameters[1].super.parameters[2])

type Foo_{T} x::Foo_{Int} end

@test is(Foo_.types[1], Foo_{Int})
@test is(Foo_.types[1].types[1], Foo_{Int})

type Circ_{T} x::Circ_{T} end
@test is(Circ_{Int}, Circ_{Int}.types[1])

# issue #786
type Node{T}
    v::Vector{Node}
end

@test is(Node{Int}.types[1].parameters[1], Node)

type Node2{T}
    v::Vector{Node2{T}}
end

@test is(Node2{Int}.types[1].parameters[1], Node2{Int})

type FooFoo{A,B} y::FooFoo{A} end

@test FooFoo{Int} <: FooFoo{Int,String}.types[1]


x = (2,3)
@test +(x...) == 5

# bits types
if WORD_SIZE == 64
    @test isa((()->Intrinsics.box(Ptr{Int8},Intrinsics.unbox(Int64,0)))(), Ptr{Int8})
else
    @test isa((()->Intrinsics.box(Ptr{Int8},Intrinsics.unbox(Int32,0)))(), Ptr{Int8})
end
@test isa(convert(Char,65), Char)

# conversions
function fooo()
    local x::Int8
    x = 1000
    x
end
@test int32(fooo()) == -24
function fooo_2()
    local x::Int8
    x = 1000
end
@test fooo_2() == 1000
function fooo_3()
    local x::Int8
    y = x = 1000
    @test x == -24
    y
end
@test fooo_3() == 1000
function foo()
    local x::Int8
    function bar()
        x = 1000
    end
    bar()
    x
end
@test int32(foo()) == -24

function bar{T}(x::T)
    local z::Complex{T}
    z = x
    z
end
@test bar(3.0) == Complex(3.0,0.0)

z = convert(Complex{Float64},2)
@test z == Complex(2.0,0.0)

# misc
fib(n) = n < 2 ? n : fib(n-1) + fib(n-2)
@test fib(20) == 6765

# static parameters
sptest1{T}(x::T, y::T) = 42
sptest1{T,S}(x::T, y::S) = 43
@test sptest1(1,2) == 42
@test sptest1(1,"b") == 43

sptest2{T}(x::T) = T
@test is(sptest2(:a),Symbol)

sptest3{T}(x::T) = y->T
let m = sptest3(:a)
    @test is(m(0),Symbol)
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
    @test c == 2
    dec()
    @test c == 1
    @test (()->c)() == 1

    fibb(n) = n < 2 ? n : fibb(n-1)+fibb(n-2)
    assert(fibb(10)==55)

    return (n->(c+=n), ()->c)
end
let T = clotest()
    (inc, C) = T
    inc(11)
    @test C() == 12
end

Yc(f) = (h->f(x->h(h)(x)))(h->f(x->h(h)(x)))
yfib = Yc(fib->(n->(n < 2 ? n : fib(n-1) + fib(n-2))))
@test yfib(20) == 6765

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
        @test glob_x == 2
        @test loc_x == 10
    end
    inner()
    inner2()
    @test glob_x == 88
    @test loc_x == 8
end
glotest()
@test glob_x == 88
@test loc_x == 10

# let - new variables, including undefinedness
function let_undef()
    first = true
    for i = 1:2
        let x
            if first; x=1; first=false; end
            x+1
        end
    end
end
@test_throws let_undef()

# const implies local in a local scope block
function const_implies_local()
    let
        x = 1
        local y
        let
            const x = 0
            y = x
        end
        x, y
    end
end
@test const_implies_local() === (1, 0)

a = cell(3)
for i=1:3
    let ii = i
        a[i] = x->x+ii
    end
end
@test a[1](10) == 11
@test a[2](10) == 12
@test a[3](10) == 13

# syntax
@test (true ? 1 : false ? 2 : 3) == 1

# tricky space sensitive syntax cases
@test [-1 ~1] == [(-1) (~1)]

# undefinedness
type UndefField
    field
    UndefField() = new()
end

begin
    local a
    a = cell(2)
    @test !isdefined(a,1) && !isdefined(a,2)
    a[1] = 1
    @test isdefined(a,1) && !isdefined(a,2)
    a = Array(Float64,1)
    @test isdefined(a,1)
    @test isdefined(a)
    @test_throws isdefined(a,2)

    @test isdefined("a",:data)
    a = UndefField()
    @test !isdefined(a, :field)
    @test_throws isdefined(a, :foo)

    @test_throws isdefined(2)
    @test_throws isdefined(2, :a)
    @test_throws isdefined("a", 2)
end

# dispatch
begin
    local foo, bar, baz
    foo(x::(Any...))=0
    foo(x::(Integer...))=1
    @test foo((:a,))==0
    @test foo(( 2,))==1

    bar{T}(x::(T,T,T,T))=1
    bar(x::(Any,Any,Any,Any))=2
    @test bar((1,1,1,1)) == 1
    @test bar((1,1,1,"a")) == 2
    @test bar((:a,:a,:a,:a)) == 1

    baz(::Type{Rational}) = 1
    baz{T}(::Type{Rational{T}}) = 2
    @test baz(Rational) == 1
    @test baz(Rational{Int}) == 2
end

begin
    local mytype
    function mytype(vec)
        convert(Vector{(ASCIIString, DataType)}, vec)
    end
    some_data = {("a", Int32), ("b", Int32)}
    @test isa(mytype(some_data),Vector{(ASCIIString, DataType)})
end

type MyArray{N} <: AbstractArray{Int, N}
end
begin
    local x
    x = MyArray{1}()
    foob(x::AbstractArray)=0
    foob{T}(x::AbstractVector{T})=1
    @test foob(x) == 1
end

begin
    local f, g, a
    f{T}(a::Vector{Vector{T}}) = a
    g{T}(a::Vector{Vector{T}}) = a
    a = Vector{Int}[]
    @test is(f(a), a)
    @test is(g(a), a)
end

type _AA{T}; a::T; end
typealias _AoA{T} _AA{_AA{T}}
begin
    local g, a
    g{T}(a::_AA{_AA{T}}) = a
    a = _AA(_AA(1))
    @test is(g(a),a)
end

# try/finally
begin
    after = 0
    b = try
        1+2
    finally
        after = 1
    end
    @test b == 3
    @test after == 1

    after = 0
    gothere = 0
    try
        try
            error(" ")
        finally
            after = 1
        end
        gothere = 1
    end
    @test after == 1
    @test gothere == 0

    after = 0
    b = try
        error(" ")
    catch
        42
    finally
        after = 1
    end
    @test b == 42
    @test after == 1

    glo = 0
    function retfinally()
        try
            return 5
        finally
            glo = 18
        end
    end
    @test retfinally() == 5
    @test glo == 18
end

# chained and multiple assignment behavior (issue #2913)
begin
    local x, a, b, c, d, e
    x = (a,b,b,b,e) = (1,2,3,4,5)
    @test x === (1,2,3,4,5)
    @test a == 1
    @test b == 4
    @test e == 5
    x = (a,b,b,e) = (1,2,3,4,5)
    @test x === (1,2,3,4,5)
    @test a == 1
    @test b == 3
    @test e == 4

    a = complex(1,2)
    b = 3
    b, a = a.re, b
    @test b == 1
    @test a == 3
    a = complex(1,2)
    b = 3
    a, b = b, a.re
    @test a == 3
    @test b == 1
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
    @test my_func([], M, M) == 0
end

begin
    local my_func, a, c
    my_func{T}(P::Vector{T}, Q::Vector{T}) = 0
    my_func{T}(x::T, P::Vector{T}) = 1
    # todo: this gives an ambiguity warning
    #my_func{T}(P::Vector{T}, x::T) = 2
    a = Int[3]
    c = Vector[a]

    @test my_func(c,c)==0
    @test_throws my_func(a,c)
end

begin
    local baar, foor, boor
    # issue #1131
    baar(x::DataType) = 0
    baar(x::UnionType) = 1
    baar(x::TypeConstructor) = 2
    @test baar(StridedArray) == 2
    @test baar(StridedArray.body) == 1
    @test baar(Vector) == 2
    @test baar(Vector.body) == 0

    boor(x) = 0
    boor(x::UnionType) = 1
    @test boor(StridedArray) == 0
    @test boor(StridedArray.body) == 1

    # issue #1202
    foor(x::UnionType) = 1
    @test_throws foor(StridedArray)
    @test foor(StridedArray.body) == 1
    @test_throws foor(StridedArray)
end

# issue #1153
type SI{m, s, kg}
    value::FloatingPoint
end

import Base.*

*{m1, m2, s1, s2, kg1, kg2}(x::SI{m1, s1, kg1}, y::SI{m2, s2, kg2}) = SI{m1 + m2, s1 + s2, kg1 + kg2}(x.value * y.value)

begin
    local a,b
    a = SI{0,0,1}(1.0) * SI{1,2,0}(2.0)
    b = SI{0,0,1}(1.0) * SI{1,-2,0}(2.0)
    @test typeof(a) === SI{1,2,1}
    @test typeof(b) === SI{1,-2,1}
end

# pull request 1270
begin
    local a,p, a2,p2
    a = [11,12,13]
    p = pointer(a)
    @test unsafe_load(p, 1) == 11
    unsafe_store!(p, 99, 2)
    @test a == [11,99,13]
    a2 = Any[101,102,103]
    p2 = pointer(a2)
    @test unsafe_load(p2) == 101
    @test_throws unsafe_store!(p2, 909, 3)
    @test a2 == [101,102,103]
end

@test unsafe_pointer_to_objref(ccall(:jl_call1, Ptr{Void}, (Any,Any),
                                     x -> x+1, 314158)) == 314159
@test unsafe_pointer_to_objref(pointer_from_objref(e+pi)) == e+pi

immutable FooBar
    foo::Int
    bar::Int
end
begin
    local X, p
    X = FooBar[ FooBar(3,1), FooBar(4,4) ]
    p = convert(Ptr{FooBar}, X)
    @test unsafe_load(p, 2) == FooBar(4,4)
    unsafe_store!(p, FooBar(7,3), 1)
    @test X[1] == FooBar(7,3)
end

# issue #1287, combinations of try, catch, return
begin
    local f, g

    function f()
        try
            return 1
        end
    end
    @test f() == 1

    function g()
        try
            error("badness")
        catch
            return 2
        end
    end
    @test g() == 2
end

# issue #1442
type S1442{T}
end

begin
    local f1442
    f1442(::DataType) = 1
    f1442{T}(::Type{S1442{T}}) = 2

    @test f1442(S1442{Int}) == 2
    @test f1442(DataType) == 1
end

# issue #1727
abstract Component

type Transform <: Component
  x
  y
  z

  Transform() = new(0, 0, 0)
end

type Body <: Component
  vel
  curr_force

  Body() = new(0, 0)
end

function NewEntity{ T<:Component }(components::Type{T}...)
  map((c)->c(), components)
end

@test_throws NewEntity(Transform, Transform, Body, Body)
@test isa(NewEntity(Transform, Transform), (Transform, Transform))
@test_throws NewEntity(Transform, Transform, Body, Body)

# issue #1826
let
    a = (1,2)
    a,b = a
    @test a==1 && b==2
end

# issue #1876
let
    tst = 1
    m1(i) = (tst+=1;i-1)
    x = [1:4]
    x[1:end] *= 2
    @test x == [2:2:8]
    x[m1(end)] += 3
    @test x == [2,4,9,8]
    @test tst == 2

    # issue #1886
    X = [1:4]
    r = Array(Range1{Int},1)
    r[1] = 2:3
    X[r...] *= 2
    @test X == [1,4,6,4]
end

# issue #1632
let
    f1632{R,S}(::R, ::S) = 1
    f1632{T}(  ::T, ::T) = 2
    @test f1632(1, 2) == 2
    @test f1632(:a, 2) == 1
    g1632{T}(  ::T, ::T) = 2
    g1632{R,S}(::R, ::S) = 1
    @test g1632(1, 2) == 2
    @test g1632(:a, 2) == 1
end

# issue #1628
type I1628{X}
    x::X
end
let
    # here the potential problem is that the run-time value of static
    # parameter X in the I1628 constructor is (DataType,DataType),
    # but type inference will track it more accurately as
    # (Type{Integer}, Type{Int}).
    f1628() = I1628((Integer,Int))
    @test isa(f1628(), I1628{(DataType,DataType)})
end

let
    fT{T}(x::T) = T
    @test fT(Any) === DataType
    @test fT(Int) === DataType
    @test fT(Type{Any}) === DataType
    @test fT(Type{Int}) === DataType

    ff{T}(x::Type{T}) = T
    @test ff(Type{Any}) === Type{Any}
    @test ff(Type{Int}) === Type{Int}
    @test ff(Any) === Any
    @test ff(Int) === Int
end

# issue #2098
let
    i2098() = (c={2.0};[1:1:c[1]])
    @test isequal(i2098(), [1.0,2.0])
end

# issue #2161
let
    i2161_1() = promote(2,2,2.0,2)
    i2161_2() = i2161_1()[1]
    @test i2161_2() === 2.0
end

# issue #2169
let
    i2169{T}(a::Array{T}) = typemin(T)
    @test invoke(i2169,(Array,),Int8[1]) === int8(-128)
end

# issue #2365
type B2365{T}
     v::Union(T, Nothing)
end
@test B2365{Int}(nothing).v === nothing
@test B2365{Int}(0).v === 0

# issue #2352
Sum=0.0; for n=1:2:10000
Sum += -1/n + 1/(n+1)
end
@test Sum < -0.69

include("test_sourcepath.jl")

# issue #2509
immutable Foo2509; foo::Int; end
@test Foo2509(1) != Foo2509(2)
@test Foo2509(42) == Foo2509(42)

# issue #2517
immutable Foo2517; end
@test repr(Foo2517()) == "Foo2517()"
@test repr(Array(Foo2517,1)) == "[Foo2517()]"
@test Foo2517() === Foo2517()

# issue #1474
type X1474{a,b} end
begin
    local Y
    Y{A,B}(::Type{X1474{A,B}}) = 1
    Y{A}(::Type{X1474{A}}) = 2
    Y(::Type{X1474}) = 3
    @test Y(X1474) == 3
    @test Y(X1474{Int}) == 2
    @test Y(X1474{Int,String}) == 1
end

# issue #2562
type Node2562{T}
    value::T
    Node2562(value::T) = new(value)
end
Node2562{T}(value::T, args...) = Node2562{T}(value, args...)
makenode2562(value) = Node2562(value)
@test isa(Node2562(0), Node2562)
@test isa(makenode2562(0), Node2562)

# issue #2619
type I2619{T}
    v::T
    I2619(v) = new(convert(T,v))
end
bad2619 = false
function i2619()
    global e2619 = try
        I2619{Float64}(0.0f)
        global bad2619 = true
    catch _e
        _e
    end
end
i2619()
@test !bad2619
@test isa(e2619,ErrorException) && e2619.msg == "f not defined"

# issue #2919
typealias Foo2919 Int
type Baz2919; Foo2919::Foo2919; end
@test Baz2919(3).Foo2919 === 3

# issue #2959
@test 1.0:1.5 == 1.0:1.0:1.5 == 1.0:1.0
@test 1.0:(.3-.1)/.1 == 1.0:2.0

# issue #2982
module M2982
abstract U
macro bad(Y)
    quote
        type $(esc(Y)) <: U
        end
    end
end
export @bad
end

@M2982.bad(T2982)
@test T2982.super === M2982.U

# issue #3182
f3182{T}(::Type{T}) = 0
f3182(x) = 1
function g3182(t::DataType)
    # tricky thing here is that DataType is a concrete type, and a
    # subtype of Type, but we cannot infer the T in Type{T} just
    # by knowing (at compile time) that the argument is a DataType.
    # however the ::Type{T} method should still match at run time.
    f3182(t)
end
@test g3182(Complex) == 0

# issue #3221
let x = fill(nothing, 1)
    @test_throws x[1] = 1
end

# issue #3220
function x3220()
    a = [1]
    a::Vector{Int} += [1]
end
@test x3220() == [2]

# issue #3471
function f3471(y)
    convert(Array{typeof(y[1]),1}, y)
end
@test isa(f3471({1.0,2.0}), Vector{Float64})

# issue #3729
typealias A3729{B} Vector{Vector{B}}
typealias C3729{D} Vector{Vector{D}}
@test Vector{Vector{Int}} === A3729{Int} === C3729{Int}

# issue #3789
x3789 = 0
while(all([false for idx in 1:10]))
    x3789 = 1
end
@test x3789 == 0
