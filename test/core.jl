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
@test !(Type{None} <: Type{Int32})
@test !(Vector{Float64} <: Vector{Union(Float64,Float32)})
@test is(None, typeintersect(Vector{Float64},Vector{Union(Float64,Float32)}))

@test !isa(Array,Type{Any})
@test Type{Complex} <: DataType
@test isa(Complex,Type{Complex})
@test !(Type{Ptr{None}} <: Type{Ptr})
@test !(Type{Rational{Int}} <: Type{Rational})
let T = TypeVar(:T,true)
    @test !is(None, typeintersect(Array{None},AbstractArray{T}))
    @test  is(None, typeintersect((Type{Ptr{Uint8}},Ptr{None}),
                                  (Type{Ptr{T}},Ptr{T})))
    @test !(Type{T} <: TypeVar)

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
    @test typeintersect((T,T), (Union(Float64,Int64),Int64)) == (Int64,Int64)
    @test typeintersect((T,T), (Int64,Union(Float64,Int64))) == (Int64,Int64)
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
@test !(Type{Array{Integer}} <: Type{AbstractArray{Integer}})
@test !(Type{Array{Integer}} <: Type{Array{TypeVar(:T,Integer)}})
@test is(None, typeintersect(Type{Function},UnionType))
@test is(Type{Int32}, typeintersect(Type{Int32},DataType))
@test !(Type <: TypeVar)
@test !is(None, typeintersect(DataType, Type))
@test !is(None, typeintersect(UnionType, Type))
@test !is(None, typeintersect(DataType, Type{Int}))
@test !is(None, typeintersect(DataType, Type{TypeVar(:T,Int)}))
@test !is(None, typeintersect(DataType, Type{TypeVar(:T,Integer)}))

@test isa(Int,Type{TypeVar(:T,Number)})
@test !isa(DataType,Type{TypeVar(:T,Number)})
@test DataType <: Type{TypeVar(:T,Type)}

@test isa((),Type{()})
@test (DataType,) <: Type{TypeVar(:T,Tuple)}
@test !((Int,) <: Type{TypeVar(:T,Tuple)})
@test isa((Int,),Type{TypeVar(:T,Tuple)})

@test !isa(Type{(Int,Int)},Tuple)
@test Type{(Int,Int)} <: Tuple
@test Type{(Int,)} <: (DataType,)

# this is fancy: know that any type T<:Number must be either a DataType or a UnionType
@test Type{TypeVar(:T,Number)} <: Union(DataType,UnionType)
@test !(Type{TypeVar(:T,Number)} <: DataType)
@test Type{TypeVar(:T,Tuple)} <: Union(Tuple,UnionType)
@test !(Type{TypeVar(:T,Tuple)} <: Union(DataType,UnionType))

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

@test Qux_{Int}.super <: Sup_
@test is(Qux_{Int}, Qux_{Int}.super.parameters[1])
@test is(Qux_{Int}.super.parameters[2], Int)
@test Qux_{Char}.super <: Sup_
@test is(Qux_{Int}, Qux_{Char}.super.parameters[1])
@test is(Qux_{Char}.super.parameters[2], Char)

@test Qux_.super.parameters[1].super <: Sup_
@test is(Qux_{Int}, Qux_.super.parameters[1].super.parameters[1])
@test is(Int, Qux_.super.parameters[1].super.parameters[2])

type Foo_{T} x::Foo_{Int} end

@test is(Foo_.types[1], Foo_{Int})
@test is(Foo_.types[1].types[1], Foo_{Int})

type Circ_{T} x::Circ_{T} end
@test is(Circ_{Int}, Circ_{Int}.types[1])

# issue #3890
type A3890{T1}
    x::Matrix{Complex{T1}}
end
@test A3890{Float64}.types[1] === Array{Complex{Float64},2}
# make sure the field type Matrix{Complex{T1}} isn't cached
type B3890{T2}
    x::Matrix{Complex{T2}}
end
@test B3890{Float64}.types[1] === Array{Complex{Float64},2}

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
    @test !isdefined(a,2)

    @test isdefined("a",:data)
    a = UndefField()
    @test !isdefined(a, :field)
    @test !isdefined(a, :foo)
    @test !isdefined(2, :a)

    @test_throws isdefined(2)
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
    unsafe_store!(p2, 909, 3)
    @test a2 == [101,102,909]
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
begin
    local Sum, n
    Sum=0.0; for n=1:2:10000
        Sum += -1/n + 1/(n+1)
    end
    @test Sum < -0.69
end

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

# issue #3852
function f3852()
    local x
    for i = 1:10
        x = identity
    end
    x("hi")
end
@test f3852() == "hi"

# issue #3821
function f3821()
    p = []
    [x for x in p]
end
@test isa(f3821(), Array)

# issue #4075
immutable Foo4075
    x::Int64
    y::Float64
end

function foo4075(f::Foo4075, s::Symbol)
    x = getfield(f,s)
    gc()
    x
end

@test isa(foo4075(Foo4075(int64(1),2.0),:y), Float64)
# very likely to segfault the second time if this is broken
@test isa(foo4075(Foo4075(int64(1),2.0),:y), Float64)

# issue #3167
function foo(x)
    ret=Array(typeof(x[1]), length(x))
    for j = 1:length(x)
        ret[j] = x[j]
    end
    return ret
end
x = Array(Union(Dict{Int64,String},Array{Int64,3},Number,String,Nothing), 3)
x[1] = 1.0
x[2] = 2.0
x[3] = 3.0
foo(x) == [1.0, 2.0, 3.0]

# issue #4115
type Foo4115
end
typealias Foo4115s NTuple{3,Union(Foo4115,Type{Foo4115})}
baz4115(x::Foo4115s) = x
@test baz4115((Foo4115,Foo4115,Foo4115())) === (Foo4115,Foo4115,Foo4115())

# issue #4129
type Foo4129; end

abstract Bar4129

type Bar41291 <: Bar4129
    f::Foo4129
end
type Bar41292 <: Bar4129
    f::Foo4129
end

type Baz4129
    b::Bar4129
end

foo4129(a::Baz4129,c::Foo4129,b::Bar4129,x::ANY,y) = (a,b,c,x,y)
foo4129(a::Baz4129,b::Bar41291,args...) = foo4129(a,b.f,b,args...)
foo4129(a::Baz4129,b::Bar41292,args...) = foo4129(a,b.f,b,args...)
foo4129(a::Baz4129,args...)         = foo4129(a,a.b,args...)

@test isa(foo4129(Baz4129(Bar41291(Foo4129())),1,2), (Baz4129,Bar4129,Foo4129,Int,Int))

# issue #4141
type Vertex4141{N,T}; end
type Face4141{V}; end
type Hull4141{F<:Face4141}; end

g4141(N,T) = Hull4141{Face4141{Vertex4141{N,T}}}()
@test isa(g4141(4,Int), Hull4141{Face4141{Vertex4141{4,Int}}})

# issue #4154
type MyType4154{T}
    a1::T
    a2
end

foo4154(x) = MyType4154(x, {})
h4154() = typeof(foo4154(rand(2,2,2)))
g4154() = typeof(foo4154(rand(2,2,2,2,2,2,2,2,2)))

@test h4154() === MyType4154{Array{Float64,3}}
@test g4154() === MyType4154{Array{Float64,9}}

# issue #4208
type a4208
    a4208
end
@test isa(a4208(5),a4208)
type b4208
    b4208() = (local b4208=1;new())
end
@test isa(b4208(),b4208)

# make sure convert_default error isn't swallowed by typeof()
convert_default_should_fail_here() = similar([1],typeof(zero(typeof(rand(2,2)))))
@test_throws convert_default_should_fail_here()

# issue #4343
@test_throws Array{Float64}{Int, 2}

type Foo4376{T}
    x
    Foo4376(x::T) = new(x)
    Foo4376(a::Foo4376{Int}) = new(a.x)
end

@test isa(Foo4376{Float32}(Foo4376{Int}(2)), Foo4376{Float32})
@test_throws Foo4376{Float32}(Foo4376{Float32}(2.0f0))

type _0_test_ctor_syntax_
    _0_test_ctor_syntax_{T<:String}(files::Vector{T},step) = 0
end

# issue #4413
type A4413 end
type B4413 end
type C4413 end
f4413(::Union(A4413, B4413, C4413)) = "ABC"
f4413(::Union(A4413, B4413)) = "AB"
g4413(::Union(A4413, C4413)) = "AC"
g4413(::Union(A4413, B4413, C4413)) = "ABC"

@test f4413(A4413()) == "AB" && f4413(B4413()) == "AB"
@test g4413(A4413()) == "AC" && g4413(C4413()) == "AC"

# issue #4482
# what happens here: the method cache logic wants to widen the type of a
# tuple argument, but it shouldn't do that for an argument that a static
# parameter depends on.
f4482{T}(x::T) = T
@test f4482((Ptr,Ptr)) === (DataType,DataType)
@test f4482((Ptr,))    === (DataType,)

# issue #4486
try
    # note: this test expression must run at the top level,
    # in the interpreter.
    (function() end)(1)
    # should throw an argument count error
    @test false
end

# issue #4526
f4526(x) = isa(x.a, Nothing)
@test_throws f4526(1)
@test_throws f4526(im)
@test_throws f4526(1+2im)

# issue #4528
function f4528(A, B)
    if A
        reinterpret(Uint64, B)
    end
end
@test f4528(false, int32(12)) === nothing
@test_throws f4528(true, int32(12))

# issue #4518
f4518(x, y::Union(Int32,Int64)) = 0
f4518(x::ASCIIString, y::Union(Int32,Int64)) = 1
@test f4518("",1) == 1

# issue #4581
bitstype 64 Date4581{T}
let
    x = Intrinsics.box(Date4581{Int}, Intrinsics.unbox(Int64,int64(1234)))
    xs = Date4581[x]
    ys = copy(xs)
    @test ys !== xs
    @test ys == xs
end

# issue #4645
i4645(x) = (println(zz); zz = x; zz)
@test_throws i4645(4)

# issue #4505
let
    g4505{X}(::X) = 0
    @test g4505(0) == 0
end
@test !isdefined(:g4505)

# issue #4681
# ccall should error if convert() returns something of the wrong type
type Z4681
    x::Ptr{Void}
    Z4681() = new(C_NULL)
end
Base.convert(::Type{Ptr{Z4681}},b::Z4681) = b.x
@test_throws ccall(:printf,Int,(Ptr{Uint8},Ptr{Z4681}),"",Z4681())

# issue #4479
f4479(::Real,c) = 1
f4479(::Int, ::Int, ::Bool) = 2
f4479(::Int, x, a...) = 0
@test f4479(1,1,true) == 2

# issue #4688
a4688(y) = "should be unreachable by calling b"
b4688(y) = "not an Int"
begin
    a4688(y::Int) = "an Int"
    let x = true
        b4688(y::Int) = x == true ? a4688(y) : a4688(y)
    end
end
@test b4688(1) == "an Int"

# issue #4731
type SIQ{A,B} <: Number
    x::A
end
import Base: promote_rule
promote_rule{T,T2,S,S2}(A::Type{SIQ{T,T2}},B::Type{SIQ{S,S2}}) = SIQ{promote_type(T,S)}
@test_throws promote_type(SIQ{Int},SIQ{Float64})

# issue #4675
f4675(x::StridedArray...) = 1
f4675{T}(x::StridedArray{T}...) = 2
@test f4675(zeros(50,50), zeros(50,50)) == 2
g4675{T}(x::StridedArray{T}...) = 2
g4675(x::StridedArray...) = 1
@test g4675(zeros(50,50), zeros(50,50)) == 2

# issue #4771
module Lib4771
export @make_closure
macro make_closure()
    quote
        f = (x)->1
    end
end
end # module
@test (Lib4771.@make_closure)(0) == 1

# issue #4805
abstract IT4805{N, T}

let
    T = TypeVar(:T,Int)
    N = TypeVar(:N)
    @test typeintersect(Type{IT4805{1,T}}, Type{TypeVar(:_,IT4805{N,Int})}) != None
end

let
    test0{T <: Int64}(::Type{IT4805{1, T}}, x) = x
    test1() = test0(IT4805{1, Int64}, 1)
    test2() = test0(IT4805{1+0, Int64}, 1)
    test3(n) = test0(IT4805{n, Int64}, 1)

    @test test1() == 1
    @test test2() == 1
    @test test3(1) == 1
    @test_throws test3(2)
end
