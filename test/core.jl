# test core language features

const Bottom = Union()

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
@test !(Type{Bottom} <: Type{Int32})
@test !(Vector{Float64} <: Vector{Union(Float64,Float32)})
@test is(Bottom, typeintersect(Vector{Float64},Vector{Union(Float64,Float32)}))

@test !isa(Array,Type{Any})
@test Type{Complex} <: DataType
@test isa(Complex,Type{Complex})
@test !(Type{Ptr{Bottom}} <: Type{Ptr})
@test !(Type{Rational{Int}} <: Type{Rational})
let T = TypeVar(:T,true)
    @test !is(Bottom, typeintersect(Array{Bottom},AbstractArray{T}))
    @test  is(Bottom, typeintersect((Type{Ptr{UInt8}},Ptr{Bottom}),
                                  (Type{Ptr{T}},Ptr{T})))
    @test !(Type{T} <: TypeVar)

    @test isequal(typeintersect((Range{Int},(Int,Int)),(AbstractArray{T},Dims)),
                  (Range{Int},(Int,Int)))

    @test isequal(typeintersect((T, AbstractArray{T}),(Number, Array{Int,1})),
                  (Int, Array{Int,1}))

    @test isequal(typeintersect((T, AbstractArray{T}),(Int, Array{Number,1})),
                  (Int, Array{Number,1}))

    @test isequal(typeintersect((T, AbstractArray{T}),(Any, Array{Number,1})),
                  (Number, Array{Number,1}))
    @test !is(Bottom, typeintersect((Array{T}, Array{T}), (Array, Array{Any})))
    f47{T}(x::Vector{Vector{T}}) = 0
    @test_throws MethodError f47(Array(Vector,0))
    @test f47(Array(Vector{Int},0)) == 0
    @test typeintersect((T,T), (Union(Float64,Int64),Int64)) == (Int64,Int64)
    @test typeintersect((T,T), (Int64,Union(Float64,Int64))) == (Int64,Int64)

    TT = TypeVar(:T,Top)
    S = TypeVar(:S,true); N = TypeVar(:N,true)
    @test typeintersect(Type{TypeVar(:T,Array{TT,1})},Type{Array{S,N}}) == Type{Array{S,1}}
    # issue #5359
    @test typeintersect((Type{Array{T,1}},Array{T,1}),
                        (Type{AbstractVector},Vector{Int})) === Bottom
    # issue #5559
    @test typeintersect((Type{Vector{Complex128}}, AbstractVector),
                        (Type{Array{T,N}}, Array{S,N})) == (Type{Vector{Complex128}},Vector)
    @test typeintersect((Type{Vector{Complex128}}, AbstractArray),
                        (Type{Array{T,N}}, Array{S,N})) == (Type{Vector{Complex128}},Vector)

    @test typeintersect(Type{Array{T}}, Type{AbstractArray{T}}) === Bottom

    @test typeintersect(Type{(Bool,Int...)}, Type{(T...)}) === Bottom
    @test typeintersect(Type{(Bool,Int...)}, Type{(T,T...)}) === Bottom

    @test typeintersect((Rational{T},T), (Rational{Integer},Int)) === (Rational{Integer},Int)
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

    @test isequal(typeintersect((Type{Void},Type{Void}), Type{NTuple{N,Void}}),
                  Type{(Void,Void)})
end
@test is(Bottom, typeintersect(Type{Any},Type{Complex}))
@test is(Bottom, typeintersect(Type{Any},Type{TypeVar(:T,Real)}))
@test !(Type{Array{Integer}} <: Type{AbstractArray{Integer}})
@test !(Type{Array{Integer}} <: Type{Array{TypeVar(:T,Integer)}})
@test is(Bottom, typeintersect(Type{Function},UnionType))
@test is(Type{Int32}, typeintersect(Type{Int32},DataType))
@test !(Type <: TypeVar)
@test !is(Bottom, typeintersect(DataType, Type))
@test !is(Bottom, typeintersect(UnionType, Type))
@test !is(Bottom, typeintersect(DataType, Type{Int}))
@test !is(Bottom, typeintersect(DataType, Type{TypeVar(:T,Int)}))
@test !is(Bottom, typeintersect(DataType, Type{TypeVar(:T,Integer)}))

@test typeintersect((Int...), (Bool...)) === ()
@test typeintersect(Type{(Int...)}, Type{(Bool...)}) === Bottom
@test typeintersect((Bool,Int...), (Bool...)) === (Bool,)

let T = TypeVar(:T,Union(Float32,Float64))
    @test typeintersect(AbstractArray, Matrix{T}) == Matrix{T}
end
let T = TypeVar(:T,Union(Float32,Float64),true)
    @test typeintersect(AbstractArray, Matrix{T}) == Matrix{T}
end

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
@test !isa((Int,), Type{(Int...,)})
@test !isa((Int,), Type{(Any...,)})

@test !issubtype(Type{Array{TypeVar(:T,true)}}, Type{Array})

# issue #6561
@test issubtype(Array{Tuple}, Array{NTuple})
@test issubtype(Array{(Any...)}, Array{NTuple})
@test !issubtype(Array{(Int...)}, Array{NTuple})
@test !issubtype(Array{(Int,Int)}, Array{NTuple})
@test issubtype(Type{(Void,)}, (Type{Void},))
@test issubtype((Type{Void},),Type{(Void,)})

# this is fancy: know that any type T<:Number must be either a DataType or a UnionType
@test Type{TypeVar(:T,Number)} <: Union(DataType,UnionType)
@test !(Type{TypeVar(:T,Number)} <: DataType)
@test Type{TypeVar(:T,Tuple)} <: Union(Tuple,UnionType)
@test !(Type{TypeVar(:T,Tuple)} <: Union(DataType,UnionType))

@test !is(Bottom, typeintersect((DataType,DataType),Type{TypeVar(:T,(Number,Number))}))
@test !is(Bottom, typeintersect((DataType,UnionType),Type{(Number,Bottom)}))

# issue #2997
let T = TypeVar(:T,Union(Float64,Array{Float64,1}),true)
    @test typeintersect(T,Real) === Float64
end

# join
@test typejoin(Int8,Int16) === Signed
@test typejoin(Int,AbstractString) === Any
@test typejoin(Array{Float64},BitArray) <: AbstractArray
@test typejoin(Array{Bool},BitArray) <: AbstractArray{Bool}
@test typejoin((Int,Int8),(Int8,Float64)) === (Signed,Real)
@test Base.typeseq(typejoin((ASCIIString,ASCIIString),(UTF8String,ASCIIString),
                            (ASCIIString,UTF8String),(Int,ASCIIString,Int)),
                   (Any,AbstractString,Int...))
@test Base.typeseq(typejoin((Int8,Int...),(Int8,Int8)),
                   (Int8,Signed...))
@test Base.typeseq(typejoin((Int8,Int...),(Int8,Int8...)),
                   (Int8,Signed...))
@test Base.typeseq(typejoin((Int8,UInt8,Int...),(Int8,Int8...)),
                   (Int8,Integer...))
@test Base.typeseq(typejoin(Union(Int,AbstractString),Int), Union(Int,AbstractString))
@test Base.typeseq(typejoin(Union(Int,AbstractString),Int8), Any)

@test promote_type(Bool,Bottom) === Bool

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

@test FooFoo{Int} <: FooFoo{Int,AbstractString}.types[1]


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
    x = 100
    x
end
@test fooo() === convert(Int8,100)
function fooo_2()
    local x::Int8
    x = 100
end
@test fooo_2() === 100
function fooo_3()
    local x::Int8
    y = x = 100
    @test isa(x,Int8)
    y
end
@test fooo_3() === 100
function foo()
    local x::Int8
    function bar()
        x = 100
    end
    bar()
    x
end
@test foo() === convert(Int8,100)

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

# issue #7272
@test expand(parse("let
              global x = 2
              local x = 1
              end")) == Expr(:error, "variable \"x\" declared both local and global")

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
@test_throws UndefVarError let_undef()

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

# ? syntax
@test (true ? 1 : false ? 2 : 3) == 1

# issue #7252
begin
    local a
    1 > 0 ? a=2 : a=3
    @test a == 2
    1 < 0 ? a=2 : a=3
    @test a == 3
end

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

    a = UndefField()
    @test !isdefined(a, :field)
    @test !isdefined(a, :foo)
    @test !isdefined(2, :a)

    @test  isdefined("a",:data)
    @test  isdefined("a", 1)
    @test !isdefined("a", 2)

    @test_throws TypeError isdefined(2)
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
    some_data = Any[("a", Int32), ("b", Int32)]
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

    @test try error() end === nothing
end

# finalizers
let
    A = [1]
    x = 0
    finalizer(A, a->(x+=1))
    finalize(A)
    @test x == 1
    A = 0
    gc(); gc()
    @test x == 1
end

# issue #7307
function test7307(a, ret)
    try
        try
            ret && return a
        finally
            push!(a, "inner")
        end
    finally
        push!(a, "outer")
    end
    return a
end
@test test7307([], true) == ["inner","outer"]
@test test7307([], false) == ["inner","outer"]

# issue #8277
function test8277(a)
    i = 0
    for j=1:2
        try
            if i == 0
                push!(a,0)
            end
            i += 1
            error()
        catch
        end
    end
end
let a = []
    test8277(a)
    @test length(a) == 1
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

# accessing fields by index
begin
    local z = complex(3, 4)
    v = Int[0,0]
    for i=1:2
        v[i] = getfield(z, i)
    end
    @test v == [3,4]
    @test_throws BoundsError getfield(z, -1)
    @test_throws BoundsError getfield(z, 0)
    @test_throws BoundsError getfield(z, 3)

    strct = LoadError("", 0, "")
    setfield!(strct, 2, 8)
    @test strct.line == 8
    setfield!(strct, 3, "hi")
    @test strct.error == "hi"
    setfield!(strct, 1, "yo")
    @test strct.file == "yo"
    @test_throws BoundsError getfield(strct, 10)
    @test_throws BoundsError setfield!(strct, 0, "")
    @test_throws BoundsError setfield!(strct, 4, "")
end

# allow typevar in Union to match as long as the arguments contain
# sufficient information
# issue #814
begin
    local MatOrNot, my_func, M
    typealias MatOrNot{T} Union(AbstractMatrix{T}, Vector{Union()})
    my_func{T<:Real}(A::MatOrNot{T}, B::MatOrNot{T}, C::MatOrNot{T}) = 0
    M = [ 2. 1. ; 1. 1. ]
    @test my_func(Union()[], M, M) == 0
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
    @test my_func(a,c)==1
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
    @test_throws MethodError foor(StridedArray)
    @test foor(StridedArray.body) == 1
    @test_throws MethodError foor(StridedArray)
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

# pointer arithmetic
begin
   local a,b,c
   a = C_NULL
   b = C_NULL + 1
   c = C_NULL - 1

   @test a != b != c
   @test UInt(a) == 0
   @test UInt(b) == 1
   @test UInt(c) == typemax(UInt)

   @test b - a == -(a - b) == 1
   @test c - a == -(a - c) == typemax(UInt)
   @test c - b == -(b - c) == typemax(UInt) - 1
   @test a < b < c
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

begin
    local a, aa
    a = [1,2,3]
    aa = pointer_to_array(pointer(a), length(a))
    @test aa == a
    aa = pointer_to_array(pointer(a), (length(a),))
    @test aa == a
    aa = pointer_to_array(pointer(a), uint(length(a)))
    @test aa == a
    aa = pointer_to_array(pointer(a), uint16(length(a)))
    @test aa == a
    @test_throws ErrorException pointer_to_array(pointer(a), -3)
end

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

@test_throws MethodError NewEntity(Transform, Transform, Body, Body)
@test isa(NewEntity(Transform, Transform), (Transform, Transform))
@test_throws MethodError NewEntity(Transform, Transform, Body, Body)

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
    r = Array(UnitRange{Int},1)
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
    i2098() = begin
        c = Any[2.0]
        [1:1:c[1]]
    end
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
     v::Union(T, Void)
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
    @test Y(X1474{Int,AbstractString}) == 1
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
@test isa(e2619,UndefVarError) && e2619.var === :f

# issue #2919
typealias Foo2919 Int
type Baz2919; Foo2919::Foo2919; end
@test Baz2919(3).Foo2919 === 3

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
    @test_throws MethodError x[1] = 1
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
@test isa(f3471(Any[1.0,2.0]), Vector{Float64})

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
x = Array(Union(Dict{Int64,AbstractString},Array{Int64,3},Number,AbstractString,Void), 3)
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

foo4154(x) = MyType4154(x, [])
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
@test_throws MethodError convert_default_should_fail_here()

# issue #4343
@test_throws ErrorException Array{Float64}{Int, 2}

type Foo4376{T}
    x
    Foo4376(x::T) = new(x)
    Foo4376(a::Foo4376{Int}) = new(a.x)
end

@test isa(Foo4376{Float32}(Foo4376{Int}(2)), Foo4376{Float32})

type _0_test_ctor_syntax_
    _0_test_ctor_syntax_{T<:AbstractString}(files::Vector{T},step) = 0
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
f4526(x) = isa(x.a, Void)
@test_throws ErrorException f4526(1)
@test_throws ErrorException f4526(im)
@test_throws ErrorException f4526(1+2im)

# issue #4528
function f4528(A, B)
    if A
        reinterpret(UInt64, B)
    end
end
@test f4528(false, int32(12)) === nothing
@test_throws ErrorException f4528(true, int32(12))

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

# issue #6591
function f6591(d)
    Intrinsics.box(Int64, d)
    (f->f(d))(identity)
end
let d = Intrinsics.box(Date4581{Int}, int64(1))
    @test isa(f6591(d), Date4581)
end

# issue #4645
i4645(x) = (println(zz); zz = x; zz)
@test_throws UndefVarError i4645(4)

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
@test_throws TypeError ccall(:printf,Int,(Ptr{UInt8},Ptr{Z4681}),"",Z4681())

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
@test_throws ErrorException promote_type(SIQ{Int},SIQ{Float64})

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
    T = TypeVar(:T,Int,true)
    N = TypeVar(:N,true)
    @test typeintersect(Type{IT4805{1,T}}, Type{TypeVar(:_,IT4805{N,Int})}) != Bottom
end

let
    test0{T <: Int64}(::Type{IT4805{1, T}}, x) = x
    test1() = test0(IT4805{1, Int64}, 1)
    test2() = test0(IT4805{1+0, Int64}, 1)
    test3(n) = test0(IT4805{n, Int64}, 1)

    @test test1() == 1
    @test test2() == 1
    @test test3(1) == 1
    @test_throws MethodError test3(2)
end

# issue #4873
macro myassert4873(ex)
    :($ex ? nothing : error("Assertion failed: ", $(string(ex))))
end
x4873 = 1
@myassert4873 (x -> x)(x4873) == 1

# issue from IRC
function invalid_tupleref()
    A = (1, "2", 3.0)
    try
        return A[0]
    catch
        return true
    end
end
@test invalid_tupleref()==true

# issue #5150
f5150(T) = Array(Rational{T},1)
@test typeof(f5150(Int)) === Array{Rational{Int},1}


# issue #5165
bitstype 64 T5165{S}
make_t(x::Int64) = Base.box(T5165{Void}, Base.unbox(Int64, x))
xs5165 = T5165[make_t(int64(1))]
b5165 = IOBuffer()
for x in xs5165
    println(b5165, x)   # segfaulted
end

# support tuples as type parameters

type TupleParam{P}
    x::Bool
end

function tupledispatch(a::TupleParam{(1,:a)})
    a.x
end

let
    # tuples can be used as type params
    t1 = TupleParam{(1,:a)}(true)
    t2 = TupleParam{(1,:b)}(true)

    # tuple type params can't contain invalid type params
    @test_throws TypeError t3 = TupleParam{(1,"nope")}(true)

    # dispatch works properly
    @test tupledispatch(t1) == true
    @test_throws MethodError tupledispatch(t2)
end

# issue #5254
f5254{T}(::Type{T}, b::T) = 0
f5254(a, b) = 1
@test f5254(Bottom, 1) == 1

# evaluate arguments left-to-right, including assignments. issue #4990
let
    i = 0
    @test (i, i+=1, i+=1) === (0, 1, 2)
    @test i == 2
    x = 65
    @test [x, x|=0x20] == [65, 97]
end

# issue #5312
let
    local x = 0
    global incr5312, foo5312
    incr5312() = (x+=1; nothing)
    foo5312() = (incr5312(),)
    @test foo5312() === (nothing,)
    @test x == 1
end

# issue #5319
cnvt(T, x) = convert_default(T, x, cnvt)
cnvt{S, T, N}(::Type{Array{S, N}}, x::Array{T, N}) = convert(Array{S}, x)

function tighttypes!(adf)
    T = Bottom
    tt = Any[Int]
    for t in tt
        T = typejoin(T, t)
    end
    cnvt(Vector{T}, adf[1])
end

@test isequal(tighttypes!(Any[Any[1.0,2.0],]), [1,2])

# issue #5142
bitstype 64 Int5142
function h5142(a::Bool)
    x=a ? (int64(0),reinterpret(Int5142,int64(0))) : (int64(1),reinterpret(Int5142,int64(1)))
    x[2]::Int5142
end
function h5142(a::Int)
    x=(int64(0),reinterpret(Int5142,int64(0)))
    x[a]::Int5142
end
h5142(true)
@test_throws TypeError h5142(1)
h5142(2)

bitstype 8 Int5142b
function h5142b(a::Int)
    x=((int8(1),int8(2)),(reinterpret(Int5142b,int8(3)),reinterpret(Int5142b,int8(4))))
    x[a]::(Int8,Int8)
end
h5142b(1)
@test_throws TypeError h5142b(2)

# accessing bits tuples of structs
function test_bits_tuples()
    a = (complex(1,2),complex(1,3));s=0
    for i=1:10
        s += a[rand(1:2)]
    end
    s
end
@test real(test_bits_tuples()) == 10

# issue #5374
type FileObj5374
    io::IO
end
function read_file5374(fileobj)
    read(fileobj.io, Float32)
end
@test isa(read_file5374(FileObj5374(IOBuffer(UInt8[0,0,0,0]))), Float32)

# issue #5457
function f5457(obj_ptr::Ptr{Float64}, f)
    new_obj = convert(Float64, f(1.0))
    unsafe_store!(obj_ptr, new_obj)
    return int32(1)
end
let
    a = [1.0]
    f5457(pointer(a,1), sin)
end

# issue #5584
# this is an intermittent memory bug, but this code is very likely to trigger it
mapshape_5584{N}(s1::NTuple{N,Int}, s2::NTuple{N,Int}) =
    (s1 == s2 || error("Argument dimensions are not map-compatible."); s1)
function f5584()
    for i = 1:1000000
        a = rand(1:1000, 3)
        # the bug was a failure to root these tuples
        mapshape_5584(tuple(a...), tuple(a...))
    end
end
f5584()

# issue #5884

type Polygon5884{T<:Real}
    points::Vector{Complex{T}}
end

function test5884()
    star = Array(Polygon5884,(3,))
    star[1] = Polygon5884([Complex(1.0,1.0)])
    p1 = star[1].points[1]
    @test p1 == Complex(1.0,1.0)
    @test p1.re == 1.0
    @test star[1].points[1].re == 1.0
end
test5884()

# issue #5924
let
    function Test()
        func = function () end
        func
    end
    @test Test()() === nothing
end

# issue #5906

abstract Outer5906{T}

immutable Inner5906{T}
   a:: T
end

immutable Empty5906{T} <: Outer5906{T}
end

immutable Hanoi5906{T} <: Outer5906{T}
    a::T
    succ :: Outer5906{Inner5906{T}}
    Hanoi5906(a) = new(a, Empty5906{Inner5906{T}}())
end

function f5906{T}(h::Hanoi5906{T})
    if isa(h.succ, Empty5906) return end
    f5906(h.succ)
end

# can cause infinite recursion in type inference via instantiation of
# the type of the `succ` field
@test f5906(Hanoi5906{Int}(1)) === nothing

# make sure front end can correctly print values to error messages
let
    ex = expand(parse("\"a\"=1"))
    @test ex == Expr(:error, "invalid assignment location \"\"a\"\"") ||
          ex == Expr(:error, "invalid assignment location \"#<julia_value>\"")
end

# make sure that incomplete tags are detected correctly
# (i.e. error messages in src/julia-parser.scm must be matched correctly
# by the code in base/client.jl)
for (str, tag) in Dict("" => :none, "\"" => :string, "#=" => :comment, "'" => :char,
                       "`" => :cmd, "begin;" => :block, "quote;" => :block,
                       "let;" => :block, "for i=1;" => :block, "function f();" => :block,
                       "f() do x;" => :block, "module X;" => :block, "type X;" => :block,
                       "immutable X;" => :block, "(" => :other, "[" => :other,
                       "begin" => :other, "quote" => :other,
                       "let" => :other, "for" => :other, "function" => :other,
                       "f() do" => :other, "module" => :other, "type" => :other,
                       "immutable" => :other)
    @test Base.incomplete_tag(parse(str, raise=false)) == tag
end

# issue #6031
macro m6031(x); x; end
@test @m6031([2,4,6])[3] == 6
@test (@m6031 [2,4,6])[2] == 4

# issue #6050
@test Base.getfield_tfunc([nothing, QuoteNode(:vals)],
                          Dict{Int64,(UnitRange{Int64},UnitRange{Int64})},
                          :vals) == Array{(UnitRange{Int64},UnitRange{Int64}),1}

# issue #6068
x6068 = 1
function test6068()
    local a
    while true
        a = x6068
        break
    end
    a + 1
end
@test test6068() == 2

# issue #6074
macro X6074()
    quote
        global x6074
        let x6074 = x6074
            x6074
        end
    end
end
x6074 = 6074
@test @X6074() == 6074

# issue #5536
test5536(a::Union(Real, AbstractArray)...) = "Splatting"
test5536(a::Union(Real, AbstractArray)) = "Non-splatting"
@test test5536(5) == "Non-splatting"

# multiline comments (#6139 and others raised in #6128)
@test 3 == include_string("1 + 2") == include_string("1 + #==# 2") == include_string("1 + #===# 2") == include_string("1 + #= #= blah =# =# 2") == include_string("1 + #= #= #= nested =# =# =# 2")
@test_throws LoadError include_string("#=")
@test_throws LoadError include_string("#= #= #= =# =# =")

# issue #6142
type A6142 <: AbstractMatrix{Float64}; end
+{TJ}(x::A6142, y::UniformScaling{TJ}) = "UniformScaling method called"
+(x::A6142, y::AbstractArray) = "AbstractArray method called"
@test A6142() + I == "UniformScaling method called"

# issue #6175
function g6175(); print(""); (); end
g6175(i::Real, I...) = g6175(I...)
g6175(i, I...) = tuple(length(i), g6175(I...)...)
@test g6175(1:5) === (5,)

# issue #6242
f6242{N}(x::NTuple{N,Int})=(N==0 ? 1 : ntuple(n->x[n],N))
@test f6242(()) === 1

# issue #6292
let i = 0
    global g6292() = i+=1
end
@test g6292() == 1
@test g6292() == 2

# issue #6404
type type_2{T <: Integer, N} <: Number
    x::T
    type_2(n::T) = new(n)
end
type type_1{T <: Number} <: Number
    x::Vector{T}
    type_1(x::Vector{T}) = new(x)
end
type_1{T <: Number}(x::Vector{T}) = type_1{T}(x)
type_1{T <: Number}(c::T) = type_1{T}([c])
Base.convert{T<:Number, S<:Number}(::Type{type_1{T}}, x::S) = type_1(convert(T, x))
+{T <: Number}(a::type_1{T}, b::type_1{T}) = a

function func1_6404(v1::Integer)
    e1 = type_1([type_2{Int,v1}(0)])
    e1+e1
end

@test isa(func1_6404(3), type_1)

# issue #5577
f5577(::Any) = false
f5577(::Type) = true
@test !f5577((Int,AbstractString,2))
@test f5577(((Int,AbstractString),AbstractString))
@test f5577(Int)
@test !f5577(2)

# issue #6426
f6426(x,args...) = f6426(x,map(a->(isa(a,Type) ? Type{a} : typeof(a)), args))
f6426(x,t::(Type...)) = string(t)
@test f6426(1, (1.,2.)) == "((Float64,Float64),)"

# issue #6502
f6502() = convert(Base.tail((Bool,Int...)), (10,))
@test f6502() === (10,)
@test convert((Bool,Int...,), (true,10)) === (true,10)
@test convert((Int,Bool...), (true,1,0)) === (1,true,false)

# issue on the flight from DFW
# (type inference deducing Type{:x} rather than Symbol)
type FooBarDFW{s}; end
fooDFW(p::Type{FooBarDFW}) = string(p.parameters[1])
fooDFW(p) = string(p.parameters[1])
@test fooDFW(FooBarDFW{:x}) == "x" # not ":x"

# issue #6611
function crc6611(spec)
    direcn = spec ? 1 : 2
    local remainder::blech
    ()->(remainder=1)
end
@test_throws UndefVarError crc6611(true)()

# issue #6634
function crc6634(spec)
    A = UInt
    remainder::A = 1
    function handler(append)
        remainder = append ? 1 : 2
    end
end
@test crc6634(0x1)(true) == 1
@test crc6634(0x1)(false) == 2

# issue #5876
module A5876
macro x()
    quote
        function $(esc(:f5876)){T}(::Type{T})
            T
        end
        42
    end
end
end

let
    local z = A5876.@x()
    @test z == 42
    @test f5876(Int) === Int
end

# issue #6387
bitstype 64 Date6387{C}

type DateRange6387{C} <: Range{Date6387{C}}
end

type ObjMember
    member::DateRange6387
end

obj = ObjMember(DateRange6387{Int64}())

function v6387{T}(r::Range{T})
    a = Array(T,1)
    a[1] = Intrinsics.box(Date6387{Int64}, Intrinsics.unbox(Int64,int64(1)))
    a
end

function day_in(obj::ObjMember)
    x = v6387(obj.member)
    @test isa(x, Vector{Date6387{Int64}})
    @test isa(x[1], Date6387{Int64})
end
day_in(obj)

# issue #6784
@test ndims(Array(Array{Float64},3,5)) == 2
@test ndims(Array(Array,3,5)) == 2

# issue #6793
function segfault6793(;gamma=1)
    A = 1
    B = 1
    print()
    return
    -gamma
    nothing
end
@test segfault6793() === nothing

# issue #6896
g6896(x) = x::Int=x
@test g6896(5.0) === 5.0
f6896(x) = y::Int=x
@test f6896(5.0) === 5.0

# issue #6938
module M6938
macro mac()
    quote
        let
            y = 0
            y
        end
    end
end
end
@test @M6938.mac() == 0

# issue #7012
let x = zeros(2)
    x[1]::Float64 = 1
    @test x == [1.0, 0.0]
    @test_throws TypeError (x[1]::Int = 1)

    x[1]::Float64 += 1
    @test x == [2.0, 0.0]
    @test_throws TypeError (x[1]::Int += 1)
end

# issue #6980
abstract A6980
type B6980 <: A6980 end
f6980(::Union(Int, Float64), ::A6980) = false
f6980(::Union(Int, Float64), ::B6980) = true
@test f6980(1, B6980())

# issue #7049
typealias Maybe7049{T} Union(T,Void)
function ttt7049(;init::Maybe7049{Union(AbstractString,(Int,Char))} = nothing)
    string("init=", init)
end
@test ttt7049(init="a") == "init=a"

# issue #7074
let z{T<:Union(Float64,Complex{Float64},Float32,Complex{Float32})}(A::StridedMatrix{T}) = T,
    S = zeros(Complex,2,2)
    @test_throws MethodError z(S)
end

# issue #7062
f7062{t,n}(::Type{Array{t}}  , ::Array{t,n}) = (t,n,1)
f7062{t,n}(::Type{Array{t,n}}, ::Array{t,n}) = (t,n,2)
@test f7062(Array{Int,1}, [1,2,3]) === (Int,1,2)
@test f7062(Array{Int}  , [1,2,3]) === (Int,1,1)

# issue #7302
function test7302()
    t = [UInt64][1]
    convert(t, "5")
end
@test_throws MethodError test7302()

macro let_with_uninit()
    quote
        let x
            x = 1
            x+1
        end
    end
end

@test @let_with_uninit() == 2

# issue #5154
let
    v = []
    for i=1:3, j=1:3
        push!(v, (i, j))
        i == 1 && j == 2 && break
    end
    @test v == Any[(1,1), (1,2)]
end

# addition of ¬ (\neg) parsing
const (¬) = !
@test ¬false

# issue #7652
type A7652
    a :: Int
end
a7652 = A7652(0)
t_a7652 = A7652
f7652() = issubtype(fieldtype(t_a7652, :a), Int)
@test f7652() == issubtype(fieldtype(A7652, :a), Int) == true
g7652() = fieldtype(DataType, :types)
@test g7652() == fieldtype(DataType, :types) == Tuple
@test fieldtype(t_a7652, 1) == Int
h7652() = a7652.(1) = 2
h7652()
@test a7652.a == 2
i7652() = a7652.(1) = 3.0
i7652()
@test a7652.a == 3

# issue #7679
@test map(f->f(), Any[ ()->i for i=1:3 ]) == Any[1,2,3]

# issue #7810
type Foo7810{T<:AbstractVector}
    v::T
end
bar7810() = [Foo7810([(a,b) for a in 1:2]) for b in 3:4]
@test Base.return_types(bar7810,())[1] == Array{Foo7810{Array{(Int,Int),1}},1}

# issue 7897
function issue7897!(data, arr)
    data = reinterpret(UInt32, data)
    a = arr[1]
end

a = ones(UInt8, 10)
sa = sub(a,4:6)
# This can throw an error, but shouldn't segfault
try
    issue7897!(sa, zeros(10))
end

# issue #7582
aₜ = "a variable using Unicode 6"

immutable My8156{A, B}
    a::A
    b::B
end
let m = My8156(nothing, 1)
    @test sizeof(m) == sizeof(1)
    @test m.a === nothing
    @test m.b === 1
end

# issue #8184
immutable Foo8184
    x::Void
    y::Void
    z::Float64
end
let f = Foo8184(nothing,nothing,1.0)
    g(x) = x.z
    @test g(f) === 1.0
end

# issue #8213
@test map((x...)->x,(1,2),(3,4),(5,6)) === ((1,3,5),(2,4,6))

# issue #8338
let ex = Expr(:(=), :(f8338(x;y=4)), :(x*y))
    eval(ex)
    @test f8338(2) == 8
end

# call overloading (#2403)
Base.call(x::Int, y::Int) = x + 3y
issue2403func(f) = f(7)
let x = 10
    @test x(3) == 19
    @test x((3,)...) == 19
    @test issue2403func(x) == 31
end
type Issue2403
    x
end
Base.call(i::Issue2403, y) = i.x + 2y
let x = Issue2403(20)
    @test x(3) == 26
    @test issue2403func(x) == 34
end

# a method specificity issue
c99991{T}(::Type{T},x::T) = 0
c99991{T}(::Type{UnitRange{T}},x::FloatRange{T}) = 1
c99991{T}(::Type{UnitRange{T}},x::Range{T}) = 2
@test c99991(UnitRange{Float64}, 1.0:2.0) == 1
@test c99991(UnitRange{Int}, 1:2) == 2

# issue #8798
let
    const npy_typestrs = Dict("b1"=>Bool,
                              "i1"=>Int8,      "u1"=>UInt8,
                              "i2"=>Int16,     "u2"=>UInt16,
                              "i4"=>Int32,     "u4"=>UInt32,
                              "i8"=>Int64,     "u8"=>UInt64)
    sizeof_lookup() = sizeof(npy_typestrs["i8"])
    @test sizeof_lookup() == 8
end

# issue #8851
abstract AbstractThing{T,N}
type ConcreteThing{T<:FloatingPoint,N} <: AbstractThing{T,N}
end

@test typeintersect(AbstractThing{TypeVar(:T,true),2}, ConcreteThing) == ConcreteThing{TypeVar(:T,FloatingPoint),2}

# issue #8978
module I8978
y = 1
g() = f(y)
f(x) = 2
f(x::Int) = 3.0
module II8978
function callf(f)
    try
        f()
    finally
    end
end
end
h(f) = II8978.callf() do
    local x
    for i = 1:1
        x = g()+f
    end
    x
end
end

@test I8978.h(4) === 7.0

# issue #9134
function f9134()
    ii = zeros(Int32, 1)
    let i
        ii[1] = i
    end
end
@test_throws UndefVarError f9134()

# issue #9475
module I9475
    arr = Array(Any, 1)
    @eval @eval $arr[1] = 1
end

# issue #9520
f9520a(::Any, ::Any, args...) = 15
f9520b(::Any, ::Any, ::Any, args...) = 23
f9520c(::Any, ::Any, ::Any, ::Any, ::Any, ::Any, args...) = 46
@test invoke(f9520a, (Any, Any), 1, 2) == 15
@test invoke(f9520a, (Any, Any, Any), 1, 2, 3) == 15
@test invoke(f9520b, (Any, Any, Any), 1, 2, 3) == 23
@test invoke(f9520b, (Any, Any, Any, Any, Any, Any), 1, 2, 3, 4, 5, 6) == 23
@test invoke(f9520c, (Any, Any, Any, Any, Any, Any), 1, 2, 3, 4, 5, 6) == 46
@test invoke(f9520c, (Any, Any, Any, Any, Any, Any, Any), 1, 2, 3, 4, 5, 6, 7) == 46
