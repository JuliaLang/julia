# This file is a part of Julia. License is MIT: http://julialang.org/license

# test core language features
const Bottom = Union{}
const curmod = current_module()
const curmod_name = fullname(curmod)
const curmod_prefix = "$(["$m." for m in curmod_name]...)"

macro testintersect(args...)
    _testintersect(args...)
end

function _testintersect(a, b, result, cmp=(===))
    quote
        @test $(esc(cmp))(typeintersect($(esc(a)), $(esc(b))), $(esc(result)))
        @test $(esc(cmp))(typeintersect($(esc(b)), $(esc(a))), $(esc(result)))
    end
end

# basic type relationships
@test Int8 <: Integer
@test Int32 <: Integer
@test Tuple{Int8,Int8} <: Tuple{Integer,Integer}
@test !(AbstractArray{Float64,2} <: AbstractArray{Number,2})
@test !(AbstractArray{Float64,1} <: AbstractArray{Float64,2})
@test Tuple{Integer,Vararg{Integer}} <: Tuple{Integer,Vararg{Real}}
@test Tuple{Integer,Float64,Vararg{Integer}} <: Tuple{Integer,Vararg{Number}}
@test Tuple{Integer,Float64} <: Tuple{Integer,Vararg{Number}}
@test Tuple{Int32,} <: Tuple{Vararg{Number}}
@test Tuple{} <: Tuple{Vararg{Number}}
@test !(Tuple{Vararg{Int32}} <: Tuple{Int32,})
@test !(Tuple{Vararg{Int32}} <: Tuple{Number,Integer})
@test !(Tuple{Vararg{Integer}} <: Tuple{Integer,Integer,Vararg{Integer}})
@test !(Array{Int8,1} <: Array{Any,1})
@test !(Array{Any,1} <: Array{Int8,1})
@test Array{Int8,1} <: Array{Int8,1}
@test !(Type{Bottom} <: Type{Int32})
@test !(Vector{Float64} <: Vector{Union{Float64,Float32}})
@testintersect(Vector{Float64}, Vector{Union{Float64,Float32}}, Bottom)

@test !isa(Array,Type{Any})
@test Type{Complex} <: DataType
@test isa(Complex,Type{Complex})
@test !(Type{Ptr{Bottom}} <: Type{Ptr})
@test !(Type{Rational{Int}} <: Type{Rational})
@test Tuple{} <: Tuple{Vararg}
@test Tuple{} <: NTuple{TypeVar(:N,true)}
@test !(Type{Tuple{}} <: Type{Tuple{Vararg}})
@test !(Type{Tuple{}} <: Type{NTuple{TypeVar(:N,true)}})
let T = TypeVar(:T,true)
    @testintersect(Array{Bottom},AbstractArray{T}, Bottom, !==)
    @testintersect(Tuple{Type{Ptr{UInt8}},Ptr{Bottom}},
                  Tuple{Type{Ptr{T}},Ptr{T}}, Bottom)
    @test !(Type{T} <: TypeVar)

    @testintersect(Tuple{Range{Int},Tuple{Int,Int}},Tuple{AbstractArray{T},Dims},
                  Tuple{Range{Int},Tuple{Int,Int}})

    @testintersect(Tuple{T, AbstractArray{T}}, Tuple{Number, Array{Int,1}},
                  Tuple{Int, Array{Int,1}})

    @testintersect(Tuple{T, AbstractArray{T}}, Tuple{Int, Array{Number,1}},
                  Tuple{Int, Array{Number,1}})

    @testintersect(Tuple{T, AbstractArray{T}},Tuple{Any, Array{Number,1}},
                  Tuple{Number, Array{Number,1}}, isequal)
    @testintersect(Tuple{Array{T}, Array{T}}, Tuple{Array, Array{Any}}, Bottom, !==)
    f47{T}(x::Vector{Vector{T}}) = 0
    @test_throws MethodError f47(Array{Vector}(0))
    @test f47(Array{Vector{Int}}(0)) == 0
    @testintersect(Tuple{T,T}, Tuple{Union{Float64,Int64},Int64}, Tuple{Int64,Int64})
    @testintersect(Tuple{T,T}, Tuple{Int64,Union{Float64,Int64}}, Tuple{Int64,Int64})

    TT = TypeVar(:T)
    S = TypeVar(:S,true); N = TypeVar(:N,true); SN = TypeVar(:S,Number,true)
    @testintersect(Type{TypeVar(:T,Array{TT,1})},Type{Array{SN,N}}, Type{Array{SN,1}})
    # issue #5359
    @testintersect(Tuple{Type{Array{T,1}},Array{T,1}},
                  Tuple{Type{AbstractVector},Vector{Int}}, Bottom)
    # issue #5559
    @testintersect(Tuple{Type{Vector{Complex128}}, AbstractVector},
                  Tuple{Type{Array{T,N}}, Array{S,N}}, Tuple{Type{Vector{Complex128}},Vector}, isequal)
    @testintersect(Tuple{Type{Vector{Complex128}}, AbstractArray},
                  Tuple{Type{Array{T,N}}, Array{S,N}}, Tuple{Type{Vector{Complex128}},Vector}, isequal)

    @testintersect(Type{Array{T}}, Type{AbstractArray{T}}, Bottom)

    @testintersect(Type{Tuple{Bool,Vararg{Int}}}, Type{Tuple{Vararg{T}}}, Bottom)
    @testintersect(Type{Tuple{Bool,Vararg{Int}}}, Type{Tuple{T,Vararg{T}}}, Bottom)
    @testintersect(Tuple{Vararg{T}}, Tuple{Float64,Int}, Bottom)

    @testintersect(Tuple{Rational{T},T}, Tuple{Rational{Integer},Int}, Tuple{Rational{Integer},Int})

    # issue #1631
    @testintersect(Pair{T,Ptr{T}}, Pair{Ptr{S},S}, Bottom)
    @testintersect(Tuple{T,Ptr{T}}, Tuple{Ptr{S},S}, Bottom)
end
let N = TypeVar(:N,true)
    @testintersect(Tuple{NTuple{N,Integer},NTuple{N,Integer}},
                  Tuple{Tuple{Integer,Integer}, Tuple{Vararg{Integer}}},
                  Tuple{Tuple{Integer,Integer}, Tuple{Integer,Integer}})
    @testintersect(Tuple{NTuple{N,Integer},NTuple{N,Integer}},
                  Tuple{Tuple{Vararg{Integer}}, Tuple{Integer,Integer}},
                  Tuple{Tuple{Integer,Integer}, Tuple{Integer,Integer}})
    local A = typeintersect(Tuple{NTuple{N,Any},Array{Int,N}},
                            Tuple{Tuple{Int,Vararg{Int}},Array})
    local B = Tuple{Tuple{Int,Vararg{Int}},Array{Int,N}}
    @test A<:B && B<:A
    @testintersect(Tuple{NTuple{N,Any},Array{Int,N}},
                  Tuple{Tuple{Int,Vararg{Int}},Array{Int,2}},
                  Tuple{Tuple{Int,Int}, Array{Int,2}})
end
@testintersect(Type{Any},Type{Complex}, Bottom)
@testintersect(Type{Any},Type{TypeVar(:T,Real)}, Bottom)
@test !(Type{Array{Integer}} <: Type{AbstractArray{Integer}})
@test !(Type{Array{Integer}} <: Type{Array{TypeVar(:T,Integer)}})
@testintersect(Type{Function},Union,Bottom)
@testintersect(Type{Int32}, DataType, Type{Int32})
@test !(Type <: TypeVar)
@testintersect(DataType, Type, Bottom, !==)
@testintersect(Union, Type, Bottom, !==)
@testintersect(DataType, Type{Int}, Bottom, !==)
@testintersect(DataType, Type{TypeVar(:T,Int)}, Bottom, !==)
@testintersect(DataType, Type{TypeVar(:T,Integer)}, Bottom, !==)

@testintersect(Tuple{Vararg{Int}}, Tuple{Vararg{Bool}}, Tuple{})
@testintersect(Type{Tuple{Vararg{Int}}}, Type{Tuple{Vararg{Bool}}}, Bottom)
@testintersect(Tuple{Bool,Vararg{Int}}, Tuple{Vararg{Bool}}, Tuple{Bool,})

let T = TypeVar(:T,Union{Float32,Float64})
    @testintersect(AbstractArray, Matrix{T}, Matrix{T})
end
let T = TypeVar(:T,Union{Float32,Float64},true)
    @testintersect(AbstractArray, Matrix{T}, Matrix{T})
end

# Vararg{T,N}
let N = TypeVar(:N,true)
    @test Bottom === typeintersect(Tuple{Array{Int,N},Vararg{Int,N}}, Tuple{Vector{Int},Real,Real,Real})
    @test Bottom === typeintersect(Tuple{Vector{Int},Real,Real,Real}, Tuple{Array{Int,N},Vararg{Int,N}})
    @test Tuple{Int,Vararg{Int,2}} == Tuple{Int,Int,Int}
    @test Tuple{Int,Vararg{Int,2}} === Tuple{Int,Int,Int}
    @test Tuple{Any, Any} === Tuple{Vararg{Any,2}}
    @test Tuple{Int,Vararg{Int,2}} == Tuple{Int,Int,Vararg{Int,1}}
    @test Tuple{Int,Vararg{Int,2}} == Tuple{Int,Int,Int,Vararg{Int,0}}
    @test !(Tuple{Int,Vararg{Int,2}} <: Tuple{Int,Int,Int,Vararg{Int,1}})
    #@test !(Tuple{Int,Vararg{Int,2}} <: Tuple{Int,Vararg{Int,N}})
    @test Tuple{Int,Vararg{Int,N}} == Tuple{Int,Vararg{Int,N}}
    #@test !(Tuple{Int,Vararg{Int,2}} <: Tuple{Int,Int,Vararg{Int}})
    @test typeintersect(Tuple{Array{Int,N},Vararg{Int,N}},Tuple{Array{Int,0}}) == Tuple{Array{Int,0}}
    @test typeintersect(Tuple{Array{Int,N},Vararg{Int,N}},Tuple{Array{Int,2}}) == Bottom

    @test typeintersect(Tuple{Int,Vararg{Int,N}}, Tuple{Int,Int,Int,Vararg{Float64}}) == Tuple{Int,Int,Int}
    @test typeintersect(Tuple{Int,Vararg{Int,N}}, Tuple{Int,Vararg{Float64}}) == Tuple{Int}
    @test typeintersect(Tuple{Array{Int,N},Vararg{Int,N}}, Tuple{Matrix{Int},Int,Int,Vararg{Float64}}) == Tuple{Matrix{Int},Int,Int}
    @test typeintersect(Tuple{Array{Int,N},Vararg{Int,N}}, Tuple{Matrix{Int},Int,Vararg{Float64}}) == Bottom
end

@test isa(Int,Type{TypeVar(:T,Number)})
@test !isa(DataType,Type{TypeVar(:T,Number)})
@test DataType <: Type{TypeVar(:T,Type)}

@test isa(Tuple{},Type{Tuple{}})
@test !(Tuple{Int,} <: Type{TypeVar(:T,Tuple)})
@test isa(Tuple{Int,},Type{TypeVar(:T,Tuple)})

@test !isa(Type{Tuple{Int,Int}},Tuple)
@test !(Type{Tuple{Int,Int}} <: Tuple)
@test Tuple{Type{Int}} <: Tuple{DataType}

@test () != Type{Tuple{}}

# issue #6561
@test issubtype(Array{Tuple}, Array{NTuple})
@test issubtype(Array{Tuple{Vararg{Any}}}, Array{NTuple})
@test issubtype(Array{Tuple{Vararg}}, Array{NTuple})
@test !issubtype(Type{Tuple{Void}}, Tuple{Type{Void}})

# this is fancy: know that any type T<:Number must be either a DataType or a Union
@test Type{TypeVar(:T,Number)} <: Union{DataType,Union}
@test !(Type{TypeVar(:T,Number)} <: DataType)
@test !(Type{TypeVar(:T,Tuple)} <: Union{Tuple,Union})
@test Type{TypeVar(:T,Tuple)} <: Union{DataType,Union}

# issue #2997
let T = TypeVar(:T,Union{Float64,Array{Float64,1}},true)
    @testintersect(T,Real,Float64)
end

# issue #8652
args_morespecific(a, b) = ccall(:jl_args_morespecific, Cint, (Any,Any), a, b) != 0
let T1 = TypeVar(:T, Integer, true), T2 = TypeVar(:T, Integer, true)
    a = Tuple{Type{T1}, T1}
    b2 = Tuple{Type{T2}, Integer}
    @test args_morespecific(a, b2)
    @test !args_morespecific(b2, a)
    a = Tuple{Type{T1}, Ptr{T1}}
    b2 = Tuple{Type{T2}, Ptr{Integer}}
    @test args_morespecific(a, b2)
    @test !args_morespecific(b2, a)
end

# issue #11534
let T = TypeVar(:T, Tuple{Vararg{RangeIndex}}, true)
    t1 = Tuple{AbstractArray, Tuple{Vararg{RangeIndex}}}
    t2 = Tuple{Array, T}
    @test !args_morespecific(t1, t2)
    @test  args_morespecific(t2, t1)
end

let T = TypeVar(:T, Any, true), N = TypeVar(:N, Any, true)
    a = Tuple{Array{T,N}, Vararg{Int,N}}
    b = Tuple{Array,Int}
    @test  args_morespecific(a, b)
    @test !args_morespecific(b, a)
    a = Tuple{Array, Vararg{Int,N}}
    @test !args_morespecific(a, b)
    @test  args_morespecific(b, a)
end

# with bound varargs

_bound_vararg_specificity_1{T,N}(::Type{Array{T,N}}, d::Vararg{Int, N}) = 0
_bound_vararg_specificity_1{T}(::Type{Array{T,1}}, d::Int) = 1
@test _bound_vararg_specificity_1(Array{Int,1}, 1) == 1
@test _bound_vararg_specificity_1(Array{Int,2}, 1, 1) == 0

# issue #11840
typealias TT11840{T} Tuple{T,T}
f11840(::Type) = "Type"
f11840(::DataType) = "DataType"
f11840{T<:Tuple}(::Type{T}) = "Tuple"
@test f11840(Type) == "DataType"
@test f11840(AbstractVector) == "Type"
@test f11840(Tuple) == "Tuple"
@test f11840(TT11840) == "Tuple"

g11840(::DataType) = 1
g11840(::Type) = 2
g11840{T<:Tuple}(sig::Type{T}) = 3
@test g11840(Vector.body) == 1
@test g11840(Vector) == 2
@test g11840(Vector.body) == 1
@test g11840(Vector) == 2
@test g11840(Tuple) == 3
@test g11840(TT11840) == 3

g11840b(::DataType) = 1
g11840b(::Type) = 2
# FIXME: how to compute that the guard entry is still required,
# even though Type{Vector} ∩ DataType = Bottom and this method would set cache_with_orig = true
#g11840b{T<:Tuple}(sig::Type{T}) = 3
@test g11840b(Vector) == 2
@test g11840b(Vector.body) == 1
@test g11840b(Vector) == 2
@test g11840b(Vector.body) == 1
#@test g11840b(Tuple) == 3
#@test g11840b(TT11840) == 3

h11840(::DataType) = '1'
h11840(::Type) = '2'
h11840(::TypeConstructor) = '3'
h11840{T<:Tuple}(::Type{T}) = '4'
@test h11840(Vector) == '3'
@test h11840(Vector.body) == '1'
@test h11840(Vector) == '3'
@test h11840(Union{Vector, Matrix}) == '2'
@test h11840(Union{Vector.body, Matrix.body}) == '2'
@test h11840(Tuple) == '4'
@test h11840(TT11840) == '4'

# join
@test typejoin(Int8,Int16) === Signed
@test typejoin(Int,AbstractString) === Any
@test typejoin(Array{Float64},BitArray) <: AbstractArray
@test typejoin(Array{Bool},BitArray) <: AbstractArray{Bool}
@test typejoin(Tuple{Int,Int8},Tuple{Int8,Float64}) === Tuple{Signed,Real}
@test Base.typeseq(typejoin(Tuple{String,String},Tuple{DirectIndexString,String},
                            Tuple{String,DirectIndexString},Tuple{Int,String,Int}),
                   Tuple{Any,AbstractString,Vararg{Int}})
@test Base.typeseq(typejoin(Tuple{Int8,Vararg{Int}},Tuple{Int8,Int8}),
                   Tuple{Int8,Vararg{Signed}})
@test Base.typeseq(typejoin(Tuple{Int8,Vararg{Int}},Tuple{Int8,Vararg{Int8}}),
                   Tuple{Int8,Vararg{Signed}})
@test Base.typeseq(typejoin(Tuple{Int8,UInt8,Vararg{Int}},Tuple{Int8,Vararg{Int8}}),
                   Tuple{Int8,Vararg{Integer}})
@test Base.typeseq(typejoin(Union{Int,AbstractString},Int), Union{Int,AbstractString})
@test Base.typeseq(typejoin(Union{Int,AbstractString},Int8), Any)

# typejoin associativity
abstract Foo____{K}
type Wow____{K,V} <: Foo____{K} end
type Bar____{K,V} <: Foo____{K} end
let
    a = Wow____{Int64, Int64}
    b = Wow____{Int64, Float64}
    c = Bar____{Int64, Int64}
    @test typejoin(typejoin(b,c), a) == typejoin(typejoin(b,a), c) == Foo____{Int64}
end

# typejoin with Vararg{T,N}
@test typejoin(Tuple{Vararg{Int,2}}, Tuple{Int,Int,Int}) === Tuple{Int,Int,Vararg{Int}}
@test typejoin(Tuple{Vararg{Int,2}}, Tuple{Vararg{Int}}) === Tuple{Vararg{Int}}

@test promote_type(Bool,Bottom) === Bool

# ntuples
nttest1{n}(x::NTuple{n,Int}) = n
@test nttest1(()) == 0
@test nttest1((1,2)) == 2
@test NTuple <: Tuple
@test NTuple{TypeVar(:T),Int32} <: Tuple{Vararg{Int32}}
@test !(NTuple{TypeVar(:T),Int32} <: Tuple{Int32,Vararg{Int32}})
@test Tuple{Vararg{Int32}} <: NTuple{TypeVar(:T),Int32}
@test Tuple{Int32,Vararg{Int32}} <: NTuple{TypeVar(:T),Int32}

# #17198
@test_throws MethodError convert(Tuple{Int}, (1.0, 2.0, 3.0))

# type declarations

abstract Sup_{A,B}
abstract Qux_{T} <: Sup_{Qux_{Int},T}

@test Qux_{Int}.super <: Sup_
@test ===(Qux_{Int}, Qux_{Int}.super.parameters[1])
@test ===(Qux_{Int}.super.parameters[2], Int)
@test Qux_{Char}.super <: Sup_
@test ===(Qux_{Int}, Qux_{Char}.super.parameters[1])
@test ===(Qux_{Char}.super.parameters[2], Char)

@test Qux_.super.parameters[1].super <: Sup_
@test ===(Qux_{Int}, Qux_.super.parameters[1].super.parameters[1])
@test ===(Int, Qux_.super.parameters[1].super.parameters[2])

type Foo_{T} x::Foo_{Int} end

@test ===(Foo_.types[1], Foo_{Int})
@test ===(Foo_.types[1].types[1], Foo_{Int})

type Circ_{T} x::Circ_{T} end
@test ===(Circ_{Int}, Circ_{Int}.types[1])

abstract Sup2a_
abstract Sup2b_{A <: Sup2a_, B} <: Sup2a_
@test_throws ErrorException eval(:(abstract Qux2_{T} <: Sup2b_{Qux2_{Int}, T})) # wrapped in eval to avoid #16793

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

@test ===(Node{Int}.types[1].parameters[1], Node)

type Node2{T}
    v::Vector{Node2{T}}
end

@test ===(Node2{Int}.types[1].parameters[1], Node2{Int})

type FooFoo{A,B} y::FooFoo{A} end

@test FooFoo{Int} <: FooFoo{Int,AbstractString}.types[1]


let x = (2,3)
    @test +(x...) == 5
end

# bits types
@test isa((()->Core.Intrinsics.box(Ptr{Int8}, Core.Intrinsics.unbox(Int, 0)))(), Ptr{Int8})
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
let
    function foo()
        local x::Int8
        function bar()
            x = 100
        end
        bar()
        x
    end
    @test foo() === convert(Int8,100)
end

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
@test ===(sptest2(:a),Symbol)

sptest3{T}(x::T) = y->T
let m = sptest3(:a)
    @test ===(m(0),Symbol)
end

sptest4{T}(x::T, y::T) = 42
sptest4{T}(x::T, y) = 44
@test sptest4(1,2) == 42
@test sptest4(1, "cat") == 44

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
    @test fibb(10) == 55

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

function capt_before_def()
    f() = y
    y = 2
    f
end
@test capt_before_def()() == 2

function i18408()
    local i
    x->i
end
let f = i18408()
    @test_throws UndefRefError f(0)
end

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

# runtime intrinsics

let f = Any[Core.Intrinsics.add_int, Core.Intrinsics.sub_int]
    @test f[1](1, 1) == 2
    @test f[2](1, 1) == 0
end

# issue #7234
begin
    glob_x2 = 24
    f7234_a() = (glob_x2 += 1)
end
@test_throws UndefVarError f7234_a()
begin
    global glob_x2 = 24
    f7234_b() = (glob_x2 += 1)
end
@test_throws UndefVarError f7234_b()
# existing globals can be inherited by non-function blocks
for i = 1:2
    glob_x2 += 1
end
@test glob_x2 == 26
# globals declared as such in a non-global scope are inherited
let
    global glob_x3 = 11
    f7234_2() = (glob_x3 += 1)
    f7234_2()
end
@test glob_x3 == 12

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

a = Vector{Any}(3)
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
let
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

let
    local a
    a = UndefField()
    @test !isdefined(a, :field)
    @test !isdefined(a, :foo)
    @test !isdefined(2, :a)

    @test  isdefined("a",:data)
    @test  isdefined("a", 1)
    @test !isdefined("a", 2)

    @test_throws TypeError isdefined(2)
end

let
    local a
    a = Vector{Any}(2)
    @test !isassigned(a,1) && !isassigned(a,2)
    a[1] = 1
    @test isassigned(a,1) && !isassigned(a,2)
    a = Array{Float64}(1)
    @test isassigned(a,1)
    @test isassigned(a)
    @test !isassigned(a,2)
end

# isassigned, issue #11167
type Type11167{T,N} end
Type11167{Int,2}
@test !isassigned(Type11167.name.cache, 0)
@test isassigned(Type11167.name.cache, 1)
@test !isassigned(Type11167.name.cache, 2)
Type11167{Float32,5}
@test isassigned(Type11167.name.cache, 2)
@test !isassigned(Type11167.name.cache, 3)

# dispatch
let
    local foo, foo2, fooN, bar, baz
    foo(x::Tuple{Vararg{Any}}) = 0
    foo(x::Tuple{Vararg{Integer}}) = 1
    @test foo((:a,)) == 0
    @test foo(( 2,)) == 1

    foo2(x::Vararg{Any,2}) = 2
    @test foo2(1,2) == 2
    @test_throws MethodError foo2(1)
    @test_throws MethodError foo2(1,2,3)

    fooN{T,N}(A::Array{T,N}, x::Vararg{Any,N}) = -1
    @test fooN([1,2], 1) == -1
    @test_throws MethodError fooN([1,2], 1, 2) == -1
    @test fooN([1 2; 3 4], 1, 2) == -1
    @test_throws MethodError fooN([1 2; 3 4], 1)
    @test_throws MethodError fooN([1 2; 3 4], 1, 2, 3)

    bar{T}(x::Tuple{T,T,T,T})=1
    bar(x::Tuple{Any,Any,Any,Any})=2
    @test bar((1,1,1,1)) == 1
    @test bar((1,1,1,"a")) == 2
    @test bar((:a,:a,:a,:a)) == 1

    baz(::Type{Rational}) = 1
    baz{T}(::Type{Rational{T}}) = 2
    @test baz(Rational) == 1
    @test baz(Rational{Int}) == 2
end

let
    local mytype
    function mytype(vec)
        convert(Vector{Tuple{String, DataType}}, vec)
    end
    some_data = Any[("a", Int32), ("b", Int32)]
    @test isa(mytype(some_data),Vector{Tuple{String, DataType}})
end

type MyArray{N} <: AbstractArray{Int, N}
end
let
    local x
    x = MyArray{1}()
    foob(x::AbstractArray)=0
    foob{T}(x::AbstractVector{T})=1
    @test foob(x) == 1
end

let
    local f, g, a
    f{T}(a::Vector{Vector{T}}) = a
    g{T}(a::Vector{Vector{T}}) = a
    a = Vector{Int}[]
    @test ===(f(a), a)
    @test ===(g(a), a)
end

type _AA{T}; a::T; end
typealias _AoA{T} _AA{_AA{T}}
let
    local g, a
    g{T}(a::_AA{_AA{T}}) = a
    a = _AA(_AA(1))
    @test ===(g(a),a)
end

# Method specificity
begin
    local f, A
    f{T}(dims::Tuple{}, A::AbstractArray{T,0}) = 1
    f{T,N}(dims::NTuple{N,Int}, A::AbstractArray{T,N}) = 2
    f{T,M,N}(dims::NTuple{M,Int}, A::AbstractArray{T,N}) = 3
    A = zeros(2,2)
    @test f((1,2,3), A) == 3
    @test f((1,2), A) == 2
    @test f((), reshape([1])) == 1
    f{T,N}(dims::NTuple{N,Int}, A::AbstractArray{T,N}) = 4
    @test f((1,2), A) == 4
    @test f((1,2,3), A) == 3
end

# dispatch using Val{T}. See discussion in #9452 for instances vs types
let
    local firstlast
    firstlast(::Type{Val{true}}) = "First"
    firstlast(::Type{Val{false}}) = "Last"

    @test firstlast(Val{true}) == "First"
    @test firstlast(Val{false}) == "Last"
end

# x::Vararg{Any} declarations
let
    local f1, f2, f3
    f1(x...) = [x...]
    f2(x::Vararg{Any}) = [x...]
    f3(x::Vararg) = [x...]
    @test f1(1,2,3) == [1,2,3]
    @test f2(1,2,3) == [1,2,3]
    @test f3(1,2,3) == [1,2,3]
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
            global glo = 18
        end
    end
    @test retfinally() == 5
    @test glo == 18

    @test try error() end === nothing
end

# finalizers
let A = [1]
    local x = 0
    finalizer(A, a->(x+=1))
    finalize(A)
    @test x == 1
    A = 0
    gc(); gc()
    @test x == 1
end

# Module() constructor
@test names(Module(:anonymous), true, true) == [:anonymous]
@test names(Module(:anonymous, false), true, true) == [:anonymous]

# exception from __init__()
let didthrow =
    try
        include_string(
            """
            module TestInitError
                __init__() = error()
            end
            """)
        false
    catch ex
        @test isa(ex, LoadError)
        @test isa(ex.error, InitError)
        true
    end
    @test didthrow
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
let
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
let
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
let
    local MatOrNot, my_func, M
    typealias MatOrNot{T} Union{AbstractMatrix{T}, Vector{Union{}}}
    my_func{T<:Real}(A::MatOrNot{T}, B::MatOrNot{T}, C::MatOrNot{T}) = 0
    M = [ 2. 1. ; 1. 1. ]
    @test my_func(Union{}[], M, M) == 0
end

let
    local my_func, a, c
    my_func{T}(P::Vector{T}, Q::Vector{T}) = 0
    my_func{T}(x::T, P::Vector{T}) = 1
    my_func{T}(P::Vector{T}, x::T) = 2
    a = Int[3]
    c = Vector[a]

    @test my_func(c,c)==0
    @test my_func(a,c)==1
end

let
    local baar, foor, boor
    # issue #1131
    baar(x::DataType) = 0
    baar(x::Union) = 1
    baar(x::TypeConstructor) = 2
    @test baar(StridedArray) == 2
    @test baar(StridedArray.body) == 1
    @test baar(Vector) == 2
    @test baar(Vector.body) == 0

    boor(x) = 0
    boor(x::Union) = 1
    @test boor(StridedArray) == 0
    @test boor(StridedArray.body) == 1

    # issue #1202
    foor(x::Union) = 1
    @test_throws MethodError foor(StridedArray)
    @test foor(StridedArray.body) == 1
    @test_throws MethodError foor(StridedArray)
end

# issue #1153
type SI{m, s, kg}
    value::AbstractFloat
end

import Base.*

*{m1, m2, s1, s2, kg1, kg2}(x::SI{m1, s1, kg1}, y::SI{m2, s2, kg2}) = SI{m1 + m2, s1 + s2, kg1 + kg2}(x.value * y.value)

let
    local a,b
    a = SI{0,0,1}(1.0) * SI{1,2,0}(2.0)
    b = SI{0,0,1}(1.0) * SI{1,-2,0}(2.0)
    @test typeof(a) === SI{1,2,1}
    @test typeof(b) === SI{1,-2,1}
end

# pointer arithmetic
let
   local a,b,c
   a = C_NULL
   b = C_NULL + 1
   c = C_NULL - 1
   d = 1 + C_NULL
   @test eltype(a) == Void

   @test a != b != c
   @test b == d
   @test UInt(a) == 0
   @test UInt(b) == 1
   @test UInt(c) == typemax(UInt)

   @test b - a == -(a - b) == 1
   @test c - a == -(a - c) == typemax(UInt)
   @test c - b == -(b - c) == typemax(UInt) - 1
   @test a < b < c
end

# pull request 1270
let
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

let
    local a, aa
    a = [1,2,3]
    aa = unsafe_wrap(Array, pointer(a), length(a))
    @test aa == a
    aa = unsafe_wrap(Array, pointer(a), (length(a),))
    @test aa == a
    aa = unsafe_wrap(Array, pointer(a), UInt(length(a)))
    @test aa == a
    aa = unsafe_wrap(Array, pointer(a), UInt16(length(a)))
    @test aa == a
    @test_throws InexactError unsafe_wrap(Array, pointer(a), -3)
end

immutable FooBar2515
    foo::Int
    bar::Int
end
let
    local X, p
    X = FooBar2515[ FooBar2515(3,1), FooBar2515(4,4) ]
    p = pointer(X)
    @test unsafe_load(p) == FooBar2515(3,1)
    @test unsafe_load(p, 2) == FooBar2515(4,4)
    unsafe_store!(p, FooBar2515(8,4))
    @test X[1] == FooBar2515(8,4)
    unsafe_store!(p, FooBar2515(7,3), 1)
    @test X[1] == FooBar2515(7,3)
end

# issue #1287, combinations of try, catch, return
let
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

let
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

function NewEntity{T<:Component}(components::Type{T}...)
    map((c)->c(), components)
end

@test_throws MethodError NewEntity(Transform, Transform, Body, Body)
@test isa(NewEntity(Transform, Transform), Tuple{Transform, Transform})
@test_throws MethodError NewEntity(Transform, Transform, Body, Body)

# issue #1826
let
    a = (1,2)
    a,b = a
    @test a==1 && b==2
end

@testset "issue #1876" begin
let
    tst = 1
    m1(i) = (tst+=1;i-1)
    x = [1:4;]
    x[1:end] *= 2
    @test x == [2:2:8;]
    x[m1(end)] += 3
    @test x == [2,4,9,8]
    @test tst == 2

    # issue #1886
    X = [1:4;]
    r = Array{UnitRange{Int}}(1)
    r[1] = 2:3
    X[r...] *= 2
    @test X == [1,4,6,4]
end
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

# issue #2098
let
    i2098() = begin
        c = Any[2.0]
        [1:1:c[1];]
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
    @test invoke(i2169, Tuple{Array} ,Int8[1]) === Int8(-128)
end

# issue #2365
type B2365{T}
     v::Union{T, Void}
end
@test B2365{Int}(nothing).v === nothing
@test B2365{Int}(0).v === 0

# issue #2352
let
    local Sum, n
    Sum=0.0; for n=1:2:10000
        Sum += -1/n + 1/(n+1)
    end
    @test Sum < -0.69
end

# issue #2509
immutable Foo2509; foo::Int; end
@test Foo2509(1) != Foo2509(2)
@test Foo2509(42) == Foo2509(42)

# issue #2517
immutable Foo2517; end
@test repr(Foo2517()) == "$(curmod_prefix)Foo2517()"
@test repr(Array{Foo2517}(1)) == "$(curmod_prefix)Foo2517[$(curmod_prefix)Foo2517()]"
@test Foo2517() === Foo2517()

# issue #1474
type X1474{a,b} end
let
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

@test isa(foo4075(Foo4075(Int64(1),2.0),:y), Float64)
# very likely to segfault the second time if this is broken
@test isa(foo4075(Foo4075(Int64(1),2.0),:y), Float64)

# issue #3167
let
    function foo(x)
        ret=Array{typeof(x[1])}(length(x))
        for j = 1:length(x)
            ret[j] = x[j]
        end
        return ret
    end
    x = Array{Union{Dict{Int64,AbstractString},Array{Int64,3},Number,AbstractString,Void}}(3)
    x[1] = 1.0
    x[2] = 2.0
    x[3] = 3.0
    @test foo(x) == [1.0, 2.0, 3.0]
end

# TODO!!
# issue #4115
#type Foo4115
#end
#typealias Foo4115s NTuple{3,Union{Foo4115,Type{Foo4115}}}
#baz4115(x::Foo4115s) = x
#@test baz4115(convert(Tuple{Type{Foo4115},Type{Foo4115},Foo4115},
#                      (Foo4115,Foo4115,Foo4115()))) == (Foo4115,Foo4115,Foo4115())

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

@test isa(foo4129(Baz4129(Bar41291(Foo4129())),1,2), Tuple{Baz4129,Bar4129,Foo4129,Int,Int})

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
f4413(::Union{A4413, B4413, C4413}) = "ABC"
f4413(::Union{A4413, B4413}) = "AB"
g4413(::Union{A4413, C4413}) = "AC"
g4413(::Union{A4413, B4413, C4413}) = "ABC"

@test f4413(A4413()) == "AB" && f4413(B4413()) == "AB"
@test g4413(A4413()) == "AC" && g4413(C4413()) == "AC"

# issue #4482
# what happens here: the method cache logic wants to widen the type of a
# tuple argument, but it shouldn't do that for an argument that a static
# parameter depends on.
f4482{T}(x::T) = T
@test f4482((Ptr,Ptr)) === Tuple{DataType,DataType}
@test f4482((Ptr,))    === Tuple{DataType,}

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
@test f4528(false, Int32(12)) === nothing
@test_throws ErrorException f4528(true, Int32(12))

# issue #4518
f4518(x, y::Union{Int32,Int64}) = 0
f4518(x::String, y::Union{Int32,Int64}) = 1
@test f4518("",1) == 1

# issue #4581
bitstype 64 Date4581{T}
let
    x = Core.Intrinsics.box(Date4581{Int}, Core.Intrinsics.unbox(Int64,Int64(1234)))
    xs = Date4581[x]
    ys = copy(xs)
    @test ys !== xs
    @test ys == xs
end

# issue #6591
function f6591(d)
    Core.Intrinsics.box(Int64, d)
    (f->f(d))(identity)
end
let d = Core.Intrinsics.box(Date4581{Int}, Int64(1))
    @test isa(f6591(d), Date4581)
end

# issue #4645
i4645(x) = (println(zz); zz = x; zz)
@test_throws UndefVarError i4645(4)

# more undef var errors
function test_undef_var_9898(a)
    a1 = a1
    a
end
@test_throws UndefVarError test_undef_var_9898(1)

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
Base.unsafe_convert(::Type{Ptr{Z4681}},b::Z4681) = b.x
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
    @testintersect(Type{IT4805{1,T}}, Type{TypeVar(:_,IT4805{N,Int})}, Bottom, !==)
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
f5150(T) = Array{Rational{T}}(1)
@test typeof(f5150(Int)) === Array{Rational{Int},1}


# issue #5165
bitstype 64 T5165{S}
make_t(x::Int64) = Base.box(T5165{Void}, Base.unbox(Int64, x))
xs5165 = T5165[make_t(Int64(1))]
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
let i = 0, x = 65
    @test (i, i+=1, i+=1) === (0, 1, 2)
    @test i == 2
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
    x=a ? (Int64(0),reinterpret(Int5142,Int64(0))) : (Int64(1),reinterpret(Int5142,Int64(1)))
    x[2]::Int5142
end
function h5142(a::Int)
    x=(Int64(0),reinterpret(Int5142,Int64(0)))
    x[a]::Int5142
end
h5142(true)
@test_throws TypeError h5142(1)
h5142(2)
f5142() = h5142(1)
try
    # try running this code in a different context that triggers the codegen
    # assertion `assert(isboxed || v.typ == typ)`.
    f5142()
end

bitstype 8 Int5142b
function h5142b(a::Int)
    x=((Int8(1),Int8(2)),(reinterpret(Int5142b,Int8(3)),reinterpret(Int5142b,Int8(4))))
    x[a]::Tuple{Int8,Int8}
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
    return Int32(1)
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
    star = Array{Polygon5884}((3,))
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

# issue #6031
macro m6031(x); x; end
@test @m6031([2,4,6])[3] == 6
@test (@m6031 [2,4,6])[2] == 4

# issue #6050
@test Core.Inference.getfield_tfunc(
          Dict{Int64,Tuple{UnitRange{Int64},UnitRange{Int64}}},
          Core.Inference.Const(:vals)) == (Array{Tuple{UnitRange{Int64},UnitRange{Int64}},1},true)

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
test5536(a::Union{Real, AbstractArray}...) = "Splatting"
test5536(a::Union{Real, AbstractArray}) = "Non-splatting"
@test test5536(5) == "Non-splatting"

# multiline comments (#6139 and others raised in #6128) and embedded NUL chars (#10994)
@test 3 == include_string("1 + 2") == include_string("1 + #==# 2") == include_string("1 + #===# 2") == include_string("1 + #= #= blah =# =# 2") == include_string("1 + #= #= #= nested =# =# =# 2") == include_string("1 + #= \0 =# 2")
@test_throws LoadError include_string("#=")
@test_throws LoadError include_string("#= #= #= =# =# =")

# issue #6142
import Base: +
type A6142 <: AbstractMatrix{Float64}; end
+{TJ}(x::A6142, y::UniformScaling{TJ}) = "UniformScaling method called"
+(x::A6142, y::AbstractArray) = "AbstractArray method called"
@test A6142() + I == "UniformScaling method called"
+(x::A6142, y::Range) = "Range method called" #16324 ambiguity

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
@test !f5577(((Int,AbstractString),AbstractString))
@test f5577(Tuple{Tuple{Int,AbstractString},AbstractString})
@test f5577(Int)
@test !f5577(2)

# issue #6426
f6426(x,args...) = f6426(x,map(a->(isa(a,Type) ? Type{a} : typeof(a)), args))
f6426(x,t::Tuple{Vararg{Type}}) = string(t)
@test f6426(1, (1.,2.)) == "(Tuple{Float64,Float64},)"

# issue #6502
f6502() = convert(Tuple{Vararg{Int}}, (10,))
@test f6502() === (10,)
@test convert(Tuple{Bool,Vararg{Int}}, (true,10)) === (true,10)
@test convert(Tuple{Int,Vararg{Bool}}, (true,1,0)) === (1,true,false)

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

obj6387 = ObjMember(DateRange6387{Int64}())

function v6387{T}(r::Range{T})
    a = Array{T}(1)
    a[1] = Core.Intrinsics.box(Date6387{Int64}, Core.Intrinsics.unbox(Int64,Int64(1)))
    a
end

function day_in(obj::ObjMember)
    x = v6387(obj.member)
    @test isa(x, Vector{Date6387{Int64}})
    @test isa(x[1], Date6387{Int64})
end
day_in(obj6387)

# issue #6784
@test ndims(Array{Array{Float64}}(3,5)) == 2
@test ndims(Array{Array}(3,5)) == 2

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
f6980(::Union{Int, Float64}, ::A6980) = false
f6980(::Union{Int, Float64}, ::B6980) = true
@test f6980(1, B6980())

# issue #7049
typealias Maybe7049{T} Union{T,Void}
function ttt7049(;init::Maybe7049{Union{AbstractString,Tuple{Int,Char}}} = nothing)
    string("init=", init)
end
@test ttt7049(init="a") == "init=a"

# issue #7074
let z{T<:Union{Float64,Complex{Float64},Float32,Complex{Float32}}}(A::StridedMatrix{T}) = T,
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
@test g7652() == fieldtype(DataType, :types) == SimpleVector
@test fieldtype(t_a7652, 1) == Int
h7652() = setfield!(a7652, 1, 2)
h7652()
@test a7652.a == 2
# commented out due to issue #16195: setfield! does not perform conversions
#   i7652() = setfield!(a7652, 1, 3.0)
#   i7652()
#   @test a7652.a == 3

# issue #7679
@test map(f->f(), Any[ ()->i for i=1:3 ]) == Any[1,2,3]

# issue 7897
function issue7897!(data, arr)
    data = reinterpret(UInt32, data)
    a = arr[1]
end

a = ones(UInt8, 10)
sa = view(a,4:6)
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
(x::Int)(y::Int) = x + 3y
issue2403func(f) = f(7)
let x = 10
    @test x(3) == 19
    @test x((3,)...) == 19
    @test issue2403func(x) == 31
end
type Issue2403
    x
end
(i::Issue2403)(y) = i.x + 2y
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

# issue #17016, method specificity involving vararg tuples
typealias T_17016{N} Tuple{Any,Any,Vararg{Any,N}}
f17016(f, t::T_17016) = 0
f17016(f, t1::Tuple) = 1
@test f17016(0, (1,2,3)) == 0

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
type ConcreteThing{T<:AbstractFloat,N} <: AbstractThing{T,N}
end

@testintersect(AbstractThing{TypeVar(:T,true),2}, ConcreteThing, ConcreteThing{TypeVar(:T,AbstractFloat),2}, isequal)

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
    arr = Array{Any}(1)
    @eval @eval $arr[1] = 1
end

# issue #9520
f9520a(::Any, ::Any, args...) = 15
f9520b(::Any, ::Any, ::Any, args...) = 23
f9520c(::Any, ::Any, ::Any, ::Any, ::Any, ::Any, args...) = 46
@test invoke(f9520a, Tuple{Any, Any}, 1, 2) == 15
@test invoke(f9520a, Tuple{Any, Any, Any}, 1, 2, 3) == 15
@test invoke(f9520b, Tuple{Any, Any, Any}, 1, 2, 3) == 23
@test invoke(f9520b, Tuple{Any, Any, Any, Any, Any, Any}, 1, 2, 3, 4, 5, 6) == 23
@test invoke(f9520c, Tuple{Any, Any, Any, Any, Any, Any}, 1, 2, 3, 4, 5, 6) == 46
@test invoke(f9520c, Tuple{Any, Any, Any, Any, Any, Any, Any}, 1, 2, 3, 4, 5, 6, 7) == 46
# Keep until the old signature of invoke is dropped.
@test invoke(f9520a, (Any, Any), 1, 2) == 15
@test invoke(f9520a, (Any, Any, Any), 1, 2, 3) == 15
@test invoke(f9520b, (Any, Any, Any), 1, 2, 3) == 23
@test invoke(f9520b, (Any, Any, Any, Any, Any, Any), 1, 2, 3, 4, 5, 6) == 23
@test invoke(f9520c, (Any, Any, Any, Any, Any, Any), 1, 2, 3, 4, 5, 6) == 46
@test invoke(f9520c, (Any, Any, Any, Any, Any, Any, Any), 1, 2, 3, 4, 5, 6, 7) == 46

call_lambda1() = (()->x)(1)
call_lambda2() = ((x)->x)()
call_lambda3() = ((x)->x)(1,2)
call_lambda4() = ((x,y...)->x)()
@test_throws MethodError call_lambda1()
@test_throws MethodError call_lambda2()
@test_throws MethodError call_lambda3()
@test_throws MethodError call_lambda4()
call_lambda5() = ((x...)->x)()
call_lambda6() = ((x...)->x)(1)
call_lambda7() = ((x...)->x)(1,2)
@test call_lambda5() == ()
@test call_lambda6() == (1,)
@test call_lambda7() == (1,2)

# jl_new_bits testing
let x = [1,2,3]
    @test ccall(:jl_new_bits, Any, (Any,Ptr{Void},), Int, x) === 1
    @test ccall(:jl_new_bits, Any, (Any,Ptr{Void},), Complex{Int}, x) === 1+2im
    @test ccall(:jl_new_bits, Any, (Any,Ptr{Void},), NTuple{3,Int}, x) === (1,2,3)
    @test ccall(:jl_new_bits, Any, (Any,Ptr{Void},), Tuple{Int,Int,Int}, x) === (1,2,3)
    @test (ccall(:jl_new_bits, Any, (Any,Ptr{Void},), Tuple{Int16,Tuple{Void},Int8,Tuple{},Int,Void,Int}, x)::Tuple)[[2,4,5,6,7]] === ((nothing,),(),2,nothing,3)
end

# sig 2 is SIGINT per the POSIX.1-1990 standard
if !is_windows()
    ccall(:jl_exit_on_sigint, Void, (Cint,), 0)
    @test_throws InterruptException begin
        ccall(:kill, Void, (Cint, Cint,), getpid(), 2)
        for i in 1:10
            Libc.systemsleep(0.1)
            ccall(:jl_gc_safepoint, Void, ()) # wait for SIGINT to arrive
        end
    end
    ccall(:jl_exit_on_sigint, Void, (Cint,), 1)
end
let
    # Exception frame automatically restores sigatomic counter.
    Base.sigatomic_begin()
    @test_throws ErrorException begin
        for i = 1:2
            Base.sigatomic_end()
        end
    end
    Base.sigatomic_end()
end

# pull request #9534
@test try; a,b,c = 1,2; catch ex; (ex::BoundsError).a === (1,2) && ex.i == 3; end
@test try; [][]; catch ex; isempty((ex::BoundsError).a::Array{Any,1}) && ex.i == (1,); end
@test try; [][1,2]; catch ex; isempty((ex::BoundsError).a::Array{Any,1}) && ex.i == (1,2); end
@test try; [][10]; catch ex; isempty((ex::BoundsError).a::Array{Any,1}) && ex.i == (10,); end
f9534a() = (a=1+2im; getfield(a, -100))
f9534a(x) = (a=1+2im; getfield(a, x))
@test try; f9534a() catch ex; (ex::BoundsError).a === 1+2im && ex.i == -100; end
@test try; f9534a(3) catch ex; (ex::BoundsError).a === 1+2im && ex.i == 3; end
f9534b() = (a=(1,2.,""); a[5])
f9534b(x) = (a=(1,2.,""); a[x])
@test try; f9534b() catch ex; (ex::BoundsError).a == (1,2.,"") && ex.i == 5; end
@test try; f9534b(4) catch ex; (ex::BoundsError).a == (1,2.,"") && ex.i == 4; end
f9534c() = (a=(1,2.); a[3])
f9534c(x) = (a=(1,2.); a[x])
@test try; f9534c() catch ex; (ex::BoundsError).a === (1,2.) && ex.i == 3; end
@test try; f9534c(0) catch ex; (ex::BoundsError).a === (1,2.) && ex.i == 0; end
f9534d() = (a=(1,2,4,6,7); a[7])
f9534d(x) = (a=(1,2,4,6,7); a[x])
@test try; f9534d() catch ex; (ex::BoundsError).a === (1,2,4,6,7) && ex.i == 7; end
@test try; f9534d(-1) catch ex; (ex::BoundsError).a === (1,2,4,6,7) && ex.i == -1; end
f9534e(x) = (a=IOBuffer(); setfield!(a, x, 3))
@test try; f9534e(-2) catch ex; isa((ex::BoundsError).a,Base.IOBuffer) && ex.i == -2; end
f9534f() = (a=IOBuffer(); getfield(a, -2))
f9534f(x) = (a=IOBuffer(); getfield(a, x))
@test try; f9534f() catch ex; isa((ex::BoundsError).a,Base.IOBuffer) && ex.i == -2; end
@test try; f9534f(typemin(Int)+2) catch ex; isa((ex::BoundsError).a,Base.IOBuffer) && ex.i == typemin(Int)+2; end
x9634 = 3
@test try; getfield(1+2im, x9634); catch ex; (ex::BoundsError).a === 1+2im && ex.i == 3; end
@test try; throw(BoundsError()) catch ex; !isdefined((ex::BoundsError), :a) && !isdefined((ex::BoundsError), :i); end
@test try; throw(BoundsError(Int)) catch ex; (ex::BoundsError).a == Int && !isdefined((ex::BoundsError), :i); end
@test try; throw(BoundsError(Int, typemin(Int))) catch ex; (ex::BoundsError).a == Int && (ex::BoundsError).i == typemin(Int); end
@test try; throw(BoundsError(Int, (:a,))) catch ex; (ex::BoundsError).a == Int && (ex::BoundsError).i == (:a,); end
f9534g(a,b,c...) = c[0]
@test try; f9534g(1,2,3,4,5,6) catch ex; (ex::BoundsError).a === (3,4,5,6) && ex.i == 0; end
f9534h(a,b,c...) = c[a]
@test f9534h(4,2,3,4,5,6) == 6
@test try; f9534h(5,2,3,4,5,6) catch ex; (ex::BoundsError).a === (3,4,5,6) && ex.i == 5; end

# issue #9535
counter9535 = 0
f9535() = (global counter9535; counter9535 += 1; counter9535)
g9535() = (f9535(),f9535())
@test g9535() == (1,2)
@test g9535() == (3,4)

# weak references
type Obj; x; end
@testset "weak references" begin
    @noinline function mk_wr(r, wr)
        x = Obj(1)
        push!(r, x)
        push!(wr, WeakRef(x))
    end
    test_wr(r,wr) = @test r[1] == wr[1].value
    function test_wr()
        ref = []
        wref = []
        mk_wr(ref, wref)
        test_wr(ref, wref)
        gc()
        test_wr(ref, wref)
        pop!(ref)
        gc()
        @test wref[1].value === nothing
    end
    test_wr()
end

# issue #9947
function f9947()
    if 1 == 0
        1
    else
        min(UInt128(2),1)
    end
end
@test f9947() == UInt128(1)

#issue #9835
module M9835
    using Base.Test
    type A end; type B end
    f() = (isa(A(), A) ? A : B)()
    @test isa(f(), A)
end

#issue #10163
let a = :(()), b = :(())
    @test a.args !== b.args
end

# issue caused by commit 189b00aef0376d1a998d36115cd11b17464d26ce and worked around
# by commit 24c64b86bd4e793dbfe9d85c067dc0579b320d14
let
    g{T}(x::T...) = T
    g(x...) = 0
    @test g((),Int) == 0
    @test g((),()) == Tuple{}
end

# TODO: hopefully this issue is obsolete after the tuple type change
## issue #8631
#f8631(::(Type, Type...), ::(Any, Any...)) = 1
#f8631{T}(::Type{(T...)}, x::Tuple) = 2
#@test length(methods(f8631, ((Type, Type...), (Any, Any...)))) == 2

# issue caused by 8d0037cb377257fc4232c8526b12337dd7bdf0a7
args8d003 = (:x, :y)
@test eval(:(:(f($(($args8d003)...))))) == :(f(x,y))
x8d003 = Any[:y8d003]
y8d003 = 777
@test eval(:(string(:(f($($(x8d003...))))))) == "f(777)"

# issue #9378
abstract Foo9378{T,S}
immutable B9378{T} end
typealias FooB9378{T} Foo9378{T,B9378}
immutable CFoo9378 <: FooB9378{Float64} end
@test isa(CFoo9378(),FooB9378)

# issue #10281
const N10281 = 1000
@test if false
    for i in 1:N10281
    end
end === nothing

# issue #10221
module GCbrokentype
OLD_STDOUT = STDOUT
fname = tempname()
file = open(fname, "w")
redirect_stdout(file)
versioninfo()
try
    type Foo{T}
        val::Bar{T}
    end
end
gc()
redirect_stdout(OLD_STDOUT)
close(file)
rm(fname)
end

# issue #10373
f10373(x) = x
g10373(x) = x
type newtype10373
end
let f
    for f in (f10373,g10373)
        (::typeof(f))(x::newtype10373) = println("$f")
    end
end
for m in methods(f10373)
    @test m.name == :f10373
end
for m in methods(g10373)
    @test m.name == :g10373
end

# issue #7221
f7221{T<:Number}(::T) = 1
f7221(::BitArray) = 2
f7221(::AbstractVecOrMat) = 3
@test f7221(trues(1)) == 2

# test functionality of non-power-of-2 bitstype constants
bitstype 24 Int24
Int24(x::Int) = Core.Intrinsics.box(Int24,Core.Intrinsics.trunc_int(Int24,Core.Intrinsics.unbox(Int,x)))
Int(x::Int24) = Core.Intrinsics.box(Int,Core.Intrinsics.zext_int(Int,Core.Intrinsics.unbox(Int24,x)))
let x,y,f
    x = Int24(Int(0x12345678)) # create something (via truncation)
    @test Int(0x345678) === Int(x)
    function f() Int24(Int(0x02468ace)) end
    y = f() # invoke llvm constant folding
    @test Int(0x468ace) === Int(y)
    @test x !== y
    @test string(y) == "$(curmod_prefix)Int24(0x468ace)"
end

# issue #10570
immutable Array_512_Uint8
    d1::UInt8
    d2::UInt8
    d3::UInt8
    d4::UInt8
    d5::UInt8
    d6::UInt8
    d7::UInt8
    d8::UInt8
    d9::UInt8
    d10::UInt8
    d11::UInt8
    d12::UInt8
    d13::UInt8
    d14::UInt8
    d15::UInt8
    d16::UInt8
    d17::UInt8
    d18::UInt8
    d19::UInt8
    d20::UInt8
    d21::UInt8
    d22::UInt8
    d23::UInt8
    d24::UInt8
    d25::UInt8
    d26::UInt8
    d27::UInt8
    d28::UInt8
    d29::UInt8
    d30::UInt8
    d31::UInt8
    d32::UInt8
    d33::UInt8
    d34::UInt8
    d35::UInt8
    d36::UInt8
    d37::UInt8
    d38::UInt8
    d39::UInt8
    d40::UInt8
    d41::UInt8
    d42::UInt8
    d43::UInt8
    d44::UInt8
    d45::UInt8
    d46::UInt8
    d47::UInt8
    d48::UInt8
    d49::UInt8
    d50::UInt8
    d51::UInt8
    d52::UInt8
    d53::UInt8
    d54::UInt8
    d55::UInt8
    d56::UInt8
    d57::UInt8
    d58::UInt8
    d59::UInt8
    d60::UInt8
    d61::UInt8
    d62::UInt8
    d63::UInt8
    d64::UInt8
    d65::UInt8
    d66::UInt8
    d67::UInt8
    d68::UInt8
    d69::UInt8
    d70::UInt8
    d71::UInt8
    d72::UInt8
    d73::UInt8
    d74::UInt8
    d75::UInt8
    d76::UInt8
    d77::UInt8
    d78::UInt8
    d79::UInt8
    d80::UInt8
    d81::UInt8
    d82::UInt8
    d83::UInt8
    d84::UInt8
    d85::UInt8
    d86::UInt8
    d87::UInt8
    d88::UInt8
    d89::UInt8
    d90::UInt8
    d91::UInt8
    d92::UInt8
    d93::UInt8
    d94::UInt8
    d95::UInt8
    d96::UInt8
    d97::UInt8
    d98::UInt8
    d99::UInt8
    d100::UInt8
    d101::UInt8
    d102::UInt8
    d103::UInt8
    d104::UInt8
    d105::UInt8
    d106::UInt8
    d107::UInt8
    d108::UInt8
    d109::UInt8
    d110::UInt8
    d111::UInt8
    d112::UInt8
    d113::UInt8
    d114::UInt8
    d115::UInt8
    d116::UInt8
    d117::UInt8
    d118::UInt8
    d119::UInt8
    d120::UInt8
    d121::UInt8
    d122::UInt8
    d123::UInt8
    d124::UInt8
    d125::UInt8
    d126::UInt8
    d127::UInt8
    d128::UInt8
    d129::UInt8
    d130::UInt8
    d131::UInt8
    d132::UInt8
    d133::UInt8
    d134::UInt8
    d135::UInt8
    d136::UInt8
    d137::UInt8
    d138::UInt8
    d139::UInt8
    d140::UInt8
    d141::UInt8
    d142::UInt8
    d143::UInt8
    d144::UInt8
    d145::UInt8
    d146::UInt8
    d147::UInt8
    d148::UInt8
    d149::UInt8
    d150::UInt8
    d151::UInt8
    d152::UInt8
    d153::UInt8
    d154::UInt8
    d155::UInt8
    d156::UInt8
    d157::UInt8
    d158::UInt8
    d159::UInt8
    d160::UInt8
    d161::UInt8
    d162::UInt8
    d163::UInt8
    d164::UInt8
    d165::UInt8
    d166::UInt8
    d167::UInt8
    d168::UInt8
    d169::UInt8
    d170::UInt8
    d171::UInt8
    d172::UInt8
    d173::UInt8
    d174::UInt8
    d175::UInt8
    d176::UInt8
    d177::UInt8
    d178::UInt8
    d179::UInt8
    d180::UInt8
    d181::UInt8
    d182::UInt8
    d183::UInt8
    d184::UInt8
    d185::UInt8
    d186::UInt8
    d187::UInt8
    d188::UInt8
    d189::UInt8
    d190::UInt8
    d191::UInt8
    d192::UInt8
    d193::UInt8
    d194::UInt8
    d195::UInt8
    d196::UInt8
    d197::UInt8
    d198::UInt8
    d199::UInt8
    d200::UInt8
    d201::UInt8
    d202::UInt8
    d203::UInt8
    d204::UInt8
    d205::UInt8
    d206::UInt8
    d207::UInt8
    d208::UInt8
    d209::UInt8
    d210::UInt8
    d211::UInt8
    d212::UInt8
    d213::UInt8
    d214::UInt8
    d215::UInt8
    d216::UInt8
    d217::UInt8
    d218::UInt8
    d219::UInt8
    d220::UInt8
    d221::UInt8
    d222::UInt8
    d223::UInt8
    d224::UInt8
    d225::UInt8
    d226::UInt8
    d227::UInt8
    d228::UInt8
    d229::UInt8
    d230::UInt8
    d231::UInt8
    d232::UInt8
    d233::UInt8
    d234::UInt8
    d235::UInt8
    d236::UInt8
    d237::UInt8
    d238::UInt8
    d239::UInt8
    d240::UInt8
    d241::UInt8
    d242::UInt8
    d243::UInt8
    d244::UInt8
    d245::UInt8
    d246::UInt8
    d247::UInt8
    d248::UInt8
    d249::UInt8
    d250::UInt8
    d251::UInt8
    d252::UInt8
    d253::UInt8
    d254::UInt8
    d255::UInt8
    d256::UInt8
    d257::UInt8
    d258::UInt8
    d259::UInt8
    d260::UInt8
    d261::UInt8
    d262::UInt8
    d263::UInt8
    d264::UInt8
    d265::UInt8
    d266::UInt8
    d267::UInt8
    d268::UInt8
    d269::UInt8
    d270::UInt8
    d271::UInt8
    d272::UInt8
    d273::UInt8
    d274::UInt8
    d275::UInt8
    d276::UInt8
    d277::UInt8
    d278::UInt8
    d279::UInt8
    d280::UInt8
    d281::UInt8
    d282::UInt8
    d283::UInt8
    d284::UInt8
    d285::UInt8
    d286::UInt8
    d287::UInt8
    d288::UInt8
    d289::UInt8
    d290::UInt8
    d291::UInt8
    d292::UInt8
    d293::UInt8
    d294::UInt8
    d295::UInt8
    d296::UInt8
    d297::UInt8
    d298::UInt8
    d299::UInt8
    d300::UInt8
    d301::UInt8
    d302::UInt8
    d303::UInt8
    d304::UInt8
    d305::UInt8
    d306::UInt8
    d307::UInt8
    d308::UInt8
    d309::UInt8
    d310::UInt8
    d311::UInt8
    d312::UInt8
    d313::UInt8
    d314::UInt8
    d315::UInt8
    d316::UInt8
    d317::UInt8
    d318::UInt8
    d319::UInt8
    d320::UInt8
    d321::UInt8
    d322::UInt8
    d323::UInt8
    d324::UInt8
    d325::UInt8
    d326::UInt8
    d327::UInt8
    d328::UInt8
    d329::UInt8
    d330::UInt8
    d331::UInt8
    d332::UInt8
    d333::UInt8
    d334::UInt8
    d335::UInt8
    d336::UInt8
    d337::UInt8
    d338::UInt8
    d339::UInt8
    d340::UInt8
    d341::UInt8
    d342::UInt8
    d343::UInt8
    d344::UInt8
    d345::UInt8
    d346::UInt8
    d347::UInt8
    d348::UInt8
    d349::UInt8
    d350::UInt8
    d351::UInt8
    d352::UInt8
    d353::UInt8
    d354::UInt8
    d355::UInt8
    d356::UInt8
    d357::UInt8
    d358::UInt8
    d359::UInt8
    d360::UInt8
    d361::UInt8
    d362::UInt8
    d363::UInt8
    d364::UInt8
    d365::UInt8
    d366::UInt8
    d367::UInt8
    d368::UInt8
    d369::UInt8
    d370::UInt8
    d371::UInt8
    d372::UInt8
    d373::UInt8
    d374::UInt8
    d375::UInt8
    d376::UInt8
    d377::UInt8
    d378::UInt8
    d379::UInt8
    d380::UInt8
    d381::UInt8
    d382::UInt8
    d383::UInt8
    d384::UInt8
    d385::UInt8
    d386::UInt8
    d387::UInt8
    d388::UInt8
    d389::UInt8
    d390::UInt8
    d391::UInt8
    d392::UInt8
    d393::UInt8
    d394::UInt8
    d395::UInt8
    d396::UInt8
    d397::UInt8
    d398::UInt8
    d399::UInt8
    d400::UInt8
    d401::UInt8
    d402::UInt8
    d403::UInt8
    d404::UInt8
    d405::UInt8
    d406::UInt8
    d407::UInt8
    d408::UInt8
    d409::UInt8
    d410::UInt8
    d411::UInt8
    d412::UInt8
    d413::UInt8
    d414::UInt8
    d415::UInt8
    d416::UInt8
    d417::UInt8
    d418::UInt8
    d419::UInt8
    d420::UInt8
    d421::UInt8
    d422::UInt8
    d423::UInt8
    d424::UInt8
    d425::UInt8
    d426::UInt8
    d427::UInt8
    d428::UInt8
    d429::UInt8
    d430::UInt8
    d431::UInt8
    d432::UInt8
    d433::UInt8
    d434::UInt8
    d435::UInt8
    d436::UInt8
    d437::UInt8
    d438::UInt8
    d439::UInt8
    d440::UInt8
    d441::UInt8
    d442::UInt8
    d443::UInt8
    d444::UInt8
    d445::UInt8
    d446::UInt8
    d447::UInt8
    d448::UInt8
    d449::UInt8
    d450::UInt8
    d451::UInt8
    d452::UInt8
    d453::UInt8
    d454::UInt8
    d455::UInt8
    d456::UInt8
    d457::UInt8
    d458::UInt8
    d459::UInt8
    d460::UInt8
    d461::UInt8
    d462::UInt8
    d463::UInt8
    d464::UInt8
    d465::UInt8
    d466::UInt8
    d467::UInt8
    d468::UInt8
    d469::UInt8
    d470::UInt8
    d471::UInt8
    d472::UInt8
    d473::UInt8
    d474::UInt8
    d475::UInt8
    d476::UInt8
    d477::UInt8
    d478::UInt8
    d479::UInt8
    d480::UInt8
    d481::UInt8
    d482::UInt8
    d483::UInt8
    d484::UInt8
    d485::UInt8
    d486::UInt8
    d487::UInt8
    d488::UInt8
    d489::UInt8
    d490::UInt8
    d491::UInt8
    d492::UInt8
    d493::UInt8
    d494::UInt8
    d495::UInt8
    d496::UInt8
    d497::UInt8
    d498::UInt8
    d499::UInt8
    d500::UInt8
    d501::UInt8
    d502::UInt8
    d503::UInt8
    d504::UInt8
    d505::UInt8
    d506::UInt8
    d507::UInt8
    d508::UInt8
    d509::UInt8
    d510::UInt8
    d511::UInt8
    d512::UInt8
end
gc()

# issue #10867
@test collect(enumerate((Tuple,Int))) == [(1,Tuple), (2,Int)]
@test collect(enumerate((Tuple,3))) == [(1,Tuple), (2,3)]

# issue #10978
typealias TupleType10978{T<:Tuple} Type{T}
f10978(T::TupleType10978) = isa(T, TupleType10978)
@test f10978(Tuple{Int})

# issue #10995
#typealias TupleType{T<:Tuple} Type{T}
f10995(::Any) = (while false; end; nothing)
f10995(T::TupleType10978) = (while false; end; @assert isa(T, TupleType10978))
g10995(x) = f10995(typeof(x))
g10995((1, 2))
@test g10995(UInt8) === nothing

# issue #11149
@noinline f11149(a,b,args...) = (a,b,args...)
@test f11149(1,2,3) == invoke(f11149, Tuple{Int,Int,Int}, 1,2,3)

# issue #11357
function f11357()
    x = (1,2,3)
    i = (1,)
    x[i...]
end
@test f11357() === 1

# issue #11136
type A11136 end
type B11136 end
let T = TypeVar(:T, true), TB = TypeVar(:T, B11136, true)
    @testintersect(Tuple{T, T}, Tuple{A11136, TB}, Bottom)
end

# issue #11367
abstract Foo11367
let T1 = TypeVar(:T1, true), T2 = TypeVar(:T2, Foo11367, true)
    @testintersect(Tuple{T1, T1}, Tuple{Type{BigInt}, T2}, Bottom)
end

# issue #11355
function f11355{T<:Tuple}(sig::Type{T})
    f11355(sig.parameters[1])
end
function f11355(arg::DataType)
    if arg <: Tuple
        return 200
    end
    return 100
end
let t = Tuple{Type{Vector{Int}}}
    @test f11355(t) == 100
    t = Tuple{Type{Dict{TypeVar(:K, true)}}}
    @test f11355(t) == 100
end

# issue #8283
function func8283 end
@test isa(func8283,Function)
@test_throws MethodError func8283()

# issue #11243
type Type11243{A, B}
    x::A
    y::B
end
let a = [Type11243(1,2), Type11243("a","b")]
    @test typeof(a) == Vector{Type11243}
    @test typeof(a) <: Vector{Type11243}
end

# issue #11065, #1571
function f11065()
    for i = 1:2
        if i == 1
            z = "z is defined"
        elseif i == 2
            print(z)
        end
    end
end
@test_throws UndefVarError f11065()

# issue #11295
function f11295(x...)
    call = Expr(x...)
end
@test isa(f11295(:a,:b), Expr)

# issue #11675
immutable T11675{T}
    x::T
    T11675() = new()
end
let x = T11675{Union{}}()
    function f11675(x)
        x.x + 1
    end
    @test_throws UndefRefError f11675(x)
end

# issue #7864
module M7864
export x7864
x7864 = 1
end

@test_throws UndefVarError x7864
using .M7864
@test x7864 == 1

# issue #11715
f11715(x) = (x === Tuple{Any})
@test f11715(Tuple{Any})

# part of #11597
# make sure invalid, partly-constructed types don't end up in the cache
abstract C11597{T<:Union{Void, Int}}
type D11597{T} <: C11597{T} d::T end
@test_throws TypeError D11597(1.0)
@test_throws TypeError repr(D11597(1.0))

# issue #11772
@test_throws UndefRefError (Vector{Any}(5)...)

# issue #11813
let a = UInt8[1, 107, 66, 88, 2, 99, 254, 13, 0, 0, 0, 0]
    u32 = UInt32[0x3]
    a[9:end] = reinterpret(UInt8, u32)
    p = pointer(a)
    @test (Int8(1),(Int8(2),Int32(3))) === unsafe_load(convert(Ptr{Tuple{Int8,Tuple{Int8,Int32}}},p))
    f11813(p) = (Int8(1),(Int8(2),Int32(3))) === unsafe_load(convert(Ptr{Tuple{Int8,Tuple{Int8,Int32}}},p))
    @test f11813(p) === true # redundant comparison test seems to make this test more reliable, don't remove
end
# issue #13037
let a = UInt8[0, 0, 0, 0, 0x66, 99, 254, 13, 0, 0, 0, 0]
    u32 = UInt32[0x3]
    a[1:4] = reinterpret(UInt8, u32)
    p = pointer(a)
    @test ((Int32(3),UInt8(0x66)),Int32(0)) === unsafe_load(convert(Ptr{Tuple{Tuple{Int32,UInt8},Int32}},p))
    f11813(p) = ((Int32(3),UInt8(0x66)),Int32(0)) === unsafe_load(convert(Ptr{Tuple{Tuple{Int32,UInt8},Int32}},p))
    @test f11813(p) === true # redundant comparison test seems to make this test more reliable, don't remove
end
let a = (1:1000...),
    b = (1:1000...)
    @test a == b
    @test a === b
    @test (a == b) === true
    @test (a === b) === true
end

# issue 11858
type Foo11858
    x::Float64
    Foo11858(x::Float64) = new(x)
end

type Bar11858
    x::Float64
    Bar11858(x::Float64) = new(x)
end

g11858(x::Float64) = x
f11858(a) = for Baz in a
    (f::Baz)(x) = f(float(x))
end
f11858(Any[Type{Foo11858}, Type{Bar11858}, typeof(g11858)])

@test g11858(1) == 1.0
@test Foo11858(1).x == 1.0
@test Bar11858(1).x == 1.0

# issue 11904
@noinline throw_error() = error()
foo11904(x::Int) = x
@inline function foo11904{S}(x::Nullable{S})
    if isbits(S)
        Nullable(foo11904(x.value), x.hasvalue)
    else
        throw_error()
    end
end

@test !isnull(foo11904(Nullable(1)))

# issue 11874
immutable Foo11874
    x::Int
end

function bar11874(x)
    local y::Foo11874
    y = x
    nothing
end

Base.convert(::Type{Foo11874},x::Int) = float(x)

@test_throws TypeError bar11874(1)

# issue #9233
let
    try
        NTuple{Int, 1}
        @test false
    catch err
        @test isa(err, TypeError)
        @test err.func == :apply_type
        @test err.expected == Int
        @test err.got == Int
    end

    try
        NTuple{0x1, Int}
        @test false
    catch err
        @test isa(err, TypeError)
        @test err.func == :apply_type
        @test err.expected == Int
        @test err.got == 0x1
    end
end

# 11996
@test_throws ErrorException NTuple{-1, Int}
@test_throws TypeError Union{Int, 1}

type FooNTuple{N}
    z::Tuple{Integer, Vararg{Int, N}}
end
@test_throws ErrorException FooNTuple{-1}
@test_throws ErrorException FooNTuple{typemin(Int)}
@test_throws TypeError FooNTuple{0x01}
@test fieldtype(FooNTuple{0}, 1) == Tuple{Integer}

type FooTupleT{T}
    z::Tuple{Int, T, Int}
end
@test_throws TypeError FooTupleT{Vararg{Int, 2}}
@test fieldtype(FooTupleT{Int}, 1) == NTuple{3, Int}

@test Tuple{} === NTuple{0, Any}
@test Tuple{Int} === Tuple{Int, Vararg{Integer, 0}}

# issue #12003
const DATE12003 = DateTime(1917,1,1)
failure12003(dt=DATE12003) = Dates.year(dt)
@test isa(failure12003(), Integer)

# issue #12023 Test error checking in bitstype
@test_throws ErrorException (@eval bitstype 0 SPJa12023)
@test_throws ErrorException (@eval bitstype 4294967312 SPJb12023)
@test_throws ErrorException (@eval bitstype -4294967280 SPJc12023)

# issue #12089
type A12089{K, N}
    sz::NTuple{N, Int}
    A12089(sz::NTuple{N, Int}) = new(sz)
end
A12089{-1, 1}((1,))

# issue #12092
f12092(x::Int, y) = 0
f12092(x::Int,) = 1
f12092(x::Int, y::Int...) = 2
@test f12092(1) == 1

# issue #12096
let a = Val{Val{TypeVar(:_,Int,true)}}
    @test_throws UndefRefError a.instance
    @test !isleaftype(a)
end

# PR #12058
let N = TypeVar(:N,true)
    @test typeintersect(NTuple{N,Int}, NTuple{N,Float64}) === Tuple{}
end

# issue #12063
# NOTE: should have > MAX_TUPLETYPE_LEN arguments
f12063{T}(tt, g, p, c, b, v, cu::T, d::AbstractArray{T, 2}, ve) = 1
f12063(args...) = 2
g12063() = f12063(0, 0, 0, 0, 0, 0, 0.0, spzeros(0,0), Int[])
@test g12063() == 1

# issue #11587
type Sampler11587{N}
    clampedpos::Array{Int,2}
    buf::Array{Float64,N}
end
function Sampler11587()
    a = tuple(Any[32,32]...,)
    Sampler11587(zeros(Int,a), zeros(Float64,a))
end
@test isa(Sampler11587(), Sampler11587{2})

# issue #8010 - error when convert returns wrong type during new()
immutable Vec8010{T}
    x::T
    y::T
end
Vec8010(a::AbstractVector) = Vec8010(ntuple(x->a[x],2)...)
Base.convert{T}(::Type{Vec8010{T}},x::AbstractVector) = Vec8010(x)
Base.convert(::Type{Void},x::AbstractVector) = Vec8010(x)
immutable MyType8010
     m::Vec8010{Float32}
end
immutable MyType8010_ghost
     m::Void
end
@test_throws TypeError MyType8010([3.0;4.0])
@test_throws TypeError MyType8010_ghost([3.0;4.0])

# don't allow redefining types if ninitialized changes
immutable NInitializedTestType
    a
end

@test_throws ErrorException @eval immutable NInitializedTestType
    a
    NInitializedTestType() = new()
end

# issue #12394
type Empty12394 end
let x = Array{Empty12394}(1), y = [Empty12394()]
    @test_throws UndefRefError x==y
    @test_throws UndefRefError y==x
end

module TestRecursiveConstGlobalStructCtor
const x = (1,2)
const y = (x,(3,4))
f() = (x,y,(5,6))
end
@test TestRecursiveConstGlobalStructCtor.f() == ((1,2),((1,2),(3,4)),(5,6))

const const_array_int1 = Array{Int}
const const_array_int2 = Array{Int}
test_eq_array_int() = ===(const_array_int1, const_array_int2)
@test test_eq_array_int()

# object_id of haspadding field
immutable HasPadding
    x::Bool
    y::Int
end
immutable HasHasPadding
    x::HasPadding
end
hashaspadding = HasHasPadding(HasPadding(true,1))
hashaspadding2 = HasHasPadding(HasPadding(true,1))
unsafe_store!(convert(Ptr{UInt8},pointer_from_objref(hashaspadding)), 0x12, 2)
unsafe_store!(convert(Ptr{UInt8},pointer_from_objref(hashaspadding2)), 0x21, 2)
@test object_id(hashaspadding) == object_id(hashaspadding2)

# issue #12517
let x = (1,2)
    @eval f12517() = Val{$x}
    @test f12517() === Val{(1,2)}
end

# don't allow Vararg{} in Union{} type constructor
@test_throws TypeError Union{Int,Vararg{Int}}

# don't allow Vararg{} in Tuple{} type constructor in non-trailing position
@test_throws TypeError Tuple{Vararg{Int32},Int64,Float64}
@test_throws TypeError Tuple{Int64,Vararg{Int32},Float64}

# don't allow non-types in Union
@test_throws TypeError Union{1}
@test_throws TypeError Union{Int,0}
typealias PossiblyInvalidUnion{T} Union{T,Int}
@test_throws TypeError PossiblyInvalidUnion{1}

# issue #12569
@test Symbol("x") === Symbol("x")
@test split(string(gensym("abc")),'#')[3] == "abc"

# issue #13007
call13007{T,N}(::Type{Array{T,N}}) = 0
call13007(::Type{Array}) = 1
@test length(Base._methods(call13007, Tuple{Type{TypeVar(:_,Array)}}, 4)) == 2

# detecting cycles during type intersection, e.g. #1631
cycle_in_solve_tvar_constraints{S}(::Type{Nullable{S}}, x::S) = 0
cycle_in_solve_tvar_constraints{T}(::Type{T}, x::Val{T}) = 1
@test length(methods(cycle_in_solve_tvar_constraints)) == 2

# issue #12967
foo12967(x, ::ANY) = 1
typealias TupleType12967{T<:Tuple} Type{T}
foo12967(x, ::TupleType12967) = 2
@test foo12967(1, Int) == 1
@test foo12967(1, Tuple{}) == 2

# issue #13083
@test Void() === nothing

# issue discovered in #11973
for j = 1:1
    x = try
        error()
        2
    catch
        continue
    end
end

# PR 11888
immutable A11888{T}
    a::NTuple{16,T}
end

typealias B11888{T} A11888{A11888{A11888{T}}}

@test sizeof(B11888{B11888{Int64}}) == (1 << 24) * 8

# issue #13175
immutable EmptyImmutable13175 end
immutable EmptyIIOtherField13175
    x::EmptyImmutable13175
    y::Float64
end
@test EmptyIIOtherField13175(EmptyImmutable13175(), 1.0) == EmptyIIOtherField13175(EmptyImmutable13175(), 1.0)
@test EmptyIIOtherField13175(EmptyImmutable13175(), 1.0) != EmptyIIOtherField13175(EmptyImmutable13175(), 2.0)

# issue #13183
gg13183{X}(x::X...) = 1==0 ? gg13183(x, x) : 0
@test gg13183(5) == 0

# issue 8932 (llvm return type legalizer error)
immutable Vec3_8932
    x::Float32
    y::Float32
    z::Float32
end
f8932(a::Vec3_8932, b::Vec3_8932) = Vec3_8932(a.x % b.x, a.y % b.y, a.z % b.z)
a8932 = Vec3_8932(1,1,1)
b8932 = Vec3_8932(2,2,2)
@test f8932(a8932, b8932) == Vec3_8932(1.0, 1.0, 1.0)

# issue #13261
f13261() = (x = (error("oops"),); +(x...))
g13261() = f13261()
@test_throws ErrorException g13261()

# issue 13432
@noinline function f13432(x)
    offset = x ? Base.Bottom : 1
    return ===(offset, Base.Bottom)
end
@test f13432(true) == true
@test f13432(false) == false
@noinline function f13432b(x)
    a = x ? 1 : 1.0
    b = x ? 1 : 1.0f0
    return ===(a, b)
end
@test f13432b(true) == true
@test f13432b(false) == false

#13433, read!(::IO, a::Vector{UInt8}) should return a
type IO13433 <: IO end
Base.read(::IO13433, ::Type{UInt8}) = 0x01
@test read!(IO13433(), Array{UInt8}(4)) == [0x01, 0x01, 0x01, 0x01]

# issue #13647, comparing boxed isbits immutables
immutable X13647
    a::Int
    b::Bool
end
function f13647(x, y)
    z = false
    z = y
    x === z
end
@test f13647(X13647(1, false), X13647(1, false))
@test !f13647(X13647(1, false), X13647(1, true))
@test !f13647(X13647(2, false), X13647(1, false))

# issue #13636
module I13636
foo(x) = 1
end
let cache = Dict()
    function I13636.foo(y::Int;k::Int=1)
        cache[1] = y+k
    end
end
@test I13636.foo(1,k=2) == 3

# issue #11327 and #13547
@test_throws MethodError convert(Type{Int}, Float32)
@test_throws MethodError Array{Type{Int64}}([Float32])
abstract A11327
abstract B11327 <: A11327
f11327{T}(::Type{T},x::T) = x
@test_throws MethodError f11327(Type{A11327},B11327)
let T=TypeVar(:T,true)
    @test typeintersect(Tuple{Type{T},T}, Tuple{Type{Type{Float64}},Type{Int}}) === Union{}
end

# issue 13855
macro m13855()
    Expr(:localize, :(() -> x))
end
@noinline function foo13855(x)
    @m13855()
end
@test foo13855(+)() == +
@test foo13855(*)() == *

# issue #8487
@test [x for x in 1:3] == [x for x ∈ 1:3] == [x for x = 1:3]
let A = Array{Int}(4,3)
    for i ∈ 1:size(A,1), j ∈ 1:size(A,2)
        A[i,j] = 17*i + 51*j
    end
    @test A == [17*i + 51*j for i ∈ 1:size(A,1), j ∈ 1:size(A,2)]
end

# check if finalizers for the old gen can be triggered manually
# issue #13986
let
    obj = Ref(1)
    finalized = 0
    finalizer(obj, (obj) -> (finalized = 1))
    # obj should be marked for promotion after the second gc and be promoted
    # after the third GC
    # GC_CLEAN; age = 0
    gc(false)
    # GC_CLEAN; age = 1
    gc(false)
    # GC_QUEUED; age = 1
    gc(false)
    # GC_MARKED; age = 1
    finalize(obj)
    @test finalized == 1
end

# check if finalizers for the old gen can be triggered manually
# PR #14181
let
    # The following three `gc(false)` clears the `finalizer_list`. It is
    # not strictly necessary to make the test pass but should make the failure
    # more repeatable if something breaks.
    gc(false)
    # At least: GC_CLEAN; age = 1
    gc(false)
    # At least: GC_QUEUED; age = 1
    gc(false)
    # all objects in `finalizer_list` are now moved to `finalizer_list_marked`

    obj1 = Ref(1)
    obj2 = Ref(1)
    finalized = 0
    finalizer(obj1, (obj) -> (finalized += 1))
    finalizer(obj1, (obj) -> (finalized += 1))
    finalizer(obj2, (obj) -> (finalized += 1; finalize(obj1)))
    finalizer(obj2, (obj) -> (finalized += 1; finalize(obj1)))
    finalize(obj2)
    @test finalized == 4
end

# issue #14323
@test_throws ErrorException eval(Expr(:body, :(1)))

# issue #14339
f14339{T<:Union{}}(x::T, y::T) = 0
@test_throws MethodError f14339(1, 2)

# Make sure jlcall objects are rooted
# PR #14301
module JLCall14301

# Define f
function f end

let i = Any[[1.23], [2.34]]
    # f() with capture variables
    # Intentionally type unstable so that the dynamic dispatch will
    # read the corrupted tag if the object is incorrectly GC'd.
    global @noinline f() = i[1][1] * i[2][1]
end

# Another function that use f()
g() = f() * 100
# Compile it
g()

let i = 9.0
    # Override f()
    global @noinline f() = i + 1
end

# Make sure the old f() method is GC'd if it was not rooted properly
gc()
gc()
gc()

# Run again.
g()

end

# make sure codegen doesn't remove argument to `isa`
@noinline __g_isa_test_1(a) = push!(a,1)
function __f_isa_arg_1()
    a = []
    isa(__g_isa_test_1(a), Any)
    length(a)
end
@test __f_isa_arg_1() == 1

# issue #14477
immutable Z14477
    fld::Z14477
    Z14477() = new(new())
end
let z1 = Z14477()
    @test isa(z1, Z14477)
    @test isa(z1.fld, Z14477)
end

# issue #14482
let T = TypeVar(:T, true)
    @test typeintersect(T, Type{Int8}) == Type{Int8}
    @test typeintersect(Tuple{T}, Tuple{Type{Int8}}) == Tuple{Type{Int8}}
end

# issue #8846, generic macros
macro m8846(a, b=0)
    a, b
end
@test @m8846(a) === (:a, 0)
@test @m8846(a,1) === (:a, 1)
@test_throws MethodError eval(:(@m8846(a,b,c)))

# a simple case of parametric dispatch with unions
let foo{T}(x::Union{T,Void},y::Union{T,Void}) = 1
    @test foo(1, nothing) === 1
    @test_throws MethodError foo(nothing, nothing)  # can't determine T
end

module TestMacroGlobalFunction
macro makefn(f,g)
    quote
        global $(f)
        function $(f)(x)
            x+1
        end
        global $(g)
        $(g)(x) = x+2
    end
end
@makefn ff gg
end
@test TestMacroGlobalFunction.ff(1) == 2
@test TestMacroGlobalFunction.gg(1) == 3

# issue #18672
macro x18672()
    quote
        function f
        end
    end
end
let
    @test isa(@x18672, Function)
end

# issue #14564
@test isa(object_id(Tuple.name.cache), Integer)

# issue #14691
type T14691; a::UInt; end
@test (T14691(0).a = 0) === 0

# issue #14245
f14245() = (v = []; push!(v, length(v)); v)
@test f14245()[1] == 0

# issue #9677
@generated function foo9677{T,N}(x::AbstractArray{T,N})
    quote
        x=$N
        y=x+1
        return y
    end
end
foo9677(x::Array) = invoke(foo9677,(AbstractArray,),x)
@test foo9677(1:5) == foo9677(randn(3))

# issue #6846
f6846() = (please6846; 2)
@test_throws UndefVarError f6846()

# issue #14758
@test isa(eval(:(f14758(; $([]...)) = ())), Function)

# issue #14767
@inline f14767(x) = x ? A14767 : ()
const A14767 = f14767(false)
@test A14767 === ()

# issue #10985
f10985(::Any...) = 1
@test f10985(1, 2, 3) == 1

# a tricky case for closure conversion
type _CaptureInCtor
    yy
    function _CaptureInCtor(list_file::AbstractString="")
        y = 0
        f = x->add_node(y)
        new(f(2))
    end
    add_node(y) = y+1
end
@test _CaptureInCtor().yy == 1

# issue #14610
let sometypes = (Int,Int8)
    f(::Union{ntuple(i->Type{sometypes[i]}, length(sometypes))...}) = 1
    @test method_exists(f, (Union{Type{Int},Type{Int8}},))
end

let
    b=()->c
    c=1
    @test b() == 1
end

# issue #14825
abstract abstest_14825

type t1_14825{A <: abstest_14825, B}
  x::A
  y::B
end

type t2_14825{C, B} <: abstest_14825
  x::C
  y::t1_14825{t2_14825{C, B}, B}
end

@test t2_14825{Int,Int}.types[2] <: t1_14825

# issue #14917
@test isa(let generic
          function generic end
          end,
          Function)

# f.(x) vectorization syntax (#15032)
@test (x -> 2x).([1,2,3]) == [2,4,6]
@test ((x,y) -> 2x+y^2).([1,2,3],[3,4,5]) == [1,2,3]*2 + [3,4,5].^2

# let syntax with multiple lhs
let z = (3,9,42)
    let (a,b,c) = z
        @test a == 3 && b == 9 && c == 42
    end
    let (a,b::Float64,c::Int8) = z
        @test a == 3 && b === 9.0 && c === Int8(42)
    end
    z = (1, z, 10)
    let (a, (b,c,d), e) = z
        @test (a,b,c,d,e) == (1,3,9,42,10)
    end
end

# issue #15072
let grphtest = ((1, [2]),)
    for (s, g) in grphtest
        g_ = map(s -> s+1, g)
        @test g_ == [3]
    end
    for s = 1:1
    end
end

# issue #13229
module I13229
using Base.Test
if !startswith(string(Sys.ARCH), "arm")
    global z = 0
    @timed @profile for i = 1:5
        function f(x)
            return x + i
        end
        global z = f(i)
    end
    @test z == 10
else
    warn("@profile test skipped")
end
end

# issue #15186
let ex = quote
             $(if true; :(test); end)
         end
    @test ex.args[2] == :test
end

# issue #15180
function f15180{T}(x::T)
    X = Array{T}(1)
    X[1] = x
    @noinline ef{J}(::J) = (J,X[1]) # Use T
    ef{J}(::J, ::Int) = (T,J)
    return ef
end
@test map(f15180(1), [1,2]) == [(Int,1),(Int,1)]

let ary = Vector{Any}(10)
    check_undef_and_fill(ary, rng) = for i in rng
        @test !isassigned(ary, i)
        ary[i] = (Float64(i), i) # some non-cached content
        @test isassigned(ary, i)
    end
    # Check if the memory is initially zerod and fill it with value
    # to check if these values are not reused later.
    check_undef_and_fill(ary, 1:10)
    # Check if the memory grown at the end are zerod
    ccall(:jl_array_grow_end, Void, (Any, Csize_t), ary, 10)
    check_undef_and_fill(ary, 11:20)
    # Make sure the content of the memory deleted at the end are not reused
    ccall(:jl_array_del_end, Void, (Any, Csize_t), ary, 5)
    ccall(:jl_array_grow_end, Void, (Any, Csize_t), ary, 5)
    check_undef_and_fill(ary, 16:20)

    # Now check grow/del_end
    ary = Vector{Any}(1010)
    check_undef_and_fill(ary, 1:1010)
    # This del_beg should move the buffer
    ccall(:jl_array_del_beg, Void, (Any, Csize_t), ary, 1000)
    ccall(:jl_array_grow_beg, Void, (Any, Csize_t), ary, 1000)
    check_undef_and_fill(ary, 1:1000)
    ary = Vector{Any}(1010)
    check_undef_and_fill(ary, 1:1010)
    # This del_beg should not move the buffer
    ccall(:jl_array_del_beg, Void, (Any, Csize_t), ary, 10)
    ccall(:jl_array_grow_beg, Void, (Any, Csize_t), ary, 10)
    check_undef_and_fill(ary, 1:10)

    ary = Vector{Any}(1010)
    check_undef_and_fill(ary, 1:1010)
    ccall(:jl_array_grow_end, Void, (Any, Csize_t), ary, 10)
    check_undef_and_fill(ary, 1011:1020)
    ccall(:jl_array_del_end, Void, (Any, Csize_t), ary, 10)
    ccall(:jl_array_grow_beg, Void, (Any, Csize_t), ary, 10)
    check_undef_and_fill(ary, 1:10)

    # Make sure newly malloc'd buffers are filled with 0
    # test this for a few different sizes since we need to make sure
    # we are malloc'ing the buffer after the grow_end and malloc is not using
    # mmap directly (which may return a zero'd new page).
    for n in [50, 51, 100, 101, 200, 201, 300, 301]
        ary = Vector{Any}(n)
        # Try to free the previous buffer that was filled with random content
        # and to increase the chance of getting a non-zero'd buffer next time
        gc()
        gc()
        gc()
        ccall(:jl_array_grow_beg, Void, (Any, Csize_t), ary, 4)
        ccall(:jl_array_del_beg, Void, (Any, Csize_t), ary, 4)
        ccall(:jl_array_grow_end, Void, (Any, Csize_t), ary, n)
        ccall(:jl_array_grow_beg, Void, (Any, Csize_t), ary, 4)
        check_undef_and_fill(ary, 1:(2n + 4))
    end

    ary = Vector{Any}(100)
    ccall(:jl_array_grow_end, Void, (Any, Csize_t), ary, 10000)
    ary[:] = 1:length(ary)
    ccall(:jl_array_del_beg, Void, (Any, Csize_t), ary, 10000)
    # grow on the back until a buffer reallocation happens
    cur_ptr = pointer(ary)
    while cur_ptr == pointer(ary)
        len = length(ary)
        ccall(:jl_array_grow_end, Void, (Any, Csize_t), ary, 10)
        for i in (len + 1):(len + 10)
            @test !isassigned(ary, i)
        end
    end

    ary = Vector{Any}(100)
    ary[:] = 1:length(ary)
    ccall(:jl_array_grow_at, Void, (Any, Csize_t, Csize_t), ary, 50, 10)
    for i in 51:60
        @test !isassigned(ary, i)
    end
end

# check if we can run multiple finalizers at the same time
# Use a `@noinline` function to make sure the inefficient gc root generation
# doesn't keep the object alive.
@noinline function create_dead_object13995(finalized)
    obj = Ref(1)
    finalizer(obj, (x)->(finalized[1] = true))
    finalizer(obj, (x)->(finalized[2] = true))
    finalizer(obj, (x)->(finalized[3] = true))
    finalizer(obj, (x)->(finalized[4] = true))
    nothing
end
# disable GC to make sure no collection/promotion happens
# when we are constructing the objects
let gc_enabled13995 = gc_enable(false)
    finalized13995 = [false, false, false, false]
    create_dead_object13995(finalized13995)
    gc_enable(true)
    # obj is unreachable and young, a single young gc should collect it
    # and trigger all the finalizers.
    gc(false)
    gc_enable(false)
    @test finalized13995 == [true, true, true, true]
    gc_enable(gc_enabled13995)
end

# issue #15283
j15283 = 0
let
    k15283 = j15283+=1
end
@test j15283 == 1
@test !isdefined(:k15283)

# issue #15264
module Test15264
    mod1{T}(x::T) = x < 1 ? x : mod1(x-1)
end
@test Test15264.mod1 !== Base.mod1

module M15455
function rpm_provides{T}(r::T)
    push!([], select(r,T))
end
select(a,b) = 0
end
@test M15455.select(1,2)==0

# check that medium-sized array is 64-byte aligned (#15139)
@test Int(pointer(Vector{Float64}(1024))) % 64 == 0

# PR #15413
# Make sure arrayset can handle `Array{T}` (where `T` is a type and not a
# `TypeVar`) without crashing
let
    function arrayset_unknown_dim{T}(::Type{T}, n)
        Base.arrayset(reshape(Vector{T}(1), ones(Int, n)...), 2, 1)
    end
    arrayset_unknown_dim(Any, 1)
    arrayset_unknown_dim(Any, 2)
    arrayset_unknown_dim(Any, 3)
    arrayset_unknown_dim(Int, 1)
    arrayset_unknown_dim(Int, 2)
    arrayset_unknown_dim(Int, 3)
end

module TestSharedArrayResize
using Base.Test
# Attempting to change the shape of a shared array should unshare it and
# not modify the original data
function test_shared_array_resize{T}(::Type{T})
    len = 100
    a = Vector{T}(len)
    function test_unshare(f)
        a′ = reshape(reshape(a, (len ÷ 2, 2)), len)
        a[:] = 1:length(a)
        # The operation should fail on the owner shared array
        # and has no side effect.
        @test_throws ErrorException f(a)
        @test a == [1:len;]
        @test a′ == [1:len;]
        @test pointer(a) == pointer(a′)
        # The operation should pass on the non-owner shared array
        # and should unshare the arrays with no effect on the original one.
        f(a′)
        @test a == [1:len;]
        @test pointer(a) != pointer(a′)
    end

    test_unshare(a->ccall(:jl_array_del_end, Void, (Any, Csize_t), a, 0))
    test_unshare(a->ccall(:jl_array_del_end, Void, (Any, Csize_t), a, 1))
    test_unshare(a->ccall(:jl_array_del_beg, Void, (Any, Csize_t), a, 0))
    test_unshare(a->ccall(:jl_array_del_beg, Void, (Any, Csize_t), a, 1))
    test_unshare(a->deleteat!(a, 10))
    test_unshare(a->deleteat!(a, 90))
    test_unshare(a->ccall(:jl_array_grow_end, Void, (Any, Csize_t), a, 0))
    test_unshare(a->ccall(:jl_array_grow_end, Void, (Any, Csize_t), a, 1))
    test_unshare(a->ccall(:jl_array_grow_beg, Void, (Any, Csize_t), a, 0))
    test_unshare(a->ccall(:jl_array_grow_beg, Void, (Any, Csize_t), a, 1))
    test_unshare(a->insert!(a, 10, 10))
    test_unshare(a->insert!(a, 90, 90))
end
test_shared_array_resize(Int)
test_shared_array_resize(Any)
end

module TestArrayNUL
using Base.Test
function check_nul(a::Vector{UInt8})
    b = ccall(:jl_array_cconvert_cstring,
              Ref{Vector{UInt8}}, (Vector{UInt8},), a)
    @test unsafe_load(pointer(b), length(b) + 1) == 0x0
    return b === a
end

a = UInt8[]
b = "aaa"
c = [0x2, 0x1, 0x3]

@test check_nul(a)
@test check_nul(b.data)
@test check_nul(c)
d = [0x2, 0x1, 0x3]
@test check_nul(d)
push!(d, 0x3)
@test check_nul(d)
push!(d, 0x3)
@test check_nul(d)
ccall(:jl_array_del_end, Void, (Any, UInt), d, 2)
@test check_nul(d)
ccall(:jl_array_grow_end, Void, (Any, UInt), d, 1)
@test check_nul(d)
ccall(:jl_array_grow_end, Void, (Any, UInt), d, 1)
@test check_nul(d)
ccall(:jl_array_grow_end, Void, (Any, UInt), d, 10)
@test check_nul(d)
ccall(:jl_array_del_beg, Void, (Any, UInt), d, 8)
@test check_nul(d)
ccall(:jl_array_grow_beg, Void, (Any, UInt), d, 8)
@test check_nul(d)
ccall(:jl_array_grow_beg, Void, (Any, UInt), d, 8)
@test check_nul(d)
f = unsafe_wrap(Array, pointer(d), length(d))
@test !check_nul(f)
f = unsafe_wrap(Array, ccall(:malloc, Ptr{UInt8}, (Csize_t,), 10), 10, true)
@test !check_nul(f)
g = reinterpret(UInt8, UInt16[0x1, 0x2])
@test !check_nul(g)
@test check_nul(copy(g))
end

# Copy of `#undef`
copy!(Vector{Any}(10), Vector{Any}(10))
function test_copy_alias{T}(::Type{T})
    ary = T[1:100;]
    unsafe_copy!(ary, 1, ary, 11, 90)
    @test ary == [11:100; 91:100]
    ary = T[1:100;]
    unsafe_copy!(ary, 11, ary, 1, 90)
    @test ary == [1:10; 1:90]
end
test_copy_alias(Int)
test_copy_alias(Any)

# issue #15370
@test isdefined(Core, :Box)
@test !isdefined(Base, :Box)
@test !isdefined(Main, :Box)

# issue #1784
let a = [false]
function foo1784()
    (a,b) = try
        return true
        (0,1)
    finally
         a[1] = true
    end
end
@test foo1784()
@test a[1] == true
end

# issue #14113
module A14113
    using Base.Test
    # show that making several thousand methods (and lots of AST constants)
    # doesn't cause any serious issues (for example, for the serializer)
    # although to keep runtime on the order of several seconds for this test,
    # only several hundred of them are compiled / called
    for i = 1:2^14 + 256
        r = rand(2^4)
        code = Expr(:tuple, r...)
        f = @eval () -> $code
        i > (2^14 - 256) && @test [f()...] == r
    end
end

# issue #15425
@noinline function f15425(x)
end
@test f15425(1) === nothing

# issue #15809 --- TODO: this code should be disallowed
function f15809()
    global g15809
    g15809{T}(x::T) = T
end
f15809()
@test g15809(2) === Int

module Macro_Yielding_Global_Assignment
macro m()
    quote
        global x
        x = 2
    end
end
@m
end
@test Macro_Yielding_Global_Assignment.x == 2

# issue #15718
@test :(f($NaN)) == :(f($NaN))
@test isequal(:(f($NaN)), :(f($NaN)))

# PR #16011 Make sure dead code elimination doesn't delete push and pop
# of metadata
module TestDeadElim16011
using Base.Test

function count_expr_push(ex::Expr, head::Symbol, counter)
    if ex.head === head
        if ex.args[1] === :pop
            counter[] -= 1
        else
            counter[] += 1
        end
        return
    end
    for arg in ex.args
        isa(arg, Expr) && count_expr_push(arg, head, counter)
    end
    return false
end

function metadata_matches(ast::CodeInfo)
    inbounds_cnt = Ref(0)
    boundscheck_cnt = Ref(0)
    for ex in ast.code::Array{Any,1}
        if isa(ex, Expr)
            ex = ex::Expr
            count_expr_push(ex, :inbounds, inbounds_cnt)
            count_expr_push(ex, :boundscheck, boundscheck_cnt)
        end
    end
    @test inbounds_cnt[] == 0
    @test boundscheck_cnt[] == 0
end

function test_metadata_matches(f::ANY, tt::ANY)
    metadata_matches(code_typed(f, tt)[1][1])
end

function f1()
    @inbounds return 1
end
function f2()
    @boundscheck begin
        error()
    end
end
# No, don't write code this way...
@eval function f3()
    a = $(Expr(:boundscheck, true))
    return 1
    b = $(Expr(:boundscheck, :pop))
end
@noinline function g(a)
end
@eval function f4()
    g($(Expr(:inbounds, true)))
    @goto out
    g($(Expr(:inbounds, :pop)))
    @label out
end

test_metadata_matches(f1, Tuple{})
test_metadata_matches(f2, Tuple{})
test_metadata_matches(f3, Tuple{})
test_metadata_matches(f4, Tuple{})

end

# SSA value where the assignment is after the user in syntactic order
let f = function(a, b)
    @goto a
    @label b
    return j[1] + j[2] * 2
    @label a
    j = (a, b)
    @goto b
end
    @test f(1, 2) == 5
end

# issue #8712
type Issue8712; end
@test isa(invoke(Issue8712, ()), Issue8712)

# issue #16089
f16089(args...) = typeof(args)
g16089() = f16089(UInt8)
@test g16089() === Tuple{DataType}

# issue #16023
function f16023()
    x
    x = 1
end
@test_throws UndefVarError f16023()

# issue #16158
function f16158(x)
    bar(x) = length(x)==1 ? x : string(x, bar(x[1:end-1]))
    bar(x)
end
@test f16158("abc") == "abcaba"

# LLVM verifier error for noreturn function
# the `code_llvm(DevNull, ...)` tests are only meaningful on debug build
# with verifier on (but should still pass on release build).
module TestSSA16244

using Base.Test
@noinline k(a) = a

# unreachable branch due to `ccall(:jl_throw)`
function f1(a)
    if a
        b = (k(a) + 1, 3)
    else
        throw(DivideError())
    end
    b[1]
end
code_llvm(DevNull, f1, Tuple{Bool})
@test f1(true) == 2
@test_throws DivideError f1(false)

# unreachable branch due to function that does not return
@noinline g() = error()
function f2(a)
    if a
        b = (k(a) + 1, 3)
    else
        # Make sure type inference can infer the type of `g`
        g()
    end
    b[1]
end
code_llvm(DevNull, f2, Tuple{Bool})
@test f2(true) == 2
@test_throws ErrorException f2(false)

# SA but not SSA
function f3(a)
    if a
        b = (k(a) + 1, 3)
    end
    b[1]
end
code_llvm(DevNull, f3, Tuple{Bool})
@test f3(true) == 2
ex = try
    f3(false)
catch _ex
    _ex
end
@test isa(ex, UndefVarError)
@test ex.var === :b

# unreachable branch due to ccall that does not return
function f4(a, p)
    if a
        b = (k(a) + 1, 3)
    else
        ccall(p, Union{}, ())
    end
    b[1]
end
code_llvm(DevNull, f4, Tuple{Bool,Ptr{Void}})
@test f4(true, C_NULL) == 2
@test_throws UndefRefError f4(false, C_NULL)

# SSA due to const prop of condition
function f5(a)
    c = true
    if c
        b = (k(a) + 1, 3)
    end
    b[1]
end
code_llvm(DevNull, f5, Tuple{Bool})
@test f5(true) == 2
@test f5(false) == 1

# SSA due to const prop of condition
function f6(a)
    if 1 === 1
        b = (k(a) + 1, 3)
    end
    b[1]
end
code_llvm(DevNull, f6, Tuple{Bool})
@test f6(true) == 2
@test f6(false) == 1

# unreachable branch due to typeassert
function f7(a)
    if a
        b = (k(a) + 1, 3)
    else
        a = a::Int
    end
    b[1]
end
code_llvm(DevNull, f7, Tuple{Bool})
@test f7(true) == 2
@test_throws TypeError f7(false)

# unreachable branch due to non-Bool used in Bool context
function f8(a, c)
    if a
        b = (k(a) + 1, 3)
    else
        c && a
    end
    b[1]
end
code_llvm(DevNull, f8, Tuple{Bool,Int})
@test f8(true, 1) == 2
@test_throws TypeError f8(false, 1)

# unreachable branch due to undef local variable
function f9(a)
    if a
        b = (k(a) + 1, 3)
    else
        d
        d = 1
    end
    b[1]
end
code_llvm(DevNull, f9, Tuple{Bool})
@test f9(true) == 2
ex = try
    f9(false)
catch _ex
    _ex
end
@test isa(ex, UndefVarError)
@test ex.var === :d

end

# issue #16153
f16153(x) = 1
f16153(x::ANY, y...) = 2
@test f16153("") == 1
ff16153(x::ANY, y...) = 2
ff16153(x) = 1
@test ff16153("") == 1
g16153(x::ANY, y...) = 1
g16153(x::ANY, y::ANY) = 2
@test g16153(1, 1) == 2
gg16153(x::ANY, y::ANY) = 2
gg16153(x::ANY, y...) = 1
@test gg16153(1, 1) == 2

# don't remove global variable accesses even if we "know" their type
# see #16090
f16090() = typeof(undefined_x16090::Tuple{Type{Int}})
@test_throws UndefVarError f16090()
undefined_x16090 = (Int,)
@test_throws TypeError f16090()

# issue #12238
type A12238{T} end
type B12238{T,S}
    a::A12238{B12238{Int,S}}
end
@test B12238.types[1] === A12238{B12238{Int}}
@test A12238{B12238{Int}}.instance === B12238.types[1].instance

# issue #16315
let a = Any[]
    @noinline f() = a[end]
    @test (push!(a,10); f()) - (push!(a,2); f()) == 8
    @test a == [10, 2]
end

# issue #12096
let a = Val{Val{TypeVar(:_, Int, true)}},
    b = Val{Val{TypeVar(:_, Int)}}

    @test !isdefined(a, :instance)
    @test  isdefined(b, :instance)
    @test isleaftype(b)
end

# A return type widened to Type{Union{T,Void}} should not confuse
# codegen
@noinline MaybeFunc(T) = Union{T, Void}
fMaybeFunc() = MaybeFunc(Int64)
@test fMaybeFunc() == Union{Int64, Void}

# issue #16431
function f16431(x)
    z::Int = x * 2
    g(y) = begin z = z + y; y + x end
    z * g(x)
end
@test @inferred(f16431(1)) == 4

# issue #14878
type A14878
    ext
end
A14878() = A14878(Dict())
type B14878
end
B14878(ng) = B14878()
function trigger14878()
    w = A14878()
    w.ext[:14878] = B14878(junk)  # junk not defined!
    return w
end
@test_throws UndefVarError trigger14878()

# issue #1090
function f1090(x)::Int
    if x == 1
        return 1
    end
    2.0
end
@test f1090(1) === 1
@test f1090(2) === 2
g1090{T}(x::T)::T = x+1.0
@test g1090(1) === 2
@test g1090(Float32(3)) === Float32(4)

function f17613_2(x)::Float64
    try
        return x
    catch
        return x+1
    end
end
@test isa(f17613_2(1), Float64)

type A1090 end
Base.convert(::Type{Int}, ::A1090) = "hey"
f1090()::Int = A1090()
@test_throws TypeError f1090()

# issue #19106
function f19106()::Void end
@test f19106() === nothing

# issue #16783
function f16783()
    T = UInt32
    x::T = 0
    bar() = x+1
end
@test f16783()() == 1

# issue #16767
type A16767{T}
    a::Base.RefValue{T}
end
type B16767{T}
    b::A16767{B16767{T}}
end
type C16767{T}
    b::A16767{C16767{:a}}
end
@test B16767.types[1].types[1].parameters[1].types === Core.svec(A16767{B16767})
@test C16767.types[1].types[1].parameters[1].types === Core.svec(A16767{C16767{:a}})

# issue #16340
function f16340{T}(x::T)
    function g{T}(y::T)
        return (T,T)
    end
    return g
end
let g = f16340(1)
    @test isa(typeof(g).name.mt.defs.tvars, TypeVar)
end

# issue #16793
try
    abstract T16793
catch
end
@test isa(T16793, Type)
@test isa(abstract T16793_2, Void)

# issue #17147
f17147(::Tuple) = 1
f17147{N}(::Vararg{Tuple,N}) = 2
@test f17147((), ()) == 2

# issue #17449, argument evaluation order
@noinline f17449(x, y) = nothing
@noinline function g17449(r)
    r[] = :g
    return 1
end
@noinline function k17449(r, v)
    r[] = :k
    return v ? 1 : 1.0
end
function h17449(v)
    r = Ref(:h)
    f17449(g17449(r), k17449(r, v))
    return r[]
end
@test h17449(true) === :k

# make sure lowering agrees on sp order
function captsp{T, S}(x::T, y::S)
    subf(x2::Int) = T
    subf(x2::UInt) = S
    return subf(Int(1)), subf(UInt(1))
end
@test captsp(1, 2.0) == (Int, Float64)

# issue #15068
function sp_innersig{T}(x::T)
   subf(x2::T) = (x, x2, :a)
   subf(x2) = (x, x2, :b)
   return (subf(one(T)), subf(unsigned(one(T))))
end
@test sp_innersig(2) == ((2, 1, :a), (2, UInt(1), :b))

# TODO: also allow local variables?
#function local_innersig{T}(x::T)
#   V = typeof(x)
#   U = unsigned(T)
#   subf(x2::T, x3::Complex{V}) = (x, x2, x3)
#   subf(x2::U) = (x, x2)
#   return (subf(one(T), x * im), subf(unsigned(one(T))))
#end
#@test local_innersig(Int32(2)) == ((Int32(2), Int32(1), Int32(2)im), (Int32(2), UInt32(1)))
#@test local_innersig(Int64(3)) == ((Int64(3), Int64(1), Int64(3)im), (Int64(3), UInt64(1)))

# Issue 4914
let
    j(j) = j
    @test j(1) == 1
    k(x) = (k = x; k)
    @test k(1) == 1
end

# PR #18054: compilation of cfunction leaves IRBuilder in bad state,
#            causing heap-use-after-free when compiling f18054
function f18054()
    return Cint(0)
end
cfunction(f18054, Cint, ())

# issue #18986: the ccall optimization of cfunction leaves JL_TRY stack in bad state
dummy18996() = return nothing
function main18986()
    cfunction(dummy18986, Void, ())
    ccall((:dummy2, "this_is_a_nonexisting_library"), Void, ())
end
@test_throws ErrorException main18986()

# issue #18085
f18085(a,x...) = (0,)
for (f,g) in ((:asin,:sin), (:acos,:cos))
    gx = eval(g)
    f18085(::Type{Val{f}},x...) = map(x->2gx(x), f18085(Val{g},x...))
end
@test f18085(Val{:asin},3) === (0.0,)

# issue #18236 constant VecElement in ast triggers codegen assertion/undef
# VecElement of scalar
v18236 = VecElement(1.0)
ptr18236 = cfunction(identity, VecElement{Float64}, Tuple{VecElement{Float64}})
@eval @noinline f18236(ptr) = ccall(ptr, VecElement{Float64},
                                    (VecElement{Float64},), $v18236)
@test f18236(ptr18236) === v18236
@test !contains(sprint(code_llvm, f18236, Tuple{Ptr{Void}}), "double undef")
# VecElement of struct type, not necessarily useful but does have special
# ABI so should be handled correctly
# This struct should be small enough to be passed by value in C ABI
# in order to trigger the problematic code path.
# We should be at least testing this on some platforms.
# Not sure if there's a better way to trigger unboxing in codegen.
v18236_2 = VecElement((Int8(1), Int8(2)))
ptr18236_2 = cfunction(identity, VecElement{NTuple{2,Int8}},
                       Tuple{VecElement{NTuple{2,Int8}}})
@eval @noinline f18236_2(ptr) = ccall(ptr, VecElement{NTuple{2,Int8}},
                                      (VecElement{NTuple{2,Int8}},),
                                      $v18236_2)
@test f18236_2(ptr18236_2) === v18236_2

# issue #18385
function f18385(g)
    if g
        a = (1, 2)
    end
    return a[1]
end
@test f18385(true) === 1
# variable name in the error is tested above in `TestSSA16244`
@test_throws UndefVarError f18385(false)

# Another similar issue, make sure newvar nodes are created for the fields
# variables too.
function f18386(a, b, second_pass)
    s = 0
    firstpass = true
    for i in 1:2
        if firstpass
            x = (a, b)
            firstpass = !second_pass
        end
        s += x[1]
    end
    s
end
@test f18386(1, 2, false) === 2
# variable name in the error is tested above in `TestSSA16244`
@test_throws UndefVarError f18386(1, 2, true)

Base.@propagate_inbounds function f18412(a)
    @inbounds b = a[1]
    return b
end
@test f18412([1]) == 1

# issue #18173
function f18173()
    identity(()->successflag)
    successflag = false
end
@test f18173() == false

let _true = Ref(true), f, g, h
    @noinline f() = ccall((:time, "error_library_doesnt_exist\0"), Void, ()) # some expression that throws an error in codegen
    @noinline g() = _true[] ? 0 : h()
    @noinline h() = (g(); f())
    @test_throws ErrorException @code_native h() # due to a failure to compile f()
    @test g() == 0
end

fVararg(x) = Vararg{x}
gVararg(a::fVararg(Int)) = length(a)
@test gVararg(1,2,3,4,5) == 5

# issue #18577
@generated f18577() = quote ()->1 end
@test try
    f18577()
    false
catch e
    (e::ErrorException).msg
end == "generated function body is not pure. this likely means it contains a closure or comprehension."

# issue #10981, long argument lists
let a = fill(["sdf"], 2*10^6), temp_vcat(x...) = vcat(x...)
    # we introduce a new function `temp_vcat` to make sure there is no existing
    # method cache match, leading to a path that allocates a large tuple type.
    b = temp_vcat(a...)
    @test isa(b, Vector{String})
    @test length(b) == 2*10^6
    @test b[1] == b[end] == "sdf"
end

# issue #17255, take `deferred_alloc` into account
# when calculating total allocation size.
@noinline function f17255(n)
    gc_enable(false)
    b0 = Base.gc_bytes()
    local a
    for i in 1:n
        a, t, allocd = @timed [Ref(1) for i in 1:1000]
        @test allocd > 0
        b1 = Base.gc_bytes()
        if b1 < b0
            return false, a
        end
    end
    return true, a
end
@test f17255(10000)[1]
gc_enable(true)

# issue #18710
bad_tvars{T}() = 1
@test isa(@which(bad_tvars()), Method)
@test_throws MethodError bad_tvars()

# issue #19059 - test for lowering of `let` with assignment not adding Box in simple cases
contains_Box(e::GlobalRef) = (e.name === :Box)
contains_Box(e::ANY) = false
contains_Box(e::Expr) = any(contains_Box, e.args)

function let_noBox()
    local x
    for i = 1:2
        if i == 1
            x = 21
        end
        let x = x
            return () -> x
        end
    end
end
function let_Box1()
    local x
    for i = 1:2
        if i == 1
            x = 22
        end
        let y = x
            return () -> x
        end
    end
end
function let_Box2()
    local x
    for i = 1:2
        if i == 1
            x = 23
        end
        let x = x
            # In the future, this may change to no-Box if lowering improves
            return () -> x
            x = 43
        end
    end
end
function let_Box3()
    local x
    for i = 1:2
        if i == 1
            x = 24
        end
        let y
            # In the future, this may change to no-Box if lowering improves
            y = x
            return () -> x
        end
    end
end
function let_Box4()
    local x, g
    for i = 1:2
        if i == 1
            x = 25
        end
        let x = x
            g = () -> x
            x = 44
        end
        @test x == 25
        return g
    end
end
function let_Box5()
    local x, g, h
    for i = 1:2
        if i == 1
            x = 25
        end
        let x = x
            g = () -> (x = 46)
            h = () -> x
        end
        @test x == 25
        @test h() == 25
        @test g() == 46
        @test h() == 46
        @test x == 25
        return g
    end
end
@test any(contains_Box, (@code_lowered let_Box1()).code)
@test any(contains_Box, (@code_lowered let_Box2()).code)
@test any(contains_Box, (@code_lowered let_Box3()).code)
@test any(contains_Box, (@code_lowered let_Box4()).code)
@test any(contains_Box, (@code_lowered let_Box5()).code)
@test !any(contains_Box, (@code_lowered let_noBox()).code)
@test let_Box1()() == 22
@test let_Box2()() == 23
@test let_Box3()() == 24
@test let_Box4()() == 44
@test let_Box5()() == 46
@test let_noBox()() == 21
