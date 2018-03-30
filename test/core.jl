# This file is a part of Julia. License is MIT: https://julialang.org/license

# test core language features

using Random, SparseArrays, InteractiveUtils

const Bottom = Union{}


# For curmod_*
include("testenv.jl")

f47(x::Vector{Vector{T}}) where {T} = 0
@test_throws MethodError f47(Vector{Vector}())
@test f47(Vector{Vector{Int}}()) == 0

# checking unionall and typevar components
@test_throws TypeError ([] where T)
@test_throws TypeError ([T] where T)
@test_throws TypeError (Array{T} where T<:[])
@test_throws TypeError (Array{T} where T>:[])
@test_throws TypeError (Array{T} where T<:Vararg)
@test_throws TypeError (Array{T} where T>:Vararg)
@test_throws TypeError (Array{T} where T<:Vararg{Int})
@test_throws TypeError (Array{T} where T<:Vararg{Int,2})

@test_throws TypeError TypeVar(:T) <: Any
@test_throws TypeError TypeVar(:T) >: Any

# issue #12939
module Issue12939
abstract type Abs; end
struct Foo <: Abs; end
struct Bar; val::Int64; end
struct Baz; val::Int64; end
f(::Type{T}, x::T) where {T} = T(3)
f(::Type{Bar}, x::T) where {T <: Abs} = Bar(2)
f(::Type{Bar}, x) = Bar(1)
f(::Type{Baz}, x) = Baz(1)
f(::Type{Baz}, x::T) where {T <: Abs} = Baz(2)
end

@test Issue12939.f(Issue12939.Baz,Issue12939.Foo()) === Issue12939.Baz(2)
@test Issue12939.f(Issue12939.Bar,Issue12939.Foo()) === Issue12939.Bar(2)

# issue #11840
TT11840{T} = Tuple{T,T}
f11840(::Type) = "Type"
f11840(::DataType) = "DataType"
f11840(::UnionAll) = "UnionAll"
f11840(::Type{T}) where {T<:Tuple} = "Tuple"
@test f11840(Type) == "UnionAll"
@test f11840(Type.body) == "DataType"
@test f11840(Union{Int,Int8}) == "Type"
@test f11840(Tuple) == "Tuple"
@test f11840(TT11840) == "Tuple"

g11840(::DataType) = 1
g11840(::Type) = 2
g11840(sig::Type{T}) where {T<:Tuple} = 3
@test g11840(Vector.body) == 1
@test g11840(Vector) == 2
@test g11840(Vector.body) == 1
@test g11840(Vector) == 2
@test g11840(Tuple) == 3
@test g11840(TT11840) == 3

g11840b(::DataType) = 1
g11840b(::Type) = 2
# FIXME (needs a test): how to compute that the guard entry is still required,
# even though Type{Vector} ∩ DataType = Bottom and this method would set
# cache_with_orig = true
g11840b(sig::Type{T}) where {T<:Tuple} = 3
@test g11840b(Vector) == 2
@test g11840b(Vector.body) == 1
@test g11840b(Vector) == 2
@test g11840b(Vector.body) == 1
@test g11840b(Tuple) == 3
@test g11840b(TT11840) == 3

h11840(::DataType) = '1'
h11840(::Type) = '2'
h11840(::UnionAll) = '3'
h11840(::Type{T}) where {T<:Tuple} = '4'
@test h11840(Vector) == '3'
@test h11840(Vector.body) == '1'
@test h11840(Vector) == '3'
@test h11840(Union{Vector, Matrix}) == '2'
@test h11840(Union{Vector.body, Matrix.body}) == '2'
@test h11840(Tuple) == '4'
@test h11840(TT11840) == '4'

# issue #20511
f20511(x::DataType) = 0
f20511(x) = 1
Type{Integer}  # cache this
@test f20511(Union{Integer,T} where T <: Unsigned) == 1

# join
@test typejoin(Int8,Int16) === Signed
@test typejoin(Int,AbstractString) === Any
@test typejoin(Array{Float64},BitArray) <: AbstractArray
@test typejoin(Array{Bool},BitArray) <: AbstractArray{Bool}
@test typejoin(Tuple{Int,Int8},Tuple{Int8,Float64}) === Tuple{Signed,Real}
@test typejoin(Tuple{String,String}, Tuple{GenericString,String},
               Tuple{String,GenericString}, Tuple{Int,String,Int}) ==
    Tuple{Any,AbstractString,Vararg{Int}}
@test typejoin(Tuple{Int8,Vararg{Int}}, Tuple{Int8,Int8}) ==
    Tuple{Int8,Vararg{Signed}}
@test typejoin(Tuple{Int8,Vararg{Int}}, Tuple{Int8,Vararg{Int8}}) ==
    Tuple{Int8,Vararg{Signed}}
@test typejoin(Tuple{Int8,UInt8,Vararg{Int}}, Tuple{Int8,Vararg{Int8}}) ==
    Tuple{Int8,Vararg{Integer}}
@test typejoin(Union{Int,AbstractString}, Int) == Union{Int,AbstractString}
@test typejoin(Union{Int,AbstractString}, Int8) == Any
@test typejoin(Tuple{}, Tuple{Int}) == Tuple{Vararg{Int}}

# typejoin associativity
abstract type Foo____{K} end
mutable struct Wow____{K,V} <: Foo____{K} end
mutable struct Bar____{K,V} <: Foo____{K} end
let
    a = Wow____{Int64, Int64}
    b = Wow____{Int64, Float64}
    c = Bar____{Int64, Int64}
    @test typejoin(typejoin(b,c), a) == typejoin(typejoin(b,a), c) == Foo____{Int64}
end

# typejoin with Vararg{T,N}
@test typejoin(Tuple{Vararg{Int,2}}, Tuple{Int,Int,Int}) === Tuple{Int,Int,Vararg{Int}}
@test typejoin(Tuple{Vararg{Int,2}}, Tuple{Vararg{Int}}) === Tuple{Vararg{Int}}

# issue #26321
struct T26321{N,S<:NTuple{N}}
    t::S
end
let mi = T26321{3,NTuple{3,Int}}((1,2,3)), mf = T26321{3,NTuple{3,Float64}}((1.0,2.0,3.0))
    J = T26321{3,S} where S<:(Tuple{T,T,T} where T)
    @test typejoin(typeof(mi),typeof(mf)) == J
    a = [mi, mf]
    @test a[1] === mi
    @test a[2] === mf
    @test eltype(a) == J
    @test a isa Vector{<:T26321{3}}
end

# promote_typejoin returns a Union only with Nothing/Missing combined with concrete types
for T in (Nothing, Missing)
    @test Base.promote_typejoin(Int, Float64) === Real
    @test Base.promote_typejoin(Int, T) === Union{Int, T}
    @test Base.promote_typejoin(T, String) === Union{T, String}
    @test Base.promote_typejoin(Vector{Int}, T) === Union{Vector{Int}, T}
    @test Base.promote_typejoin(Vector, T) === Any
    @test Base.promote_typejoin(Real, T) === Any
    @test Base.promote_typejoin(Int, String) === Any
    @test Base.promote_typejoin(Int, Union{Float64, T}) === Any
    @test Base.promote_typejoin(Int, Union{String, T}) === Any
    @test Base.promote_typejoin(T, Union{}) === T
    @test Base.promote_typejoin(Union{}, T) === T
end

@test promote_type(Bool,Bottom) === Bool

# type declarations

abstract type Sup_{A,B} end
abstract type Qux_{T} <: Sup_{Qux_{Int},T} end

@test Qux_{Int}.super <: Sup_
@test ===(Qux_{Int}, Qux_{Int}.super.parameters[1])
@test ===(Qux_{Int}.super.parameters[2], Int)
@test Qux_{Char}.super <: Sup_
@test ===(Qux_{Int}, Qux_{Char}.super.parameters[1])
@test ===(Qux_{Char}.super.parameters[2], Char)

@test Qux_.body.super.parameters[1].super <: Sup_
@test ===(Qux_{Int}, Qux_.body.super.parameters[1].super.parameters[1])
@test ===(Int, Qux_.body.super.parameters[1].super.parameters[2])

mutable struct Foo_{T} x::Foo_{Int} end

@test ===(Foo_.body.types[1], Foo_{Int})
@test ===(Foo_.body.types[1].types[1], Foo_{Int})

mutable struct Circ_{T} x::Circ_{T} end
@test ===(Circ_{Int}, Circ_{Int}.types[1])

abstract type Sup2a_ end
abstract type Sup2b_{A <: Sup2a_, B} <: Sup2a_ end
@test_throws ErrorException @eval abstract type Qux2_{T} <: Sup2b_{Qux2_{Int}, T} end # wrapped in eval to avoid #16793

# issue #21923
struct A21923{T,N}; v::Vector{A21923{T}}; end
@test fieldtype(A21923,1) == Vector{A21923{T}} where T
struct B21923{T,N}; v::Vector{B21923{T,M} where M}; end
@test fieldtype(B21923, 1) == Vector{B21923{T,M} where M} where T
struct C21923{T,N}; v::C21923{T,M} where M; end
@test fieldtype(C21923, 1) == C21923
struct D21923{T,N}; v::D21923{T}; end
@test fieldtype(D21923, 1) == D21923

# issue #22624, more circular definitions
struct T22624{A,B,C}; v::Vector{T22624{Int64,A}}; end
let elT = T22624.body.body.body.types[1].parameters[1]
    @test elT == T22624{Int64, T22624.var, C} where C
    elT2 = elT.body.types[1].parameters[1]
    @test elT2 == T22624{Int64, Int64, C} where C
    @test elT2.body.types[1].parameters[1] === elT2
    @test Base.isconcretetype(elT2.body.types[1])
end

# issue #3890
mutable struct A3890{T1}
    x::Matrix{Complex{T1}}
end
@test A3890{Float64}.types[1] === Array{Complex{Float64},2}
# make sure the field type Matrix{Complex{T1}} isn't cached
mutable struct B3890{T2}
    x::Matrix{Complex{T2}}
end
@test B3890{Float64}.types[1] === Array{Complex{Float64},2}

# issue #786
mutable struct Node{T}
    v::Vector{Node}
end

@test ===(Node{Int}.types[1].parameters[1], Node)

mutable struct Node2{T}
    v::Vector{Node2{T}}
end

@test ===(Node2{Int}.types[1].parameters[1], Node2{Int})

mutable struct FooFoo{A,B} y::FooFoo{A} end

@test FooFoo{Int} <: FooFoo{Int,AbstractString}.types[1]


let x = (2,3)
    @test +(x...) == 5
end

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

function bar(x::T) where T
    local z::Complex{T}
    z = x
    z
end
@test bar(3.0) == Complex(3.0,0.0)

z = convert(Complex{Float64},2)
@test z == Complex(2.0,0.0)

function typeassert_instead_of_decl()
    local x
    x = 1
    x::Float64
    return 0
end
@test_throws TypeError typeassert_instead_of_decl()

# type declarations on globals not implemented yet
@test_throws ErrorException eval(Meta.parse("global x20327::Int"))

y20327 = 1
@test_throws TypeError y20327::Float64

# misc
fib(n) = n < 2 ? n : fib(n-1) + fib(n-2)
@test fib(20) == 6765

# static parameters
sptest1(x::T, y::T) where {T} = 42
sptest1(x::T, y::S) where {T,S} = 43
@test sptest1(1,2) == 42
@test sptest1(1,"b") == 43

sptest2(x::T) where {T} = T
@test ===(sptest2(:a),Symbol)

sptest3(x::T) where {T} = y->T
let m = sptest3(:a)
    @test ===(m(0),Symbol)
end

sptest4(x::T, y::T) where {T} = 42
sptest4(x::T, y) where {T} = 44
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
    return (x -> i)
end
let f = i18408()
    @test_throws UndefVarError(:i) f(0)
end

# issue #23558
c23558(n,k) =
    let fact(n) = if (n == 0) 1 else n*fact(n-1) end
        fact(n)/fact(k)/fact(n-k)
    end
@test c23558(10, 5) == 252

# issue #23996
function foo23996(xs...)
    rets = []
    bar(::Int) = push!(rets, 1)
    foobar() = push!(rets, 3)
    bar(::AbstractFloat) = push!(rets, 2)
    bar(::Bool) = foobar()
    for x in xs
	bar(x)
    end
    rets
end
@test foo23996(1,2.0,false) == [1,2,3]

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

# issue #7234
f7234_cnt = 0
begin
    glob_x2 = 24
    function f7234_a()
        global f7234_cnt += 1
        glob_x2 += 1
        global f7234_cnt += -10000
    end
end
@test_throws UndefVarError(:glob_x2) f7234_a()
@test f7234_cnt == 1
begin
    global glob_x2 = 24
    function f7234_b()
        global f7234_cnt += 1
        glob_x2 += 1
        global f7234_cnt += -10000
    end
end
@test_throws UndefVarError(:glob_x2) f7234_b()
@test f7234_cnt == 2
# globals can accessed if declared
for i = 1:2
    global glob_x2 += 1
end
@test glob_x2 == 26
# globals declared as such in a non-global scope are inherited
let
    global glob_x3 = 11
    f7234_2() = (glob_x3 += 1)
    f7234_2()
end
@test glob_x3 == 12

# interaction between local variable renaming and nested globals (#19333)
x19333 = 1
function f19333(x19333)
    return let x19333 = x19333
        g19333() = (global x19333 += 2)
        g19333() + (x19333 += 1)
    end + (x19333 += 1)
end
@test f19333(0) == 5
@test f19333(0) == 7
@test x19333 == 5

function h19333()
    s = 0
    for (i, j) in ((1, 2),)
        s += i + j # use + as a global
    end
    for (k, +) in ((3, 4),)
        s -= (k - +) # use + as a local
    end
    return s
end
@test h19333() == 4

# let - new variables, including undefinedness
let_undef_cnt = 0
function let_undef()
    first = true
    for i = 1:2
        let x # new x
            if first # not defined on second pass
                x = 1
                first = false
            end
            global let_undef_cnt += 1
            x + 1
            global let_undef_cnt += 23
        end
    end
end
@test_throws UndefVarError let_undef()
@test let_undef_cnt == 25

# const implies local in a local scope block
function const_implies_local()
    let
        x = 1
        local y
        let
            # TODO: change back to `const` if that's ever allowed
            local x = 0
            y = x
        end
        x, y
    end
end
@test const_implies_local() === (1, 0)

a_global_closure_vector = Vector{Any}(undef, 3)
for i = 1:3
    let ii = i
        a_global_closure_vector[i] = x -> x + ii
    end
end
@test a_global_closure_vector[1](10) == 11
@test a_global_closure_vector[2](10) == 12
@test a_global_closure_vector[3](10) == 13

# issue #22032
let a = [], fs = []
    for f() in 1:3
        push!(a, f())
        push!(fs, f)
    end
    @test a == [1,2,3]
    @test [f() for f in fs] == [1,2,3]
end
let t = (22,33)
    (g(), x) = t
    @test g() == 22
    @test x == 33
end

# issue #23091
let (f(), x) = (1, 2)
    @test f() == 1
    @test x == 2
end

# issue #21900
f21900_cnt = 0
function f21900()
    for i = 1:1
        x = 0
    end
    global f21900_cnt += 1
    x # should be global
    global f21900_cnt += -1000
    nothing
end
@test_throws UndefVarError(:x) f21900()
@test f21900_cnt == 1

# use @eval so this runs as a toplevel scope block
@test_throws UndefVarError(:foo21900) @eval begin
    for i21900 = 1:10
        local bar21900
        for j21900 = 1:10
            foo21900 = 10
        end
        bar21900 = 0
        bar21900 = foo21900 + 1
    end
end
@test !@isdefined(foo21900)
@test !@isdefined(bar21900)
bar21900 = 0
@test_throws UndefVarError(:foo21900) @eval begin
    for i21900 = 1:10
        global bar21900
        for j21900 = 1:10
            foo21900 = 10
        end
        bar21900 = -1
        bar21900 = foo21900 + 1
    end
end
@test bar21900 == -1
@test !@isdefined foo21900
foo21900 = 0
@test nothing === begin
    for i21900 = 1:10
        global bar21900, foo21900
        for j21900 = 1:10
            foo21900 = 10
        end
        bar21900 = -1
        bar21900 = foo21900 + 1
    end
end
@test foo21900 == 10
@test bar21900 == 11

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
mutable struct UndefField
    field
    UndefField() = new()
end

let
    local a
    a = UndefField()
    @test !isdefined(a, :field)
    @test !isdefined(a, :foo)
    @test !isdefined(2, :a)

    @test_throws TypeError isdefined(2)
end

let
    local a
    a = Vector{Any}(undef, 2)
    @test !isassigned(a,1) && !isassigned(a,2)
    a[1] = 1
    @test isassigned(a,1) && !isassigned(a,2)
    a = Vector{Float64}(undef,1)
    @test isassigned(a,1)
    @test isassigned(a)
    @test !isassigned(a,2)
end

# isassigned, issue #11167
mutable struct Type11167{T,N} end
Type11167{Int,2}
let tname = Type11167.body.body.name
    @test !isassigned(tname.cache, 0)
    @test isassigned(tname.cache, 1)
    @test !isassigned(tname.cache, 2)
    Type11167{Float32,5}
    @test isassigned(tname.cache, 2)
    @test !isassigned(tname.cache, 3)
end

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

    fooN(A::Array{T,N}, x::Vararg{Any,N}) where {T,N} = -1
    @test fooN([1,2], 1) == -1
    @test_throws MethodError fooN([1,2], 1, 2) == -1
    @test fooN([1 2; 3 4], 1, 2) == -1
    @test_throws MethodError fooN([1 2; 3 4], 1)
    @test_throws MethodError fooN([1 2; 3 4], 1, 2, 3)

    bar(x::Tuple{T,T,T,T}) where {T} = 1
    bar(x::Tuple{Any,Any,Any,Any})=2
    @test bar((1,1,1,1)) == 1
    @test bar((1,1,1,"a")) == 2
    @test bar((:a,:a,:a,:a)) == 1

    baz(::Type{Rational}) = 1
    baz(::Type{Rational{T}}) where {T} = 2
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

mutable struct MyArray{N} <: AbstractArray{Int, N}
end
let
    local x
    x = MyArray{1}()
    foob(x::AbstractArray) = 0
    foob(x::AbstractVector{T}) where {T} = 1
    @test foob(x) == 1
end

let
    local f, g, a
    f(a::Vector{Vector{T}}) where {T} = a
    g(a::Vector{Vector{T}}) where {T} = a
    a = Vector{Int}[]
    @test ===(f(a), a)
    @test ===(g(a), a)
end

mutable struct _AA{T}; a::T; end
_AoA{T} = _AA{_AA{T}}
let
    local g, a
    g(a::_AA{_AA{T}}) where {T} = a
    a = _AA(_AA(1))
    @test ===(g(a),a)
end

# dispatch using Val{T}. See discussion in #9452, #22475 for instances vs types
let
    local firstlast
    firstlast(::Val{true}) = "First"
    firstlast(::Val{false}) = "Last"

    @test firstlast(Val(true)) == "First"
    @test firstlast(Val(false)) == "Last"
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
    try_finally_glo_after = 0
    try_finally_loc_after = 0
    try_finally_glo_b = try
        1+2
    finally
        # try_finally_loc_after = 1 # enable with #19324
        global try_finally_glo_after = 1
    end
    @test try_finally_loc_after == 0
    @test try_finally_glo_b == 3
    @test try_finally_glo_after == 1

    try_finally_glo_after = 0
    gothere = 0
    try
        try
            error(" ")
        finally
            # try_finally_loc_after = 1 # enable with #19324
            global try_finally_glo_after = 1
        end
        global gothere = 1
    end
    @test try_finally_loc_after == 0
    @test try_finally_glo_after == 1
    @test gothere == 0

    try_finally_glo_after = 0
    try_finally_glo_b = try
        error(" ")
    catch
        42
    finally
        # try_finally_loc_after = 1 # enable with #19324
        global try_finally_glo_after = 1
    end
    @test try_finally_loc_after == 0
    @test try_finally_glo_b == 42
    @test try_finally_glo_after == 1

    global glo = 0
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

# issue #12806
let i = 0, x = 0
    for outer i = 1:10
        try
            break
        finally
            x = 1
        end
    end
    @test i == 1
    @test x == 1
end

let i = 1, a = []
    while true
        try
            push!(a, i)
            i += 1
            i < 5 && continue
            break
        catch
            push!(a, "catch")
        finally
            push!(a, "finally")
        end
    end
    @test a == [1, "finally", 2, "finally", 3, "finally", 4, "finally"]
end

function _two_finally(n)
    a = []
    for i = 1:5
        push!(a, i)
        try
            try
                n == 1 && break
                n == 2 && i > 1 && return [copy(a), a]
            finally
                push!(a, "finally 1")
            end
        finally
            push!(a, "finally 2")
        end
    end
    return a
end
@test _two_finally(1) == [1, "finally 1", "finally 2"]
@test _two_finally(2) == [[1, "finally 1", "finally 2", 2],
                          [1, "finally 1", "finally 2", 2, "finally 1", "finally 2"]]

let i = 0
    caught = nothing
    try
        try
            error("oops")
        catch
            throw(42)
        finally
            i = 1
        end
    catch e
        caught = e
    end
    @test caught == 42
    @test i == 1
end

let i = 0, a = []
    for i = 1:2
        try
            continue
        finally
            push!(a, "finally")
        end
        push!(a, "oops")
    end
    @test a == ["finally", "finally"]
end

# test from #13660
let x = 0, y = 0, z = 0
    for i = 1:2
        try
            i == 1 && continue
        finally
            x = 11
        end
        try
            i == 2 && throw(42)
        catch
            break
        finally
            y = 12
        end
    end
    for i = 1:2
        try i == 1 && break
        finally z = 13
        end
    end
    @test x == 11
    @test y == 12
    @test z == 13
end

function test12806()
    let catchb = false, catchc = false, catchr = false, a = []
        for i in 1:3
            try
                throw("try err")
            catch e
                i == 1 && break
                i == 2 && continue
                i == 3 && return (catchb, catchc, catchr, a)
            finally
                i == 1 && (catchb = true; continue)
                i == 2 && (catchc = true; )
                i == 3 && (catchr = true; push!(a, 1))
            end
        end
    end
end
@test test12806() == (true, true, false, [1])

# issue #24331
try
    c24331 = 1
finally
end
@test !isdefined(@__MODULE__, :c24331)
function f24331()
    try
        x = [2]
    finally
    end
end
@test f24331() == [2]

# finalizers
let A = [1]
    local x = 0
    finalizer(a->(x+=1), A)
    finalize(A)
    @test x == 1
    A = 0
    GC.gc(); GC.gc()
    @test x == 1
end

# Module() constructor
@test names(Module(:anonymous), all = true, imported = true) == [:anonymous]
@test names(Module(:anonymous, false), all = true, imported = true) == [:anonymous]

# exception from __init__()
let didthrow =
    try
        include_string(
            @__MODULE__,
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
mutable struct TestMutable
    file::String
    line::Int
    error
end

let
    local z = complex(3, 4)
    v = Int[0, 0]
    for i = 1:2
        v[i] = getfield(z, i)
    end
    @test v == [3, 4]
    @test_throws BoundsError(z, -1) getfield(z, -1)
    @test_throws BoundsError(z, 0) getfield(z, 0)
    @test_throws BoundsError(z, 3) getfield(z, 3)

    strct = LoadError("yofile", 0, "bad")
    @test nfields(strct) == 3 # sanity test
    @test_throws BoundsError(strct, 10) getfield(strct, 10)
    @test_throws ErrorException("type LoadError is immutable") setfield!(strct, 0, "")
    @test_throws ErrorException("type LoadError is immutable") setfield!(strct, 4, "")
    @test_throws ErrorException("type is immutable") setfield!(strct, :line, 0)
    @test strct.file == "yofile"
    @test strct.line === 0
    @test strct.error == "bad"
    @test getfield(strct, 1) == "yofile"
    @test getfield(strct, 2) === 0
    @test getfield(strct, 3) == "bad"

    mstrct = TestMutable("melm", 1, nothing)
    Base.setproperty!(mstrct, :line, 8.0)
    @test mstrct.line === 8
    @test_throws TypeError(:setfield!, "", Int, 8.0) setfield!(mstrct, :line, 8.0)
    @test_throws TypeError(:setfield!, "", Int, 8.0) setfield!(mstrct, 2, 8.0)
    setfield!(mstrct, 3, "hi")
    @test mstrct.error == "hi"
    setfield!(mstrct, 1, "yo")
    @test mstrct.file == "yo"
    @test_throws BoundsError(mstrct, 10) getfield(mstrct, 10)
    @test_throws BoundsError(mstrct, 0) setfield!(mstrct, 0, "")
    @test_throws BoundsError(mstrct, 4) setfield!(mstrct, 4, "")
end

# test getfield-overloading
function Base.getproperty(mstrct::TestMutable, p::Symbol)
    return (p, getfield(mstrct, :error))
end
function Base.setproperty!(mstrct::TestMutable, p::Symbol, v)
    setfield!(mstrct, :error, (p, v))
end

let
    mstrct = TestMutable("melm", 1, nothing)
    @test mstrct.line === (:line, nothing)
    @test mstrct.bar === (:bar, nothing)
    @test getfield(mstrct, 1) == "melm"
    @test getfield(mstrct, :file) == "melm"
    @test_throws MethodError Base.getproperty(mstrct, 1)
    mstrct.error = 8.0
    @test mstrct.bar === (:bar, (:error, 8.0))
    mstrct.line = 8.0
    @test getfield(mstrct, :line) === 1
    @test getfield(mstrct, :error) === (:line, 8.0)
    @test mstrct.bar === (:bar, (:line, 8.0))
    @test mstrct.error === (:error, (:line, 8.0))
end

# allow typevar in Union to match as long as the arguments contain
# sufficient information
# issue #814
let
    local MatOrNot, my_func, M
    MatOrNot{T} = Union{AbstractMatrix{T}, Vector{Union{}}}
    my_func(A::MatOrNot{T}, B::MatOrNot{T}, C::MatOrNot{T}) where {T<:Real} = 0
    M = [ 2. 1. ; 1. 1. ]
    @test my_func(Union{}[], M, M) == 0
end

let
    local my_func, a, c
    my_func(P::Vector{T}, Q::Vector{T}) where {T} = 0
    my_func(x::T, P::Vector{T}) where {T} = 1
    my_func(P::Vector{T}, x::T) where {T} = 2
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
    baar(x::UnionAll) = 2
    @test baar(StridedArray) == 2
    @test baar(Base.unwrap_unionall(StridedArray)) == 1
    @test baar(Vector) == 2
    @test baar(Vector.body) == 0

    boor(x) = 0
    boor(x::Union) = 1
    @test boor(StridedArray) == 0
    @test boor(Base.unwrap_unionall(StridedArray)) == 1

    # issue #1202
    foor(x::Union) = 1
    @test_throws MethodError foor(StridedArray)
    @test foor(Base.unwrap_unionall(StridedArray)) == 1
    @test_throws MethodError foor(StridedArray)
end

# issue #22842
f22842(x::UnionAll) = UnionAll
f22842(x::DataType) = length(x.parameters)
@test f22842(Tuple{Vararg{Int64,N} where N}) == 1
@test f22842(Tuple{Vararg{Int64,N}} where N) === UnionAll

# issue #1153
mutable struct SI{m, s, kg}
    value::AbstractFloat
end

import Base.*

*(x::SI{m1, s1, kg1}, y::SI{m2, s2, kg2}) where {m1, m2, s1, s2, kg1, kg2} = SI{m1 + m2, s1 + s2, kg1 + kg2}(x.value * y.value)

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
   @test eltype(a) == Nothing

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

@test unsafe_pointer_to_objref(ccall(:jl_call1, Ptr{Cvoid}, (Any,Any),
                                     x -> x+1, 314158)) == 314159
let x = [1,2,3]
    @test unsafe_pointer_to_objref(pointer_from_objref(x)) == x
    @test unsafe_pointer_to_objref(pointer_from_objref(x)) === x
end

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
    aaa = unsafe_wrap(Array, pointer(a), (1, 1))
    @test size(aaa) == (1, 1)
    @test aaa[1] == a[1]
    @test_throws InexactError unsafe_wrap(Array, pointer(a), -3)
    # Misaligned pointer
    res = @test_throws ArgumentError unsafe_wrap(Array, pointer(a) + 1, length(a))
    @test occursin("is not properly aligned to $(sizeof(Int)) bytes", res.value.msg)
    res = @test_throws ArgumentError unsafe_wrap(Array, pointer(a) + 1, (1, 1))
    @test occursin("is not properly aligned to $(sizeof(Int)) bytes", res.value.msg)
end

struct FooBar2515
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
mutable struct S1442{T}
end

let
    local f1442
    f1442(::DataType) = 1
    f1442(::Type{S1442{T}}) where {T} = 2

    @test f1442(S1442{Int}) == 2
    @test f1442(DataType) == 1
end

# issue #1727
abstract type Component end

mutable struct Transform <: Component
    x
    y
    z

    Transform() = new(0, 0, 0)
end

mutable struct Body <: Component
    vel
    curr_force

    Body() = new(0, 0)
end

function NewEntity(components::Type{T}...) where T<:Component
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
    r = Vector{UnitRange{Int}}(undef, 1)
    r[1] = 2:3
    X[r...] *= 2
    @test X == [1,4,6,4]
end
end

# issue #1632
let
    f1632(::R, ::S) where {R,S} = 1
    f1632(::T, ::T) where {T} = 2
    @test f1632(1, 2) == 2
    @test f1632(:a, 2) == 1
    g1632(::T, ::T) where {T} = 2
    g1632(::R, ::S) where {R,S} = 1
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
    i2169(a::Array{T}) where {T} = typemin(T)
    @test invoke(i2169, Tuple{Array}, Int8[1]) === Int8(-128)
end

# issue #2365
mutable struct B2365{T}
     v::Union{T, Nothing}
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
struct Foo2509; foo::Int; end
@test Foo2509(1) != Foo2509(2)
@test Foo2509(42) == Foo2509(42)

# issue #2517
struct Foo2517; end
@test repr(Foo2517()) == "$(curmod_prefix)Foo2517()"
@test repr(Vector{Foo2517}(undef, 1)) == "$(curmod_prefix)Foo2517[$(curmod_prefix)Foo2517()]"
@test Foo2517() === Foo2517()

# issue #1474
mutable struct X1474{a,b} end
let
    local Y
    Y(::Type{X1474{A,B}}) where {A,B} = 1
    Y(::Type{X1474{A}}) where {A} = 2
    Y(::Type{X1474}) = 3
    @test Y(X1474) == 3
    @test Y(X1474{Int}) == 2
    @test Y(X1474{Int,AbstractString}) == 1
end

# issue #2562
mutable struct Node2562{T}
    value::T
    Node2562{T}(value::T) where T = new(value)
end
Node2562(value::T, args...) where {T} = Node2562{T}(value, args...)
makenode2562(value) = Node2562(value)
@test isa(Node2562(0), Node2562)
@test isa(makenode2562(0), Node2562)

# issue #2619
mutable struct I2619{T}
    v::T
    I2619{T}(v) where T = new(convert(T,v))
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
const Foo2919 = Int
mutable struct Baz2919; Foo2919::Foo2919; end
@test Baz2919(3).Foo2919 === 3

# issue #2982
module M2982
abstract type U end
macro bad(Y)
    quote
        mutable struct $(esc(Y)) <: U
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
    convert(Vector{typeof(y[1])}, y)
end
@test isa(f3471(Any[1.0,2.0]), Vector{Float64})

# issue #3729
A3729{B} = Vector{Vector{B}}
C3729{D} = Vector{Vector{D}}
@test Vector{Vector{Int}} === A3729{Int} === C3729{Int}

# issue #3789
x3789 = 0
while(all([false for idx in 1:10]))
    global x3789 = 1
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
struct Foo4075
    x::Int64
    y::Float64
end

function foo4075(f::Foo4075, s::Symbol)
    x = getfield(f,s)
    GC.gc()
    x
end

@test isa(foo4075(Foo4075(Int64(1),2.0),:y), Float64)
# very likely to segfault the second time if this is broken
@test isa(foo4075(Foo4075(Int64(1),2.0),:y), Float64)

# issue #3167
let
    function foo(x)
        ret=Vector{typeof(x[1])}(undef, length(x))
        for j = 1:length(x)
            ret[j] = x[j]
        end
        return ret
    end
    x = Vector{Union{Dict{Int64,AbstractString},Array{Int64,3},Number,AbstractString,Nothing}}(undef, 3)
    x[1] = 1.0
    x[2] = 2.0
    x[3] = 3.0
    @test foo(x) == [1.0, 2.0, 3.0]
end

# issue #4115
mutable struct Foo4115 end
const Foo4115s = NTuple{3, Union{Foo4115, Type{Foo4115}}}
baz4115(x::Foo4115s) = x
let t = (Foo4115, Foo4115, Foo4115())
    @test_throws MethodError baz4115(t)
end

# issue #4129
mutable struct Foo4129; end

abstract type Bar4129 end

mutable struct Bar41291 <: Bar4129
    f::Foo4129
end
mutable struct Bar41292 <: Bar4129
    f::Foo4129
end

mutable struct Baz4129
    b::Bar4129
end

foo4129(a::Baz4129,c::Foo4129,b::Bar4129,@nospecialize(x),y) = (a,b,c,x,y)
foo4129(a::Baz4129,b::Bar41291,args...) = foo4129(a,b.f,b,args...)
foo4129(a::Baz4129,b::Bar41292,args...) = foo4129(a,b.f,b,args...)
foo4129(a::Baz4129,args...)         = foo4129(a,a.b,args...)

@test isa(foo4129(Baz4129(Bar41291(Foo4129())),1,2), Tuple{Baz4129,Bar4129,Foo4129,Int,Int})

# issue #4141
mutable struct Vertex4141{N,T}; end
mutable struct Face4141{V}; end
mutable struct Hull4141{F<:Face4141}; end

g4141(N,T) = Hull4141{Face4141{Vertex4141{N,T}}}()
@test isa(g4141(4,Int), Hull4141{Face4141{Vertex4141{4,Int}}})

# issue #4154
mutable struct MyType4154{T}
    a1::T
    a2
end

foo4154(x) = MyType4154(x, [])
h4154() = typeof(foo4154(rand(2,2,2)))
g4154() = typeof(foo4154(rand(2,2,2,2,2,2,2,2,2)))

@test h4154() === MyType4154{Array{Float64,3}}
@test g4154() === MyType4154{Array{Float64,9}}

# issue #4208
mutable struct a4208
    a4208
end
@test isa(a4208(5),a4208)
mutable struct b4208
    b4208() = (local b4208=1;new())
end
@test isa(b4208(),b4208)

# make sure convert_default error isn't swallowed by typeof()
convert_default_should_fail_here() = similar([1],typeof(zero(typeof(rand(2,2)))))
@test_throws MethodError convert_default_should_fail_here()

# issue #4343
@test_throws ErrorException Array{Float64}{Int, 2}

mutable struct Foo4376{T}
    x
    Foo4376{T}(x::T) where T = new(x)
    Foo4376{T}(a::Foo4376{Int}) where T = new(a.x)
end

@test isa(Foo4376{Float32}(Foo4376{Int}(2)), Foo4376{Float32})

mutable struct _0_test_ctor_syntax_
    _0_test_ctor_syntax_(files::Vector{T},step) where {T<:AbstractString} = 0
end

# issue #4413
mutable struct A4413 end
mutable struct B4413 end
mutable struct C4413 end
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
f4482(x::T) where {T} = T
@test f4482((Ptr,Ptr)) === Tuple{UnionAll,UnionAll}
@test f4482((Ptr,))    === Tuple{UnionAll,}

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
    g4505(::X) where {X} = 0
    @test g4505(0) == 0
end
@test !@isdefined g4505
@test !isdefined(@__MODULE__, :g4505)

# issue #4681
# ccall should error if convert() returns something of the wrong type
mutable struct Z4681
    x::Ptr{Cvoid}
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
        global b4688(y::Int) = x == true ? a4688(y) : a4688(y)
    end
end
@test b4688(1) == "an Int"

# issue #4731
mutable struct SIQ{A,B} <: Number
    x::A
end
import Base: promote_rule
promote_rule(A::Type{SIQ{T,T2}},B::Type{SIQ{S,S2}}) where {T,T2,S,S2} = SIQ{promote_type(T,S)}
@test_throws ErrorException promote_type(SIQ{Int},SIQ{Float64})
f4731(x::T...) where {T} = ""
f4731(x...) = 0
g4731() = f4731()
@test f4731() == ""
@test g4731() == ""

# issue #4675
f4675(x::StridedArray...) = 1
f4675(x::StridedArray{T}...) where {T} = 2
@test f4675(zeros(50,50), zeros(50,50)) == 2
g4675(x::StridedArray{T}...) where {T} = 2
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
abstract type IT4805{N, T} end

let
    test0(::Type{IT4805{1, T}}, x) where {T <: Int64} = x
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
f5150(T) = Vector{Rational{T}}(undef, 1)
@test typeof(f5150(Int)) === Vector{Rational{Int}}


# issue #5165
primitive type T5165{S} 64 end
make_t(x::Int64) = Core.Intrinsics.bitcast(T5165{Nothing}, x)
xs5165 = T5165[make_t(Int64(1))]
b5165 = IOBuffer()
for x in xs5165
    println(b5165, x)   # segfaulted
end

# support tuples as type parameters

mutable struct TupleParam{P}
    x::Bool
end

function tupledispatch(a::TupleParam{(1,:a)})
    a.x
end

# tuples can be used as type params
let t1 = TupleParam{(1,:a)}(true),
    t2 = TupleParam{(1,:b)}(true)

    # tuple type params can't contain invalid type params
    @test_throws TypeError t3 = TupleParam{(1,"nope")}(true)

    # dispatch works properly
    @test tupledispatch(t1) == true
    @test_throws MethodError tupledispatch(t2)
end

# issue #5254
f5254(::Type{T}, b::T) where {T} = 0
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
cnvt(::Type{Array{S, N}}, x::Array{T, N}) where {S, T, N} = convert(Array{S}, x)

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
primitive type Int5142 64 end
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

primitive type Int5142b 8 end
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
mutable struct FileObj5374
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
mapshape_5584(s1::NTuple{N,Int}, s2::NTuple{N,Int}) where {N} =
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

mutable struct Polygon5884{T<:Real}
    points::Vector{Complex{T}}
end

function test5884()
    star = Vector{Polygon5884}(undef, (3,))
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
@test 3 ==
    include_string(@__MODULE__, "1 + 2") ==
    include_string(@__MODULE__, "1 + #==# 2") ==
    include_string(@__MODULE__, "1 + #===# 2") ==
    include_string(@__MODULE__, "1 + #= #= blah =# =# 2") ==
    include_string(@__MODULE__, "1 + #= #= #= nested =# =# =# 2") ==
    include_string(@__MODULE__, "1 + #= \0 =# 2")
@test_throws LoadError include_string(@__MODULE__, "#=")
@test_throws LoadError include_string(@__MODULE__, "#= #= #= =# =# =")

# issue #6142
import Base: +
import LinearAlgebra: UniformScaling, I
mutable struct A6142 <: AbstractMatrix{Float64}; end
+(x::A6142, y::UniformScaling) = "UniformScaling method called"
+(x::A6142, y::AbstractArray) = "AbstractArray method called"
@test A6142() + I == "UniformScaling method called"
+(x::A6142, y::AbstractRange) = "AbstractRange method called" #16324 ambiguity

# issue #6175
function g6175(); print(""); (); end
g6175(i::Real, I...) = g6175(I...)
g6175(i, I...) = tuple(length(i), g6175(I...)...)
@test g6175(1:5) === (5,)

# issue #6242
f6242(x::NTuple{N,Int}) where {N} = (N==0 ? 1 : ntuple(n->x[n],N))
@test f6242(()) === 1

# issue #6292
let i = 0
    global g6292() = i+=1
end
@test g6292() == 1
@test g6292() == 2

# issue #6404
mutable struct type_2{T <: Integer, N} <: Number
    x::T
    type_2{T,N}(n::T) where {T<:Integer,N} = new(n)
end
mutable struct type_1{T <: Number} <: Number
    x::Vector{T}
    type_1{T}(x::Vector{T}) where T<:Number = new(x)
end
type_1(x::Vector{T}) where {T <: Number} = type_1{T}(x)
type_1(c::T) where {T <: Number} = type_1{T}([c])
Base.convert(::Type{type_1{T}}, x::S) where {T<:Number, S<:Number} = type_1(convert(T, x))
+(a::type_1{T}, b::type_1{T}) where {T <: Number} = a

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
        function $(esc(:f5876))(::Type{T}) where T
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

# issue #20524
macro m20524(ex)
    quote
        global f20524
        function f20524()
            $ex
        end
    end
end
@m20524 ((a,(b20524,c)) = (8,(1,5)); (a,b20524,c))
@test f20524() === (8,1,5)
@test !@isdefined b20524 # should not assign to a global

# issue #6387
primitive type Date6387{C} 64 end

mutable struct DateRange6387{C} <: AbstractRange{Date6387{C}}
end

mutable struct ObjMember
    member::DateRange6387
end

obj6387 = ObjMember(DateRange6387{Int64}())

function v6387(r::AbstractRange{T}) where T
    a = Vector{T}(undef, 1)
    a[1] = Core.Intrinsics.bitcast(Date6387{Int64}, Int64(1))
    return a
end

function day_in(obj::ObjMember)
    x = v6387(obj.member)
    @test isa(x, Vector{Date6387{Int64}})
    @test isa(x[1], Date6387{Int64})
end
day_in(obj6387)

# issue #6784
@test ndims(Array{Array{Float64}}(undef, 3,5)) == 2
@test ndims(Array{Array}(undef, 3,5)) == 2

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
abstract type A6980 end
mutable struct B6980 <: A6980 end
f6980(::Union{Int, Float64}, ::A6980) = false
f6980(::Union{Int, Float64}, ::B6980) = true
@test f6980(1, B6980())

# issue #7049
Maybe7049{T} = Union{T,Nothing}
function ttt7049(;init::Maybe7049{Union{AbstractString,Tuple{Int,Char}}} = nothing)
    string("init=", init)
end
@test ttt7049(init="a") == "init=a"

# issue #7074
let z(A::StridedMatrix{T}) where {T<:Union{Float64,Complex{Float64},Float32,Complex{Float32}}} = T,
    S = zeros(Complex,2,2)
    @test_throws MethodError z(S)
end

# issue #7062
f7062(::Type{Array{t}}  , ::Array{t,n}) where {t,n} = (t,n,1)
f7062(::Type{Array{t,n}}, ::Array{t,n}) where {t,n} = (t,n,2)
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
mutable struct A7652
    a :: Int
end
a7652 = A7652(0)
t_a7652 = A7652
f7652() = fieldtype(t_a7652, :a) <: Int
@test f7652() == (fieldtype(A7652, :a) <: Int) == true
g7652() = fieldtype(DataType, :types)
@test g7652() == fieldtype(DataType, :types) == Core.SimpleVector
@test fieldtype(t_a7652, 1) == Int
h7652() = setfield!(a7652, 1, 2)
h7652()
@test a7652.a == 2
i7652() = Base.setproperty!(a7652, :a, 3.0)
i7652()
@test a7652.a == 3

# issue #7679
@test map(f->f(), Any[ ()->i for i=1:3 ]) == Any[1,2,3]

# issue 7897
function issue7897!(data, arr)
    data = reinterpret(UInt32, data)
    a = arr[1]
end

let
    a = fill(0x01, 10)
    sa = view(a, 4:6)
    # This can throw an error, but shouldn't segfault
    try
        issue7897!(sa, zeros(10))
    end
end

# issue #7582
aₜ = "a variable using Unicode 6"

struct My8156{A, B}
    a::A
    b::B
end
let m = My8156(nothing, 1)
    @test sizeof(m) == sizeof(1)
    @test m.a === nothing
    @test m.b === 1
end

# issue #8184
struct Foo8184
    x::Nothing
    y::Nothing
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
mutable struct Issue2403
    x
end
(i::Issue2403)(y) = i.x + 2y
let x = Issue2403(20)
    @test x(3) == 26
    @test issue2403func(x) == 34
end

# issue #8798
let
    npy_typestrs = Dict("b1"=>Bool,
                        "i1"=>Int8,      "u1"=>UInt8,
                        "i2"=>Int16,     "u2"=>UInt16,
                        "i4"=>Int32,     "u4"=>UInt32,
                        "i8"=>Int64,     "u8"=>UInt64)
    sizeof_lookup() = sizeof(npy_typestrs["i8"])
    @test sizeof_lookup() == 8
end

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
    arr = Vector{Any}(undef, 1)
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

# issue #24460
f24460(x, y) = 1
f24460(x::T, y::T) where {T} = 2.0
f24460(x::Int, y::Int) = "3"
@test f24460(1, 2) === "3"
@test invoke(f24460, Tuple{T,T} where T, 1, 2) === 2.0
const T24460 = Tuple{T,T} where T
g24460() = invoke(f24460, T24460, 1, 2)
@test @inferred(g24460()) === 2.0

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
    @test ccall(:jl_new_bits, Any, (Any,Ptr{Cvoid},), Int, x) === 1
    @test ccall(:jl_new_bits, Any, (Any,Ptr{Cvoid},), Complex{Int}, x) === 1+2im
    @test ccall(:jl_new_bits, Any, (Any,Ptr{Cvoid},), NTuple{3,Int}, x) === (1,2,3)
    @test ccall(:jl_new_bits, Any, (Any,Ptr{Cvoid},), Tuple{Int,Int,Int}, x) === (1,2,3)
    @test (ccall(:jl_new_bits, Any, (Any,Ptr{Cvoid},), Tuple{Int16,Tuple{Cvoid},Int8,Tuple{},Int,Cvoid,Int}, x)::Tuple)[[2,4,5,6,7]] === ((nothing,),(),2,nothing,3)
end

# sig 2 is SIGINT per the POSIX.1-1990 standard
if !Sys.iswindows()
    ccall(:jl_exit_on_sigint, Cvoid, (Cint,), 0)
    @test_throws InterruptException begin
        ccall(:kill, Cvoid, (Cint, Cint,), getpid(), 2)
        for i in 1:10
            Libc.systemsleep(0.1)
            ccall(:jl_gc_safepoint, Cvoid, ()) # wait for SIGINT to arrive
        end
    end
    ccall(:jl_exit_on_sigint, Cvoid, (Cint,), 1)
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
@test_throws BoundsError((1, 2), 3) begin; a, b, c = 1, 2; end
let a = []
    @test_broken try; a[]; catch ex; (ex::BoundsError).a === a && ex.i == (1,); end # TODO: Re-enable after PLI
    @test_throws BoundsError(a, (1, 2)) a[1, 2]
    @test_throws BoundsError(a, (10,)) a[10]
end
f9534a() = (a = 1 + 2im; getfield(a, -100))
f9534a(x) = (a = 1 + 2im; getfield(a, x))
@test_throws BoundsError(1 + 2im, -100) f9534a()
@test_throws BoundsError(1 + 2im, 3) f9534a(3)
f9534b() = (a = (1, 2., ""); a[5])
f9534b(x) = (a = (1, 2., ""); a[x])
@test_throws BoundsError((1, 2., ""), 5) f9534b()
@test_throws BoundsError((1, 2., ""), 4) f9534b(4)
f9534c() = (a = (1, 2.); a[3])
f9534c(x) = (a = (1, 2.); a[x])
@test_throws BoundsError((1, 2.), 3) f9534c()
@test_throws BoundsError((1, 2.), 0) f9534c(0)
f9534d() = (a = (1, 2, 4, 6, 7); a[7])
f9534d(x) = (a = (1, 2, 4, 6, 7); a[x])
@test_throws BoundsError((1, 2, 4, 6, 7), 7) f9534d()
@test_throws BoundsError((1, 2, 4, 6, 7), -1) f9534d(-1)
let a = IOBuffer()
    f9534e(x) = setfield!(a, x, 3)
    @test_throws BoundsError(a, -2) f9534e(-2)
    f9534f() = getfield(a, -2)
    f9534f(x) = getfield(a, x)
    @test_throws BoundsError(a, -2) f9534f()
    @test_throws BoundsError(a, typemin(Int) + 2) f9534f(typemin(Int) + 2)
end
x9634 = 3
@test_throws BoundsError(1 + 2im, 3) getfield(1 + 2im, x9634)
@test try; throw(BoundsError()); catch ex; !isdefined((ex::BoundsError), :a) && !isdefined((ex::BoundsError), :i); end
@test try; throw(BoundsError(Int)); catch ex; (ex::BoundsError).a == Int && !isdefined((ex::BoundsError), :i); end
@test_throws BoundsError(Int, typemin(Int)) throw(BoundsError(Int, typemin(Int)))
@test_throws BoundsError(Int, (:a,)) throw(BoundsError(Int, (:a,)))
f9534g(a, b, c...) = c[0]
@test_throws BoundsError((3, 4, 5, 6), 0) f9534g(1, 2, 3, 4, 5, 6)
f9534h(a, b, c...) = c[a]
@test f9534h(4, 2, 3, 4, 5, 6) == 6
@test_throws BoundsError((3, 4, 5, 6), 5) f9534h(5, 2, 3, 4, 5, 6)

# issue #7978, comment 332352438
f7978a() = 1
@test_throws BoundsError(1, 2) begin; a, b = f7978a(); end
f7978b() = 1, 2
@test_throws BoundsError((1, 2), 3) begin; a, b, c = f7978b(); end

# issue #9535
counter9535 = 0
f9535() = (global counter9535; counter9535 += 1; counter9535)
g9535() = (f9535(),f9535())
@test g9535() == (1,2)
@test g9535() == (3,4)

# weak references
mutable struct Obj; x; end
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
        GC.gc()
        test_wr(ref, wref)
        pop!(ref)
        GC.gc()
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
    using Test
    mutable struct A end; mutable struct B end
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
    g(x::T...) where {T} = T
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
abstract type Foo9378{T,S} end
struct B9378{T} end
FooB9378{T} = Foo9378{T,B9378}
struct CFoo9378 <: FooB9378{Float64} end
@test isa(CFoo9378(),FooB9378)

# issue #10281
const N10281 = 1000
@test if false
    for i in 1:N10281
    end
end === nothing


# issue #10221
module GCbrokentype
using InteractiveUtils
OLD_STDOUT = stdout
fname = tempname()
file = open(fname, "w")
redirect_stdout(file)
versioninfo()
try
    mutable struct Foo{T}
        val::Bar{T}
    end
end
GC.gc()
redirect_stdout(OLD_STDOUT)
close(file)
rm(fname)
end

# issue #10373
f10373(x) = x
g10373(x) = x
mutable struct newtype10373
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
f7221(::T) where {T<:Number} = 1
f7221(::BitArray) = 2
f7221(::AbstractVecOrMat) = 3
@test f7221(trues(1)) == 2

# issue #10570
struct Array_512_Uint8
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
GC.gc()

# issue #10867
@test collect(enumerate((Tuple,Int))) == [(1,Tuple), (2,Int)]
@test collect(enumerate((Tuple,3))) == [(1,Tuple), (2,3)]

# issue #10978
TupleType10978{T<:Tuple} = Type{T}
f10978(T::TupleType10978) = isa(T, TupleType10978)
@test f10978(Tuple{Int})

# issue #10995
#TupleType{T<:Tuple} = Type{T}
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

# issue #11355
function f11355(sig::Type{T}) where T<:Tuple
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
    t = Tuple{Type{Dict{K} where K}}
    @test f11355(t) == 100
end

# issue #8283
function func8283 end
@test isa(func8283,Function)
@test_throws MethodError func8283()

# issue #11243
mutable struct Type11243{A, B}
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

# issue #25724
a25724 = Any[]
for i = 1:3
    needX = false
    try
        X = X
        X[1] = X[1] + 1
    catch err
        needX = true
    end
    if needX
        X = [0]
    end
    push!(a25724, copy(X))
end
@test a25724 == [[0], [0], [0]]

# for loop iterator expression should be evaluated in outer scope
let
    for i in (local a = 1:2)
    end
    @test a == 1:2
end

# `for outer`
let
    function forouter()
        i = 1
        for outer i = 2:3
        end
        return i
    end
    @test forouter() == 3
end

@test_throws ErrorException("syntax: no outer variable declaration exists for \"for outer\"") @eval function f()
    for outer i = 1:2
    end
end

# issue #11295
function f11295(x...)
    call = Expr(x...)
end
@test isa(f11295(:a,:b), Expr)

# issue #11675
struct T11675{T}
    x::T
    T11675{T}() where T = new()
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
abstract type C11597{T<:Union{Nothing, Int}} end
mutable struct D11597{T} <: C11597{T} d::T end
@test_throws TypeError D11597(1.0)
@test_throws TypeError repr(D11597(1.0))

# issue #11772
@test_throws UndefRefError (Vector{Any}(undef, 5)...,)

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
let a = (1:1000...,),
    b = (1:1000...,)
    @test a == b
    @test a === b
    @test (a == b) === true
    @test (a === b) === true
end

# issue 11858
mutable struct Foo11858
    x::Float64
    Foo11858(x::Float64) = new(x)
end

mutable struct Bar11858
    x::Float64
    Bar11858(x::Float64) = new(x)
end

g11858(x::Float64) = x
f11858(a) = for Baz in a
    @eval (f::$Baz)(x) = f(float(x))
end
f11858(Any[Type{Foo11858}, Type{Bar11858}, typeof(g11858)])

@test g11858(1) == 1.0
@test Foo11858(1).x == 1.0
@test Bar11858(1).x == 1.0

# issue 11904
struct Nullable11904{T}
    value::T
    hasvalue::Bool
end
@noinline throw_error() = error()
foo11904(x::Int) = x
@inline function foo11904(x::Nullable11904{S}) where S
    if isbits(S)
        Nullable11904(foo11904(x.value), x.hasvalue)
    else
        throw_error()
    end
end

@test foo11904(Nullable11904(1, true)).hasvalue

# issue 11874
struct Foo11874
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

mutable struct FooNTuple{N}
    z::Tuple{Integer, Vararg{Int, N}}
end
@test_throws ErrorException FooNTuple{-1}
@test_throws ErrorException FooNTuple{typemin(Int)}
@test_throws TypeError FooNTuple{0x01}
@test fieldtype(FooNTuple{0}, 1) == Tuple{Integer}

mutable struct FooTupleT{T}
    z::Tuple{Int, T, Int}
end
@test_throws TypeError FooTupleT{Vararg{Int, 2}}
@test fieldtype(FooTupleT{Int}, 1) == NTuple{3, Int}

@test Tuple{} === NTuple{0, Any}
@test Tuple{Int} === Tuple{Int, Vararg{Integer, 0}}

# issue #12003
using Dates
const DATE12003 = DateTime(1917,1,1)
failure12003(dt=DATE12003) = Dates.year(dt)
@test isa(failure12003(), Integer)

# issue #12023 Test error checking in primitive type
@test_throws ErrorException (@eval primitive type 0 SPJa12023 end)
@test_throws ErrorException (@eval primitive type 4294967312 SPJb12023 end)
@test_throws ErrorException (@eval primitive type -4294967280 SPJc12023 end)

# issue #12089
mutable struct A12089{K, N}
    sz::NTuple{N, Int}
    A12089{K,N}(sz::NTuple{N, Int}) where {K,N} = new(sz)
end
A12089{-1, 1}((1,))

# issue #12092
f12092(x::Int, y) = 0
f12092(x::Int,) = 1
f12092(x::Int, y::Int...) = 2
@test f12092(1) == 1

# issue #12063
# NOTE: should have > MAX_TUPLETYPE_LEN arguments
f12063(tt, g, p, c, b, v, cu::T, d::AbstractArray{T, 2}, ve) where {T} = 1
f12063(args...) = 2
g12063() = f12063(0, 0, 0, 0, 0, 0, 0.0, spzeros(0,0), Int[])
@test g12063() == 1

# issue #11587
mutable struct Sampler11587{N}
    clampedpos::Array{Int,2}
    buf::Array{Float64,N}
end
function Sampler11587()
    a = tuple(Any[32,32]...,)
    Sampler11587(zeros(Int,a), zeros(Float64,a))
end
@test isa(Sampler11587(), Sampler11587{2})

# issue #8010 - error when convert returns wrong type during new()
struct Vec8010{T}
    x::T
    y::T
end
Vec8010(a::AbstractVector) = Vec8010(ntuple(x->a[x],2)...)
Base.convert(::Type{Vec8010{T}},x::AbstractVector) where {T} = Vec8010(x)
Base.convert(::Type{Nothing},x::AbstractVector) = Vec8010(x)
struct MyType8010
     m::Vec8010{Float32}
end
struct MyType8010_ghost
     m::Nothing
end
@test_throws TypeError MyType8010([3.0;4.0])
@test_throws TypeError MyType8010_ghost([3.0;4.0])

module TestNewTypeError
using Test

struct A
end
struct B
    a::A
end
@eval function f1()
    # Emitting this direction is not recommended but it can come from `convert` that does not
    # return the correct type.
    $(Expr(:new, B, 1))
end
@eval function f2()
    a = $(Expr(:new, B, 1))
    a = a
    return nothing
end
@generated function f3()
    quote
        $(Expr(:new, B, 1))
        return nothing
    end
end
@test_throws TypeError f1()
@test_throws TypeError f2()
@test_throws TypeError f3()
@test_throws TypeError eval(Expr(:new, B, 1))

end

# don't allow redefining types if ninitialized changes
struct NInitializedTestType
    a
end

@test_throws ErrorException @eval struct NInitializedTestType
    a
    NInitializedTestType() = new()
end

# issue #12394
mutable struct Empty12394 end
let x = Vector{Empty12394}(undef, 1), y = [Empty12394()]
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

# objectid of haspadding field
struct HasPadding
    x::Bool
    y::Int
end
struct HasHasPadding
    x::HasPadding
end
let hashaspadding = Ref(HasHasPadding(HasPadding(true,1))),
    hashaspadding2 = Ref(HasHasPadding(HasPadding(true,1)))
    unsafe_store!(convert(Ptr{UInt8},pointer_from_objref(hashaspadding)), 0x12, 2)
    unsafe_store!(convert(Ptr{UInt8},pointer_from_objref(hashaspadding2)), 0x21, 2)
    @test objectid(hashaspadding[]) == objectid(hashaspadding2[])
end

# issue #12517
let x = (1,2)
    @eval f12517() = Val{$x}
    @test f12517() === Val{(1,2)}
end

# don't allow Vararg{} in Union{} type constructor
@test_throws TypeError Union{Int,Vararg{Int}}
@test_throws TypeError Union{Vararg{Int}}

# only allow Vararg{} in last position of Tuple{ }
@test_throws TypeError Tuple{Vararg{Int32},Int64,Float64}
@test_throws TypeError Tuple{Int64,Vararg{Int32},Float64}
@test_throws TypeError Array{Vararg}

# don't allow non-types in Union
@test_throws TypeError Union{1}
@test_throws TypeError Union{Int,0}
PossiblyInvalidUnion{T} = Union{T,Int}
@test_throws TypeError PossiblyInvalidUnion{1}

# issue #12569
@test Symbol("x") === Symbol("x")
@test split(string(gensym("abc")),'#')[3] == "abc"

# issue #13007
call13007(::Type{Array{T,N}}) where {T,N} = 0
call13007(::Type{Array}) = 1
@test length(Base._methods(call13007, Tuple{Type{x} where x<:Array}, 4, typemax(UInt))) == 2

# detecting cycles during type intersection, e.g. #1631
cycle_in_solve_tvar_constraints(::Type{Some{S}}, x::S) where {S} = 0
cycle_in_solve_tvar_constraints(::Type{T}, x::Val{T}) where {T} = 1
@test length(methods(cycle_in_solve_tvar_constraints)) == 2

# issue #12967
foo12967(x, @nospecialize y) = 1
TupleType12967{T<:Tuple} = Type{T}
foo12967(x, ::TupleType12967) = 2
@test foo12967(1, Int) == 1
@test foo12967(1, Tuple{}) == 2

# issue #13083
@test Nothing() === nothing

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
struct A11888{T}
    a::NTuple{16,T}
end

B11888{T} = A11888{A11888{A11888{T}}}

@test sizeof(B11888{B11888{Int64}}) == (1 << 24) * 8

# issue #13175
struct EmptyImmutable13175 end
struct EmptyIIOtherField13175
    x::EmptyImmutable13175
    y::Float64
end
@test EmptyIIOtherField13175(EmptyImmutable13175(), 1.0) == EmptyIIOtherField13175(EmptyImmutable13175(), 1.0)
@test EmptyIIOtherField13175(EmptyImmutable13175(), 1.0) != EmptyIIOtherField13175(EmptyImmutable13175(), 2.0)

# issue 8932 (llvm return type legalizer error)
struct Vec3_8932
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
mutable struct IO13433 <: IO end
Base.read(::IO13433, ::Type{UInt8}) = 0x01
@test read!(IO13433(), Array{UInt8}(undef, 4)) == [0x01, 0x01, 0x01, 0x01]

# issue #13647, comparing boxed isbits immutables
struct X13647
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
abstract type A11327 end
abstract type B11327 <: A11327 end
f11327(::Type{T},x::T) where {T} = x
@test_throws MethodError f11327(Type{A11327},B11327)

# issue #8487
@test [x for x in 1:3] == [x for x ∈ 1:3] == [x for x = 1:3]
let A = Matrix{Int}(undef, 4,3)
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
    finalizer((obj) -> (finalized = 1), obj)
    # obj should be marked for promotion after the second gc and be promoted
    # after the third GC
    # GC_CLEAN; age = 0
    GC.gc(false)
    # GC_CLEAN; age = 1
    GC.gc(false)
    # GC_QUEUED; age = 1
    GC.gc(false)
    # GC_MARKED; age = 1
    finalize(obj)
    @test finalized == 1
end

# check if finalizers for the old gen can be triggered manually
# PR #14181
let
    # The following three `GC.gc(false)` clears the `finalizer_list`. It is
    # not strictly necessary to make the test pass but should make the failure
    # more repeatable if something breaks.
    GC.gc(false)
    # At least: GC_CLEAN; age = 1
    GC.gc(false)
    # At least: GC_QUEUED; age = 1
    GC.gc(false)
    # all objects in `finalizer_list` are now moved to `finalizer_list_marked`

    obj1 = Ref(1)
    obj2 = Ref(1)
    finalized = 0
    finalizer((obj) -> (finalized += 1), obj1)
    finalizer((obj) -> (finalized += 1), obj1)
    finalizer((obj) -> (finalized += 1; finalize(obj1)), obj2)
    finalizer((obj) -> (finalized += 1; finalize(obj1)), obj2)
    finalize(obj2)
    @test finalized == 4
end

# issue #14323
@test_throws ErrorException eval(Expr(:body, :(1)))

# issue #14339
f14339(x::T, y::T) where {T<:Union{}} = 0
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
GC.gc()
GC.gc()
GC.gc()

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
struct Z14477
    fld::Z14477
    Z14477() = new(new())
end
let z1 = Z14477()
    @test isa(z1, Z14477)
    @test isa(z1.fld, Z14477)
end

# issue #8846, generic macros
macro m8846(a, b=0)
    a, b
end
@test @m8846(a) === (:a, 0)
@test @m8846(a, 1) === (:a, 1)
let nometh = try; @eval @m8846(a, b, c); false; catch ex; ex; end
    __source__ = LineNumberNode(@__LINE__() -  1, Symbol(@__FILE__))
    nometh::LoadError
    @test nometh.file === string(__source__.file)
    @test nometh.line === __source__.line
    e = nometh.error::MethodError
    @test e.f === getfield(@__MODULE__, Symbol("@m8846"))
    @test e.args === (__source__, @__MODULE__, :a, :b, :c)
 end

# a simple case of parametric dispatch with unions
let foo(x::Union{T, Nothing}, y::Union{T, Nothing}) where {T} = 1
    @test foo(1, nothing) === 1
    @test foo(nothing, nothing) === 1
end
let foo(x::Union{T, Nothing}, y::Union{T, Nothing}) where {T} = T
    @test foo(1, nothing) === Int
    @test_throws UndefVarError(:T) foo(nothing, nothing)
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
@test isa(objectid(Tuple.name.cache), Integer)

# issue #14691
mutable struct T14691; a::UInt; end
@test (T14691(0).a = 0) === 0

# issue #14245
f14245() = (v = []; push!(v, length(v)); v)
@test f14245()[1] == 0

# issue #9677
@generated function foo9677(x::AbstractArray{T,N}) where {T,N}
    quote
        x=$N
        y=x+1
        return y
    end
end
foo9677(x::Array) = invoke(foo9677, Tuple{AbstractArray}, x)
@test foo9677(1:5) == foo9677(randn(3))

# issue #6846
f6846() = (please6846; 2)
@test_throws UndefVarError(:please6846) f6846()

module M6846
    macro f()
        return esc(:(please6846; 2))
    end
end
@test_throws UndefVarError(:please6846) @M6846.f()

# issue #14758
@test isa(@eval(f14758(; $([]...)) = ()), Function)

# issue #14767
@inline f14767(x) = x ? A14767 : ()
const A14767 = f14767(false)
@test A14767 === ()

# issue #10985
f10985(::Any...) = 1
@test f10985(1, 2, 3) == 1

# a tricky case for closure conversion
mutable struct _CaptureInCtor
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
    @test hasmethod(f, (Union{Type{Int},Type{Int8}},))
end

let
    b=()->c
    c=1
    @test b() == 1
end

# issue #14825
abstract type abstest_14825 end

mutable struct t1_14825{A <: abstest_14825, B}
  x::A
  y::B
end

mutable struct t2_14825{C, B} <: abstest_14825
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

# issue #15186
let ex = quote
             $(if true; :(test); end)
         end
    @test ex.args[2] == :test
end

# issue #25652
x25652 = 1
x25652_2 = let (x25652, _) = (x25652, nothing)
    x25652 = x25652 + 1
    x25652
end
@test x25652_2 == 2
@test x25652 == 1

# issue #15180
function f15180(x::T) where T
    X = Vector{T}(undef, 1)
    X[1] = x
    @noinline ef(::J) where {J} = (J,X[1]) # Use T
    ef(::J, ::Int) where {J} = (T,J)
    return ef
end
@test map(f15180(1), [1,2]) == [(Int,1),(Int,1)]

let ary = Vector{Any}(undef, 10)
    check_undef_and_fill(ary, rng) = for i in rng
        @test !isassigned(ary, i)
        ary[i] = (Float64(i), i) # some non-cached content
        @test isassigned(ary, i)
    end
    # Check if the memory is initially zerod and fill it with value
    # to check if these values are not reused later.
    check_undef_and_fill(ary, 1:10)
    # Check if the memory grown at the end are zerod
    ccall(:jl_array_grow_end, Cvoid, (Any, Csize_t), ary, 10)
    check_undef_and_fill(ary, 11:20)
    # Make sure the content of the memory deleted at the end are not reused
    ccall(:jl_array_del_end, Cvoid, (Any, Csize_t), ary, 5)
    ccall(:jl_array_grow_end, Cvoid, (Any, Csize_t), ary, 5)
    check_undef_and_fill(ary, 16:20)

    # Now check grow/del_end
    ary = Vector{Any}(undef, 1010)
    check_undef_and_fill(ary, 1:1010)
    # This del_beg should move the buffer
    ccall(:jl_array_del_beg, Cvoid, (Any, Csize_t), ary, 1000)
    ccall(:jl_array_grow_beg, Cvoid, (Any, Csize_t), ary, 1000)
    check_undef_and_fill(ary, 1:1000)
    ary = Vector{Any}(undef, 1010)
    check_undef_and_fill(ary, 1:1010)
    # This del_beg should not move the buffer
    ccall(:jl_array_del_beg, Cvoid, (Any, Csize_t), ary, 10)
    ccall(:jl_array_grow_beg, Cvoid, (Any, Csize_t), ary, 10)
    check_undef_and_fill(ary, 1:10)

    ary = Vector{Any}(undef, 1010)
    check_undef_and_fill(ary, 1:1010)
    ccall(:jl_array_grow_end, Cvoid, (Any, Csize_t), ary, 10)
    check_undef_and_fill(ary, 1011:1020)
    ccall(:jl_array_del_end, Cvoid, (Any, Csize_t), ary, 10)
    ccall(:jl_array_grow_beg, Cvoid, (Any, Csize_t), ary, 10)
    check_undef_and_fill(ary, 1:10)

    # Make sure newly malloc'd buffers are filled with 0
    # test this for a few different sizes since we need to make sure
    # we are malloc'ing the buffer after the grow_end and malloc is not using
    # mmap directly (which may return a zero'd new page).
    for n in [50, 51, 100, 101, 200, 201, 300, 301]
        ary = Vector{Any}(undef, n)
        # Try to free the previous buffer that was filled with random content
        # and to increase the chance of getting a non-zero'd buffer next time
        GC.gc()
        GC.gc()
        GC.gc()
        ccall(:jl_array_grow_beg, Cvoid, (Any, Csize_t), ary, 4)
        ccall(:jl_array_del_beg, Cvoid, (Any, Csize_t), ary, 4)
        ccall(:jl_array_grow_end, Cvoid, (Any, Csize_t), ary, n)
        ccall(:jl_array_grow_beg, Cvoid, (Any, Csize_t), ary, 4)
        check_undef_and_fill(ary, 1:(2n + 4))
    end

    ary = Vector{Any}(undef, 100)
    ccall(:jl_array_grow_end, Cvoid, (Any, Csize_t), ary, 10000)
    ary[:] = 1:length(ary)
    ccall(:jl_array_del_beg, Cvoid, (Any, Csize_t), ary, 10000)
    # grow on the back until a buffer reallocation happens
    cur_ptr = pointer(ary)
    while cur_ptr == pointer(ary)
        len = length(ary)
        ccall(:jl_array_grow_end, Cvoid, (Any, Csize_t), ary, 10)
        for i in (len + 1):(len + 10)
            @test !isassigned(ary, i)
        end
    end

    ary = Vector{Any}(undef, 100)
    ary[:] = 1:length(ary)
    ccall(:jl_array_grow_at, Cvoid, (Any, Csize_t, Csize_t), ary, 50, 10)
    for i in 51:60
        @test !isassigned(ary, i)
    end
end

# check if we can run multiple finalizers at the same time
# Use a `@noinline` function to make sure the inefficient gc root generation
# doesn't keep the object alive.
@noinline function create_dead_object13995(finalized)
    obj = Ref(1)
    finalizer((x)->(finalized[1] = true), obj)
    finalizer((x)->(finalized[2] = true), obj)
    finalizer((x)->(finalized[3] = true), obj)
    finalizer((x)->(finalized[4] = true), obj)
    nothing
end
# disable GC to make sure no collection/promotion happens
# when we are constructing the objects
let gc_enabled13995 = GC.enable(false)
    finalized13995 = [false, false, false, false]
    create_dead_object13995(finalized13995)
    GC.enable(true)
    # obj is unreachable and young, a single young gc should collect it
    # and trigger all the finalizers.
    GC.gc(false)
    GC.enable(false)
    @test finalized13995 == [true, true, true, true]
    GC.enable(gc_enabled13995)
end

# issue #15283
j15283 = 0
let
    global j15283
    k15283 = (j15283 += 1)
end
@test j15283 == 1
@test !@isdefined k15283

# issue #15264
module Test15264
    mod1(x::T) where {T} = x < 1 ? x : mod1(x-1)
end
@test Test15264.mod1 !== Base.mod1

module M15455
function rpm_provides(r::T) where T
    push!([], partialsort(r,T))
end
partialsort(a,b) = 0
end
@test M15455.partialsort(1,2)==0

# check that medium-sized array is 64-byte aligned (#15139)
@test Int(pointer(Vector{Float64}(undef, 1024))) % 64 == 0

# PR #15413
# Make sure arrayset can handle `Array{T}` (where `T` is a type and not a
# `TypeVar`) without crashing
let
    function arrayset_unknown_dim(::Type{T}, n) where T
        Base.arrayset(true, reshape(Vector{T}(undef, 1), fill(1, n)...), 2, 1)
    end
    arrayset_unknown_dim(Any, 1)
    arrayset_unknown_dim(Any, 2)
    arrayset_unknown_dim(Any, 3)
    arrayset_unknown_dim(Int, 1)
    arrayset_unknown_dim(Int, 2)
    arrayset_unknown_dim(Int, 3)
end

module TestSharedArrayResize
using Test
# Attempting to change the shape of a shared array should unshare it and
# not modify the original data
function test_shared_array_resize(::Type{T}) where T
    len = 100
    a = Vector{T}(undef, len)
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

    test_unshare(a->ccall(:jl_array_del_end, Cvoid, (Any, Csize_t), a, 0))
    test_unshare(a->ccall(:jl_array_del_end, Cvoid, (Any, Csize_t), a, 1))
    test_unshare(a->ccall(:jl_array_del_beg, Cvoid, (Any, Csize_t), a, 0))
    test_unshare(a->ccall(:jl_array_del_beg, Cvoid, (Any, Csize_t), a, 1))
    test_unshare(a->deleteat!(a, 10))
    test_unshare(a->deleteat!(a, 90))
    test_unshare(a->ccall(:jl_array_grow_end, Cvoid, (Any, Csize_t), a, 0))
    test_unshare(a->ccall(:jl_array_grow_end, Cvoid, (Any, Csize_t), a, 1))
    test_unshare(a->ccall(:jl_array_grow_beg, Cvoid, (Any, Csize_t), a, 0))
    test_unshare(a->ccall(:jl_array_grow_beg, Cvoid, (Any, Csize_t), a, 1))
    test_unshare(a->insert!(a, 10, 10))
    test_unshare(a->insert!(a, 90, 90))
end
test_shared_array_resize(Int)
test_shared_array_resize(Any)
end

module TestArrayNUL
using Test
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
@test check_nul(unsafe_wrap(Vector{UInt8},b))
@test check_nul(c)
d = [0x2, 0x1, 0x3]
@test check_nul(d)
push!(d, 0x3)
@test check_nul(d)
push!(d, 0x3)
@test check_nul(d)
ccall(:jl_array_del_end, Cvoid, (Any, UInt), d, 2)
@test check_nul(d)
ccall(:jl_array_grow_end, Cvoid, (Any, UInt), d, 1)
@test check_nul(d)
ccall(:jl_array_grow_end, Cvoid, (Any, UInt), d, 1)
@test check_nul(d)
ccall(:jl_array_grow_end, Cvoid, (Any, UInt), d, 10)
@test check_nul(d)
ccall(:jl_array_del_beg, Cvoid, (Any, UInt), d, 8)
@test check_nul(d)
ccall(:jl_array_grow_beg, Cvoid, (Any, UInt), d, 8)
@test check_nul(d)
ccall(:jl_array_grow_beg, Cvoid, (Any, UInt), d, 8)
@test check_nul(d)
f = unsafe_wrap(Array, pointer(d), length(d))
@test !check_nul(f)
f = unsafe_wrap(Array, ccall(:malloc, Ptr{UInt8}, (Csize_t,), 10), 10, own = true)
@test !check_nul(f)
end

# Copy of `#undef`
copyto!(Vector{Any}(undef, 10), Vector{Any}(undef, 10))
function test_copy_alias(::Type{T}) where T
    ary = T[1:100;]
    unsafe_copyto!(ary, 1, ary, 11, 90)
    @test ary == [11:100; 91:100]
    ary = T[1:100;]
    unsafe_copyto!(ary, 11, ary, 1, 90)
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
    using Test, Random
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

# issue #15809
# but note, direct global method defs inside functions have since been disallowed
function f15809()
    @eval g15809(x::T) where {T} = T
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
using Test

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

function metadata_matches(ast::Core.CodeInfo)
    inbounds_cnt = Ref(0)
    for ex in ast.code::Array{Any,1}
        if isa(ex, Expr)
            ex = ex::Expr
            count_expr_push(ex, :inbounds, inbounds_cnt)
        end
    end
    @test inbounds_cnt[] == 0
end

function test_metadata_matches(@nospecialize(f), @nospecialize(tt))
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
@noinline function g(a)
end
@eval function f3()
    g($(Expr(:inbounds, true)))
    @goto out
    g($(Expr(:inbounds, :pop)))
    @label out
end

test_metadata_matches(f1, Tuple{})
test_metadata_matches(f2, Tuple{})
test_metadata_matches(f3, Tuple{})

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
mutable struct Issue8712; end
@test isa(invoke(Issue8712, Tuple{}), Issue8712)

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
# the `code_llvm(devnull, ...)` tests are only meaningful on debug build
# with verifier on (but should still pass on release build).
module TestSSA16244

using Test, InteractiveUtils
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
code_llvm(devnull, f1, Tuple{Bool})
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
code_llvm(devnull, f2, Tuple{Bool})
@test f2(true) == 2
@test_throws ErrorException f2(false)

# SA but not SSA
function f3(a)
    if a
        b = (k(a) + 1, 3)
    end
    b[1]
end
code_llvm(devnull, f3, Tuple{Bool})
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
code_llvm(devnull, f4, Tuple{Bool,Ptr{Cvoid}})
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
code_llvm(devnull, f5, Tuple{Bool})
@test f5(true) == 2
@test f5(false) == 1

# SSA due to const prop of condition
function f6(a)
    if 1 === 1
        b = (k(a) + 1, 3)
    end
    b[1]
end
code_llvm(devnull, f6, Tuple{Bool})
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
code_llvm(devnull, f7, Tuple{Bool})
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
code_llvm(devnull, f8, Tuple{Bool,Int})
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
code_llvm(devnull, f9, Tuple{Bool})
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
f16153(@nospecialize(x), y...) = 2
@test f16153("") == 1
ff16153(@nospecialize(x), y...) = 2
ff16153(x) = 1
@test ff16153("") == 1
g16153(@nospecialize(x), y...) = 1
g16153(@nospecialize(x), @nospecialize(y)) = 2
@test g16153(1, 1) == 2
gg16153(@nospecialize(x), @nospecialize(y)) = 2
gg16153(@nospecialize(x), y...) = 1
@test gg16153(1, 1) == 2

# don't remove global variable accesses even if we "know" their type
# see #16090
f16090() = typeof(undefined_x16090::Tuple{Type{Int}})
@test_throws UndefVarError f16090()
undefined_x16090 = (Int,)
@test_throws TypeError f16090()

# issue #12238
struct A12238{T} end
mutable struct B12238{T,S}
    a::A12238{B12238{Int,S}}
end
@test B12238.body.body.types[1] === A12238{B12238{Int}.body}
@test isa(A12238{B12238{Int}}.instance, A12238{B12238{Int}})
@test !isdefined(B12238.body.body.types[1], :instance)  # has free type vars

# issue #16315
let a = Any[]
    @noinline f() = a[end]
    @test (push!(a,10); f()) - (push!(a,2); f()) == 8
    @test a == [10, 2]
end

# issue #12096
let a = Val{Val{TypeVar(:_, Int)}},
    b = Val{Val{x} where x<:Int}

    @test !isdefined(a, :instance)
    @test  isdefined(b, :instance)
    @test Base.isconcretetype(b)
end

# A return type widened to Type{Union{T,Nothing}} should not confuse
# codegen
@noinline MaybeFunc(T) = Union{T, Nothing}
fMaybeFunc() = MaybeFunc(Int64)
@test fMaybeFunc() == Union{Int64, Nothing}

# issue #16431
function f16431(x)
    z::Int = x * 2
    g(y) = begin z = z + y; y + x end
    z * g(x)
end
@test @inferred(f16431(1)) == 4

# issue #14878
mutable struct A14878
    ext
end
A14878() = A14878(Dict())
mutable struct B14878
end
B14878(ng) = B14878()
function trigger14878()
    w = A14878()
    w.ext[:14878] = B14878(junk)  # global junk not defined!
    return w
end
@test_throws UndefVarError(:junk) trigger14878()

# issue #1090
function f1090(x)::Int
    if x == 1
        return 1
    end
    2.0
end
@test f1090(1) === 1
@test f1090(2) === 2
(g1090(x::T)::T) where {T} = x+1.0
@test g1090(1) === 2
@test g1090(Float32(3)) === Float32(4)

# error during conversion to return type
function f1090_err()::Int
    try
        return ""
    catch
        8
    end
end
@test_throws MethodError f1090_err()

function f17613_2(x)::Float64
    try
        return x
    catch
        return x+1
    end
end
@test isa(f17613_2(1), Float64)

# return type decl with `where`
function where1090(x::Array{T})::T where T<:Real
    return x[1] + 2.0
end
@test where1090([4]) === 6
@test_throws MethodError where1090(String[])

mutable struct A1090 end
Base.convert(::Type{Int}, ::A1090) = "hey"
f1090()::Int = A1090()
@test_throws TypeError f1090()

# issue #19106
function f19106()::Nothing end
@test f19106() === nothing

# issue #16783
function f16783()
    T = UInt32
    x::T = 0
    bar() = x+1
end
@test f16783()() == 1

# issue #16767
mutable struct A16767{T}
    a::Base.RefValue{T}
end
mutable struct B16767{T}
    b::A16767{B16767{T}}
end
mutable struct C16767{T}
    b::A16767{C16767{:a}}
end
@test B16767.body.types[1].types[1].parameters[1].types[1] === A16767{B16767.body}
@test C16767.body.types[1].types[1].parameters[1].types[1] === A16767{C16767{:a}}

# issue #16340
function f16340(x::T) where T
    function g(y::T) where T
        return (T,T)
    end
    return g
end
let g = f16340(1)
    @test isa(typeof(g).name.mt.defs.sig, UnionAll)
end

# issue #16793
try
    abstract type T16793 end
catch
end
@test isa(T16793, Type)
@test isa(abstract type T16793_2 end, Nothing)

# issue #17147
f17147(::Tuple) = 1
f17147(::Vararg{Tuple,N}) where {N} = 2
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
function captsp(x::T, y::S) where {T, S}
    subf(x2::Int) = T
    subf(x2::UInt) = S
    return subf(Int(1)), subf(UInt(1))
end
@test captsp(1, 2.0) == (Int, Float64)

# issue #15068
function sp_innersig(x::T) where {T}
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
cfunction(f18054, Cint, Tuple{})

# issue #18986: the ccall optimization of cfunction leaves JL_TRY stack in bad state
dummy18996() = return nothing
function main18986()
    cfunction(dummy18986, Cvoid, ())
    ccall((:dummy2, "this_is_a_nonexisting_library"), Cvoid, ())
end
@test_throws ErrorException main18986()

# issue #18085
f18085(a, x...) = (0, )
for (f, g) in ((:asin, :sin), (:acos, :cos))
    gx = eval(g)
    global f18085(::Type{Val{f}}, x...) = map(x -> 2gx(x), f18085(Val{g}, x...))
end
@test f18085(Val{:asin}, 3) === (0.0,)

# issue #18236 constant VecElement in ast triggers codegen assertion/undef
# VecElement of scalar
v18236 = VecElement(1.0)
ptr18236 = cfunction(identity, VecElement{Float64}, Tuple{VecElement{Float64}})
@eval @noinline f18236(ptr) = ccall(ptr, VecElement{Float64},
                                    (VecElement{Float64},), $v18236)
@test f18236(ptr18236) === v18236
@test !occursin("double undef", sprint(code_llvm, f18236, Tuple{Ptr{Cvoid}}))
# VecElement of struct, not necessarily useful but does have special
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

let x = 1
    global g18444
    @noinline g18444(a) = (x += 1; a[])
    f18444_1(a) = invoke(sin, Tuple{Int}, g18444(a))
    f18444_2(a) = invoke(sin, Tuple{Integer}, g18444(a))
    @test_throws ErrorException("invoke: argument type error") f18444_1(Ref{Any}(1.0))
    @test x == 2
    @test_throws ErrorException("invoke: argument type error") f18444_2(Ref{Any}(1.0))
    @test x == 3
    @test f18444_1(Ref{Any}(1)) === sin(1)
    @test x == 4
    @test f18444_2(Ref{Any}(1)) === sin(1)
    @test x == 5
end

f18095(::Int, ::Number) = 0x21
f18095(::Number, ::Int) = 0x12
@test_throws MethodError f18095(1, 2)
@test_throws MethodError invoke(f18095, Tuple{Int, Int}, 1, 2)
@test_throws MethodError invoke(f18095, Tuple{Int, Any}, 1, 2)
@test invoke(f18095, Tuple{Int, Real}, 1, 2) === 0x21

# `invoke` with non-constant function
struct CassetteLikeWrapper{F}
    x
    f::F
end
(foo::CassetteLikeWrapper)(args...) = foo.f(args...)
(foo::CassetteLikeWrapper)(x) = invoke(foo, Tuple{Vararg{Any}}, x)
@test CassetteLikeWrapper(1,-)(2) == -2

f26301(x) = 1
f26301(x::Int) = 2
function g26301()
    f = Any[f26301][1]
    invoke(f, Tuple{Any}, 0)
end
@test g26301() == 1

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
    GC.enable(false)
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
GC.enable(true)

# issue #18710
bad_tvars() where {T} = 1
@test isa(which(bad_tvars, ()), Method)
@test bad_tvars() === 1
bad_tvars2() where {T} = T
@test_throws UndefVarError(:T) bad_tvars2()
missing_tvar(::T...) where {T} = T
@test_throws UndefVarError(:T) missing_tvar()
@test missing_tvar(1) === Int
@test missing_tvar(1, 2, 3) === Int
@test_throws MethodError missing_tvar(1, 2, "3")

# issue #19059 - test for lowering of `let` with assignment not adding Box in simple cases
contains_Box(e::GlobalRef) = (e.name === :Box)
contains_Box(@nospecialize(e)) = false
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
@test any(contains_Box, code_lowered(let_Box1,())[1].code)
@test any(contains_Box, code_lowered(let_Box2,())[1].code)
@test any(contains_Box, code_lowered(let_Box3,())[1].code)
@test any(contains_Box, code_lowered(let_Box4,())[1].code)
@test any(contains_Box, code_lowered(let_Box5,())[1].code)
@test !any(contains_Box, code_lowered(let_noBox,())[1].code)
@test let_Box1()() == 22
@test let_Box2()() == 23
@test let_Box3()() == 24
@test let_Box4()() == 44
@test let_Box5()() == 46
@test let_noBox()() == 21

module TestModuleAssignment
using Test
@eval $(GlobalRef(TestModuleAssignment, :x)) = 1
@test x == 1
@eval $(GlobalRef(TestModuleAssignment, :x)) = 2
@test x == 2
end

# issue #14893
module M14893
x = 14893
macro m14893()
    :x
end
function f14893()
    x = 1
    @m14893
end
end
function f14893()
    x = 2
    M14893.@m14893
end

@test f14893() == 14893
@test M14893.f14893() == 14893

# issue #19599
f19599(x::((S)->Vector{S})(T)...) where {T} = 1
@test f19599([1],[1]) == 1
@test_throws MethodError f19599([1],[1.0])

# avoiding StackOverflowErrors (issues #12007, #10326, #15736)
module SOE
mutable struct Sgnd <: Signed
    v::Int
end
using Test
using Dates
@test_throws ErrorException abs(Sgnd(1))       #12007
io = IOBuffer()
@test_throws ErrorException show(io, Sgnd(1))  #12007

struct MyTime <: Dates.TimeType
    value::Int
end
@test_throws ErrorException isless(MyTime(1), now())

end # module SOE

# issue #15240
@test_nowarn begin
    local p15240
    p15240 = ccall(:jl_realloc, Ptr{Cvoid}, (Ptr{Cvoid}, Csize_t), C_NULL, 10)
    ccall(:jl_free, Cvoid, (Ptr{Cvoid},), p15240)
end

# issue #19963
@test_nowarn ccall(:jl_free, Cvoid, (Ptr{Cvoid},), C_NULL)

# Wrong string size on 64bits for large string.
if Sys.WORD_SIZE == 64
    @noinline function test_large_string20360(slot)
        try
            # Do no touch the string to avoid triggering OOM
            slot[] = Base._string_n(2^32)
            GC.gc(false)
        catch ex
            # This can happen if there's a virtual address size limit
            @test isa(ex, OutOfMemoryError)
            @test_broken false
        end
        return
    end
    @noinline function tester20360()
        GC.gc()
        # Makes sure the string is rooted during the `GC.gc(false)`
        # but is not before the last gc in this function.
        slot = Ref{Any}()
        test_large_string20360(slot)
        slot[] = nothing
        GC.gc()
        return
    end
    @test_nowarn tester20360()
end

@test_throws ArgumentError eltype(Bottom)

# issue #16424, re-evaluating type definitions
struct A16424
    x
    y
end

struct A16424  # allowed
    x
    y
end

@test_throws ErrorException @eval struct A16424
    x
    z
end

@test_throws ErrorException @eval struct A16424
    x
    y::Real
end

struct B16424{T}
    a
end

struct B16424{T}
    a
end

@test_throws ErrorException @eval struct B16424{S}
    a
end

struct C16424{T,S}
    x::T
    y::S
end

struct C16424{T,S}
    x::T
    y::S
end

@test_throws ErrorException @eval struct C16424{T,S}
    x::S
    y::T
end

struct D16424{T<:Real,S<:T}
    x::Vector{S}
    y::Vector{T}
end

struct D16424{T<:Real,S<:T}
    x::Vector{S}
    y::Vector{T}
end

@test_throws ErrorException struct D16424{T<:Real,S<:Real}
    x::Vector{S}
    y::Vector{T}
end

# issue #20999, allow more type redefinitions
struct T20999
    x::Array{T} where T<:Real
end

struct T20999
    x::Array{T} where T<:Real
end

@test_throws ErrorException struct T20999
    x::Array{T} where T<:Integer
end

let a = Vector{Core.TypeofBottom}(undef, 2)
    @test a[1] == Union{}
    @test a == [Union{}, Union{}]
end

@test_throws TypeError(:T17951, "type definition", Type, Vararg) @eval begin
    struct T17951
        x::Vararg
    end
end

# issue #21178
struct F21178{A,B} end
b21178(::F1,::F2) where {B1,B2,F1<:F21178{B1,<:Any},F2<:F21178{B2}} = F1,F2,B1,B2
@test b21178(F21178{1,2}(),F21178{1,2}()) == (F21178{1,2}, F21178{1,2}, 1, 1)

# issue #21172
a21172 = f21172(x) = 2x
@test f21172(8) == 16
@test a21172 === f21172

# issue #21271
f21271() = convert(Tuple{Type{Int}, Type{Float64}}, (Int, Float64))::Tuple{Type{Int}, Type{Float64}}
f21271(x) = x::Tuple{Type{Int}, Type{Float64}}
@test_throws TypeError f21271()
@test_throws TypeError f21271((Int, Float64))

# issue #21397
bar21397(x::T) where {T} = T
foo21397(x) = bar21397(x)
@test foo21397(Tuple) == DataType

# issue 21216
primitive type FP128test <: AbstractFloat 128 end
struct FP128align <: AbstractFloat
    i::Int # cause forced misalignment
    fp::FP128test
end
let ni128 = sizeof(FP128test) ÷ sizeof(Int),
    ns128 = sizeof(FP128align) ÷ sizeof(Int),
    nbit = sizeof(Int) * 8,
    arr = Vector{FP128align}(undef, 2),
    offset = Base.datatype_alignment(FP128test) ÷ sizeof(Int),
    little,
    expected,
    arrint = reinterpret(Int, arr)

    @test length(arrint) == 2 * ns128
    arrint .= 1:(2 * ns128)
    @test sizeof(FP128test) == 16
    @test arr[1].i == 1
    @test arr[2].i == 1 + ns128
    expected = UInt128(0)
    for little in ni128:-1:1
        little += offset
        expected = (expected << nbit) + little
    end
    @test arr[1].fp == reinterpret(FP128test, expected)
    expected = UInt128(0)
    for little in ni128:-1:1
        little += offset + ns128
        expected = (expected << nbit) + little
    end
    @test reinterpret(UInt128, arr[2].fp) == expected
end

# issue #21516
struct T21516
    x::Vector{Float64}
    y::Vector{Float64}
    # check that this definition works
    T21516(x::Vector{T}, y::Vector{T}) where {T<:Real} = new(float.(x), float.(y))
end
@test isa(T21516([1],[2]).x, Vector{Float64})

# let with type declaration
let letvar::Int = 2
    letvar = 3.0
    @test letvar === 3
end

# issue #21568
f21568() = 0
function foo21568()
    y = 1
    @eval f21568(x::AbstractArray{T,1}) where {T<:Real} = $y
end
foo21568()
@test f21568([0]) == 1

# issue #21719
mutable struct T21719{V}
    f
    tol::Float64
    goal::V
end
g21719(f, goal; tol = 1e-6) = T21719(f, tol, goal)
@test isa(g21719(identity, 1.0; tol=0.1), T21719)

# Alignment of perm boxes
for i in 1:10
    # Int64 box should be 16bytes aligned even on 32bits
    ptr1 = ccall(:jl_box_int64, UInt, (Int64,), i)
    ptr2 = ccall(:jl_box_int64, UInt, (Int64,), i)
    @test ptr1 === ptr2
    @test ptr1 % 16 == 0
end

# issue #21581
global function f21581()::Int
    return 2.0
end
@test f21581() === 2
global g21581()::Int = 2.0
@test g21581() === 2
module M21581
macro bar()
    :(foo21581(x)::Int = x)
end
M21581.@bar
end
@test M21581.foo21581(1) === 1

module N21581
macro foo(var)
    quote
        function f(x::T = 1) where T
            ($(esc(var)), x)
        end
        f()
    end
end
end
let x = 8
    @test @N21581.foo(x) === (8, 1)
end

# issue #22122
let
    global @inline function f22122(x::T) where {T}
        T
    end
end
@test f22122(1) === Int

# issue #22256
mutable struct Bar22256{AParameter}
    inner::Int
end
mutable struct Foo22256
    bar::Bar22256
end
setbar22256_inner(a) = (a.bar.inner = 3; nothing)
let a_foo = Foo22256(Bar22256{true}(2))
    @test a_foo.bar.inner == 2
    setbar22256_inner(a_foo)
    @test a_foo.bar.inner == 3
end

# macro hygiene scope (#22307, #23239)
macro a22307()
    return esc(:a22307)
end
macro b22307()
    return :(@a22307)
end
function c22307()
    a22307 = 1
    return @b22307
end
a22307 = 2
@test c22307() == 2

macro identity23239b(x)
    return esc(x)
end
macro identity23239c(x)
    return quote
        $(esc(x))
    end
end
macro assign23239d(x, v)
    return esc(:($x = $v))
end
macro assign23239e(x, v)
    return quote
        $(esc(:($x = $v)))
    end
end
macro aa23239()
    return quote
        a = 1
        @identity23239b b = 2
        @identity23239c c = 3
        @assign23239d d 4
        @assign23239e e 5
        (a, b, c, d, e)
    end
end
f23239() = @aa23239()
@test @inferred(f23239()) === (1, 2, 3, 4, 5)


# issue #22026
module M22026

macro foo(TYP)
    quote
        global foofunction
        foofunction(x::Type{T}) where {T<:Number} = x
    end
end
struct Foo end
@foo Foo

macro foo2()
    quote
        global foofunction2
        (foofunction2(x::T)::Float32) where {T<:Number} = 2x
    end
end

@foo2

end
@test M22026.foofunction(Int16) === Int16
@test M22026.foofunction2(3) === 6.0f0

# tests for isdefined behavior and code generation
global undefined_variable
@test @isdefined Test
@test !@isdefined undefined_variable
@test !@isdefined undefined_variable2
@test let local_undef, local_def = 1
    !@isdefined local_undef
    @isdefined local_def
end
f_isdefined_latedef() = @isdefined f_isdefined_def
@test !f_isdefined_latedef()
f_isdefined(x) = @isdefined x
f_isdefined_undef() = @isdefined x_isundef
f_isdefined_def() = @isdefined f_isdefined_def
@test f_isdefined(1)
@test f_isdefined("")
@test !f_isdefined_undef()
@test f_isdefined_def()
@test f_isdefined_latedef()
f_isdefined_defvarI() = (x = rand(Int); @isdefined x)
f_isdefined_defvarS() = (x = randstring(1); @isdefined x)
@test f_isdefined_defvarI()
@test f_isdefined_defvarS()
f_isdefined_undefvar() = (local x; @isdefined x)
@test !f_isdefined_undefvar()
f_isdefined_unionvar(y, t) = (t > 0 && (x = (t == 1 ? 1 : y)); @isdefined x)
@test f_isdefined_unionvar(nothing, 1)
@test f_isdefined_unionvar("", 1)
@test f_isdefined_unionvar(1.0, 1)
@test f_isdefined_unionvar(1, 1)
@test !f_isdefined_unionvar(nothing, 0)
@test !f_isdefined_unionvar("", 0)
@test !f_isdefined_unionvar(1.0, 0)
@test !f_isdefined_unionvar(1, 0)
f_isdefined_splat(x...) = @isdefined x
@test f_isdefined_splat(1, 2, 3)
let err = try; @macroexpand @isdefined :x; false; catch ex; ex; end,
    __source__ = LineNumberNode(@__LINE__() - 1, Symbol(@__FILE__))
    @test err.file === string(__source__.file)
    @test err.line === __source__.line
    e = err.error::MethodError
    @test e.f === getfield(@__MODULE__, Symbol("@isdefined"))
    @test e.args === (__source__, @__MODULE__, :(:x))
end
f_isdefined_cl_1(y) = (local x; for i = 1:y; x = 2; end; () -> x; @isdefined x)
f_isdefined_cl_2(y) = (local x; for i = 1:y; x = 2; end; () -> @isdefined x)
f_isdefined_cl_3() = (x = 2; () -> x; @isdefined x)
f_isdefined_cl_4() = (local x; () -> x; @isdefined x)
f_isdefined_cl_5() = (x = 2; () -> @isdefined x)
f_isdefined_cl_6() = (local x; () -> @isdefined x)
@test f_isdefined_cl_1(1)
@test !f_isdefined_cl_1(0)
@test f_isdefined_cl_2(1)()
@test !f_isdefined_cl_2(0)()
@test f_isdefined_cl_3()
@test !f_isdefined_cl_4()
@test f_isdefined_cl_5()()
@test !f_isdefined_cl_6()()
f_isdefined_tv(::T) where {T} = @isdefined T
@test f_isdefined_tv(1)
f_isdefined_va(::T...) where {T} = @isdefined T
@test !f_isdefined_va()
@test f_isdefined_va(1, 2, 3)

# note: the constant `5` here should be > DataType.ninitialized.
# This tests that there's no crash due to accessing Type.body.layout.
let f(n) = isdefined(typeof(n), 5)
    @test f(0) === false
    @test isdefined(Int, 5) === false
end

# @isdefined in a loop
let a = []
    for i = 1:2
        push!(a, @isdefined(j))
        local j = 1
    end
    @test a == [false, false]
end

# while loop scope
let a = [], i = 0
    while i < (local b = 2)
        push!(a, @isdefined(j))
        local j = 1
        i += 1
    end
    @test a == [false, false]
    @test b == 2
end

mutable struct MyStruct22929
    x::MyStruct22929
    MyStruct22929() = new()
end
isdefined_22929_1(x) = isdefined(x, 1)
isdefined_22929_x(x) = isdefined(x, :x)
m22929_1 = MyStruct22929()
m22929_2 = MyStruct22929()
m22929_2.x = m22929_1
@test !isdefined_22929_1(m22929_1)
@test !isdefined_22929_x(m22929_1)
@test isdefined_22929_1(m22929_2)
@test isdefined_22929_x(m22929_2)

# Union type sorting
for T in (
        (Nothing, Int8),
        (Nothing, Int64),
        (Nothing, Tuple{Int64, String}),
        (Nothing, Array),
        (Float64, Int64),
        (Float64, String),
        (Float64, Array),
        (String, Array),
        (Int64, Tuple{Int64, Float64}),
        (Tuple{Int64, Float64}, Array)
    )
    @test Base.uniontypes(Union{T...}) == collect(T)
    @test Base.uniontypes(Union{reverse(T)...}) == collect(T)
end
@test Base.uniontypes(Union{Nothing, Union{Int64, Float64}}) == Any[Nothing, Float64, Int64]
module AlternativeIntModule
    struct Int64
        val::UInt64
    end
end
@test Base.uniontypes(Union{Int64, AlternativeIntModule.Int64}) == Any[AlternativeIntModule.Int64, Int64]
@test Base.uniontypes(Union{AlternativeIntModule.Int64, Int64}) == Any[AlternativeIntModule.Int64, Int64]
# because DAlternativeIntModule is alphabetically after Core.Int64
module DAlternativeIntModule
    struct Int64
        val::UInt64
    end
end
@test Base.uniontypes(Union{Int64, DAlternativeIntModule.Int64}) == Any[Int64, DAlternativeIntModule.Int64]
@test Base.uniontypes(Union{DAlternativeIntModule.Int64, Int64}) == Any[Int64, DAlternativeIntModule.Int64]
@test Base.uniontypes(Union{Vector{Int8}, Vector{Int16}}) == Base.uniontypes(Union{Vector{Int16}, Vector{Int8}})
mutable struct ANonIsBitsType
    v::Int64
end
@test Base.uniontypes(Union{Int64, ANonIsBitsType}) == Base.uniontypes(Union{ANonIsBitsType, Int64})

# issue 18933
module GlobalDef18933
    using Test
    import Base.sqrt
    # test that global declaration vs assignment operates correctly in local scope
    f() = (global sin; nothing)
    g() = (global cos; cos = 2; nothing)
    h() = (global sqrt; nothing)
    @test !@isdefined sin
    @test !@isdefined cos
    @test @isdefined sqrt
    f()
    g()
    h()
    @test !@isdefined sin
    @test @isdefined cos
    @test sqrt === Base.sqrt
    @test cos === 2
    # test that function definitions declared global
    # introduce a new, local global
    let
        global tan
        @test !@isdefined tan
        tan() = nothing
        @test @isdefined tan
        @test tan() === nothing
    end
    # test that global declaration side-effects don't ignore conditionals
    if false
        global sincos
        nothing
    end
    @test which(Main, :sincos) === Base.Math
    @test @isdefined sincos
    @test sincos === Base.sincos
end

# issue #23218
let idx = (7,5,9)
    (v,) = (idx...,)
    @test v == 7
end

module UnionOptimizations

using Test
using Dates
using Random

const boxedunions = [Union{}, Union{String, Nothing}]
const unboxedunions = [Union{Int8, Nothing},
                       Union{Int8, Float16, Nothing},
                       Union{Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128},
                       Union{Char, Date, Int}]

@test !Base.isbitsunion(boxedunions[1])
@test !Base.isbitsunion(boxedunions[2])
@test Base.isbitsunion(unboxedunions[1])
@test Base.isbitsunion(unboxedunions[2])
@test Base.isbitsunion(unboxedunions[3])

@test Base.bitsunionsize(unboxedunions[1]) == 1
@test Base.bitsunionsize(unboxedunions[2]) == 2
@test Base.bitsunionsize(unboxedunions[3]) == 16
@test Base.bitsunionsize(unboxedunions[4]) == 8

@test sizeof(unboxedunions[1]) == 1
@test sizeof(unboxedunions[2]) == 2
@test sizeof(unboxedunions[3]) == 16
@test sizeof(unboxedunions[4]) == 8

initvalue(::Type{Nothing}) = nothing
initvalue(::Type{Char}) = '\0'
initvalue(::Type{Date}) = Date(0, 12, 31)
initvalue(::Type{T}) where {T <: Number} = T(0)

initvalue2(::Type{Nothing}) = nothing
initvalue2(::Type{Char}) = Char(0x01)
initvalue2(::Type{Date}) = Date(1)
initvalue2(::Type{T}) where {T <: Number} = T(1)

U = unboxedunions[1]

@noinline compare(a, b) = (a === b) # make sure we are testing code-generation of `is`
egal(x, y) = (ccall(:jl_egal, Cint, (Any, Any), x, y) != 0) # make sure we are NOT testing code-generate of `is`

mutable struct UnionField
    u::U
end

let x = UnionField(initvalue(Base.uniontypes(U)[1]))
    @test x.u === initvalue(Base.uniontypes(U)[1])
    x.u = initvalue2(Base.uniontypes(U)[1])
    @test x.u === initvalue2(Base.uniontypes(U)[1])
    x.u = initvalue(Base.uniontypes(U)[2])
    @test x.u === initvalue(Base.uniontypes(U)[2])
end

mutable struct UnionField2
    x::Union{Nothing, Int}
    @noinline UnionField2() = new()
end
@test UnionField2().x === nothing

struct UnionField3
    x::Union{Nothing, Int}
    @noinline UnionField3() = new()
end
@test UnionField3().x === nothing

mutable struct UnionField4
    x::Union{Nothing, Float64}
    y::Union{Nothing, Int8}
    z::NTuple{8, UInt8}
    @noinline UnionField4() = new()
    @noinline UnionField4(x, y) = new(x, y, (0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88))
end
@test UnionField4().x === nothing
@test UnionField4().y === nothing
let x4 = UnionField4(nothing, Int8(3))
    x4copy = deepcopy(x4)
    @test x4.x === nothing
    @test x4.y === Int8(3)
    @test x4.z[1] === 0x11
    @test compare(x4, x4)
    @test x4 == x4
    @test egal(x4, x4)
    @test !(x4 === x4copy)
    @test !(x4 == x4copy)
    @test !egal(x4, x4copy)
end

struct UnionField5
    x::Union{Nothing, Float64}
    y::Union{Nothing, Int8}
    z::NTuple{8, UInt8}
    @noinline UnionField5() = new()
    @noinline UnionField5(x, y) = new(x, y, (0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88))
end
@test UnionField5().x === nothing
@test UnionField5().y === nothing
let x5 = UnionField5(nothing, Int8(3))
    x5copy = deepcopy(x5)
    @test x5.x === nothing
    @test x5.y === Int8(3)
    @test x5.z[1] === 0x11
    @test compare(x5, x5)
    @test x5 == x5
    @test compare(x5, x5copy)
    @test x5 == x5copy
    @test egal(x5, x5copy)
    @test objectid(x5) === objectid(x5copy)
    @test hash(x5) === hash(x5copy)
end


# PR #23367
struct A23367
    x::Union{Int8, Int16, NTuple{7, Int8}, Nothing}
end
struct B23367
    x::Int8
    y::A23367
    z::Int8
end
@noinline get_x(a::A23367) = a.x
function constant23367 end
let
    b = B23367(91, A23367(ntuple(i -> Int8(i), Val(7))), 23)
    @eval @noinline constant23367(a, b) = (a ? b : $b)
    b2 = Ref(b)[] # copy b via field assignment
    b3 = B23367[b][1] # copy b via array assignment
    addr(@nospecialize x) = ccall(:jl_value_ptr, Ptr{Cvoid}, (Any,), x)
    @test addr(b)  == addr(b)
    @test addr(b)  == addr(b2)
    @test addr(b)  == addr(b3)
    @test addr(b2) == addr(b3)

    @test b === b2 === b3 === b
    @test egal(b, b2) && egal(b2, b3) && egal(b3, b)
    @test compare(b, b2) && compare(b, b3) && compare(b2, b3)
    @test objectid(b) === objectid(b2) == objectid(b3)
    @test b.x === Int8(91)
    @test b.z === Int8(23)
    @test b.y === A23367((Int8(1), Int8(2), Int8(3), Int8(4), Int8(5), Int8(6), Int8(7)))
    @test sizeof(b) == sizeof(Int) * 3
    @test A23367(Int8(1)).x === Int8(1)
    @test A23367(Int8(0)).x === Int8(0)
    @test A23367(Int16(1)).x === Int16(1)
    @test A23367(nothing).x === nothing
    @test sizeof(b.y) == 8
    @test get_x(A23367(Int8(1))) === Int8(1)

    # test code-generation of constants
    other = B23367(91, A23367(nothing), 23)
    @test constant23367(true, other) === other
    @test constant23367(false, other) === b
end

for U in boxedunions
    local U
    for N in (1, 2, 3, 4)
        A = Array{U}(undef, ntuple(x->0, N)...)
        @test isempty(A)
        @test sizeof(A) == 0

        A = Array{U}(undef, ntuple(x->10, N)...)
        @test length(A) == 10^N
        @test sizeof(A) == sizeof(Int) * (10^N)
        @test !isassigned(A, 1)
    end
end

# unsafe_wrap
let
    A4 = [1, 2, 3]
    @test_throws ArgumentError unsafe_wrap(Array, convert(Ptr{Union{Int, Nothing}}, pointer(A4)), 3)
    A5 = [1 2 3; 4 5 6]
    @test_throws ArgumentError unsafe_wrap(Array, convert(Ptr{Union{Int, Nothing}}, pointer(A5)), 6)
end

# copyto!
A23567 = Vector{Union{Float64, Nothing}}(undef, 5)
B23567 = collect(Union{Float64, Nothing}, 1.0:3.0)
copyto!(A23567, 2, B23567)
@test A23567[1] === nothing
@test A23567[2] === 1.0
@test A23567[3] === 2.0
@test A23567[4] === 3.0

# vcat
t2 = deepcopy(A23567)
t3 = deepcopy(A23567)
t4 = vcat(A23567, t2, t3)
@test t4[1:5] == A23567
@test t4[6:10] == A23567
@test t4[11:15] == A23567

using Serialization

for U in unboxedunions
    local U
    for N in (1, 2, 3, 4)
        A = Array{U}(undef, ntuple(x->0, N)...)
        @test isempty(A)
        @test sizeof(A) == 0

        len = ntuple(x->10, N)
        mxsz = maximum(sizeof, Base.uniontypes(U))
        A = Array{U}(undef, len)
        @test length(A) == prod(len)
        @test sizeof(A) == prod(len) * mxsz
        @test isassigned(A, 1)
        @test isassigned(A, length(A))

        # arrayref / arrayset
        F = Base.uniontypes(U)[1]
        @test A[1] === initvalue(F)
        A[1] = initvalue2(F)
        @test A[1] === initvalue2(F)

        F2 = Base.uniontypes(U)[2]
        A[2] = initvalue(F2)
        @test A[2] === initvalue(F2)

        for (i, U2) in enumerate(Base.uniontypes(U))
            A[i] = initvalue2(U2)
            @test A[i] === initvalue2(U2)
        end

        # serialize / deserialize
        io = IOBuffer()
        serialize(io, A)
        seekstart(io)
        A2 = deserialize(io)
        @test A == A2

        # reshape
        A3 = reshape(A, (div(prod(len), 2), 2))
        @test sizeof(A) == prod(len) * mxsz
        @test isassigned(A, 1)
        @test A[1] === initvalue2(F)

        # copy
        A4 = copy(A)
        @test A == A4

        if N == 1
            ## Dequeue functions
            # pop!
            F2 = Base.uniontypes(U)[2]
            len = len[1]
            A = U[initvalue2(F2) for i = 1:len]
            for i = 1:len
                @test A[end] === initvalue2(F2)
                v = pop!(A)
                @test v === initvalue2(F2)
            end
            @test isempty(A)

            # popfirst!
            A = U[initvalue2(F2) for i = 1:len]
            for i = 1:len
                @test A[1] === initvalue2(F2)
                popfirst!(A)
            end
            @test isempty(A)

            # empty!
            A = U[initvalue2(F2) for i = 1:len]
            empty!(A)
            @test isempty(A)

            # resize!
            A = U[initvalue2(F2) for i = 1:len]
            resize!(A, 1)
            @test length(A) === 1
            @test A[1] === initvalue2(F2)
            resize!(A, len)
            @test length(A) === len
            @test A[1] === initvalue2(F2)
            @test typeof(A[end]) === F

            # deleteat!
            F = Base.uniontypes(U)[2]
            A = U[rand(F(1):F(len)) for i = 1:len]
            # The 2-arg `unique` method works around #22688
            deleteat!(A, map(Int, sort!(unique(identity, A[1:4]))))
            A = U[initvalue2(F2) for i = 1:len]
            deleteat!(A, 1:2)
            @test length(A) == len - 2
            @test all(A .== initvalue2(F2))
            deleteat!(A, 1:2)
            @test length(A) == len - 4
            @test all(A .== initvalue2(F2))
            A = U[initvalue2(F2) for i = 1:len]
            deleteat!(A, length(A)-1:length(A))
            @test length(A) == len - 2
            @test all(A .== initvalue2(F2))
            deleteat!(A, length(A)-1:length(A))
            @test length(A) == len - 4
            @test all(A .== initvalue2(F2))
            A = U[initvalue2(F2) for i = 1:len]
            deleteat!(A, 2:3)
            @test length(A) == len - 2
            @test all(A .== initvalue2(F2))
            A = U[initvalue2(F2) for i = 1:len]
            deleteat!(A, length(A)-2:length(A)-1)
            @test length(A) == len - 2
            @test all(A .== initvalue2(F2))

            # pushfirst!
            A = U[initvalue2(F2) for i = 1:len]
            for i = 1:5
                pushfirst!(A, initvalue2(F))
                pushfirst!(A, initvalue(F2))
                @test A[1] === initvalue(F2)
                @test A[2] === initvalue2(F)
            end

            # push! / append! / prepend!
            A = U[initvalue2(F2) for i = 1:len]
            push!(A, initvalue2(F))
            @test A[end] === initvalue2(F)
            push!(A, initvalue2(F2))
            @test A[end] === initvalue2(F2)
            append!(A, [initvalue(F), initvalue2(F)])
            @test A[end] === initvalue2(F)
            @test A[end-1] === initvalue(F)
            prepend!(A, [initvalue(F), initvalue2(F)])
            @test A[2] === initvalue2(F)
            @test A[1] === initvalue(F)

            # insert!
            A = U[initvalue2(F2) for i = 1:len]
            insert!(A, 2, initvalue2(F))
            @test A[2] === initvalue2(F)
            @test A[1] === initvalue2(F2)
            @test A[3] === initvalue2(F2)
            @test A[end] === initvalue2(F2)
            A = U[initvalue2(F2) for i = 1:len]
            insert!(A, 8, initvalue2(F))
            @test A[8] === initvalue2(F)
            @test A[7] === initvalue2(F2)
            @test A[9] === initvalue2(F2)
            @test A[end] === initvalue2(F2)

            # splice!
            A = U[initvalue2(F2) for i = 1:len]
            V = splice!(A, 1:2)
            @test length(A) == len - 2
            @test length(V) == 2
            @test V[1] == initvalue2(F2)
            @test V[2] == initvalue2(F2)
            @test A[1] == initvalue2(F2)
            @test A[end] == initvalue2(F2)

            A = U[initvalue2(F2) for i = 1:len]
            V = splice!(A, 4:5)
            @test length(A) == len - 2
            @test length(V) == 2
            @test V[1] == initvalue2(F2)
            @test V[2] == initvalue2(F2)
            @test A[1] == initvalue2(F2)
            @test A[end] == initvalue2(F2)
        end
    end
end

end # module UnionOptimizations

# issue #6614, argument destructuring
f6614((x, y)) = [x, y]
@test f6614((4, 3)) == [4, 3]
g6614((x, y), (z,), (a, b)) = (x,y,z,a,b)
@test g6614((1, 2), (3,), (4, 5)) === (1,2,3,4,5)
@test_throws MethodError g6614(1, 2)
@test_throws MethodError g6614((1, 2), (3,))
@test_throws BoundsError g6614((1, 2), (3,), (1,))
h6614((x, y) = (5, 6)) = (y, x)
@test h6614() == (6, 5)
@test h6614((4, 5)) == (5, 4)
ff6614((x, y)::Tuple{Int, String}) = (x, y)
@test ff6614((1, "")) == (1, "")
@test_throws MethodError ff6614((1, 1))
gg6614((x, y)::Tuple{Int, String} = (2, " ")) = (x, y)
@test gg6614() == (2, " ")
function hh6614()
    x, y = 1, 2
    function g((x,y))
        # make sure x and y are local
    end
    g((4,5))
    x, y
end
@test hh6614() == (1, 2)
# issue #26518
function f26518((a,b)) end
@test f26518((1,2)) === nothing

# issue 22098
macro m22098 end
handle_on_m22098 = getfield(@__MODULE__, Symbol("@m22098"))
@test isempty(methods(handle_on_m22098))

# issue 24363
mutable struct A24363
    x::Union{Int,Nothing}
end

int24363 = A24363(65535)
void24363 = A24363(nothing)
f24363(a) = a.x
@test f24363(int24363) === 65535
@test f24363(void24363) === nothing

# issue 17149
mutable struct Foo17149
end
@test Foo17149() !== Foo17149()
let a = Foo17149()
    @test a === a
end

# issue #25907
g25907a(x) = x[1]::Integer
@test g25907a(Union{Int, UInt, Nothing}[1]) === 1
g25907b(x) = x[1]::Complex
@test g25907b(Union{Complex{Int}, Complex{UInt}, Nothing}[1im]) === 1im

#issue #26363
@test eltype(Ref(Float64(1))) === Float64
