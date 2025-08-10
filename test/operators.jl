# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random: randstring

include(joinpath(@__DIR__,"../Compiler/test/irutils.jl"))

@testset "ifelse" begin
    @test ifelse(true, 1, 2) == 1
    @test ifelse(false, 1, 2) == 2

    let s = Set()
        ifelse(true, push!(s, 1), push!(s, 2))
        @test s == Set([1, 2])
    end

    let s = Set()
        true ? push!(s, 1) : push!(s, 2)
        false ? push!(s, 3) : push!(s, 4)
        @test s == Set([1, 4])
    end

    let B = [true true false]
        @test ifelse.(B, 1, 2) == [1 1 2]
        @test ifelse.(B, 1, [2 3 4]) == [1 1 4]
        @test ifelse.(B, [2 3 4], 1) == [2 3 1]
        @test ifelse.(B, [2 3 4], [5 6 7]) == [2 3 7]
    end
end

@testset "operations on Pairs" begin
    @test reverse(Pair(1,2)) == Pair(2,1)
    @test reverse(Pair("13","24")) == Pair("24","13")
    @test typeof(reverse(Pair{String,Int64}("a",1))) == Pair{Int64,String}
    @test convert(Pair{Float64,Float64}, 17 => 4711) === (17.0 => 4711.0)
    @test convert(Pair{Int,Float64}, 17 => 4711) === (17 => 4711.0)
    @test convert(Pair{Float64,Int}, 17 => 4711) === (17.0 => 4711)
    @test convert(Pair{Any,Any}, 17 => 4711) === Pair{Any,Any}(17, 4711)
    @test convert(Pair{Number,Number}, 17 => 4711) === Pair{Number,Number}(17, 4711)
    @test promote(1=>1, 2=>2.0) === (1=>1.0, 2=>2.0)
    @test promote(1=>1, 2.0=>2) === (1.0=>1, 2.0=>2)
    @test promote(1=>1.0, 2.0=>2) === (1.0=>1.0, 2.0=>2.0)
    @test promote(1=>1, :b=>2.0) === (Pair{Any,Float64}(1,1.0),Pair{Any,Float64}(:b,2.0))
    @test isa([:a=>1, :b=>2], Vector{Pair{Symbol,Int}})
    @test isa([:a=>1, :b=>2.0], Vector{Pair{Symbol,Float64}})
    @test isa(["a"=>1, :b=>2.0], Vector{Pair{Any,Float64}})

    p = 1=>:foo
    @test first(p) == 1
    @test last(p)  === :foo
    @test first(reverse(p)) === :foo
    @test last(reverse(p))  == 1
    @test lastindex(p) == 2
    @test p[lastindex(p)] == p[end] == p[2] === :foo
end

# Infix `isa`
@test 1 isa Integer

@test (|)(2) == 2
@test xor(2) == 2
@test (⊻)(2) == 2

@test_throws MethodError min(Set([1]), Set([2]))
@test_throws MethodError max(Set([1]), Set([2]))
@test_throws MethodError minmax(Set([1]), Set([2]))

# Test if isless (not <) is used by min, max, minmax
# and commutativity.
struct TO23094
    x::Int
end
Base.isless(a::TO23094, b::TO23094) = isless(a.x, b.x)
Base.isequal(a::TO23094, b::TO23094) = isequal(a.x, b.x)
import Base.<
<(a::TO23094, b::TO23094) = error("< should not be called")

@test isequal(min(TO23094(1), TO23094(2)), TO23094(1))
@test isequal(min(TO23094(2), TO23094(1)), TO23094(1))
@test isequal(max(TO23094(1), TO23094(2)), TO23094(2))
@test isequal(max(TO23094(2), TO23094(1)), TO23094(2))
@test isequal(minmax(TO23094(1), TO23094(2))[1], TO23094(1))
@test isequal(minmax(TO23094(1), TO23094(2))[2], TO23094(2))
@test isequal(minmax(TO23094(2), TO23094(1))[1], TO23094(1))
@test isequal(minmax(TO23094(2), TO23094(1))[2], TO23094(2))

let m = Module()
    @eval m begin
        struct Foo end
        foo(xs) = isequal(xs[1], Foo())
    end
    @test !(@inferred m.foo(Any[42]))
end

@test isless('a','b')

@testset "isless on pairs of integers (because there is a fastpath)" begin
    @test isless((1,2), (1,3))
    @test isless((0,-2), (0,2))
    @test isless((-1,2), (1,2))
    @test isless((-1,-2), (1,2))
    @test !isless((1,3), (1,2))
    @test !isless((0,2), (0,-2))
    @test !isless((1,2), (-1,2))
    @test !isless((1,2), (-1,-2))
    @test !isless((-1,-2), (-1,-2))

    @test isless((typemin(Int), typemin(Int)), (0,0))
    @test isless((1, 1), (Int8(2), Int8(2)))
    @test !isless((UInt8(200),Int8(-1)), (UInt8(200),Int8(-1)))
    @test isless((1, 1), (1, unsigned(2)))
end

@testset "isgreater" begin
    # isgreater should be compatible with min.
    min1(a, b) = Base.isgreater(a, b) ? b : a
    # min promotes numerical arguments to the same type, but our quick min1
    # doesn't, so use float test values instead of ints.
    values = (1.0, 5.0, NaN, missing, Inf)
    for a in values, b in values
        @test min(a, b) === min1(a, b)
        @test min((a,), (b,)) === min1((a,), (b,))
        @test all(min([a], [b]) .=== min1([a], [b]))
    end
end

@testset "isunordered" begin
    @test  isunordered(NaN)
    @test  isunordered(NaN32)
    @test  isunordered(missing)
    @test !isunordered(1)
    @test !isunordered([NaN, 1])
    @test !isunordered([1.0, missing])
end

@testset "vectorized comparisons between numbers" begin
    @test 1 .!= 2
    @test 1 .== 1
    @test 1 .< 2
    @test 1 .<= 2
end

# issue #13144: max() with 4 or more array arguments
let xs = [[i:i+4;] for i in 1:10]
    for n in 2:10
        @test max.(xs[1:n]...) == [n:n+4;]
    end
end

# issue #19714
struct T19714 <: Integer end
Base.float(::T19714) = 19714.0
Base.:/(::T19714, ::T19714) = T19714()
Base.convert(::Type{T19714}, ::Int) = T19714()
Base.promote_rule(::Type{T19714}, ::Type{Int}) = T19714
@test T19714()/1 === 1/T19714() === T19714()

@testset "operators with zero argument" begin
    @test_throws(MethodError, +())
    @test_throws(MethodError, *())
    @test isempty(methods(+, ()))
    @test isempty(methods(*, ()))
end

# pr #17155 and #33568
@testset "function composition" begin
    @test (uppercase∘(x->string(x,base=16)))(239487) == "3A77F"
    @test ∘(x -> x-2, x -> x-3, x -> x+5)(7) == 7
    fs = [x -> x[1:2], uppercase, lowercase]
    @test ∘(fs...)("ABC") == "AB"

    # Like +() and *() we leave ∘() undefined.
    # While `∘() = identity` is a reasonable definition for functions, this
    # would cause headaches for composition of user defined morphisms.
    # See also #34251
    @test_throws(MethodError, ∘())

    @test ∘(x -> (x, 1))(0) === (0, 1)
    @test ∘(x -> (x, 2), x -> (x, 1))(0) === ((0, 1), 2)
    @test ∘(x -> (x, 3), x -> (x, 2), x->(x,1))(0) === (((0, 1), 2), 3)
    @test ∘(x -> (x, 4), x -> (x, 3), x->(x,2), x-> (x, 1))(0) === ((((0, 1), 2), 3), 4)

    # test that user defined functors only need to overload the two arg version
    struct FreeMagma
        word
    end
    Base.:(∘)(a::FreeMagma, b::FreeMagma) = FreeMagma((a.word, b.word))

    @test ∘(FreeMagma(1)) === FreeMagma(1)
    @test ∘(FreeMagma(1), FreeMagma(2)) === FreeMagma((1,2))
    @test ∘(FreeMagma(1), FreeMagma(2), FreeMagma(3)) === FreeMagma(((1,2), 3))
    @test ∘(FreeMagma(1), FreeMagma(2), FreeMagma(3), FreeMagma(4)) === FreeMagma((((1,2), 3), 4))

    @test fieldtypes(typeof(Float64 ∘ Int)) == (Type{Float64}, Type{Int})

    @test repr(uppercase ∘ first) == "uppercase ∘ first"
    @test sprint(show, "text/plain", uppercase ∘ first) == "uppercase ∘ first"

    # test keyword args in composition
    function kwf(a;b,c); a + b + c; end
    @test (abs2 ∘ kwf)(1,b=2,c=3) == 36

end

@testset "Nested ComposedFunction's stability" begin
    f(x) = (1, 1, x...)
    g = (f ∘ (f ∘ f)) ∘ (f ∘ f ∘ f)
    @test (@inferred (g∘g)(1)) == ntuple(Returns(1), 25)
    @test (@inferred g(1)) == ntuple(Returns(1), 13)
    h = (-) ∘ (-) ∘ (-) ∘ (-) ∘ (-) ∘ (-) ∘ sum
    @test (@inferred h((1, 2, 3); init = 0.0)) == 6.0
    issue_45877 = reduce(∘, fill(sin, 50))
    @test Core.Compiler.is_foldable(Base.infer_effects(Base.unwrap_composed, (typeof(issue_45877),)))
    @test fully_eliminated() do
        issue_45877(1.0)
    end
end

@testset "function negation" begin
    str = randstring(20)
    @test filter(!isuppercase, str) == replace(str, r"[A-Z]" => "")
    @test filter(!islowercase, str) == replace(str, r"[a-z]" => "")
    @test !!isnan === isnan
    @test repr(!isnan) == "!isnan"
    @test repr((-) ∘ sin) == "(-) ∘ sin"
    @test repr(cos ∘ (sin ∘ tan)) == "cos ∘ (sin ∘ tan)"
    @test repr(!(cos ∘ !sin)) == "!(cos ∘ !sin)"
    @test repr(cos ∘ sin ∘ tan) == "cos ∘ sin ∘ tan" == repr((cos ∘ sin) ∘ tan)
end

# issue #19891
@testset "chained comparison" begin
    B = 0 .< [1 -1 5] .< 3
    @test B == [true false false]
    B = 3 .> [1 -1 5] .> 0
    @test B == [true false false]
end

struct TypeWrapper
    t::Type
end
Base.:(<)(x::TypeWrapper, y::TypeWrapper) = (x.t <: y.t) & (x.t != y.t)
@testset "poset" begin
    #   Real
    #  /    \
    # Int  Float64
    #  \    /
    #  Union{}
    @test TypeWrapper(Int) <= TypeWrapper(Int)
    @test TypeWrapper(Int) <= TypeWrapper(Real)
    @test !(TypeWrapper(Int) <= TypeWrapper(Float64))
end

# issue #20355
@testset "mod1, fld1" begin
    for T in [Int8, Int16, Int32, Int64],
        x in T[typemin(T); typemin(T) + 1; -10:10; typemax(T)-1; typemax(T)],
        y in T[typemin(T); typemin(T) + 1; -10:-1; 1:10; typemax(T)-1; typemax(T)]

        m = mod1(x, y)
        @test mod(x, y) == mod(m, y)
        if y > 0
            @test 0 < m <= y
        else
            @test y <= m < 0
        end
        if x == typemin(T) && y == -1
            @test_throws DivideError fld1(x, y)
        else
            f = fld1(x, y)
            @test (f - 1) * y + m == x
        end
    end

    for T in [UInt8, UInt16, UInt32, UInt64],
        x in T[0:10; typemax(T)-1; typemax(T)],
        y in T[1:10; typemax(T)-1; typemax(T)]

        m = mod1(x, y)
        @test mod(x, y) == mod(m, y)
        @test 0 < m <= y
        f = fld1(x, y)
        @test (f - 1) * y + m == x
    end

    for T in [Float32, Float64, Rational{Int64}],
        x in T[k // 4 for k in -10:10],
        y in T[k // 4 for k in [-10:-1; 1:10]]

        m = mod1(x, y)
        @test mod(x, y) == mod(m, y)
        if y > 0
            @test 0 < m <= y
        else
            @test y <= m < 0
        end
        f = fld1(x, y)
        @test (f - 1) * y + m == x
    end

    @test fldmod1(4.0, 3) == fldmod1(4, 3)

    # issue 28973
    @test fld1(0.4, 0.9) == fld1(nextfloat(0.4), 0.9) == 1.0
end

@testset "Fix12" begin
    x = 9
    y = 7.0
    fx = Base.Fix1(/, x)
    fy = Base.Fix2(/, y)
    @test fx(y) == x / y
    @test fy(x) == x / y
end

@testset "curried comparisons" begin
    eql5 = (==)(5)
    neq5 = (!=)(5)
    gte5 = (>=)(5)
    lte5 = (<=)(5)
    gt5  = (>)(5)
    lt5  = (<)(5)

    @test eql5(5) && !eql5(0)
    @test neq5(6) && !neq5(5)
    @test gte5(5) && gte5(6)
    @test lte5(5) && lte5(4)
    @test gt5(6) && !gt5(5)
    @test lt5(4) && !lt5(5)
end

@testset "in tuples" begin
    @test ∈(5, (1,5,10,11))
    @test ∉(0, (1,5,10,11))
    @test ∈(5, (1,"hi","hey",5.0))
    @test ∉(0, (1,"hi","hey",5.0))
    @test ∈(5, (5,))
    @test ∉(0, (5,))
    @test ∉(5, ())
end

@testset "ni" begin
    @test ∋([1,5,10,11], 5)
    @test !∋([1,10,11], 5)
    @test ∋((1,5,10,11), 5)
    @test ∌((1,10,11), 5)
    @test ∋(5)([5,1])
    @test !∋(42)([0,1,100])
    @test ∌(0)(1:10)
    @test ∋(0)(-2:2)
end

@testset "in" begin
    @test in(0x00, [0x04, 0x03, 0x02, 0x00])
    @test !in(UInt8('a'), b"lkefjldk")
    @test in(Int8(-1), Int8[2, 5, -1, 2])
    @test !in(Int8(-1), UInt8[1, 3, 2, 0xff])
end

@test [Base.afoldl(+, 1:i...) for i = 1:40] == [i * (i + 1) ÷ 2 for i = 1:40]
@test Core.Compiler.is_terminates(Base.infer_effects(Base.afoldl, Tuple{typeof(+), Vararg{Int, 100}}))

@testset "Returns" begin
    @test @inferred(Returns(1)()   ) === 1
    @test @inferred(Returns(1)(23) ) === 1
    @test @inferred(Returns("a")(2,3)) == "a"
    @test @inferred(Returns(1)(x=1, y=2)) === 1
    @test @inferred(Returns(Int)()) === Int
    @test @inferred(Returns(Returns(1))()) === Returns(1)
    f = @inferred Returns(Int)
    @inferred f(1,2)
    val = [1,2,3]
    @test Returns(val)(1) === val
    @test sprint(show, Returns(1.0)) == "Returns{Float64}(1.0)"

    illtype = Vector{Core.TypeVar(:T)}
    @test Returns(illtype) == Returns{DataType}(illtype)
end

@testset "<= (issue #46327)" begin
    struct A46327 <: Real end
    Base.:(==)(::A46327, ::A46327) = false
    Base.:(<)(::A46327, ::A46327) = false
    @test !(A46327() <= A46327())
    struct B46327 <: Real end
    Base.:(==)(::B46327, ::B46327) = true
    Base.:(<)(::B46327, ::B46327) = false
    @test B46327() <= B46327()
end

@testset "inference for `x in itr::Tuple`" begin
    # concrete evaluation
    @test Core.Compiler.is_foldable(Base.infer_effects(in, (Int,Tuple{Int,Int,Int})))
    @test Core.Compiler.is_foldable(Base.infer_effects(in, (Char,Tuple{Char,Char,Char})))
    for i = (1,2,3)
        @testset let i = i
            @test @eval Base.return_types() do
                Val($i in (1,2,3))
            end |> only == Val{true}
        end
    end
    @test Base.infer_return_type() do
        Val(4 in (1,2,3))
    end == Val{false}
    @test Base.infer_return_type() do
        Val('1' in ('1','2','3'))
    end == Val{true}

    # constant propagation
    @test Base.infer_return_type((Int,Int)) do x, y
        Val(1 in (x,2,y))
    end >: Val{true}
    @test Base.infer_return_type((Int,Int)) do x, y
        Val(2 in (x,2,y))
    end == Val{true}

    # should use the loop implementation given large tuples to avoid inference blowup
    let t = ntuple(x->'A', 10000);
        @test Base.infer_return_type(in, (Char,typeof(t))) == Bool
    end
end
