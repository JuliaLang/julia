# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random: randstring

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
    @test last(p)  == :foo
    @test first(reverse(p)) == :foo
    @test last(reverse(p))  == 1
    @test lastindex(p) == 2
    @test p[lastindex(p)] == p[end] == p[2] == :foo
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

@test isless('a','b')

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
end

@testset "function negation" begin
    str = randstring(20)
    @test filter(!isuppercase, str) == replace(str, r"[A-Z]" => "")
    @test filter(!islowercase, str) == replace(str, r"[a-z]" => "")
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

@testset "generic isa" begin
    @test 1 isa Int
    @test "2" isa String

    ismatrix = isa(Matrix)
    @test ismatrix([1 2; 3 4])
    @test !ismatrix("hi")
end
