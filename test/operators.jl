# This file is a part of Julia. License is MIT: http://julialang.org/license

@test ifelse(true, 1, 2) == 1
@test ifelse(false, 1, 2) == 2

s = Set()
ifelse(true, push!(s, 1), push!(s, 2))
@test s == Set([1, 2])

s = Set()
true ? push!(s, 1) : push!(s, 2)
false ? push!(s, 3) : push!(s, 4)
@test s == Set([1, 4])

B = [true true false]
@test ifelse.(B, 1, 2) == [1 1 2]
@test ifelse.(B, 1, [2 3 4]) == [1 1 4]
@test ifelse.(B, [2 3 4], 1) == [2 3 1]
@test ifelse.(B, [2 3 4], [5 6 7]) == [2 3 7]

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

# Infix `isa`
@test 1 isa Integer

p = 1=>:foo
@test first(p) == 1
@test last(p)  == :foo
@test first(reverse(p)) == :foo
@test last(reverse(p))  == 1
@test endof(p) == 2
@test p[endof(p)] == p[end] == p[2] == :foo

@test (|)(2) == 2
@test xor(2) == 2
@test (⊻)(2) == 2

# @test ctranspose('a') == 'a' # (c)transpose of Chars no longer supported

@test_throws ArgumentError Base.scalarmin(['a','b'],['c','d'])
@test_throws ArgumentError Base.scalarmin('a',['c','d'])
@test_throws ArgumentError Base.scalarmin(['a','b'],'c')
@test_throws ArgumentError Base.scalarmax(['a','b'],['c','d'])
@test_throws ArgumentError Base.scalarmax('a',['c','d'])
@test_throws ArgumentError Base.scalarmax(['a','b'],'c')

@test lexless('a','b')

@test 1 .!= 2
@test 1 .== 1
@test 1 .< 2
@test 1 .<= 2

# issue #13144: max() with 4 or more array arguments
let xs = [[i:i+4;] for i in 1:10]
    for n in 2:10
        @test max.(xs[1:n]...) == [n:n+4;]
    end
end

# issue #19714
immutable T19714 <: Integer end
Base.float(::T19714) = 19714.0
Base.:/(::T19714, ::T19714) = T19714()
Base.convert(::Type{T19714}, ::Int) = T19714()
Base.promote_rule(::Type{T19714}, ::Type{Int}) = T19714
@test T19714()/1 === 1/T19714() === T19714()

# pr #17155
@testset "function composition" begin
    @test (uppercase∘hex)(239487) == "3A77F"
end
@testset "function negation" begin
    str = randstring(20)
    @test filter(!isupper, str) == replace(str, r"[A-Z]", "")
    @test filter(!islower, str) == replace(str, r"[a-z]", "")
end

# issue #19891
@testset "chained comparison" begin
    B = 0 .< [1 -1 5] .< 3
    @test B == [true false false]
    B = 3 .> [1 -1 5] .> 0
    @test B == [true false false]
end
