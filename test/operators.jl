# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "operators" begin
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
@test ifelse(B, 1, 2) == [1 1 2]
@test ifelse(B, 1, [2 3 4]) == [1 1 4]
@test ifelse(B, [2 3 4], 1) == [2 3 1]
@test ifelse(B, [2 3 4], [5 6 7]) == [2 3 7]

@test reverse(Pair(1,2)) == Pair(2,1)
@test reverse(Pair("13","24")) == Pair("24","13")
@test typeof(reverse(Pair{String,Int64}("a",1))) == Pair{Int64,String}

p = 1=>:foo
@test first(p) == 1
@test last(p)  == :foo
@test first(reverse(p)) == :foo
@test last(reverse(p))  == 1
@test endof(p) == 2
@test p[endof(p)] == p[end] == p[2] == :foo

@test (|)(2) == 2
@test ($)(2) == 2

@test ctranspose('a') == 'a'

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
        @test max(xs[1:n]...) == [n:n+4;]
    end
end
end
