# This file is a part of Julia. License is MIT: http://julialang.org/license

# Test functionality of IntSet

@testset "Construction, collect" begin
    data_in = (1,5,100)
    s = IntSet(data_in)
    data_out = collect(s)
    @test all(map(d->in(d,data_out), data_in))
    @test length(data_out) === length(data_in)
end

@testset "eltype, similar" begin
    @test eltype(IntSet()) === Int
    @test eltype(IntSet) === Int
    @test isequal(similar(IntSet([1,2,3])), IntSet())
end

@testset "show" begin
    @test sprint(show, IntSet()) == "IntSet([])"
    @test sprint(show, IntSet([1,2,3])) == "IntSet([1, 2, 3])"
    show(IOBuffer(), IntSet())
end

@testset "in, hashing" begin
    s = IntSet([1,2,10,20,200,300,1000,10000,10002])
    @test last(s) === 10002
    @test first(s) === 1
    @test length(s) === 9
    @test pop!(s) === 10002
    @test_throws KeyError pop!(s, -1)
    @test length(s) === 8
    @test shift!(s) === 1
    @test length(s) === 7
    @test !in(0,s)
    @test !in(1,s)
    @test in(2,s)
    @test !in(10002,s)
    @test in(10000,s)
    @test in(10000.0,s)
    @test !in(10002.0,s)
    @test_throws ArgumentError first(IntSet())
    @test_throws ArgumentError last(IntSet())
    t = copy(s)
    sizehint!(t, 20000) #check that hash does not depend on size of internal storage
    @test hash(s) === hash(t)
    push!(t, 20000)
    @test 20000 in t
    sizehint!(t, 200) # ensure that sizehint!'ing a small amount isn't destructive
    @test 20000 in t
    @test pop!(t, 20000) === 20000
    @test hash(s) === hash(t)
    # Ensure empty chunks don't affect hash
    @test hash(IntSet([1])) != hash(IntSet([17]))
    @test hash(IntSet([1])) != hash(IntSet([33]))
    @test hash(IntSet([1])) != hash(IntSet([65]))
    @test hash(IntSet([1])) != hash(IntSet([129]))

    # issue #7851
    @test_throws ArgumentError IntSet(-1)
    @test !(-1 in IntSet(1:10))
end

# # issue #8570
# This requires 2^29 bytes of storage, which is too much for a simple test
# s = IntSet(typemax(Int32))
# @test length(s) === 1
# for b in s; b; end

@testset "union!, symdiff!" begin
    i = IntSet([1, 2, 3])
    union!(i, [1, 2])
    @test length(i) === 3
    union!(i, [3, 4, 5])
    @test length(i) === 5

    @test_throws KeyError pop!(i, 10)

    empty!(i)
    @test length(i) === 0

    @test_throws ArgumentError symdiff!(i, -3)
    @test symdiff!(i, 3) == IntSet([3])
    @test symdiff!(i, 257) == IntSet([3, 257])
    @test symdiff!(i, [3, 6]) == IntSet([6, 257])

    i = IntSet(1:6)
    @test symdiff!(i, IntSet([6, 513])) == IntSet([1:5; 513])
end

@testset "copy, copy!, similar" begin
    s1 = IntSet([1,2,3])
    s2 = similar(s1)
    copy!(s2, s1)
    s3 = copy(s2)
    @test s3 == s2 == s1
    @test collect(s3) == collect(s2) == [1,2,3]
end

@testset "push!, union" begin
    i = IntSet([1, 2, 3])
    j = union(i)
    @test j == i
    @test !(j === i)

    j = IntSet([4, 5, 6])
    @test union(i, j) == IntSet(1:6)

    k = IntSet([7, 8, 9])
    @test union(i, j, k) == IntSet(1:9)
    i = IntSet([1, 2, 3])
    j = union(i)
    @test j == i
    @test !(j === i)

    j = IntSet([4, 5, 6])
    @test union(i, j) == IntSet(1:6)

    k = IntSet([7, 8, 9])
    @test union(i, j, k) == IntSet(1:9)

    s1 = IntSet()
    @test_throws ArgumentError push!(s1, -1)
    push!(s1, 1, 10, 100, 1000)
    @test collect(s1) == [1, 10, 100, 1000]
    push!(s1, 606)
    @test collect(s1) == [1, 10, 100, 606, 1000]
    s2 = IntSet()
    @test s2 === union!(s2, s1)
    s3 = IntSet([1, 10, 100])
    union!(s3, [1, 606, 1000])
    s4 = union(IntSet([1, 100, 1000]), IntSet([10, 100, 606]))
    @test s1 == s2 == s3 == s4
end

@testset "pop!, delete!" begin
    s = IntSet(1:2:10)
    @test pop!(s, 1) === 1
    @test !(1 in s)
    @test_throws KeyError pop!(s, 1)
    @test_throws KeyError pop!(s, -1)
    @test pop!(s, -1, 1) === 1
    @test pop!(s, 1, 0) === 0
    @test s === delete!(s, 1)
    for i in s; pop!(s, i); end
    @test isempty(s)
    push!(s, 100)
    @test pop!(s, 100) == 100
    push!(s, 1:2:10...)
    @test pop!(s) === 9
    @test pop!(s) === 7
    @test shift!(s) === 1
    @test shift!(s) === 3
    @test collect(s) == [5]
    empty!(s)
    @test isempty(s)
end

@testset "intersect" begin
    i = IntSet([1, 2, 3])
    j = IntSet([4, 5, 6])

    @test intersect(i) == i
    @test !(intersect(i) === i)
    @test intersect(i, j) == IntSet([])
    push!(j, 257)
    @test intersect(i, j) == IntSet([])
    push!(j, 2, 3, 17)
    @test intersect(i, j) == IntSet([2, 3])
    k = IntSet([1, 2, 3, 4, 5, 6, 7])
    @test intersect(i, j, k) == IntSet([2, 3])

    @test isempty(intersect(IntSet()))
    @test isempty(intersect(IntSet(1:10), IntSet()))
    @test isempty(intersect(IntSet(), IntSet(1:10)))

    @test intersect(IntSet([1,2,3])) == IntSet([1,2,3])
    @test intersect(IntSet(1:7), IntSet(3:10)) ==
    	  intersect(IntSet(3:10), IntSet(1:7)) == IntSet(3:7)
    @test intersect(IntSet(1:10), IntSet(1:4), 1:5, [2,3,10]) == [2,3]
end

@testset "setdiff, symdiff" begin
    @test setdiff(IntSet([1, 2, 3, 4]), IntSet([2, 4, 5, 6])) == IntSet([1, 3])
    @test symdiff(IntSet([1, 2, 3, 4]), IntSet([2, 4, 5, 6])) == IntSet([1, 3, 5, 6])

    s2 = IntSet([1, 2, 3, 4])
    setdiff!(s2, IntSet([2, 4, 5, 6]))
    @test s2 == IntSet([1, 3])

    s1 = IntSet(1:100)
    setdiff!(s1, IntSet(1:2:100))
    s2 = setdiff(IntSet(1:100), IntSet(1:2:100))
    @test s1 == s2 == IntSet(2:2:100)
    @test collect(s1) == collect(2:2:100)

    @test symdiff(IntSet([1, 2, 3, 4]), IntSet([2, 4, 5, 6])) ==
          symdiff(IntSet([2, 4, 5, 6]), IntSet([1, 2, 3, 4])) ==
          symdiff(IntSet([1, 2, 3, 4]), [2, 4, 5, 6]) ==
          symdiff(IntSet([2, 4, 5, 6]), [1, 2, 3, 4]) == IntSet([1, 3, 5, 6])
end

@testset "subsets, equality" begin
    i = IntSet([1, 2, 3])
    k = IntSet([4, 5])
    copy!(k, i)
    @test k == i
    @test !(k === i)
    copy!(k, k)
    @test k == i

    i = IntSet([1, 2, 3])
    j = IntSet([1, 2, 4])
    @test i != j

    push!(j, 257)
    pop!(j, 257)
    @test i != j
    @test j != i

    @test issubset(IntSet([1, 2, 4]), IntSet(1:10))
    @test issubset(IntSet([]), IntSet([]))
    @test IntSet([1, 2, 4]) < IntSet(1:10)
    @test !(IntSet([]) < IntSet([]))
    @test IntSet([1, 2, 4]) <= IntSet(1:10)
    @test IntSet([1, 2, 4]) <= IntSet([1, 2, 4])
    @test IntSet([]) <= IntSet([])

    @test IntSet(2:2:10) < IntSet(1:10)
    @test !(IntSet(2:2:10) < IntSet(2:2:10))
    @test IntSet(2:2:10) <= IntSet(2:10)
    @test IntSet(2:2:10) <= IntSet(2:2:10)

    # == with last-bit set (groups.google.com/forum/#!topic/julia-users/vZNjiIEG_sY)
    s = IntSet(255)
    @test s == s
end

@testset "setlike" begin
    p = IntSet([1,2,5,6])
    resize!(p.bits, 6)
    q = IntSet([1,3,5,7])
    resize!(q.bits, 8)
    a = Set(p)
    b = Set(q)
    for f in (union, intersect, setdiff, symdiff)
        @test collect(f(p, p)) == sort(collect(f(a, a)))
        @test collect(f(q, q)) == sort(collect(f(b, b)))
        @test collect(f(p, q)) == sort(collect(f(a, b)))
        @test collect(f(q, p)) == sort(collect(f(b, a)))
    end
end

@testset "misc" begin
    s = IntSet()
    push!(s, 1, 2, 100)
    @test !(0 in s)
    @test 1 in s
    @test 2 in s
    @test !(3 in s)
    @test 100 in s
    @test !(101 in s)
    @test !(1000 in s)
    @test first(s) === 1
    @test last(s) === 100
    @test s == IntSet([1, 2, 100])
    push!(s, 1000)
    @test [i for i in s] == [1, 2, 100, 1000]
    @test pop!(s) === 1000
    @test s == IntSet([1, 2, 100])
    @test hash(s) === hash(IntSet([1, 2, 100]))

    b = 1:1000
    s = IntSet(b)
    @test collect(s) == collect(b)
    @test length(s) === length(b)
    @test pop!(s, 100) === 100
    @test collect(s) == [1:99; 101:1000]
    @test_throws KeyError pop!(s, 100)
    @test_throws KeyError pop!(s, 0)
    @test pop!(s, 100, 0) === 0
    @test pop!(s, 99, 0) === 99
end
