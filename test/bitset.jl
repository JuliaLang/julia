# This file is a part of Julia. License is MIT: https://julialang.org/license

# Test functionality of BitSet

using Random

@testset "Construction, collect" begin
    data_in = (1,5,100)
    s = BitSet(data_in)
    data_out = collect(s)
    @test all(map(in(data_out), data_in))
    @test length(data_out) === length(data_in)
end

@testset "eltype, empty" begin
    @test eltype(BitSet()) === Int
    @test eltype(BitSet) === Int
    @test isequal(empty(BitSet([1,2,3])), BitSet())
end

@testset "show" begin
    @test sprint(show, BitSet()) == "BitSet([])"
    @test sprint(show, BitSet([1,2,3])) == "BitSet([1, 2, 3])"
    show(IOBuffer(), BitSet())
end

@testset "in, hashing" begin
    s = BitSet([1,2,10,20,200,300,1000,10000,10002])
    @test last(s) === 10002
    @test first(s) === 1
    @test length(s) === 9
    @test pop!(s) === 10002
    @test_throws KeyError pop!(s, -1)
    @test length(s) === 8
    @test popfirst!(s) === 1
    @test length(s) === 7
    @test !in(0,s)
    @test !in(1,s)
    @test in(2,s)
    @test !in(10002,s)
    @test in(10000,s)
    @test in(10000.0,s)
    @test !in(10002.0,s)
    @test_throws ArgumentError first(BitSet())
    @test_throws ArgumentError last(BitSet())
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
    @test hash(BitSet([1])) != hash(BitSet([17]))
    @test hash(BitSet([1])) != hash(BitSet([33]))
    @test hash(BitSet([1])) != hash(BitSet([65]))
    @test hash(BitSet([1])) != hash(BitSet([129]))
    # test with a different internal structure
    s = BitSet([129])
    pop!(push!(s, 65), 65)
    @test hash(BitSet([1])) != hash(s)

    @test !(-1 in BitSet(1:10))
end

# # issue #8570
# This requires 2^29 bytes of storage, which is too much for a simple test
# s = BitSet(typemax(Int32))
# @test length(s) === 1
# for b in s; b; end

@testset "union!, symdiff!" begin
    i = BitSet([1, 2, 3])
    union!(i, [1, 2])
    @test length(i) === 3
    union!(i, [3, 4, 5])
    @test length(i) === 5

    @test_throws KeyError pop!(i, 10)

    empty!(i)
    @test length(i) === 0

    @test symdiff!(i, -3) == BitSet([-3])
    @test symdiff!(i, -3) == BitSet([])

    @test symdiff!(i, 3) == BitSet([3])
    @test symdiff!(i, 257) == BitSet([3, 257])
    @test symdiff!(i, [3, 6]) == BitSet([6, 257])

    i = BitSet(1:6)
    @test symdiff!(i, BitSet([6, 513])) == BitSet([1:5; 513])

    @test 0 ∈ symdiff!(BitSet(rand(1:100, 30)), 0)
    @test BitSet(0:2:4) ⊆ symdiff!(BitSet(rand(5:100, 30)), [0, 2, 4])

    # issue #23557 :
    @test_throws MethodError symdiff!(BitSet([1]), ['a']) # should no stack-overflow
    @test_throws MethodError symdiff!(BitSet([1, 2]),  [[1]]) # should not return BitSet([2])
end

@testset "copy, copy!, empty" begin
    s1 = BitSet([1,2,3])
    s2 = empty(s1)
    copy!(s2, s1)
    s3 = copy(s2)
    @test s3 == s2 == s1
    @test collect(s3) == collect(s2) == [1,2,3]
end

@testset "push!, union" begin
    i = BitSet([1, 2, 3])
    j = union(i)
    @test j == i
    @test !(j === i)

    j = BitSet([4, 5, 6])
    @test union(i, j) == BitSet(1:6)

    k = BitSet([7, 8, 9])
    @test union(i, j, k) == BitSet(1:9)
    i = BitSet([1, 2, 3])
    j = union(i)
    @test j == i
    @test !(j === i)

    j = BitSet([4, 5, 6])
    @test union(i, j) == BitSet(1:6)

    k = BitSet([7, 8, 9])
    @test union(i, j, k) == BitSet(1:9)

    s1 = BitSet()
    @test push!(s1, -1) == BitSet([-1])
    push!(s1, -10, 1, 10, 100, 1000)
    @test collect(s1) == [-10, -1, 1, 10, 100, 1000]
    push!(s1, 606)
    @test collect(s1) == [-10, -1, 1, 10, 100, 606, 1000]

    s2 = BitSet()
    @test s2 === union!(s2, s1)
    s3 = BitSet([-1, 1, 10, 100])
    union!(s3, [-10, 1, 606, 1000])
    s4 = union(BitSet([-1, 1, 100, 1000]), BitSet([-10, 10, 100, 606]))
    @test s1 == s2 == s3 == s4
end

@testset "pop!, delete!" begin
    s = BitSet(1:2:10)
    # deleting non-positive values should be no-op
    # (Issue #23179 : delete!(s, 0) should not crash)
    len = length(s)
    for n in -20:0
        @test length(delete!(s, n)) == len
    end
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
    @test popfirst!(s) === 1
    @test popfirst!(s) === 3
    @test collect(s) == [5]
    empty!(s)
    @test isempty(s)
end

@testset "intersect" begin
    i = BitSet([1, 2, 3])
    j = BitSet([4, 5, 6])

    @test intersect(i) == i
    @test !(intersect(i) === i)
    @test intersect(i, j) == BitSet([])
    push!(j, 257)
    @test intersect(i, j) == BitSet([])
    push!(j, 2, 3, 17)
    @test intersect(i, j) == BitSet([2, 3])
    k = BitSet([1, 2, 3, 4, 5, 6, 7])
    @test intersect(i, j, k) == BitSet([2, 3])

    @test isempty(intersect(BitSet()))
    @test isempty(intersect(BitSet(1:10), BitSet()))
    @test isempty(intersect(BitSet(), BitSet(1:10)))

    @test intersect(BitSet([1,2,3])) == BitSet([1,2,3])
    @test intersect(BitSet(1:7), BitSet(3:10)) ==
    	  intersect(BitSet(3:10), BitSet(1:7)) == BitSet(3:7)
    @test intersect(BitSet(1:10), BitSet(1:4), 1:5, [2,3,10]) == BitSet([2,3])
end

@testset "setdiff, symdiff" begin
    @test setdiff(BitSet([1, 2, 3, 4]), BitSet([2, 4, 5, 6])) == BitSet([1, 3])
    @test symdiff(BitSet([1, 2, 3, 4]), BitSet([2, 4, 5, 6])) == BitSet([1, 3, 5, 6])

    s2 = BitSet([1, 2, 3, 4])
    setdiff!(s2, BitSet([2, 4, 5, 6]))
    @test s2 == BitSet([1, 3])

    s1 = BitSet(1:100)
    setdiff!(s1, BitSet(1:2:100))
    s2 = setdiff(BitSet(1:100), BitSet(1:2:100))
    @test s1 == s2 == BitSet(2:2:100)
    @test collect(s1) == collect(2:2:100)

    # issue #23191 : these tests should not segfault
    @test setdiff(s1, 0) == s1
    @test setdiff(s1, -9:0) == s1

    @test symdiff(BitSet([1, 2, 3, 4]), BitSet([2, 4, 5, 6])) ==
          symdiff(BitSet([2, 4, 5, 6]), BitSet([1, 2, 3, 4])) ==
          symdiff(BitSet([1, 2, 3, 4]), [2, 4, 5, 6]) ==
          symdiff(BitSet([2, 4, 5, 6]), [1, 2, 3, 4]) == BitSet([1, 3, 5, 6])
end

@testset "subsets, equality" begin
    i = BitSet([1, 2, 3])
    k = BitSet([4, 5])
    copy!(k, i)
    @test k == i
    @test !(k === i)
    copy!(k, k)
    @test k == i

    i = BitSet([1, 2, 3])
    j = BitSet([1, 2, 4])
    @test i != j

    push!(j, 257)
    pop!(j, 257)
    @test i != j
    @test j != i

    @test issubset(BitSet([1, 2, 4]), BitSet(1:10))
    @test issubset(BitSet([]), BitSet([]))
    @test BitSet([1, 2, 4]) < BitSet(1:10)
    @test !(BitSet([]) < BitSet([]))
    @test BitSet([1, 2, 4]) <= BitSet(1:10)
    @test BitSet([1, 2, 4]) <= BitSet([1, 2, 4])
    @test BitSet([]) <= BitSet([])

    @test BitSet(2:2:10) < BitSet(1:10)
    @test !(BitSet(2:2:10) < BitSet(2:2:10))
    @test BitSet(2:2:10) <= BitSet(2:10)
    @test BitSet(2:2:10) <= BitSet(2:2:10)

    # == with last-bit set (groups.google.com/forum/#!topic/julia-users/vZNjiIEG_sY)
    s = BitSet(255)
    @test s == s
end

@testset "setlike" begin
    p = BitSet([1,2,5,6])
    q = BitSet([1,3,5,7])
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
    s = BitSet()
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
    @test s == BitSet([1, 2, 100])
    push!(s, 1000)
    @test [i for i in s] == [1, 2, 100, 1000]
    @test pop!(s) === 1000
    @test s == BitSet([1, 2, 100])
    @test hash(s) === hash(BitSet([1, 2, 100]))

    b = 1:1000
    s = BitSet(b)
    @test collect(s) == collect(b)
    @test length(s) === length(b)
    @test pop!(s, 100) === 100
    @test collect(s) == [1:99; 101:1000]
    @test_throws KeyError pop!(s, 100)
    @test_throws KeyError pop!(s, 0)
    @test pop!(s, 100, 0) === 0
    @test pop!(s, 99, 0) === 99
end

@testset "unsigned overflow" begin
    @test BitSet(UInt8(2^8-1)) == BitSet(2^8-1)
    @test [x for x in BitSet(UInt8(2^8-1))] == [UInt8(2^8-1)]
    @test BitSet(UInt16(2^16-1)) == BitSet(2^16-1)
    @test [x for x in BitSet(UInt16(2^16-1))] == [UInt16(2^16-1)]
end

@testset "order" begin
    a = rand(1:1000, 100)
    s = BitSet(a)
    m, M = extrema(s)
    @test m == first(s) == minimum(s) == minimum(a)
    @test M == last(s)  == maximum(s) == maximum(a)
    @test issorted(s)
end

@testset "extreme values" begin
    @test pop!(BitSet(typemin(Int))) == typemin(Int)
    @test pop!(BitSet(typemax(Int))) == typemax(Int)
end

@testset "sizehint! returns a BitSet" begin
    # see #25029
    @test sizehint!(BitSet(), 100) isa BitSet
    # TODO: test that we don't delegate sizehint! to the underlying bits
    # field without dividing by 64 (i.e. the 100 above should allocate
    # only 2 UInt64 words
end
