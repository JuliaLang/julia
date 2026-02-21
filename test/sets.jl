# This file is a part of Julia. License is MIT: https://julialang.org/license

# Set tests
isdefined(Main, :OffsetArrays) || @eval Main include("testhelpers/OffsetArrays.jl")
using .Main.OffsetArrays

using Dates

@testset "Construction, collect" begin
    @test Set([1,2,3]) isa Set{Int}
    @test Set{Int}([3]) isa Set{Int}
    data_in = (1,"banana", ())
    s = Set(data_in)
    data_out = collect(s)
    @test s isa Set{Any}
    @test data_out isa Array{Any,1}
    @test all(map(in(data_out), data_in))
    @test length(data_out) == length(data_in)
    let f17741 = x -> x < 0 ? false : 1
        @test isa(Set(x for x = 1:3), Set{Int})
        @test isa(Set(x for x = 1:3 for j = 1:1), Set{Int})
        @test isa(Set(sin(x) for x = 1:3), Set{Float64})
        @test isa(Set(f17741(x) for x = 1:3), Set{Int})
        @test isa(Set(f17741(x) for x = -1:1), Set{Integer})
        @test isa(Set(f17741(x) for x = 1:0), Set{Integer})
    end
    let s1 = Set(["foo", "bar"]), s2 = Set(s1)
        @test s1 == s2
        x = pop!(s1)
        @test s1 != s2
        @test !(x in s1)
        @test x in s2
        push!(s1, "baz")
        push!(s2, "baz2")
        @test "baz" in s1
        @test !("baz" in s2)
        @test !("baz2" in s1)
        @test "baz2" in s2
    end
end

@testset "hash" begin
    s1 = Set(["bar", "foo"])
    s2 = Set(["foo", "bar"])
    s3 = Set(["baz"])
    @test hash(s1) == hash(s2)
    @test hash(s1) != hash(s3)
    d1 = Dict(Set([3]) => 33, Set([2]) => 22)
    d2 = Dict(Set([2]) => 33, Set([3]) => 22)
    @test hash(d1) != hash(d2)
end

@testset "equality" for eq in (isequal, ==)
    @test  eq(Set(), Set())
    @test !eq(Set(), Set([1]))
    @test  eq(Set{Any}(Any[1,2]), Set{Int}([1,2]))
    @test !eq(Set{Any}(Any[1,2]), Set{Int}([1,2,3]))

    # Comparison of unrelated types
    @test  eq(Set{Int}(), Set{AbstractString}())
    @test !eq(Set{Int}(), Set{AbstractString}([""]))
    @test !eq(Set{AbstractString}(), Set{Int}([0]))
    @test !eq(Set{Int}([1]), Set{AbstractString}())
    @test  eq(Set{Any}([1,2,3]), Set{Int}([1,2,3]))
    @test  eq(Set{Int}([1,2,3]), Set{Any}([1,2,3]))
    @test !eq(Set{Any}([1,2,3]), Set{Int}([1,2,3,4]))
    @test !eq(Set{Int}([1,2,3]), Set{Any}([1,2,3,4]))
    @test !eq(Set{Any}([1,2,3,4]), Set{Int}([1,2,3]))
    @test !eq(Set{Int}([1,2,3,4]), Set{Any}([1,2,3]))

    # Special cases
    @test  eq(Set([-0.0]), Set([-0.0]))
    @test !eq(Set([0.0]), Set([-0.0]))
    @test  eq(Set([NaN]), Set([NaN]))
    @test !eq(Set([NaN]), Set([1.0]))
    @test  eq(Set([missing]), Set([missing]))
    @test !eq(Set([missing]), Set([1]))
end

@testset "hash and == for Set/BitSet" begin
    for s = (Set([1]), Set(1:10), Set(-100:7:100))
        b = BitSet(s)
        @test hash(s) == hash(b)
        @test s == b
    end
end

@testset "eltype, empty" begin
    s1 = empty(Set([1,"hello"]))
    @test isequal(s1, Set())
    @test ===(eltype(s1), Any)
    s2 = empty(Set{Float32}([2.0f0,3.0f0,4.0f0]))
    @test isequal(s2, Set())
    @test ===(eltype(s2), Float32)
    s3 = empty(Set([1,"hello"]),Float32)
    @test isequal(s3, Set())
    @test ===(eltype(s3), Float32)
end
@testset "show" begin
    @test sprint(show, Set()) == "Set{Any}()"
    @test repr([Set(),Set()]) == "Set{Any}[Set(), Set()]"
    @test sprint(show, Set(['a'])) == "Set(['a'])"
end
@testset "isempty, length, in, push, pop, delete" begin
    # also test for no duplicates
    s = Set(); push!(s,1); push!(s,2); push!(s,3)
    @test !isempty(s)
    @test in(1,s)
    @test in(2,s)
    @test length(s) == 3
    push!(s,1); push!(s,2); push!(s,3)
    @test length(s) == 3
    @test pop!(s,1) == 1
    @test !in(1,s)
    @test in(2,s)
    @test length(s) == 2
    @test_throws KeyError pop!(s,1)
    @test pop!(s,1,:foo) === :foo
    @test length(delete!(s,2)) == 1
    @test !in(1,s)
    @test !in(2,s)
    @test pop!(s) == 3
    @test length(s) == 0
    @test isempty(s)
    @test_throws ArgumentError pop!(s)
    @test length(Set(['x',120])) == 2

    # Test that pop! returns the element in the set, not the query
    s = Set{Any}(Any[0x01, UInt(2), 3, 4.0])
    @test pop!(s, 1) === 0x01
    @test pop!(s, 2) === UInt(2)
    @test pop!(s, 3) === 3
    @test pop!(s, 4) === 4.0
    @test_throws KeyError pop!(s, 5)
end

@testset "in!" begin
    s = Set()
    @test !(in!(0x01, s))
    @test !(in!(Int32(2), s))
    @test in!(1, s)
    @test in!(2.0, s)
    (a, b, c...) = sort!(collect(s))
    @test a === 0x01
    @test b === Int32(2)
    @test isempty(c)

    # in! will convert to the right type automatically
    s = Set{Int32}()
    @test !(in!(1, s))
    @test only(s) === Int32(1)
    @test_throws Exception in!("hello", s)

    # Other set types
    s = BitSet()
    @test !(in!(13, s))
    @test in!(UInt16(13), s)
    @test only(s) === 13
end

@testset "copy" begin
    data_in = (1,2,9,8,4)
    s = Set(data_in)
    c = copy(s)
    @test isequal(s,c)
    v = pop!(s)
    @test !in(v,s)
    @test  in(v,c)
    push!(s,100)
    push!(c,200)
    @test !in(100,c)
    @test !in(200,s)
end

@testset "copy(::KeySet) (issue #41537)" begin
    @test union(keys(Dict(1=>2, 3=>4))) == copy(keys(Dict(1=>2, 3=>4))) == Set([1,3])
end

@testset "copy!" begin
    for S = (Set, BitSet)
        s = S([1, 2])
        for a = ([1], UInt[1], [3, 4, 5], UInt[3, 4, 5])
            @test s === copy!(s, Set(a)) == S(a)
            @test s === copy!(s, BitSet(a)) == S(a)
        end
    end
    s = Set([1, 2])
    s2 = copy(s)
    @test copy!(s, s) == s2
end

@testset "sizehint, empty" begin
    s = Set([1])
    @test isequal(sizehint!(s, 10), Set([1]))
    @test isequal(empty!(s), Set())
    s2 = GenericSet(s)
    sizehint!(s2, 10)
    @test s2 == GenericSet(s)
end

@testset "shrinking" begin # Similar test as for the underlying Dict
    d = Set(i for i = 1:1000)
    filter!(x -> x < 10, d)
    sizehint!(d, 10)
    @test length(d.dict.slots) < 100
    sizehint!(d, 1000)
    sizehint!(d, 1; shrink = false)
    @test length(d.dict.slots) >= 1000
    sizehint!(d, 1; shrink = true)
    @test length(d.dict.slots) < 1000
end

@testset "rehash!" begin
    # Use a pointer type to have defined behavior for uninitialized
    # array element
    s = Set(["a", "b", "c"])
    Base.rehash!(s)
    k = s.dict.keys
    Base.rehash!(s)
    @test length(k) == length(s.dict.keys)
    for i in 1:length(k)
        if isassigned(k, i)
            @test k[i] == s.dict.keys[i]
        else
            @test !isassigned(s.dict.keys, i)
        end
    end
    @test s == Set(["a", "b", "c"])
end

@testset "start, done, next" begin
    for data_in in ((7, 8, 4, 5),
                  ("hello", 23, 2.7, (), [], (1, 8)))
        local data_in, s, t
        s = Set(data_in)

        s_new = Set()
        for el in s
            push!(s_new, el)
        end
        @test isequal(s, s_new)

        t = tuple(s...)
        @test length(t) == length(s)
        for e in t
            @test in(e,s)
        end
    end
end

@testset "union" begin
    for S in (Set, BitSet, Vector)
        s = ∪(S([1,2]), S([3,4]))
        @test s == S([1,2,3,4])
        s = union(S([5,6,7,8]), S([7,8,9]))
        @test s == S([5,6,7,8,9])
        s = S([1,3,5,7])
        union!(s, (2,3,4,5))
        @test s == S([1,3,5,7,2,4]) # order matters for Vector
        let s1 = S([1, 2, 3])
            @test s1 !== union(s1) == s1
            @test s1 !== union(s1, 2:4) == S([1,2,3,4])
            @test s1 !== union(s1, [2,3,4]) == S([1,2,3,4])
            @test s1 !== union(s1, [2,3,4], S([5])) == S([1,2,3,4,5])
            @test s1 === union!(s1, [2,3,4], S([5])) == S([1,2,3,4,5])
        end
    end
    @test union(Set([1]), BitSet()) isa Set{Int}
    @test union(BitSet([1]), Set()) isa BitSet
    @test union([1], BitSet()) isa Vector{Int}
    # union must uniquify
    @test union([1, 2, 1]) == union!([1, 2, 1]) == [1, 2]
    @test union([1, 2, 1], [2, 2]) == union!([1, 2, 1], [2, 2]) == [1, 2]
    s2 = Set([nothing])
    union!(s2, [nothing])
    @test s2 == Set([nothing])

    @testset "promotion" begin
        ints = [1:5, [1, 2], Set([1, 2])]
        floats = [2:0.1:3, [2.0, 3.5], Set([2.0, 3.5])]

        for a in ints, b in floats
            @test eltype(union(a, b)) == Float64
            @test eltype(union(b, a)) == Float64
        end
    end
end

@testset "intersect" begin
    for S in (Set, BitSet, Vector)
        s = S([1,2]) ∩ S([3,4])
        @test s == S()
        s = intersect(S([5,6,7,8]), S([7,8,9]))
        slong = S(collect(3:63))
        # test #36339 length/order short-cut
        @test intersect(S([5,6,7,8]), slong) == intersect(slong, S([5,6,7,8]))
        @test s == S([7,8])
        @test intersect(S([2,3,1]), S([4,2,3]), S([5,4,3,2])) == S([2,3])
        let s1 = S([1,2,3])
            @test s1 !== intersect(s1) == s1
            @test s1 !== intersect(s1, 2:10) == S([2,3])
            @test s1 !== intersect(s1, [2,3,4]) == S([2,3])
            @test s1 !== intersect(s1, [2,3,4], 3:4) == S([3])
            @test s1 === intersect!(s1, [2,3,4], 3:4) == S([3])
        end
    end
    @test intersect(Set([1]), BitSet()) isa Set{Int}
    @test intersect(BitSet([1]), Set()) isa Set{Any}
    @test intersect(BitSet([1]), Set([1])) isa BitSet
    @test intersect(BitSet([1]), Set([1]), Set([1])) isa BitSet
    @test intersect([1], BitSet()) isa Vector{Int}
    # intersect must uniquify
    @test intersect([1, 2, 1]) == intersect!([1, 2, 1]) == [1, 2]
    @test intersect([1, 2, 1], [2, 2]) == intersect!([1, 2, 1], [2, 2]) == [2]

    # issue #25801
    x = () ∩ (:something,)
    y = () ∩ (42,)
    @test isempty(x)
    @test isempty(y)

    # Discussed in PR#41769
    @testset "promotion" begin
        ints = [1:5, [1, 2], Set([1, 2])]
        floats = [2:0.1:3, [2.0, 3.5], Set([2.0, 3.5])]

        for a in ints, b in floats
            @test eltype(intersect(a, b)) == Float64
            @test eltype(intersect(b, a)) == Float64
            @test eltype(intersect(a, a, b)) == Float64
        end
    end

    # 3-argument version is correctly covered
    @test intersect(Set([1,2]), Set([2]), Set([1,2,3])) == Set([2])
    @test intersect(Set([1,2]), Set([2]), Set([1.,2,3])) == Set([2.])
end

@testset "setdiff" begin
    for S in (Set, BitSet, Vector)
        @test setdiff(S([1,2,3]), S())        == S([1,2,3])
        @test setdiff(S([1,2,3]), S([1]))     == S([2,3])
        @test setdiff(S([1,2,3]), S([1,2]))   == S([3])
        @test setdiff(S([1,2,3]), S([1,2,3])) == S()
        @test setdiff(S([1,2,3]), S([4]))     == S([1,2,3])
        @test setdiff(S([1,2,3]), S([4,1]))   == S([2,3])
        let s1 = S([1, 2, 3])
            @test s1 !== setdiff(s1) == s1
            @test s1 !== setdiff(s1, 2:10) == S([1])
            @test s1 !== setdiff(s1, [2,3,4]) == S([1])
            @test s1 !== setdiff(s1, S([2,3,4]), S([1])) == S()
            @test s1 === setdiff!(s1, S([2,3,4]), S([1])) == S()
        end
    end

    @test setdiff(Set([1]), BitSet()) isa Set{Int}
    @test setdiff(BitSet([1]), Set()) isa BitSet
    @test setdiff([1], BitSet()) isa Vector{Int}
    # setdiff must uniquify
    @test setdiff([1, 2, 1]) == setdiff!([1, 2, 1]) == [1, 2]
    @test setdiff([1, 2, 1], [2, 2]) == setdiff!([1, 2, 1], [2, 2]) == [1]

    s = Set([1,3,5,7])
    setdiff!(s,(3,5))
    @test isequal(s,Set([1,7]))
    s = Set([1,2,3,4])
    setdiff!(s, Set([2,4,5,6]))
    @test isequal(s,Set([1,3]))

    # setdiff iterates the shorter set - make sure this algorithm works
    sa, sb = Set([1,2,3,4,5,6,7]), Set([2,3,9])
    @test Set([1,4,5,6,7]) == setdiff(sa, sb) !== sa
    @test Set([1,4,5,6,7]) == setdiff!(sa, sb) === sa
    sa, sb = Set([1,2,3,4,5,6,7]), Set([2,3,9])
    @test Set([9]) == setdiff(sb, sa) !== sb
    @test Set([9]) == setdiff!(sb, sa) === sb
end

@testset "ordering" begin
    @test Set() < Set([1])
    @test Set([1]) < Set([1,2])
    @test !(Set([3]) < Set([1,2]))
    @test !(Set([3]) > Set([1,2]))
    @test Set([1,2,3]) > Set([1,2])
    @test !(Set([3]) <= Set([1,2]))
    @test !(Set([3]) >= Set([1,2]))
    @test Set([1]) <= Set([1,2])
    @test Set([1,2]) <= Set([1,2])
    @test Set([1,2]) >= Set([1,2])
    @test Set([1,2,3]) >= Set([1,2])
    @test !(Set([1,2,3]) >= Set([1,2,4]))
    @test !(Set([1,2,3]) <= Set([1,2,4]))
end

@testset "issubset, symdiff, isdisjoint" begin
    for S in (Set, BitSet, Vector)
        for (l,r) in ((S([1,2]),     S([3,4])),
                      (S([5,6,7,8]), S([7,8,9])),
                      (S([1,2]),     S([3,4])),
                      (S([5,6,7,8]), S([7,8,9])),
                      (S([1,2,3]),   S()),
                      (S(),          S()),
                      (S(),          S([1,2,3])),
                      (S([1,2,3]),   S([1])),
                      (S([1,2,3]),   S([1,2])),
                      (S([1,2,3]),   S([1,2,3])),
                      (S([1,2,3]),   S([4])),
                      (S([1,2,3]),   S([4,1])))
            @test issubset(intersect(l,r), l)
            @test issubset(intersect(l,r), r)
            @test issubset(l, union(l,r))
            @test issubset(r, union(l,r))
            @test issubset(union(l,r))(r)
            @test isdisjoint(l,l) == isempty(l)
            @test isdisjoint(l)(l) == isempty(l)
            @test isdisjoint(l,r) == isempty(intersect(l,r))
            if S === Vector
                @test sort(union(intersect(l,r),symdiff(l,r))) == sort(union(l,r))
            else
                @test union(intersect(l,r),symdiff(l,r)) == union(l,r)
            end
        end
        if S !== Vector
            @test ⊆(S([1]), S([1,2]))
            @test ⊊(S([1]), S([1,2]))
            @test !⊊(S([1]), S([1]))
            @test ⊈(S([1]), S([2]))
            @test ⊇(S([1,2]), S([1]))
            @test ⊋(S([1,2]), S([1]))
            @test !⊋(S([1]), S([1]))
            @test ⊉(S([1]), S([2]))

            @test ⊆(S([1,2]))(S([1]))
            @test ⊊(S([1,2]))(S([1]))
            @test !⊊(S([1]))(S([1]))
            @test ⊈(S([2]))(S([1]))
            @test ⊇(S([1]))(S([1,2]))
            @test ⊋(S([1]))(S([1,2]))
            @test !⊋(S([1]))(S([1]))
            @test ⊉(S([2]))(S([1]))
        end
        let s1 = S([1,2,3,4])
            @test s1 !== symdiff(s1) == s1
            @test s1 !== symdiff(s1, S([2,4,5,6])) == S([1,3,5,6])
            @test s1 !== symdiff(s1, S([2,4,5,6]), [1,6,7]) == S([3,5,7])
            @test s1 === symdiff!(s1, S([2,4,5,6]), [1,6,7]) == S([3,5,7])
        end
    end
    @test symdiff(Set([1,2,3,4]), Set([2,4,5,6])) == Set([1,3,5,6])
    @test symdiff(Set([1]), BitSet()) isa Set{Int}
    @test symdiff(BitSet([1]), Set{Int}()) isa BitSet
    @test symdiff([1], BitSet()) isa Vector{Int}
    #symdiff does uniquify
    @test symdiff([1, 2, 1]) == symdiff!([1, 2, 1]) == [1,2]
    @test symdiff([1, 2, 1], [2, 2]) == symdiff!([1, 2, 1], [2, 2]) == [1]
    @test symdiff([1, 2, 1], [2, 2]) == symdiff!([1, 2, 1], [2, 2]) == [1]

    # Base.hasfastin
    @test all(Base.hasfastin, Any[Dict(1=>2), Set(1), BitSet(1), 1:9, 1:2:9,
                                  Dict, Set, BitSet, UnitRange, StepRange])
    @test !any(Base.hasfastin, Any[[1, 2, 3], "123",
                                   Array, String])

    # tests for Dict
    d1 = Dict(1=>nothing, 2=>nothing)
    d2 = Dict(1=>nothing, 3=>nothing)
    d3 = Dict(1=>nothing, 2=>nothing, 3=>nothing)
    @test d3 == merge(d1, d2)
    @test !issubset(d1, d2)
    @test !issubset(d2, d1)
    @test !issubset(d3, d1)
    @test !issubset(d3, d2)
    @test issubset(d1, d3)
    @test issubset(d2, d3)

    # no fast in, long enough container
    @test issubset(Set(Bool[]), rand(Bool, 100)) == true
    # neither has a fast in, right doesn't have a length
    @test isdisjoint([1, 3, 5, 7, 9], Iterators.filter(iseven, 1:10))

    # range fast-path
    for (truth, a, b) in (
                   # Integers
                   (true, 1:10, 11:20), # not overlapping
                   (false, 1:10, 5:20), # partial overlap
                   (false, 5:9, 1:10), # complete overlap
                   # complete overlap, unequal steps
                   (false, 3:6:60, 9:9:60),
                   (true, 4:6:60, 9:9:60),
                   (true, 0:6:12, 9:9:60),
                   (false, 6:6:18, 9:9:60),
                   (false, 12:6:18, 9:9:60),
                   (false, 18:6:18, 9:9:60),
                   (true, 1:2:3, 2:3:5),
                   (true, 1:4:5, 2:1:4),
                   (false, 4:12:124, 1:1:8),
                   # potential overflow
                   (false, 0x1:0x3:0x4, 0x4:0x3:0x4),
                   (true, 0x3:0x3:0x6, 0x4:0x3:0x4),
                   (false, typemax(Int8):Int8(3):typemax(Int8), typemin(Int8):Int8(3):typemax(Int8)),
                   # Chars
                   (true, 'a':'l', 'o':'p'), # not overlapping
                   (false, 'a':'l', 'h':'p'), # partial overlap
                   (false, 'a':'l', 'c':'e'), # complete overlap
                   # Floats
                   (true, 1.:10., 11.:20.), # not overlapping
                   (false, 1.:10., 5.:20.), # partial overlap
                   (false, 5.:9., 1.:10.), # complete overlap
                   # Inputs that may hang
                   (false, -6011687643038262928:3545293653953105048, -6446834672754204848:3271267329311042532),
                   )
        @test isdisjoint(a, b) == truth
        @test isdisjoint(b, a) == truth
        @test isdisjoint(a, reverse(b)) == truth
        @test isdisjoint(reverse(a), b) == truth
        @test isdisjoint(b, reverse(a)) == truth
        @test isdisjoint(reverse(b), a) == truth
    end
    @test isdisjoint(10:9, 1:10) # empty range
    @test !isdisjoint(1e-100:.1:1, 0:.1:1)
    @test !isdisjoint(eps()/4:.1:.71, 0:.1:1)
end

@testset "unique" begin
    u = @inferred(unique([1, 1, 2]))
    @test in(1, u)
    @test in(2, u)
    @test length(u) == 2
    @test unique(iseven, []) == []
    # type promotion
    @test unique(x -> x^2, [1, 3.]) == [1, 3.]
    @test @inferred(unique(iseven, [5, 1, 8, 9, 3, 4, 10, 7, 2, 6])) == [5, 8]
    @test @inferred(unique(x->x^2, Integer[3, -4, 5, 4])) == Integer[3, -4, 5]
    @test @inferred(unique(iseven, Integer[3, -4, 5, 4]; seen=Set{Bool}())) == Integer[3, -4]
    @test @inferred(unique(n -> n % 3, [5, 1, 8, 9, 3, 4, 10, 7, 2, 6])) == [5, 1, 9]
    for r = (Base.OneTo(-1), Base.OneTo(0), Base.OneTo(1), Base.OneTo(5),
             1:0, 1:1, 1:2, 1:10, 1:.5:.5, 1:.5:1, 1:.5:10, 3:-2:5, 3:-2:3, 3:-2:1,
             StepRangeLen(1.0, 2.0, 0), StepRangeLen(1.0, 2.0, 2), StepRangeLen(1.0, 2.0, 3),
             StepRangeLen(1.0, 0.0, 0), StepRangeLen(1.0, -0.0, 1), StepRangeLen(1.0, 0.0, 2),
             LinRange(1, 2, 3), LinRange(1, 1, 0), LinRange(1, 1, 1), LinRange(1, 1, 10))
        @test @inferred(unique(r)) == invoke(unique, Tuple{Any}, r)
    end
end

@testset "issue 20105" begin
    @test @inferred(unique(x for x in 1:1)) == [1]
    @test unique(x for x in Any[1, 1.0])::Vector{Real} == [1]
    @test unique(x for x in Real[1, 1.0])::Vector{Real} == [1]
    @test @inferred(unique(Integer[1, 1, 2]))::Vector{Integer} == [1, 2]
    @test unique(x for x in []) isa Vector{Any}
end

@testset "unique!" begin
    u = []
    @test unique!(u) === u
    u = [1,1,3,2,1]
    @inferred(unique!(u))
    @test u == [1,3,2]
    @test @inferred(unique!([])) == []
    @test @inferred(unique!(Float64[])) == Float64[]
    u = [1,2,2,3,5,5]
    @test unique!(u) === u
    @test u == [1,2,3,5]
    u = [6,5,5,3,3,2,1]
    @test unique!(u) === u
    @test u == [6,5,3,2,1]
    u = OffsetArray([1,2,2,3,5,5], -1)
    @test unique!(u) === u
    @test u == OffsetArray([1,2,3,5], -1)
    u = OffsetArray([5,5,4,4,2,2,0,-1,-1], -1)
    @test unique!(u) === u
    @test u == OffsetArray([5,4,2,0,-1], -1)
    u = OffsetArray(["w","we","w",5,"r",5,5], -1)
    @test unique!(u) === u
    @test u == OffsetArray(["w","we",5,"r"], -1)
    u = [0.0,-0.0,1.0,2]
    @test unique!(u) === u
    @test u == [0.0,-0.0,1.0,2.0]
    u = [1,NaN,NaN,3]
    @test unique!(u) === u
    @test u[1] == 1
    @test isnan(u[2])
    @test u[3] == 3
    u = [5,"w","we","w","r",5,"w"]
    unique!(u)
    @test u == [5,"w","we","r"]
    u = [1,2,5,1,3,2]
    @test unique!(x -> x ^ 2, [1, -1, 3, -3, 5, -5]) == [1, 3, 5]
    @test unique!(n -> n % 3, [5, 1, 8, 9, 3, 4, 10, 7, 2, 6]) == [5, 1, 9]
    @test @inferred(unique!(iseven, [2, 3, 5, 7, 9])) == [2, 3]
    @test @inferred(unique!(x -> x % 2 == 0 ? :even : :odd, [1, 2, 3, 4, 2, 2, 1])) == [1, 2]
    @test @inferred(unique!(x -> x % 2 == 0 ? :even : "odd", [1, 2, 3, 4, 2, 2, 1]; seen=Set{Union{Symbol,String}}())) == [1, 2]

    @test isempty(unique!(Union{}[]))
    @test eltype(unique!([i for i in ["1"] if i isa Int])) <: Union{}
end

@testset "allunique" begin
    @test allunique([])
    @test allunique(Set())
    @test allunique([1,2,3])
    @test allunique([1 2; 3 4])
    @test allunique([:a,:b,:c])
    @test allunique(Set([1,2,3]))
    @test !allunique([1,1,2])
    @test !allunique([:a,:b,:c,:a])
    @test allunique(unique(randn(100)))  # longer than 32
    @test allunique(collect(1:100)) # sorted/unique && longer than 32
    @test allunique(collect(100:-1:1)) # sorted/unique && longer than 32
    @test !allunique(fill(1,100)) # sorted/repeating && longer than 32
    @test allunique(collect('A':'z')) # 58-element Vector{Char}
    @test !allunique(repeat(1:99, 1, 2))
    @test !allunique(vcat(pi, randn(1998), pi))  # longer than 1000
    @test allunique(eachrow(hcat(1:10, 1:10)))
    @test allunique(x for x in 'A':'Z' if randn()>0)
    @test !allunique(x for x in repeat(1:2000, 3) if true)
    @test allunique([0.0, -0.0])
    @test allunique(x for x in [0.0, -0.0] if true)
    @test !allunique([NaN, NaN])
    @test !allunique(x for x in [NaN, NaN] if true)
    # ranges
    @test allunique(4:7)
    @test allunique(1:1)
    @test allunique(4.0:0.3:7.0)
    @test allunique(4:-1:5)       # empty range
    @test allunique(7:-1:1)       # negative step
    @test allunique(Date(2018, 8, 7):Day(1):Date(2018, 8, 11))  # JuliaCon 2018
    @test allunique(DateTime(2018, 8, 7):Hour(1):DateTime(2018, 8, 11))
    @test allunique(('a':1:'c')[1:2]) == true
    @test allunique(collect(1:1001))
    for r = (Base.OneTo(-1), Base.OneTo(0), Base.OneTo(1), Base.OneTo(5),
             1:0, 1:1, 1:2, 1:10, 1:.5:.5, 1:.5:1, 1:.5:10, 3:-2:5, 3:-2:3, 3:-2:1,
             StepRangeLen(1.0, 2.0, 0), StepRangeLen(1.0, 2.0, 2), StepRangeLen(1.0, 2.0, 3),
             StepRangeLen(1.0, 0.0, 0), StepRangeLen(1.0, -0.0, 1), StepRangeLen(1.0, 0.0, 2),
             LinRange(1, 2, 3), LinRange(1, 1, 0), LinRange(1, 1, 1), LinRange(1, 1, 10))
        @test allunique(r) == invoke(allunique, Tuple{Any}, r)
    end
    # tuples
    @test allunique(())
    @test allunique((1,2,3))
    @test allunique(ntuple(identity, 40))
    @test !allunique((1,2,3,4,3))
    @test allunique((0.0, -0.0))
    @test !allunique((NaN, NaN))
    # Known length 1, need not evaluate:
    @test allunique(error(x) for x in [1])
    # @test_opt allunique(Int[])
end

@testset "allunique(f, xs)" begin
    @test allunique(sin, 1:3)
    @test !allunique(sin, [1,2,3,1])
    @test allunique(sin, (1, 2, pi, im))  # eltype Any
    @test allunique(abs2, 1:100)
    @test !allunique(abs, -10:10)
    @test allunique(abs2, Vector{Any}(1:100))
    # These cases don't call the function at all:
    @test allunique(error, [])
    @test allunique(error, [1])
end

@testset "allequal" begin
    # sets & dictionaries
    @test allequal(Set())
    @test allequal(Set(1))
    @test !allequal(Set([1, 2]))
    @test allequal(Dict())
    @test allequal(Dict(:a => 1))
    @test !allequal(Dict(:a => 1, :b => 2))
    # vectors
    @test allequal([])
    @test allequal([1])
    @test allequal([1, 1])
    @test !allequal([1, 1, 2])
    @test allequal([:a, :a])
    @test !allequal([:a, :b])
    # ranges
    @test !allequal(1:2)
    @test allequal(1:1)
    @test !allequal(4.0:0.3:7.0)
    @test allequal(4:-1:5)       # empty range
    @test !allequal(7:-1:1)       # negative step
    @test !allequal(Date(2018, 8, 7):Day(1):Date(2018, 8, 11))  # JuliaCon 2018
    @test !allequal(DateTime(2018, 8, 7):Hour(1):DateTime(2018, 8, 11))
    @test allequal(StepRangeLen(1.0, 0.0, 2))
    @test !allequal(StepRangeLen(1.0, 1.0, 2))
    @test allequal(LinRange(1, 1, 0))
    @test allequal(LinRange(1, 1, 1))
    @test allequal(LinRange(1, 1, 2))
    @test !allequal(LinRange(1, 2, 2))
    # Known length 1, need not evaluate:
    @test allequal(error(x) for x in [1])
    # Empty, but !haslength:
    @test allequal(error(x) for x in 1:3 if false)
end

@testset "allequal(f, xs)" begin
    @test allequal(abs2, [3, -3])
    @test allequal(x -> 1, rand(3))
    @test !allequal(x -> rand(), [1,1,1])
    # tuples
    @test allequal(abs2, (3, -3))
    @test allequal(x -> 1, Tuple(rand(3)))
    @test !allequal(x -> rand(), (1,1,1))
    # These cases don't call the function at all:
    @test allequal(error, [])
    @test allequal(error, ())
    @test allequal(error, (x for x in 1:3 if false))
    @test allequal(error, [1])
    @test allequal(error, (1,))
end

@testset "filter(f, ::$S)" for S = (Set, BitSet)
    s = S([1,2,3,4])
    @test s !== filter( isodd, s) == S([1,3])
    @test s === filter!(isodd, s) == S([1,3])
end
@testset "first" begin
    @test_throws ArgumentError first(Set())
    @test first(Set(2)) == 2
end
@testset "pop!" begin
    s = Set(1:5)
    @test 2 in s
    @test pop!(s, 2) == 2
    @test !(2 in s)
    @test_throws KeyError pop!(s, 2)
    @test pop!(s, 2, ()) == ()
    @test 3 in s
    @test pop!(s, 3, ()) == 3
    @test !(3 in s)
    @test pop!(Set(1:2), 2, nothing) == 2
end

@testset "convert" begin
    iset = Set([17, 4711])
    cfset = convert(Set{Float64}, iset)
    @test typeof(cfset) == Set{Float64}
    @test cfset == iset
    fset = Set([17.0, 4711.0])
    ciset = convert(Set{Int}, fset)
    @test typeof(ciset) == Set{Int}
    @test ciset == fset
    ssset = Set(split("foo bar"))
    cssset = convert(Set{String}, ssset)
    @test typeof(cssset) == Set{String}
    @test cssset == Set(["foo", "bar"])
end

@testset "fuzzy testing Set & BitSet" begin
    b1, b2 = rand(-1000:1000, 2)
    e1 = rand(b1-9:1000) # -9 to have an empty list sometimes
    e2 = rand(b2-9:1000)
    l1, l2 = rand(1:1000, 2)
    a1 = b1 <= e1 ? rand(b1:e1, l1) : Int[]
    a2 = b2 <= e2 ? rand(b2:e2, l2) : Int[]
    s1, s2 = Set(a1), Set(a2)
    t1, t2 = BitSet(a1), BitSet(a2)

    for (s, t) = ((s1, t1), (s2, t2))
        @test length(s) == length(t)
        @test issubset(s, t)
        @test issubset(t, s)
        @test isempty(s) == isempty(t)
        isempty(s) && continue
        @test maximum(s) == maximum(t)
        @test minimum(s) == minimum(t)
        @test extrema(s) == extrema(t)
        rs, rt = rand(s), rand(t)
        @test rs in s
        @test rt in s
        @test rs in t
        @test rt in t
        for y in (rs, rt)
            ss = copy(s)
            tt = copy(t)
            pop!(ss, y)
            pop!(tt, y)
            @test BitSet(ss) == tt
            @test Set(tt) == ss
            z = rand(1001:1100) # z ∉ s or t
            push!(ss, z)
            push!(tt, z)
            @test BitSet(ss) == tt
            @test Set(tt) == ss
        end
    end

    res = Dict{String,Union{Bool,Vector{Int}}}()
    function check(desc, val)
        n = val isa Bool ? val : sort!(collect(val))
        r = get!(res, desc, n)
        if n isa Bool || r !== n
            @test r == n
        end
    end
    asbitset(x) = x isa BitSet ? x : BitSet(x)
    asset(x) = x isa Set ? x : Set(x)

    for x1 = (s1, t1), x2 = (s2, t2)
        check("union", union(x1, x2))
        check("intersect", intersect(x1, x2))
        check("symdiff", symdiff(x1, x2))
        check("setdiff", setdiff(x1, x2))
        check("== as Bitset", asbitset(x1) == asbitset(x2))
        check("== as Set", asset(x1) == asset(x2))
        check("issubset", issubset(x1, x2))
        if typeof(x1) == typeof(x2)
            check("<", x1 < x2)
            check("<=", x1 > x2)
            check("union!", union!(copy(x1), x2))
            check("setdiff!", setdiff!(copy(x1), x2))
            x1 isa Set && continue
            check("intersect!", intersect!(copy(x1), x2))
            check("symdiff!", symdiff!(copy(x1), x2))
        end
    end
end

@testset "replace! & replace" begin
    a = [1, 2, 3, 1]
    @test replace(x -> iseven(x) ? 2x : x, a) == [1, 4, 3, 1]
    @test replace(x -> iseven(x) ? 2x : x, Tuple(a)) === (1, 4, 3, 1)
    @test replace!(x -> iseven(x) ? 2x : x, a) === a
    @test a == [1, 4, 3, 1]
    @test replace(a, 1=>0) == [0, 4, 3, 0]
    @test replace(Tuple(a), 1=>0) === (0, 4, 3, 0)
    for count = (1, 0x1, big(1))
        @test replace(a, 1=>0, count=count) == [0, 4, 3, 1]
        @test replace(Tuple(a), 1=>0, count=count) === (0, 4, 3, 1)
    end
    @test replace!(a, 1=>2) === a
    @test a == [2, 4, 3, 2]
    @test replace!(x->2x, a, count=0x2) == [4, 8, 3, 2]

    d = Dict(1=>2, 3=>4)
    @test replace!(x -> x.first > 2 ? x.first=>2*x.second : x, d) === d
    @test d == Dict(1=>2, 3=>8)
    @test replace(d, (3=>8)=>(0=>0)) == Dict(1=>2, 0=>0)
    @test replace!(d, (3=>8)=>(2=>2)) === d
    @test d == Dict(1=>2, 2=>2)

    s = Set([1, 2, 3])
    @test replace(x -> x > 1 ? 2x : x, s) == Set([1, 4, 6])
    for count = (1, 0x1, big(1))
        @test replace(x -> x > 1 ? 2x : x, s, count=count) in [Set([1, 4, 3]), Set([1, 2, 6])]
    end
    @test replace(s, 1=>4) == Set([2, 3, 4])
    @test replace!(s, 1=>2) === s
    @test s == Set([2, 3])
    @test replace!(x->2x, s, count=0x1) in [Set([4, 3]), Set([2, 6])]

    for count = (0, 0x0, big(0)) # count == 0 --> no replacements
        @test replace([1, 2], 1=>0, 2=>0; count) == [1, 2]
        @test replace((1, 2), 1=>0, 2=>0; count) === (1, 2)
        for dict = (Dict(1=>2, 2=>3), IdDict(1=>2, 2=>3))
            @test replace(dict, (1=>2) => (1=>3); count) == dict
        end
        @test replace(Set([1, 2]), 2=>-1; count) == Set([1, 2])
    end

    # test collisions with AbstractSet/AbstractDict
    @test replace!(x->2x, Set([3, 6])) == Set([6, 12])
    @test replace!(x->2x, Set([1:20;])) == Set([2:2:40;])
    @test replace!(kv -> (2kv[1] => kv[2]), Dict(1=>2, 2=>4, 4=>8, 8=>16)) == Dict(2=>2, 4=>4, 8=>8, 16=>16)

    # avoid recursive call issue #25384
    @test_throws MethodError replace!("")

    # test eltype promotion
    x = @inferred replace([1, 2], 2=>2.5)
    @test x == [1, 2.5] && x isa Vector{Float64}

    x = @inferred replace([1, 2], 2=>missing)
    @test isequal(x, [1, missing]) && x isa Vector{Union{Int, Missing}}

    x = @inferred replace([1, missing], missing=>2)
    @test x == [1, 2] && x isa Vector{Int}
    x = @inferred replace([1, missing], missing=>2, count=1)
    @test x == [1, 2] && x isa Vector{Union{Int, Missing}}
    x = @inferred replace([1, missing], missing=>missing)
    @test isequal(x, [1, missing]) && x isa Vector{Union{Int, Missing}}
    x = @inferred replace([1, missing], missing=>2, 1=>missing)
    @test isequal(x, [missing, 2]) && x isa Vector{Union{Int, Missing}}

    # eltype promotion for dicts
    d = Dict(1=>2, 3=>4)
    f = replace(d, (1=>2) => (1=>nothing))
    @test f == Dict(3=>4, 1=>nothing)
    @test eltype(f) == Pair{Int, Union{Nothing, Int}}
    f = replace(d, (1=>2) => (1=>missing), (3=>4)=>(3=>missing))
    @test valtype(f) == Union{Missing,Int}
    f = replace(d, (1=>2) => (1=>'a'), (3=>4)=>(3=>'b'))
    @test valtype(f) == Any
    @test f == Dict(3=>'b', 1=>'a')

    # eltype promotion for sets
    s = Set([1, 2, 3])
    f = replace(s, 2=>missing, 3=>nothing)
    @test f == Set([1, missing, nothing])
    @test eltype(f) == Union{Int,Missing,Nothing}
    f = replace(s, 2=>'a')
    @test eltype(f) == Any
    @test f == Set([1, 3, 'a'])

    # test that isequal is used
    @test replace([NaN, 1.0], NaN=>0.0) == [0.0, 1.0]
    @test replace((NaN, 1.0), NaN=>0.0) === (0.0, 1.0)
    @test replace([1, missing], missing=>0) == [1, 0]
    @test replace((1, missing), missing=>0) === (1, 0)

    # test that MethodError is thrown for pairs
    @test_throws MethodError replace(identity, 1=>2)
    @test_throws MethodError replace(identity, 1=>2, 3=>4)
    @test_throws MethodError replace!(identity, 1=>2)
    @test_throws MethodError replace!(identity, 1=>2, 3=>4)

    # test replace and friends for AbstractDicts
    d1 = GenericDict(Dict(1=>2, 3=>4))
    d2 = replace(d1, (1=>2) => (1=>"a"))
    @test d2 == Dict(1=>"a", 3=>4)
    @test d2 isa Dict{Int, Any}
    @test d1 === replace!(d1, (1=>2) => (1=>-2))
    @test d1 == Dict(1=>-2, 3=>4)

    dd = Dict(1=>2, 3=>1, 5=>1, 7=>1)
    for d1 in (dd, GenericDict(dd))
        @test replace(d1, (1=>2) => (1=>"a"), count=0) == d1
        d2 = replace(kv->(kv[2] == 1 ? kv[1]=>2 : kv), d1, count=2)
        @test count(==(2), values(d2)) == 3
        @test count(==(1), values(d2)) == 1
    end
end

@testset "⊆, ⊊, ⊈, ⊇, ⊋, ⊉, <, <=, issetequal" begin
    a = [2, 1, 2]
    b = [2, 3, 1, 3]
    ua = unique(a)
    ub = unique(b)
    for TA in (Tuple, identity, Set, BitSet, IdSet{Int}),
        TB in (Tuple, identity, Set, BitSet, IdSet{Int}),
        uA = false:true,
        uB = false:true
        A = TA(uA ? ua : a)
        B = TB(uB ? ub : b)
        @test A ⊆ B
        @test A ⊊ B
        @test !(A ⊈ B)
        @test !(A ⊇ B)
        @test !(A ⊋ B)
        @test A ⊉ B
        @test !(B ⊆ A)
        @test !(B ⊊ A)
        @test B ⊈ A
        @test B ⊇ A
        @test B ⊋ A
        @test !(B ⊉ A)
        @test !issetequal(A, B)
        @test !issetequal(B, A)
        @test !issetequal(B)(A)
        @test !issetequal(A)(B)
        for T = (Tuple, identity, Set, BitSet, IdSet{Int})
            @test issetequal(A, T(A))
            @test issetequal(B, T(B))
        end
        if A isa AbstractSet && B isa AbstractSet
            @test A <= B
            @test A <  B
            @test !(A >= B)
            @test !(A >  B)
            @test !(B <= A)
            @test !(B <  A)
            @test B >= A
            @test B >  A
        end
    end
    # first doesn't have length
    @test issetequal(Iterators.filter(iseven, 1:10), [2, 4, 6, 8, 10])
    # both don't have length
    @test issetequal(Iterators.filter(iseven, 1:10), Iterators.filter(iseven, 1:10))
end

@testset "optimized union! with max_values" begin
    # issue #30315
    T = Union{Nothing, Bool}
    @test Base.max_values(T) == 3
    d = Set{T}()
    union!(d, (nothing, true, false))
    @test length(d) == 3
    @test d == Set((nothing, true, false))
    @test nothing in d
    @test true    in d
    @test false   in d

    for X = (Int8, Int16, Int32, Int64)
        @test Base.max_values(Union{Nothing, X}) == (sizeof(X) < sizeof(Int) ?
                                                     2^(8*sizeof(X)) + 1 :
                                                     typemax(Int))
    end
    # this does not account for non-empty intersections of the unioned types
    @test Base.max_values(Union{Int8,Int16}) == 2^8 + 2^16
end

struct OpenInterval{T}
    lower::T
    upper::T
end
Base.in(x, i::OpenInterval) = i.lower < x < i.upper
Base.IteratorSize(::Type{<:OpenInterval}) = Base.SizeUnknown()

@testset "Continuous sets" begin
    i = OpenInterval(2, 4)
    @test 3 ∈ i
    @test issubset(3, i)
end

@testset "IdSet" begin
    a = [1]
    b = [2]
    c = [3]
    d = [4]
    e = [5]
    A = IdSet{Vector{Int}}([a, b, c, d])
    @test !isempty(A)
    B = copy(A)
    @test A ⊆ B
    @test B ⊆ A
    A = filter!(x->isodd(x[1]), A)
    @test A ⊆ B
    @test !(B ⊆ A)
    @test !isempty(A)
    a_ = pop!(A, a)
    @test a_ === a
    @test !isempty(A)
    e_ = pop!(A, a, e)
    @test e_ === e
    @test !isempty(A)
    A = empty!(A)
    @test isempty(A)
    @test isnothing(sizehint!(A, 10))
    @test Base.copymutable(A) == copy(A)
end

@testset "⊊, ⊋" begin
    @test !((1, 2) ⊊ (1, 2, 2))
    @test !((1, 2, 2) ⊋ (1, 2))
end

@testset "AbstractSet & Fallback" begin
    mutable struct TestSet{T} <: AbstractSet{T}
        set::Set{T}
        function TestSet{T}() where T
            new{T}(Set{T}())
        end
    end
    set = TestSet{Any}()
    @test sizehint!(set, 1) === set
    @test sizehint!(set, 1; shrink = true) === set
    @test sizehint!(set, 1; shrink = false) === set
end
