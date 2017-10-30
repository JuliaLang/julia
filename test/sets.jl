# This file is a part of Julia. License is MIT: https://julialang.org/license

# Set tests
isdefined(Main, :TestHelpers) || @eval Main include("TestHelpers.jl")
using Main.TestHelpers.OAs

@testset "Construction, collect" begin
    @test ===(typeof(Set([1,2,3])), Set{Int})
    @test ===(typeof(Set{Int}([3])), Set{Int})
    data_in = (1,"banana", ())
    s = Set(data_in)
    data_out = collect(s)
    @test ===(typeof(data_out), Array{Any,1})
    @test all(map(d->in(d,data_out), data_in))
    @test length(data_out) == length(data_in)
    let f17741 = x -> x < 0 ? false : 1
        @test isa(Set(x for x = 1:3), Set{Int})
        @test isa(Set(sin(x) for x = 1:3), Set{Float64})
        @test isa(Set(f17741(x) for x = 1:3), Set{Int})
        @test isa(Set(f17741(x) for x = -1:1), Set{Integer})
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
end
@testset "isequal" begin
    @test  isequal(Set(), Set())
    @test !isequal(Set(), Set([1]))
    @test  isequal(Set{Any}(Any[1,2]), Set{Int}([1,2]))
    @test !isequal(Set{Any}(Any[1,2]), Set{Int}([1,2,3]))
    # Comparison of unrelated types seems rather inconsistent
    @test  isequal(Set{Int}(), Set{AbstractString}())
    @test !isequal(Set{Int}(), Set{AbstractString}([""]))
    @test !isequal(Set{AbstractString}(), Set{Int}([0]))
    @test !isequal(Set{Int}([1]), Set{AbstractString}())
    @test  isequal(Set{Any}([1,2,3]), Set{Int}([1,2,3]))
    @test  isequal(Set{Int}([1,2,3]), Set{Any}([1,2,3]))
    @test !isequal(Set{Any}([1,2,3]), Set{Int}([1,2,3,4]))
    @test !isequal(Set{Int}([1,2,3]), Set{Any}([1,2,3,4]))
    @test !isequal(Set{Any}([1,2,3,4]), Set{Int}([1,2,3]))
    @test !isequal(Set{Int}([1,2,3,4]), Set{Any}([1,2,3]))
end
@testset "eltype, similar" begin
    s1 = similar(Set([1,"hello"]))
    @test isequal(s1, Set())
    @test ===(eltype(s1), Any)
    s2 = similar(Set{Float32}([2.0f0,3.0f0,4.0f0]))
    @test isequal(s2, Set())
    @test ===(eltype(s2), Float32)
    s3 = similar(Set([1,"hello"]),Float32)
    @test isequal(s3, Set())
    @test ===(eltype(s3), Float32)
end
@testset "show" begin
    @test sprint(show, Set()) == "Set{Any}()"
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
    @test pop!(s,1,:foo) == :foo
    @test length(delete!(s,2)) == 1
    @test !in(1,s)
    @test !in(2,s)
    @test pop!(s) == 3
    @test length(s) == 0
    @test isempty(s)
    @test_throws ArgumentError pop!(s)
    @test length(Set(['x',120])) == 2
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
@testset "sizehint, empty" begin
    s = Set([1])
    @test isequal(sizehint!(s, 10), Set([1]))
    @test isequal(empty!(s), Set())
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
    @test isequal(union(Set([1])),Set([1]))
    s = ∪(Set([1,2]), Set([3,4]))
    @test isequal(s, Set([1,2,3,4]))
    s = union(Set([5,6,7,8]), Set([7,8,9]))
    @test isequal(s, Set([5,6,7,8,9]))
    s = Set([1,3,5,7])
    union!(s,(2,3,4,5))
    @test isequal(s,Set([1,2,3,4,5,7]))
    @test ===(typeof(union(Set([1]), BitSet())), Set{Int})
    @test isequal(union(Set([1,2,3]), 2:4), Set([1,2,3,4]))
    @test isequal(union(Set([1,2,3]), [2,3,4]), Set([1,2,3,4]))
    @test isequal(union(Set([1,2,3]), [2,3,4], Set([5])), Set([1,2,3,4,5]))
end
@testset "intersect" begin
    @test isequal(intersect(Set([1])),Set([1]))
    s = ∩(Set([1,2]), Set([3,4]))
    @test isequal(s, Set())
    s = intersect(Set([5,6,7,8]), Set([7,8,9]))
    @test isequal(s, Set([7,8]))
    @test isequal(intersect(Set([2,3,1]), Set([4,2,3]), Set([5,4,3,2])), Set([2,3]))
    @test ===(typeof(intersect(Set([1]), BitSet())), Set{Int})
    @test isequal(intersect(Set([1,2,3]), 2:10), Set([2,3]))
    @test isequal(intersect(Set([1,2,3]), [2,3,4]), Set([2,3]))
    @test isequal(intersect(Set([1,2,3]), [2,3,4], 3:4), Set([3]))
end
@testset "setdiff" begin
    @test isequal(setdiff(Set([1,2,3]), Set()),        Set([1,2,3]))
    @test isequal(setdiff(Set([1,2,3]), Set([1])),     Set([2,3]))
    @test isequal(setdiff(Set([1,2,3]), Set([1,2])),   Set([3]))
    @test isequal(setdiff(Set([1,2,3]), Set([1,2,3])), Set())
    @test isequal(setdiff(Set([1,2,3]), Set([4])),     Set([1,2,3]))
    @test isequal(setdiff(Set([1,2,3]), Set([4,1])),   Set([2,3]))
    @test ===(typeof(setdiff(Set([1]), BitSet())), Set{Int})
    @test isequal(setdiff(Set([1,2,3]), 2:10), Set([1]))
    @test isequal(setdiff(Set([1,2,3]), [2,3,4]), Set([1]))
    @test_throws MethodError setdiff(Set([1,2,3]), Set([2,3,4]), Set([1]))
    s = Set([1,3,5,7])
    setdiff!(s,(3,5))
    @test isequal(s,Set([1,7]))
    s = Set([1,2,3,4])
    setdiff!(s, Set([2,4,5,6]))
    @test isequal(s,Set([1,3]))
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
@testset "issubset, symdiff" begin
    for (l,r) in ((Set([1,2]),     Set([3,4])),
                  (Set([5,6,7,8]), Set([7,8,9])),
                  (Set([1,2]),     Set([3,4])),
                  (Set([5,6,7,8]), Set([7,8,9])),
                  (Set([1,2,3]),   Set()),
                  (Set([1,2,3]),   Set([1])),
                  (Set([1,2,3]),   Set([1,2])),
                  (Set([1,2,3]),   Set([1,2,3])),
                  (Set([1,2,3]),   Set([4])),
                  (Set([1,2,3]),   Set([4,1])))
        @test issubset(intersect(l,r), l)
        @test issubset(intersect(l,r), r)
        @test issubset(l, union(l,r))
        @test issubset(r, union(l,r))
        @test isequal(union(intersect(l,r),symdiff(l,r)), union(l,r))
    end
    @test ⊆(Set([1]), Set([1,2]))
    @test ⊊(Set([1]), Set([1,2]))
    @test !⊊(Set([1]), Set([1]))
    @test ⊈(Set([1]), Set([2]))
    @test ⊇(Set([1,2]), Set([1]))
    @test ⊋(Set([1,2]), Set([1]))
    @test !⊋(Set([1]), Set([1]))
    @test ⊉(Set([1]), Set([2]))
    @test symdiff(Set([1,2,3,4]), Set([2,4,5,6])) == Set([1,3,5,6])
end
@testset "unique" begin
    u = unique([1, 1, 2])
    @test in(1, u)
    @test in(2, u)
    @test length(u) == 2
    @test unique(iseven, [5, 1, 8, 9, 3, 4, 10, 7, 2, 6]) == [5, 8]
    @test unique(n -> n % 3, [5, 1, 8, 9, 3, 4, 10, 7, 2, 6]) == [5, 1, 9]
end

@testset "issue 20105" begin
    @test @inferred(unique(x for x in 1:1)) == [1]
    @test unique(x for x in Any[1, 1.0])::Vector{Real} == [1]
    @test unique(x for x in Real[1, 1.0])::Vector{Real} == [1]
    @test unique(Integer[1, 1, 2])::Vector{Integer} == [1, 2]
end

@testset "unique!" begin
    u = [1,1,3,2,1]
    unique!(u)
    @test u == [1,3,2]
    @test unique!([]) == []
    @test unique!(Float64[]) == Float64[]
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
end

@testset "allunique" begin
    @test allunique([])
    @test allunique(Set())
    @test allunique([1,2,3])
    @test allunique([:a,:b,:c])
    @test allunique(Set([1,2,3]))
    @test !allunique([1,1,2])
    @test !allunique([:a,:b,:c,:a])
    @test allunique(4:7)
    @test allunique(1:1)
    @test allunique(4.0:0.3:7.0)
    @test allunique(4:-1:5)       # empty range
    @test allunique(7:-1:1)       # negative step
end
@testset "filter" begin
    s = Set([1,2,3,4])
    @test isequal(filter(isodd,s), Set([1,3]))
    filter!(isodd, s)
    @test isequal(s, Set([1,3]))
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
