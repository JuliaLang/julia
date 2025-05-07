# This file is a part of Julia. License is MIT: https://julialang.org/license

module SortingTests

using Base.Order
using Random
using Test

isdefined(Main, :OffsetArrays) || @eval Main include("testhelpers/OffsetArrays.jl")
using .Main.OffsetArrays

@testset "Base.Sort docstrings" begin
    undoc = Docs.undocumented_names(Base.Sort)
    @test_broken isempty(undoc)
    @test undoc == [:Algorithm, :SMALL_THRESHOLD, :Sort]
end

@testset "Order" begin
    @test Forward == ForwardOrdering()
    @test ReverseOrdering(Forward) == ReverseOrdering() == Reverse
end

@testset "midpoint" begin
    @test Base.Sort.midpoint(1, 3) === 2
    @test Base.Sort.midpoint(2, 4) === 3
    @test 2 <= Base.Sort.midpoint(1, 4) <= 3
    @test Base.Sort.midpoint(-3, -1) === -2
    @test Base.Sort.midpoint(-4, -2) === -3
    @test -3 <= Base.Sort.midpoint(-4, -1) <= -2
    @test Base.Sort.midpoint(-1, 1) ===  0
    @test -1 <= Base.Sort.midpoint(-2, 1) <= 0
    @test 0 <= Base.Sort.midpoint(-1, 2) <= 1
    @test Base.Sort.midpoint(-2, 2) ===  0
    @test Base.Sort.midpoint(typemax(Int)-2, typemax(Int)) === typemax(Int)-1
    @test Base.Sort.midpoint(typemin(Int), typemin(Int)+2) === typemin(Int)+1
    @test -1 <= Base.Sort.midpoint(typemin(Int), typemax(Int)) <= 0
end

@testset "sort" begin
    @test sort([2,3,1]) == [1,2,3] == sort([2,3,1]; order=Forward)
    @test sort([2,3,1], rev=true) == [3,2,1] == sort([2,3,1], order=Reverse)
    @test sort(['z':-1:'a';]) == ['a':'z';]
    @test sort(['a':'z';], rev=true) == ['z':-1:'a';]
    @test sort(OffsetVector([3,1,2], -2)) == OffsetVector([1,2,3], -2)
    @test sort(OffsetVector([3.0,1.0,2.0], 2), rev=true) == OffsetVector([3.0,2.0,1.0], 2)
end

@testset "sortperm" begin
    @test sortperm([2,3,1]) == [3,1,2]
    @test sortperm!([1,2,3], [2,3,1]) == [3,1,2]
    let s = view([1,2,3,4], 1:3),
        r = sortperm!(s, [2,3,1])
        @test r == [3,1,2]
        @test r === s
    end
    @test_throws ArgumentError sortperm!(view([1, 2, 3, 4], 1:4), [2, 3, 1])
    @test sortperm(OffsetVector([8.0, -2.0, 0.5], -4)) == OffsetVector([-2, -1, -3], -4)
    @test sortperm!(Int32[1, 2], [2.0, 1.0]) == Int32[2, 1]
    @test_throws ArgumentError sortperm!(Int32[1, 2], [2.0, 1.0]; dims=1)
    let A = rand(4, 4, 4)
        for dims = 1:3
            perm = sortperm(A; dims)
            sorted = sort(A; dims)
            @test A[perm] == sorted

            perm_idx = similar(Array{Int}, axes(A))
            sortperm!(perm_idx, A; dims)
            @test perm_idx == perm
        end
    end
    @test_throws ArgumentError sortperm!(zeros(Int, 3, 3), rand(3, 3);)
    @test_throws ArgumentError sortperm!(zeros(Int, 3, 3), rand(3, 3); dims=3)
    @test_throws ArgumentError sortperm!(zeros(Int, 3, 4), rand(4, 4); dims=1)
    @test_throws ArgumentError sortperm!(OffsetArray(zeros(Int, 4, 4), -4:-1, 1:4), rand(4, 4); dims=1)
end

@testset "misc sorting" begin
    @test !issorted([2,3,1])
    @test issorted([1,2,3])
    @test reverse([2,3,1]) == [1,3,2]
    @test sum(randperm(6)) == 21
    @test length(reverse(0x1:0x2)) == 2
    @test issorted(sort(rand(UInt64(1):UInt64(2), 7); rev=true); rev=true) # issue #43034
    @test sort(Union{}[]) == Union{}[] # issue #45280
end

@testset "stability" begin
    for Alg in [InsertionSort, MergeSort, Base.Sort.ScratchQuickSort(), Base.DEFAULT_STABLE,
            Base.Sort.ScratchQuickSort(missing, 1729), Base.Sort.ScratchQuickSort(1729, missing)]
        @test issorted(sort(1:2000, alg=Alg, by=x->0))
        @test issorted(sort(1:2000, alg=Alg, by=x->x÷100))
    end
    @test sort(1:2000, by=x->x÷100, rev=true) == sort(1:2000, by=x->-x÷100) ==
        vcat(2000, (x:x+99 for x in 1900:-100:100)..., 1:99)
end

function tuple_sort_test(x)
    @test issorted(sort(x))
    length(x) > 9 && return # length > 9 uses a vector fallback
    @test 0 == @allocated sort(x)
end
@testset "sort(::NTuple)" begin
    @test sort(()) == ()
    @test sort((9,8,3,3,6,2,0,8)) == (0,2,3,3,6,8,8,9)
    @test sort((9,8,3,3,6,2,0,8), by=x->x÷3) == (2,0,3,3,8,6,8,9)
    for i in 1:40
        tuple_sort_test(rand(NTuple{i, Float64}))
    end
    @test_throws MethodError sort((1,2,3.0))
    @test Base.infer_return_type(sort, Tuple{Tuple{Vararg{Int}}}) == Tuple{Vararg{Int}}
end

@testset "KeySet and ValueIterator" begin
    x = Dict(rand() => randstring() for _ in 1:10)
    x0 = deepcopy(x)
    @test issorted(sort(keys(x))::Vector{Float64})
    @test issorted(sort(values(x))::Vector{String})
    @test x == x0
end

@testset "partialsort" begin
    @test partialsort([3,6,30,1,9],3) == 6
    @test partialsort([3,6,30,1,9],3:4) == [6,9]
    @test partialsortperm([3,6,30,1,9], 3:4) == [2,5]
    @test partialsortperm!(Vector(1:5), [3,6,30,1,9], 3:4) == [2,5]
    let a=[1:10;]
        for r in Any[2:4, 1:2, 10:10, 4:2, 2:1, 4:-1:2, 2:-1:1, 10:-1:10, 4:1:3, 1:2:8, 10:-3:1, UInt(2):UInt(5)]
            @test partialsort(a, r) == [r;]
            @test partialsortperm(a, r) == [r;]
            @test partialsort(a, r, rev=true) == (11 .- [r;])
            @test partialsortperm(a, r, rev=true) == (11 .- [r;])
        end
        for i in (2, UInt(2), Int128(1), big(10))
            @test partialsort(a, i) == i
            @test partialsortperm(a, i) == i
            @test partialsort(a, i, rev=true) == (11 - i)
            @test partialsortperm(a, i, rev=true) == (11 - i)
        end
    end
    @test_throws ArgumentError partialsortperm!([1,2], [2,3,1], 1:2)
end

# exercise the codepath in searchsorted* methods for ranges that check for zero step range
struct ConstantRange{T} <: AbstractRange{T}
   val::T
   len::Int
end

Base.length(r::ConstantRange) = r.len
Base.getindex(r::ConstantRange, i::Int) = (1 <= i <= r.len || throw(BoundsError(r,i)); r.val)
Base.step(r::ConstantRange) = 0

@testset "searchsorted method with ranges which check for zero step range" begin
    r = ConstantRange(1, 5)

    @test searchsortedfirst(r, 1.0, Forward) == 1
    @test searchsortedfirst(r, 1, Forward) == 1
    @test searchsortedfirst(r, UInt(1), Forward) == 1

    @test searchsortedlast(r, 1.0, Forward) == 5
    @test searchsortedlast(r, 1, Forward) == 5
    @test searchsortedlast(r, UInt(1), Forward) == 5
end

@testset "Each sorting algorithm individually" begin
    a = rand(1:10000, 1000)
    for alg in [InsertionSort, MergeSort, QuickSort, Base.DEFAULT_STABLE, Base.DEFAULT_UNSTABLE]

        b = sort(a, alg=alg)
        @test issorted(b)

        ix = sortperm(a, alg=alg)
        b = a[ix]
        @test issorted(b)
        @test a[ix] == b

        sortperm!(view(ix, 1:100), view(a, 1:100), alg=alg)
        b = a[ix][1:100]
        @test issorted(b)

        sortperm!(ix, a, alg=alg)
        b = a[ix]
        @test issorted(b)
        @test a[ix] == b

        b = sort(a, alg=alg, rev=true)
        @test issorted(b, rev=true)
        ix = sortperm(a, alg=alg, rev=true)
        b = a[ix]
        @test issorted(b, rev=true)
        @test a[ix] == b

        sortperm!(view(ix, 1:100), view(a, 1:100), alg=alg, rev=true)
        b = a[ix][1:100]
        @test issorted(b, rev=true)

        sortperm!(ix, a, alg=alg, rev=true)
        b = a[ix]
        @test issorted(b, rev=true)
        @test a[ix] == b

        b = sort(a, alg=alg, by=x->1/x)
        @test issorted(b, by=x->1/x)
        ix = sortperm(a, alg=alg, by=x->1/x)
        b = a[ix]
        @test issorted(b, by=x->1/x)
        @test a[ix] == b

        sortperm!(view(ix, 1:100), view(a, 1:100), by=x->1/x)
        b = a[ix][1:100]
        @test issorted(b, by=x->1/x)

        sortperm!(ix, a, alg=alg, by=x->1/x)
        b = a[ix]
        @test issorted(b, by=x->1/x)
        @test a[ix] == b

        c = copy(a)
        permute!(c, ix)
        @test c == b

        invpermute!(c, ix)
        @test c == a

        c = sort(a, alg=alg, lt=(>))
        @test b == c

        c = sort(a, alg=alg, by=x->1/x)
        @test b == c
    end

    @testset "PartialQuickSort" begin
        b = sort(a)
        @test issorted(b)
        @test last(b) == last(sort(a, alg=PartialQuickSort(length(a))))
        b = sort(a, rev=true)
        @test issorted(b, rev=true)
        @test last(b) == last(sort(a, alg=PartialQuickSort(length(a)), rev=true))
        b = sort(a, by=x->1/x)
        @test issorted(b, by=x->1/x)
        @test last(b) == last(sort(a, alg=PartialQuickSort(length(a)), by=x->1/x))
    end
end
@testset "insorted" begin
    numTypes = [Int8,  Int16,  Int32,  Int64,  Int128,
                UInt8, UInt16, UInt32, UInt64, UInt128,
                Float16, Float32, Float64, BigInt, BigFloat]

    @test insorted(1, collect(1:10), by=(>=(5)))
    @test insorted(10, collect(1:10), by=(>=(5)))

    for R in numTypes, T in numTypes
        @test !insorted(T(0), R[1, 1, 2, 2, 3, 3])
        @test insorted(T(1), R[1, 1, 2, 2, 3, 3])
        @test insorted(T(2), R[1, 1, 2, 2, 3, 3])
        @test !insorted(T(4), R[1, 1, 2, 2, 3, 3])
        @test !insorted(2.5, R[1, 1, 2, 2, 3, 3])

        @test !insorted(T(0), 1:3)
        @test insorted(T(1), 1:3)
        @test insorted(T(2), 1:3)
        @test !insorted(T(4), 1:3)

        @test insorted(T(1), R.(collect(1:10)), by=(>=(5)))
        @test insorted(T(10), R.(collect(1:10)), by=(>=(5)))
    end

    for (rg,I) in Any[(49:57,47:59), (1:2:17,-1:19), (-3:0.5:2,-5:.5:4)]
        rg_r = reverse(rg)
        rgv, rgv_r = collect(rg), collect(rg_r)
        for i = I
            @test insorted(i,rg) === insorted(i,rgv)
            @test insorted(i,rg_r) === insorted(i,rgv_r,rev=true)
        end
    end

    rg = 0.0:0.01:1.0
    for i = 2:101
        @test insorted(rg[i], rg)
        @test !insorted(prevfloat(rg[i]), rg)
        @test !insorted(nextfloat(rg[i]), rg)
    end

    rg_r = reverse(rg)
    for i = 1:100
        @test insorted(rg_r[i], rg_r)
        @test !insorted(prevfloat(rg_r[i]), rg_r)
        @test !insorted(nextfloat(rg_r[i]), rg_r)
    end

    @test insorted(1, 1:10) == insorted(1, collect(1:10), by=(>=(5)))
    @test insorted(10, 1:10) == insorted(10, collect(1:10), by=(>=(5)))

    @test !insorted(0, [])
    @test !insorted(0, [1,2,3])
    @test !insorted(4, [1,2,3])
    @test insorted(3, [10,8,6,9,4,7,2,5,3,1], by=(x -> iseven(x) ? x+5 : x), rev=true)
end
@testset "PartialQuickSort" begin
    a = rand(1:10000, 1000)
    # test PartialQuickSort only does a partial sort
    let k = 1:div(length(a), 10)
        alg = PartialQuickSort(k)
        b = sort(a, alg=alg)
        c = sort(a, alg=alg, by=x->1/x)
        d = sort(a, alg=alg, rev=true)
        @test issorted(b[k])
        @test issorted(c[k], by=x->1/x)
        @test issorted(d[k], rev=true)
        @test !issorted(b)
        @test !issorted(c, by=x->1/x)
        @test !issorted(d, rev=true)
    end
    let k = div(length(a), 10)
        alg = PartialQuickSort(k)
        b = sort(a, alg=alg)
        c = sort(a, alg=alg, by=x->1/x)
        d = sort(a, alg=alg, rev=true)
        @test b[k] == sort(a)[k]
        @test c[k] == sort(a, by=x->1/x)[k]
        @test d[k] == sort(a, rev=true)[k]
        @test !issorted(b)
        @test !issorted(c, by=x->1/x)
        @test !issorted(d, rev=true)
    end

    @test partialsort([3,6,30,1,9], 2, rev=true) == 9
    @test partialsort([3,6,30,1,9], 2, by=x->1/x) == 9
    @test partialsortperm([3,6,30,1,9], 2, rev=true) == 5
    @test partialsortperm([3,6,30,1,9], 2, by=x->1/x) == 5
end

## more advanced sorting tests ##

randnans(n) = reinterpret(Float64,[rand(UInt64)|0x7ff8000000000000 for i=1:n])

function randn_with_nans(n,p)
    v = randn(n)
    x = findall(rand(n).<p)
    v[x] = randnans(length(x))
    return v
end

@testset "advanced sorting" begin
    Random.seed!(0xdeadbeef)
    for n in [0:10; 100; 101; 1000; 1001]
        local r
        r = -5:5
        v = rand(r,n)
        h = [sum(v .== x) for x in r]

        for rev in [false,true]
            # insertion sort (stable) as reference
            pi = sortperm(v, alg=InsertionSort, rev=rev)
            @test pi == sortperm(float(v), alg=InsertionSort, rev=rev)
            @test isperm(pi)
            si = v[pi]
            @test [sum(si .== x) for x in r] == h
            @test issorted(si, rev=rev)
            @test all(issorted,[pi[si.==x] for x in r])
            c = copy(v)
            permute!(c, pi)
            @test c == si
            invpermute!(c, pi)
            @test c == v

            # stable algorithms
            for alg in [MergeSort, Base.Sort.ScratchQuickSort(), Base.Sort.ScratchQuickSort(1:n), Base.DEFAULT_STABLE]
                p = sortperm(v, alg=alg, rev=rev)
                p2 = sortperm(float(v), alg=alg, rev=rev)
                @test p == p2
                @test p == pi
                s = copy(v)
                permute!(s, p)
                @test s == si
                invpermute!(s, p)
                @test s == v

                # Ensure stability, even with reverse short circuit
                @test all(sort!(Real[fill(2.0, 15); fill(2, 15); fill(1.0, 15); fill(1, 15)])
                           .=== Real[fill(1.0, 15); fill(1, 15); fill(2.0, 15); fill(2, 15)])
            end

            # unstable algorithms
            for alg in [QuickSort, PartialQuickSort(1:n), Base.DEFAULT_UNSTABLE]
                p = sortperm(v, alg=alg, rev=rev)
                p2 = sortperm(float(v), alg=alg, rev=rev)
                @test p == p2
                @test isperm(p)
                @test v[p] == si
                s = copy(v)
                permute!(s, p)
                @test s == si
                invpermute!(s, p)
                @test s == v
            end
            for alg in [PartialQuickSort(n)]
                p = sortperm(v, alg=alg, rev=rev)
                p2 = sortperm(float(v), alg=alg, rev=rev)
                if n == 0
                    @test isempty(p) && isempty(p2)
                else
                    @test p[n] == p2[n]
                    @test v[p][n] == si[n]
                    @test isperm(p)
                    s = copy(v)
                    permute!(s, p)
                    @test s[n] == si[n]
                    invpermute!(s, p)
                    @test s == v
                end
            end
        end

        v = randn_with_nans(n,0.1)
        for alg in [InsertionSort, MergeSort, Base.Sort.ScratchQuickSort(), Base.Sort.ScratchQuickSort(1, n), Base.DEFAULT_UNSTABLE, Base.DEFAULT_STABLE],
            rev in [false,true]
            alg === InsertionSort && n >= 3000 && continue
            # test float sorting with NaNs
            s = sort(v, alg=alg, rev=rev)
            @test issorted(s, rev=rev)
            @test reinterpret(UInt64,v[isnan.(v)]) == reinterpret(UInt64,s[isnan.(s)])

            # test float permutation with NaNs
            p = sortperm(v, alg=alg, rev=rev)
            @test isperm(p)
            vp = v[p]
            @test isequal(vp,s)
            @test reinterpret(UInt64,vp) == reinterpret(UInt64,s)
        end
    end
end

@testset "sortperm" begin
    inds = [
        1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8,8,9,9,9,10,
        10,10,11,11,11,12,12,12,13,13,13,14,14,14,15,15,15,16,16,
        16,17,17,17,18,18,18,19,19,19,20,20,20,21,21,22,22,22,23,
        23,24,24,24,25,25,25,26,26,26,27,27,27,28,28,28,29,29,29,
        30,30,30,31,31,32,32,32,33,33,33,34,34,34,35,35,35,36,36,
        36,37,37,37,38,38,38,39,39,39,40,40,40,41,41,41,42,42,42,
        43,43,43,44,44,44,45,45,45,46,46,46,47,47,47,48,48,48,49,
        49,49,50,50,50,51,51,52,52,52,53,53,53,54,54,54,55,55,55,
        56,56,56,57,57,57,58,58,58,59,60,60,60,61,61,61,62,62,63,
        64,64,64,65,65,65,66,66,66,67,67,67,68,68,69,69,69,70,70,
        70,71,71,71,72,72,72,73,73,73,74,75,75,75,76,76,76,77,77,
        77,78,78,78,79,79,79,80,80,80,81,81,82,82,82,83,83,83,84,
        84,84,85,85,85,86,86,86,87,87,87,88,88,88,89,89,89,90,90,
        90,91,91,91,92,92,93,93,93,94,94,94,95,95,95,96,96,96,97,
        97,98,98,98,99,99,99,100,100,100,101,101,101,102,102,102,
        103,103,103,104,105,105,105,106,106,106,107,107,107,108,
        108,108,109,109,109,110,110,110,111,111,111,112,112,112,
        113,113,113,114,114,115,115,115,116,116,116,117,117,117,
        118,118,118,119,119,119,120,120,120,121,121,121,122,122,
        122,123,123,123,124,124,124,125,125,125,126,126,126,127,
        127,127,128,128,128,129,129,129,130,130,130,131,131,131,
        132,132,132,133,133,133,134,134,134,135,135,135,136,136,
        136,137,137,137,138,138,138,139,139,139,140,140,140,141,
        141,142,142,142,143,143,143,144,144,144,145,145,145,146,
        146,146,147,147,147,148,148,148,149,149,149,150,150,150,
        151,151,151,152,152,152,153,153,153,154,154,154,155,155,
        155,156,156,156,157,157,157,158,158,158,159,159,159,160,
        160,160,161,161,161,162,162,162,163,163,163,164,164,164,
        165,165,165,166,166,166,167,167,167,168,168,168,169,169,
        169,170,170,170,171,171,171,172,172,172,173,173,173,174,
        174,174,175,175,175,176,176,176,177,177,177,178,178,178,
        179,179,179,180,180,180,181,181,181,182,182,182,183,183,
        183,184,184,184,185,185,185,186,186,186,187,187,187,188,
        188,188,189,189,189,190,190,190,191,191,191,192,192,192,
        193,193,193,194,194,194,195,195,195,196,196,197,197,197,
        198,198,198,199,199,199,200,200,200
    ]

    let sp = sortperm(inds)
        @test all(issorted, [sp[inds.==x] for x in 1:200])
    end

    for alg in [InsertionSort, MergeSort, QuickSort, Base.DEFAULT_STABLE]
        sp = sortperm(inds, alg=alg)
        @test all(issorted, [sp[inds.==x] for x in 1:200])
    end
end
@testset "issue #6177" begin
    @test sortperm([ 0.0, 1.0, 1.0], rev=true) == [2, 3, 1]
    @test sortperm([-0.0, 1.0, 1.0], rev=true) == [2, 3, 1]
    @test sortperm([-1.0, 1.0, 1.0], rev=true) == [2, 3, 1]
end
# issue #8825 - stability of min/max
mutable struct Twain
    a :: Int
    b :: Int
end
Base.isless(x :: Twain, y :: Twain) = x.a < y.a
let x = Twain(2,3), y = Twain(2,4)
    @test (min(x,y), max(x,y)) == (x,y) == minmax(x,y)
end

# issue #12833 - type stability of sort
@test Base.return_types(sort, (Vector{Int},)) == [Vector{Int}]

@testset "PR #18791" begin
    @test sort([typemax(Int),typemin(Int)]) == [typemin(Int),typemax(Int)]
    @test sort([typemax(UInt),0]) == [0,typemax(UInt)]
end
@testset "issue #19005" begin
    @test searchsortedfirst(0:256, 0x80) == 129
    @test searchsortedlast(0:256, 0x80) == 129
end
# https://discourse.julialang.org/t/sorting-big-int-with-v-0-6/1241
@test sort([big(3), big(2)]) == [big(2), big(3)]

@testset "issue #30763" begin
    for T in [:Int8, :Int16, :Int32, :Int64, :Int128, :UInt8, :UInt16, :UInt32, :UInt64, :UInt128]
        @eval begin
            struct T_30763{T}
                n::T
            end

            Base.zero(::T_30763{$T}) = T_30763{$T}(0)
            Base.convert(::Type{T_30763{$T}}, n::Integer) = T_30763{$T}($T(n))
            Base.isless(a::T_30763{$T}, b::T_30763{$T}) = isless(a.n, b.n)
            Base.:(-)(a::T_30763{$T}, b::T_30763{$T}) = T_30763{$T}(a.n - b.n)
            Base.:(+)(a::T_30763{$T}, b::T_30763{$T}) = T_30763{$T}(a.n + b.n)
            Base.:(*)(n::Integer, a::T_30763{$T}) = T_30763{$T}(n * a.n)
            Base.rem(a::T_30763{$T}, b::T_30763{$T}) = T_30763{$T}(rem(a.n, b.n))

            # The important part of this test is that the return type of length might be different from Int
            Base.length(r::StepRange{T_30763{$T},T_30763{$T}}) = $T((last(r).n - first(r).n) ÷ step(r).n)

            @test searchsorted(T_30763{$T}(1):T_30763{$T}(3), T_30763{$T}(2)) == 2:2
        end
    end
end

@testset "sorting of views with strange axes" for T in (Int, UInt, Int128, UInt128, BigInt)
    a = [8,6,7,5,3,0,9]
    b = @view a[T(2):T(5)]
    @test issorted(sort!(b))
    @test b == [3,5,6,7]
    @test a == [8,3,5,6,7,0,9]

    a = [8,6,7,5,3,0,9]
    b = @view a[T(2):T(5)]
    c = sort(b)
    @test issorted(c)
    @test c == [3,5,6,7]
    @test a == [8,6,7,5,3,0,9]

    a = [8,6,7,NaN,5,3,0,9]
    b = @view a[T(2):T(5)]
    @test issorted(sort!(b))
    @test isequal(b, [5,6,7,NaN])
    @test isequal(a, [8,5,6,7,NaN,3,0,9])

    a = [8,6,7,NaN,5,3,0,9]
    b = @view a[T(2):T(5)]
    c = sort(b)
    @test issorted(c)
    @test isequal(c, [5,6,7,NaN])
    @test isequal(a, [8,6,7,NaN,5,3,0,9])
end

@testset "sort!(::AbstractVector{<:Integer}) with short int range" begin
    a = view([9:-1:0;], :)::SubArray
    sort!(a)
    @test issorted(a)

    a = view([9:-1:0;], :)::SubArray
    Base.Sort._sort!(a, Base.Sort.CountingSort(), Base.Forward, (; mn=0, mx=9))  # test it supports non-Vector
    @test issorted(a)

    a = OffsetArray([9:-1:0;], -5)
    Base.Sort._sort!(a, Base.Sort.CountingSort(), Base.Forward, (; mn=0, mx=9))
    @test issorted(a)
end

@testset "sort!(::OffsetVector)" begin
    for length in vcat(0:5, [10, 300, 500, 1000])
        for offset in [-100000, -10, -1, 0, 1, 17, 1729]
            x = OffsetVector(rand(length), offset)
            sort!(x)
            @test issorted(x)
        end
    end
end

@testset "sort!(::OffsetMatrix; dims)" begin
    x = OffsetMatrix(rand(5,5), 5, -5)
    sort!(x; dims=1)
    for i in axes(x, 2)
        @test issorted(x[:,i])
    end
end

@testset "Offset with missing (#48862)" begin
    v = [-1.0, missing, 1.0, 0.0, missing, -0.5, 0.5, 1.0, -0.5, missing, 0.5, -0.8, 1.5, NaN]
    vo = OffsetArray(v, (firstindex(v):lastindex(v)).+100)
    @test issorted(sort!(vo))
    @test issorted(v)
end

@testset "searchsortedfirst/last with generalized indexing" begin
    o = OffsetVector(1:3, -2)
    @test searchsortedfirst(o, 4) == lastindex(o) + 1
    @test searchsortedfirst(o, 1.5) == 0
    @test searchsortedlast(o, 0) == firstindex(o) - 1
    @test searchsortedlast(o, 1.5) == -1

    # Issue #56457
    o2 = OffsetArray([2,2,3], typemax(Int)-3);
    @test searchsorted(o2, 2) == firstindex(o2):firstindex(o2)+1

    struct IdentityVector <: AbstractVector{Int}
        lo::Int
        hi::Int
    end
    function Base.getindex(s::IdentityVector, i::Int)
        s.lo <= i <= s.hi || throw(BoundsError(s, i))
        i
    end
    Base.axes(s::IdentityVector) = (s.lo:s.hi,)
    Base.size(s::IdentityVector) = length.(axes(s))

    o3 = IdentityVector(typemin(Int), typemin(Int)+5)
    @test searchsortedfirst(o3, typemin(Int)+2) === typemin(Int)+2
    @test searchsortedlast(o3, typemin(Int)+2) === typemin(Int)+2
    @test searchsorted(o3, typemin(Int)+2) === typemin(Int)+2:typemin(Int)+2
end

function adaptive_sort_test(v; trusted=InsertionSort, kw...)
    sm = sum(hash.(v))
    truth = sort!(deepcopy(v); alg=trusted, kw...)
    return (
        v === sort!(v; kw...) &&
        issorted(v; kw...) &&
        sum(hash.(v)) == sm &&
        all(v .=== truth))
end
@testset "AdaptiveSort" begin
    len = 70

    @testset "Bool" begin
        @test sort([false, true, false]) == [false, false, true]
        @test sort([false, true, false], by=x->0) == [false, true, false]
        @test sort([false, true, false], rev=true) == [true, false, false]
    end

    @testset "fallback" begin
        @test adaptive_sort_test(rand(1:typemax(Int32), len), by=x->x^2)# fallback
        @test adaptive_sort_test(rand(Int, len), by=x->0, trusted=Base.Sort.ScratchQuickSort())
    end

    @test adaptive_sort_test(rand(Int, 20)) # InsertionSort

    @testset "large eltype" begin
        for rev in [true, false]
            @test adaptive_sort_test(rand(Int128, len), rev=rev) # direct ordered int
            @test adaptive_sort_test(fill(rand(UInt128), len), rev=rev) # all same
            @test adaptive_sort_test(rand(Int128.(1:len), len), rev=rev) # short int range
        end
    end

    @test adaptive_sort_test(fill(rand(), len)) # All same

    @testset "count sort" begin
        @test adaptive_sort_test(rand(1:20, len))
        @test adaptive_sort_test(rand(1:20, len), rev=true)
    end

    @testset "post-serialization count sort" begin
        v = reinterpret(Float64, rand(1:20, len))
        @test adaptive_sort_test(copy(v))
        @test adaptive_sort_test(copy(v), rev=true)
    end

    @testset "presorted" begin
        @test adaptive_sort_test(sort!(rand(len)))
        @test adaptive_sort_test(sort!(rand(Float32, len), rev=true))
        @test adaptive_sort_test(vcat(sort!(rand(Int16, len)), Int16(0)))
        @test adaptive_sort_test(vcat(sort!(rand(UInt64, len), rev=true), 0))
    end

    @testset "lenm1 < 3bits fallback" begin
        @test adaptive_sort_test(rand(len)) # InsertionSort
        @test adaptive_sort_test(rand(130)) # QuickSort
    end

    @test adaptive_sort_test(rand(1000)) # RadixSort
end

@testset "uint mappings" begin

    #Construct value lists
    floats = [reinterpret(U, vcat(T[-π, -1.0, -1/π, 1/π, 1.0, π, -0.0, 0.0, Inf, -Inf, NaN, -NaN,
                prevfloat(T(0)), nextfloat(T(0)), prevfloat(T(Inf)), nextfloat(T(-Inf))], randnans(4)))
        for (U, T) in [(UInt16, Float16), (UInt32, Float32), (UInt64, Float64)]]

    ints = [T[17, -T(17), 0, -one(T), 1, typemax(T), typemin(T), typemax(T)-1, typemin(T)+1]
        for T in Base.BitInteger_types]

    char = Char['\n', ' ', Char(0), Char(8), Char(17), typemax(Char)]

    vals = vcat(floats, ints, [char])

    #Add random values
    UIntN(::Val{1}) = UInt8
    UIntN(::Val{2}) = UInt16
    UIntN(::Val{4}) = UInt32
    UIntN(::Val{8}) = UInt64
    UIntN(::Val{16}) = UInt128
    map(vals) do x
        x isa Base.ReinterpretArray && return
        T = eltype(x)
        U = UIntN(Val(sizeof(T)))
        append!(x, rand(T, 4))
        append!(x, reinterpret.(T, rand(U, 4)))
    end

    for x in vals
        T = eltype(x)
        U = UIntN(Val(sizeof(T)))
        for order in [Forward, Reverse, By(Forward, identity)]
            if order isa Base.Order.By
                @test Base.Sort.UIntMappable(T, order) === nothing
                continue
            end

            @test Base.Sort.UIntMappable(T, order) === U
            x2 = deepcopy(x)
            u = Base.Sort.uint_map!(x2, 1, length(x), order)
            @test eltype(u) === U
            @test all(Base.Sort.uint_map.(x, (order,)) .=== u)
            mn = rand(U)
            u .-= mn
            @test x2 === Base.Sort.uint_unmap!(x2, u, 1, length(x), order, mn)
            @test all(x2 .=== x)

            for a in x
                for b in x
                    @test Base.Order.lt(order, a, b) === Base.Order.lt(Forward, Base.Sort.uint_map(a, order), Base.Sort.uint_map(b, order))
                end
            end
        end
    end

    @test Base.Sort.UIntMappable(Union{Int, UInt}, Base.Forward) === nothing # issue #45280
end

@testset "invalid lt (#11429)" begin
    # lt must be a total linear order (e.g. < not <=) so this usage is
    # not allowed. Consequently, none of the behavior tested in this
    # testset is guaranteed to work in future minor versions of Julia.

    safe_algs = [InsertionSort, MergeSort, Base.Sort.ScratchQuickSort(), Base.DEFAULT_STABLE, Base.DEFAULT_UNSTABLE]

    n = 1000
    Random.seed!(0x3588d23f15e74060);
    v = rand(1:5, n);
    s = sort(v);

    # Nevertheless, it still works...
    for alg in safe_algs
        @test sort(v, alg=alg, lt = <=) == s
    end
    @test partialsort(v, 172, lt = <=) == s[172]
    @test partialsort(v, 315:415, lt = <=) == s[315:415]

    # ...and it is consistently reverse stable. All these algorithms swap v[i] and v[j]
    # where i < j if and only if lt(o, v[j], v[i]). This invariant holds even for
    # this invalid lt order.
    perm = reverse(sortperm(v, rev=true))
    for alg in safe_algs
        @test sort(1:n, alg=alg, lt = (i,j) -> v[i]<=v[j]) == perm
    end
    # Broken by the introduction of BracketedSort in #52006 which is unstable
    # @test_broken partialsort(1:n, 172, lt = (i,j) -> v[i]<=v[j]) == perm[172] (sometimes passes due to RNG)
    @test_broken partialsort(1:n, 315:415, lt = (i,j) -> v[i]<=v[j]) == perm[315:415]

    # lt can be very poorly behaved and sort will still permute its input in some way.
    for alg in safe_algs
        @test sort!(sort(v, alg=alg, lt = (x,y) -> rand([false, true]))) == s
    end
    @test partialsort(v, 172, lt = (x,y) -> rand([false, true])) ∈ 1:5
    @test all(partialsort(v, 315:415, lt = (x,y) -> rand([false, true])) .∈ (1:5,))

    # issue #32675
    k = [38, 18, 38, 38, 3, 37, 26, 26, 6, 29, 38, 36, 38, 1, 38, 36, 38, 38, 38, 36, 36,
        36, 28, 34, 35, 38, 25, 20, 38, 1, 1, 5, 38, 38, 3, 34, 16, 38, 4, 10, 35, 37, 38,
        38, 2, 38, 25, 35, 38, 1, 35, 36, 20, 33, 36, 18, 38, 1, 24, 4, 38, 18, 12, 38, 34,
        35, 36, 38, 26, 31, 36, 38, 38, 30, 36, 35, 35, 7, 22, 35, 38, 35, 30, 21, 37]
    idx = sortperm(k; lt=!isless)
    @test issorted(k[idx], rev=true)
end

@testset "sort(x; scratch)" begin
    for n in [1,10,100,1000]
        v = rand(n)
        scratch = [0.0]
        @test sort(v) == sort(v; scratch)
        @test sort!(copy(v)) == sort!(copy(v); scratch)
        @test sortperm(v) == sortperm(v; scratch=[4])
        @test sortperm!(Vector{Int}(undef, n), v) == sortperm!(Vector{Int}(undef, n), v; scratch=[4])

        n > 100 && continue
        M = rand(n, n)
        @test sort(M; dims=2) == sort(M; dims=2, scratch)
        @test sort!(copy(M); dims=1) == sort!(copy(M); dims=1, scratch)
    end
end

@testset "sorting preserves identity" begin
    a = BigInt.([2, 2, 2, 1, 1, 1]) # issue #39620
    sort!(a)
    @test length(IdDict(a .=> a)) == 6

    for v in [BigInt.(rand(1:5, 40)), BigInt.(rand(Int, 70)), BigFloat.(rand(52))]
        hashes = Set(hash.(v))
        ids = Set(objectid.(v))
        sort!(v)
        @test hashes == Set(hash.(v))
        @test ids == Set(objectid.(v))
    end
end

@testset "Unions with missing" begin
    @test issorted(sort(shuffle!(vcat(fill(missing, 10), rand(Int, 100)))))
    @test issorted(sort(vcat(rand(Int8, 600), [missing])))

    # Because we define defalg(::AbstractArray{Missing})
    @test all(fill(missing, 10) .=== sort(fill(missing, 10)))

    # Unit tests for WithoutMissingVector
    a = [1,7,missing,4]
    @test_throws ArgumentError Base.Sort.WithoutMissingVector(a)
    @test eltype(a[[1,2,4]]) == eltype(a)
    @test eltype(Base.Sort.WithoutMissingVector(a[[1,2,4]])) == Int
    am = Base.Sort.WithoutMissingVector(a, unsafe=true)
    @test am[2] == 7
    @test eltype(am) == Int
end

@testset "Specific algorithms" begin
    let
        requires_uint_mappable = Union{Base.Sort.RadixSort, Base.Sort.ConsiderRadixSort,
            Base.Sort.CountingSort, Base.Sort.ConsiderCountingSort,
            typeof(Base.Sort._DEFAULT_ALGORITHMS_FOR_VECTORS.next.next.next.big.next.yes),
            typeof(Base.Sort._DEFAULT_ALGORITHMS_FOR_VECTORS.next.next.next.big.next.yes.big),
            typeof(Base.Sort._DEFAULT_ALGORITHMS_FOR_VECTORS.next.next.next.big.next.yes.big.next)}

        function test_alg(kw, alg, float=true)
            for order in [Base.Forward, Base.Reverse, Base.By(x -> x^2)]
                order isa Base.By && alg isa requires_uint_mappable && continue
                for n in [1,7,179,1312]

                    n == 1 && alg isa Base.Sort.RadixSort && continue

                    x = rand(1:n+1, n)
                    y = sort(x; order)
                    @test Base.Sort._sort!(x, alg, order, (;kw(y)...)) !== x
                    @test all(y .=== x)

                    alg isa requires_uint_mappable && continue

                    x = randn(n)
                    y = sort(x; order)
                    @test Base.Sort._sort!(x, alg, order, (;kw(y)...)) !== x
                    @test all(y .=== x)
                end
            end
        end
        test_alg(alg) = test_alg(x -> (), alg)

        function test_alg_rec(alg, extrema=false)
            if extrema
                test_alg(alg) do y
                    (;mn=first(y),mx=last(y))
                end
            else
                test_alg(alg)
            end
            extrema |= alg isa Base.Sort.ComputeExtrema
            for name in fieldnames(typeof(alg))
                a = getfield(alg, name)
                a isa Base.Sort.Algorithm && test_alg_rec(a, extrema)
            end
        end

        test_alg_rec(Base.Sort._DEFAULT_ALGORITHMS_FOR_VECTORS)
    end
end

@testset "show(::Algorithm)" begin
    @test eval(Meta.parse(string(Base.Sort._DEFAULT_ALGORITHMS_FOR_VECTORS))) === Base.Sort._DEFAULT_ALGORITHMS_FOR_VECTORS
    lines = split(string(Base.Sort._DEFAULT_ALGORITHMS_FOR_VECTORS), '\n')
    @test 10 < maximum(length, lines) < 100
    @test 1 < length(lines) < 30

    @test eval(Meta.parse(string(Base.DEFAULT_STABLE))) === Base.DEFAULT_STABLE
    @test string(Base.DEFAULT_STABLE) == "Base.Sort.DefaultStable()"
end

@testset "Extensibility" begin
    # Defining new algorithms & backwards compatibility with packages that use sorting internals

    struct MyFirstAlg <: Base.Sort.Algorithm end

    @test_throws ArgumentError sort([1,2,3], alg=MyFirstAlg()) # not a stack overflow error

    v = shuffle(vcat(fill(missing, 10), rand(Int, 100)))

    # The pre 1.9 dispatch method
    function Base.sort!(v::AbstractVector{Int}, lo::Integer, hi::Integer, ::MyFirstAlg, o::Base.Order.Ordering)
        v[lo:hi] .= 7
    end
    @test sort([1,2,3], alg=MyFirstAlg()) == [7,7,7]
    @test all(sort(v, alg=Base.Sort.InitialOptimizations(MyFirstAlg())) .=== vcat(fill(7, 100), fill(missing, 10)))

    # Using the old hook with old entry-point
    @test sort!([3,1,2], MyFirstAlg(), Base.Forward) == [7,7,7]
    @test sort!([3,1,2], 1, 3, MyFirstAlg(), Base.Forward) == [7,7,7]

    # Use the pre 1.9 entry-point into the internals
    function Base.sort!(v::AbstractVector{Int}, lo::Integer, hi::Integer, ::MyFirstAlg, o::Base.Order.Ordering)
        sort!(v, lo, hi, Base.DEFAULT_STABLE, o)
    end
    @test sort([3,1,2], alg=MyFirstAlg()) == [1,2,3]
    @test issorted(sort(v, alg=Base.Sort.InitialOptimizations(MyFirstAlg())))

    # Another pre 1.9 entry-point into the internals
    @test issorted(sort!(rand(100), InsertionSort, Base.Order.Forward))

    struct MySecondAlg <: Base.Sort.Algorithm end
    # A new dispatch method
    function Base.Sort._sort!(v::AbstractVector, ::MySecondAlg, o::Base.Order.Ordering, kw)
        Base.Sort.@getkw lo hi
        v[lo:hi] .= 9
    end
    @test sort([1,2,3], alg=MySecondAlg()) == [9,9,9]
    @test all(sort(v, alg=Base.Sort.InitialOptimizations(MySecondAlg())) .=== vcat(fill(9, 100), fill(missing, 10)))

    # Tuple extensions (custom alg)
    @test_throws MethodError sort((1,2,3), alg=MyFirstAlg())
    Base.Sort._sort(v::NTuple, ::MyFirstAlg, o::Base.Order.Ordering, kw) = (17,2,9)
    @test sort((1,2,3), alg=MyFirstAlg()) == (17,2,9)

    struct TupleFoo
        x::Int
    end

    # Tuple extensions (custom type)
    @test_throws MethodError sort(TupleFoo.((3,1,2)))
    Base.Sort._sort(v::NTuple{N, TupleFoo}, ::Base.Sort.DefaultStable, o::Base.Order.Ordering, kw) where N = v
    @test sort(TupleFoo.((3,1,2))) === TupleFoo.((3,1,2))
end

@testset "sort!(v, lo, hi, alg, order)" begin
    v = Vector{Float64}(undef, 4000)
    for alg in [MergeSort, QuickSort, InsertionSort, Base.DEFAULT_STABLE, Base.DEFAULT_UNSTABLE]
        rand!(v)
        sort!(v, 1, 2000, alg, Base.Forward)
        @test issorted(v[1:2000])
        @test !issorted(v)

        sort!(v, 2001, 4000, alg, Base.Forward)
        @test issorted(v[1:2000])
        @test issorted(v[2001:4000])
        @test !issorted(v)

        sort!(v, 1001, 3000, alg, Base.Forward)
        @test issorted(v[1:1000])
        @test issorted(v[1001:3000])
        @test issorted(v[3001:4000])
        @test !issorted(v[1:2000])
        @test !issorted(v[2001:4000])
        @test !issorted(v)
    end
end

@testset "IEEEFloatOptimization with -0.0" begin
    x = vcat(round.(100 .* randn(1000)) ./ 100) # Also test lots of duplicates
    x[rand(1:1000, 5)] .= 0.0
    x[rand(1:1000, 5)] .= -0.0  # To be sure that -0.0 is present
    @test issorted(sort!(x))
end

@testset "Count sort near the edge of its range" begin
    @test issorted(sort(rand(typemin(Int):typemin(Int)+100, 1000)))
    @test issorted(sort(rand(typemax(Int)-100:typemax(Int), 1000)))
    @test issorted(sort(rand(Int8, 600)))
end

@testset "ScratchQuickSort API" begin
    bsqs = Base.Sort.ScratchQuickSort
    @test bsqs(1, 2, MergeSort)             === bsqs(1, 2, MergeSort)
    @test bsqs(missing, 2, MergeSort)       === bsqs(missing, 2, MergeSort)
    @test bsqs(1, missing, MergeSort)       === bsqs(1, missing, MergeSort)
    @test bsqs(missing, missing, MergeSort) === bsqs(missing, missing, MergeSort)
    @test bsqs(1, MergeSort)                === bsqs(1, 1, MergeSort)
    @test bsqs(missing, MergeSort)          === bsqs(missing, missing, MergeSort)
    @test bsqs(MergeSort)                   === bsqs(missing, missing, MergeSort)

    @test bsqs(1, 2)                        === bsqs(1, 2, InsertionSort)
    @test bsqs(missing, 2)                  === bsqs(missing, 2, InsertionSort)
    @test bsqs(1, missing)                  === bsqs(1, missing, InsertionSort)
    @test bsqs(missing, missing)            === bsqs(missing, missing, InsertionSort)
    @test bsqs(1)                           === bsqs(1, 1, InsertionSort)
    @test bsqs(missing)                     === bsqs(missing, missing, InsertionSort)
    @test bsqs()                            === bsqs(missing, missing, InsertionSort)
end

@testset "ScratchQuickSort allocations on non-concrete eltype" begin
    let v = Vector{Union{Nothing, Bool}}(rand(Bool, 10000))
        @test 10 > @allocations sort(v)
        @test 10 > @allocations sort(v; alg=Base.Sort.ScratchQuickSort())
    end
    # it would be nice if these numbers were lower (1 or 2), but these
    # test that we don't have O(n) allocations due to type instability
end

function test_allocs()
    v = rand(10)
    i = randperm(length(v))
    @test 2 >= @allocations sort(v)
    @test 0 == @allocations sortperm!(i, v)
    @test 0 == @allocations sort!(i)
    @test 0 == @allocations sortperm!(i, v, rev=true)
    @test 2 >= @allocations sortperm(v, rev=true)
    @test 2 >= @allocations sortperm(v, rev=false)
    @test 0 == @allocations sortperm!(i, v, order=Base.Reverse)
    @test 2 >= @allocations sortperm(v)
    @test 2 >= @allocations sortperm(i, by=sqrt)
    @test 0 == @allocations sort!(v, lt=(a, b) -> hash(a) < hash(b))
    sort!(Int[], rev=false) # compile
    @test 0 == @allocations sort!(i, rev=false)
    rand!(i)
    @test 0 == @allocations sort!(i, order=Base.Reverse)
end
@testset "Small calls do not unnecessarily allocate" begin
    test_allocs()
end

@testset "Presorted and reverse-presorted" begin
    for len in [7, 92, 412, 780]
        x = sort(randn(len))
        for _ in 1:2
            @test issorted(sort(x))
            @test issorted(sort(x), by=x -> x+7)
            reverse!(x)
        end
    end
end

struct MyArray49392{T, N} <: AbstractArray{T, N}
    data::Array{T, N}
end
Base.size(A::MyArray49392) = size(A.data)
Base.getindex(A::MyArray49392, i...) = getindex(A.data, i...)
Base.setindex!(A::MyArray49392, v, i...) = setindex!(A.data, v, i...)
Base.similar(A::MyArray49392, ::Type{T}, dims::Dims{N}) where {T, N} = MyArray49392(similar(A.data, T, dims))

@testset "Custom matrices (#49392)" begin
    x = rand(10, 10)
    y = MyArray49392(copy(x))
    @test all(sort!(y, dims=2) .== sort!(x,dims=2))
end

@testset "MissingOptimization fastpath for Perm ordering when lo:hi ≠ eachindex(v)" begin
    v = [rand() < .5 ? missing : rand() for _ in 1:100]
    ix = collect(1:100)
    sort!(ix, 1, 10, Base.Sort.DEFAULT_STABLE, Base.Order.Perm(Base.Order.Forward, v))
    @test issorted(v[ix[1:10]])
end

struct NonScalarIndexingOfWithoutMissingVectorAlg <: Base.Sort.Algorithm end
function Base.Sort._sort!(v::AbstractVector, ::NonScalarIndexingOfWithoutMissingVectorAlg, o::Base.Order.Ordering, kw)
    Base.Sort.@getkw lo hi
    first_half = v[lo:lo+(hi-lo)÷2]
    second_half = v[lo+(hi-lo)÷2+1:hi]
    whole = v[lo:hi]
    all(vcat(first_half, second_half) .=== whole) || error()
    out = Base.Sort._sort!(whole, Base.Sort.DEFAULT_STABLE, o, (;kw..., lo=1, hi=length(whole)))
    v[lo:hi] .= whole
    out
end

@testset "Non-scaler indexing of WithoutMissingVector" begin
    @testset "Unit test" begin
        wmv = Base.Sort.WithoutMissingVector(Union{Missing, Int}[1, 7, 2, 9])
        @test wmv[[1, 3]] == [1, 2]
        @test wmv[1:3] == [1, 7, 2]
    end
    @testset "End to end" begin
        alg = Base.Sort.InitialOptimizations(NonScalarIndexingOfWithoutMissingVectorAlg())
        @test issorted(sort(rand(100); alg))
        @test issorted(sort([rand() < .5 ? missing : randstring() for _ in 1:100]; alg))
    end
end

struct DispatchLoopTestAlg <: Base.Sort.Algorithm end
function Base.sort!(v::AbstractVector, lo::Integer, hi::Integer, ::DispatchLoopTestAlg, order::Base.Order.Ordering)
    sort!(view(v, lo:hi); order)
end
@testset "Support dispatch from the old style to the new style and back" begin
    @test issorted(sort!(rand(100), Base.Sort.InitialOptimizations(DispatchLoopTestAlg()), Base.Order.Forward))
end

# Pathologize 0 is a noop, pathologize 3 is fully pathological
function pathologize!(x, level)
    Base.require_one_based_indexing(x)
    k2 = Int(cbrt(length(x))^2)
    seed = hash(length(x), Int === Int64 ? 0x85eb830e0216012d : 0xae6c4e15)
    for a in 1:level
        seed = hash(a, seed)
        x[mod.(hash.(1:k2, seed), range.(1:k2,lastindex(x)))] .= a
    end
    x
end

@testset "partialsort tests added for BracketedSort #52006" begin
    for x in [pathologize!.(Ref(rand(Int, 1000)), 0:3); pathologize!.(Ref(rand(1000)), 0:3); [pathologize!(rand(Int, 1_000_000), 3)]]
        @test partialsort(x, 1) == minimum(x)
        @test partialsort(x, lastindex(x)) == maximum(x)
        sx = sort(x)
        for i in [1, 2, 4, 10, 11, 425, 500, 845, 991, 997, 999, 1000]
            @test partialsort(x, i) == sx[i]
        end
        for i in [1:1, 1:2, 1:5, 1:8, 1:9, 1:11, 1:108, 135:812, 220:586, 363:368, 450:574, 458:597, 469:638, 487:488, 500:501, 584:594, 1000:1000]
            @test partialsort(x, i) == sx[i]
        end
    end
end

# This testset is at the end of the file because it is slow.
@testset "searchsorted" begin
    numTypes = [ Int8,  Int16,  Int32,  Int64,  Int128,
                UInt8, UInt16, UInt32, UInt64, UInt128,
                Float16, Float32, Float64, BigInt, BigFloat]

    @test searchsorted([1:10;], 1, by=(x -> x >= 5)) == 1:4
    @test searchsorted([1:10;], 10, by=(x -> x >= 5)) == 5:10
    @test searchsorted([1:5; 1:5; 1:5], 1, 6, 10, Forward) == 6:6
    @test searchsorted(fill(1, 15), 1, 6, 10, Forward) == 6:10

    for R in numTypes, T in numTypes
        @test searchsorted(R[1, 1, 2, 2, 3, 3], T(0)) === 1:0
        @test searchsorted(R[1, 1, 2, 2, 3, 3], T(1)) == 1:2
        @test searchsorted(R[1, 1, 2, 2, 3, 3], T(2)) == 3:4
        @test searchsorted(R[1, 1, 2, 2, 3, 3], T(4)) === 7:6
        @test searchsorted(R[1, 1, 2, 2, 3, 3], 2.5) === 5:4

        @test searchsorted(1:3, T(0)) === 1:0
        @test searchsorted(1:3, T(1)) == 1:1
        @test searchsorted(1:3, T(2)) == 2:2
        @test searchsorted(1:3, T(4)) === 4:3

        @test searchsorted(R[1:10;], T(1), by=(x -> x >= 5)) == 1:4
        @test searchsorted(R[1:10;], T(10), by=(x -> x >= 5)) == 5:10
        @test searchsorted(R[1:5; 1:5; 1:5], T(1), 6, 10, Forward) == 6:6
        @test searchsorted(fill(R(1), 15), T(1), 6, 10, Forward) == 6:10
    end

    for (rg,I) in Any[(49:57,47:59), (1:2:17,-1:19), (-3:0.5:2,-5:.5:4)]
        rg_r = reverse(rg)
        rgv, rgv_r = [rg;], [rg_r;]
        for i = I
            @test searchsorted(rg,i) === searchsorted(rgv,i)
            @test searchsorted(rg_r,i,rev=true) === searchsorted(rgv_r,i,rev=true)
        end
    end

    rg = 0.0:0.01:1.0
    for i = 2:101
        @test searchsorted(rg, rg[i]) == i:i
        @test searchsorted(rg, prevfloat(rg[i])) === i:i-1
        @test searchsorted(rg, nextfloat(rg[i])) === i+1:i
    end

    rg_r = reverse(rg)
    for i = 1:100
        @test searchsorted(rg_r, rg_r[i], rev=true) == i:i
        @test searchsorted(rg_r, prevfloat(rg_r[i]), rev=true) === i+1:i
        @test searchsorted(rg_r, nextfloat(rg_r[i]), rev=true) === i:i-1
    end

    @test searchsorted(1:10, 1, by=(x -> x >= 5)) == searchsorted([1:10;], 1, by=(x -> x >= 5))
    @test searchsorted(1:10, 10, by=(x -> x >= 5)) == searchsorted([1:10;], 10, by=(x -> x >= 5))

    @test searchsorted([], 0) === 1:0
    @test searchsorted([1,2,3], 0) === 1:0
    @test searchsorted([1,2,3], 4) === 4:3

    @testset "issue 8866" begin
        @test searchsortedfirst(500:1.0:600, -1.0e20) == 1
        @test searchsortedfirst(500:1.0:600, 1.0e20) == 102
        @test searchsortedlast(500:1.0:600, -1.0e20) == 0
        @test searchsortedlast(500:1.0:600, 1.0e20) == 101
    end

    @testset "issue 10966" begin
        for R in numTypes, T in numTypes
            @test searchsortedfirst(R(2):R(2), T(0)) == 1
            @test searchsortedfirst(R(2):R(2), T(2)) == 1
            @test searchsortedfirst(R(2):R(2), T(3)) == 2
            @test searchsortedfirst(R(1):1//2:R(5), T(0)) == 1
            @test searchsortedfirst(R(1):1//2:R(5), T(2)) == 3
            @test searchsortedfirst(R(1):1//2:R(5), T(6)) == 10
            @test searchsortedlast(R(2):R(2), T(0)) == 0
            @test searchsortedlast(R(2):R(2), T(2)) == 1
            @test searchsortedlast(R(2):R(2), T(3)) == 1
            @test searchsortedlast(R(1):1//2:R(5), T(0)) == 0
            @test searchsortedlast(R(1):1//2:R(5), T(2)) == 3
            @test searchsortedlast(R(1):1//2:R(5), T(6)) == 9
            @test searchsorted(R(2):R(2), T(0)) === 1:0
            @test searchsorted(R(2):R(2), T(2)) == 1:1
            @test searchsorted(R(2):R(2), T(3)) === 2:1
        end
    end

    @testset "issue 32568" begin
        for R in numTypes, T in numTypes
            for arr in Any[R[1:5;], R(1):R(5), R(1):2:R(5)]
                @test eltype(searchsorted(arr, T(2))) == keytype(arr)
                @test eltype(searchsorted(arr, T(2), big(1), big(4), Forward)) == keytype(arr)
                @test searchsortedfirst(arr, T(2)) isa keytype(arr)
                @test searchsortedfirst(arr, T(2), big(1), big(4), Forward) isa keytype(arr)
                @test searchsortedlast(arr, T(2)) isa keytype(arr)
                @test searchsortedlast(arr, T(2), big(1), big(4), Forward) isa keytype(arr)
            end
        end
    end

    @testset "issue #34157" begin
        @test searchsorted(1:2.0, -Inf) === 1:0
        @test searchsorted([1,2], -Inf) === 1:0
        @test searchsorted(1:2,   -Inf) === 1:0

        @test searchsorted(1:2.0, Inf) === 3:2
        @test searchsorted([1,2], Inf) === 3:2
        @test searchsorted(1:2,   Inf) === 3:2

        for coll in Any[
                Base.OneTo(10),
                1:2,
                0x01:0x02,
                -4:6,
                5:2:10,
                [1,2],
                1.0:4,
                [10.0,20.0],
            ]
            for huge in Any[Inf, 1e300, typemax(Int64), typemax(UInt64)]
                @test searchsortedfirst(coll, huge) === lastindex(coll) + 1
                @test searchsortedlast(coll, huge)  === lastindex(coll)
                @test searchsorted(coll, huge)      === lastindex(coll)+1 : lastindex(coll)
                if !(eltype(coll) <: Unsigned)
                    @test searchsortedfirst(reverse(coll), huge, rev=true) === firstindex(coll)
                    @test searchsortedlast(reverse(coll), huge, rev=true) === firstindex(coll) - 1
                    @test searchsorted(reverse(coll), huge, rev=true) === firstindex(coll):firstindex(coll) - 1
                end

                if !(huge isa Unsigned)
                    @test searchsortedfirst(coll, -huge)=== firstindex(coll)
                    @test searchsortedlast(coll, -huge) === firstindex(coll) - 1
                    @test searchsorted(coll, -huge)     === firstindex(coll) : firstindex(coll) - 1
                    if !(eltype(coll) <: Unsigned)
                        @test searchsortedfirst(reverse(coll), -huge, rev=true) === lastindex(coll) + 1
                        @test searchsortedlast(reverse(coll), -huge, rev=true) === lastindex(coll)
                        @test searchsorted(reverse(coll), -huge, rev=true) === lastindex(coll)+1:lastindex(coll)
                    end
                end
            end
        end

        @testset "issue #34408" begin
            r = 1f8-10:1f8
            @test collect(r) == Float32[9.999999e7, 9.999999e7, 9.999999e7, 9.999999e7, 1.0e8, 1.0e8, 1.0e8, 1.0e8, 1.0e8]
            for i in r
                @test_broken searchsorted(collect(r), i) == searchsorted(r, i)
            end
        end
    end
    @testset "issue #35272" begin
        for v0 = (3:-1:1, 3.0:-1.0:1.0), v = (v0, collect(v0))
            @test searchsorted(v, 3, rev=true) == 1:1
            @test searchsorted(v, 3.0, rev=true) == 1:1
            @test searchsorted(v, 2.5, rev=true) === 2:1
            @test searchsorted(v, 2, rev=true) == 2:2
            @test searchsorted(v, 1.2, rev=true) === 3:2
            @test searchsorted(v, 1, rev=true) == 3:3
            @test searchsorted(v, 0.1, rev=true) === 4:3
        end
    end

    @testset "ranges issue #44102, PR #50365" begin
        # range sorting test for different Ordering parameter combinations
        @test searchsorted(-1000.0:1:1000, -0.0) === 1001:1000
        @test searchsorted(-1000.0:1:1000, -0.0; lt=<) === 1001:1001
        @test searchsorted(-1000.0:1:1000, -0.0; lt=<, by=x->x) === 1001:1001
        @test searchsorted(reverse(-1000.0:1:1000), -0.0; lt=<, by=-) === 1001:1001
        @test searchsorted(reverse(-1000.0:1:1000), -0.0, rev=true) === 1002:1001
        @test searchsorted(reverse(-1000.0:1:1000), -0.0; lt=<, rev=true) === 1001:1001
    end
end
# The "searchsorted" testset is at the end of the file because it is slow.

end
