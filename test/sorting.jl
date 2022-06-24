# This file is a part of Julia. License is MIT: https://julialang.org/license

module SortingTests

using Base.Order
using Random
using Test

isdefined(Main, :OffsetArrays) || @eval Main include("testhelpers/OffsetArrays.jl")
using .Main.OffsetArrays

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

    a = rand(1:10000, 1000)
    for alg in [InsertionSort, MergeSort, Base.DEFAULT_STABLE]

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

    @testset "unstable algorithms" begin
        for alg in [QuickSort, Base.DEFAULT_UNSTABLE]
            b = sort(a, alg=alg)
            @test issorted(b)
            @test last(b) == last(sort(a, alg=PartialQuickSort(length(a))))
            b = sort(a, alg=alg, rev=true)
            @test issorted(b, rev=true)
            @test last(b) == last(sort(a, alg=PartialQuickSort(length(a)), rev=true))
            b = sort(a, alg=alg, by=x->1/x)
            @test issorted(b, by=x->1/x)
            @test last(b) == last(sort(a, alg=PartialQuickSort(length(a)), by=x->1/x))
        end
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
    let alg = PartialQuickSort(1:div(length(a), 10))
        k = alg.k
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
    let alg = PartialQuickSort(div(length(a), 10))
        k = alg.k
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
            for alg in [MergeSort, Base.DEFAULT_STABLE]
                p = sortperm(v, alg=alg, rev=rev)
                p2 = sortperm(float(v), alg=alg, rev=rev)
                @test p == p2
                @test p == pi
                s = copy(v)
                permute!(s, p)
                @test s == si
                invpermute!(s, p)
                @test s == v
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
        # TODO: alg = PartialQuickSort(n) fails here
        for alg in [InsertionSort, QuickSort, MergeSort, Base.DEFAULT_UNSTABLE, Base.DEFAULT_STABLE],
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

    for alg in [InsertionSort, MergeSort, Base.DEFAULT_STABLE]
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
    Base.Sort.sort_int_range!(a, 10, 0, identity)  # test it supports non-Vector
    @test issorted(a)

    a = OffsetArray([9:-1:0;], -5)
    Base.Sort.sort_int_range!(a, 10, 0, identity)
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

@testset "searchsortedfirst/last with generalized indexing" begin
    o = OffsetVector(1:3, -2)
    @test searchsortedfirst(o, 4) == lastindex(o) + 1
    @test searchsortedfirst(o, 1.5) == 0
    @test searchsortedlast(o, 0) == firstindex(o) - 1
    @test searchsortedlast(o, 1.5) == -1
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
        @test adaptive_sort_test(rand(Int, len), by=x->0, trusted=QuickSort)
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
    floats = [T[-π, -1.0, -1/π, 1/π, 1.0, π, -0.0, 0.0, Inf, -Inf, NaN, -NaN,
                prevfloat(T(0)), nextfloat(T(0)), prevfloat(T(Inf)), nextfloat(T(-Inf))]
        for T in [Float16, Float32, Float64]]

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
        T = eltype(x)
        U = UIntN(Val(sizeof(T)))
        append!(x, rand(T, 4))
        append!(x, reinterpret.(T, rand(U, 4)))
        if T <: AbstractFloat
            mask = reinterpret(U, T(NaN))
            append!(x, reinterpret.(T, mask .| rand(U, 4)))
        end
    end

    for x in vals
        T = eltype(x)
        U = UIntN(Val(sizeof(T)))
        for order in [Forward, Reverse, Base.Sort.Float.Left(), Base.Sort.Float.Right(), By(Forward, identity)]
            if order isa Base.Order.By || T === Float16 ||
                ((T <: AbstractFloat) == (order isa DirectOrdering))
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
                    if order === Base.Sort.Float.Left() || order === Base.Sort.Float.Right()
                        # Left and Right orderings guarantee homogeneous sign and no NaNs
                        (isnan(a) || isnan(b) || signbit(a) != signbit(b)) && continue
                    end
                    @test Base.Order.lt(order, a, b) === Base.Order.lt(Forward, Base.Sort.uint_map(a, order), Base.Sort.uint_map(b, order))
                end
            end
        end
    end
end

@testset "sort(x; workspace=w) " begin
    for n in [1,10,100,1000]
        v = rand(n)
        w = [0.0]
        @test sort(v) == sort(v; workspace=w)
        @test sort!(copy(v)) == sort!(copy(v); workspace=w)
        @test sortperm(v) == sortperm(v; workspace=[4])
        @test sortperm!(Vector{Int}(undef, n), v) == sortperm!(Vector{Int}(undef, n), v; workspace=[4])

        n > 100 && continue
        M = rand(n, n)
        @test sort(M; dims=2) == sort(M; dims=2, workspace=w)
        @test sort!(copy(M); dims=1) == sort!(copy(M); dims=1, workspace=w)
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
            # collect(r) = Float32[9.999999e7, 9.999999e7, 9.999999e7, 9.999999e7, 1.0e8, 1.0e8, 1.0e8, 1.0e8, 1.0e8]
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
end
# The "searchsorted" testset is at the end of the file because it is slow.

end
