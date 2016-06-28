# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "sorting" begin
using Base.Order: Forward

@test sort([2,3,1]) == [1,2,3]
@test sort([2,3,1], rev=true) == [3,2,1]
@test sort(['z':-1:'a';]) == ['a':'z';]
@test sort(['a':'z';], rev=true) == ['z':-1:'a';]
@test sortperm([2,3,1]) == [3,1,2]
@test sortperm!([1,2,3], [2,3,1]) == [3,1,2]
let s = view([1,2,3,4], 1:3)
    r = sortperm!(s, [2,3,1])
    @test r == [3,1,2]
    @test r === s
end
@test_throws ArgumentError sortperm!(view([1,2,3,4], 1:4), [2,3,1])
@test !issorted([2,3,1])
@test issorted([1,2,3])
@test reverse([2,3,1]) == [1,3,2]
@test select([3,6,30,1,9],3) == 6
@test select([3,6,30,1,9],3:4) == [6,9]
@test selectperm([3,6,30,1,9], 3:4) == [2,5]
@test selectperm!(collect(1:5), [3,6,30,1,9], 3:4) == [2,5]
let a=[1:10;]
    for r in Any[2:4, 1:2, 10:10, 4:2, 2:1, 4:-1:2, 2:-1:1, 10:-1:10, 4:1:3, 1:2:8, 10:-3:1]
        @test select(a, r) == [r;]
        @test selectperm(a, r) == [r;]
        @test select(a, r, rev=true) == (11 .- [r;])
        @test selectperm(a, r, rev=true) == (11 .- [r;])
    end
end
@test sum(randperm(6)) == 21

numTypes = [ Int8,  Int16,  Int32,  Int64,  Int128,
            UInt8, UInt16, UInt32, UInt64, UInt128,
            Float16, Float32, Float64, BigInt, BigFloat]

@test searchsorted([1:10;], 1, by=(x -> x >= 5)) == 1:4
@test searchsorted([1:10;], 10, by=(x -> x >= 5)) == 5:10
@test searchsorted([1:5; 1:5; 1:5], 1, 6, 10, Forward) == 6:6
@test searchsorted(ones(15), 1, 6, 10, Forward) == 6:10

for R in numTypes, T in numTypes
    @test searchsorted(R[1, 1, 2, 2, 3, 3], T(0)) == 1:0
    @test searchsorted(R[1, 1, 2, 2, 3, 3], T(1)) == 1:2
    @test searchsorted(R[1, 1, 2, 2, 3, 3], T(2)) == 3:4
    @test searchsorted(R[1, 1, 2, 2, 3, 3], T(4)) == 7:6
    @test searchsorted(R[1, 1, 2, 2, 3, 3], 2.5) == 5:4

    @test searchsorted(1:3, T(0)) == 1:0
    @test searchsorted(1:3, T(1)) == 1:1
    @test searchsorted(1:3, T(2)) == 2:2
    @test searchsorted(1:3, T(4)) == 4:3

    @test searchsorted(R[1:10;], T(1), by=(x -> x >= 5)) == 1:4
    @test searchsorted(R[1:10;], T(10), by=(x -> x >= 5)) == 5:10
    @test searchsorted(R[1:5; 1:5; 1:5], T(1), 6, 10, Forward) == 6:6
    @test searchsorted(ones(R, 15), T(1), 6, 10, Forward) == 6:10
end

for (rg,I) in [(49:57,47:59), (1:2:17,-1:19), (-3:0.5:2,-5:.5:4)]
    rg_r = reverse(rg)
    rgv, rgv_r = [rg;], [rg_r;]
    for i = I
        @test searchsorted(rg,i) == searchsorted(rgv,i)
        @test searchsorted(rg_r,i,rev=true) == searchsorted(rgv_r,i,rev=true)
    end
end

rg = 0.0:0.01:1.0
for i = 2:101
    @test searchsorted(rg, rg[i]) == i:i
    @test searchsorted(rg, prevfloat(rg[i])) == i:i-1
    @test searchsorted(rg, nextfloat(rg[i])) == i+1:i
end

rg_r = reverse(rg)
for i = 1:100
    @test searchsorted(rg_r, rg_r[i], rev=true) == i:i
    @test searchsorted(rg_r, prevfloat(rg_r[i]), rev=true) == i+1:i
    @test searchsorted(rg_r, nextfloat(rg_r[i]), rev=true) == i:i-1
end

@test searchsorted(1:10, 1, by=(x -> x >= 5)) == searchsorted([1:10;], 1, by=(x -> x >= 5))
@test searchsorted(1:10, 10, by=(x -> x >= 5)) == searchsorted([1:10;], 10, by=(x -> x >= 5))

@test searchsorted([], 0) == 1:0
@test searchsorted([1,2,3], 0) == 1:0
@test searchsorted([1,2,3], 4) == 4:3

# issue 8866
@test searchsortedfirst(500:1.0:600, -1.0e20) == 1
@test searchsortedfirst(500:1.0:600, 1.0e20) == 102
@test searchsortedlast(500:1.0:600, -1.0e20) == 0
@test searchsortedlast(500:1.0:600, 1.0e20) == 101

# exercise the codepath in searchsorted* methods for ranges that check for zero step range
immutable ConstantRange{T} <: Range{T}
   val::T
   len::Int
end

Base.length(r::ConstantRange) = r.len
Base.getindex(r::ConstantRange, i::Int) = (1 <= i <= r.len || throw(BoundsError(r,i)); r.val)
Base.step(r::ConstantRange) = 0

r = ConstantRange(1, 5)

@test searchsortedfirst(r, 1.0, Forward) == 1
@test searchsortedfirst(r, 1, Forward) == 1
@test searchsortedfirst(r, UInt(1), Forward) == 1

@test searchsortedlast(r, 1.0, Forward) == 5
@test searchsortedlast(r, 1, Forward) == 5
@test searchsortedlast(r, UInt(1), Forward) == 5

a = rand(1:10000, 1000)

for alg in [InsertionSort, MergeSort]
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

    ipermute!(c, ix)
    @test c == a

    c = sort(a, alg=alg, lt=(>))
    @test b == c

    c = sort(a, alg=alg, by=x->1/x)
    @test b == c
end

# unstable algorithms
for alg in [QuickSort, PartialQuickSort(length(a))]
    b = sort(a, alg=alg)
    @test issorted(b)
    b = sort(a, alg=alg, rev=true)
    @test issorted(b, rev=true)
    b = sort(a, alg=alg, by=x->1/x)
    @test issorted(b, by=x->1/x)
end

# test PartialQuickSort only does a partial sort
let alg = PartialQuickSort(div(length(a), 10))
    k = alg.k
    b = sort(a, alg=alg)
    c = sort(a, alg=alg, by=x->1/x)
    d = sort(a, alg=alg, rev=true)
    @test issorted(b[1:k])
    @test issorted(c[1:k], by=x->1/x)
    @test issorted(d[1:k], rev=true)
    @test !issorted(b)
    @test !issorted(c, by=x->1/x)
    @test !issorted(d, rev=true)
end

@test select([3,6,30,1,9], 2, rev=true) == 9
@test select([3,6,30,1,9], 2, by=x->1/x) == 9
@test selectperm([3,6,30,1,9], 2, rev=true) == 5
@test selectperm([3,6,30,1,9], 2, by=x->1/x) == 5

## more advanced sorting tests ##

randnans(n) = reinterpret(Float64,[rand(UInt64)|0x7ff8000000000000 for i=1:n])

function randn_with_nans(n,p)
    v = randn(n)
    x = find(rand(n).<p)
    v[x] = randnans(length(x))
    return v
end

srand(0xdeadbeef)

for n in [0:10; 100; 101; 1000; 1001]
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
        ipermute!(c, pi)
        @test c == v

        # stable algorithms
        for alg in [MergeSort]
            p = sortperm(v, alg=alg, rev=rev)
            @test p == sortperm(float(v), alg=alg, rev=rev)
            @test p == pi
            s = copy(v)
            permute!(s, p)
            @test s == si
            ipermute!(s, p)
            @test s == v
        end

        # unstable algorithms
        for alg in [QuickSort, PartialQuickSort(n)]
            p = sortperm(v, alg=alg, rev=rev)
            @test p == sortperm(float(v), alg=alg, rev=rev)
            @test isperm(p)
            @test v[p] == si
            s = copy(v)
            permute!(s, p)
            @test s == si
            ipermute!(s, p)
            @test s == v
        end
    end

    v = randn_with_nans(n,0.1)
    # TODO: alg = PartialQuickSort(n) fails here
    for alg in [InsertionSort, QuickSort, MergeSort],
        rev in [false,true]
        # test float sorting with NaNs
        s = sort(v, alg=alg, rev=rev)
        @test issorted(s, rev=rev)
        @test reinterpret(UInt64,v[isnan(v)]) == reinterpret(UInt64,s[isnan(s)])

        # test float permutation with NaNs
        p = sortperm(v, alg=alg, rev=rev)
        @test isperm(p)
        vp = v[p]
        @test isequal(vp,s)
        @test reinterpret(UInt64,vp) == reinterpret(UInt64,s)
    end
end

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
sp = sortperm(inds)
@test all(issorted, [sp[inds.==x] for x in 1:200])
for alg in [InsertionSort, MergeSort]
    sp = sortperm(inds, alg=alg)
    @test all(issorted, [sp[inds.==x] for x in 1:200])
end

# issue #6177
@test sortperm([ 0.0, 1.0, 1.0], rev=true) == [2, 3, 1]
@test sortperm([-0.0, 1.0, 1.0], rev=true) == [2, 3, 1]
@test sortperm([-1.0, 1.0, 1.0], rev=true) == [2, 3, 1]

# issue #8825 - stability of min/max
type Twain
    a :: Int
    b :: Int
end
Base.isless(x :: Twain, y :: Twain) = x.a < y.a
let x = Twain(2,3), y = Twain(2,4)
    @test (min(x,y), max(x,y)) == (x,y) == minmax(x,y)
end

# issue #12833 - type stability of sort
@test Base.return_types(sort, (Vector{Int},)) == [Vector{Int}]

end
