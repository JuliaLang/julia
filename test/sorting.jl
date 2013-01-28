@test sort([2,3,1]) == [1,2,3]
@test sort(Sort.Reverse,[2,3,1]) == [3,2,1]
@test sortperm([2,3,1]) == [3,1,2]
@test !issorted([2,3,1])
@test issorted([1,2,3])
@test reverse([2,3,1]) == [1,3,2]
@test select([3,6,30,1,9],3) == 6
@test sum(randperm(6)) == 21
@test nthperm([0,1,2],3) == [1,0,2]

@test searchsortedfirst([1, 1, 2, 2, 3, 3], 0) == 1
@test searchsortedfirst([1, 1, 2, 2, 3, 3], 1) == 1
@test searchsortedfirst([1, 1, 2, 2, 3, 3], 2) == 3
@test searchsortedfirst([1, 1, 2, 2, 3, 3], 4) == 7
@test searchsortedfirst([1.0, 1, 2, 2, 3, 3], 2.5) == 5
@test searchsortedlast([1, 1, 2, 2, 3, 3], 0) == 0
@test searchsortedlast([1, 1, 2, 2, 3, 3], 1) == 2
@test searchsortedlast([1, 1, 2, 2, 3, 3], 2) == 4
@test searchsortedlast([1, 1, 2, 2, 3, 3], 4) == 6
@test searchsortedlast([1.0, 1, 2, 2, 3, 3], 2.5) == 4

rg = 49:57; rgv = [rg]
rg_r = 57:-1:49; rgv_r = [rg_r]
for i = 47:59
    @test searchsortedfirst(rg, i) == searchsortedfirst(rgv, i)
    @test searchsortedlast(rg, i) == searchsortedlast(rgv, i)
    @test searchsortedfirst(Sort.Reverse, rg_r, i) ==
          searchsortedfirst(Sort.Reverse, rgv_r, i)
    @test searchsortedlast(Sort.Reverse, rg_r, i) ==
          searchsortedlast(Sort.Reverse, rgv_r, i)
end
rg = 1:2:17; rgv = [rg]
rg_r = 17:-2:1; rgv_r = [rg_r]
for i = -1:19
    @test searchsortedfirst(rg, i) == searchsortedfirst(rgv, i)
    @test searchsortedlast(rg, i) == searchsortedlast(rgv, i)
    @test searchsortedfirst(Sort.Reverse, rg_r, i) ==
          searchsortedfirst(Sort.Reverse, rgv_r, i)
    @test searchsortedlast(Sort.Reverse, rg_r, i) ==
          searchsortedlast(Sort.Reverse, rgv_r, i)
end
rg = -3:0.5:2; rgv = [rg]
rg_r = 2:-0.5:-3; rgv_r = [rg_r]
for i = -5:.5:4
    @test searchsortedfirst(rg, i) == searchsortedfirst(rgv, i)
    @test searchsortedlast(rg, i) == searchsortedlast(rgv, i)
    @test searchsortedfirst(Sort.Reverse, rg_r, i) ==
          searchsortedfirst(Sort.Reverse, rgv_r, i)
    @test searchsortedlast(Sort.Reverse, rg_r, i) ==
          searchsortedlast(Sort.Reverse, rgv_r, i)
end

a = rand(1:10000, 1000)

for alg in [InsertionSort, MergeSort, TimSort]
    b = sort(alg, a)
    @test issorted(b)
    ix = sortperm(alg, a)
    b = a[ix]
    @test issorted(b)
    @test a[ix] == b

    b = sort(alg, Sort.Reverse, a)
    @test issorted(Sort.Reverse, b)
    ix = sortperm(alg, Sort.Reverse, a)
    b = a[ix]
    @test issorted(Sort.Reverse, b)
    @test a[ix] == b

    b = sortby(alg, x -> -10x, a)
    @test issorted(Sort.By(x -> -10x), b)
    ix = sortperm(alg, Sort.By(x -> -10x), a)
    b = a[ix]
    @test issorted(Sort.By(x -> -10x), b)
    @test a[ix] == b
end

b = sort(QuickSort, a)
@test issorted(b)
b = sort(QuickSort, Sort.Reverse, a)
@test issorted(Sort.Reverse, b)
b = sortby(QuickSort, x -> -10x, a)
@test issorted(Sort.By(x -> -10x), b)

@test select(Sort.Reverse, [3,6,30,1,9], 2) == 9
@test select(Sort.By(x -> -x), [3,6,30,1,9], 2) == 9

## more advanced sorting tests ##

randnans(n) = reinterpret(Float64,[rand(Uint64)|0x7ff8000000000000 for i=1:n])

function randn_with_nans(n,p)
    v = randn(n)
    x = find(rand(n).<p)
    v[x] = randnans(length(x))
    return v
end

srand(0xdeadbeef)

for n in [0:10, 100, 1000]
    r = 1:10
    v = rand(1:10,n)
    h = hist(v,length(r))

    for ord in [Sort.Forward, Sort.Reverse]
        # insersion sort as a reference
        pi = sortperm(InsertionSort,ord,v)
        @test isperm(pi)
        s = v[pi]
        @test issorted(ord,s)
        @test hist(s) == h
        @test all([ issorted(pi[s.==i]) for i in r ])

        # mergesort
        pm = sortperm(MergeSort,ord,v)
        @test pi == pm

        # timsort
        pt = sortperm(TimSort,ord,v)
        # @test pi == pt ### FIXME: #2138
        @test isperm(pt)
        @test v[pt] == s

        # quicksort (unstable)
        pq = sortperm(QuickSort,ord,v)
        @test isperm(pi)
        @test v[pq] == s
    end

    v = randn_with_nans(n,0.1)
    for ord in [Sort.Forward, Sort.Reverse],
        alg in [InsertionSort, QuickSort, MergeSort, TimSort]
        # test float sorting with NaNs
        s = sort(alg,ord,v)
        @test issorted(ord,s)
        @test reinterpret(Uint64,v[isnan(v)]) == reinterpret(Uint64,s[isnan(s)])

        # test float permutation with NaNs
        p = sortperm(alg,ord,v)
        @test isperm(p)
        vp = v[p]
        @test isequal(s,vp)
        @test reinterpret(Uint64,s) == reinterpret(Uint64,vp)
    end
end
