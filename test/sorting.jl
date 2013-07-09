@test sort([2,3,1]) == [1,2,3]
@test sort([2,3,1], order=Sort.Reverse) == [3,2,1]
@test sortperm([2,3,1]) == [3,1,2]
@test !issorted([2,3,1])
@test issorted([1,2,3])
@test reverse([2,3,1]) == [1,3,2]
@test select([3,6,30,1,9],3) == 6
@test select([3,6,30,1,9],3:4) == [6,9]
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
@test searchsorted([1, 1, 2, 2, 3, 3], 0) == 1:0
@test searchsorted([1, 1, 2, 2, 3, 3], 1) == 1:2
@test searchsorted([1, 1, 2, 2, 3, 3], 2) == 3:4
@test searchsorted([1, 1, 2, 2, 3, 3], 4) == 7:6
@test searchsorted([1.0, 1, 2, 2, 3, 3], 2.5) == 5:4

rg = 49:57; rgv = [rg]
rg_r = 57:-1:49; rgv_r = [rg_r]
for i = 47:59
    @test searchsortedfirst(rg, i) == searchsortedfirst(rgv, i)
    @test searchsortedlast(rg, i) == searchsortedlast(rgv, i)
    @test searchsortedfirst(rg_r, i, order=Sort.Reverse) ==
          searchsortedfirst(rgv_r, i, order=Sort.Reverse)
    @test searchsortedlast(rg_r, i, order=Sort.Reverse) ==
          searchsortedlast(rgv_r, i, order=Sort.Reverse)
end
rg = 1:2:17; rgv = [rg]
rg_r = 17:-2:1; rgv_r = [rg_r]
for i = -1:19
    @test searchsortedfirst(rg, i) == searchsortedfirst(rgv, i)
    @test searchsortedlast(rg, i) == searchsortedlast(rgv, i)
    @test searchsortedfirst(rg_r, i, order=Sort.Reverse) ==
          searchsortedfirst(rgv_r, i, order=Sort.Reverse)
    @test searchsortedlast(rg_r, i, order=Sort.Reverse) ==
          searchsortedlast(rgv_r, i, order=Sort.Reverse)
end
rg = -3:0.5:2; rgv = [rg]
rg_r = 2:-0.5:-3; rgv_r = [rg_r]
for i = -5:.5:4
    @test searchsortedfirst(rg, i) == searchsortedfirst(rgv, i)
    @test searchsortedlast(rg, i) == searchsortedlast(rgv, i)
    @test searchsortedfirst(rg_r, i, order=Sort.Reverse) ==
          searchsortedfirst(rgv_r, i, order=Sort.Reverse)
    @test searchsortedlast(rg_r, i, order=Sort.Reverse) ==
          searchsortedlast(rgv_r, i, order=Sort.Reverse)
end

rg = 3+0*(1:5); rgv = [rg]
rg_r = rg; rgv_r = [rg_r]
for i = 2:4
    @test searchsortedfirst(rg, i) == searchsortedfirst(rgv, i)
    @test searchsortedlast(rg, i) == searchsortedlast(rgv, i)
    @test searchsortedfirst(rg_r, i, order=Sort.Reverse) ==
          searchsortedfirst(rgv_r, i, order=Sort.Reverse)
    @test searchsortedlast(rg_r, i, order=Sort.Reverse) ==
          searchsortedlast(rgv_r, i, order=Sort.Reverse)
end

rg = 0.0:0.01:1.0
for i = 2:101
    @test searchsortedfirst(rg, rg[i]) == i
    @test searchsortedfirst(rg, prevfloat(rg[i])) == i
    @test searchsortedfirst(rg, nextfloat(rg[i])) == i+1

    @test searchsortedlast(rg, rg[i]) == i
    @test searchsortedlast(rg, prevfloat(rg[i])) == i-1
    @test searchsortedlast(rg, nextfloat(rg[i])) == i
end

rg_r = reverse(rg)
for i = 1:100
    @test searchsortedfirst(rg_r, rg_r[i], order=Sort.Reverse) == i
    @test searchsortedfirst(rg_r, prevfloat(rg_r[i]), order=Sort.Reverse) == i+1
    @test searchsortedfirst(rg_r, nextfloat(rg_r[i]), order=Sort.Reverse) == i

    @test searchsortedlast(rg_r, rg_r[i], order=Sort.Reverse) == i
    @test searchsortedlast(rg_r, prevfloat(rg_r[i]), order=Sort.Reverse) == i
    @test searchsortedlast(rg_r, nextfloat(rg_r[i]), order=Sort.Reverse) == i-1
end

a = rand(1:10000, 1000)

for alg in [InsertionSort, MergeSort, TimSort]
    b = sort(a, alg=alg)
    @test issorted(b)
    ix = sortperm(a, alg=alg)
    b = a[ix]
    @test issorted(b)
    @test a[ix] == b

    b = sort(a, alg=alg, order=Sort.Reverse)
    @test issorted(b, order=Sort.Reverse)
    ix = sortperm(a, alg=alg, order=Sort.Reverse)
    b = a[ix]
    @test issorted(b, order=Sort.Reverse)
    @test a[ix] == b

    b = sortby(a, x->1/x, alg=alg)
    @test issorted(b, order=Sort.By(x->1/x))
    ix = sortperm(a, alg=alg, order=Sort.By(x->1/x))
    b = a[ix]
    @test issorted(b, order=Sort.By(x->1/x))
    @test a[ix] == b

    c = copy(a)
    permute!(c, ix)
    @test c == b

    ipermute!(c, ix)
    @test c == a

    c = sort(a, alg=alg) do x,y
        x > y
    end
    @test b == c

    c = sortby(a, alg=alg) do x
        -10x
    end
    @test b == c
end

b = sort(a, alg=QuickSort)
@test issorted(b)
b = sort(a, alg=QuickSort, order=Sort.Reverse)
@test issorted(b, order=Sort.Reverse)
b = sortby(a, x->1/x, alg=QuickSort)
@test issorted(b, order=Sort.By(x->1/x))

@test select([3,6,30,1,9], 2, order=Sort.Reverse) == 9
@test select([3,6,30,1,9], 2, order=Sort.By(x->1/x)) == 9

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
    h = hist(v,r)

    for ord in [Sort.Forward, Sort.Reverse]
        # insersion sort as a reference
        pi = sortperm(v, alg=InsertionSort, order=ord)
        @test isperm(pi)
        s = v[pi]
        @test issorted(s, order=ord)
        @test hist(s,r) == h
        @test all(issorted,[pi[s.==i] for i in r])
        si = copy(v)
        permute!(si, pi)
        @test si == s
        ipermute!(si, pi)
        @test si == v

        # mergesort
        pm = sortperm(v, alg=MergeSort, order=ord)
        @test pi == pm
        sm = copy(v)
        permute!(sm, pm)
        @test sm == s
        ipermute!(sm, pm)
        @test sm == v

        # timsort
        pt = sortperm(v, alg=TimSort, order=ord)
        @test pi == pt
        st = copy(v)
        permute!(st, pt)
        @test st == s
        ipermute!(st, pt)
        @test st == v

        # quicksort (unstable)
        pq = sortperm(v, alg=QuickSort, order=ord)
        @test isperm(pi)
        @test v[pq] == s
        sq = copy(v)
        permute!(sq, pq)
        @test sq == s
        ipermute!(sq, pq)
        @test sq == v

    end

    v = randn_with_nans(n,0.1)
    for ord in [Sort.Forward, Sort.Reverse],
        alg in [InsertionSort, QuickSort, MergeSort, TimSort]
        # test float sorting with NaNs
        s = sort(v, alg=alg, order=ord)
        @test issorted(s, order=ord)
        @test reinterpret(Uint64,v[isnan(v)]) == reinterpret(Uint64,s[isnan(s)])

        # test float permutation with NaNs
        p = sortperm(v, alg=alg, order=ord)
        @test isperm(p)
        vp = v[p]
        @test isequal(s,vp)
        @test reinterpret(Uint64,s) == reinterpret(Uint64,vp)
    end
end
