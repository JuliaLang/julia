using Sort

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

for alg in [Sort.InsertionSort, Sort.MergeSort] #, Sort.TimSort]
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

    b = sort(alg, Sort.By(x -> -10x), a)
    @test issorted(Sort.By(x -> -10x), b)
    ix = sortperm(alg, Sort.By(x -> -10x), a)
    b = a[ix]
    @test issorted(Sort.By(x -> -10x), b)
    @test a[ix] == b
end

b = sort(Sort.QuickSort, a)
@test issorted(b)
b = sort(Sort.QuickSort, Sort.Reverse, a)
@test issorted(Sort.Reverse, b)
b = sort(Sort.QuickSort, Sort.By(x -> -10x), a)
@test issorted(Sort.By(x -> -10x), b)

@test select(Sort.Reverse, [3,6,30,1,9], 2) == 9
@test select(Sort.By(x -> -x), [3,6,30,1,9], 2) == 9
