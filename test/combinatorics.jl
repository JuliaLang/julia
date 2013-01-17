@test sort([2,3,1]) == [1,2,3]
@test sortr([2,3,1]) == [3,2,1]
@test sortperm([2,3,1]) == ([1,2,3],[3,1,2])
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
    @test searchsortedfirstr(rg_r, i) == searchsortedfirstr(rgv_r, i)
    @test searchsortedlastr(rg_r, i) == searchsortedlastr(rgv_r, i)
end
rg = 1:2:17; rgv = [rg]
rg_r = 17:-2:1; rgv_r = [rg_r]
for i = -1:19
    @test searchsortedfirst(rg, i) == searchsortedfirst(rgv, i)
    @test searchsortedlast(rg, i) == searchsortedlast(rgv, i)
    @test searchsortedfirstr(rg_r, i) == searchsortedfirstr(rgv_r, i)
    @test searchsortedlastr(rg_r, i) == searchsortedlastr(rgv_r, i)
end
rg = -3:0.5:2; rgv = [rg]
rg_r = 2:-0.5:-3; rgv_r = [rg_r]
for i = -5:.5:4
    @test searchsortedfirst(rg, i) == searchsortedfirst(rgv, i)
    @test searchsortedlast(rg, i) == searchsortedlast(rgv, i)
    @test searchsortedfirstr(rg_r, i) == searchsortedfirstr(rgv_r, i)
    @test searchsortedlastr(rg_r, i) == searchsortedlastr(rgv_r, i)
end

a = randi(10000, 1000)

# insertion_sort
for alg in [Sort.InsertionSort, Sort.MergeSort, Sort.TimSort]

    b = sort(alg, a)
    @test issorted(b)
    (b, ix) = sortperm(alg, a)
    @test issorted(b)
    @test a[ix] == b

    b = sortr(alg, a)
    @test issortedr(b)
    (b, ix) = sortpermr(alg, a)
    @test issortedr(b)
    @test a[ix] == b

    b = sortby(alg, (x -> -10x), a)
    @test issortedby((x -> -10x), b)
    (b, ix) = sortpermby(alg, (x -> -10x), a)
    @test issortedby((x -> -10x), b)
    @test a[ix] == b
end

b = sort(Sort.QuickSort, a)
@test issorted(b)
b = sortr(Sort.QuickSort, a)
@test issortedr(b)
b = sortby(Sort.QuickSort, (x -> -10x), a)
@test issortedby((x -> -10x), b)

@test selectr([3,6,30,1,9],2) == 9
@test selectby((x -> -x),[3,6,30,1,9],2) == 9
