@test sort([2,3,1]) == [1,2,3]
@test sortr([2,3,1]) == [3,2,1]
@test sortperm([2,3,1]) == ([1,2,3],[3,1,2])
@test !issorted([2,3,1])
@test issorted([1,2,3])
@test reverse([2,3,1]) == [1,3,2]
@test select([3,6,30,1,9],3) == 6
@test sum(randperm(6)) == 21
@test nthperm([0,1,2],3) == [1,0,2]

@test search_sorted_first([1, 1, 2, 2, 3, 3], 0) == 1
@test search_sorted_first([1, 1, 2, 2, 3, 3], 1) == 1
@test search_sorted_first([1, 1, 2, 2, 3, 3], 2) == 3
@test search_sorted_first([1, 1, 2, 2, 3, 3], 4) == 7
@test search_sorted_first([1.0, 1, 2, 2, 3, 3], 2.5) == 5
@test search_sorted_last([1, 1, 2, 2, 3, 3], 0) == 0
@test search_sorted_last([1, 1, 2, 2, 3, 3], 1) == 2
@test search_sorted_last([1, 1, 2, 2, 3, 3], 2) == 4
@test search_sorted_last([1, 1, 2, 2, 3, 3], 4) == 6
@test search_sorted_last([1.0, 1, 2, 2, 3, 3], 2.5) == 4

rg = 49:57; rgv = [rg]
rg_r = 57:-1:49; rgv_r = [rg_r]
for i = 47:59
    @test search_sorted_first(rg, i) == search_sorted_first(rgv, i)
    @test search_sorted_last(rg, i) == search_sorted_last(rgv, i)
    @test search_sorted_first_r(rg_r, i) == search_sorted_first_r(rgv_r, i)
    @test search_sorted_last_r(rg_r, i) == search_sorted_last_r(rgv_r, i)
end
rg = 1:2:17; rgv = [rg]
rg_r = 17:-2:1; rgv_r = [rg_r]
for i = -1:19
    @test search_sorted_first(rg, i) == search_sorted_first(rgv, i)
    @test search_sorted_last(rg, i) == search_sorted_last(rgv, i)
    @test search_sorted_first_r(rg_r, i) == search_sorted_first_r(rgv_r, i)
    @test search_sorted_last_r(rg_r, i) == search_sorted_last_r(rgv_r, i)
end
rg = -3:0.5:2; rgv = [rg]
rg_r = 2:-0.5:-3; rgv_r = [rg_r]
for i = -5:.5:4
    @test search_sorted_first(rg, i) == search_sorted_first(rgv, i)
    @test search_sorted_last(rg, i) == search_sorted_last(rgv, i)
    @test search_sorted_first_r(rg_r, i) == search_sorted_first_r(rgv_r, i)
    @test search_sorted_last_r(rg_r, i) == search_sorted_last_r(rgv_r, i)
end

a = rand(1:10000, 1000)

# insertion_sort
for _sort in [insertionsort, mergesort, timsort]
    _sort_r = symbol("$(_sort)_r")
    _sort_by = symbol("$(_sort)_by")
    _sort_perm = symbol("$(_sort)_perm")
    _sort_perm_r = symbol("$(_sort)_perm_r")
    _sort_perm_by = symbol("$(_sort)_perm_by")

    b = _sort(a)
    @test issorted(b)
    (b, ix) = Base.(_sort_perm)(a)
    @test issorted(b)
    @test a[ix] == b

    b = Base.(_sort_r)(a)
    @test issorted_r(b)
    (b, ix) = Base.(_sort_perm_r)(a)
    @test issorted_r(b)
    @test a[ix] == b

    b = Base.(_sort_by)((x -> -10x), a)
    @test issorted_by((x -> -10x), b)
    (b, ix) = Base.(_sort_perm_by)((x -> -10x), a)
    @test issorted_by((x -> -10x), b)
    @test a[ix] == b
end

b = quicksort(a)
@test issorted(b)
b = Base.quicksort_r(a)
@test issorted_r(b)
b = Base.quicksort_by((x -> -10x), a)
@test issorted_by((x -> -10x), b)

@test select_r([3,6,30,1,9],2) == 9
@test select_by((x -> -x),[3,6,30,1,9],2) == 9
