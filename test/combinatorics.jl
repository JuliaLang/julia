@assert sort([2,3,1]) == [1,2,3]
@assert sortr([2,3,1]) == [3,2,1]
@assert sortperm([2,3,1]) == ([1,2,3],[3,1,2])
@assert !issorted([2,3,1])
@assert issorted([1,2,3])
@assert reverse([2,3,1]) == [1,3,2]
@assert select([3,6,30,1,9],3) == 6
@assert sum(randperm(6)) == 21
@assert nthperm([0,1,2],3) == [1,0,2]

@assert search_sorted_first([1, 1, 2, 2, 3, 3], 0) == 1
@assert search_sorted_first([1, 1, 2, 2, 3, 3], 1) == 1
@assert search_sorted_first([1, 1, 2, 2, 3, 3], 2) == 3
@assert search_sorted_first([1, 1, 2, 2, 3, 3], 4) == 7
@assert search_sorted_first([1.0, 1, 2, 2, 3, 3], 2.5) == 5
@assert search_sorted_last([1, 1, 2, 2, 3, 3], 0) == 0
@assert search_sorted_last([1, 1, 2, 2, 3, 3], 1) == 2
@assert search_sorted_last([1, 1, 2, 2, 3, 3], 2) == 4
@assert search_sorted_last([1, 1, 2, 2, 3, 3], 4) == 6
@assert search_sorted_last([1.0, 1, 2, 2, 3, 3], 2.5) == 4
