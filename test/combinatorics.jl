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
