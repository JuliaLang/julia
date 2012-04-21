# tests related to functional programming functions and styles

# map -- array.jl
@assert isequal(map((x)->"$x"[end:end], [9:11]), ["9", "0", "1"])
# TODO: @assert map_to()

# maps of tuples (formerly in test/core.jl) -- tuple.jl
@assert map((x,y)->x+y,(1,2,3),(4,5,6)) == (5,7,9)
@assert map((x,y)->x+y,
            (100001,100002,100003),
            (100004,100005,100006)) == (200005,200007,200009)

# maps of strings (character arrays) -- string.jl
@assert map((c)->char(c+1), "abcDEF") == "bcdEFG"

# reduce -- reduce.jl
@assert reduce((x,y)->"($x+$y)", [9:11]) == "((9+10)+11)"
@assert reduce(max, [8 6 7 5 3 0 9]) == 9
@assert reduce(-, 1000, [1:5]) == (1000 - 1 - 2 - 3 - 4 - 5)

# mapreduce -- reduce.jl
@assert mapreduce(-, -, [-10 -9 -3]) == ((10 - 9) - 3)
@assert mapreduce((x,y)->"($x+$y)", (x)->x[1:3], ["abcd", "efgh", "01234"]) == "((abc+efg)+012)"

# filter -- array.jl
@assert isequal(filter(x->(x>1), [0 1 2 3 2 1 0]), [2, 3, 2])
# TODO: @assert_fails isequal(filter(x->x+1, [0 1 2 3 2 1 0]), [2, 3, 2])
@assert isequal(filter(x->(x>10), [0 1 2 3 2 1 0]), [])
@assert isequal(filter((ss)->length(ss)==3, ["abcd", "efg", "hij", "klmn", "opq"]), ["efg", "hij", "opq"])
