# tests related to functional programming functions and styles

# map -- array.jl
@test isequal(map((x)->"$x"[end:end], [9:11]), ["9", "0", "1"])
# TODO: @test map_to()
# map -- ranges.jl
@test isequal(map(i->sqrt(i), 1:5), [sqrt(i) for i in 1:5])
@test isequal(map(i->sqrt(i), 2:6), [sqrt(i) for i in 2:6])

# maps of tuples (formerly in test/core.jl) -- tuple.jl
@test map((x,y)->x+y,(1,2,3),(4,5,6)) == (5,7,9)
@test map((x,y)->x+y,
            (100001,100002,100003),
            (100004,100005,100006)) == (200005,200007,200009)

# maps of strings (character arrays) -- string.jl
@test map((c)->char(c+1), "abcDEF") == "bcdEFG"

# reduce -- reduce.jl
@test reduce((x,y)->"($x+$y)", [9:11]) == "((9+10)+11)"
@test reduce(max, [8 6 7 5 3 0 9]) == 9
@test reduce(-, 1000, [1:5]) == (1000 - 1 - 2 - 3 - 4 - 5)

# mapreduce -- reduce.jl
@test mapreduce(-, -, [-10 -9 -3]) == ((10 - 9) - 3)
@test mapreduce((x,y)->"($x+$y)", (x)->x[1:3], ["abcd", "efgh", "01234"]) == "((abc+efg)+012)"

# filter -- array.jl
@test isequal(filter(x->(x>1), [0 1 2 3 2 1 0]), [2, 3, 2])
# TODO: @test_fails isequal(filter(x->x+1, [0 1 2 3 2 1 0]), [2, 3, 2])
@test isequal(filter(x->(x>10), [0 1 2 3 2 1 0]), [])
@test isequal(filter((ss)->length(ss)==3, ["abcd", "efg", "hij", "klmn", "opq"]), ["efg", "hij", "opq"])
