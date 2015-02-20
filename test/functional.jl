# tests related to functional programming functions and styles

# map -- array.jl
@test isequal(map((x)->"$x"[end:end], 9:11), ["9", "0", "1"])
# TODO: @test map!()
# map -- ranges.jl
@test isequal(map(i->sqrt(i), 1:5), [sqrt(i) for i in 1:5])
@test isequal(map(i->sqrt(i), 2:6), [sqrt(i) for i in 2:6])

# map on ranges should evaluate first value only once (#4453)
let io=IOBuffer(3)
    map(x->print(io,x), 1:2)
    @test takebuf_string(io)=="12"
end

# map over Bottom[] should return Bottom[]
# issue #6719
@test isequal(typeof(map(x -> x, Array(Union(),0))), Array{Union(),1})

# maps of tuples (formerly in test/core.jl) -- tuple.jl
@test map((x,y)->x+y,(1,2,3),(4,5,6)) == (5,7,9)
@test map((x,y)->x+y,
            (100001,100002,100003),
            (100004,100005,100006)) == (200005,200007,200009)

# maps of strings (character arrays) -- string.jl
@test map((c)->char(c+1), "abcDEF") == "bcdEFG"

# filter -- array.jl
@test isequal(filter(x->(x>1), [0 1 2 3 2 1 0]), [2, 3, 2])
# TODO: @test_throws isequal(filter(x->x+1, [0 1 2 3 2 1 0]), [2, 3, 2])
@test isequal(filter(x->(x>10), [0 1 2 3 2 1 0]), [])
@test isequal(filter((ss)->length(ss)==3, ["abcd", "efg", "hij", "klmn", "opq"]), ["efg", "hij", "opq"])

# zip and filter iterators
# issue #4718
@test collect(filter(x->x[1], zip([true, false, true, false],"abcd"))) == [(true,'a'),(true,'c')]

# enumerate (issue #6284)
let b = IOBuffer("1\n2\n3\n"), a = []
    for (i,x) in enumerate(eachline(b))
        push!(a, (i,x))
    end
    @test a == [(1,"1\n"),(2,"2\n"),(3,"3\n")]
end

# zip eachline (issue #7369)
let zeb     = IOBuffer("1\n2\n3\n4\n5\n"),
    letters = ['a', 'b', 'c', 'd', 'e'],
    res     = []
    for (number, letter) in zip(eachline(zeb), letters)
        push!(res, (int(strip(number)), letter))
    end
    @test res == [(1, 'a'), (2, 'b'), (3, 'c'), (4, 'd'), (5, 'e')]
end

# rest
# ----
let s = "hello"
    _, st = next(s, start(s))
    @test collect(rest(s, st)) == ['e','l','l','o']
end

# countfrom
# ---------

let i = 0
    for j = countfrom(0, 2)
	@test j == i*2
	i += 1
	i <= 10 || break
    end
end

# take
# ----

let t = take(0:2:8, 10), i = 0
    @test length(collect(t)) == 5

    for j = t
	@test j == i*2
	i += 1
    end
    @test i == 5
end

let i = 0
    for j = take(0:2:100, 10)
	@test j == i*2
	i += 1
    end
    @test i == 10
end

# drop
# ----

let i = 0
    for j = drop(0:2:10, 2)
	@test j == (i+2)*2
	i += 1
    end
    @test i == 4
end

# cycle
# -----

let i = 0
    for j = cycle(0:3)
	@test j == i % 4
	i += 1
	i <= 10 || break
    end
end

# repeated
# --------

let i = 0
    for j = repeated(1, 10)
	@test j == 1
	i += 1
    end
    @test i == 10
end
let i = 0
    for j = repeated(1)
	@test j == 1
	i += 1
	i <= 10 || break
    end
end
