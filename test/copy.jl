# This file is a part of Julia. License is MIT: http://julialang.org/license

mainres = ([4, 5, 3],
           [1, 5, 3])
bitres = ([true, true, false],
          [false, true, false])

tskprod(x) = @task for i in x; produce(i); end

for (dest, src, bigsrc, emptysrc, res) in [
    ([1, 2, 3], () -> [4, 5], () -> [1, 2, 3, 4, 5], () -> Int[], mainres),
    ([1, 2, 3], () -> 4:5, () -> 1:5, () -> 1:0, mainres),
    ([1, 2, 3], () -> tskprod(4:5), () -> tskprod(1:5), () -> tskprod(1:0), mainres),
    (falses(3), () -> trues(2), () -> trues(5), () -> trues(0), bitres)]

    @test copy!(copy(dest), src()) == res[1]
    @test copy!(copy(dest), 1, src()) == res[1]
    @test copy!(copy(dest), 2, src(), 2) == res[2]
    @test copy!(copy(dest), 2, src(), 2, 1) == res[2]

    @test copy!(copy(dest), 99, src(), 99, 0) == dest

    @test copy!(copy(dest), 1, emptysrc()) == dest
    x = emptysrc()
    exc = isa(x, AbstractArray) ? BoundsError : ArgumentError
    @test_throws exc copy!(dest, 1, emptysrc(), 1)

    for idx in (0, 4)
        @test_throws BoundsError copy!(dest, idx, src())
        @test_throws BoundsError copy!(dest, idx, src(), 1)
        @test_throws BoundsError copy!(dest, idx, src(), 1, 1)
        x = src()
        exc = isa(x, AbstractArray) ? BoundsError : ArgumentError
        @test_throws exc copy!(dest, 1, x, idx)
        x = src()
        exc = isa(x, AbstractArray) ? BoundsError : ArgumentError
        @test_throws exc copy!(dest, 1, x, idx, 1)
    end

    @test_throws BoundsError copy!(dest, 1, src(), 1, -1)

    @test_throws BoundsError copy!(dest, bigsrc())

    @test_throws BoundsError copy!(dest, 3, src())
    @test_throws BoundsError copy!(dest, 3, src(), 1)
    @test_throws BoundsError copy!(dest, 3, src(), 1, 2)

    @test_throws BoundsError copy!(dest, 1, src(), 2, 2)
end

# test behavior of shallow and deep copying
let a = Any[[1]], q = QuoteNode([1])
    ca = copy(a); dca = deepcopy(a)
    @test ca !== a
    @test ca[1] === a[1]
    @test dca !== a
    @test dca[1] !== a[1]
    @test deepcopy(q).value !== q.value
end

# issue #13124
let a = rand(3, 5)
    b = (a,a)
    c = deepcopy(b)
    @test c[1] === c[2]
end

# issue #14027
@test isnull(deepcopy(Nullable{Array}()))
