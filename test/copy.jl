# This file is a part of Julia. License is MIT: http://julialang.org/license

mainres = ([4, 5, 3],
           [1, 5, 3])
bitres = ([true, true, false],
          [false, true, false])

chnlprod(x) = Channel(c->for i in x; put!(c,i); end)

for (dest, src, bigsrc, emptysrc, res) in [
    ([1, 2, 3], () -> [4, 5], () -> [1, 2, 3, 4, 5], () -> Int[], mainres),
    ([1, 2, 3], () -> 4:5, () -> 1:5, () -> 1:0, mainres),
    ([1, 2, 3], () -> chnlprod(4:5), () -> chnlprod(1:5), () -> chnlprod(1:0), mainres),
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

    @test_throws ArgumentError copy!(dest, 1, src(), 1, -1)

    @test_throws BoundsError copy!(dest, bigsrc())

    @test_throws BoundsError copy!(dest, 3, src())
    @test_throws BoundsError copy!(dest, 3, src(), 1)
    @test_throws BoundsError copy!(dest, 3, src(), 1, 2)

    @test_throws BoundsError copy!(dest, 1, src(), 2, 2)
end

# test behavior of shallow and deep copying
let a = Any[[1]], q = QuoteNode([1])
    ca = copy(a); dca = @inferred(deepcopy(a))
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

# issue #15250
let a1 = Core.svec(1, 2, 3, []), a2 = Core.svec(1, 2, 3)
    a3 = Core.svec(a1,a1)
    b1 = deepcopy(a1)
    @test a1 == b1
    @test a1 !== b1
    @test a1[4] !== b1[4]
    b2 = deepcopy(a2)
    @test a2 === b2
    b3 = deepcopy(a3)
    @test a3 == b3
    @test a3 !== b3
    @test a3[1] === a3[2]
end

# issue #16667
let x = BigInt[1:1000;], y = deepcopy(x), v
    # Finalize the original values to make sure the deep copy is indeed
    # independent
    for v in x
        finalize(v)
    end
    # Allocate some memory to make it more likely to trigger an error
    # if `deepcopy` went wrong
    x = BigInt[1:1000;]
    @test y == x
end
let x = BigFloat[1:1000;], y, z, v
    y, z = setprecision(2) do
        deepcopy(x), BigFloat[1:1000;]
    end
    for v in x
        finalize(v)
    end
    x = BigFloat[1:1000;]
    # Make sure the difference in precision doesn't affect deep copy
    @test y == x
    # Check that the setprecision indeed does something
    @test z != x
end

# issue #19921
type Foo19921
    a::String
end

type Bar19921
    foo::Foo19921
    fooDict::Dict{Foo19921, Int64}
end

@testset "issue 19921" begin
    for i = 1 : 100
        foo = Foo19921("foo")
        bar = Bar19921(foo, Dict(foo => 3))
        bar2 = deepcopy(bar)
        @test bar2.foo ∈ keys(bar2.fooDict)
        @test bar2.fooDict[bar2.foo] != nothing
    end
end

