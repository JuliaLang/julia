# This file is a part of Julia. License is MIT: https://julialang.org/license

mainres = ([4, 5, 3],
           [1, 5, 3])
bitres = ([true, true, false],
          [false, true, false])

chnlprod(x) = Channel(c->for i in x; put!(c,i); end)
@testset "copy!" begin
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
end

@testset "with CartesianRange" begin
    let A = reshape(1:6, 3, 2), B = similar(A)
        RA = CartesianRange(indices(A))
        copy!(B, RA, A, RA)
        @test B == A
    end
    let A = reshape(1:6, 3, 2), B = zeros(8,8)
        RA = CartesianRange(indices(A))
        copy!(B, CartesianRange((5:7,2:3)), A, RA)
        @test B[5:7,2:3] == A
        B[5:7,2:3] = 0
        @test all(x->x==0, B)
    end
end

@testset "shallow and deep copying" begin
    a = Any[[1]]
    q = QuoteNode([1])
    ca = copy(a); dca = @inferred(deepcopy(a))
    @test ca !== a
    @test ca[1] === a[1]
    @test dca !== a
    @test dca[1] !== a[1]
    @test deepcopy(q).value !== q.value
end

@testset "issue #13124" begin
    a = rand(3, 5)
    b = (a,a)
    c = deepcopy(b)
    @test c[1] === c[2]
end

# issue #14027
@test isnull(deepcopy(Nullable{Array}()))

@testset "issue #15250" begin
    a1 = Core.svec(1, 2, 3, [])
    a2 = Core.svec(1, 2, 3)
    a3 = Core.svec(a1, a1)
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

mutable struct Foo19921
    a::String
end

mutable struct Bar19921
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

# issue #16667 (Specific fixes for BigFloat, BigInt only)
let a1 = [big"1.5", big"1e42"], a2 = [big"123", big"456"]
    b1 = deepcopy(a1)
    @test a1[1] === b1[1]
    b2 = deepcopy(a2)
    @test a2[1] === b2[1]
end
