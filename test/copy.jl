# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random

mainres = ([4, 5, 3],
           [1, 5, 3])
bitres = ([true, true, false],
          [false, true, false])

chnlprod(x) = Channel(c->for i in x; put!(c,i); end)
@testset "copyto!" begin
    for (dest, src, bigsrc, emptysrc, res) in [
        ([1, 2, 3], () -> [4, 5], () -> [1, 2, 3, 4, 5], () -> Int[], mainres),
        ([1, 2, 3], () -> 4:5, () -> 1:5, () -> 1:0, mainres),
        ([1, 2, 3], () -> chnlprod(4:5), () -> chnlprod(1:5), () -> chnlprod(1:0), mainres),
        (falses(3), () -> trues(2), () -> trues(5), () -> trues(0), bitres)]

        @test copyto!(copy(dest), src()) == res[1]
        @test copyto!(copy(dest), 1, src()) == res[1]
        @test copyto!(copy(dest), 2, src(), 2) == res[2]
        @test copyto!(copy(dest), 2, src(), 2, 1) == res[2]

        @test copyto!(copy(dest), 99, src(), 99, 0) == dest

        @test copyto!(copy(dest), 1, emptysrc()) == dest
        x = emptysrc()
        exc = isa(x, AbstractArray) ? BoundsError : ArgumentError
        @test_throws exc copyto!(dest, 1, emptysrc(), 1)

        for idx in (0, 4)
            @test_throws BoundsError copyto!(dest, idx, src())
            @test_throws BoundsError copyto!(dest, idx, src(), 1)
            @test_throws BoundsError copyto!(dest, idx, src(), 1, 1)
            x = src()
            exc = isa(x, AbstractArray) ? BoundsError : ArgumentError
            @test_throws exc copyto!(dest, 1, x, idx)
            x = src()
            exc = isa(x, AbstractArray) ? BoundsError : ArgumentError
            @test_throws exc copyto!(dest, 1, x, idx, 1)
        end

        @test_throws ArgumentError copyto!(dest, 1, src(), 1, -1)

        @test_throws Union{BoundsError, ArgumentError} copyto!(dest, bigsrc())

        @test_throws Union{BoundsError, ArgumentError} copyto!(dest, 3, src())
        @test_throws Union{BoundsError, ArgumentError} copyto!(dest, 3, src(), 1)
        @test_throws Union{BoundsError, ArgumentError} copyto!(dest, 3, src(), 1, 2)

        @test_throws Union{BoundsError, ArgumentError} copyto!(dest, 1, src(), 2, 2)
    end
end

@testset "with CartesianIndices" begin
    let A = reshape(1:6, 3, 2), B = similar(A)
        RA = CartesianIndices(axes(A))
        copyto!(B, RA, A, RA)
        @test B == A
    end
    let A = reshape(1:6, 3, 2), B = zeros(8,8)
        RA = CartesianIndices(axes(A))
        copyto!(B, CartesianIndices((5:7,2:3)), A, RA)
        @test B[5:7,2:3] == A
        B[5:7,2:3] .= 0
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

    @test_throws ErrorException("deepcopy of Modules not supported") deepcopy(Base)

    # deepcopy recursive dicts
    x = Dict{Dict, Int}()
    x[x] = 0
    @test length(deepcopy(x)) == 1
end

@testset "issue #13124" begin
    a = rand(3, 5)
    b = (a,a)
    c = deepcopy(b)
    @test c[1] === c[2]
end

@testset "issue #31309" begin
    rgx1 = match(deepcopy(r""), "")
    @test rgx1.regex == r""
    @test rgx1.offset == 1
    @test rgx1.match == ""
    @test isempty(rgx1.offsets)
    @test isempty(rgx1.captures)
end

@testset "deepcopy for bits types" begin
    struct Immutable; x::Int; end
    mutable struct Mutable; x::Int; end

    @test deepcopy(Immutable(2)) === Immutable(2)
    @test deepcopy(Mutable(2))   !== Mutable(2)
    @inferred deepcopy(Immutable(2))
    @inferred deepcopy(Mutable(2))

    @test deepcopy(Dict(0 => 0))[0] == 0
end

# issue #30911
@test deepcopy(Array{Int,N} where N) == Array{Int,N} where N

# issue #14027
struct Nullable14027{T}
    hasvalue::Bool
    value::T

    Nullable14027{T}() where {T} = new(false)
end
@test !deepcopy(Nullable14027{Array}()).hasvalue

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

@testset "issue #16667" begin
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

    let d = IdDict(rand(2) => rand(2) for i = 1:100)
        d2 = deepcopy(d)
        for k in keys(d2)
            @test haskey(d2, k)
        end
        for k in keys(d)
            @test haskey(d, k)
        end
    end
end

# issue #17149
mutable struct Bar17149
end
let x = Bar17149()
    @test deepcopy(x) !== x
end

@testset "copying CodeInfo" begin
    _testfunc() = nothing
    ci,_ = code_typed(_testfunc, ())[1]
    ci.edges = [_testfunc]

    ci2 = copy(ci)
    # Test that edges are not shared
    @test ci2.edges !== ci.edges
end

@testset "issue #34025" begin
    s = [2 0; 0 3]
    r = ones(Int, 3, 3)
    @test copyto!(copy(r), s') == [2 3 1; 0 1 1; 0 1 1]
    @test copyto!(copy(r), s) == copyto!(copy(r), s') ==
          copyto!(copy(r)', s) == copyto!(copy(r)', s')
    r = ones(Int, 3, 3)
    s = [1 2 3 4]'
    @test copyto!(r, s) == [1 4 1; 2 1 1; 3 1 1]
    a = fill(1, 5)
    r = Base.IdentityUnitRange(-1:1)
    copyto!(a, r)
    @test a[1:3] == [-1, 0, 1]
end