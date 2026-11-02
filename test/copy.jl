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

    v = rand(Float32, 4)
    a = Memory{Float32}(v)
    b = similar(a)
    copyto!(b, a)
    @test a == b

    c = Memory{Float32}(undef, 3)
    @test_throws BoundsError copyto!(c, a)
end

@testset "with CartesianIndices" begin
    let A = reshape(1:6, 3, 2), B = similar(A)
        RA = CartesianIndices(axes(A))
        copyto!(B, RA, A, RA)
        @test B == A
    end
    let A = reshape(1:6, 3, 2), B = zeros(8,8)
        RBs = Any[(5:7,2:3), (3:2:7,1:2:3), (6:-1:4,2:-1:1)]
        RAs = Any[axes(A), reverse.(axes(A))]
        for RB in RBs, RA in RAs
            copyto!(B, CartesianIndices(RB), A, CartesianIndices(RA))
            @test B[RB...] == A[RA...]
            B[RB...] .= 0
            @test all(iszero, B)
        end
    end
    let A = [reshape(1:6, 3, 2);;]
        copyto!(A, CartesianIndices((2:3,2)), A, CartesianIndices((2,2)))
        @test A[2:3,:] == [1 4;2 5]
    end
    @test copyto!(zeros(Int, 3, 3), CartesianIndices((1:3, 2:2)), [1, 2, 3], CartesianIndices((1:3, 1:1))) == [0 1 0; 0 2 0; 0 3 0]
    @test copyto!(zeros(Int, 2, 5), CartesianIndices((2:2, 1:2:5)), [1 2 3], CartesianIndices((1:1, 1:3))) == [0 0 0 0 0; 1 0 2 0 3]
    for src in (bitrand(70, 3), bitrand(2, 5), rand(Bool, 70, 3))
        dest = falses(80, 7)
        copyto!(dest, CartesianIndices((5:size(src, 1)+4, 2:size(src, 2)+1)), src, CartesianIndices(src))
        @test dest[5:size(src, 1)+4, 2:size(src, 2)+1] == src
        @test count(dest) == count(src)
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
        @test bar2.fooDict[bar2.foo] !== nothing
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
    if isdefined(ci, :edges)
        ci.edges = [_testfunc]

        ci2 = copy(ci)
        # Test that edges are not shared
        @test ci2.edges !== ci.edges
    end
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

@testset "issue #34889" begin
    s = [1, 2]
    @test copyto!(s, view(Int[],Int[])) == [1, 2]
    @test copyto!(s, Float64[]) == [1, 2]
    @test copyto!(s, String[]) == [1, 2] # No error
end

@testset "circular reference arrays" begin
    # issue 56775
    p = Any[nothing]
    p[1] = p
    p2 = deepcopy(p)
    @test p2 === p2[1]
    @test p2 !== p
end

@testset "deepcopy_internal arrays" begin
    @test (@inferred Base.deepcopy_internal(zeros(), IdDict())) == zeros()
end

@testset "deepcopy_internal inference" begin
    @inferred Base.deepcopy_internal(1, IdDict())
    @inferred Base.deepcopy_internal(1.0, IdDict())
    @inferred Base.deepcopy_internal(big(1), IdDict())
    @inferred Base.deepcopy_internal(big(1.0), IdDict())
    @inferred Base.deepcopy_internal('a', IdDict())
    @inferred Base.deepcopy_internal("abc", IdDict())
    @inferred Base.deepcopy_internal([1,2,3], IdDict())

    # structs without custom deepcopy_internal method
    struct Immutable2; x::Int; end
    mutable struct Mutable2; x::Int; end
    @inferred Base.deepcopy_internal(Immutable2(1), IdDict())
    @inferred Base.deepcopy_internal(Mutable2(1), IdDict())
end

@testset "`copyto!`'s unaliasing" begin
    a = view([1:3;], :)
    @test copyto!(a, 2, a, 1, 2) == [1;1:2;]
    a = [1:3;]
    @test copyto!(a, 2:3, 1:1, a, 1:2, 1:1) == [1;1:2;]
    b = falses(65); b[1] = true
    @test copyto!(b, view(b, 65:-1:1))[65]
end

struct BoolFlag
    b::Bool
end
Base.convert(::Type{Bool}, x::BoolFlag) = x.b

struct CopyLog <: AbstractVector{Int}
    data::Vector{Int}
end
Base.size(a::CopyLog) = size(a.data)
Base.getindex(a::CopyLog, i::Int) = a.data[i]
Base.setindex!(a::CopyLog, x, i::Int) = (a.data[i] = x; a)
Base.dataids(a::CopyLog) = Base.dataids(a.data)
Base.unaliascopy(a::CopyLog) = CopyLog(copy(a.data))

struct OtherLog <: AbstractVector{Int}
    data::Vector{Int}
end
Base.size(a::OtherLog) = size(a.data)
Base.getindex(a::OtherLog, i::Int) = a.data[i]
Base.setindex!(a::OtherLog, x, i::Int) = (a.data[i] = x; a)

const copylog_calls = Any[]
struct CopyLogStyle <: Base.CopyToStyle end
struct OtherLogStyle <: Base.CopyToStyle end
Base.CopyToStyle(::Type{CopyLog}) = CopyLogStyle()
Base.CopyToStyle(::Type{OtherLog}) = OtherLogStyle()
function Base.copyto_unaliased!(::CopyLogStyle, dest::AbstractArray, src::AbstractArray)
    push!(copylog_calls, (:whole, dest, src))
    return Base.copyto_unaliased!(Base.DefaultCopyToStyle(), dest, src)
end
function Base.copyto_unaliased!(::CopyLogStyle, dest::AbstractArray, Rdest, src::AbstractArray, Rsrc)
    push!(copylog_calls, (:region, Rdest, Rsrc))
    return Base.copyto_unaliased!(Base.DefaultCopyToStyle(), dest, Rdest, src, Rsrc)
end

@testset "CopyToStyle" begin
    empty!(copylog_calls)
    @test copyto!(CopyLog(zeros(Int, 4)), [1, 2, 3]) == [1, 2, 3, 0]
    @test copylog_calls[end][1] === :whole
    @test copyto!(zeros(Int, 4), CopyLog([1, 2, 3])) == [1, 2, 3, 0]
    @test copylog_calls[end][1] === :whole
    @test collect(CopyLog([1, 2])) == [1, 2]
    @test copylog_calls[end][1] === :whole
    @test copyto!(CopyLog(zeros(Int, 5)), 2, [1, 2, 3], 1, 3) == [0, 1, 2, 3, 0]
    @test copylog_calls[end] == (:region, 2:4, 1:3)
    @test copyto!(CopyLog(zeros(Int, 3)), CartesianIndices((2:3,)), [1, 2], CartesianIndices((1:2,))) == [0, 1, 2]
    @test copylog_calls[end] == (:region, CartesianIndices((2:3,)), CartesianIndices((1:2,)))

    empty!(copylog_calls)
    @test copyto!(CopyLog([1, 2]), Int[]) == [1, 2]
    @test copyto!(CopyLog([1, 2]), 1, Int[], 1, 0) == [1, 2]
    @test isempty(copylog_calls)
    @test_throws BoundsError copyto!(CopyLog([1, 2]), [1, 2, 3])
    @test_throws ArgumentError copyto!(CopyLog([1, 2]), 1, [1], 1, -1)
    @test isempty(copylog_calls)

    x = CopyLog([1, 2, 3, 4])
    @test copyto!(x, 2, x, 1, 3) == [1, 1, 2, 3]
    @test copylog_calls[end][1] === :region

    empty!(copylog_calls)
    @test copyto!(OtherLog(zeros(Int, 2)), CopyLog([1, 2])) == [1, 2]
    @test isempty(copylog_calls)
    @test copyto!(CopyLog(zeros(Int, 2)), CopyLog([1, 2])) == [1, 2]
    @test copylog_calls[end][1] === :whole

    let a = PermutedDimsArray(zeros(3, 3), (2, 1))
        @test copyto!(a, ones(2, 2)) == copyto!(zeros(3, 3), ones(2, 2))
    end
    let a = PermutedDimsArray(zeros(2, 3), (2, 1))
        @test copyto!(a, reshape(1:6, 3, 2)) == reshape(1:6, 3, 2)
    end

    let b = falses(70)
        @test copyto!(b, trues(65)) == [trues(65); falses(5)]
        @test copyto!(falses(5), view(trues(10), 2:6)) == trues(5)
        @test copyto!(falses(4), Bool[1, 0, 1]) == [true, false, true, false]
        @test copyto!(falses(4), PermutedDimsArray(Bool[1 0; 1 1], (2, 1))) == [true, false, true, true]
        @test copyto!(zeros(Bool, 3), BitVector([true, false, true])) == [true, false, true]
        flags = [BoolFlag(true), BoolFlag(false)]
        @test copyto!(falses(2), view(flags, :)) == [true, false]
        @test copyto!(falses(3), 2, view(flags, :), 1, 2) == [false, true, false]
        @test copyto!(falses(3), flags) == [true, false, false]
        @test copyto!(falses(3), 2, flags, 1, 2) == [false, true, false]
    end
    let src = [isodd(i ÷ 3) for i in 1:130]
        for T in (Int, Int8, Float64)
            @test copyto!(falses(130), T.(src)) == src
            @test copyto!(trues(140), 7, T.(src), 3, 120) == [trues(6); src[3:122]; trues(14)]
        end
        @test_throws InexactError copyto!(falses(3), [0, 2, 1])
        @test_throws InexactError copyto!(falses(3), 1, [0.0, 0.5, 1.0], 1, 3)
    end
    let bits = BitMatrix([isodd(i + 2j) for i in 1:5, j in 1:7])
        @test Base._copyto_style(falses(7, 5), PermutedDimsArray(bits, (2, 1))) isa Base.BitArrayCopyToStyle
        @test copyto!(falses(7, 5), PermutedDimsArray(bits, (2, 1))) == permutedims(bits)
        dest = PermutedDimsArray(falses(5, 7), (2, 1))
        @test Base._copyto_style(dest, permutedims(bits)) isa Base.PermutedDimsArrays.PermutedDimsCopyToStyle
        @test copyto!(dest, permutedims(bits)) == permutedims(bits)
    end
end

Base.CopyToStyle(::OtherLogStyle, ::CopyLogStyle) = CopyLogStyle()

@testset "CopyToStyle rules" begin
    empty!(copylog_calls)
    @test copyto!(OtherLog(zeros(Int, 2)), CopyLog([1, 2])) == [1, 2]
    @test copylog_calls[end][1] === :whole
    @test copyto!(CopyLog(zeros(Int, 2)), OtherLog([1, 2])) == [1, 2]
    @test length(copylog_calls) == 2
end

@testset "`deepcopy` a `GenericCondition`" begin
    a = Base.GenericCondition(ReentrantLock())
    # Test printing
    @test repr(a) == "Base.GenericCondition(ReentrantLock())"
    @test !islocked(a.lock)
    lock(a.lock)
    @test islocked(a.lock)
    b = deepcopy(a)
    @test typeof(a) === typeof(b)
    @test a != b
    @test a !== b
    @test typeof(a.lock) === typeof(b.lock)
    @test a.lock != b.lock
    @test a.lock !== b.lock
    @test islocked(a.lock)
    @test !islocked(b.lock)
    @inferred deepcopy(a)
    @inferred deepcopy(a.lock)
end
