# This file is a part of Julia. License is MIT: https://julialang.org/license

isdefined(Main, :OffsetArrays) || @eval Main include("testhelpers/OffsetArrays.jl")
using .Main.OffsetArrays

struct BitPerm_19352
    p::NTuple{8,UInt8}
    function BitPerm(p::NTuple{8,UInt8})
        sort(collect(p)) != 0:7 && error("$p is not a permutation of 0:7")
        new(p)
    end
    BitPerm_19352(xs::Vararg{Any,8}) = BitPerm(map(UInt8, xs))
end

@testset "conversion and construction" begin
    @test convert(Tuple, ()) === ()
    @test convert(Tuple, (1, 2)) === (1, 2)
    @test convert(Tuple, (1.0, 2)) === (1.0, 2)

    @test convert(NTuple, ()) === ()
    @test convert(Tuple{}, ()) === ()
    @test convert(Tuple{Vararg{Int}}, ()) === ()
    @test convert(Tuple{Vararg{T}} where T<:Integer, ()) === ()

    @test convert(NTuple{3, Int}, (1, 2, 3)) === (1, 2, 3)
    @test convert(NTuple, (1, 2, 3)) === (1, 2, 3)
    @test convert(Tuple{Vararg{Int}}, (1, 2, 3)) === (1, 2, 3)
    @test convert(Tuple{Int, Vararg{Int}}, (1, 2, 3)) === (1, 2, 3)
    @test convert(Tuple{Vararg{T}} where T<:Integer, (1, 2, 3)) === (1, 2, 3)
    @test convert(Tuple{T, Vararg{T}} where T<:Integer, (1, 2, 3)) === (1, 2, 3)
    @test convert(Tuple{Int, Int, Float64}, (1, 2, 3)) === (1, 2, 3.0)

    @test convert(Tuple{Float64, Int, UInt8}, (1.0, 2, 0x3)) === (1.0, 2, 0x3)
    @test convert(Tuple{Vararg{Real}}, (1.0, 2, 0x3)) === (1.0, 2, 0x3)
    @test convert(Tuple{Vararg{Integer}}, (1.0, 2, 0x3)) === (1, 2, 0x3)
    @test convert(Tuple{Vararg{Int}}, (1.0, 2, 0x3)) === (1, 2, 3)
    @test convert(Tuple{Int, Vararg{Int}}, (1.0, 2, 0x3)) === (1, 2, 3)
    @test convert(NTuple{3, Int}, (1.0, 2, 0x3)) === (1, 2, 3)
    @test convert(Tuple{Int, Int, Float64}, (1.0, 2, 0x3)) === (1, 2, 3.0)

    @test convert(Tuple{Vararg{AbstractFloat}}, (2,)) == (2.0,)
    @test convert(Tuple{Int, Vararg{AbstractFloat}}, (-9.0+0im, 2,)) == (-9, 2.0,)
    let x = @inferred(convert(Tuple{Integer, UInt8, UInt16, UInt32, Int, Vararg{Real}}, (2.0, 3, 5, 6.0, 42, 3.0+0im)))
        @test x == (2, 0x03, 0x0005, 0x00000006, 42, 3.0)
    end
    for x in (Int(2), UInt8(3), UInt16(5), UInt32(6), 42, 5.0, 3.0+0im)
        @test (x,) == @inferred Tuple(x)
    end

    @test_throws MethodError convert(Tuple{Int}, ())
    @test_throws MethodError convert(Tuple{Any}, ())
    @test_throws MethodError convert(Tuple{Int, Vararg{Int}}, ())
    @test_throws MethodError convert(Tuple{}, (1, 2, 3))
    @test_throws MethodError convert(Tuple{}, (1.0, 2, 3))
    @test_throws MethodError convert(NTuple{3, Int}, ())
    @test_throws MethodError convert(NTuple{3, Int}, (1, 2))
    @test_throws MethodError convert(NTuple{3, Int}, (1, 2, 3, 4))
    @test_throws MethodError convert(Tuple{Int, Int, Float64}, ())
    @test_throws MethodError convert(Tuple{Int, Int, Float64}, (1, 2))
    @test_throws MethodError convert(Tuple{Int, Int, Float64}, (1, 2, 3, 4))
    # #17198
    @test_throws MethodError convert(Tuple{Int}, (1.0, 2.0, 3.0))
    # #21238
    @test_throws MethodError convert(Tuple{Int, Int, Int}, (1, 2))
    # issue #26589
    @test_throws MethodError convert(NTuple{4}, (1.0,2.0,3.0,4.0,5.0))
    # issue #44179
    @test_throws TypeError NTuple{3}([1, nothing, nothing])
    @test_throws TypeError NTuple{3}([nothing, 1, nothing])
    # issue #31824
    @test convert(NTuple, (1, 1.0)) === (1, 1.0)
    let T = Tuple{Vararg{T}} where T<:Integer, v = (1.0, 2, 0x3)
        @test convert(T, v) === (1, 2, 0x3)
    end
    let T = Tuple{T, Vararg{T}} where T<:Integer, v = (1.0, 2, 0x3)
        @test convert(T, v) === (1, 2, 0x3)
    end
    function f31824(input...)
        b::NTuple = input
        return b
    end
    @test f31824(1, 2, 3) === (1, 2, 3)

    # PR #15516
    @test Tuple{Char,Char}("za") === ('z','a')
    @test_throws ArgumentError Tuple{Char,Char}("z")

    @test NTuple{20,Int}(Iterators.countfrom(2)) === (2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)
    @test NTuple{20,Float64}(Iterators.countfrom(2)) === (2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15.,16.,17.,18.,19.,20.,21.)
    @test_throws ArgumentError NTuple{20,Int}([1,2])

    @test Tuple{Vararg{Float32}}(Float64[1,2,3]) === (1.0f0, 2.0f0, 3.0f0)
    @test Tuple{Int,Vararg{Float32}}(Float64[1,2,3]) === (1, 2.0f0, 3.0f0)
    @test Tuple{Int,Vararg{Any}}(Float64[1,2,3]) === (1, 2.0, 3.0)
    @test Tuple(fill(1.,5)) === (1.0,1.0,1.0,1.0,1.0)
    @test_throws MethodError convert(Tuple, fill(1.,5))

    @testset "ambiguity between tuple constructors #20990" begin
        Tuple16Int = Tuple{Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int}
        tuple16int = (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        @test Tuple16Int(tuple16int) isa Tuple16Int
    end

    @testset "side effect in tuple constructor #19352" begin
        @test BitPerm_19352(0,2,4,6,1,3,5,7).p[2] == 0x02
    end

    @testset "n_uninitialized" begin
        @test Tuple.name.n_uninitialized == 0
        @test Core.Compiler.datatype_min_ninitialized(Tuple{Int,Any}) == 2
        @test Core.Compiler.datatype_min_ninitialized(Tuple) == 0
        @test Core.Compiler.datatype_min_ninitialized(Tuple{Int,Vararg{Any}}) == 1
        @test Core.Compiler.datatype_min_ninitialized(Tuple{Any,Any,Vararg{Any}}) == 2
        @test Core.Compiler.datatype_min_ninitialized(Tuple{Any,Any,Vararg{Any,3}}) == 5
    end

    @test empty((1, 2.0, "c")) === ()

    # issue #28915
    @test convert(Union{Tuple{}, Tuple{Int}}, (1,)) === (1,)

    @testset "one-element containers" begin
        r = Ref(3)
        @test (3,) === @inferred Tuple(r)
        z = Array{Float64,0}(undef); z[] = 3.0
        @test (3.0,) === @inferred Tuple(z)
    end
end

@testset "size" begin
    @test length(()) == 0
    @test length((1,)) === 1
    @test length((1,2)) === 2

    @test_throws ArgumentError Base.front(())
    @test_throws ArgumentError Base.tail(())
    @test_throws ArgumentError first(())

    @test lastindex(()) === 0
    @test lastindex((1,)) === 1
    @test lastindex((1,2)) === 2

    @test size((), 1) === 0
    @test size((1,), 1) === 1
    @test size((1,2), 1) === 2

    # @test_throws ArgumentError size((), 2)
    # @test_throws ArgumentError size((1,), 2)
    # @test_throws ArgumentError size((1,2), 2)
end

@testset "indexing" begin
    @test getindex((1,), 1) === 1
    @test getindex((1,2), 2) === 2
    @test_throws BoundsError getindex((), 1)
    @test_throws BoundsError getindex((1,2), 0)
    @test_throws BoundsError getindex((1,2), -1)

    @test getindex((5,6,7,8), [1,2,3]) === (5,6,7)
    @test_throws BoundsError getindex((1,2), [3,4])

    @test getindex((5,6,7,8), [true, false, false, true]) === (5,8)
    @test_throws BoundsError getindex((5,6,7,8), [true, false, false, true, true])

    @test getindex((5,6,7,8), []) === ()
    @test getindex((1,9,9,3),:) === (1,9,9,3)
    @test getindex((),:) === ()
    @test getindex((1,),:) === (1,)

    @testset "boolean arrays" begin
        # issue #19719
        @test_throws BoundsError (1,2,3)[falses(4)]
        @test_throws BoundsError (1,2,3)[[false,false,true,true]]
        @test_throws BoundsError (1,2,3)[trues(2)]
        @test_throws BoundsError (1,2,3)[falses(2)]
        @test_throws BoundsError ()[[false]]
        @test_throws BoundsError ()[[true]]
    end

    @testset "Multidimensional indexing (issue #20453)" begin
        @test_throws MethodError (1,)[]
        @test_throws MethodError (1,1,1)[1,1]
    end

    @testset "get() method for Tuple (Issue #40809)" begin
        @test get((5, 6, 7), 1, 0) == 5
        @test get((), 5, 0) == 0
        @test get((1,), 3, 0) == 0
        @test get(()->0, (5, 6, 7), 1) == 5
        @test get(()->0, (), 4) == 0
        @test get(()->0, (1,), 3) == 0
    end
end

@testset "fill to length" begin
    ## filling to specified length
    @test @inferred(Base.fill_to_length((1,2,3), -1, Val(5))) == (1,2,3,-1,-1)
    @test_throws ArgumentError Base.fill_to_length((1,2,3), -1, Val(2))
end

@testset "iterating" begin
    @test iterate(()) === nothing
    t = (1,2,3)
    y1 = iterate(t)
    y2 = iterate(t, y1[2])
    y3 = iterate(t, y2[2])
    @test y3 !== nothing
    @test iterate(t, y3[2]) === nothing

    @test eachindex((2,5,"foo")) === Base.OneTo(3)
    @test eachindex((2,5,"foo"), (1,2,5,7)) === Base.OneTo(4)
end


@testset "element type" begin
    @test eltype((1,2,3)) === Int
    @test eltype((1.0,2.0,3.0)) <: AbstractFloat
    @test eltype((true, false)) === Bool
    @test eltype((1, 2.0, false)) === typejoin(Int, Float64, Bool)
    @test eltype(()) === Union{}
    @test eltype(Tuple{Int, Float64, Vararg{Bool}}) === typejoin(Int, Float64, Bool)
    @test eltype(Tuple{Int, T, Vararg{Bool}} where T <: AbstractFloat) ===
        typejoin(Int, AbstractFloat, Bool)
    @test eltype(Tuple{Int, Bool, Vararg{T}} where T <: AbstractFloat) ===
        typejoin(Int, AbstractFloat, Bool)
    @test eltype(Union{Tuple{Int, Float64}, Tuple{Vararg{Bool}}}) ===
        typejoin(Int, Float64, Bool)
    @test eltype(Tuple{Int, Missing}) === Union{Missing, Int}
    @test eltype(Tuple{Int, Nothing}) === Union{Nothing, Int}
end

@testset "map with Nothing and Missing" begin
    for T in (Nothing, Missing)
        x = [(1, T()), (1, 2)]
        y = map(v -> (v[1], v[2]), [(1, T()), (1, 2)])
        @test y isa Vector{Tuple{Int, Any}}
        @test isequal(x, y)
    end
    y = map(v -> (v[1], v[1] + v[2]), [(1, missing), (1, 2)])
    @test y isa Vector{Tuple{Int, Any}}
    @test isequal(y, [(1, missing), (1, 3)])
end

@testset "mapping" begin
    foo() = 2
    foo(x) = 2x
    foo(x, y) = x + y
    foo(x, y, z) = x + y + z
    longtuple = ntuple(identity, 20)
    vlongtuple = ntuple(identity, 33)

    @testset "1 argument" begin
        @test map(foo, ()) === ()
        @test map(foo, (1,)) === (2,)
        @test map(foo, (1,2)) === (2,4)
        @test map(foo, (1,2,3,4)) === (2,4,6,8)
        @test map(foo, longtuple) === ntuple(i->2i,20)
        @test map(foo, vlongtuple) === ntuple(i->2i,33)
    end

    @testset "2 arguments" begin
        @test map(foo, (), ()) === ()
        @test map(foo, (1,), (1,)) === (2,)
        @test map(foo, (1,2), (1,2)) === (2,4)
        @test map(foo, (1,2,3,4), (1,2,3,4)) === (2,4,6,8)
        @test map(foo, longtuple, longtuple) === ntuple(i->2i,20)
        @test map(foo, vlongtuple, vlongtuple) === ntuple(i->2i,33)
        @test_throws BoundsError map(foo, (), (1,))
        @test_throws BoundsError map(foo, (1,), ())
    end

    @testset "n arguments" begin
        @test map(foo, (), (), ()) === ()
        @test map(foo, (1,), (1,), (1,)) === (3,)
        @test map(foo, (1,2), (1,2), (1,2)) === (3,6)
        @test map(foo, (1,2,3,4), (1,2,3,4), (1,2,3,4)) === (3,6,9,12)
        @test map(foo, longtuple, longtuple, longtuple) === ntuple(i->3i,20)
        @test map(foo, vlongtuple, vlongtuple, vlongtuple) === ntuple(i->3i,33)
        @test_throws BoundsError map(foo, (), (1,), (1,))
        @test_throws BoundsError map(foo, (1,), (1,), ())
    end
end

@testset "foreach" begin
    longtuple = ntuple(identity, 33)

    @testset "1 argument" begin
        foo(x) = push!(a, x)

        a = []
        foreach(foo, ())
        @test a == []

        a = []
        foreach(foo, (1,))
        @test a == [1]

        a = []
        foreach(foo, longtuple)
        @test a == [longtuple...]
    end

    @testset "n arguments" begin
        foo(x, y) = push!(a, (x, y))

        a = []
        foreach(foo, (), ())
        @test a == []

        a = []
        foreach(foo, (1,), (2,))
        @test a == [(1, 2)]

        a = []
        foreach(foo, longtuple, longtuple)
        @test a == [(x, x) for x in longtuple]
    end
end

@testset "mapfoldl" begin
    @test (((1=>2)=>3)=>4) == foldl(=>, (1,2,3,4)) ==
          mapfoldl(identity, =>, (1,2,3,4)) == mapfoldl(abs, =>, (-1,-2,-3,-4))
    @test mapfoldl(abs, =>, (-1,-2,-3,-4), init=-10) == ((((-10=>1)=>2)=>3)=>4)
    @test mapfoldl(abs, =>, (), init=-10) == -10
    @test mapfoldl(abs, Pair{Any,Any}, (-30:-1...,)) == mapfoldl(abs, Pair{Any,Any}, [-30:-1...,])
    @test_throws "reducing over an empty collection" mapfoldl(abs, =>, ())
end

@testset "filter" begin
    @test filter(isodd, (1,2,3)) == (1, 3)
    @test filter(isequal(2), (true, 2.0, 3)) === (2.0,)
    @test filter(Returns(true), ()) == ()
    @test filter(identity, (true,)) === (true,)
    longtuple = ntuple(identity, 20)
    @test filter(iseven, longtuple) == ntuple(i->2i, 10)
    @test filter(x -> x<2, (longtuple..., 1.5)) === (1, 1.5)
end

@testset "comparison and hash" begin
    @test isequal((), ())
    @test isequal((1,2,3), (1,2,3))
    @test !isequal((1,2,3), (1,2,4))
    @test !isequal((1,2,3), (1,2))

    @test ==((), ())
    @test ==((1,2,3), (1,2,3))
    @test !==((1,2,3), (1,2,4))
    @test !==((1,2,3), (1,2))

    @test (1,2) < (1,3)
    @test (1,) < (1,2)
    @test !((1,2) < (1,2))
    @test (2,1) > (1,2)

    @test isless((1,2), (1,3))
    @test isless((1,), (1,2))
    @test !isless((1,2), (1,2))
    @test !isless((2,1), (1,2))

    @test hash(()) === Base.tuplehash_seed
    @test hash((1,)) === hash(1, Base.tuplehash_seed)
    @test hash((1,2)) === hash(1, hash(2, Base.tuplehash_seed))

    # Test Any32 methods
    t = ntuple(identity, 32)
    @test isequal((t...,1,2,3), (t...,1,2,3))
    @test !isequal((t...,1,2,3), (t...,1,2,4))
    @test !isequal((t...,1,2,3), (t...,1,2))

    @test ==((t...,1,2,3), (t...,1,2,3))
    @test !==((t...,1,2,3), (t...,1,2,4))
    @test !==((t...,1,2,3), (t...,1,2))

    @test (t...,1,2) < (t...,1,3)
    @test (t...,1,) < (t...,1,2)
    @test !((t...,1,2) < (t...,1,2))
    @test (t...,2,1) > (t...,1,2)

    @test isless((t...,1,2), (t...,1,3))
    @test isless((t...,1,), (t...,1,2))
    @test !isless((t...,1,2), (t...,1,2))
    @test !isless((t...,2,1), (t...,1,2))

    @test hash(t) === foldr(hash, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,(),UInt(0)])
end

@testset "functions" begin
    @test isempty(())
    @test !isempty((1,))

    @test reverse(()) === ()
    @test reverse((1,2,3)) === (3,2,1)
end
# issue #21697
@test_throws ArgumentError ntuple(identity, -1)


@testset "specialized reduction" begin
    @test sum((1,2,3)) === 6

    @test prod(()) === 1
    @test prod((1,2,3)) === 6

    # issue 39182
    @test sum((0xe1, 0x1f)) === sum([0xe1, 0x1f])
    @test sum((Int8(3),)) === Int(3)
    @test sum((UInt8(3),)) === UInt(3)
    @test sum((3,)) === Int(3)
    @test sum((3.0,)) === 3.0
    @test sum(("a",)) == sum(["a"])
    @test sum((0xe1, 0x1f), init=0x0) == sum([0xe1, 0x1f], init=0x0)

    # issue 39183
    @test prod((Int8(100), Int8(100))) === 10000
    @test prod((Int8(3),)) === Int(3)
    @test prod((UInt8(3),)) === UInt(3)
    @test prod((3,)) === Int(3)
    @test prod((3.0,)) === 3.0
    @test prod(("a",)) == prod(["a"])
    @test prod((0xe1, 0x1f), init=0x1) == prod([0xe1, 0x1f], init=0x1)

    @testset "all" begin
        @test all(()) === true
        @test all((false,)) === false
        @test all((true,)) === true
        @test all((true, true)) === true
        @test all((true, false)) === false
        @test all((false, false)) === false
        @test all((missing, true)) === missing
        @test all((true, missing)) === missing
        @test all((missing, false)) === false
        @test all((false, missing)) === false
        @test all((missing, true, false)) === false
        @test_throws TypeError all((missing, 3.2, true))
        ts = (missing, true, false)
        @test @allocated(all(ts)) == 0  # PR #44063
        @test (@inferred (()->all((missing, true)))()) === missing
        @test (@inferred (()->all((true, missing)))()) === missing
        @test (@inferred (()->all((missing, false)))()) === false
        @test (@inferred (()->all((false, missing)))()) === false
        @test (@inferred (()->all((missing, true, false)))()) === false
    end

    @testset "any" begin
        @test any(()) === false
        @test any((true,)) === true
        @test any((false,)) === false
        @test any((true, true)) === true
        @test any((true, false)) === true
        @test any((false, false)) === false
        @test any((false,false,false)) === false
        @test any((true,false,false)) === true
        @test any((false,true,false)) === true
        @test any((false,false,true)) === true
        @test any((true,true,false)) === true
        @test any((true,false,true)) === true
        @test any((true,true,false)) === true
        @test any((true,true,true)) === true
        @test any((missing, true)) === true
        @test any((true, missing)) === true
        @test any((missing, false)) === missing
        @test any((false, missing)) === missing
        @test any((missing, true, false)) === true
        @test any((missing, false, false)) === missing
        @test_throws TypeError any((missing, 3.2, true))
        ts = (missing, true, false)
        @test @allocated(any(ts)) == 0  # PR #44063
        @test (@inferred (()->any((missing, true)))()) === true
        @test (@inferred (()->any((true, missing)))()) === true
        @test (@inferred (()->any((missing, false)))()) === missing
        @test (@inferred (()->any((false, missing)))()) === missing
        @test (@inferred (()->any((missing, true, false)))()) === true
        @test (@inferred (()->any((missing, false, false)))()) === missing
    end
end

@testset "accumulate" begin
    @test @inferred(cumsum(())) == ()
    @test @inferred(cumsum((1, 2, 3))) == (1, 3, 6)
    @test @inferred(cumprod((1, 2, 3))) == (1, 2, 6)
    @test @inferred(accumulate(+, (1, 2, 3); init=10)) == (11, 13, 16)
    op(::Nothing, ::Any) = missing
    op(::Missing, ::Any) = nothing
    @test @inferred(accumulate(op, (1, 2, 3, 4); init = nothing)) ===
          (missing, nothing, missing, nothing)
end

@testset "ntuple" begin
    nttest1(x::NTuple{n, Int}) where {n} = n
    @test nttest1(()) == 0
    @test nttest1((1, 2)) == 2
    @test NTuple <: Tuple
    @test (NTuple{T, Int32} where T) <: Tuple{Vararg{Int32}}
    @test !((NTuple{T, Int32} where T) <: Tuple{Int32, Vararg{Int32}})
    @test Tuple{Vararg{Int32}} <: (NTuple{T, Int32} where T)
    @test Tuple{Int32, Vararg{Int32}} <: (NTuple{T, Int32} where T)
    @test @inferred(ntuple(abs2, Val(0))) == ()
    @test @inferred(ntuple(abs2, Val(2))) == (1, 4)
    @test @inferred(ntuple(abs2, Val(3))) == (1, 4, 9)
    @test @inferred(ntuple(abs2, Val(4))) == (1, 4, 9, 16)
    @test @inferred(ntuple(abs2, Val(5))) == (1, 4, 9, 16, 25)
    @test @inferred(ntuple(abs2, Val(6))) == (1, 4, 9, 16, 25, 36)
    # issue #21697
    @test_throws ArgumentError ntuple(abs2, Val(-1))

    # issue #12854
    @test_throws TypeError ntuple(identity, Val(1:2))

    for n = 0:20
        t = ntuple(identity, n)
        @test length(t) == n
        for i = 1:n
            @test t[i] == i
        end
    end
    # issue #21697
    @test_throws ArgumentError ntuple(identity, -1)

    # PR #21446
    for n = 0:15
        @test ntuple(identity, Val(n)) == ntuple(identity, n)
    end
end

struct A_15703{N}
    keys::NTuple{N, Int}
end

struct B_15703
    x::A_15703
end
@testset "issue #15703" begin
    function bug_15703(xs...)
        [x for x in xs]
    end

    function test_15703()
        s = (1,)
        a = A_15703(s)
        ss = B_15703(a).x.keys
        @test ss === s
        bug_15703(ss...)
    end

    test_15703()
end

@testset "#21026" begin
    # https://github.com/JuliaLang/julia/issues/21026#issuecomment-317113307
    VecTuple21026{T} = Tuple{VecElement{T}}
    @test convert(VecTuple21026, (1,)) === (VecElement(1),)

    @test convert(Tuple{Complex{T}, Complex{T}} where T<:Real, (1, 2)) ===
        (Complex(1), Complex(2))
    @test convert(Tuple{Complex{T}, Complex{T}} where T<:Real, (1, 2.0)) ===
        (Complex(1), Complex(2.0))
    @test convert(Tuple{Complex, Complex}, (1, 2)) ===
        (Complex(1), Complex(2))
    @test convert(Tuple{Complex, Complex}, (1, 2.0)) ===
        (Complex(1), Complex(2.0))
end

@testset "issue 24707" begin
    @test eltype(Tuple{Vararg{T}} where T<:Integer) >: Integer
end

@testset "find" begin
    @test findall(isequal(1), (1, 2)) == [1]
    @test findall(isequal(1), (1, 1)) == [1, 2]
    @test isempty(findall(isequal(1), ()))
    @test isempty(findall(isequal(1), (2, 3)))

    @test findfirst(isequal(1), (1, 2)) == 1
    @test findlast(isequal(1), (1, 2)) == 1
    @test findfirst(isequal(1), (1, 1)) == 1
    @test findlast(isequal(1), (1, 1)) == 2
    @test findfirst(isequal(1), ()) === nothing
    @test findlast(isequal(1), ()) === nothing
    @test findfirst(isequal(1), (2, 3)) === nothing
    @test findlast(isequal(1), (2, 3)) === nothing

    @test findnext(isequal(1), (1, 2), 1) == 1
    @test findprev(isequal(1), (1, 2), 2) == 1
    @test findnext(isequal(1), (1, 1), 2) == 2
    @test findprev(isequal(1), (1, 1), 1) == 1
    @test findnext(isequal(1), (2, 3), 1) === nothing
    @test findprev(isequal(1), (2, 3), 2) === nothing

    @testset "issue 32568" begin
        @test findnext(isequal(1), (1, 2), big(1)) isa Int
        @test findprev(isequal(1), (1, 2), big(2)) isa Int
        @test findnext(isequal(1), (1, 1), UInt(2)) isa Int
        @test findprev(isequal(1), (1, 1), UInt(1)) isa Int
    end

    # recursive implementation should allow constant-folding for small tuples
    @test Base.return_types() do
        findfirst(==(2), (1.0,2,3f0))
    end == Any[Int]
    @test Base.return_types() do
        findfirst(==(0), (1.0,2,3f0))
    end == Any[Nothing]
    @test Base.return_types() do
        findlast(==(2), (1.0,2,3f0))
    end == Any[Int]
    @test Base.return_types() do
        findlast(==(0), (1.0,2,3f0))
    end == Any[Nothing]

    @testset "long tuples" begin
        longtuple = ntuple(i -> i in (15,17) ? 1 : 0, 40)
        @test findfirst(isequal(1), longtuple) == 15
        @test findlast(isequal(1), longtuple) == 17
    end
end

@testset "properties" begin
    ttest = (:a, :b, :c)
    @test propertynames(ttest) == (1, 2, 3)
    @test getproperty(ttest, 2) === :b
    @test map(p->getproperty(ttest, p), propertynames(ttest)) == ttest
    @test_throws ErrorException setproperty!(ttest, 1, :d)
end

# tuple_type_tail on non-normalized vararg tuple
@test Base.tuple_type_tail(Tuple{Vararg{T, 3}} where T<:Real) == Tuple{Vararg{T, 2}} where T<:Real
@test Base.tuple_type_tail(Tuple{Vararg{Int}}) == Tuple{Vararg{Int}}

@testset "setindex" begin
    @test Base.setindex((1, ), 2, 1) === (2, )
    @test Base.setindex((1, 2), 3, 1) === (3, 2)
    @test_throws BoundsError Base.setindex((), 1, 1)
    @test_throws BoundsError Base.setindex((1, ), 2, 2)
    @test_throws BoundsError Base.setindex((1, 2), 2, 0)
    @test_throws BoundsError Base.setindex((1, 2, 3), 2, -1)

    @test_throws BoundsError Base.setindex((1, 2), 2, 3)
    @test_throws BoundsError Base.setindex((1, 2), 2, 4)

    @test Base.setindex((1, 2, 4), 4, true) === (4, 2, 4)
    @test_throws BoundsError Base.setindex((1, 2), 2, false)

    f() = Base.setindex((1:1, 2:2, 3:3), 9, 1)
    @test @inferred(f()) == (9, 2:2, 3:3)
end

@testset "inferable range indexing with constant values" begin
    whole(t) = t[1:end]
    tail(t) = t[2:end]
    ttail(t) = t[3:end]
    front(t) = t[1:end-1]
    ffront(t) = t[1:end-2]

    @test @inferred( whole(())) == ()
    @test @inferred(  tail(())) == ()
    @test @inferred( ttail(())) == ()
    @test @inferred( front(())) == ()
    @test @inferred(ffront(())) == ()

    @test @inferred( whole((1,))) == (1,)
    @test @inferred(  tail((1,))) == ()
    @test @inferred( ttail((1,))) == ()
    @test @inferred( front((1,))) == ()
    @test @inferred(ffront((1,))) == ()

    @test @inferred( whole((1,2.0))) == (1,2.0)
    @test @inferred(  tail((1,2.0))) == (2.0,)
    @test @inferred( ttail((1,2.0))) == ()
    @test @inferred( front((1,2.0))) == (1.0,)
    @test @inferred(ffront((1,2.0))) == ()

    @test @inferred( whole((1,2.0,3//1))) == (1,2.0,3//1)
    @test @inferred(  tail((1,2.0,3//1))) == (2.0,3//1)
    @test @inferred( ttail((1,2.0,3//1))) == (3//1,)
    @test @inferred( front((1,2.0,3//1))) == (1,2.0)
    @test @inferred(ffront((1,2.0,3//1))) == (1,)

    @test @inferred( whole((1,2.0,3//1,0x04))) == (1,2.0,3//1,0x04)
    @test @inferred(  tail((1,2.0,3//1,0x04))) == (2.0,3//1,0x04)
    @test @inferred( ttail((1,2.0,3//1,0x04))) == (3//1,0x04)
    @test @inferred( front((1,2.0,3//1,0x04))) == (1,2.0,3//1)
    @test @inferred(ffront((1,2.0,3//1,0x04))) == (1,2.0)

    @test (1,)[0:-1] == ()
    @test (1,)[1:0] == ()
    @test (1,)[2:1] == ()
    @test (1,2.0)[0:-1] == ()
    @test (1,2.0)[1:0] == ()
    @test (1,2.0)[2:1] == ()
    @test (1,2.0)[3:2] == ()

    @test_throws BoundsError (1,)[2:2]
    @test_throws BoundsError (1,)[1:2]
    @test_throws BoundsError (1,)[0:1]
    @test_throws BoundsError (1,)[0:0]
    @test_throws BoundsError (1,2.0)[3:3]
    @test_throws BoundsError (1,2.0)[1:3]
    @test_throws BoundsError (1,2.0)[0:2]
    @test_throws BoundsError (1,2.0)[0:1]
    @test_throws BoundsError (1,2.0)[0:0]
end

@testset "Base.rest" begin
    t = (1, 2.0, 0x03, 4f0)
    @test Base.rest(t) === t
    @test Base.rest(t, 2) === (2.0, 0x03, 4f0)

    a = [1 2; 3 4]
    @test Base.rest(a) == a[:]
    @test pointer(Base.rest(a)) != pointer(a)
    @test Base.rest(a, 3) == [2, 4]

    itr = (-i for i in a)
    @test Base.rest(itr) == itr
    _, st = iterate(itr)
    r = Base.rest(itr, st)
    @test r isa Iterators.Rest
    @test collect(r) == -[3, 2, 4]
end

# issue #38837
f38837(xs) = map((F,x)->F(x), (Float32, Float64), xs)
@test @inferred(f38837((1,2))) === (1.0f0, 2.0)

@testset "indexing with UnitRanges" begin
    f(t) = t[3:end-2]
    @test @inferred(f(Tuple(1:10))) === Tuple(3:8)
    @test @inferred(f((true, 2., 3, 4f0, 0x05, 6, 7.))) === (3, 4f0, 0x05)

    f2(t) = t[Base.OneTo(5)]
    @test @inferred(f2(Tuple(1:10))) === Tuple(1:5)
    @test @inferred(f2((true, 2., 3, 4f0, 0x05, 6, 7.))) === (true, 2., 3, 4f0, 0x05)

    @test @inferred((t -> t[1:end])(Tuple(1:15))) === Tuple(1:15)
    @test @inferred((t -> t[2:end])(Tuple(1:15))) === Tuple(2:15)
    @test @inferred((t -> t[3:end])(Tuple(1:15))) === Tuple(3:15)
    @test @inferred((t -> t[1:end-1])(Tuple(1:15))) === Tuple(1:14)
    @test @inferred((t -> t[1:end-2])(Tuple(1:15))) === Tuple(1:13)
    @test @inferred((t -> t[3:2])(Tuple(1:15))) === ()

    @test_throws BoundsError (1, 2)[1:4]
    @test_throws BoundsError (1, 2)[0:2]
    @test_throws ArgumentError (1, 2)[OffsetArrays.IdOffsetRange(1:2, -1)]
end

# https://github.com/JuliaLang/julia/issues/40814
@test Base.return_types(NTuple{3,Int}, (Vector{Int},)) == Any[NTuple{3,Int}]

# issue #42457
f42457(a::NTuple{3,Int}, b::Tuple)::Bool = Base.isequal(a, Base.inferencebarrier(b)::Tuple)
@test f42457((1, 1, 1), (1, 1, 1))
@test !isempty(methods(Base._isequal, (NTuple{3, Int}, Tuple)))
g42457(a, b) = Base.isequal(a, b) ? 1 : 2.0
@test only(Base.return_types(g42457, (NTuple{3, Int}, Tuple))) === Union{Float64, Int}
@test only(Base.return_types(g42457, (NTuple{3, Int}, NTuple))) === Union{Float64, Int}
@test only(Base.return_types(g42457, (NTuple{3, Int}, NTuple{4}))) === Float64

# issue #46049: setindex(::Tuple) regression
@inferred Base.setindex((1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), 42, 1)

# issue #47326
function fun1_47326(args...)
    head..., tail = args
    head
end
function fun2_47326(args...)
    head, tail... = args
    tail
end
@test @inferred(fun1_47326(1,2,3)) === (1, 2)
@test @inferred(fun2_47326(1,2,3)) === (2, 3)

f47326(x::Union{Tuple, NamedTuple}) = Base.split_rest(x, 1)
tup = (1, 2, 3)
namedtup = (;a=1, b=2, c=3)
@test only(Base.return_types(f47326, (typeof(tup),))) == Tuple{Tuple{Int, Int}, Tuple{Int}}
@test only(Base.return_types(f47326, (typeof(namedtup),))) ==
    Tuple{
        NamedTuple{(:a, :b), Tuple{Int, Int}},
        NamedTuple{(:c,), Tuple{Int}},
    }

# Make sure that tuple iteration is foldable
@test Core.Compiler.is_foldable(Base.infer_effects(iterate, Tuple{NTuple{4, Float64}, Int}))
@test Core.Compiler.is_foldable(Base.infer_effects(eltype, Tuple{Tuple}))
