using Test
import Core: ImmutableArray, arrayfreeze, mutating_arrayfreeze, arraythaw
import Core.Compiler: arrayfreeze_tfunc, mutating_arrayfreeze_tfunc, arraythaw_tfunc

@testset "basic ImmutableArray functionality" begin
    eltypes = (Float16, Float32, Float64, Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128)
    for t in eltypes
        a = rand(t, rand(1:100), rand(1:10))
        b = ImmutableArray(a)
        @test a == b
        @test a !== b
        @test length(a) == length(b)
        for i in 1:length(a)
            getindex(a, i) == getindex(b, i)
        end
        @test size(a) == size(b)
        if t in (Float16, Float32, Float64)
            # @test_broken sum(a) == sum(b) # issue #43772, sometimes works, sometimes doesn't
        else
            @test sum(a) == sum(b)
        end
        @test reverse(a) == reverse(b)
        @test ndims(a) == ndims(b)
        @test axes(a) == axes(b)
        @test strides(a) == strides(b)
        @test keys(a) == keys(b)
        @test_broken IndexStyle(a) == IndexStyle(b) # ImmutableArray is IndexCartesian whereas Array is IndexLinear - worth looking into
        @test_broken eachindex(a) == eachindex(b)
    end
end

const ImmutableVector{T} = ImmutableArray{T,1}
@testset "ImmutableArray tfuncs" begin
    @test arrayfreeze_tfunc(Vector{Int}) === ImmutableVector{Int}
    @test arrayfreeze_tfunc(Vector) === ImmutableVector
    @test arrayfreeze_tfunc(Array) === ImmutableArray
    @test arrayfreeze_tfunc(Any) === ImmutableArray
    @test arrayfreeze_tfunc(ImmutableVector{Int}) === Union{}
    @test arrayfreeze_tfunc(ImmutableVector) === Union{}
    @test arrayfreeze_tfunc(ImmutableArray) === Union{}
    @test mutating_arrayfreeze_tfunc(Vector{Int}) === ImmutableVector{Int}
    @test mutating_arrayfreeze_tfunc(Vector) === ImmutableVector
    @test mutating_arrayfreeze_tfunc(Array) === ImmutableArray
    @test mutating_arrayfreeze_tfunc(Any) === ImmutableArray
    @test mutating_arrayfreeze_tfunc(ImmutableVector{Int}) === Union{}
    @test mutating_arrayfreeze_tfunc(ImmutableVector) === Union{}
    @test mutating_arrayfreeze_tfunc(ImmutableArray) === Union{}
    @test arraythaw_tfunc(ImmutableVector{Int}) === Vector{Int}
    @test arraythaw_tfunc(ImmutableVector) === Vector
    @test arraythaw_tfunc(ImmutableArray) === Array
    @test arraythaw_tfunc(Any) === Array
    @test arraythaw_tfunc(Vector{Int}) === Union{}
    @test arraythaw_tfunc(Vector) === Union{}
    @test arraythaw_tfunc(Array) === Union{}
end

@testset "ImmutableArray builtins" begin
    # basic functionality
    let
        a = [1,2,3]
        b = ImmutableArray(a)
        @test arrayfreeze(a) === b
        @test mutating_arrayfreeze(a) === b
        @test arraythaw(b) !== a # arraythaw copies so not ===
    end
    # errors
    a = [1,2,3]
    b = ImmutableArray(a)
    @test_throws ArgumentError arrayfreeze()
    @test_throws ArgumentError arrayfreeze([1,2,3], nothing)
    @test_throws TypeError arrayfreeze(b)
    @test_throws TypeError arrayfreeze("not an array")
    @test_throws ArgumentError mutating_arrayfreeze()
    @test_throws ArgumentError mutating_arrayfreeze([1,2,3], nothing)
    @test_throws TypeError mutating_arrayfreeze(b)
    @test_throws TypeError mutating_arrayfreeze("not an array")
    @test_throws ArgumentError arraythaw()
    @test_throws ArgumentError arraythaw([1,2,3], nothing)
    @test_throws TypeError arraythaw(a)
    @test_throws TypeError arraythaw("not an array")
end