using Test
import Core: arrayfreeze, mutating_arrayfreeze, arraythaw

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
        @test IndexStyle(a) == IndexStyle(b) # ImmutableArray is IndexCartesian whereas Array is IndexLinear - worth looking into
        @test eachindex(a) == eachindex(b)
    end
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
