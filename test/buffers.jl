
@testset "Buffer AbstractArray interface" begin
    x = Buffer{Int}(undef, 2);
    @test firstindex(x) == 1
    @test lastindex(x) == 2
    @test axes(x) == (1:2,)
    @test axes(x, 1) == 1:2
    @test axes(x, 2) == 1:1
    @test x == x
end

@testset "Buffer with tagged unions" begin
    x = Buffer{Union{UInt8, UInt16}}(undef, 4);
    x[1] = UInt8(1)
    x[2] = UInt16(2)
    x[3] = UInt8(3)
    x[4] = UInt16(4)
    @test x[1] === UInt8(1)
    @test x[2] === UInt16(2)
    @test x[3] === UInt8(3)
    @test x[4] === UInt16(4)
end
