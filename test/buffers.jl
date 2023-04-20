
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

@testset "Buffer with pointer fields" begin
    ptrs = Buffer{Any}(undef, 4);
    @test !isassigned(ptrs, 1)
    ptrs[1] = 4
    @test isassigned(ptrs, 1)
    @test ptrs[1] == 4
    d = Dict{Int,Int}()
    @test !isassigned(ptrs, 2)
    ptrs[2] = d
    @test isassigned(ptrs, 2)
    @test ptrs[2] === d
end

@testset "Buffer with embedded pointers" begin
    b = Buffer{Tuple{Int16,Vector{Int},Vector{Int}}}(undef, 1000);
    v = (Int16(1), Int[], Int[])
    @test !isassigned(b, 1)
    b[1] = v
    @test isassigned(b, 1)
    push!(v[2], 1)
    @test b[1] == v
end

# TODO: test copy(::Buffer) and unsafe_copyto!
@testset "Buffer copy" begin
    b = Buffer{Int}(undef, 3)
    b[1] = 1; b[2] = 2; b[3] = 3;
    b2 = copy(b)
    @test b2 == b
    @test b2 !== b
    b = Buffer{Union{Int32,Int64}}(undef, 3)
    b[1] = 1; b[2] = 2; b[3] = 3;
    b2 = copy(b)
    @test b2 == b
    @test b2 !== b
    b = Buffer{Any}(undef, 3)
    b[1] = 1; b[2] = 2; b[3] = 3;
    b2 = copy(b)
    @test b2 == b
    @test b2 !== b

end


@test Base.summarysize(Buffer{Union{Nothing,Missing}}(undef, 16)) <
    Base.summarysize(Buffer{Union{Nothing,Missing}}(undef, 32))
@test Base.summarysize(Buffer{Nothing}(undef, 16)) ==
    Base.summarysize(Buffer{Nothing}(undef, 32))



