module GenericIOTest

# NB: Old IOs refer to IOs written before the specification of the IO
# interface. We must ensure these keep on working.

using Test
using Test: GenericOldIO, GenericBufferedIO, GenericUnbufferedIO

buffered(s::String) = GenericBufferedIO(IOBuffer(s), 8)
unbuffered(s::String) = GenericUnbufferedIO(IOBuffer(s))

@testset "EOF and read a single byte" begin
    # Only defined by default for buffered streams
    s = "abcdefghij" # 10 bytes
    io = buffered(s)
    v = UInt8[]
    @test !eof(io)
    for i in 1:7
        push!(v, read(io, UInt8))
    end
    @test !eof(io)
    for i in 1:3
        push!(v, read(io, UInt8))
    end
    @test String(v) == s
    @test eof(io)
end

@testset "bytesavailable and readavailable" begin
    # Only sensible for buffered streams, not defined by default
    # for old IOs
    io = buffered("")
    @test iszero(bytesavailable)
    v = readavailable(io)
    @test v isa Vector{UInt8} && v == UInt8[]

    io = buffered("abcdefghij")
    @test bytesavailable(io) == 0
    @test isempty(readavailable(io))
    fillbuffer(io)
    @test bytesavailable(io) == 8
    @test readavailable(io) == b"abcdefgh"
    @test bytesavailable(io) == 0
    @test isempty(readavailable(io))
    fillbuffer(io)
    @test bytesavailable(io) == 2
    @test readavailable(io) == b"ij"
    @test bytesavailable(io) == 0
    @test isempty(readavailable(io))
end

@testset "readbytes!" begin
    @testset "buffered" begin
    end
end


end # module