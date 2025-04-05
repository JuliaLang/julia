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
    @test iszero(bytesavailable(io))
    v = readavailable(io)
    @test v isa Vector{UInt8} && v == UInt8[]

    io = buffered("abcdefghij")
    @test bytesavailable(io) == 0
    fillbuffer(io)
    n = bytesavailable(io)
    @test bytesavailable(io) == 8
    @test readavailable(io) == b"abcdefgh"
    @test bytesavailable(io) == 0

    fillbuffer(io)
    @test bytesavailable(io) == 2
    @test readavailable(io) == b"ij"
    @test bytesavailable(io) == 0
    @test isempty(readavailable(io))

    # Test: readavailable will fill buffer if empty
    io = buffered("abcdefghij")
    @test bytesavailable(io) == 0
    @test readavailable(io) == b"abcdefgh"
    @test bytesavailable(io) == 0
    @test readavailable(io) == b"ij"
end

# The following function requires `eof`, so define it here.
Base.eof(io::GenericOldIO) = eof(io.inner)
Base.eof(io::GenericUnbufferedIO) = eof(io.inner)

@testset "readbytes!" begin
    for T in [GenericBufferedIO, GenericUnbufferedIO, GenericOldIO]
        # Resizing
        s = "abcdefg"
        io = T(IOBuffer(s))
        v = UInt8[]
        @test readbytes!(io, v, 7) == 7
        @test v == b"abcdefg"

        # With exact/larger buffer
        io = T(IOBuffer(s))
        v = zeros(UInt8, 4)
        @test readbytes!(io, v) == 4
        @test v == b"abcd"
        @test readbytes!(io, v) == 3
        @test v == b"efgd"
    end
end

Base.delete_method(Base.which(eof, Tuple{GenericOldIO}))
@test_throws MethodError eof(GenericOldIO(IOBuffer()))

Base.delete_method(Base.which(eof, Tuple{GenericUnbufferedIO}))
@test_throws MethodError eof(GenericUnbufferedIO(IOBuffer()))

end # module
