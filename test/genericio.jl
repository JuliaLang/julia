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

# Requires: `eof`, and `readinto!` if new else `read(::IO, UInt8)`
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

    s = "abcdefg"
    v = zeros(UInt8, 32)
    io = GenericBufferedIO(IOBuffer(s), 5)
    @test readbytes!(io, v) == 7
    @test v == vcat(codeunits(s), fill(0x00, 32-7))
end

# Requires: `readbytes!`
@testset "read" begin
    for T in [GenericBufferedIO, GenericUnbufferedIO, GenericOldIO]
        s = "abcdefghijklmnopqrstuv"
        io = T(IOBuffer(s))
        v = read(io)
        @test v == codeunits(s)[1:length(v)]
        if length(v) == ncodeunits(s) # not guaranteed
            @test read(io) == UInt8[]
        end

        io = T(IOBuffer(s))
        v = read(io, 6)
        @test length(v) ≤ 6
        @test v == codeunits(s)[1:length(v)]
        v2 = read(io)
        @test length(v2) ≤ ncodeunits(s) - length(v)
        @test vcat(v, v2) == codeunits(s)[1:length(v)+length(v2)]

        io = T(IOBuffer(collect(codeunits(s))))
        @test read(io, String) === s
    end
end


Base.read(io::GenericUnbufferedIO, ::Type{UInt8}) = read(io.inner, UInt8)

# read(::IO, UInt8) and eof
@testset "readeach" begin
    @testset "UInt8" begin
        for T in [GenericBufferedIO, GenericUnbufferedIO, GenericOldIO]
            s = "abcdefg"
            v = UInt8[]
            io = T(IOBuffer(collect(codeunits(s))))
            for i in readeach(io, UInt8)
                push!(v, i)
            end
            @test v == codeunits(s)
            @test_throws EOFError read(io, UInt8)
        end
    end
end


end # module
