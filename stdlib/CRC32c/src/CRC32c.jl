# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Standard library module for computing the CRC-32c checksum.

See [`CRC32c.crc32c`](@ref) for more information.
"""
module CRC32c

import Base.FastContiguousSubArray
import Base: DenseUInt8OrInt8

export crc32c

"""
    crc32c(data, crc::UInt32=0x00000000)

Compute the CRC-32c checksum of the given `data`, which can be
an `Array{UInt8}`, a contiguous subarray thereof, an `AbstractVector{UInt8}`, or a `String`.
Optionally, you can pass a starting `crc` integer to be mixed in with the checksum.
The `crc` parameter can be used to compute a checksum on data divided into chunks: performing
`crc32c(data2, crc32c(data1))` is equivalent to the checksum of `[data1; data2]`.
(Technically, a little-endian checksum is computed.)

There is also a method `crc32c(io, nb, crc)` to checksum `nb` bytes from
a stream `io`, or `crc32c(io, crc)` to checksum all the remaining bytes.
Hence you can do [`open(crc32c, filename)`](@ref) to checksum an entire file,
or `crc32c(seekstart(buf))` to checksum an [`IOBuffer`](@ref) without
calling [`take!`](@ref).

For a `String`, note that the result is specific to the UTF-8 encoding
(a different checksum would be obtained from a different Unicode encoding).
To checksum an `a::AbstractArray` of some other bitstype without padding,
you can do `crc32c(vec(reinterpret(UInt8,a)))`,
but note that the result may be endian-dependent.
"""
function crc32c end

function crc32c(a::AbstractVector{UInt8}, crc::UInt32=0x00000000)
    # use block size 24576=8192*3, since that is the threshold for
    # 3-way parallel SIMD code in the underlying jl_crc32c C function.
    last = lastindex(a)
    nb = length(a)
    buf = Memory{UInt8}(undef, Int(min(nb, 24576)))
    while nb > 0
        n = min(nb, 24576)
        copyto!(buf, 1, a, last - nb + 1, n)
        crc = Base.unsafe_crc32c(buf, n % Csize_t, crc)
        nb -= n
    end
    return crc
end

function crc32c(a::DenseUInt8OrInt8, crc::UInt32=0x00000000)
    Base._crc32c(a, crc)
end

crc32c(s::Union{String, SubString{String}}, crc::UInt32=0x00000000) = Base._crc32c(s, crc)

"""
    crc32c(io::IO, [nb::Integer,] crc::UInt32=0x00000000)

Read up to `nb` bytes from `io` and return the CRC-32c checksum, optionally
mixed with a starting `crc` integer.  If `nb` is not supplied, then
`io` will be read until the end of the stream.
"""
crc32c(io::IO, nb::Integer, crc::UInt32=0x00000000) = Base._crc32c(io, nb, crc)
crc32c(io::IO, crc::UInt32=0x00000000) = Base._crc32c(io, crc)

end
