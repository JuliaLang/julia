# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Standard library module for computing the CRC-32c checksum.

See [`CRC32c.crc32c`](@ref) for more information.
"""
module CRC32c

# contiguous byte arrays compatible with underlying C API
const ByteArray = Union{Array{UInt8},
                        Base.FastContiguousSubArray{UInt8,N,<:Array{UInt8}} where N,
                        Base.CodeUnits{UInt8, String}, Base.CodeUnits{UInt8, SubString{String}}}

export crc32c

"""
    crc32c(data, crc::UInt32=0x00000000)

Compute the CRC-32c checksum of the given `data`, which can be
an `Array{UInt8}`, a contiguous subarray thereof, or a `String`.  Optionally, you can pass
a starting `crc` integer to be mixed in with the checksum.  The `crc` parameter
can be used to compute a checksum on data divided into chunks: performing
`crc32c(data2, crc32c(data1))` is equivalent to the checksum of `[data1; data2]`.
(Technically, a little-endian checksum is computed.)

There is also a method `crc32c(io, nb, crc)` to checksum `nb` bytes from
a stream `io`, or `crc32c(io, crc)` to checksum all the remaining bytes.
Hence you can do [`open(crc32c, filename)`](@ref) to checksum an entire file,
or `crc32c(seekstart(buf))` to checksum an [`IOBuffer`](@ref) without
calling [`take!`](@ref).

For a `String`, note that the result is specific to the UTF-8 encoding
(a different checksum would be obtained from a different Unicode encoding).
To checksum an `a::Array` of some other bitstype, you can do `crc32c(reinterpret(UInt8,a))`,
but note that the result may be endian-dependent.
"""
function crc32c end


crc32c(a::ByteArray, crc::UInt32=0x00000000) = Base.unsafe_crc32c(a, length(a) % Csize_t, crc)
crc32c(s::Union{String, SubString{String}}, crc::UInt32=0x00000000) = Base._crc32c(s, crc)

"""
    crc32c(io::IO, [nb::Integer,] crc::UInt32=0x00000000)

Read up to `nb` bytes from `io` and return the CRC-32c checksum, optionally
mixed with a starting `crc` integer.  If `nb` is not supplied, then
`io` will be read until the end of the stream.
"""
crc32c(io::IO, nb::Integer, crc::UInt32=0x00000000) = Base._crc32c(io, nb, crc)
crc32c(io::IO, crc::UInt32=0x00000000) = Base._crc32c(io, crc)
crc32c(io::IOStream, crc::UInt32=0x00000000) = Base._crc32c(io, crc)

# optimized (copy-free) crc of IOBuffer
const ByteBuffer = Base.GenericIOBuffer{<:ByteArray}
crc32c(buf::ByteBuffer, crc::UInt32=0x00000000) = crc32c(buf, buf.size - position(buf), crc)
function crc32c(buf::ByteBuffer, nb::Integer, crc::UInt32=0x00000000)
    nb < 0 && throw(ArgumentError("number of bytes to checksum must be ≥ 0, got $nb"))
    isreadable(buf) || throw(ArgumentError("read failed, IOBuffer is not readable"))
    nb = min(nb, buf.size - position(buf))
    crc = GC.@preserve buf Base.unsafe_crc32c(pointer(buf.data) + position(buf), nb % Csize_t, crc)
    buf.ptr += nb
    return crc
end

end
