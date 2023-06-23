# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Standard library module for computing the CRC-32c checksum.

See [`CRC32c.crc32c`](@ref) for more information.
"""
module CRC32c

import Base.FastContiguousSubArray

export crc32c, adjust_crc32c!

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


crc32c(a::Union{Array{UInt8},FastContiguousSubArray{UInt8,N,<:Array{UInt8}} where N}, crc::UInt32=0x00000000) = Base._crc32c(a, crc)
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

#####################################################################
# Code to adjust a byte array to have an arbitrary given crc, by
# injecting 4 bytes at fixpos, following:
#     Martin Stigge, Henryk Plötz, Wolf Müller, Jens-Peter Redlich,
#     "Reversing CRC — Theory and Practice",
#     HU Berlin Public Report SAR-PR-2006-05 (May 2006).
# This is useful if you want to store the CRC of a file in the file.

const POLY = 0x82f63b78 # CRC-32C (iSCSI) polynomial in reversed bit order.

# reversed CRC32c table: Algorithm 5 from Stigge et al.
const revtable = let table = Vector{UInt32}(undef, 256)
    for index = UInt32(0):UInt32(255)
        crc = index << 24;
        for i = 1:8
            crc = !iszero(crc & 0x80000000) ? ((crc ⊻ POLY) << 1) + 0x01 : crc << 1;
        end
        table[index+1] = crc;
    end
    table
end

# Table-driven "backwards" calculation of CRC: Algorithm 6 from Stigge et al.
function bwcrc32c(a::AbstractVector{UInt8}, crc::UInt32)
    crc = crc ⊻ 0xffffffff
    for i = reverse(eachindex(a))
        crc = (crc << 8) ⊻ revtable[(crc >> 24) + 1] ⊻ a[i]
    end
    return crc
end

"""
    adjust_crc32c!(a::AbstractVector{UInt8}, wantcrc::UInt32, fixpos::Integer)

Write 4 bytes to `a[fixpos:fixpos+3]` so that `crc32c(a)` becomes equal to `wantcrc`.

This is especially useful if you want to store the checksum of some data *within
the data* itself, which is accomplished by:

1. Pad the data with 8 bytes (of any values).
2. Compute the checksum `crc` of the data.
3. Store `crc` in the data using 4 bytes of the padding.
4. Call `adjust_crc32c!` to change the other 4 bytes so that the checksum equals `crc`.

For example, the following code takes some arbitrary `data`, pads it with 8
bytes at the end, stores its checksum in the last 4 bytes (in little-endian order),
and then adjusts the preceding 4 bytes so that the original checksum is restored:

```jldoctest
julia> using CRC32c

julia> data = UInt8[0xff, 0x20, 0x21, 0x09, 0x2d, 0x25, 0xa4, 0xff, 0xa3, 0xbe];

julia> data = [data; 0x01:0x08]; # pad with 8 bytes

julia> crc = crc32c(data)
0x1a5f345c

julia> data[end-3:end] = reinterpret(UInt8, [htol(crc)]); # write crc at end

julia> crc32c(data) # crc has changed 😢
0x01b684ee

julia> adjust_crc32c!(data, crc, length(data)-7); # adjust crc via padding bytes

julia> crc32c(data) # original crc is restored! 😄
0x1a5f345c

julia> ltoh(reinterpret(UInt32, data[end-3:end])[1]) # crc is stored in data 😄
0x1a5f345c
```

See also [`adjust_crc32c`](@ref) to append similar padding bytes to the end of
a file or I/O stream (which has the advantage of not requiring you to read the
entire file into memory at once).
"""
function adjust_crc32c!(a::AbstractVector{UInt8}, wantcrc::UInt32, fixpos::Integer)
    # store v in little-endian order at b[k:k+3]
    function store_le!(b::AbstractVector{UInt8}, k::Integer, v::UInt32)
        @inbounds b[k],b[k+1],b[k+2],b[k+3] =
            v%UInt8, (v>>8)%UInt8, (v>>16)%UInt8, (v>>24)%UInt8
    end

    # Algorithm 8 from Stigge et al.
    checkbounds(a, fixpos:fixpos+3)
    @views store_le!(a, fixpos, crc32c(a[begin:fixpos-1]) ⊻ 0xffffffff)
    @views store_le!(a, fixpos, bwcrc32c(a[fixpos:end], wantcrc))
    return a
end

"""
    adjust_crc32c(filename::AbstractString, wantcrc::UInt32)
    adjust_crc32c(io::IO, wantcrc::UInt32)

Write 4 bytes of "padding" to the *end* of the the I/O stream `io`
(which *must* be seekable and read/write) or the file `filename`, in order
to cause the CRC32c checksum of the whole stream/file to equal `wantcrc`.

(This is mainly useful if you want to store the checksum of the file *within the file*:
simply set `wantcrc` to be an arbitrary number, such as `rand(UInt32)`, store it within
the file as desired, and then call `adjust_crc32c` to write padding bytes that force
the checksum to match `wantcrc`.)

See also [`adjust_crc32c!`](@ref) to write similar padding bytes to an arbitrary
position within an array.
"""
function adjust_crc32c(io::IO, wantcrc::UInt32)
    le(v::UInt32) = [v%UInt8, (v>>8)%UInt8, (v>>16)%UInt8, (v>>24)%UInt8]
    # specialized version of adjust_crc32c! for writing to end
    write(io, le(bwcrc32c(le(crc32c(seekstart(io)) ⊻ 0xffffffff), wantcrc)))
    return io
end

adjust_crc32c(filename::AbstractString, wantcrc::UInt32) =
    open(io -> adjust_crc32c(io, wantcrc), filename, "r+")

end
