# This file is a part of Julia. License is MIT: http://julialang.org/license

## hashing strings ##

const memhash = UInt === UInt64 ? :memhash_seed : :memhash32_seed
const memhash_seed = UInt === UInt64 ? 0x71e729fd56419c81 : 0x56419c81

function hash{T<:ByteString}(s::Union{T,SubString{T}}, h::UInt)
    h += memhash_seed
    # note: use pointer(s) here (see #6058).
    ccall(memhash, UInt, (Ptr{UInt8}, Csize_t, UInt32), pointer(s), sizeof(s), h % UInt32) + h
end
hash(s::AbstractString, h::UInt) = hash(bytestring(s), h)
