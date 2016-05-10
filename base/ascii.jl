# This file is a part of Julia. License is MIT: http://julialang.org/license

ascii(x) = ascii(convert(String, x))
ascii(p::Ptr{UInt8}) = ascii(bytestring(p))
ascii(p::Ptr{UInt8}, len::Integer) = ascii(bytestring(p, len))
ascii(s::String) = byte_string_classify(s.data) == 1 ? s :
    throw(ArgumentError("invalid ASCII sequence"))
