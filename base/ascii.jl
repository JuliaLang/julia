# This file is a part of Julia. License is MIT: http://julialang.org/license

ascii(x) = ascii(convert(String, x))
ascii(s::String) = byte_string_classify(s.data) == 1 ? s :
    throw(ArgumentError("invalid ASCII sequence"))
ascii(p::Ptr{UInt8}) = String(bytestring(p))
ascii(p::Ptr{UInt8}, len::Integer) = ascii(pointer_to_array(p, len))
