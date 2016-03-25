# This file is a part of Julia. License is MIT: http://julialang.org/license

function bytestring(p::Union{Ptr{UInt8},Ptr{Int8}})
    p == C_NULL ? throw(ArgumentError("cannot convert NULL to string")) :
    ccall(:jl_cstr_to_string, Any, (Cstring,), p)::String
end
bytestring(s::Cstring) = bytestring(convert(Ptr{UInt8}, s))

function bytestring(p::Union{Ptr{UInt8},Ptr{Int8}},len::Integer)
    p == C_NULL ? throw(ArgumentError("cannot convert NULL to string")) :
    ccall(:jl_pchar_to_string, Any, (Ptr{UInt8},Int), p, len)::String
end

cmp(a::String, b::String) = lexcmp(a.data, b.data)
==(a::String, b::String) = endof(a) == endof(b) && cmp(a,b) == 0

## checking UTF-8 & ACSII validity ##

isvalid(::Type{String}, s::Union{Vector{UInt8},String}) = byte_string_classify(s) != 0

byte_string_classify(data::Vector{UInt8}) =
    ccall(:u8_isvalid, Int32, (Ptr{UInt8}, Int), data, length(data))
byte_string_classify(s::String) = byte_string_classify(s.data)
    # 0: neither valid ASCII nor UTF-8
    # 1: valid ASCII
    # 2: valid UTF-8

