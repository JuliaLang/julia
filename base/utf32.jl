# This file is a part of Julia. License is MIT: http://julialang.org/license

# UTF-32 basic functions
next(s::UTF32String, i::Int) = (s.data[i], i+1)
endof(s::UTF32String) = length(s.data) - 1
length(s::UTF32String) = length(s.data) - 1

utf32(x) = convert(UTF32String, x)
convert(::Type{UTF32String}, c::Char) = UTF32String(Char[c, Char(0)])
convert(::Type{UTF32String}, s::UTF32String) = s

function convert(::Type{UTF32String}, s::AbstractString)
    a = Array(Char, length(s) + 1)
    i = 0
    for c in s
        a[i += 1] = c
    end
    a[end] = Char(0) # NULL terminate
    UTF32String(a)
end

function convert(::Type{UTF32String}, data::AbstractVector{Char})
    len = length(data)
    d = Array(Char, len + 1)
    d[end] = Char(0) # NULL terminate
    UTF32String(copy!(d,1, data,1, len))
end

convert{T<:Union{Int32,UInt32}}(::Type{UTF32String}, data::AbstractVector{T}) =
    convert(UTF32String, reinterpret(Char, data))

convert{T<:AbstractString}(::Type{T}, v::AbstractVector{Char}) = convert(T, utf32(v))

# specialize for performance reasons:
function convert{T<:ByteString}(::Type{T}, data::AbstractVector{Char})
    s = IOBuffer(Array(UInt8,length(data)), true, true)
    truncate(s,0)
    for x in data
        print(s, x)
    end
    convert(T, takebuf_string(s))
end

convert(::Type{Vector{Char}}, str::UTF32String) = str.data
convert(::Type{Array{Char}},  str::UTF32String) = str.data

reverse(s::UTF32String) = UTF32String(reverse!(copy(s.data), 1, length(s)))

sizeof(s::UTF32String) = sizeof(s.data) - sizeof(Char)
unsafe_convert{T<:Union{Int32,UInt32,Char}}(::Type{Ptr{T}}, s::UTF32String) =
    convert(Ptr{T}, pointer(s))

function convert(T::Type{UTF32String}, bytes::AbstractArray{UInt8})
    isempty(bytes) && return UTF32String(Char[0])
    length(bytes) & 3 != 0 && throw(UnicodeError(UTF_ERR_ODD_BYTES_32,0,0))
    data = reinterpret(Char, bytes)
    # check for byte-order mark (BOM):
    if data[1] == Char(0x0000feff) # native byte order
        d = Array(Char, length(data))
        copy!(d,1, data, 2, length(data)-1)
    elseif data[1] == Char(0xfffe0000) # byte-swapped
        d = Array(Char, length(data))
        for i = 2:length(data)
            @inbounds d[i-1] = bswap(data[i])
        end
    else
        d = Array(Char, length(data) + 1)
        copy!(d, 1, data, 1, length(data)) # assume native byte order
    end
    d[end] = 0 # NULL terminate
    UTF32String(d)
end

function isvalid(::Type{UTF32String}, str::Union{Vector{Char}, Vector{UInt32}})
    for i=1:length(str)
        @inbounds if !isvalid(Char, UInt32(str[i])) ; return false ; end
    end
    return true
end
isvalid(str::Vector{Char}) = isvalid(UTF32String, str)

utf32(p::Ptr{Char}, len::Integer) = utf32(pointer_to_array(p, len))
utf32(p::Union{Ptr{UInt32}, Ptr{Int32}}, len::Integer) = utf32(convert(Ptr{Char}, p), len)
function utf32(p::Union{Ptr{Char}, Ptr{UInt32}, Ptr{Int32}})
    len = 0
    while unsafe_load(p, len+1) != 0; len += 1; end
    utf32(p, len)
end

function map(f, s::UTF32String)
    d = s.data
    out = similar(d)
    out[end] = 0

    @inbounds for i = 1:(length(d)-1)
        c2 = f(d[i])
        if !isa(c2, Char)
            throw(UnicodeError(UTF_ERR_MAP_CHAR, 0, 0))
        end
        out[i] = (c2::Char)
    end
    UTF32String(out)
end
