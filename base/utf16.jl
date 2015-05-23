# This file is a part of Julia. License is MIT: http://julialang.org/license

immutable UTF16String <: AbstractString
    data::Array{UInt16,1} # includes 16-bit NULL termination after string chars
    function UTF16String(data::Vector{UInt16})
        if length(data) < 1 || data[end] != 0
            throw(ArgumentError("UTF16String data must be NULL-terminated"))
        end
        new(data)
    end
end

utf16_is_lead(c::UInt16) = (c & 0xfc00) == 0xd800
utf16_is_trail(c::UInt16) = (c & 0xfc00) == 0xdc00
utf16_is_surrogate(c::UInt16) = (c & 0xf800) == 0xd800
utf16_get_supplementary(lead::UInt16, trail::UInt16) = Char(UInt32(lead-0xd7f7)<<10 + trail)

function length(s::UTF16String)
    d = s.data
    len = length(d) - 1
    len == 0 && return 0
    cnum = 0
    for i = 1:len
        @inbounds cnum += !utf16_is_trail(d[i])
    end
    cnum
end

function endof(s::UTF16String)
    d = s.data
    i = length(d) - 1
    i == 0 && return i
    utf16_is_surrogate(d[i]) ? i-1 : i
end

function next(s::UTF16String, i::Int)
    if !utf16_is_surrogate(s.data[i])
        return Char(s.data[i]), i+1
    elseif length(s.data)-1 > i && utf16_is_lead(s.data[i]) && utf16_is_trail(s.data[i+1])
        return utf16_get_supplementary(s.data[i], s.data[i+1]), i+2
    end
    throw(ArgumentError("invalid UTF-16 character index"))
end

function reverseind(s::UTF16String, i::Integer)
    j = length(s.data) - i
    return Base.utf16_is_trail(s.data[j]) ? j-1 : j
end

lastidx(s::UTF16String) = length(s.data) - 1 # s.data includes NULL terminator

function reverse(s::UTF16String)
    d =s.data
    out = similar(d)
    out[end] = 0 # NULL termination
    n = length(d)
    for i = 1:n-1
        out[i] = d[n-i]
        if Base.utf16_is_lead(out[i])
            out[i],out[i-1] = out[i-1],out[i]
        end
    end
    return UTF16String(out)
end

# TODO: optimize this
function encode16(s::AbstractString)
    buf = UInt16[]
    for ch in s
        c = reinterpret(UInt32, ch)
        if c < 0x10000
            push!(buf, UInt16(c))
        elseif c <= 0x10ffff
            push!(buf, UInt16(0xd7c0 + (c>>10)))
            push!(buf, UInt16(0xdc00 + (c & 0x3ff)))
        else
            throw(ArgumentError("invalid Unicode character (0x$(hex(c)) > 0x10ffff)"))
        end
    end
    push!(buf, 0) # NULL termination
    UTF16String(buf)
end

utf16(x) = convert(UTF16String, x)
convert(::Type{UTF16String}, s::UTF16String) = s
convert(::Type{UTF16String}, s::AbstractString) = encode16(s)
convert(::Type{Array{UInt16,1}}, s::UTF16String) = s.data
convert(::Type{Array{UInt16}}, s::UTF16String) = s.data

# TODO: optimize this
convert(::Type{UTF8String}, s::UTF16String) =
    sprint(length(s.data)-1, io->for c in s; write(io,c::Char); end)

sizeof(s::UTF16String) = sizeof(s.data) - sizeof(UInt16)
unsafe_convert{T<:Union(Int16,UInt16)}(::Type{Ptr{T}}, s::UTF16String) =
    convert(Ptr{T}, pointer(s))

function isvalid(::Type{UTF16String}, data::AbstractArray{UInt16})
    i = 1
    n = length(data) # this may include NULL termination; that's okay
    while i < n # check for unpaired surrogates
        if utf16_is_lead(data[i]) && utf16_is_trail(data[i+1])
            i += 2
        elseif utf16_is_surrogate(data[i])
            return false
        else
            i += 1
        end
    end
    return i > n || !utf16_is_surrogate(data[i])
end

function convert(::Type{UTF16String}, data::AbstractVector{UInt16})
    !isvalid(UTF16String, data) && throw(ArgumentError("invalid UTF16 data"))
    len = length(data)
    d = Array(UInt16, len + 1)
    d[end] = 0 # NULL terminate
    UTF16String(copy!(d,1, data,1, len))
end

convert(T::Type{UTF16String}, data::AbstractArray{UInt16}) =
    convert(T, reshape(data, length(data)))

convert(T::Type{UTF16String}, data::AbstractArray{Int16}) =
    convert(T, reinterpret(UInt16, data))

function convert(T::Type{UTF16String}, bytes::AbstractArray{UInt8})
    isempty(bytes) && return UTF16String(UInt16[0])
    isodd(length(bytes)) && throw(ArgumentError("odd number of bytes"))
    data = reinterpret(UInt16, bytes)
    # check for byte-order mark (BOM):
    if data[1] == 0xfeff        # native byte order
        d = Array(UInt16, length(data))
        copy!(d,1, data,2, length(data)-1)
    elseif data[1] == 0xfffe    # byte-swapped
        d = Array(UInt16, length(data))
        for i = 2:length(data)
            d[i-1] = bswap(data[i])
        end
    else
        d = Array(UInt16, length(data) + 1)
        copy!(d,1, data,1, length(data)) # assume native byte order
    end
    d[end] = 0 # NULL terminate
    !isvalid(UTF16String, d) && throw(ArgumentError("invalid UTF16 data"))
    UTF16String(d)
end

utf16(p::Ptr{UInt16}, len::Integer) = utf16(pointer_to_array(p, len))
utf16(p::Ptr{Int16}, len::Integer) = utf16(convert(Ptr{UInt16}, p), len)
function utf16(p::Union(Ptr{UInt16}, Ptr{Int16}))
    len = 0
    while unsafe_load(p, len+1) != 0; len += 1; end
    utf16(p, len)
end
