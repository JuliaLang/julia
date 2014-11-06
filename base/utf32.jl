## UTF-32 in the native byte order, i.e. plain old character arrays ##

immutable UTF32String <: DirectIndexString
    data::Array{Char,1} # includes 32-bit NULL termination after string chars

    function UTF32String(a::Array{Char,1})
        if length(a) < 1 || a[end] != char(0)
            throw(ArgumentError("UTF32String data must be NULL-terminated"))
        end
        new(a)
    end
end

next(s::UTF32String, i::Int) = (s.data[i], i+1)
endof(s::UTF32String) = length(s.data) - 1
length(s::UTF32String) = length(s.data) - 1

function utf32(c::Integer...)
    a = Array(Char, length(c) + 1)
    for i = 1:length(c)
        a[i] = char(c[i])
    end
    a[end] = char(0)
    UTF32String(a)
end

utf32(x) = convert(UTF32String, x)
convert(::Type{UTF32String}, c::Char) = UTF32String(Char[c, char(0)])
convert(::Type{UTF32String}, s::UTF32String) = s

function convert(::Type{UTF32String}, s::AbstractString)
    a = Array(Char, length(s) + 1)
    i = 0
    for c in s
        a[i += 1] = c
    end
    a[end] = char(0) # NULL terminate
    UTF32String(a)
end

function convert(::Type{UTF32String}, data::AbstractVector{Char})
    len = length(data)
    d = Array(Char, len + 1)
    d[end] = char(0) # NULL terminate
    UTF32String(copy!(d,1, data,1, len))
end

convert{T<:Union(Int32,UInt32)}(::Type{UTF32String}, data::AbstractVector{T}) =
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

convert(::Type{Array{Char,1}}, s::UTF32String) = s.data
convert(::Type{Array{Char}}, s::UTF32String) = s.data

reverse(s::UTF32String) = UTF32String(reverse!(copy(s.data), 1, length(s)))

sizeof(s::UTF32String) = sizeof(s.data) - sizeof(Char)
convert{T<:Union(Int32,UInt32,Char)}(::Type{Ptr{T}}, s::UTF32String) =
    convert(Ptr{T}, pointer(s))

function convert(T::Type{UTF32String}, bytes::AbstractArray{UInt8})
    isempty(bytes) && return UTF32String(Char[0])
    length(bytes) & 3 != 0 && throw(ArgumentError("need multiple of 4 bytes"))
    data = reinterpret(Char, bytes)
    # check for byte-order mark (BOM):
    if data[1] == char(0x0000feff) # native byte order
        d = Array(Char, length(data))
        copy!(d,1, data, 2, length(data)-1)
    elseif data[1] == char(0xfffe0000) # byte-swapped
        d = Array(Char, length(data))
        for i = 2:length(data)
            d[i-1] = bswap(data[i])
        end
    else
        d = Array(Char, length(data) + 1)
        copy!(d, 1, data, 1, length(data)) # assume native byte order
    end
    d[end] = char(0) # NULL terminate
    UTF32String(d)
end

utf32(p::Ptr{Char}, len::Integer) = utf32(pointer_to_array(p, len))
utf32(p::Union(Ptr{UInt32}, Ptr{Int32}), len::Integer) = utf32(convert(Ptr{Char}, p), len)
function utf32(p::Union(Ptr{Char}, Ptr{UInt32}, Ptr{Int32}))
    len = 0
    while unsafe_load(p, len+1) != 0; len += 1; end
    utf32(p, len)
end

function map(f::Function, s::UTF32String)
    d = s.data
    out = similar(d)
    out[end] = char(0)

    for i = 1:(length(d)-1)
        c2 = f(d[i])
        if !isa(c2, Char)
            error("map(f,s::AbstractString) requires f to return Char; try map(f,collect(s)) or a comprehension instead")
        end
        out[i] = (c2::Char)
    end
    UTF32String(out)
end
