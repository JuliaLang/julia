# This file is a part of Julia. License is MIT: http://julialang.org/license

## from base/boot.jl:
#
# immutable String <: AbstractString
#     data::Vector{UInt8}
# end
#

## basic UTF-8 decoding & iteration ##

is_surrogate_lead(c::Unsigned) = ((c & ~0x003ff) == 0xd800)
is_surrogate_trail(c::Unsigned) = ((c & ~0x003ff) == 0xdc00)
is_surrogate_codeunit(c::Unsigned) = ((c & ~0x007ff) == 0xd800)
is_valid_continuation(c) = ((c & 0xc0) == 0x80)

const utf8_offset = [
    0x00000000, 0x00003080,
    0x000e2080, 0x03c82080,
    0xfa082080, 0x82082080,
]

const utf8_trailing = [
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5,
]

## required core functionality ##

function endof(s::String)
    d = s.data
    i = length(d)
    @inbounds while i > 0 && is_valid_continuation(d[i])
        i -= 1
    end
    i
end

function length(s::String)
    d = s.data
    cnum = 0
    for i = 1:length(d)
        @inbounds cnum += !is_valid_continuation(d[i])
    end
    cnum
end

@noinline function slow_utf8_next(d::Vector{UInt8}, b::UInt8, i::Int)
    # potentially faster version
    # d = s.data
    # a::UInt32 = d[i]
    # if a < 0x80; return Char(a); end
    # #if a&0xc0==0x80; return '\ufffd'; end
    # b::UInt32 = a<<6 + d[i+1]
    # if a < 0xe0; return Char(b - 0x00003080); end
    # c::UInt32 = b<<6 + d[i+2]
    # if a < 0xf0; return Char(c - 0x000e2080); end
    # return Char(c<<6 + d[i+3] - 0x03c82080)

    if is_valid_continuation(b)
        throw(UnicodeError(UTF_ERR_INVALID_INDEX, i, d[i]))
    end
    trailing = utf8_trailing[b + 1]
    if length(d) < i + trailing
        return '\ufffd', i+1
    end
    c::UInt32 = 0
    for j = 1:(trailing + 1)
        c <<= 6
        c += d[i]
        i += 1
    end
    c -= utf8_offset[trailing + 1]
    return Char(c), i
end

# This implementation relies on `next` returning a value past the end of the
# String's underlying data, which is true for valid Strings
done(s::String, state) = state > endof(s.data)

@inline function next(s::String, i::Int)
    # function is split into this critical fast-path
    # for pure ascii data, such as parsing numbers,
    # and a longer function that can handle any utf8 data
    d = s.data
    b = d[i]
    if b < 0x80
        return Char(b), i + 1
    end
    return slow_utf8_next(d, b, i)
end

function first_utf8_byte(ch::Char)
    c = UInt32(ch)
    b = c < 0x80    ? c%UInt8 :
        c < 0x800   ? ((c>>6)  | 0xc0)%UInt8 :
        c < 0x10000 ? ((c>>12) | 0xe0)%UInt8 :
                      ((c>>18) | 0xf0)%UInt8
    return b
end

function reverseind(s::String, i::Integer)
    j = length(s.data) + 1 - i
    d = s.data
    while is_valid_continuation(d[j])
        j -= 1
    end
    return j
end

## overload methods for efficiency ##

sizeof(s::String) = sizeof(s.data)

isvalid(s::String, i::Integer) =
    (1 <= i <= endof(s.data)) && !is_valid_continuation(s.data[i])

const empty_utf8 = String(UInt8[])

function getindex(s::String, r::UnitRange{Int})
    isempty(r) && return empty_utf8
    i, j = first(r), last(r)
    d = s.data
    if i < 1 || i > length(s.data)
        throw(BoundsError(s, i))
    end
    if is_valid_continuation(d[i])
        throw(UnicodeError(UTF_ERR_INVALID_INDEX, i, d[i]))
    end
    if j > length(d)
        throw(BoundsError())
    end
    j = nextind(s,j)-1
    String(d[i:j])
end

function search(s::String, c::Char, i::Integer)
    if i < 1 || i > sizeof(s)
        i == sizeof(s) + 1 && return 0
        throw(BoundsError(s, i))
    end
    d = s.data
    if is_valid_continuation(d[i])
        throw(UnicodeError(UTF_ERR_INVALID_INDEX, i, d[i]))
    end
    c < Char(0x80) && return search(d, c%UInt8, i)
    while true
        i = search(d, first_utf8_byte(c), i)
        (i==0 || s[i] == c) && return i
        i = next(s,i)[2]
    end
end

function rsearch(s::String, c::Char, i::Integer)
    c < Char(0x80) && return rsearch(s.data, c%UInt8, i)
    b = first_utf8_byte(c)
    while true
        i = rsearch(s.data, b, i)
        (i==0 || s[i] == c) && return i
        i = prevind(s,i)
    end
end

function string(a::String...)
    if length(a) == 1
        return a[1]::String
    end
    # ^^ at least one must be UTF-8 or the ASCII-only method would get called
    data = Array{UInt8}(0)
    for d in a
        append!(data,d.data)
    end
    String(data)
end

function string(a::Union{String,Char}...)
    s = Array{UInt8}(0)
    for d in a
        if isa(d,Char)
            c = UInt32(d::Char)
            if c < 0x80
                push!(s, c%UInt8)
            elseif c < 0x800
                push!(s, (( c >> 6          ) | 0xC0)%UInt8)
                push!(s, (( c        & 0x3F ) | 0x80)%UInt8)
            elseif c < 0x10000
                push!(s, (( c >> 12         ) | 0xE0)%UInt8)
                push!(s, (((c >> 6)  & 0x3F ) | 0x80)%UInt8)
                push!(s, (( c        & 0x3F ) | 0x80)%UInt8)
            elseif c < 0x110000
                push!(s, (( c >> 18         ) | 0xF0)%UInt8)
                push!(s, (((c >> 12) & 0x3F ) | 0x80)%UInt8)
                push!(s, (((c >> 6)  & 0x3F ) | 0x80)%UInt8)
                push!(s, (( c        & 0x3F ) | 0x80)%UInt8)
            else
                # '\ufffd'
                push!(s, 0xef); push!(s, 0xbf); push!(s, 0xbd)
            end
        else
            append!(s,(d::String).data)
        end
    end
    String(s)
end

function reverse(s::String)
    dat = s.data
    n = length(dat)
    n <= 1 && return s
    buf = Vector{UInt8}(n)
    out = n
    pos = 1
    @inbounds while out > 0
        ch = dat[pos]
        if ch > 0xdf
            if ch < 0xf0
                (out -= 3) < 0 && throw(UnicodeError(UTF_ERR_SHORT, pos, ch))
                buf[out + 1], buf[out + 2], buf[out + 3] = ch, dat[pos + 1], dat[pos + 2]
                pos += 3
            else
                (out -= 4) < 0 && throw(UnicodeError(UTF_ERR_SHORT, pos, ch))
                buf[out+1], buf[out+2], buf[out+3], buf[out+4] = ch, dat[pos+1], dat[pos+2], dat[pos+3]
                pos += 4
            end
        elseif ch > 0x7f
            (out -= 2) < 0 && throw(UnicodeError(UTF_ERR_SHORT, pos, ch))
            buf[out + 1], buf[out + 2] = ch, dat[pos + 1]
            pos += 2
        else
            buf[out] = ch
            out -= 1
            pos += 1
        end
    end
    String(buf)
end

write(io::IO, s::String) = write(io, s.data)

pointer(x::String) = pointer(x.data)
pointer(x::String, i::Integer) = pointer(x.data)+(i-1)

convert(::Type{String}, s::String) = s
convert(::Type{String}, v::Vector{UInt8}) = String(v)
