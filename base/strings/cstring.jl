# This file is a part of Julia. License is MIT: https://julialang.org/license

import Core.Intrinsics: bitcast

"""
    Cwstring

A C-style string composed of the native wide character type
[`Cwchar_t`](@ref)s. `Cwstring`s are NUL-terminated. For
C-style strings composed of the native character
type, see [`Cstring`](@ref). For more information
about string interoperability with C, see the
[manual](@ref man-bits-types).

"""
Cwstring

"""
    Cstring

A C-style string composed of the native character type
[`Cchar`](@ref)s. `Cstring`s are NUL-terminated. For
C-style strings composed of the native wide character
type, see [`Cwstring`](@ref). For more information
about string interoperability with C, see the
[manual](@ref man-bits-types).
"""
Cstring

# construction from pointers
Cstring(p::Union{Ptr{Int8},Ptr{UInt8},Ptr{Cvoid}}) = bitcast(Cstring, p)
Cwstring(p::Union{Ptr{Cwchar_t},Ptr{Cvoid}})       = bitcast(Cwstring, p)
Ptr{T}(p::Cstring) where {T<:Union{Int8,UInt8,Cvoid}} = bitcast(Ptr{T}, p)
Ptr{T}(p::Cwstring) where {T<:Union{Cwchar_t,Cvoid}}  = bitcast(Ptr{Cwchar_t}, p)

convert(::Type{Cstring}, p::Union{Ptr{Int8},Ptr{UInt8},Ptr{Cvoid}}) = Cstring(p)
convert(::Type{Cwstring}, p::Union{Ptr{Cwchar_t},Ptr{Cvoid}}) = Cwstring(p)
convert(::Type{Ptr{T}}, p::Cstring) where {T<:Union{Int8,UInt8,Cvoid}} = Ptr{T}(p)
convert(::Type{Ptr{T}}, p::Cwstring) where {T<:Union{Cwchar_t,Cvoid}} = Ptr{T}(p)

"""
    pointer(array [, index])

Get the native address of an array or string, optionally at a given location `index`.

This function is "unsafe". Be careful to ensure that a Julia reference to
`array` exists as long as this pointer will be used. The [`GC.@preserve`](@ref)
macro should be used to protect the `array` argument from garbage collection
within a given block of code.

Calling [`Ref(array[, index])`](@ref Ref) is generally preferable to this function as it guarantees validity.
"""
function pointer end

pointer(p::Cstring) = convert(Ptr{Cchar}, p)
pointer(p::Cwstring) = convert(Ptr{Cwchar_t}, p)

# comparisons against pointers (mainly to support `cstr==C_NULL`)
==(x::Union{Cstring,Cwstring}, y::Ptr) = pointer(x) == y
==(x::Ptr, y::Union{Cstring,Cwstring}) = x == pointer(y)

unsafe_string(s::Cstring) = unsafe_string(convert(Ptr{UInt8}, s))

# convert strings to String etc. to pass as pointers
cconvert(::Type{Cstring}, s::String) = s
cconvert(::Type{Cstring}, s::AbstractString) =
    cconvert(Cstring, String(s)::String)

function cconvert(::Type{Cwstring}, s::AbstractString)
    v = transcode(Cwchar_t, String(s))
    push!(v, 0)
    return cconvert(Cwstring, v)
end

eltype(::Type{Cstring}) = Cchar
eltype(::Type{Cwstring}) = Cwchar_t

containsnul(p::Ptr, len) =
    C_NULL != ccall(:memchr, Ptr{Cchar}, (Ptr{Cchar}, Cint, Csize_t), p, 0, len)
containsnul(s::String) = containsnul(unsafe_convert(Ptr{Cchar}, s), sizeof(s))
containsnul(s::AbstractString) = '\0' in s

function unsafe_convert(::Type{Cstring}, s::String)
    p = unsafe_convert(Ptr{Cchar}, s)
    containsnul(p, sizeof(s)) &&
        throw(ArgumentError("embedded NULs are not allowed in C strings: $(repr(s))"))
    return Cstring(p)
end

unsafe_convert(::Type{Cstring}, s::Union{Memory{UInt8},Memory{Int8}}) = Cstring(unsafe_convert(Ptr{Cvoid}, s))

function cconvert(::Type{Cwstring}, v::Vector{Cwchar_t})
    for i = 1:length(v)-1
        v[i] == 0 &&
            throw(ArgumentError("embedded NULs are not allowed in C strings: $(repr(v))"))
    end
    v[end] == 0 ||
        throw(ArgumentError("C string data must be NUL terminated: $(repr(v))"))
    return cconvert(Ptr{Cwchar_t}, v)
end
unsafe_convert(::Type{Cwstring}, s) = Cwstring(unsafe_convert(Ptr{Cwchar_t}, s))
unsafe_convert(::Type{Cwstring}, s::Cwstring) = s

# symbols are guaranteed not to contain embedded NUL
cconvert(::Type{Cstring}, s::Symbol) = s
unsafe_convert(::Type{Cstring}, s::Symbol) = Cstring(unsafe_convert(Ptr{Cchar}, s))

if ccall(:jl_get_UNAME, Any, ()) === :NT
"""
    Base.cwstring(s)

Converts a string `s` to a NUL-terminated `Vector{Cwchar_t}`, suitable for passing to C
functions expecting a `Ptr{Cwchar_t}`. The main advantage of using this over the implicit
conversion provided by [`Cwstring`](@ref) is if the function is called multiple times with the
same argument.

This is only available on Windows.
"""
function cwstring(s::AbstractString)
    bytes = codeunits(String(s))
    0 in bytes && throw(ArgumentError("embedded NULs are not allowed in C strings: $(repr(s))"))
    return push!(transcode(UInt16, bytes), 0)
end
end

# transcoding between data in UTF-8 and UTF-16 for Windows APIs,
# and also UTF-32 for APIs using Cwchar_t on other platforms.

"""
    transcode(T, src)

Convert string data between Unicode encodings. `src` is either a
`String` or a `Vector{UIntXX}` of UTF-XX code units, where
`XX` is 8, 16, or 32. `T` indicates the encoding of the return value:
`String` to return a (UTF-8 encoded) `String` or `UIntXX`
to return a `Vector{UIntXX}` of UTF-`XX` data. (The alias [`Cwchar_t`](@ref)
can also be used as the integer type, for converting `wchar_t*` strings
used by external C libraries.)

The `transcode` function succeeds as long as the input data can be
reasonably represented in the target encoding; it always succeeds for
conversions between UTF-XX encodings, even for invalid Unicode data.

Only conversion to/from UTF-8 is currently supported.

# Examples
```jldoctest
julia> str = "αβγ"
"αβγ"

julia> transcode(UInt16, str)
3-element Vector{UInt16}:
 0x03b1
 0x03b2
 0x03b3

julia> transcode(String, transcode(UInt16, str))
"αβγ"
```
"""
function transcode end

transcode(::Type{T}, src::AbstractVector{T}) where {T<:Union{UInt8,UInt16,UInt32,Int32}} = src
transcode(::Type{T}, src::String) where {T<:Union{Int32,UInt32}} = T[T(c) for c in src]
transcode(::Type{T}, src::AbstractVector{UInt8}) where {T<:Union{Int32,UInt32}} =
    transcode(T, String(Vector(src)))
transcode(::Type{T}, src::CodeUnits{UInt8,String}) where {T<:Union{Int32,UInt32}} =
    transcode(T, String(src))

function transcode(::Type{UInt8}, src::Vector{<:Union{Int32,UInt32}})
    buf = IOBuffer()
    for c in src
        print(buf, Char(c))
    end
    take!(buf)
end
transcode(::Type{String}, src::String) = src
transcode(T, src::String) = transcode(T, codeunits(src))
transcode(::Type{String}, src) = String(transcode(UInt8, src))

function transcode(::Type{UInt16}, src::AbstractVector{UInt8})
    require_one_based_indexing(src)
    dst = UInt16[]
    i, n = 1, length(src)
    n > 0 || return dst
    sizehint!(dst, 2n)
    a = src[1]
    while true
        if i < n && -64 <= a % Int8 <= -12 # multi-byte character
            b = src[i += 1]
            if -64 <= (b % Int8) || a == 0xf4 && 0x8f < b
                # invalid UTF-8 (non-continuation or too-high code point)
                push!(dst, a)
                a = b; continue
            elseif a < 0xe0 # 2-byte UTF-8
                push!(dst, xor(0x3080, UInt16(a) << 6, b))
            elseif i < n # 3/4-byte character
                c = src[i += 1]
                if -64 <= (c % Int8) # invalid UTF-8 (non-continuation)
                    push!(dst, a, b)
                    a = c; continue
                elseif a < 0xf0 # 3-byte UTF-8
                    push!(dst, xor(0x2080, UInt16(a) << 12, UInt16(b) << 6, c))
                elseif i < n
                    d = src[i += 1]
                    if -64 <= (d % Int8) # invalid UTF-8 (non-continuation)
                        push!(dst, a, b, c)
                        a = d; continue
                    elseif a == 0xf0 && b < 0x90 # overlong encoding
                        push!(dst, xor(0x2080, UInt16(b) << 12, UInt16(c) << 6, d))
                    else # 4-byte UTF-8
                        push!(dst, 0xe5b8 + (UInt16(a) << 8) + (UInt16(b) << 2) + (c >> 4),
                                   xor(0xdc80, UInt16(c & 0xf) << 6, d))
                    end
                else # too short
                    push!(dst, a, b, c)
                    break
                end
            else # too short
                push!(dst, a, b)
                break
            end
        else # ASCII or invalid UTF-8 (continuation byte or too-high code point)
            push!(dst, a)
        end
        i < n || break
        a = src[i += 1]
    end
    return dst
end

function transcode(::Type{UInt8}, src::AbstractVector{UInt16})
    require_one_based_indexing(src)
    n = length(src)
    n == 0 && return UInt8[]

    # Precompute m = sizeof(dst).   This involves annoying duplication
    # of the loop over the src array.   However, this is not just an
    # optimization: it is problematic for security reasons to grow
    # dst dynamically, because Base.winprompt uses this function to
    # convert passwords to UTF-8 and we don't want to make unintentional
    # copies of the password data.
    a = src[1]
    i, m = 1, 0
    while true
        if a < 0x80
            m += 1
        elseif a < 0x800 # 2-byte UTF-8
            m += 2
        elseif a & 0xfc00 == 0xd800 && i < length(src)
            b = src[i += 1]
            if (b & 0xfc00) == 0xdc00 # 2-unit UTF-16 sequence => 4-byte UTF-8
                m += 4
            else
                m += 3
                a = b; continue
            end
        else
            # 1-unit high UTF-16 or unpaired high surrogate
            # either way, encode as 3-byte UTF-8 code point
            m += 3
        end
        i < n || break
        a = src[i += 1]
    end

    dst = StringVector(m)
    a = src[1]
    i, j = 1, 0
    while true
        if a < 0x80 # ASCII
            dst[j += 1] = a % UInt8
        elseif a < 0x800 # 2-byte UTF-8
            dst[j += 1] = 0xc0 | ((a >> 6) % UInt8)
            dst[j += 1] = 0x80 | ((a % UInt8) & 0x3f)
        elseif a & 0xfc00 == 0xd800 && i < n
            b = src[i += 1]
            if (b & 0xfc00) == 0xdc00
                # 2-unit UTF-16 sequence => 4-byte UTF-8
                a += 0x2840
                dst[j += 1] = 0xf0 | ((a >> 8) % UInt8)
                dst[j += 1] = 0x80 | ((a % UInt8) >> 2)
                dst[j += 1] = xor(0xf0, ((a % UInt8) << 4) & 0x3f, (b >> 6) % UInt8)
                dst[j += 1] = 0x80 | ((b % UInt8) & 0x3f)
            else
                dst[j += 1] = 0xe0 | ((a >> 12) % UInt8)
                dst[j += 1] = 0x80 | (((a >> 6) % UInt8) & 0x3f)
                dst[j += 1] = 0x80 | ((a % UInt8) & 0x3f)
                a = b; continue
            end
        else
            # 1-unit high UTF-16 or unpaired high surrogate
            # either way, encode as 3-byte UTF-8 code point
            dst[j += 1] = 0xe0 | ((a >> 12) % UInt8)
            dst[j += 1] = 0x80 | (((a >> 6) % UInt8) & 0x3f)
            dst[j += 1] = 0x80 | ((a % UInt8) & 0x3f)
        end
        i < n || break
        a = src[i += 1]
    end
    return dst
end

function unsafe_string(p::Ptr{T}, length::Integer) where {T<:Union{UInt16,UInt32,Cwchar_t}}
    transcode(String, unsafe_wrap(Array, p, length; own=false))
end
function unsafe_string(cw::Cwstring)
    p = convert(Ptr{Cwchar_t}, cw)
    n = 1
    while unsafe_load(p, n) != 0
        n += 1
    end
    return unsafe_string(p, n - 1)
end
