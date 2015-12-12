# This file is a part of Julia. License is MIT: http://julialang.org/license

# definitions related to C interface

import Core.Intrinsics: cglobal, box

cfunction(f::Function, r, a) = ccall(:jl_function_ptr, Ptr{Void}, (Any, Any, Any), f, r, a)

if ccall(:jl_is_char_signed, Any, ())
    typealias Cchar Int8
else
    typealias Cchar UInt8
end
typealias Cuchar UInt8
typealias Cshort Int16
typealias Cushort UInt16
typealias Cint Int32
typealias Cuint UInt32
if OS_NAME === :Windows
    typealias Clong Int32
    typealias Culong UInt32
    typealias Cwchar_t UInt16
else
    typealias Clong Int
    typealias Culong UInt
    typealias Cwchar_t Int32
end
typealias Cptrdiff_t Int
typealias Csize_t UInt
typealias Cssize_t Int
typealias Cintmax_t Int64
typealias Cuintmax_t UInt64
typealias Clonglong Int64
typealias Culonglong UInt64
typealias Cfloat Float32
typealias Cdouble Float64

if OS_NAME !== :Windows
    const sizeof_mode_t = ccall(:jl_sizeof_mode_t, Cint, ())
    if sizeof_mode_t == 2
        typealias Cmode_t Int16
    elseif sizeof_mode_t == 4
        typealias Cmode_t Int32
    elseif sizeof_mode_t == 8
        typealias Cmode_t Int64
    end
end

# C NUL-terminated string pointers; these can be used in ccall
# instead of Ptr{Cchar} and Ptr{Cwchar_t}, respectively, to enforce
# a check for embedded NUL chars in the string (to avoid silent truncation).
if Int === Int64
    bitstype 64 Cstring
    bitstype 64 Cwstring
else
    bitstype 32 Cstring
    bitstype 32 Cwstring
end

# construction from typed pointers
convert{T<:Union{Int8,UInt8}}(::Type{Cstring}, p::Ptr{T}) = box(Cstring, p)
convert(::Type{Cwstring}, p::Ptr{Cwchar_t}) = box(Cwstring, p)
convert{T<:Union{Int8,UInt8}}(::Type{Ptr{T}}, p::Cstring) = box(Ptr{T}, p)
convert(::Type{Ptr{Cwchar_t}}, p::Cwstring) = box(Ptr{Cwchar_t}, p)

# construction from untyped pointers
convert{T<:Union{Cstring,Cwstring}}(::Type{T}, p::Ptr{Void}) = box(T, p)

pointer(p::Cstring) = convert(Ptr{UInt8}, p)
pointer(p::Cwstring) = convert(Ptr{Cwchar_t}, p)

# comparisons against pointers (mainly to support `cstr==C_NULL`)
==(x::Union{Cstring,Cwstring}, y::Ptr) = pointer(x) == y
==(x::Ptr, y::Union{Cstring,Cwstring}) = x == pointer(y)

# here, not in pointer.jl, to avoid bootstrapping problems in coreimg.jl
pointer_to_string(p::Cstring, own::Bool=false) = pointer_to_string(convert(Ptr{UInt8}, p), own)

# convert strings to String etc. to pass as pointers
cconvert(::Type{Cstring}, s::AbstractString) = bytestring(s)
cconvert(::Type{Cwstring}, s::AbstractString) = wstring(s)

containsnul(p::Ptr, len) = C_NULL != ccall(:memchr, Ptr{Cchar}, (Ptr{Cchar}, Cint, Csize_t), p, 0, len)
function unsafe_convert(::Type{Cstring}, s::String)
    p = unsafe_convert(Ptr{Cchar}, s)
    if containsnul(p, sizeof(s))
        throw(ArgumentError("embedded NULs are not allowed in C strings: $(repr(s))"))
    end
    return Cstring(p)
end

# symbols are guaranteed not to contain embedded NUL
convert(::Type{Cstring}, s::Symbol) = Cstring(unsafe_convert(Ptr{Cchar}, s))

# in string.jl: unsafe_convert(::Type{Cwstring}, s::WString)

# FIXME: this should be handled by implicit conversion to Cwstring, but good luck with that
@windows_only function cwstring(s::AbstractString)
    bytes = bytestring(s).data
    0 in bytes && throw(ArgumentError("embedded NULs are not allowed in C strings: $(repr(s))"))
    return push!(utf8to16(bytes), 0)
end

# conversions between UTF-8 and UTF-16 for Windows APIs

function utf8to16(src::Vector{UInt8})
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
                push!(dst, 0x3080 $ (UInt16(a) << 6) $ b)
            elseif i < n # 3/4-byte character
                c = src[i += 1]
                if -64 <= (c % Int8) # invalid UTF-8 (non-continuation)
                    push!(dst, a, b)
                    a = c; continue
                elseif a < 0xf0 # 3-byte UTF-8
                    push!(dst, 0x2080 $ (UInt16(a) << 12) $ (UInt16(b) << 6) $ c)
                elseif i < n
                    d = src[i += 1]
                    if -64 <= (d % Int8) # invalid UTF-8 (non-continuation)
                        push!(dst, a, b, c)
                        a = d; continue
                    elseif a == 0xf0 && b < 0x90 # overlong encoding
                        push!(dst, 0x2080 $ (UInt16(b) << 12) $ (UInt16(c) << 6) $ d)
                    else # 4-byte UTF-8
                        push!(dst, 0xe5b8 + (UInt16(a) << 8) + (UInt16(b) << 2) + (c >> 4),
                                   0xdc80 $ (UInt16(c & 0xf) << 6) $ d)
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

function utf16to8(src::Vector{UInt16})
    dst = UInt8[]
    i, n = 1, length(src)
    n > 0 || return dst
    sizehint!(dst, n)
    a = src[1]
    while true
        if a < 0x80 # ASCII
            push!(dst, a % UInt8)
        elseif a < 0x800 # 2-byte UTF-8
            push!(dst, 0xc0 | ((a >> 6) % UInt8),
                       0x80 | ((a % UInt8) & 0x3f))
        elseif a & 0xfc00 == 0xd800 && i < n
            b = src[i += 1]
            if (b & 0xfc00) == 0xdc00
                # 2-unit UTF-16 sequence => 4-byte UTF-8
                a += 0x2840
                push!(dst, 0xf0 | ((a >> 8) % UInt8),
                           0x80 | ((a % UInt8) >> 2),
                           0xf0 $ ((((a % UInt8) << 4) & 0x3f) $ (b >> 6) % UInt8),
                           0x80 | ((b % UInt8) & 0x3f))
            else
                push!(dst, 0xe0 | ((a >> 12) % UInt8),
                           0x80 | (((a >> 6) % UInt8) & 0x3f),
                           0x80 | ((a % UInt8) & 0x3f))
                a = b; continue
            end
        else
            # 1-unit high UTF-16 or unpaired high surrogate
            # either way, encode as 3-byte UTF-8 code point
            push!(dst, 0xe0 | ((a >> 12) % UInt8),
                       0x80 | (((a >> 6) % UInt8) & 0x3f),
                       0x80 | ((a % UInt8) & 0x3f))
        end
        i < n || break
        a = src[i += 1]
    end
    return dst
end

# deferring (or un-deferring) ctrl-c handler for external C code that
# is not interrupt safe (see also issue #2622).  The sigatomic_begin/end
# functions should always be called in matched pairs, ideally via:
#            disable_sigint() do .. end
# reennable_sigint is provided so that immediate ctrl-c handling is
# re-enabled within a sigatomic region, e.g. inside a Julia callback function
# within a long-running C routine.
sigatomic_begin() = ccall(:jl_sigatomic_begin, Void, ())
sigatomic_end() = ccall(:jl_sigatomic_end, Void, ())
disable_sigint(f::Function) = try sigatomic_begin(); f(); finally sigatomic_end(); end
reenable_sigint(f::Function) = try sigatomic_end(); f(); finally sigatomic_begin(); end

function ccallable(f::Function, rt::Type, argt::Type, name::Union{AbstractString,Symbol}=string(f))
    ccall(:jl_extern_c, Void, (Any, Any, Any, Cstring), f, rt, argt, name)
end

function ccallable(f::Function, argt::Type, name::Union{AbstractString,Symbol}=string(f))
    ccall(:jl_extern_c, Void, (Any, Ptr{Void}, Any, Cstring), f, C_NULL, argt, name)
end

macro ccallable(def)
    if isa(def,Expr) && (def.head === :(=) || def.head === :function)
        sig = def.args[1]
        if sig.head === :call
            name = sig.args[1]
            at = map(sig.args[2:end]) do a
                if isa(a,Expr) && a.head === :(::)
                    a.args[2]
                else
                    :Any
                end
            end
            return quote
                $(esc(def))
                ccallable($(esc(name)), $(Expr(:curly, :Tuple, map(esc, at)...)))
            end
        end
    end
    error("expected method definition in @ccallable")
end
