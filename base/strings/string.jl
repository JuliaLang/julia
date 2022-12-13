# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    StringIndexError(str, i)

An error occurred when trying to access `str` at index `i` that is not valid.
"""
struct StringIndexError <: Exception
    string::AbstractString
    index::Integer
end
@noinline string_index_err(s::AbstractString, i::Integer) =
    throw(StringIndexError(s, Int(i)))
function Base.showerror(io::IO, exc::StringIndexError)
    s = exc.string
    print(io, "StringIndexError: ", "invalid index [$(exc.index)]")
    if firstindex(s) <= exc.index <= ncodeunits(s)
        iprev = thisind(s, exc.index)
        inext = nextind(s, iprev)
        escprev = escape_string(s[iprev:iprev])
        if inext <= ncodeunits(s)
            escnext = escape_string(s[inext:inext])
            print(io, ", valid nearby indices [$iprev]=>'$escprev', [$inext]=>'$escnext'")
        else
            print(io, ", valid nearby index [$iprev]=>'$escprev'")
        end
    end
end

const ByteArray = Union{CodeUnits{UInt8,String}, Vector{UInt8},Vector{Int8}, FastContiguousSubArray{UInt8,1,CodeUnits{UInt8,String}}, FastContiguousSubArray{UInt8,1,Vector{UInt8}}, FastContiguousSubArray{Int8,1,Vector{Int8}}}

@inline between(b::T, lo::T, hi::T) where {T<:Integer} = (lo ≤ b) & (b ≤ hi)

"""
    String <: AbstractString

The default string type in Julia, used by e.g. string literals.

`String`s are immutable sequences of `Char`s. A `String` is stored internally as
a contiguous byte array, and while they are interpreted as being UTF-8 encoded,
they can be composed of any byte sequence. Use [`isvalid`](@ref) to validate
that the underlying byte sequence is valid as UTF-8.
"""
String

## constructors and conversions ##

# String constructor docstring from boot.jl, workaround for #16730
# and the unavailability of @doc in boot.jl context.
"""
    String(v::AbstractVector{UInt8})

Create a new `String` object using the data buffer from byte vector `v`.
If `v` is a `Vector{UInt8}` it will be truncated to zero length and future
modification of `v` cannot affect the contents of the resulting string.
To avoid truncation of `Vector{UInt8}` data, use `String(copy(v))`; for other
`AbstractVector` types, `String(v)` already makes a copy.

When possible, the memory of `v` will be used without copying when the `String`
object is created. This is guaranteed to be the case for byte vectors returned
by [`take!`](@ref) on a writable [`IOBuffer`](@ref) and by calls to
[`read(io, nb)`](@ref). This allows zero-copy conversion of I/O data to strings.
In other cases, `Vector{UInt8}` data may be copied, but `v` is truncated anyway
to guarantee consistent behavior.
"""
String(v::AbstractVector{UInt8}) = String(copyto!(StringVector(length(v)), v))
String(v::Vector{UInt8}) = ccall(:jl_array_to_string, Ref{String}, (Any,), v)

"""
    unsafe_string(p::Ptr{UInt8}, [length::Integer])

Copy a string from the address of a C-style (NUL-terminated) string encoded as UTF-8.
(The pointer can be safely freed afterwards.) If `length` is specified
(the length of the data in bytes), the string does not have to be NUL-terminated.

This function is labeled "unsafe" because it will crash if `p` is not
a valid memory address to data of the requested length.
"""
function unsafe_string(p::Union{Ptr{UInt8},Ptr{Int8}}, len::Integer)
    p == C_NULL && throw(ArgumentError("cannot convert NULL to string"))
    ccall(:jl_pchar_to_string, Ref{String}, (Ptr{UInt8}, Int), p, len)
end
function unsafe_string(p::Union{Ptr{UInt8},Ptr{Int8}})
    p == C_NULL && throw(ArgumentError("cannot convert NULL to string"))
    ccall(:jl_cstr_to_string, Ref{String}, (Ptr{UInt8},), p)
end

# This is @assume_effects :effect_free :nothrow :terminates_globally @ccall jl_alloc_string(n::Csize_t)::Ref{String},
# but the macro is not available at this time in bootstrap, so we write it manually.
@eval _string_n(n::Integer) = $(Expr(:foreigncall, QuoteNode(:jl_alloc_string), Ref{String}, Expr(:call, Expr(:core, :svec), :Csize_t), 1, QuoteNode((:ccall,0xe)), :(convert(Csize_t, n))))

"""
    String(s::AbstractString)

Create a new `String` from an existing `AbstractString`.
"""
String(s::AbstractString) = print_to_string(s)
@assume_effects :total String(s::Symbol) = unsafe_string(unsafe_convert(Ptr{UInt8}, s))

unsafe_wrap(::Type{Vector{UInt8}}, s::String) = ccall(:jl_string_to_array, Ref{Vector{UInt8}}, (Any,), s)
unsafe_wrap(::Type{Vector{UInt8}}, s::FastContiguousSubArray{UInt8,1,Vector{UInt8}}) = unsafe_wrap(Vector{UInt8}, pointer(s), size(s))

Vector{UInt8}(s::CodeUnits{UInt8,String}) = copyto!(Vector{UInt8}(undef, length(s)), s)
Vector{UInt8}(s::String) = Vector{UInt8}(codeunits(s))
Array{UInt8}(s::String)  = Vector{UInt8}(codeunits(s))

String(s::CodeUnits{UInt8,String}) = s.s

## low-level functions ##

pointer(s::String) = unsafe_convert(Ptr{UInt8}, s)
pointer(s::String, i::Integer) = pointer(s) + Int(i)::Int - 1

ncodeunits(s::String) = Core.sizeof(s)
codeunit(s::String) = UInt8

codeunit(s::String, i::Integer) = codeunit(s, Int(i))
@assume_effects :foldable @inline function codeunit(s::String, i::Int)
    @boundscheck checkbounds(s, i)
    b = GC.@preserve s unsafe_load(pointer(s, i))
    return b
end

## comparison ##

@assume_effects :total _memcmp(a::String, b::String) = @invoke _memcmp(a::Union{Ptr{UInt8},AbstractString},b::Union{Ptr{UInt8},AbstractString})

_memcmp(a::Union{Ptr{UInt8},AbstractString}, b::Union{Ptr{UInt8},AbstractString}) = _memcmp(a, b, min(sizeof(a), sizeof(b)))
function _memcmp(a::Union{Ptr{UInt8},AbstractString}, b::Union{Ptr{UInt8},AbstractString}, len::Int)
    ccall(:memcmp, Cint, (Ptr{UInt8}, Ptr{UInt8}, Csize_t), a, b, len % Csize_t) % Int
end

function cmp(a::String, b::String)
    al, bl = sizeof(a), sizeof(b)
    c = _memcmp(a, b)
    return c < 0 ? -1 : c > 0 ? +1 : cmp(al,bl)
end

==(a::String, b::String) = a===b

typemin(::Type{String}) = ""
typemin(::String) = typemin(String)

## thisind, nextind ##

@propagate_inbounds thisind(s::String, i::Int) = _thisind_str(s, i)

# s should be String or SubString{String}
@inline function _thisind_str(s, i::Int)
    i == 0 && return 0
    n = ncodeunits(s)
    i == n + 1 && return i
    @boundscheck between(i, 1, n) || throw(BoundsError(s, i))
    @inbounds b = codeunit(s, i)
    (b & 0xc0 == 0x80) & (i-1 > 0) || return i
    @inbounds b = codeunit(s, i-1)
    between(b, 0b11000000, 0b11110111) && return i-1
    (b & 0xc0 == 0x80) & (i-2 > 0) || return i
    @inbounds b = codeunit(s, i-2)
    between(b, 0b11100000, 0b11110111) && return i-2
    (b & 0xc0 == 0x80) & (i-3 > 0) || return i
    @inbounds b = codeunit(s, i-3)
    between(b, 0b11110000, 0b11110111) && return i-3
    return i
end

@propagate_inbounds nextind(s::String, i::Int) = _nextind_str(s, i)

# s should be String or SubString{String}
@inline function _nextind_str(s, i::Int)
    i == 0 && return 1
    n = ncodeunits(s)
    @boundscheck between(i, 1, n) || throw(BoundsError(s, i))
    @inbounds l = codeunit(s, i)
    (l < 0x80) | (0xf8 ≤ l) && return i+1
    if l < 0xc0
        i′ = @inbounds thisind(s, i)
        return i′ < i ? @inbounds(nextind(s, i′)) : i+1
    end
    # first continuation byte
    (i += 1) > n && return i
    @inbounds b = codeunit(s, i)
    b & 0xc0 ≠ 0x80 && return i
    ((i += 1) > n) | (l < 0xe0) && return i
    # second continuation byte
    @inbounds b = codeunit(s, i)
    b & 0xc0 ≠ 0x80 && return i
    ((i += 1) > n) | (l < 0xf0) && return i
    # third continuation byte
    @inbounds b = codeunit(s, i)
    ifelse(b & 0xc0 ≠ 0x80, i, i+1)
end

## checking UTF-8 & ACSII validity ##
#=
    The UTF-8 Validation is performed by a shift based DFA.
    Using the state machine diagram found @ https://bjoern.hoehrmann.de/utf-8/decoder/dfa/

        Important States
            0 -> UTF8_ACCEPT is the start state and represents a complete UTF-8 String as well
                        ASCII only strings will never leave this state
            1 -> UTF8_INVALID is only reached by invalid bytes and once in this state will not
            2 -> This is the state before the last byte of a multibyte character is read
            9 -> Not important and not used which is why it is all ones
                        Current State
                    0̲  1̲  2̲  3̲  4̲  5̲  6̲  7̲  8̲  9̲
                0 | 0  1  1  1  1  1  1  1  1  1
                1 | 1  1  0  2  1  2  1  3  3  1
                2 | 2  1  1  1  1  1  1  1  1  1
                3 | 3  1  1  1  1  1  1  1  1  1
                4 | 5  1  1  1  1  1  1  1  1  1
    Character   5 | 8  1  1  1  1  1  1  1  1  1
    Class       6 | 7  1  1  1  1  1  1  1  1  1
                7 | 1  1  0  2  2  1  3  3  1  1
                8 | 1  1  1  1  1  1  1  1  1  1
                9 | 1  1  0  2  1  2  3  3  1  1
               10 | 1  1  1  1  1  1  1  1  1  1
               11 | 6  1  1  1  1  1  1  1  1  1
    
    Each character class row is encoding 10 states shift in 6 bits combined into a UInt64 such that
    it contains the number of bit needed to shift the state it is transitioning to shifted into
    the position of the current state.

    Example: character class 1 is encoded in below
                    Current State        |    9 |    8 |    7 |    6 |    5 |    4 |    3 |    2 |    1 |    0 |
                    Next State           |    1 |    3 |    3 |    1 |    2 |    1 |    2 |    0 |    1 |    1 |
                    Shift required       |  6*1 |  6*3 |  6*3 |  6*1 |  6*2 |  6*1 |  6*2 |  6*0 |  6*1 |  6*1 |
                                         |    6 |   18 |   18 |    6 |   12 |    6 |   12 |    0 |    6 |    6 |
    UInt64(113232530780455302) =   0b0000|000110|010010|010010|000110|001100|000110|001100|000000|000110|000110

    Now if the current state was 5 the state::UInt64 would have the first 6 bit representing 5*6 = 30
    so when the next character class is 7 row is in row::UInt64: 
            The reduction operation:
                state =  byte_dfa         >>                        (state & UInt64(63))
                        | Shift to get the next state shift  | Mask first 6 bits of starting state to get the current shift ie 30
            Would result in the state being 2 which is a shift of 12:
                state = 0b0000|000110|010010|010010|000110|001100|000110|001100|000000|000110|000110 >> 30
                state = 0b0000|000000|000000|000000|000000|000000|000110|010010|010010|000110|001100|

    The code below will create the _UTF8_DFA_TABLE to be pasted in source.
    It is included here in an effort to document a contrived process.
    Do Not Uncomment the code below in this file it should be pasted into REPL

            function build_utf8_validation_statemachine_table(; num_classes=12, num_states=10, bit_per_state = 6)

                # class_repeats represents the 256 byte's classes by storing the (class, #of repeats)
                class_repeats = [ (0, 128), (1, 16), (9, 16), (7, 32), (8, 2), (2, 30), (10, 1),
                                (3,  12), (4,  1), (3,  2), (11, 1), (6, 3), (5,  1), (8, 11)]

                # See discription above
                state_arrays = [[ 0, 1, 1, 1, 1, 1, 1, 1, 1, 1],
                                [ 1, 1, 0, 2, 1, 2, 1, 3, 3, 1],
                                [ 2, 1, 1, 1, 1, 1, 1, 1, 1, 1],
                                [ 3, 1, 1, 1, 1, 1, 1, 1, 1, 1],
                                [ 5, 1, 1, 1, 1, 1, 1, 1, 1, 1],
                                [ 8, 1, 1, 1, 1, 1, 1, 1, 1, 1],
                                [ 7, 1, 1, 1, 1, 1, 1, 1, 1, 1],
                                [ 1, 1, 0, 2, 2, 1, 3, 3, 1, 1],
                                [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
                                [ 1, 1, 0, 2, 1, 2, 3, 3, 1, 1],
                                [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
                                [ 6, 1, 1, 1, 1, 1, 1, 1, 1, 1]]
                #This converts the state_arrays into the shift encoded UInt64
                class_row = zeros(UInt64, num_classes)
                for i = 1:num_classes
                    row = UInt64(0)
                    for j in 1:num_states
                        to_shift = UInt8((state_arrays[i][j]) * bit_per_state)
                        row = row | (UInt64(to_shift) << ((j - 1) * bit_per_state))
                    end
                    class_row[i]=row
                end
                print("\nconst _UTF8_DFA_TABLE = [\n")
                for (class, repeats) in class_repeats
                    print("    fill(UInt64($(class_row[class+1])), $repeats);\n")
                end
                print("    ]\n")
            end
=#
# This table will be filled with 256 UInt64 representing the DFA transitions for all bytes
const _UTF8_DFA_TABLE = [
    fill(UInt64(109802048057794944), 128);
    fill(UInt64(113232530780455302), 16);
    fill(UInt64(109855655693648262), 16);
    fill(UInt64(109855649351860614), 32);
    fill(UInt64(109802048057794950), 2);
    fill(UInt64(109802048057794956), 30);
    fill(UInt64(109802048057794968), 1);
    fill(UInt64(109802048057794962), 12);
    fill(UInt64(109802048057794974), 1);
    fill(UInt64(109802048057794962), 2);
    fill(UInt64(109802048057794980), 1);
    fill(UInt64(109802048057794986), 3);
    fill(UInt64(109802048057794992), 1);
    fill(UInt64(109802048057794950), 11)
    ]

const _UTF8_DFA_ACCEPT = UInt64(0) #This state represents the start and end of any valid string
const _UTF8_DFA_INVALID = UInt64(6) # If the state machine is ever in this state just stop

# This function is designed so that you could use it on strings with discontinous memmory layouts
#   by only feeding it contiguous block and keeping track of the state inbetween. 
# Furthermore you could check in returned value is _UTF8_DFA_INVALID and stop as invalid if it was.
# For a contiguous bytestream other states are valid other than _UTF8_DFA_ACCEPT aslong as you aren't
#  at the begining or end
function _isvalid_utf8_dfa(bytes::Vector{UInt8},state::UInt64 = _UTF8_DFA_ACCEPT)
    f(byte) = @inbounds _UTF8_DFA_TABLE[byte+1]
    op(s, byte_dfa) = byte_dfa >> (s & UInt64(63))
    final_state = mapfoldl(f, op, bytes, init = state)
    return (final_state & UInt64(63)) 
end

# This is a shift based utf-8 DFA that works on string that are a contiguous block
function _isvalid_utf8(bytes::Vector{UInt8})
    final_state = _isvalid_utf8_dfa(bytes, _UTF8_DFA_ACCEPT)
    return (final_state & UInt64(63)) == _UTF8_DFA_ACCEPT
end

_isvalid_utf8(s::Union{String,FastContiguousSubArray{UInt8,1,Vector{UInt8}}}) = _isvalid_utf8(unsafe_wrap(Vector{UInt8}, s))

# Classifcations of string
    # 0: neither valid ASCII nor UTF-8
    # 1: valid ASCII
    # 2: valid UTF-8
function byte_string_classify(s::Union{String,FastContiguousSubArray{UInt8,1,Vector{UInt8}}})
    bytes = unsafe_wrap(Vector{UInt8}, s)
    byte_string_classify(bytes, kwargs...)
end

function byte_string_classify(bytes::Vector{UInt8})
    all(c -> iszero(c & 0x80), bytes) && return 1
    valid = _isvalid_utf8(bytes)
    return ifelse(valid, 2, 0)
end

function isvalid(::Type{String}, s::Union{FastContiguousSubArray{UInt8,1,Vector{UInt8}},String}) 
    bytes = unsafe_wrap(Vector{UInt8}, s)
    isvalid(String,bytes)
end
isvalid(::Type{String}, bytes::Vector{UInt8}) = @inline _isvalid_utf8(bytes)

isvalid(s::String) = isvalid(String, s)

is_valid_continuation(c) = c & 0xc0 == 0x80

## required core functionality ##

@inline function iterate(s::String, i::Int=firstindex(s))
    (i % UInt) - 1 < ncodeunits(s) || return nothing
    b = @inbounds codeunit(s, i)
    u = UInt32(b) << 24
    between(b, 0x80, 0xf7) || return reinterpret(Char, u), i+1
    return iterate_continued(s, i, u)
end

function iterate_continued(s::String, i::Int, u::UInt32)
    u < 0xc0000000 && (i += 1; @goto ret)
    n = ncodeunits(s)
    # first continuation byte
    (i += 1) > n && @goto ret
    @inbounds b = codeunit(s, i)
    b & 0xc0 == 0x80 || @goto ret
    u |= UInt32(b) << 16
    # second continuation byte
    ((i += 1) > n) | (u < 0xe0000000) && @goto ret
    @inbounds b = codeunit(s, i)
    b & 0xc0 == 0x80 || @goto ret
    u |= UInt32(b) << 8
    # third continuation byte
    ((i += 1) > n) | (u < 0xf0000000) && @goto ret
    @inbounds b = codeunit(s, i)
    b & 0xc0 == 0x80 || @goto ret
    u |= UInt32(b); i += 1
@label ret
    return reinterpret(Char, u), i
end

@propagate_inbounds function getindex(s::String, i::Int)
    b = codeunit(s, i)
    u = UInt32(b) << 24
    between(b, 0x80, 0xf7) || return reinterpret(Char, u)
    return getindex_continued(s, i, u)
end

function getindex_continued(s::String, i::Int, u::UInt32)
    if u < 0xc0000000
        # called from `getindex` which checks bounds
        @inbounds isvalid(s, i) && @goto ret
        string_index_err(s, i)
    end
    n = ncodeunits(s)

    (i += 1) > n && @goto ret
    @inbounds b = codeunit(s, i) # cont byte 1
    b & 0xc0 == 0x80 || @goto ret
    u |= UInt32(b) << 16

    ((i += 1) > n) | (u < 0xe0000000) && @goto ret
    @inbounds b = codeunit(s, i) # cont byte 2
    b & 0xc0 == 0x80 || @goto ret
    u |= UInt32(b) << 8

    ((i += 1) > n) | (u < 0xf0000000) && @goto ret
    @inbounds b = codeunit(s, i) # cont byte 3
    b & 0xc0 == 0x80 || @goto ret
    u |= UInt32(b)
@label ret
    return reinterpret(Char, u)
end

getindex(s::String, r::AbstractUnitRange{<:Integer}) = s[Int(first(r)):Int(last(r))]

@inline function getindex(s::String, r::UnitRange{Int})
    isempty(r) && return ""
    i, j = first(r), last(r)
    @boundscheck begin
        checkbounds(s, r)
        @inbounds isvalid(s, i) || string_index_err(s, i)
        @inbounds isvalid(s, j) || string_index_err(s, j)
    end
    j = nextind(s, j) - 1
    n = j - i + 1
    ss = _string_n(n)
    GC.@preserve s ss unsafe_copyto!(pointer(ss), pointer(s, i), n)
    return ss
end

# nothrow because we know the start and end indices are valid
@assume_effects :nothrow length(s::String) = length_continued(s, 1, ncodeunits(s), ncodeunits(s))

# effects needed because @inbounds
@assume_effects :consistent :effect_free @inline function length(s::String, i::Int, j::Int)
    @boundscheck begin
        0 < i ≤ ncodeunits(s)+1 || throw(BoundsError(s, i))
        0 ≤ j < ncodeunits(s)+1 || throw(BoundsError(s, j))
    end
    j < i && return 0
    @inbounds i, k = thisind(s, i), i
    c = j - i + (i == k)
    @inbounds length_continued(s, i, j, c)
end

@assume_effects :terminates_locally @inline @propagate_inbounds function length_continued(s::String, i::Int, n::Int, c::Int)
    i < n || return c
    b = codeunit(s, i)
    while true
        while true
            (i += 1) ≤ n || return c
            0xc0 ≤ b ≤ 0xf7 && break
            b = codeunit(s, i)
        end
        l = b
        b = codeunit(s, i) # cont byte 1
        c -= (x = b & 0xc0 == 0x80)
        x & (l ≥ 0xe0) || continue

        (i += 1) ≤ n || return c
        b = codeunit(s, i) # cont byte 2
        c -= (x = b & 0xc0 == 0x80)
        x & (l ≥ 0xf0) || continue

        (i += 1) ≤ n || return c
        b = codeunit(s, i) # cont byte 3
        c -= (b & 0xc0 == 0x80)
    end
end

## overload methods for efficiency ##

isvalid(s::String, i::Int) = checkbounds(Bool, s, i) && thisind(s, i) == i

isascii(s::String) = isascii(codeunits(s))

# don't assume effects for general integers since we cannot know their implementation
@assume_effects :foldable repeat(c::Char, r::BitInteger) = @invoke repeat(c::Char, r::Integer)

"""
    repeat(c::AbstractChar, r::Integer) -> String

Repeat a character `r` times. This can equivalently be accomplished by calling
[`c^r`](@ref :^(::Union{AbstractString, AbstractChar}, ::Integer)).

# Examples
```jldoctest
julia> repeat('A', 3)
"AAA"
```
"""
function repeat(c::AbstractChar, r::Integer)
    c = Char(c)::Char
    r == 0 && return ""
    r < 0 && throw(ArgumentError("can't repeat a character $r times"))
    u = bswap(reinterpret(UInt32, c))
    n = 4 - (leading_zeros(u | 0xff) >> 3)
    s = _string_n(n*r)
    p = pointer(s)
    GC.@preserve s if n == 1
        ccall(:memset, Ptr{Cvoid}, (Ptr{UInt8}, Cint, Csize_t), p, u % UInt8, r)
    elseif n == 2
        p16 = reinterpret(Ptr{UInt16}, p)
        for i = 1:r
            unsafe_store!(p16, u % UInt16, i)
        end
    elseif n == 3
        b1 = (u >> 0) % UInt8
        b2 = (u >> 8) % UInt8
        b3 = (u >> 16) % UInt8
        for i = 0:r-1
            unsafe_store!(p, b1, 3i + 1)
            unsafe_store!(p, b2, 3i + 2)
            unsafe_store!(p, b3, 3i + 3)
        end
    elseif n == 4
        p32 = reinterpret(Ptr{UInt32}, p)
        for i = 1:r
            unsafe_store!(p32, u, i)
        end
    end
    return s
end
