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
String(v::AbstractVector{UInt8}) = unsafe_takestring(copyto!(StringMemory(length(v)), v))
function String(v::Vector{UInt8})
    #return ccall(:jl_array_to_string, Ref{String}, (Any,), v)
    len = length(v)
    len == 0 && return ""
    ref = v.ref
    if ref.ptr_or_offset == ref.mem.ptr
        str = ccall(:jl_genericmemory_to_string, Ref{String}, (Any, Int), ref.mem, len)
    else
        str = ccall(:jl_pchar_to_string, Ref{String}, (Ptr{UInt8}, Int), ref, len)
    end
    # optimized empty!(v); sizehint!(v, 0) calls
    setfield!(v, :size, (0,))
    setfield!(v, :ref, memoryref(Memory{UInt8}()))
    return str
end

"Create a string re-using the memory, if possible.
Mutating or reading the memory after calling this function is undefined behaviour."
function unsafe_takestring(m::Memory{UInt8})
    isempty(m) ? "" : ccall(:jl_genericmemory_to_string, Ref{String}, (Any, Int), m, length(m))
end

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

# This is `@assume_effects :total !:consistent @ccall jl_alloc_string(n::Csize_t)::Ref{String}`,
# but the macro is not available at this time in bootstrap, so we write it manually.
const _string_n_override = 0x04ee
@eval _string_n(n::Integer) = $(Expr(:foreigncall, QuoteNode(:jl_alloc_string), Ref{String},
    :(Core.svec(Csize_t)), 1, QuoteNode((:ccall, _string_n_override, false)), :(convert(Csize_t, n))))

"""
    String(s::AbstractString)

Create a new `String` from an existing `AbstractString`.
"""
String(s::AbstractString) = print_to_string(s)
@assume_effects :total String(s::Symbol) = unsafe_string(unsafe_convert(Ptr{UInt8}, s))

unsafe_wrap(::Type{Memory{UInt8}}, s::String) = ccall(:jl_string_to_genericmemory, Ref{Memory{UInt8}}, (Any,), s)
unsafe_wrap(::Type{Vector{UInt8}}, s::String) = wrap(Array, unsafe_wrap(Memory{UInt8}, s))

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
    GC.@preserve a b begin
        pa = unsafe_convert(Ptr{UInt8}, a)
        pb = unsafe_convert(Ptr{UInt8}, b)
        memcmp(pa, pb, len % Csize_t) % Int
    end
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
    (@noinline function _thisind_continued(s, i, n) # mark the rest of the function as a slow-path
        local b
        @inbounds b = codeunit(s, i-1)
        between(b, 0b11000000, 0b11110111) && return i-1
        (b & 0xc0 == 0x80) & (i-2 > 0) || return i
        @inbounds b = codeunit(s, i-2)
        between(b, 0b11100000, 0b11110111) && return i-2
        (b & 0xc0 == 0x80) & (i-3 > 0) || return i
        @inbounds b = codeunit(s, i-3)
        between(b, 0b11110000, 0b11110111) && return i-3
        return i
    end)(s, i, n)
end

@propagate_inbounds nextind(s::String, i::Int) = _nextind_str(s, i)

# s should be String or SubString{String}
@inline function _nextind_str(s, i::Int)
    i == 0 && return 1
    n = ncodeunits(s)
    @boundscheck between(i, 1, n) || throw(BoundsError(s, i))
    @inbounds l = codeunit(s, i)
    between(l, 0x80, 0xf7) || return i+1
    (@noinline function _nextind_continued(s, i, n, l) # mark the rest of the function as a slow-path
        if l < 0xc0
            # handle invalid codeunit index by scanning back to the start of this index
            # (which may be the same as this index)
            i′ = @inbounds thisind(s, i)
            i′ >= i && return i+1
            i = i′
            @inbounds l = codeunit(s, i)
            (l < 0x80) | (0xf8 ≤ l) && return i+1
            @assert l >= 0xc0 "invalid codeunit"
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
        return ifelse(b & 0xc0 ≠ 0x80, i, i+1)
    end)(s, i, n, l)
end

## checking UTF-8 & ACSII validity ##
#=
    The UTF-8 Validation is performed by a shift based DFA.
    ┌───────────────────────────────────────────────────────────────────┐
    │    UTF-8 DFA State Diagram    ┌──────────────2──────────────┐     │
    │                               ├────────3────────┐           │     │
    │                 ┌──────────┐  │     ┌─┐        ┌▼┐          │     │
    │      ASCII      │  UTF-8   │  ├─5──►│9├───1────► │          │     │
    │                 │          │  │     ├─┤        │ │         ┌▼┐    │
    │                 │  ┌─0─┐   │  ├─6──►│8├─1,7,9──►4├──1,7,9──► │    │
    │      ┌─0─┐      │  │   │   │  │     ├─┤        │ │         │ │    │
    │      │   │      │ ┌▼───┴┐  │  ├─11─►│7├──7,9───► │ ┌───────►3├─┐  │
    │     ┌▼───┴┐     │ │     │  ▼  │     └─┘        └─┘ │       │ │ │  │
    │     │  0  ├─────┘ │  1  ├─► ──┤                    │  ┌────► │ │  │
    │     └─────┘       │     │     │     ┌─┐            │  │    └─┘ │  │
    │                   └──▲──┘     ├─10─►│5├─────7──────┘  │        │  │
    │                      │        │     ├─┤               │        │  │
    │                      │        └─4──►│6├─────1,9───────┘        │  │
    │          INVALID     │              └─┘                        │  │
    │           ┌─*─┐      └──────────────────1,7,9──────────────────┘  │
    │          ┌▼───┴┐                                                  │
    │          │  2  ◄─── All undefined transitions result in state 2   │
    │          └─────┘                                                  │
    └───────────────────────────────────────────────────────────────────┘

        Validation States
            0 -> _UTF8_DFA_ASCII is the start state and will only stay in this state if the string is only ASCII characters
                        If the DFA ends in this state the string is ASCII only
            1 -> _UTF8_DFA_ACCEPT is the valid complete character state of the DFA once it has encountered a UTF-8 Unicode character
            2 -> _UTF8_DFA_INVALID is only reached by invalid bytes and once in this state it will not change
                    as seen by all 1s in that column of table below
            3 -> One valid continuation byte needed to return to state 0
        4,5,6 -> Two valid continuation bytes needed to return to state 0
        7,8,9 -> Three valids continuation bytes needed to return to state 0

                        Current State
                    0̲  1̲  2̲  3̲  4̲  5̲  6̲  7̲  8̲  9̲
                0 | 0  1  2  2  2  2  2  2  2  2
                1 | 2  2  2  1  3  2  3  2  4  4
                2 | 3  3  2  2  2  2  2  2  2  2
                3 | 4  4  2  2  2  2  2  2  2  2
                4 | 6  6  2  2  2  2  2  2  2  2
    Character   5 | 9  9  2  2  2  2  2  2  2  2     <- Next State
    Class       6 | 8  8  2  2  2  2  2  2  2  2
                7 | 2  2  2  1  3  3  2  4  4  2
                8 | 2  2  2  2  2  2  2  2  2  2
                9 | 2  2  2  1  3  2  3  4  4  2
               10 | 5  5  2  2  2  2  2  2  2  2
               11 | 7  7  2  2  2  2  2  2  2  2

           Shifts | 0  4 10 14 18 24  8 20 12 26

    The shifts that represent each state were derived using the SMT solver Z3, to ensure when encoded into
    the rows the correct shift was a result.

    Each character class row is encoding 10 states with shifts as defined above. By shifting the bitsof a row by
    the current state then masking the result with 0x11110 give the shift for the new state


=#

#State type used by UTF-8 DFA
const _UTF8DFAState = UInt32
# Fill the table with 256 UInt64 representing the DFA transitions for all bytes
const _UTF8_DFA_TABLE = let # let block rather than function doesn't pollute base
    num_classes=12
    num_states=10
    bit_per_state = 6

    # These shifts were derived using a SMT solver
    state_shifts = [0, 4, 10, 14, 18, 24, 8, 20, 12, 26]

    character_classes = [   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                            9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
                            7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
                            7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
                            8, 8, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                            10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 3, 3,
                            11, 6, 6, 6, 5, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8 ]

    # These are the rows discussed in comments above
    state_arrays = [ 0  1  2  2  2  2  2  2  2  2;
                     2  2  2  1  3  2  3  2  4  4;
                     3  3  2  2  2  2  2  2  2  2;
                     4  4  2  2  2  2  2  2  2  2;
                     6  6  2  2  2  2  2  2  2  2;
                     9  9  2  2  2  2  2  2  2  2;
                     8  8  2  2  2  2  2  2  2  2;
                     2  2  2  1  3  3  2  4  4  2;
                     2  2  2  2  2  2  2  2  2  2;
                     2  2  2  1  3  2  3  4  4  2;
                     5  5  2  2  2  2  2  2  2  2;
                     7  7  2  2  2  2  2  2  2  2]

    #This converts the state_arrays into the shift encoded _UTF8DFAState
    class_row = zeros(_UTF8DFAState, num_classes)

    for i = 1:num_classes
        row = _UTF8DFAState(0)
        for j in 1:num_states
            #Calculate the shift required for the next state
            to_shift = UInt8((state_shifts[state_arrays[i,j]+1]) )
            #Shift the next state into the position of the current state
            row = row | (_UTF8DFAState(to_shift) << state_shifts[j])
        end
        class_row[i]=row
    end

    map(c->class_row[c+1],character_classes)
end


const _UTF8_DFA_ASCII = _UTF8DFAState(0) #This state represents the start and end of any valid string
const _UTF8_DFA_ACCEPT = _UTF8DFAState(4) #This state represents the start and end of any valid string
const _UTF8_DFA_INVALID = _UTF8DFAState(10) # If the state machine is ever in this state just stop

# The dfa step is broken out so that it may be used in other functions. The mask was calculated to work with state shifts above
@inline _utf_dfa_step(state::_UTF8DFAState, byte::UInt8) = @inbounds (_UTF8_DFA_TABLE[byte+1] >> state) & _UTF8DFAState(0x0000001E)

@inline function _isvalid_utf8_dfa(state::_UTF8DFAState, bytes::AbstractVector{UInt8}, first::Int = firstindex(bytes), last::Int = lastindex(bytes))
    for i = first:last
       @inbounds state = _utf_dfa_step(state, bytes[i])
    end
    return (state)
end

@inline function  _find_nonascii_chunk(chunk_size,cu::AbstractVector{CU}, first,last) where {CU}
    n=first
    while n <= last - chunk_size
        _isascii(cu,n,n+chunk_size-1) || return n
        n += chunk_size
    end
    n= last-chunk_size+1
    _isascii(cu,n,last) || return n
    return nothing
end

##

# Classifcations of string
    # 0: neither valid ASCII nor UTF-8
    # 1: valid ASCII
    # 2: valid UTF-8
 byte_string_classify(s::AbstractString) = byte_string_classify(codeunits(s))


function byte_string_classify(bytes::AbstractVector{UInt8})
    chunk_size = 1024
    chunk_threshold =  chunk_size + (chunk_size ÷ 2)
    n = length(bytes)
    if n > chunk_threshold
        start = _find_nonascii_chunk(chunk_size,bytes,1,n)
        isnothing(start) && return 1
    else
        _isascii(bytes,1,n) && return 1
        start = 1
    end
    return _byte_string_classify_nonascii(bytes,start,n)
end

function _byte_string_classify_nonascii(bytes::AbstractVector{UInt8}, first::Int, last::Int)
    chunk_size = 256

    start = first
    stop = min(last,first + chunk_size - 1)
    state = _UTF8_DFA_ACCEPT
    while start <= last
        # try to process ascii chunks
        while state == _UTF8_DFA_ACCEPT
            _isascii(bytes,start,stop) || break
            (start = start + chunk_size) <= last || break
            stop = min(last,stop + chunk_size)
        end
        # Process non ascii chunk
        state = _isvalid_utf8_dfa(state,bytes,start,stop)
        state == _UTF8_DFA_INVALID && return 0

        start = start + chunk_size
        stop = min(last,stop + chunk_size)
    end
    return ifelse(state == _UTF8_DFA_ACCEPT,2,0)
end

isvalid(::Type{String}, bytes::AbstractVector{UInt8}) = (@inline byte_string_classify(bytes)) ≠ 0
isvalid(::Type{String}, s::AbstractString) =  (@inline byte_string_classify(s)) ≠ 0

@inline isvalid(s::AbstractString) = @inline isvalid(String, codeunits(s))

is_valid_continuation(c) = c & 0xc0 == 0x80

## required core functionality ##

@inline function iterate(s::String, i::Int=firstindex(s))
    (i % UInt) - 1 < ncodeunits(s) || return nothing
    b = @inbounds codeunit(s, i)
    u = UInt32(b) << 24
    between(b, 0x80, 0xf7) || return reinterpret(Char, u), i+1
    return @noinline iterate_continued(s, i, u)
end

# duck-type s so that external UTF-8 string packages like StringViews can hook in
function iterate_continued(s, i::Int, u::UInt32)
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

# duck-type s so that external UTF-8 string packages like StringViews can hook in
function getindex_continued(s, i::Int, u::UInt32)
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
    r < 0 && throw(ArgumentError("can't repeat a character $r times"))
    r = UInt(r)::UInt
    c = Char(c)::Char
    r == 0 && return ""
    u = bswap(reinterpret(UInt32, c))
    n = 4 - (leading_zeros(u | 0xff) >> 3)
    s = _string_n(n*r)
    p = pointer(s)
    GC.@preserve s if n == 1
        memset(p, u % UInt8, r)
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
