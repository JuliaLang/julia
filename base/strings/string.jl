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

##
#=
  ┌─────────────────────────────────────────────────────┐
  │                 Forward Mode State Diagram          │
  │  GENERALIZED  ┌──────────────2──────────────┐       │
  │    UTF-8      │                             │       │
  │               ├────────3────────┐           │       │
  │   GUTF-8      │                 │           │       │
  │    ┌─0─┐      │     ┌─┐        ┌▼┐         ┌▼┐      │
  │    │   │      ├─4──►│3├───1────►2├────1────►1├────┐ │
  │   ┌▼───┴┐     │     └─┘        └─┘         └─┘    │ │
  │   │  0  ├─────┘   Needs 3    Needs 2     Needs 1  │ │
  │   └───▲─┘        ContBytes  ContBytes   ContBytes │ │
  │       │                                           │ │
  │       │           ContByte=Transition 1           │ │
  │       └─────────────────────1─────────────────────┘ │
  │  ┌─┐                                                │
  │  │4│◄───All undefined transitions result in state 4 │
  │  └─┘      State machine must be reset after state 4 │
  └─────────────────────────────────────────────────────┘

  ┌─────────────────────────────────────────────────────┐
  │                 Reverse Mode State Diagram          │
  │  GENERALIZED  ┌──◄───────────2:4────────────┐       │
  │    UTF-8      │                             │       │
  │   GUTF-8      ├──◄─────3:4──────┐           │       │
  │               │                 │           │       │
  │  ┌─0,2:4─┐    │     ┌─┐        ┌┴┐         ┌┴┐      │
  │  │       │    ├─4───┤3│◄──1────┤2│◄───1────┤1│◄───┐ │
  │ ┌▼───────┴┐   │     └─┘        └─┘         └─┘    │ │
  │ │    0    │◄──┘   Needs 3    Needs 2     Needs 1  │ │
  │ └─────┬───┘      ContBytes  ContBytes   ContBytes │ │
  │       │                                           │ │
  │       │           ContByte=Transition 1           │ │
  │       └─────────────────────1─────────────────────┘ │
  │  ┌─┐                                                │
  │  │4│◄───All undefined transitions result in state 4 │
  │  └─┘      State machine must be reset after state 4 │
  └─────────────────────────────────────────────────────┘
=#
const _GUTF8State = UInt16
const _GUTF8_SHIFT_MASK = _GUTF8State(0b1111)
const _GUTF8_DFA_ACCEPT = _GUTF8State(0)
const _GUTF8_DFA_INVALID = _GUTF8State(4)

const _GUTF8_DFA_TABLE, _GUTF8_DFA_REVERSE_TABLE = let
    # It should be noted that even though the invalid state is state 4 the shift is 1
    # which is the second lowest state shift.
    shifts = [0, 13, 6, 10, 4]

    # Both of these state tables are only 4 states wide even though there are 5 states
    # because the machine must be reset once it is in state 4
    forward_state_table  = [    [0,  4,  4,  4],
                                [4,  0,  1,  2],
                                [1,  4,  4,  4],
                                [2,  4,  4,  4],
                                [3,  4,  4,  4],
                                [4,  4,  4,  4] ]

    reverse_state_table  = [    [0,  4,  4,  4],
                                [1,  2,  3,  4],
                                [0,  0,  4,  4],
                                [0,  0,  0,  4],
                                [0,  0,  0,  0],
                                [4,  4,  4,  4] ]


    f(from, to) = _GUTF8State(shifts[to + 1]) << shifts[from + 1]
    r(state_row) = |([f(n - 1, state_row[n]) for n in 1:length(state_row)]...)
    forward_class_rows = [r(forward_state_table[n]) for n in 1:length(forward_state_table)]
    reverse_class_rows = [r(reverse_state_table[n]) for n in 1:length(reverse_state_table)]

    byte_class = [  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    #  0x00:0x0F      00000000:00001111
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    #  0x10:0x1F      00010000:00011111
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    #  0x20:0x2F      00100000:00101111
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    #  0x30:0x3F      00110000:00111111
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    #  0x40:0x4F      01000000:01001111
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    #  0x50:0x5F      01010000:01011111
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    #  0x60:0x6F      01100000:01101111
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    #  0x70:0x7F      01110000:01111111
                    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,    #  0x80:0x8F      10000000:10001111
                    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,    #  0x90:0x9F      10010000:10011111
                    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,    #  0xA0:0xAF      10100000:10101111
                    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,    #  0xB0:0xBF      10110000:10111111
                    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,    #  0xC0:0xCF      11000000:11001111
                    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,    #  0xD0:0xDF      11010000:11011111
                    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,    #  0xE0:0xEF      11100000:11101111
                    4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5  ]  #  0xF0:0xFF      11110000:11111111
    forward_dfa_table = zeros(_GUTF8State, 256)
    reverse_dfa_table = zeros(_GUTF8State, 256)
    for n in 1:256
        forward_dfa_table[n] = forward_class_rows[1 + byte_class[n]]
        reverse_dfa_table[n] = reverse_class_rows[1 + byte_class[n]]
    end
    (forward_dfa_table, reverse_dfa_table)
end
##
@inline function _gutf8_dfa_step(state::_GUTF8State, byte::UInt8)
    @inbounds (_GUTF8_DFA_TABLE[byte + 1] >> state) & _GUTF8_SHIFT_MASK
end

@inline function _gutf8_dfa_reverse_step(state::_GUTF8State, byte::UInt8)
    @inbounds (_GUTF8_DFA_REVERSE_TABLE[byte + 1] >> state) & _GUTF8_SHIFT_MASK
end

## thisind, nextind ##

@propagate_inbounds thisind(s::String, i::Int) = _thisind_str(s, i)

# s should be String or SubString{String}
@inline function _thisind_str(s, i::Int)
    i == 0 && return 0
    n = ncodeunits(s)
    (i == n + 1) | (i == 1) && return i
    @boundscheck Base.between(i, 1, n) || throw(BoundsError(s, i))
    bytes = codeunits(s)
    state = _GUTF8_DFA_ACCEPT
    for j in 0:3
        k = i - j
        state = @inbounds _gutf8_dfa_reverse_step(state, bytes[k])
        (state == _GUTF8_DFA_ACCEPT) && return k
        (state == _GUTF8_DFA_INVALID) | (k <= 1) && return i
    end
    return i # Should never get here
end

@propagate_inbounds nextind(s::String, i::Int) = _nextind_str(s, i)

# s should be String or SubString{String}
@inline function _nextind_str(s, i::Int)
    i == 0 && return 1
    n = ncodeunits(s)
    @boundscheck between(i, 1, n) || throw(BoundsError(s, i))
    bytes = codeunits(s)
    @inbounds l = bytes[i]
    (l < 0x80) | (0xf8 ≤ l) && return i + 1
    if l < 0xc0
        i′ = @inbounds thisind(s, i)
        (i′ >= i) && return i + 1
        i = i′
    end
    state = _GUTF8_DFA_ACCEPT
    for j in 0:3
        k = i + j
        state = @inbounds _gutf8_dfa_step(state, bytes[k])
        (state == _GUTF8_DFA_INVALID) && return k #The screening above makes sure this is never returned when k == i
        (state == _GUTF8_DFA_ACCEPT) | (k >= n) && return k + 1
    end
    return i + 4 # Should never get here
end

## checking UTF-8 & ASCII validity ##
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

# Classifications of string
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
    (b < 0x80) && return reinterpret(Char, u), i + 1
    return iterate_continued(s, i, b, u)
end

function iterate_continued(s::String, i::Int, b::UInt8, u::UInt32)
    n = ncodeunits(s)
    state = _GUTF8_DFA_ACCEPT
    state = _gutf8_dfa_step(state, b)
    k = i
    state <= _GUTF8_DFA_INVALID && @goto ret_kp1
    shift = 24
    for j in 1:3
        k = i + j
        @inbounds b = codeunit(s, k)
        state = _gutf8_dfa_step(state, b)
        state == _GUTF8_DFA_INVALID && @goto ret
        u |= UInt32(b) << (shift -= 8)
        (state == _GUTF8_DFA_ACCEPT) && @goto ret_kp1
        (k >= n) && @goto ret_kp1
    end
    @label ret_kp1
    k += 1
    @label ret
    return reinterpret(Char, u), k
end
##

@propagate_inbounds function getindex(s::String, i::Int)
    b = codeunit(s, i)
    u = UInt32(b) << 24
    #Check u rather than b here because it force compiler to calculate u now
    (b >= 0x80) || return reinterpret(Char, u)
    return getindex_continued(s, i, u)
end

function getindex_continued(s::String, i::Int, u::UInt32)
    @inbounds b = codeunit(s,i) #It is faster to refetch b than recalculate u
    n = ncodeunits(s)
    (i == n) && @goto ret
    shift = 24
    state = _gutf8_dfa_step(_GUTF8_DFA_ACCEPT, b)
    if (state == _GUTF8_DFA_INVALID)
        #Checks whether i is not at the beginning of a character which is an error
        # or a single invalid byte which returns
        @inbounds isvalid(s, i) && @goto ret
        Base.string_index_err(s, i)
    end
    for j in 1:3
        k = i + j
        @inbounds b = codeunit(s, k)
        state = _gutf8_dfa_step(state, b)
        #If the state machine goes to invalid return value from before byte was processed
        state == _GUTF8_DFA_INVALID && break
        u |= UInt32(b) << (shift -= 8)
        ((state == _GUTF8_DFA_ACCEPT) | (k == n)) && break
    end
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

const _STRING_LENGTH_CHUNKING_SIZE = 256

# The current implimentation of this function favors ascii heavy text more than multibyte,
#  currently it uses a fast loop to scan for non ascii characters then when it encounters a
#  multibyte character it process only a single multbyte character before going back to look
#  for non-ascii characters.  A more balanced algorithm would likely want to process multibyte
#  characters in blocks of 64 bytes
function _length_nonascii_decrement(
    cu::AbstractVector{UInt8}, first::Int, last::Int, c::Int, state=_GUTF8_DFA_ACCEPT
)
    state = ifelse(state == _GUTF8_DFA_INVALID, _GUTF8_DFA_ACCEPT, state)
    i = ifelse(state == _GUTF8_DFA_ACCEPT, first - 1, first)
    #@inbounds b = codeunit(s, first)
    @inbounds b = cu[first]
    @inbounds while true
        #This logic enables the first state to be >_GUTF8_DFA_INVALID so that a chunk
        # can continue from a previous chunk
        (state == _GUTF8_DFA_ACCEPT) && (i += 1)
        #Logic was taken out of the n=1:3 loop below so we must correct the count here
        (state == _GUTF8_DFA_INVALID) && (c += 1)
        if state <= _GUTF8_DFA_INVALID
            #Loop through all the one byte characters
            while true
                b = cu[i]
                ((i += 1) <= last) || break
                0xc0 ≤ b ≤ 0xf7 && break
            end
            state = _gutf8_dfa_step(_GUTF8_DFA_ACCEPT, b)
            (i <= last) || return (c, state)
        end

        #This should get unrolled
        for n in 1:3
            state = _gutf8_dfa_step(state, cu[i])
            c -= 1
            state <= _GUTF8_DFA_INVALID && break
            ((i += 1) <= last) || return (c, state)
        end
    end
    return (c, state)
end

function _length_continued_nonascii(
    cu::AbstractVector{UInt8}, first::Int, last::Int, c::Int
)
    chunk_size = _STRING_LENGTH_CHUNKING_SIZE

    start = first
    stop = min(last, first + chunk_size - 1)
    state = _GUTF8_DFA_ACCEPT

    while start <= last
        #First we process a non ascii chunk because we assume the barrier
        # function sent it here for a reason
        (c, state) = _length_nonascii_decrement(cu, start, stop, c, state)
        start = start + chunk_size
        stop = min(last, stop + chunk_size)

        while state <= _GUTF8_DFA_INVALID
            _isascii(cu, start, stop) || break
            (start = start + chunk_size) <= last || break
            stop = min(last, stop + chunk_size)
        end
    end
    return c
end

@assume_effects :foldable @inline @propagate_inbounds  function length_continued(s::String, first::Int, last::Int, c::Int)
    cu = codeunits(s)
    chunk_size = _STRING_LENGTH_CHUNKING_SIZE
    first < last || return c

    epilog_bytes = rem(last - first + 1, chunk_size)
    start = first

    chunk_last = last - epilog_bytes
    start == last && return c
    for start in start:chunk_size:chunk_last
        _isascii(cu, start, start + chunk_size - 1) ||
            return _length_continued_nonascii(cu, start, last, c)
    end
    ((chunk_last + 1 <= last) && _isascii(cu, chunk_last + 1, last)) ||
        return _length_continued_nonascii(cu, chunk_last + 1, last, c)
    return c
end

## overload methods for efficiency ##

isvalid(s::String, i::Int) = checkbounds(Bool, s, i) && thisind(s, i) == i

isascii(s::String) = isascii(codeunits(s))

# don't assume effects for general integers since we cannot know their implementation
@assume_effects :foldable repeat(c::Char, r::BitInteger) = @invoke repeat(c::Char, r::Integer)

"""
    repeat(c::AbstractChar, r::Integer)::String

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
