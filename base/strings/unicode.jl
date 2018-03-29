# This file is a part of Julia. License is MIT: https://julialang.org/license

# Various Unicode functionality from the utf8proc library
module Unicode

import Base: show, ==, hash, string, Symbol, isless, length, eltype, start,
             next, done, convert, isvalid, ismalformed, isoverlong

# whether codepoints are valid Unicode scalar values, i.e. 0-0xd7ff, 0xe000-0x10ffff

"""
    isvalid(value) -> Bool

Returns `true` if the given value is valid for its type, which currently can be either
`AbstractChar` or `String`.

# Examples
```jldoctest
julia> isvalid(Char(0xd800))
false

julia> isvalid(Char(0xd799))
true
```
"""
isvalid(value)

"""
    isvalid(T, value) -> Bool

Returns `true` if the given value is valid for that type. Types currently can
be either `AbstractChar` or `String`. Values for `AbstractChar` can be of type `AbstractChar` or [`UInt32`](@ref).
Values for `String` can be of that type, or `Vector{UInt8}`.

# Examples
```jldoctest
julia> isvalid(Char, 0xd800)
false

julia> isvalid(Char, 0xd799)
true
```
"""
isvalid(T,value)

isvalid(c::AbstractChar) = !ismalformed(c) & !isoverlong(c) & ((c ≤ '\ud7ff') | ('\ue000' ≤ c) & (c ≤ '\U10ffff'))
isvalid(::Type{<:AbstractChar}, c::Unsigned) = ((c ≤  0xd7ff ) | ( 0xe000  ≤ c) & (c ≤  0x10ffff ))
isvalid(::Type{T}, c::Integer) where {T<:AbstractChar}  = isvalid(T, Unsigned(c))
isvalid(::Type{<:AbstractChar}, c::AbstractChar)     = isvalid(c)

# utf8 category constants
const UTF8PROC_CATEGORY_CN = 0
const UTF8PROC_CATEGORY_LU = 1
const UTF8PROC_CATEGORY_LL = 2
const UTF8PROC_CATEGORY_LT = 3
const UTF8PROC_CATEGORY_LM = 4
const UTF8PROC_CATEGORY_LO = 5
const UTF8PROC_CATEGORY_MN = 6
const UTF8PROC_CATEGORY_MC = 7
const UTF8PROC_CATEGORY_ME = 8
const UTF8PROC_CATEGORY_ND = 9
const UTF8PROC_CATEGORY_NL = 10
const UTF8PROC_CATEGORY_NO = 11
const UTF8PROC_CATEGORY_PC = 12
const UTF8PROC_CATEGORY_PD = 13
const UTF8PROC_CATEGORY_PS = 14
const UTF8PROC_CATEGORY_PE = 15
const UTF8PROC_CATEGORY_PI = 16
const UTF8PROC_CATEGORY_PF = 17
const UTF8PROC_CATEGORY_PO = 18
const UTF8PROC_CATEGORY_SM = 19
const UTF8PROC_CATEGORY_SC = 20
const UTF8PROC_CATEGORY_SK = 21
const UTF8PROC_CATEGORY_SO = 22
const UTF8PROC_CATEGORY_ZS = 23
const UTF8PROC_CATEGORY_ZL = 24
const UTF8PROC_CATEGORY_ZP = 25
const UTF8PROC_CATEGORY_CC = 26
const UTF8PROC_CATEGORY_CF = 27
const UTF8PROC_CATEGORY_CS = 28
const UTF8PROC_CATEGORY_CO = 29

# strings corresponding to the category constants
const category_strings = [
    "Other, not assigned",
    "Letter, uppercase",
    "Letter, lowercase",
    "Letter, titlecase",
    "Letter, modifier",
    "Letter, other",
    "Mark, nonspacing",
    "Mark, spacing combining",
    "Mark, enclosing",
    "Number, decimal digit",
    "Number, letter",
    "Number, other",
    "Punctuation, connector",
    "Punctuation, dash",
    "Punctuation, open",
    "Punctuation, close",
    "Punctuation, initial quote",
    "Punctuation, final quote",
    "Punctuation, other",
    "Symbol, math",
    "Symbol, currency",
    "Symbol, modifier",
    "Symbol, other",
    "Separator, space",
    "Separator, line",
    "Separator, paragraph",
    "Other, control",
    "Other, format",
    "Other, surrogate",
    "Other, private use",
    "Invalid, too high",
    "Malformed, bad data",
]

const UTF8PROC_STABLE    = (1<<1)
const UTF8PROC_COMPAT    = (1<<2)
const UTF8PROC_COMPOSE   = (1<<3)
const UTF8PROC_DECOMPOSE = (1<<4)
const UTF8PROC_IGNORE    = (1<<5)
const UTF8PROC_REJECTNA  = (1<<6)
const UTF8PROC_NLF2LS    = (1<<7)
const UTF8PROC_NLF2PS    = (1<<8)
const UTF8PROC_NLF2LF    = (UTF8PROC_NLF2LS | UTF8PROC_NLF2PS)
const UTF8PROC_STRIPCC   = (1<<9)
const UTF8PROC_CASEFOLD  = (1<<10)
const UTF8PROC_CHARBOUND = (1<<11)
const UTF8PROC_LUMP      = (1<<12)
const UTF8PROC_STRIPMARK = (1<<13)

############################################################################

utf8proc_error(result) = error(unsafe_string(ccall(:utf8proc_errmsg, Cstring, (Cssize_t,), result)))

function utf8proc_map(str::String, options::Integer)
    nwords = ccall(:utf8proc_decompose, Int, (Ptr{UInt8}, Int, Ptr{UInt8}, Int, Cint),
                   str, sizeof(str), C_NULL, 0, options)
    nwords < 0 && utf8proc_error(nwords)
    buffer = Base.StringVector(nwords*4)
    nwords = ccall(:utf8proc_decompose, Int, (Ptr{UInt8}, Int, Ptr{UInt8}, Int, Cint),
                   str, sizeof(str), buffer, nwords, options)
    nwords < 0 && utf8proc_error(nwords)
    nbytes = ccall(:utf8proc_reencode, Int, (Ptr{UInt8}, Int, Cint), buffer, nwords, options)
    nbytes < 0 && utf8proc_error(nbytes)
    return String(resize!(buffer, nbytes))
end

utf8proc_map(s::AbstractString, flags::Integer) = utf8proc_map(String(s), flags)

# Documented in Unicode module
function normalize(
    s::AbstractString;
    stable::Bool=false,
    compat::Bool=false,
    compose::Bool=true,
    decompose::Bool=false,
    stripignore::Bool=false,
    rejectna::Bool=false,
    newline2ls::Bool=false,
    newline2ps::Bool=false,
    newline2lf::Bool=false,
    stripcc::Bool=false,
    casefold::Bool=false,
    lump::Bool=false,
    stripmark::Bool=false,
)
    flags = 0
    stable && (flags = flags | UTF8PROC_STABLE)
    compat && (flags = flags | UTF8PROC_COMPAT)
    # TODO: error if compose & decompose?
    if decompose
        flags = flags | UTF8PROC_DECOMPOSE
    elseif compose
        flags = flags | UTF8PROC_COMPOSE
    elseif compat || stripmark
        throw(ArgumentError("compat=true or stripmark=true require compose=true or decompose=true"))
    end
    stripignore && (flags = flags | UTF8PROC_IGNORE)
    rejectna && (flags = flags | UTF8PROC_REJECTNA)
    newline2ls + newline2ps + newline2lf > 1 && throw(ArgumentError("only one newline conversion may be specified"))
    newline2ls && (flags = flags | UTF8PROC_NLF2LS)
    newline2ps && (flags = flags | UTF8PROC_NLF2PS)
    newline2lf && (flags = flags | UTF8PROC_NLF2LF)
    stripcc && (flags = flags | UTF8PROC_STRIPCC)
    casefold && (flags = flags | UTF8PROC_CASEFOLD)
    lump && (flags = flags | UTF8PROC_LUMP)
    stripmark && (flags = flags | UTF8PROC_STRIPMARK)
    utf8proc_map(s, flags)
end

function normalize(s::AbstractString, nf::Symbol)
    utf8proc_map(s, nf == :NFC ? (UTF8PROC_STABLE | UTF8PROC_COMPOSE) :
                    nf == :NFD ? (UTF8PROC_STABLE | UTF8PROC_DECOMPOSE) :
                    nf == :NFKC ? (UTF8PROC_STABLE | UTF8PROC_COMPOSE
                                   | UTF8PROC_COMPAT) :
                    nf == :NFKD ? (UTF8PROC_STABLE | UTF8PROC_DECOMPOSE
                                   | UTF8PROC_COMPAT) :
                    throw(ArgumentError(":$nf is not one of :NFC, :NFD, :NFKC, :NFKD")))
end

############################################################################

## character column width function ##
"""
    textwidth(c)

Give the number of columns needed to print a character.

# Examples
```jldoctest
julia> textwidth('α')
1

julia> textwidth('❤')
2
```
"""
function textwidth(c::AbstractChar)
    ismalformed(c) && return 1
    Int(ccall(:utf8proc_charwidth, Cint, (UInt32,), c))
end

"""
    textwidth(s::AbstractString)

Give the number of columns needed to print a string.

# Examples
```jldoctest
julia> textwidth("March")
5
```
"""
textwidth(s::AbstractString) = mapreduce(textwidth, +, 0, s)

lowercase(c::T) where {T<:AbstractChar} = isascii(c) ? ('A' <= c <= 'Z' ? c + 0x20 : c) :
    T(ccall(:utf8proc_tolower, UInt32, (UInt32,), c))
uppercase(c::T) where {T<:AbstractChar} = isascii(c) ? ('a' <= c <= 'z' ? c - 0x20 : c) :
    T(ccall(:utf8proc_toupper, UInt32, (UInt32,), c))
titlecase(c::T) where {T<:AbstractChar} = isascii(c) ? ('a' <= c <= 'z' ? c - 0x20 : c) :
    T(ccall(:utf8proc_totitle, UInt32, (UInt32,), c))

############################################################################

# returns UTF8PROC_CATEGORY code in 0:30 giving Unicode category
function category_code(c::AbstractChar)
    !ismalformed(c) ? category_code(UInt32(c)) : Cint(31)
end

function category_code(x::Integer)
    x ≤ 0x10ffff ? ccall(:utf8proc_category, Cint, (UInt32,), x) : Cint(30)
end

# more human-readable representations of the category code
function category_abbrev(c::AbstractChar)
    ismalformed(c) && return "Ma"
    c ≤ '\U10ffff' || return "In"
    unsafe_string(ccall(:utf8proc_category_string, Cstring, (UInt32,), c))
end

category_string(c) = category_strings[category_code(c)+1]

isassigned(c) = UTF8PROC_CATEGORY_CN < category_code(c) <= UTF8PROC_CATEGORY_CO

## libc character class predicates ##

"""
    islowercase(c::AbstractChar) -> Bool

Tests whether a character is a lowercase letter.
A character is classified as lowercase if it belongs to Unicode category Ll,
Letter: Lowercase.

# Examples
```jldoctest
julia> islowercase('α')
true

julia> islowercase('Γ')
false

julia> islowercase('❤')
false
```
"""
islowercase(c::AbstractChar) = category_code(c) == UTF8PROC_CATEGORY_LL

# true for Unicode upper and mixed case

"""
    isuppercase(c::AbstractChar) -> Bool

Tests whether a character is an uppercase letter.
A character is classified as uppercase if it belongs to Unicode category Lu,
Letter: Uppercase, or Lt, Letter: Titlecase.

# Examples
```jldoctest
julia> isuppercase('γ')
false

julia> isuppercase('Γ')
true

julia> isuppercase('❤')
false
```
"""
function isuppercase(c::AbstractChar)
    cat = category_code(c)
    cat == UTF8PROC_CATEGORY_LU || cat == UTF8PROC_CATEGORY_LT
end

"""
    iscased(c::AbstractChar) -> Bool

Tests whether a character is cased, i.e. is lower-, upper- or title-cased.
"""
function iscased(c::AbstractChar)
    cat = category_code(c)
    return cat == UTF8PROC_CATEGORY_LU ||
           cat == UTF8PROC_CATEGORY_LT ||
           cat == UTF8PROC_CATEGORY_LL
end


"""
    isdigit(c::AbstractChar) -> Bool

Tests whether a character is a decimal digit (0-9).

# Examples
```jldoctest
julia> isdigit('❤')
false

julia> isdigit('9')
true

julia> isdigit('α')
false
```
"""
isdigit(c::AbstractChar) = '0' <= c <= '9'

"""
    isalpha(c::AbstractChar) -> Bool

Tests whether a character is alphabetic.
A character is classified as alphabetic if it belongs to the Unicode general
category Letter, i.e. a character whose category code begins with 'L'.

# Examples
```jldoctest
julia> isalpha('❤')
false

julia> isalpha('α')
true

julia> isalpha('9')
false
```
"""
isalpha(c::AbstractChar) = UTF8PROC_CATEGORY_LU <= category_code(c) <= UTF8PROC_CATEGORY_LO

"""
    isnumeric(c::AbstractChar) -> Bool

Tests whether a character is numeric.
A character is classified as numeric if it belongs to the Unicode general category Number,
i.e. a character whose category code begins with 'N'.

Note that this broad category includes characters such as ¾ and ௰.
Use [`isdigit`](@ref) to check whether a character a decimal digit between 0 and 9.

# Examples
```jldoctest
julia> isnumeric('௰')
true

julia> isnumeric('9')
true

julia> isnumeric('α')
false

julia> isnumeric('❤')
false
```
"""
isnumeric(c::AbstractChar) = UTF8PROC_CATEGORY_ND <= category_code(c) <= UTF8PROC_CATEGORY_NO

# following C++ only control characters from the Latin-1 subset return true

"""
    iscntrl(c::AbstractChar) -> Bool

Tests whether a character is a control character.
Control characters are the non-printing characters of the Latin-1 subset of Unicode.

# Examples
```jldoctest
julia> iscntrl('\\x01')
true

julia> iscntrl('a')
false
```
"""
iscntrl(c::AbstractChar) = c <= '\x1f' || '\x7f' <= c <= '\u9f'

"""
    ispunct(c::AbstractChar) -> Bool

Tests whether a character belongs to the Unicode general category Punctuation, i.e. a
character whose category code begins with 'P'.

# Examples
```jldoctest
julia> ispunct('α')
false

julia> ispunct('/')
true

julia> ispunct(';')
true
```
"""
ispunct(c::AbstractChar) = UTF8PROC_CATEGORY_PC <= category_code(c) <= UTF8PROC_CATEGORY_PO

# \u85 is the Unicode Next Line (NEL) character

"""
    isspace(c::AbstractChar) -> Bool

Tests whether a character is any whitespace character. Includes ASCII characters '\\t',
'\\n', '\\v', '\\f', '\\r', and ' ', Latin-1 character U+0085, and characters in Unicode
category Zs.

# Examples
```jldoctest
julia> isspace('\\n')
true

julia> isspace('\\r')
true

julia> isspace(' ')
true

julia> isspace('\\x20')
true
```
"""
@inline isspace(c::AbstractChar) =
    c == ' ' || '\t' <= c <= '\r' || c == '\u85' ||
    '\ua0' <= c && category_code(c) == UTF8PROC_CATEGORY_ZS

"""
    isprint(c::AbstractChar) -> Bool

Tests whether a character is printable, including spaces, but not a control character.

# Examples
```jldoctest
julia> isprint('\\x01')
false

julia> isprint('A')
true
```
"""
isprint(c::AbstractChar) = UTF8PROC_CATEGORY_LU <= category_code(c) <= UTF8PROC_CATEGORY_ZS

# true in principal if a printer would use ink

"""
    isxdigit(c::AbstractChar) -> Bool

Test whether a character is a valid hexadecimal digit. Note that this does not
include `x` (as in the standard `0x` prefix).

# Examples
```jldoctest
julia> isxdigit('a')
true

julia> isxdigit('x')
false
```
"""
isxdigit(c::AbstractChar) = '0'<=c<='9' || 'a'<=c<='f' || 'A'<=c<='F'

## uppercase, lowercase, and titlecase transformations ##

"""
    uppercase(s::AbstractString)

Return `s` with all characters converted to uppercase.

# Examples
```jldoctest
julia> uppercase("Julia")
"JULIA"
```
"""
uppercase(s::AbstractString) = map(uppercase, s)

"""
    lowercase(s::AbstractString)

Return `s` with all characters converted to lowercase.

# Examples
```jldoctest
julia> lowercase("STRINGS AND THINGS")
"strings and things"
```
"""
lowercase(s::AbstractString) = map(lowercase, s)

"""
    titlecase(s::AbstractString; [wordsep::Function], strict::Bool=true) -> String

Capitalize the first character of each word in `s`;
if `strict` is true, every other character is
converted to lowercase, otherwise they are left unchanged.
By default, all non-letters are considered as word separators;
a predicate can be passed as the `wordsep` keyword to determine
which characters should be considered as word separators.
See also [`uppercasefirst`](@ref) to capitalize only the first
character in `s`.

# Examples
```jldoctest
julia> titlecase("the JULIA programming language")
"The Julia Programming Language"

julia> titlecase("ISS - international space station", strict=false)
"ISS - International Space Station"

julia> titlecase("a-a b-b", wordsep = c->c==' ')
"A-a B-b"
```
"""
function titlecase(s::AbstractString; wordsep::Function = !iscased, strict::Bool=true)
    startword = true
    b = IOBuffer()
    for c in s
        if wordsep(c)
            print(b, c)
            startword = true
        else
            print(b, startword ? titlecase(c) : strict ? lowercase(c) : c)
            startword = false
        end
    end
    return String(take!(b))
end

"""
    uppercasefirst(s::AbstractString) -> String

Return `s` with the first character converted to uppercase (technically "title
case" for Unicode). See also [`titlecase`](@ref) to capitalize the first
character of every word in `s`.

See also: [`lowercasefirst`](@ref), [`uppercase`](@ref), [`lowercase`](@ref),
[`titlecase`](@ref)

# Examples
```jldoctest
julia> uppercasefirst("python")
"Python"
```
"""
function uppercasefirst(s::AbstractString)
    isempty(s) && return ""
    c = s[1]
    c′ = titlecase(c)
    c == c′ ? convert(String, s) :
    string(c′, SubString(s, nextind(s, 1)))
end

"""
    lowercasefirst(s::AbstractString)

Return `s` with the first character converted to lowercase.

See also: [`uppercasefirst`](@ref), [`uppercase`](@ref), [`lowercase`](@ref),
[`titlecase`](@ref)

# Examples
```jldoctest
julia> lowercasefirst("Julia")
"julia"
```
"""
function lowercasefirst(s::AbstractString)
    isempty(s) && return ""
    c = s[1]
    c′ = lowercase(c)
    c == c′ ? convert(String, s) :
    string(c′, SubString(s, nextind(s, 1)))
end

############################################################################
# iterators for grapheme segmentation

isgraphemebreak(c1::AbstractChar, c2::AbstractChar) =
    ismalformed(c1) || ismalformed(c2) ||
    ccall(:utf8proc_grapheme_break, Bool, (UInt32, UInt32), c1, c2)

# Stateful grapheme break required by Unicode-9 rules: the string
# must be processed in sequence, with state initialized to Ref{Int32}(0).
# Requires utf8proc v2.0 or later.
function isgraphemebreak!(state::Ref{Int32}, c1::AbstractChar, c2::AbstractChar)
    if ismalformed(c1) || ismalformed(c2)
        state[] = 0
        return true
    end
    ccall(:utf8proc_grapheme_break_stateful, Bool,
          (UInt32, UInt32, Ref{Int32}), c1, c2, state)
end

struct GraphemeIterator{S<:AbstractString}
    s::S # original string (for generation of SubStrings)
end

# Documented in Unicode module
graphemes(s::AbstractString) = GraphemeIterator{typeof(s)}(s)

eltype(::Type{GraphemeIterator{S}}) where {S} = SubString{S}
eltype(::Type{GraphemeIterator{SubString{S}}}) where {S} = SubString{S}

function length(g::GraphemeIterator{S}) where {S}
    c0 = eltype(S)(0x00000000)
    n = 0
    state = Ref{Int32}(0)
    for c in g.s
        n += isgraphemebreak!(state, c0, c)
        c0 = c
    end
    return n
end

start(g::GraphemeIterator) = (start(g.s), Ref{Int32}(0))
done(g::GraphemeIterator, i) = done(g.s, i[1])

function next(g::GraphemeIterator, i_)
    s = g.s
    i, state = i_
    j = i
    c0, k = next(s, i)
    while !done(s, k) # loop until next grapheme is s[i:j]
        c, ℓ = next(s, k)
        isgraphemebreak!(state, c0, c) && break
        j = k
        k = ℓ
        c0 = c
    end
    return (SubString(s, i, j), (k, state))
end

==(g1::GraphemeIterator, g2::GraphemeIterator) = g1.s == g2.s
hash(g::GraphemeIterator, h::UInt) = hash(g.s, h)
isless(g1::GraphemeIterator, g2::GraphemeIterator) = isless(g1.s, g2.s)

show(io::IO, g::GraphemeIterator{S}) where {S} = print(io, "length-$(length(g)) GraphemeIterator{$S} for \"$(g.s)\"")

############################################################################

end # module
