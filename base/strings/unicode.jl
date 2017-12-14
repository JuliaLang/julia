# This file is a part of Julia. License is MIT: https://julialang.org/license

# Various Unicode functionality from the utf8proc library
module Unicode

import Base: show, ==, hash, string, Symbol, isless, length, eltype, start,
             next, done, convert, isvalid, MalformedCharError, ismalformed

# whether codepoints are valid Unicode scalar values, i.e. 0-0xd7ff, 0xe000-0x10ffff

"""
    isvalid(value) -> Bool

Returns `true` if the given value is valid for its type, which currently can be either
`Char` or `String`.

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
be either `Char` or `String`. Values for `Char` can be of type `Char` or [`UInt32`](@ref).
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

isvalid(c::Char) = !ismalformed(c) & ((c ≤ '\ud7ff') | ('\ue000' ≤ c) & (c ≤ '\U10ffff'))
isvalid(::Type{Char}, c::Unsigned) = ((c ≤  0xd7ff ) | ( 0xe000  ≤ c) & (c ≤  0x10ffff ))
isvalid(::Type{Char}, c::Integer)  = isvalid(Char, Unsigned(c))
isvalid(::Type{Char}, c::Char)     = isvalid(c)

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

"""
    Unicode.normalize(s::AbstractString, normalform::Symbol)

Normalize the string `s` according to one of the four "normal forms" of the Unicode
standard: `normalform` can be `:NFC`, `:NFD`, `:NFKC`, or `:NFKD`.  Normal forms C
(canonical composition) and D (canonical decomposition) convert different visually identical
representations of the same abstract string into a single canonical form, with form C being
more compact.  Normal forms KC and KD additionally canonicalize "compatibility equivalents":
they convert characters that are abstractly similar but visually distinct into a single
canonical choice (e.g. they expand ligatures into the individual characters), with form KC
being more compact.

Alternatively, finer control and additional transformations may be be obtained by calling
`Unicode.normalize(s; keywords...)`, where any number of the following boolean keywords
options (which all default to `false` except for `compose`) are specified:

* `compose=false`: do not perform canonical composition
* `decompose=true`: do canonical decomposition instead of canonical composition
  (`compose=true` is ignored if present)
* `compat=true`: compatibility equivalents are canonicalized
* `casefold=true`: perform Unicode case folding, e.g. for case-insensitive string comparison
* `newline2lf=true`, `newline2ls=true`, or `newline2ps=true`: convert various newline
  sequences (LF, CRLF, CR, NEL) into a linefeed (LF), line-separation (LS), or
  paragraph-separation (PS) character, respectively
* `stripmark=true`: strip diacritical marks (e.g. accents)
* `stripignore=true`: strip Unicode's "default ignorable" characters (e.g. the soft hyphen
  or the left-to-right marker)
* `stripcc=true`: strip control characters; horizontal tabs and form feeds are converted to
  spaces; newlines are also converted to spaces unless a newline-conversion flag was
  specified
* `rejectna=true`: throw an error if unassigned code points are found
* `stable=true`: enforce Unicode Versioning Stability

For example, NFKC corresponds to the options `compose=true, compat=true, stable=true`.

# Examples
```jldoctest
julia> using Unicode

julia> "μ" == normalize("µ", compat=true) #LHS: Unicode U+03bc, RHS: Unicode U+00b5
true

julia> normalize("JuLiA", casefold=true)
"julia"

julia> normalize("JúLiA", stripmark=true)
"JuLiA"
```
"""
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
julia> using Unicode

julia> textwidth('α')
1

julia> textwidth('❤')
2
```
"""
function textwidth(c::Char)
    ismalformed(c) && (c = '\ufffd')
    Int(ccall(:utf8proc_charwidth, Cint, (UInt32,), c))
end

"""
    textwidth(s::AbstractString)

Give the number of columns needed to print a string.

# Examples
```jldoctest
julia> using Unicode

julia> textwidth("March")
5
```
"""
textwidth(s::AbstractString) = mapreduce(textwidth, +, 0, s)

lowercase(c::Char) = isascii(c) ? ('A' <= c <= 'Z' ? c + 0x20 : c) :
    Char(ccall(:utf8proc_tolower, UInt32, (UInt32,), c))
uppercase(c::Char) = isascii(c) ? ('a' <= c <= 'z' ? c - 0x20 : c) :
    Char(ccall(:utf8proc_toupper, UInt32, (UInt32,), c))
titlecase(c::Char) = isascii(c) ? ('a' <= c <= 'z' ? c - 0x20 : c) :
    Char(ccall(:utf8proc_totitle, UInt32, (UInt32,), c))

############################################################################

# returns UTF8PROC_CATEGORY code in 0:30 giving Unicode category
function category_code(c::Char)
    ismalformed(c) && return Cint(31)
    c ≤ '\U10ffff' || return Cint(30)
    ccall(:utf8proc_category, Cint, (UInt32,), c)
end

# more human-readable representations of the category code
function category_abbrev(c)
    ismalformed(c) && return "Ma"
    c ≤ '\U10ffff' || return "In"
    unsafe_string(ccall(:utf8proc_category_string, Cstring, (UInt32,), c))
end

category_string(c) = category_strings[category_code(c)+1]

"""
    Unicode.isassigned(c) -> Bool

Returns `true` if the given char or integer is an assigned Unicode code point.

# Examples
```jldoctest
julia> using Unicode

julia> isassigned(101)
true

julia> isassigned('\\x01')
true
```
"""
isassigned(c) = category_code(c) != UTF8PROC_CATEGORY_CN

## libc character class predicates ##

"""
    islower(c::Char) -> Bool

Tests whether a character is a lowercase letter.
A character is classified as lowercase if it belongs to Unicode category Ll,
Letter: Lowercase.

# Examples
```jldoctest
julia> using Unicode

julia> islower('α')
true

julia> islower('Γ')
false

julia> islower('❤')
false
```
"""
islower(c::Char) = category_code(c) == UTF8PROC_CATEGORY_LL

# true for Unicode upper and mixed case

"""
    isupper(c::Char) -> Bool

Tests whether a character is an uppercase letter.
A character is classified as uppercase if it belongs to Unicode category Lu,
Letter: Uppercase, or Lt, Letter: Titlecase.

# Examples
```jldoctest
julia> using Unicode

julia> isupper('γ')
false

julia> isupper('Γ')
true

julia> isupper('❤')
false
```
"""
function isupper(c::Char)
    cat = category_code(c)
    cat == UTF8PROC_CATEGORY_LU || cat == UTF8PROC_CATEGORY_LT
end

"""
    isdigit(c::Char) -> Bool

Tests whether a character is a decimal digit (0-9).

# Examples
```jldoctest
julia> using Unicode

julia> isdigit('❤')
false

julia> isdigit('9')
true

julia> isdigit('α')
false
```
"""
isdigit(c::Char) = '0' <= c <= '9'

"""
    isalpha(c::Char) -> Bool

Tests whether a character is alphabetic.
A character is classified as alphabetic if it belongs to the Unicode general
category Letter, i.e. a character whose category code begins with 'L'.

# Examples
```jldoctest
julia> using Unicode

julia> isalpha('❤')
false

julia> isalpha('α')
true

julia> isalpha('9')
false
```
"""
isalpha(c::Char) = UTF8PROC_CATEGORY_LU <= category_code(c) <= UTF8PROC_CATEGORY_LO

"""
    isnumeric(c::Char) -> Bool

Tests whether a character is numeric.
A character is classified as numeric if it belongs to the Unicode general category Number,
i.e. a character whose category code begins with 'N'.

Note that this broad category includes characters such as ¾ and ௰.
Use [`isdigit`](@ref) to check whether a character a decimal digit between 0 and 9.

# Examples
```jldoctest
julia> using Unicode

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
isnumeric(c::Char) = UTF8PROC_CATEGORY_ND <= category_code(c) <= UTF8PROC_CATEGORY_NO

"""
    isalnum(c::Char) -> Bool

Tests whether a character is alphanumeric.
A character is classified as alphabetic if it belongs to the Unicode general
category Letter or Number, i.e. a character whose category code begins with 'L' or 'N'.

# Examples
```jldoctest
julia> using Unicode

julia> isalnum('❤')
false

julia> isalnum('9')
true

julia> isalnum('α')
true
```
"""
function isalnum(c::Char)
    cat = category_code(c)
    UTF8PROC_CATEGORY_LU <= cat <= UTF8PROC_CATEGORY_LO ||
    UTF8PROC_CATEGORY_ND <= cat <= UTF8PROC_CATEGORY_NO
end

# following C++ only control characters from the Latin-1 subset return true

"""
    iscntrl(c::Char) -> Bool

Tests whether a character is a control character.
Control characters are the non-printing characters of the Latin-1 subset of Unicode.

# Examples
```jldoctest
julia> using Unicode

julia> iscntrl('\\x01')
true

julia> iscntrl('a')
false
```
"""
iscntrl(c::Char) = c <= '\x1f' || '\x7f' <= c <= '\u9f'

"""
    ispunct(c::Char) -> Bool

Tests whether a character belongs to the Unicode general category Punctuation, i.e. a
character whose category code begins with 'P'.

# Examples
```jldoctest
julia> using Unicode

julia> ispunct('α')
false

julia> ispunct('/')
true

julia> ispunct(';')
true
```
"""
ispunct(c::Char) = UTF8PROC_CATEGORY_PC <= category_code(c) <= UTF8PROC_CATEGORY_PO

# \u85 is the Unicode Next Line (NEL) character

"""
    isspace(c::Char) -> Bool

Tests whether a character is any whitespace character. Includes ASCII characters '\\t',
'\\n', '\\v', '\\f', '\\r', and ' ', Latin-1 character U+0085, and characters in Unicode
category Zs.

# Examples
```jldoctest
julia> using Unicode

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
@inline isspace(c::Char) =
    c == ' ' || '\t' <= c <= '\r' || c == '\u85' ||
    '\ua0' <= c && category_code(c) == UTF8PROC_CATEGORY_ZS

"""
    isprint(c::Char) -> Bool

Tests whether a character is printable, including spaces, but not a control character.

# Examples
```jldoctest
julia> using Unicode

julia> isprint('\\x01')
false

julia> isprint('A')
true
```
"""
isprint(c::Char) = UTF8PROC_CATEGORY_LU <= category_code(c) <= UTF8PROC_CATEGORY_ZS

# true in principal if a printer would use ink

"""
    isgraph(c::Char) -> Bool

Tests whether a character is printable, and not a space.
Any character that would cause a printer to use ink should be
classified with `isgraph(c)==true`.

# Examples
```jldoctest
julia> using Unicode

julia> isgraph('\\x01')
false

julia> isgraph('A')
true
```
"""
isgraph(c::Char) = UTF8PROC_CATEGORY_LU <= category_code(c) <= UTF8PROC_CATEGORY_SO

"""
    isxdigit(c::Char) -> Bool

Test whether a character is a valid hexadecimal digit. Note that this does not
include `x` (as in the standard `0x` prefix).

# Examples
```jldoctest
julia> using Unicode

julia> isxdigit('a')
true

julia> isxdigit('x')
false
```
"""
isxdigit(c::Char) = '0'<=c<='9' || 'a'<=c<='f' || 'A'<=c<='F'

## uppercase, lowercase, and titlecase transformations ##

"""
    uppercase(s::AbstractString)

Return `s` with all characters converted to uppercase.

# Examples
```jldoctest
julia> using Unicode

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
julia> using Unicode

julia> lowercase("STRINGS AND THINGS")
"strings and things"
```
"""
lowercase(s::AbstractString) = map(lowercase, s)

"""
    titlecase(s::AbstractString) -> String

Capitalize the first character of each word in `s`.
See also [`ucfirst`](@ref) to capitalize only the first
character in `s`.

# Examples
```jldoctest
julia> titlecase("the Julia programming language")
"The Julia Programming Language"
```
"""
function titlecase(s::AbstractString)
    startword = true
    b = IOBuffer()
    for c in s
        if isspace(c)
            print(b, c)
            startword = true
        else
            print(b, startword ? titlecase(c) : c)
            startword = false
        end
    end
    return String(take!(b))
end

"""
    ucfirst(s::AbstractString) -> String

Return `s` with the first character converted to uppercase (technically "title
case" for Unicode). See also [`titlecase`](@ref) to capitalize the first
character of every word in `s`.

See also: [`lcfirst`](@ref), [`uppercase`](@ref), [`lowercase`](@ref),
[`titlecase`](@ref)

# Examples
```jldoctest
julia> ucfirst("python")
"Python"
```
"""
function ucfirst(s::AbstractString)
    isempty(s) && return ""
    c = s[1]
    c′ = titlecase(c)
    c == c′ ? convert(String, s) :
    string(c′, SubString(s, nextind(s, 1)))
end

"""
    lcfirst(s::AbstractString)

Return `s` with the first character converted to lowercase.

See also: [`ucfirst`](@ref), [`uppercase`](@ref), [`lowercase`](@ref),
[`titlecase`](@ref)

# Examples
```jldoctest
julia> lcfirst("Julia")
"julia"
```
"""
function lcfirst(s::AbstractString)
    isempty(s) && return ""
    c = s[1]
    c′ = lowercase(c)
    c == c′ ? convert(String, s) :
    string(c′, SubString(s, nextind(s, 1)))
end

############################################################################
# iterators for grapheme segmentation

isgraphemebreak(c1::Char, c2::Char) =
    ismalformed(c1) || ismalformed(c2) ||
    ccall(:utf8proc_grapheme_break, Bool, (UInt32, UInt32), c1, c2)

# Stateful grapheme break required by Unicode-9 rules: the string
# must be processed in sequence, with state initialized to Ref{Int32}(0).
# Requires utf8proc v2.0 or later.
function isgraphemebreak!(state::Ref{Int32}, c1::Char, c2::Char)
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

"""
    graphemes(s::AbstractString) -> GraphemeIterator

Returns an iterator over substrings of `s` that correspond to the extended graphemes in the
string, as defined by Unicode UAX #29. (Roughly, these are what users would perceive as
single characters, even though they may contain more than one codepoint; for example a
letter combined with an accent mark is a single grapheme.)
"""
graphemes(s::AbstractString) = GraphemeIterator{typeof(s)}(s)

eltype(::Type{GraphemeIterator{S}}) where {S} = SubString{S}
eltype(::Type{GraphemeIterator{SubString{S}}}) where {S} = SubString{S}

function length(g::GraphemeIterator)
    c0 = typemax(Char)
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
