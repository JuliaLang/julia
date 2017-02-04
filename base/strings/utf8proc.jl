# This file is a part of Julia. License is MIT: http://julialang.org/license

# Various Unicode functionality from the utf8proc library
module UTF8proc

import Base: show, ==, hash, string, Symbol, isless, length, eltype, start, next, done, convert, isvalid, lowercase, uppercase, titlecase

export isgraphemebreak, category_code, category_abbrev, category_string

# also exported by Base:
export normalize_string, graphemes, is_assigned_char, charwidth, isvalid,
   islower, isupper, isalpha, isdigit, isnumber, isalnum,
   iscntrl, ispunct, isspace, isprint, isgraph

# whether codepoints are valid Unicode scalar values, i.e. 0-0xd7ff, 0xe000-0x10ffff
isvalid(::Type{Char}, ch::Unsigned) = !((ch - 0xd800 < 0x800) | (ch > 0x10ffff))
isvalid(::Type{Char}, ch::Integer) = isvalid(Char, Unsigned(ch))
isvalid(::Type{Char}, ch::Char) = isvalid(Char, UInt32(ch))

isvalid(ch::Char) = isvalid(Char, ch)

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
    "Other, private use"
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

function normalize_string(s::AbstractString; stable::Bool=false, compat::Bool=false, compose::Bool=true, decompose::Bool=false, stripignore::Bool=false, rejectna::Bool=false, newline2ls::Bool=false, newline2ps::Bool=false, newline2lf::Bool=false, stripcc::Bool=false, casefold::Bool=false, lump::Bool=false, stripmark::Bool=false)
    flags = 0
    stable && (flags = flags | UTF8PROC_STABLE)
    compat && (flags = flags | UTF8PROC_COMPAT)
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
    normalize_string(s::AbstractString, normalform::Symbol)

Normalize the string `s` according to one of the four "normal forms" of the Unicode
standard: `normalform` can be `:NFC`, `:NFD`, `:NFKC`, or `:NFKD`.  Normal forms C
(canonical composition) and D (canonical decomposition) convert different visually identical
representations of the same abstract string into a single canonical form, with form C being
more compact.  Normal forms KC and KD additionally canonicalize "compatibility equivalents":
they convert characters that are abstractly similar but visually distinct into a single
canonical choice (e.g. they expand ligatures into the individual characters), with form KC
being more compact.

Alternatively, finer control and additional transformations may be be obtained by calling
`normalize_string(s; keywords...)`, where any number of the following boolean keywords
options (which all default to `false` except for `compose`) are specified:

* `compose=false`: do not perform canonical composition
* `decompose=true`: do canonical decomposition instead of canonical composition (`compose=true`
  is ignored if present)
* `compat=true`: compatibility equivalents are canonicalized
* `casefold=true`: perform Unicode case folding, e.g. for case-insensitive string comparison
* `newline2lf=true`, `newline2ls=true`, or `newline2ps=true`: convert various newline sequences
  (LF, CRLF, CR, NEL) into a linefeed (LF), line-separation (LS), or paragraph-separation (PS)
  character, respectively
* `stripmark=true`: strip diacritical marks (e.g. accents)
* `stripignore=true`: strip Unicode's "default ignorable" characters (e.g. the soft hyphen
  or the left-to-right marker)
* `stripcc=true`: strip control characters; horizontal tabs and form feeds are converted to
  spaces; newlines are also converted to spaces unless a newline-conversion flag was specified
* `rejectna=true`: throw an error if unassigned code points are found
* `stable=true`: enforce Unicode Versioning Stability

For example, NFKC corresponds to the options `compose=true, compat=true, stable=true`.
"""
function normalize_string(s::AbstractString, nf::Symbol)
    utf8proc_map(s, nf == :NFC ? (UTF8PROC_STABLE | UTF8PROC_COMPOSE) :
                    nf == :NFD ? (UTF8PROC_STABLE | UTF8PROC_DECOMPOSE) :
                    nf == :NFKC ? (UTF8PROC_STABLE | UTF8PROC_COMPOSE
                                   | UTF8PROC_COMPAT) :
                    nf == :NFKD ? (UTF8PROC_STABLE | UTF8PROC_DECOMPOSE
                                   | UTF8PROC_COMPAT) :
                    throw(ArgumentError(":$nf is not one of :NFC, :NFD, :NFKC, :NFKD")))
end

############################################################################

"""
    charwidth(c)

Gives the number of columns needed to print a character.
"""
charwidth(c::Char) = Int(ccall(:utf8proc_charwidth, Cint, (UInt32,), c))

lowercase(c::Char) = isascii(c) ? ('A' <= c <= 'Z' ? c + 0x20 : c) : Char(ccall(:utf8proc_tolower, UInt32, (UInt32,), c))
uppercase(c::Char) = isascii(c) ? ('a' <= c <= 'z' ? c - 0x20 : c) : Char(ccall(:utf8proc_toupper, UInt32, (UInt32,), c))
titlecase(c::Char) = isascii(c) ? ('a' <= c <= 'z' ? c - 0x20 : c) : Char(ccall(:utf8proc_totitle, UInt32, (UInt32,), c))

############################################################################

# returns UTF8PROC_CATEGORY code in 0:30 giving Unicode category
category_code(c) = ccall(:utf8proc_category, Cint, (UInt32,), c)

# more human-readable representations of the category code
category_abbrev(c) = unsafe_string(ccall(:utf8proc_category_string, Cstring, (UInt32,), c))
category_string(c) = category_strings[category_code(c)+1]

"""
    is_assigned_char(c) -> Bool

Returns `true` if the given char or integer is an assigned Unicode code point.
"""
is_assigned_char(c) = category_code(c) != UTF8PROC_CATEGORY_CN

## libc character class predicates ##

"""
    islower(c::Char) -> Bool

Tests whether a character is a lowercase letter.
A character is classified as lowercase if it belongs to Unicode category Ll,
Letter: Lowercase.
"""
islower(c::Char) = (category_code(c) == UTF8PROC_CATEGORY_LL)

# true for Unicode upper and mixed case

"""
    isupper(c::Char) -> Bool

Tests whether a character is an uppercase letter.
A character is classified as uppercase if it belongs to Unicode category Lu,
Letter: Uppercase, or Lt, Letter: Titlecase.
"""
function isupper(c::Char)
    ccode = category_code(c)
    return ccode == UTF8PROC_CATEGORY_LU || ccode == UTF8PROC_CATEGORY_LT
end

"""
    isdigit(c::Char) -> Bool

Tests whether a character is a numeric digit (0-9).
"""
isdigit(c::Char)  = ('0' <= c <= '9')

"""
    isalpha(c::Char) -> Bool

Tests whether a character is alphabetic.
A character is classified as alphabetic if it belongs to the Unicode general
category Letter, i.e. a character whose category code begins with 'L'.
"""
isalpha(c::Char)  = (UTF8PROC_CATEGORY_LU <= category_code(c) <= UTF8PROC_CATEGORY_LO)

"""
    isnumber(c::Char) -> Bool

Tests whether a character is numeric.
A character is classified as numeric if it belongs to the Unicode general category Number,
i.e. a character whose category code begins with 'N'.
"""
isnumber(c::Char) = (UTF8PROC_CATEGORY_ND <= category_code(c) <= UTF8PROC_CATEGORY_NO)

"""
    isalnum(c::Char) -> Bool

Tests whether a character is alphanumeric.
A character is classified as alphabetic if it belongs to the Unicode general
category Letter or Number, i.e. a character whose category code begins with 'L' or 'N'.
"""
function isalnum(c::Char)
    ccode = category_code(c)
    return (UTF8PROC_CATEGORY_LU <= ccode <= UTF8PROC_CATEGORY_LO) ||
           (UTF8PROC_CATEGORY_ND <= ccode <= UTF8PROC_CATEGORY_NO)
end

# following C++ only control characters from the Latin-1 subset return true

"""
    iscntrl(c::Char) -> Bool

Tests whether a character is a control character.
Control characters are the non-printing characters of the Latin-1 subset of Unicode.
"""
iscntrl(c::Char) = (c <= Char(0x1f) || Char(0x7f) <= c <= Char(0x9f))

"""
    ispunct(c::Char) -> Bool

Tests whether a character belongs to the Unicode general category Punctuation, i.e. a
character whose category code begins with 'P'.
"""
ispunct(c::Char) = (UTF8PROC_CATEGORY_PC <= category_code(c) <= UTF8PROC_CATEGORY_PO)

# \u85 is the Unicode Next Line (NEL) character

"""
    isspace(c::Char) -> Bool

Tests whether a character is any whitespace character. Includes ASCII characters '\\t',
'\\n', '\\v', '\\f', '\\r', and ' ', Latin-1 character U+0085, and characters in Unicode
category Zs.
"""
@inline isspace(c::Char) = c == ' ' || '\t' <= c <='\r' || c == '\u85' || '\ua0' <= c && category_code(c) == UTF8PROC_CATEGORY_ZS

"""
    isprint(c::Char) -> Bool

Tests whether a character is printable, including spaces, but not a control character.
"""
isprint(c::Char) = (UTF8PROC_CATEGORY_LU <= category_code(c) <= UTF8PROC_CATEGORY_ZS)

# true in principal if a printer would use ink

"""
    isgraph(c::Char) -> Bool

Tests whether a character is printable, and not a space.
Any character that would cause a printer to use ink should be
classified with `isgraph(c)==true`.
"""
isgraph(c::Char) = (UTF8PROC_CATEGORY_LU <= category_code(c) <= UTF8PROC_CATEGORY_SO)

############################################################################
# iterators for grapheme segmentation

isgraphemebreak(c1::Char, c2::Char) =
    ccall(:utf8proc_grapheme_break, Bool, (UInt32, UInt32), c1, c2)

# Stateful grapheme break required by Unicode-9 rules: the string
# must be processed in sequence, with state initialized to Ref{Int32}(0).
# Requires utf8proc v2.0 or later.
isgraphemebreak!(state::Ref{Int32}, c1::Char, c2::Char) =
    ccall(:utf8proc_grapheme_break_stateful, Bool, (UInt32, UInt32, Ref{Int32}), c1, c2, state)

immutable GraphemeIterator{S<:AbstractString}
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

eltype{S}(::Type{GraphemeIterator{S}}) = SubString{S}

function length(g::GraphemeIterator)
    c0 = Char(0x00ad) # soft hyphen (grapheme break always allowed after this)
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

convert{S<:AbstractString}(::Type{S}, g::GraphemeIterator) = convert(S, g.s)

show{S}(io::IO, g::GraphemeIterator{S}) = print(io, "length-$(length(g)) GraphemeIterator{$S} for \"$(g.s)\"")

############################################################################

end # module
