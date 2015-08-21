# This file is a part of Julia. License is MIT: http://julialang.org/license

# Various Unicode functionality from the utf8proc library
module UTF8proc

import Base: show, ==, hash, string, symbol, isless, length, eltype, start, next, done, convert, isvalid, lowercase, uppercase

export isgraphemebreak

# also exported by Base:
export normalize_string, graphemes, is_assigned_char, charwidth, isvalid,
   islower, isupper, isalpha, isdigit, isnumber, isalnum,
   iscntrl, ispunct, isspace, isprint, isgraph, isblank

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

function utf8proc_map(s::ByteString, flags::Integer)
    p = Ref{Ptr{UInt8}}()
    result = ccall(:utf8proc_map, Cssize_t,
                   (Ptr{UInt8}, Cssize_t, Ref{Ptr{UInt8}}, Cint),
                   s, sizeof(s), p, flags)
    result < 0 && error(bytestring(ccall(:utf8proc_errmsg, Cstring,
                                         (Cssize_t,), result)))
    pointer_to_string(p[], result, true)::ByteString
end

utf8proc_map(s::AbstractString, flags::Integer) = utf8proc_map(bytestring(s), flags)

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

charwidth(c::Char) = Int(ccall(:utf8proc_charwidth, Cint, (UInt32,), c))

lowercase(c::Char) = isascii(c) ? ('A' <= c <= 'Z' ? c + 0x20 : c) : Char(ccall(:utf8proc_tolower, UInt32, (UInt32,), c))
uppercase(c::Char) = isascii(c) ? ('a' <= c <= 'z' ? c - 0x20 : c) : Char(ccall(:utf8proc_toupper, UInt32, (UInt32,), c))

############################################################################

# returns UTF8PROC_CATEGORY code in 0:30 giving Unicode category
function category_code(c)
    return ccall(:utf8proc_category, Cint, (UInt32,), c)
end

is_assigned_char(c) = category_code(c) != UTF8PROC_CATEGORY_CN

## libc character class predicates ##

islower(c::Char) = (category_code(c) == UTF8PROC_CATEGORY_LL)

# true for Unicode upper and mixed case
function isupper(c::Char)
    ccode = category_code(c)
    return ccode == UTF8PROC_CATEGORY_LU || ccode == UTF8PROC_CATEGORY_LT
end

isdigit(c::Char)  = ('0' <= c <= '9')
isalpha(c::Char)  = (UTF8PROC_CATEGORY_LU <= category_code(c) <= UTF8PROC_CATEGORY_LO)
isnumber(c::Char) = (UTF8PROC_CATEGORY_ND <= category_code(c) <= UTF8PROC_CATEGORY_NO)

function isalnum(c::Char)
    ccode = category_code(c)
    return (UTF8PROC_CATEGORY_LU <= ccode <= UTF8PROC_CATEGORY_LO) ||
           (UTF8PROC_CATEGORY_ND <= ccode <= UTF8PROC_CATEGORY_NO)
end

# following C++ only control characters from the Latin-1 subset return true
iscntrl(c::Char) = (c <= Char(0x1f) || Char(0x7f) <= c <= Char(0x9f))

ispunct(c::Char) = (UTF8PROC_CATEGORY_PC <= category_code(c) <= UTF8PROC_CATEGORY_PO)

# \u85 is the Unicode Next Line (NEL) character
@inline isspace(c::Char) = c == ' ' || '\t' <= c <='\r' || c == '\u85' || '\ua0' <= c && category_code(c) == UTF8PROC_CATEGORY_ZS

isprint(c::Char) = (UTF8PROC_CATEGORY_LU <= category_code(c) <= UTF8PROC_CATEGORY_ZS)

# true in principal if a printer would use ink
isgraph(c::Char) = (UTF8PROC_CATEGORY_LU <= category_code(c) <= UTF8PROC_CATEGORY_SO)

for name = ("alnum", "alpha", "cntrl", "digit", "number", "graph",
            "lower", "print", "punct", "space", "upper")
    f = symbol("is",name)
    @eval begin
        function $f(s::AbstractString)
            for c in s
                if !$f(c)
                    return false
                end
            end
            return true
        end
    end
end

############################################################################
# iterators for grapheme segmentation

isgraphemebreak(c1::Char, c2::Char) =
    ccall(:utf8proc_grapheme_break, Bool, (UInt32, UInt32), c1, c2)

immutable GraphemeIterator{S<:AbstractString}
    s::S # original string (for generation of SubStrings)
end
graphemes(s::AbstractString) = GraphemeIterator{typeof(s)}(s)

eltype{S}(::Type{GraphemeIterator{S}}) = SubString{S}

function length(g::GraphemeIterator)
    c0 = Char(0x00ad) # soft hyphen (grapheme break always allowed after this)
    n = 0
    for c in g.s
        n += isgraphemebreak(c0, c)
        c0 = c
    end
    return n
end

start(g::GraphemeIterator) = start(g.s)
done(g::GraphemeIterator, i) = done(g.s, i)

function next(g::GraphemeIterator, i)
    s = g.s
    j = i
    c0, k = next(s, i)
    while !done(s, k) # loop until next grapheme is s[i:j]
        c, ℓ = next(s, k)
        isgraphemebreak(c0, c) && break
        j = k
        k = ℓ
        c0 = c
    end
    return (SubString(s, i, j), k)
end

==(g1::GraphemeIterator, g2::GraphemeIterator) = g1.s == g2.s
hash(g::GraphemeIterator, h::UInt) = hash(g.s, h)
isless(g1::GraphemeIterator, g2::GraphemeIterator) = isless(g1.s, g2.s)

convert{S<:AbstractString}(::Type{S}, g::GraphemeIterator) = convert(S, g.s)

show{S}(io::IO, g::GraphemeIterator{S}) = print(io, "length-$(length(g)) GraphemeIterator{$S} for \"$(g.s)\"")

############################################################################

end # module
