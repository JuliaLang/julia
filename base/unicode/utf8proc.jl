# This file is a part of Julia. License is MIT: http://julialang.org/license

# Various Unicode functionality from the utf8proc library

const STABLE    = (1<<1)
const COMPAT    = (1<<2)
const COMPOSE   = (1<<3)
const DECOMPOSE = (1<<4)
const IGNORE    = (1<<5)
const REJECTNA  = (1<<6)
const NLF2LS    = (1<<7)
const NLF2PS    = (1<<8)
const STRIPCC   = (1<<9)
const CASEFOLD  = (1<<10)
const CHARBOUND = (1<<11)
const LUMP      = (1<<12)
const STRIPMARK = (1<<13)

const NLF2LF = (NLF2LS | NLF2PS)

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

function normalize_string(s::AbstractString; stable::Bool=false, compat::Bool=false,
                          compose::Bool=true, decompose::Bool=false, stripignore::Bool=false,
                          rejectna::Bool=false, newline2ls::Bool=false, newline2ps::Bool=false,
                          newline2lf::Bool=false, stripcc::Bool=false, casefold::Bool=false,
                          lump::Bool=false, stripmark::Bool=false)
    flags = 0
    stable && (flags = flags | STABLE)
    compat && (flags = flags | COMPAT)
    if decompose
        flags = flags | DECOMPOSE
    elseif compose
        flags = flags | COMPOSE
    elseif compat || stripmark
        throw(ArgumentError("compat=true or stripmark=true require compose=true or decompose=true"))
    end
    stripignore && (flags = flags | IGNORE)
    rejectna    && (flags = flags | REJECTNA)
    newline2ls + newline2ps + newline2lf > 1 &&
	throw(ArgumentError("only one newline conversion may be specified"))
    newline2ls  && (flags = flags | NLF2LS)
    newline2ps  && (flags = flags | NLF2PS)
    newline2lf  && (flags = flags | NLF2LF)
    stripcc     && (flags = flags | STRIPCC)
    casefold    && (flags = flags | CASEFOLD)
    lump        && (flags = flags | LUMP)
    stripmark   && (flags = flags | STRIPMARK)
    utf8proc_map(s, flags)
end

normalize_string(s::AbstractString, nf::Symbol) =
    utf8proc_map(s, nf == :NFC ? (STABLE | COMPOSE) :
                    nf == :NFD ? (STABLE | DECOMPOSE) :
                    nf == :NFKC ? (STABLE | COMPOSE | COMPAT) :
                    nf == :NFKD ? (STABLE | DECOMPOSE | COMPAT) :
                    throw(ArgumentError(":$nf is not one of :NFC, :NFD, :NFKC, :NFKD")))

############################################################################

charwidth(c::Char) = Int(ccall(:utf8proc_charwidth, Cint, (UInt32,), c))

function lowercase(c::Char)
    if isascii(c)
        'A' <= c <= 'Z' ? c + 0x20 : c
    else
        Char(ccall(:utf8proc_tolower, UInt32, (UInt32,), c))
    end
end

function uppercase(c::Char)
    if isascii(c)
        'a' <= c <= 'z' ? c - 0x20 : c
    else
        Char(ccall(:utf8proc_toupper, UInt32, (UInt32,), c))
    end
end

############################################################################

# returns Category.Code (values 0:29) giving Unicode category
charprop(::Type{Category.Code}, c) = Category.Code(ccall(:utf8proc_category, Cint, (UInt32,), c))

############################################################################

# iterators for grapheme segmentation

isgraphemebreak(c1::Char, c2::Char) =
    ccall(:utf8proc_grapheme_break, Bool, (UInt32, UInt32), c1, c2)

immutable GraphemeIterator{S<:AbstractString}
    s::S # original string (for generation of SubStrings)
end
graphemes(s::AbstractString) = GraphemeIterator{typeof(s)}(s)

Base.eltype{S}(::Type{GraphemeIterator{S}}) = SubString{S}

function length(g::GraphemeIterator)
    c0 = Char(0x00ad) # soft hyphen (grapheme break always allowed after this)
    n = 0
    for c in g.s
        n += isgraphemebreak(c0, c)
        c0 = c
    end
    return n
end

Base.start(g::GraphemeIterator) = start(g.s)
Base.done(g::GraphemeIterator, i) = done(g.s, i)

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
Base.hash(g::GraphemeIterator, h::UInt) = hash(g.s, h)
isless(g1::GraphemeIterator, g2::GraphemeIterator) = isless(g1.s, g2.s)

convert{S<:AbstractString}(::Type{S}, g::GraphemeIterator) = convert(S, g.s)

Base.show{S}(io::IO, g::GraphemeIterator{S}) =
    print(io, "length-$(length(g)) GraphemeIterator{$S} for \"$(g.s)\"")

############################################################################
