# Various Unicode functionality from the utf8proc library
module UTF8proc

import Base: show, showcompact, ==, string, symbol, isless

# also exported by Base:
export normalize_string, is_valid_char, is_assigned_char,
   islower, isupper, isalpha, isdigit, isnumber, isalnum, 
   iscntrl, ispunct, isspace, isprint, isgraph, isblank


# whether codepoints are valid Unicode
is_valid_char(c) = bool(ccall(:utf8proc_codepoint_valid, Cchar, (Int32,), c))

# utf8 category constants
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
const UTF8PROC_CATEGORY_CN = 30

const UTF8PROC_NULLTERM  = (1<<0)
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

let
    const p = Array(Ptr{Uint8}, 1)
    global utf8proc_map
    function utf8proc_map(s::String, flags::Integer)
        result = ccall(:utf8proc_map, Cssize_t,
                       (Ptr{Uint8}, Cssize_t, Ptr{Ptr{Uint8}}, Cint),
                       s, 0, p, flags | UTF8PROC_NULLTERM)
        result < 0 && error(bytestring(ccall(:utf8proc_errmsg, Ptr{Uint8},
                                             (Cssize_t,), result)))
        a = ccall(:jl_ptr_to_array_1d, Vector{Uint8}, 
                  (Any, Ptr{Uint8}, Csize_t, Cint),
                  Vector{Uint8}, p[1], result, true)
        ccall(:jl_array_to_string, Any, (Any,), a)::ByteString
    end
end

function normalize_string(s::String; stable::Bool=false, compat::Bool=false, compose::Bool=true, decompose::Bool=false, stripignore::Bool=false, rejectna::Bool=false, newline2ls::Bool=false, newline2ps::Bool=false, newline2lf::Bool=false, stripcc::Bool=false, casefold::Bool=false, lump::Bool=false, stripmark::Bool=false)
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

function normalize_string(s::String, nf::Symbol)
    utf8proc_map(s, nf == :NFC ? (UTF8PROC_STABLE | UTF8PROC_COMPOSE) :
                    nf == :NFD ? (UTF8PROC_STABLE | UTF8PROC_DECOMPOSE) :
                    nf == :NFKC ? (UTF8PROC_STABLE | UTF8PROC_COMPOSE
                                   | UTF8PROC_COMPAT) :
                    nf == :NFKD ? (UTF8PROC_STABLE | UTF8PROC_DECOMPOSE
                                   | UTF8PROC_COMPAT) :
                    throw(ArgumentError(":$nf is not one of :NFC, :NFD, :NFKC, :NFKD")))
end
    
# returns UTF8PROC_CATEGORY code in 1:30 giving Unicode category
function category_code(c)
    c > 0x10FFFF && return 0x0000 # see utf8proc_get_property docs
    cat = unsafe_load(ccall(:utf8proc_get_property, Ptr{Uint16}, (Int32,), c))
    # note: utf8proc returns 0, not UTF8PROC_CATEGORY_CN, for unassigned c
    cat == 0 ? UTF8PROC_CATEGORY_CN : cat
end

# category_code() modified to ignore case of unassigned category CN
#  used by character class predicates for improved performance
function _catcode(c)
    c > 0x10FFFF && return 0x0000 # see utf8proc_get_property docs
    cat = unsafe_load(ccall(:utf8proc_get_property, Ptr{Uint16}, (Int32,), c))
end

is_assigned_char(c) = category_code(c) != UTF8PROC_CATEGORY_CN

# TODO: use UTF8PROC_CHARBOUND to extract graphemes from a string, e.g. to iterate over graphemes?

## libc character class predicates ##

islower(c::Char) = (_catcode(c)==UTF8PROC_CATEGORY_LL)

function isupper(c::Char)
    ccode=_catcode(c)
    return ccode==UTF8PROC_CATEGORY_LU || ccode==UTF8PROC_CATEGORY_LT
end

isalpha(c::Char) = (UTF8PROC_CATEGORY_LU <= _catcode(c) <=
                                            UTF8PROC_CATEGORY_LO)

isdigit(c::Char) = ('0' <= c <= '9')

isnumber(c::Char) = (UTF8PROC_CATEGORY_ND <= _catcode(c) <=
                                            UTF8PROC_CATEGORY_NO)

function isalnum(c::Char)
    ccode=_catcode(c)
    return (UTF8PROC_CATEGORY_LU <= ccode <= UTF8PROC_CATEGORY_LO) ||
                    (UTF8PROC_CATEGORY_ND <= ccode <= UTF8PROC_CATEGORY_NO)
end

iscntrl(c::Char) = (uint(c)<= 0x1f || 0x7f<=uint(c)<=0x9f)

ispunct(c::Char) = (UTF8PROC_CATEGORY_PC <=_catcode(c) <= UTF8PROC_CATEGORY_PO)

isspace(c::Char) = c==' ' || '\t'<=c<='\r' || c==0x85 || _catcode(c)==UTF8PROC_CATEGORY_ZS
                               
isprint(c::Char) = (UTF8PROC_CATEGORY_LU <= _catcode(c) <= UTF8PROC_CATEGORY_ZS)
isgraph(c::Char) = (UTF8PROC_CATEGORY_LU <= _catcode(c) <= UTF8PROC_CATEGORY_SO)


for name = ("alnum", "alpha", "cntrl", "digit", "graph",
                     "lower", "number","print", "punct", "space", "upper")
    f = symbol(string("is",name))
    @eval $f(s::String) = all($f, s)
end

isblank(c::Char) = c==' ' || c=='\t'
isblank(s::String) = all(isblank, s)


end # module
