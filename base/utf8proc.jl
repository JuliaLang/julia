# Various Unicode functionality from the utf8proc library
module UTF8proc

import Base: show, showcompact, ==, string, symbol, isless

# also exported by Base:
export normalize_string, is_valid_char, is_assigned_char

# whether codepoints are valid Unicode
is_valid_char(c) = bool(ccall(:utf8proc_codepoint_valid, Cchar, (Int32,), c))

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
                       bytestring(s), 0, p, flags | UTF8PROC_NULLTERM)
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
    
# returns UTF8PROC_CATEGORY code in 0..30 giving Unicode category
function category_code(c)
    # note: utf8proc returns 0, not UTF8PROC_CATEGORY_CN, for unassigned c
    c > 0x10FFFF && return 0x0000 # see utf8proc_get_property docs
    unsafe_load(ccall(:utf8proc_get_property, Ptr{Uint16}, (Int32,), c))
end

is_assigned_char(c) = category_code(c) != 0

# TODO: use UTF8PROC_CHARBOUND to extract graphemes from a string, e.g. to iterate over graphemes?

end # module
