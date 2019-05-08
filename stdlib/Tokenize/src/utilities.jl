#=
The code in here has been extracted from the JuliaParser.jl package
with license:

The JuliaParser.jl package is licensed under the MIT "Expat" License:

> Copyright (c) 2014: Jake Bolewski.
>
> Permission is hereby granted, free of charge, to any person obtaining
> a copy of this software and associated documentation files (the
> "Software"), to deal in the Software without restriction, including
> without limitation the rights to use, copy, modify, merge, publish,
> distribute, sublicense, and/or sell copies of the Software, and to
> permit persons to whom the Software is furnished to do so, subject to
> the following conditions:
>
> The above copyright notice and this permission notice shall be
> included in all copies or substantial portions of the Software.
>
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
> EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
> MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
> IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
> CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
> TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
> SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
=#

import Base.Unicode


@inline function utf8_trailing(i)
    if i < 193
        return 0
    elseif i < 225
        return 1
    elseif i < 241
        return 2
    elseif i < 249
        return 3
    elseif i < 253
        return 4
    else
        return 5
    end
end

const utf8_offset = [0x00000000
                    0x00003080
                    0x000e2080
                    0x03c82080
                    0xfa082080
                    0x82082080]
# const EOF_CHAR = convert(Char,typemax(UInt32))
const EOF_CHAR = typemax(Char)


function is_cat_id_start(ch::Char, cat::Integer)
    c = UInt32(ch)
    return (cat == Unicode.UTF8PROC_CATEGORY_LU || cat == Unicode.UTF8PROC_CATEGORY_LL ||
            cat == Unicode.UTF8PROC_CATEGORY_LT || cat == Unicode.UTF8PROC_CATEGORY_LM ||
            cat == Unicode.UTF8PROC_CATEGORY_LO || cat == Unicode.UTF8PROC_CATEGORY_NL ||
            cat == Unicode.UTF8PROC_CATEGORY_SC ||  # allow currency symbols
            cat == Unicode.UTF8PROC_CATEGORY_SO ||  # other symbols

            # math symbol (category Sm) whitelist
            (c >= 0x2140 && c <= 0x2a1c &&
             ((c >= 0x2140 && c <= 0x2144) || # ⅀, ⅁, ⅂, ⅃, ⅄
              c == 0x223f || c == 0x22be || c == 0x22bf || # ∿, ⊾, ⊿
              c == 0x22a4 || c == 0x22a5 || # ⊤ ⊥
              (c >= 0x22ee && c <= 0x22f1) || # ⋮, ⋯, ⋰, ⋱

              (c >= 0x2202 && c <= 0x2233 &&
               (c == 0x2202 || c == 0x2205 || c == 0x2206 || # ∂, ∅, ∆
                c == 0x2207 || c == 0x220e || c == 0x220f || # ∇, ∎, ∏
                c == 0x2210 || c == 0x2211 || # ∐, ∑
                c == 0x221e || c == 0x221f || # ∞, ∟
                c >= 0x222b)) || # ∫, ∬, ∭, ∮, ∯, ∰, ∱, ∲, ∳

              (c >= 0x22c0 && c <= 0x22c3) ||  # N-ary big ops: ⋀, ⋁, ⋂, ⋃
              (c >= 0x25F8 && c <= 0x25ff) ||  # ◸, ◹, ◺, ◻, ◼, ◽, ◾, ◿

              (c >= 0x266f &&
               (c == 0x266f || c == 0x27d8 || c == 0x27d9 || # ♯, ⟘, ⟙
                (c >= 0x27c0 && c <= 0x27c1) ||  # ⟀, ⟁
                (c >= 0x29b0 && c <= 0x29b4) ||  # ⦰, ⦱, ⦲, ⦳, ⦴
                (c >= 0x2a00 && c <= 0x2a06) ||  # ⨀, ⨁, ⨂, ⨃, ⨄, ⨅, ⨆
                (c >= 0x2a09 && c <= 0x2a16) ||  # ⨉, ⨊, ⨋, ⨌, ⨍, ⨎, ⨏, ⨐, ⨑, ⨒,
                                                 # ⨓, ⨔, ⨕, ⨖
                c == 0x2a1b || c == 0x2a1c)))) || # ⨛, ⨜

            (c >= 0x1d6c1 && # variants of \nabla and \partial
             (c == 0x1d6c1 || c == 0x1d6db ||
              c == 0x1d6fb || c == 0x1d715 ||
              c == 0x1d735 || c == 0x1d74f ||
              c == 0x1d76f || c == 0x1d789 ||
              c == 0x1d7a9 || c == 0x1d7c3)) ||

            # super- and subscript +-=()
            (c >= 0x207a && c <= 0x207e) ||
            (c >= 0x208a && c <= 0x208e) ||

            # angle symbols
            (c >= 0x2220 && c <= 0x2222) || # ∠, ∡, ∢
            (c >= 0x299b && c <= 0x29af) || # ⦛, ⦜, ⦝, ⦞, ⦟, ⦠, ⦡, ⦢, ⦣, ⦤, ⦥,
                                            # ⦦, ⦧, ⦨, ⦩, ⦪, ⦫, ⦬, ⦭, ⦮, ⦯
            # Other_ID_Start
            c == 0x2118 || c == 0x212E || # ℘, ℮
            (c >= 0x309B && c <= 0x309C)) # katakana-hiragana sound marks
end

function is_identifier_char(c::Char)
    c == EOF_CHAR && return false
    if ((c >= 'A' && c <= 'Z') ||
        (c >= 'a' && c <= 'z') || c == '_' ||
        (c >= '0' && c <= '9') || c == '!')
        return true
    elseif (UInt32(c) < 0xA1 || UInt32(c) > 0x10ffff)
        return false
    end
    cat = Unicode.category_code(c)
    is_cat_id_start(c, cat) && return true
    if cat == Unicode.UTF8PROC_CATEGORY_MN || cat == Unicode.UTF8PROC_CATEGORY_MC ||
        cat == Unicode.UTF8PROC_CATEGORY_ND || cat == Unicode.UTF8PROC_CATEGORY_PC ||
        cat == Unicode.UTF8PROC_CATEGORY_SK || cat == Unicode.UTF8PROC_CATEGORY_ME ||
        cat == Unicode.UTF8PROC_CATEGORY_NO ||
        (0x2032 <= UInt32(c) <= 0x2034) || # primes
        UInt32(c) == 0x0387 || UInt32(c) == 0x19da ||
        (0x1369 <= UInt32(c) <= 0x1371)
       return true
    end
    return false
end

function is_identifier_start_char(c::Char)
    c == EOF_CHAR && return false
    if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_')
        return true
    elseif (UInt32(c) < 0xA1 || UInt32(c) > 0x10ffff)
        return false
    end
    cat = Unicode.category_code(c)
    return is_cat_id_start(c, cat)
end


function peekchar(io::Base.GenericIOBuffer)
    if !io.readable || io.ptr > io.size
        return EOF_CHAR
    end
    ch, _ = readutf(io)
    return ch
end

function readutf(io, offset = 0)
    ch = convert(UInt8, io.data[io.ptr + offset])
    if ch < 0x80
        return convert(Char, ch), 0
    end
    trailing = utf8_trailing(ch + 1)
    c::UInt32 = 0
    for j = 1:trailing
        c += ch
        c <<= 6
        ch = convert(UInt8, io.data[io.ptr + j + offset])
    end
    c += ch
    c -= utf8_offset[trailing + 1]
    return convert(Char, c), trailing
end

function dpeekchar(io::IOBuffer)
    if !io.readable || io.ptr > io.size
        return EOF_CHAR, EOF_CHAR
    end
    ch1, trailing = readutf(io)
    offset = trailing + 1

    if io.ptr + offset > io.size
        return ch1, EOF_CHAR
    end
    ch2, _ = readutf(io, offset)

    return ch1, ch2
end

# this implementation is copied from Base
peekchar(s::IOStream) = begin
    _CHTMP = Ref{Char}()
    if ccall(:ios_peekutf8, Int32, (Ptr{Nothing}, Ptr{Char}), s, _CHTMP) < 0
        return EOF_CHAR
    end
    return _CHTMP[]
end

eof(io::IO) = Base.eof(io)
eof(c::Char) = c === EOF_CHAR

readchar(io::IO) = eof(io) ? EOF_CHAR : read(io, Char)
takechar(io::IO) = (readchar(io); io)

# Checks whether a Char is an operator, which can not be juxtaposed with another
# Char to be an operator (i.e <=), and can be prefixed by a dot (.)
# magic number list created by filtering ops by those that successfully parse
# `a .(op) b` or `.(op)a` and where `length(string(op)) == 1`
@inline function dotop1(c1::Char)
    c1 == EOF_CHAR && return false
    c = UInt32(c1)
    c == 0x00000021 ||
    c == 0x0000002e ||
    c == 0x0000007e ||
    c == 0x000000ac ||
    c == 0x000000b1 ||
    c == 0x000000d7 ||
    c == 0x0000214b ||
    0x00002190 <= c <= 0x00002194 ||
    0x0000219a <= c <= 0x0000219b ||
    c == 0x000021a0 ||
    c == 0x000021a3 ||
    c == 0x000021a6 ||
    c == 0x000021ae ||
    0x000021ce <= c <= 0x000021cf ||
    c == 0x000021d2 ||
    c == 0x000021d4 ||
    0x000021f4 <= c <= 0x000021ff ||
    0x00002208 <= c <= 0x0000220d ||
    0x00002213 <= c <= 0x00002214 ||
    0x00002217 <= c <= 0x00002219 ||
    0x0000221a <= c <= 0x0000221d ||
    0x00002224 <= c <= 0x0000222a ||
    0x00002237 <= c <= 0x00002238 ||
    0x0000223a <= c <= 0x0000223b ||
    0x0000223d <= c <= 0x0000223e ||
    0x00002240 <= c <= 0x0000228b ||
    0x0000228d <= c <= 0x0000229c ||
    0x0000229e <= c <= 0x000022a3 ||
    c == 0x000022a9 ||
    c == 0x000022ac ||
    c == 0x000022ae ||
    0x000022b0 <= c <= 0x000022b7 ||
    0x000022bc <= c <= 0x000022bd ||
    0x000022c4 <= c <= 0x000022c7 ||
    0x000022c9 <= c <= 0x000022d3 ||
    0x000022d5 <= c <= 0x000022ed ||
    0x000022f2 <= c <= 0x000022ff ||
    c == 0x000025b7 ||
    c == 0x000027c2 ||
    0x000027c8 <= c <= 0x000027c9 ||
    0x000027d1 <= c <= 0x000027d2 ||
    0x000027d5 <= c <= 0x000027d7 ||
    0x000027f0 <= c <= 0x000027f1 ||
    0x000027f5 <= c <= 0x000027f7 ||
    0x000027f9 <= c <= 0x000027ff ||
    0x00002900 <= c <= 0x00002918 ||
    0x0000291d <= c <= 0x00002920 ||
    0x00002944 <= c <= 0x00002970 ||
    0x000029b7 <= c <= 0x000029b8 ||
    c == 0x000029bc ||
    0x000029be <= c <= 0x000029c1 ||
    c == 0x000029e1 ||
    0x000029e3 <= c <= 0x000029e5 ||
    c == 0x000029f4 ||
    0x000029f6 <= c <= 0x000029f7 ||
    0x000029fa <= c <= 0x000029fb ||
    0x00002a07 <= c <= 0x00002a08 ||
    c == 0x00002a1d ||
    0x00002a22 <= c <= 0x00002a2e ||
    0x00002a30 <= c <= 0x00002a3d ||
    0x00002a40 <= c <= 0x00002a45 ||
    0x00002a4a <= c <= 0x00002a58 ||
    0x00002a5a <= c <= 0x00002a63 ||
    0x00002a66 <= c <= 0x00002a67 ||
    0x00002a6a <= c <= 0x00002ad9 ||
    c == 0x00002adb ||
    0x00002af7 <= c <= 0x00002afa ||
    0x00002b30 <= c <= 0x00002b44 ||
    0x00002b47 <= c <= 0x00002b4c ||
    0x0000ffe9 <= c <= 0x0000ffec
end

# suffix operators
# "₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎²³¹ʰʲʳʷʸˡˢˣᴬᴮᴰᴱᴳᴴᴵᴶᴷᴸᴹᴺᴼᴾᴿᵀᵁᵂᵃᵇᵈᵉᵍᵏᵐᵒᵖᵗᵘᵛᵝᵞᵟᵠᵡᵢᵣᵤᵥᵦᵧᵨᵩᵪᶜᶠᶥᶦᶫᶰᶸᶻᶿ ⁰ⁱ⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾ⁿₐₑₒₓₕₖₗₘₙₚₛₜⱼⱽ′″‴‵‶‷⁗"
@inline function isopsuffix(c1::Char)
    c1 == EOF_CHAR && return false
    c = UInt32(c1)
    0x000000b2 <= c <= 0x000000b3 ||
    c == 0x000000b9 ||
    c == 0x000002b0 ||
    0x000002b2 <= c <= 0x000002b3 ||
    0x000002b7 <= c <= 0x000002b8 ||
    0x000002e1 <= c <= 0x000002e3 ||
    c == 0x00001d2c ||
    c == 0x00001d2e ||
    0x00001d30 <= c <= 0x00001d31 ||
    0x00001d33 <= c <= 0x00001d3a ||
    c == 0x00001d3c ||
    0x00001d3e <= c <= 0x00001d43 ||
    0x00001d47 <= c <= 0x00001d49 ||
    c == 0x00001d4d ||
    0x00001d4f <= c <= 0x00001d50 ||
    c == 0x00001d52 ||
    0x00001d56 <= c <= 0x00001d58 ||
    c == 0x00001d5b ||
    0x00001d5d <= c <= 0x00001d6a ||
    c == 0x00001d9c ||
    c == 0x00001da0 ||
    0x00001da5 <= c <= 0x00001da6 ||
    c == 0x00001dab ||
    c == 0x00001db0 ||
    c == 0x00001db8 ||
    c == 0x00001dbb ||
    c == 0x00001dbf ||
    c == 0x00002009 ||
    0x00002032 <= c <= 0x00002037 ||
    c == 0x00002057 ||
    0x00002070 <= c <= 0x00002071 ||
    0x00002074 <= c <= 0x0000208e ||
    0x00002090 <= c <= 0x00002093 ||
    0x00002095 <= c <= 0x0000209c ||
    0x00002c7c <= c <= 0x00002c7d
end


function optakessuffix(k)
    (Tokens.begin_ops < k < Tokens.end_ops) && 
    !(k == Tokens.DDDOT ||
    Tokens.EQ <= k <= k == Tokens.XOR_EQ ||
    k == Tokens.CONDITIONAL ||
    k == Tokens.RIGHT_ARROW ||
    k == Tokens.LAZY_OR ||
    k == Tokens.LAZY_AND ||
    k == Tokens.ISSUBTYPE ||
    k == Tokens.ISSUPERTYPE ||
    k == Tokens.IN ||
    k == Tokens.ISA ||
    k == Tokens.COLON_EQUALS ||
    k == Tokens.DOUBLE_COLON_EQUAL ||
    k == Tokens.COLON ||
    k == Tokens.DDOT ||
    k == Tokens.EX_OR ||
    k == Tokens.DECLARATION ||
    k == Tokens.WHERE ||
    k == Tokens.DOT ||
    k == Tokens.NOT ||
    k == Tokens.TRANSPOSE ||
    k == Tokens.ANON_FUNC ||
    Tokens.NOT_SIGN <= k <= Tokens.QUAD_ROOT
    ) 
end
