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
    return Base.is_id_char(c)
end

function is_identifier_start_char(c::Char)
    c == EOF_CHAR && return false
    return Base.is_id_start_char(c)
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
    c == 0x00002026 ||
    c == 0x0000205d ||
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
    0x000022d5 <= c <= 0x000022ff ||
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

function dotop2(pc, dpc)
    dotop1(pc) ||
    pc =='+' ||
    pc =='-' ||
    pc =='*' ||
    pc =='/' ||
    pc =='\\' ||
    pc =='^' ||
    pc =='<' ||
    pc =='>' ||
    pc =='&' && dpc === '=' ||
    pc =='&' ||
    pc =='%' ||
    pc == '=' && dpc != '>' ||
    pc == '|' && dpc != '|' ||
    pc == '!' && dpc == '=' ||
    pc == '⊻' ||
    pc == '÷' ||
    pc == '=' && dpc == '>'
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
    c == 0x00000302 ||
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

function is_operator_start_char(c::Char)
    eof(c) && return false
    is_operator_start_char(UInt32(c))
end
is_operator_start_char(u::UInt32) = u == 0x00000021 || (u == 0x00000024 || (u == 0x00000025 || (u == 0x00000026 || (u == 0x00000027 || (u == 0x0000002a || (u == 0x0000002b || (u == 0x0000002d || (u == 0x0000002e || (u == 0x0000002f || (u == 0x0000003a || (u == 0x0000003c || (u == 0x0000003d || (u == 0x0000003e || (u == 0x0000003f || (u == 0x0000005c || (u == 0x0000005e || (u == 0x00000069 || (u == 0x00000077 || (u == 0x0000007c || (u == 0x0000007e || (u == 0x000000ac || (u == 0x000000b1 || (u == 0x000000d7 || (u == 0x000000f7 || (u == 0x00002026 || (u == 0x0000205d || (u == 0x0000214b || (u == 0x00002190 || (u == 0x00002191 || (u == 0x00002192 || (u == 0x00002193 || (u == 0x00002194 || (u == 0x0000219a || (u == 0x0000219b || (u == 0x000021a0 || (u == 0x000021a3 || (u == 0x000021a6 || (u == 0x000021ae || (u == 0x000021ce || (u == 0x000021cf || (u == 0x000021d2 || (u == 0x000021d4 || (u == 0x000021f4 || (u == 0x000021f5 || (u == 0x000021f6 || (u == 0x000021f7 || (u == 0x000021f8 || (u == 0x000021f9 || (u == 0x000021fa || (u == 0x000021fb || (u == 0x000021fc || (u == 0x000021fd || (u == 0x000021fe || (u == 0x000021ff || (u == 0x00002208 || (u == 0x00002209 || (u == 0x0000220a || (u == 0x0000220b || (u == 0x0000220c || (u == 0x0000220d || (u == 0x00002213 || (u == 0x00002214 || (u == 0x00002217 || (u == 0x00002218 || (u == 0x00002219 || (u == 0x0000221a || (u == 0x0000221b || (u == 0x0000221c || (u == 0x0000221d || (u == 0x00002224 || (u == 0x00002225 || (u == 0x00002226 || (u == 0x00002227 || (u == 0x00002228 || (u == 0x00002229 || (u == 0x0000222a || (u == 0x00002237 || (u == 0x00002238 || (u == 0x0000223a || (u == 0x0000223b || (u == 0x0000223d || (u == 0x0000223e || (u == 0x00002240 || (u == 0x00002241 || (u == 0x00002242 || (u == 0x00002243 || (u == 0x00002244 || (u == 0x00002245 || (u == 0x00002246 || (u == 0x00002247 || (u == 0x00002248 || (u == 0x00002249 || (u == 0x0000224a || (u == 0x0000224b || (u == 0x0000224c || (u == 0x0000224d || (u == 0x0000224e || (u == 0x0000224f || (u == 0x00002250 || (u == 0x00002251 || (u == 0x00002252 || (u == 0x00002253 || (u == 0x00002254 || (u == 0x00002255 || (u == 0x00002256 || (u == 0x00002257 || (u == 0x00002258 || (u == 0x00002259 || (u == 0x0000225a || (u == 0x0000225b || (u == 0x0000225c || (u == 0x0000225d || (u == 0x0000225e || (u == 0x0000225f || (u == 0x00002260 || (u == 0x00002261 || (u == 0x00002262 || (u == 0x00002263 || (u == 0x00002264 || (u == 0x00002265 || (u == 0x00002266 || (u == 0x00002267 || (u == 0x00002268 || (u == 0x00002269 || (u == 0x0000226a || (u == 0x0000226b || (u == 0x0000226c || (u == 0x0000226d || (u == 0x0000226e || (u == 0x0000226f || (u == 0x00002270 || (u == 0x00002271 || (u == 0x00002272 || (u == 0x00002273 || (u == 0x00002274 || (u == 0x00002275 || (u == 0x00002276 || (u == 0x00002277 || (u == 0x00002278 || (u == 0x00002279 || (u == 0x0000227a || (u == 0x0000227b || (u == 0x0000227c || (u == 0x0000227d || (u == 0x0000227e || (u == 0x0000227f || (u == 0x00002280 || (u == 0x00002281 || (u == 0x00002282 || (u == 0x00002283 || (u == 0x00002284 || (u == 0x00002285 || (u == 0x00002286 || (u == 0x00002287 || (u == 0x00002288 || (u == 0x00002289 || (u == 0x0000228a || (u == 0x0000228b || (u == 0x0000228d || (u == 0x0000228e || (u == 0x0000228f || (u == 0x00002290 || (u == 0x00002291 || (u == 0x00002292 || (u == 0x00002293 || (u == 0x00002294 || (u == 0x00002295 || (u == 0x00002296 || (u == 0x00002297 || (u == 0x00002298 || (u == 0x00002299 || (u == 0x0000229a || (u == 0x0000229b || (u == 0x0000229c || (u == 0x0000229e || (u == 0x0000229f || (u == 0x000022a0 || (u == 0x000022a1 || (u == 0x000022a2 || (u == 0x000022a3 || (u == 0x000022a9 || (u == 0x000022ac || (u == 0x000022ae || (u == 0x000022b0 || (u == 0x000022b1 || (u == 0x000022b2 || (u == 0x000022b3 || (u == 0x000022b4 || (u == 0x000022b5 || (u == 0x000022b6 || (u == 0x000022b7 || (u == 0x000022bb || (u == 0x000022bc || (u == 0x000022bd || (u == 0x000022c4 || (u == 0x000022c5 || (u == 0x000022c6 || (u == 0x000022c7 || (u == 0x000022c9 || (u == 0x000022ca || (u == 0x000022cb || (u == 0x000022cc || (u == 0x000022cd || (u == 0x000022ce || (u == 0x000022cf || (u == 0x000022d0 || (u == 0x000022d1 || (u == 0x000022d2 || (u == 0x000022d3 || (u == 0x000022d5 || (u == 0x000022d6 || (u == 0x000022d7 || (u == 0x000022d8 || (u == 0x000022d9 || (u == 0x000022da || (u == 0x000022db || (u == 0x000022dc || (u == 0x000022dd || (u == 0x000022de || (u == 0x000022df || (u == 0x000022e0 || (u == 0x000022e1 || (u == 0x000022e2 || (u == 0x000022e3 || (u == 0x000022e4 || (u == 0x000022e5 || (u == 0x000022e6 || (u == 0x000022e7 || (u == 0x000022e8 || (u == 0x000022e9 || (u == 0x000022ea || (u == 0x000022eb || (u == 0x000022ec || (u == 0x000022ed || (u == 0x000022ee || (u == 0x000022ef || (u == 0x000022f0 || (u == 0x000022f1 || (u == 0x000022f2 || (u == 0x000022f3 || (u == 0x000022f4 || (u == 0x000022f5 || (u == 0x000022f6 || (u == 0x000022f7 || (u == 0x000022f8 || (u == 0x000022f9 || (u == 0x000022fa || (u == 0x000022fb || (u == 0x000022fc || (u == 0x000022fd || (u == 0x000022fe || (u == 0x000022ff || (u == 0x000025b7 || (u == 0x000027c2 || (u == 0x000027c8 || (u == 0x000027c9 || (u == 0x000027d1 || (u == 0x000027d2 || (u == 0x000027d5 || (u == 0x000027d6 || (u == 0x000027d7 || (u == 0x000027f0 || (u == 0x000027f1 || (u == 0x000027f5 || (u == 0x000027f6 || (u == 0x000027f7 || (u == 0x000027f9 || (u == 0x000027fa || (u == 0x000027fb || (u == 0x000027fc || (u == 0x000027fd || (u == 0x000027fe || (u == 0x000027ff || (u == 0x00002900 || (u == 0x00002901 || (u == 0x00002902 || (u == 0x00002903 || (u == 0x00002904 || (u == 0x00002905 || (u == 0x00002906 || (u == 0x00002907 || (u == 0x00002908 || (u == 0x00002909 || (u == 0x0000290a || (u == 0x0000290b || (u == 0x0000290c || (u == 0x0000290d || (u == 0x0000290e || (u == 0x0000290f || (u == 0x00002910 || (u == 0x00002911 || (u == 0x00002912 || (u == 0x00002913 || (u == 0x00002914 || (u == 0x00002915 || (u == 0x00002916 || (u == 0x00002917 || (u == 0x00002918 || (u == 0x0000291d || (u == 0x0000291e || (u == 0x0000291f || (u == 0x00002920 || (u == 0x00002944 || (u == 0x00002945 || (u == 0x00002946 || (u == 0x00002947 || (u == 0x00002948 || (u == 0x00002949 || (u == 0x0000294a || (u == 0x0000294b || (u == 0x0000294c || (u == 0x0000294d || (u == 0x0000294e || (u == 0x0000294f || (u == 0x00002950 || (u == 0x00002951 || (u == 0x00002952 || (u == 0x00002953 || (u == 0x00002954 || (u == 0x00002955 || (u == 0x00002956 || (u == 0x00002957 || (u == 0x00002958 || (u == 0x00002959 || (u == 0x0000295a || (u == 0x0000295b || (u == 0x0000295c || (u == 0x0000295d || (u == 0x0000295e || (u == 0x0000295f || (u == 0x00002960 || (u == 0x00002961 || (u == 0x00002962 || (u == 0x00002963 || (u == 0x00002964 || (u == 0x00002965 || (u == 0x00002966 || (u == 0x00002967 || (u == 0x00002968 || (u == 0x00002969 || (u == 0x0000296a || (u == 0x0000296b || (u == 0x0000296c || (u == 0x0000296d || (u == 0x0000296e || (u == 0x0000296f || (u == 0x00002970 || (u == 0x000029b7 || (u == 0x000029b8 || (u == 0x000029bc || (u == 0x000029be || (u == 0x000029bf || (u == 0x000029c0 || (u == 0x000029c1 || (u == 0x000029e1 || (u == 0x000029e3 || (u == 0x000029e4 || (u == 0x000029e5 || (u == 0x000029f4 || (u == 0x000029f6 || (u == 0x000029f7 || (u == 0x000029fa || (u == 0x000029fb || (u == 0x00002a07 || (u == 0x00002a08 || (u == 0x00002a1d || (u == 0x00002a22 || (u == 0x00002a23 || (u == 0x00002a24 || (u == 0x00002a25 || (u == 0x00002a26 || (u == 0x00002a27 || (u == 0x00002a28 || (u == 0x00002a29 || (u == 0x00002a2a || (u == 0x00002a2b || (u == 0x00002a2c || (u == 0x00002a2d || (u == 0x00002a2e || (u == 0x00002a30 || (u == 0x00002a31 || (u == 0x00002a32 || (u == 0x00002a33 || (u == 0x00002a34 || (u == 0x00002a35 || (u == 0x00002a36 || (u == 0x00002a37 || (u == 0x00002a38 || (u == 0x00002a39 || (u == 0x00002a3a || (u == 0x00002a3b || (u == 0x00002a3c || (u == 0x00002a3d || (u == 0x00002a40 || (u == 0x00002a41 || (u == 0x00002a42 || (u == 0x00002a43 || (u == 0x00002a44 || (u == 0x00002a45 || (u == 0x00002a4a || (u == 0x00002a4b || (u == 0x00002a4c || (u == 0x00002a4d || (u == 0x00002a4e || (u == 0x00002a4f || (u == 0x00002a50 || (u == 0x00002a51 || (u == 0x00002a52 || (u == 0x00002a53 || (u == 0x00002a54 || (u == 0x00002a55 || (u == 0x00002a56 || (u == 0x00002a57 || (u == 0x00002a58 || (u == 0x00002a5a || (u == 0x00002a5b || (u == 0x00002a5c || (u == 0x00002a5d || (u == 0x00002a5e || (u == 0x00002a5f || (u == 0x00002a60 || (u == 0x00002a61 || (u == 0x00002a62 || (u == 0x00002a63 || (u == 0x00002a66 || (u == 0x00002a67 || (u == 0x00002a6a || (u == 0x00002a6b || (u == 0x00002a6c || (u == 0x00002a6d || (u == 0x00002a6e || (u == 0x00002a6f || (u == 0x00002a70 || (u == 0x00002a71 || (u == 0x00002a72 || (u == 0x00002a73 || (u == 0x00002a74 || (u == 0x00002a75 || (u == 0x00002a76 || (u == 0x00002a77 || (u == 0x00002a78 || (u == 0x00002a79 || (u == 0x00002a7a || (u == 0x00002a7b || (u == 0x00002a7c || (u == 0x00002a7d || (u == 0x00002a7e || (u == 0x00002a7f || (u == 0x00002a80 || (u == 0x00002a81 || (u == 0x00002a82 || (u == 0x00002a83 || (u == 0x00002a84 || (u == 0x00002a85 || (u == 0x00002a86 || (u == 0x00002a87 || (u == 0x00002a88 || (u == 0x00002a89 || (u == 0x00002a8a || (u == 0x00002a8b || (u == 0x00002a8c || (u == 0x00002a8d || (u == 0x00002a8e || (u == 0x00002a8f || (u == 0x00002a90 || (u == 0x00002a91 || (u == 0x00002a92 || (u == 0x00002a93 || (u == 0x00002a94 || (u == 0x00002a95 || (u == 0x00002a96 || (u == 0x00002a97 || (u == 0x00002a98 || (u == 0x00002a99 || (u == 0x00002a9a || (u == 0x00002a9b || (u == 0x00002a9c || (u == 0x00002a9d || (u == 0x00002a9e || (u == 0x00002a9f || (u == 0x00002aa0 || (u == 0x00002aa1 || (u == 0x00002aa2 || (u == 0x00002aa3 || (u == 0x00002aa4 || (u == 0x00002aa5 || (u == 0x00002aa6 || (u == 0x00002aa7 || (u == 0x00002aa8 || (u == 0x00002aa9 || (u == 0x00002aaa || (u == 0x00002aab || (u == 0x00002aac || (u == 0x00002aad || (u == 0x00002aae || (u == 0x00002aaf || (u == 0x00002ab0 || (u == 0x00002ab1 || (u == 0x00002ab2 || (u == 0x00002ab3 || (u == 0x00002ab4 || (u == 0x00002ab5 || (u == 0x00002ab6 || (u == 0x00002ab7 || (u == 0x00002ab8 || (u == 0x00002ab9 || (u == 0x00002aba || (u == 0x00002abb || (u == 0x00002abc || (u == 0x00002abd || (u == 0x00002abe || (u == 0x00002abf || (u == 0x00002ac0 || (u == 0x00002ac1 || (u == 0x00002ac2 || (u == 0x00002ac3 || (u == 0x00002ac4 || (u == 0x00002ac5 || (u == 0x00002ac6 || (u == 0x00002ac7 || (u == 0x00002ac8 || (u == 0x00002ac9 || (u == 0x00002aca || (u == 0x00002acb || (u == 0x00002acc || (u == 0x00002acd || (u == 0x00002ace || (u == 0x00002acf || (u == 0x00002ad0 || (u == 0x00002ad1 || (u == 0x00002ad2 || (u == 0x00002ad3 || (u == 0x00002ad4 || (u == 0x00002ad5 || (u == 0x00002ad6 || (u == 0x00002ad7 || (u == 0x00002ad8 || (u == 0x00002ad9 || (u == 0x00002adb || (u == 0x00002af7 || (u == 0x00002af8 || (u == 0x00002af9 || (u == 0x00002afa || (u == 0x00002b30 || (u == 0x00002b31 || (u == 0x00002b32 || (u == 0x00002b33 || (u == 0x00002b34 || (u == 0x00002b35 || (u == 0x00002b36 || (u == 0x00002b37 || (u == 0x00002b38 || (u == 0x00002b39 || (u == 0x00002b3a || (u == 0x00002b3b || (u == 0x00002b3c || (u == 0x00002b3d || (u == 0x00002b3e || (u == 0x00002b3f || (u == 0x00002b40 || (u == 0x00002b41 || (u == 0x00002b42 || (u == 0x00002b43 || (u == 0x00002b44 || (u == 0x00002b47 || (u == 0x00002b48 || (u == 0x00002b49 || (u == 0x00002b4a || (u == 0x00002b4b || (u == 0x00002b4c || (u == 0x0000ffe9 || (u == 0x0000ffea || (u == 0x0000ffeb || u == 0x0000ffec)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

