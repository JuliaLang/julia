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

import Base.UTF8proc

const EOF_CHAR = convert(Char,typemax(UInt32))

function is_cat_id_start(ch::Char, cat::Integer)
    c = UInt32(ch)
    return (cat == UTF8proc.UTF8PROC_CATEGORY_LU || cat == UTF8proc.UTF8PROC_CATEGORY_LL ||
            cat == UTF8proc.UTF8PROC_CATEGORY_LT || cat == UTF8proc.UTF8PROC_CATEGORY_LM ||
            cat == UTF8proc.UTF8PROC_CATEGORY_LO || cat == UTF8proc.UTF8PROC_CATEGORY_NL ||
            cat == UTF8proc.UTF8PROC_CATEGORY_SC ||  # allow currency symbols
            cat == UTF8proc.UTF8PROC_CATEGORY_SO ||  # other symbols

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
                (c >= 0x27c0 && c <= 0x27c2) ||  # ⟀, ⟁, ⟂
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
    if ((c >= 'A' && c <= 'Z') ||
        (c >= 'a' && c <= 'z') || c == '_' ||
        (c >= '0' && c <= '9') || c == '!')
        return true
    elseif (UInt32(c) < 0xA1 || UInt32(c) > 0x10ffff)
        return false
    end
    cat = UTF8proc.category_code(c)
    is_cat_id_start(c, cat) && return true
    if cat == UTF8proc.UTF8PROC_CATEGORY_MN || cat == UTF8proc.UTF8PROC_CATEGORY_MC ||
       cat == UTF8proc.UTF8PROC_CATEGORY_ND || cat == UTF8proc.UTF8PROC_CATEGORY_PC ||
       cat == UTF8proc.UTF8PROC_CATEGORY_SK || cat == UTF8proc.UTF8PROC_CATEGORY_ME ||
       cat == UTF8proc.UTF8PROC_CATEGORY_NO ||
       (0x2032 <= UInt32(c) <= 0x2034) || # primes
       UInt32(c) == 0x0387 || UInt32(c) == 0x19da ||
       (0x1369 <= UInt32(c) <= 0x1371)
       return true
    end
    return false
end

function is_identifier_start_char(c::Char)
    if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_')
        return true
    elseif (UInt32(c) < 0xA1 || UInt32(c) > 0x10ffff)
        return false
    end
    cat = UTF8proc.category_code(c)
    return is_cat_id_start(c, cat)
end


function peekchar(io::IOBuffer)
    if !io.readable || io.ptr > io.size
        return EOF_CHAR
    end
    ch = convert(UInt8,io.data[io.ptr])
    if ch < 0x80
        return convert(Char,ch)
    end
    # mimic utf8.next function
    trailing = Base.utf8_trailing[ch+1]
    c::UInt32 = 0
    for j = 1:trailing
        c += ch
        c <<= 6
        ch = convert(UInt8,io.data[io.ptr+j])
    end
    c += ch
    c -= Base.utf8_offset[trailing+1]
    return convert(Char,c)
end

# this implementation is copied from Base
const _CHTMP = Array(Char, 1)

peekchar(s::IOStream) = begin
    if ccall(:ios_peekutf8, Int32, (Ptr{Void}, Ptr{Char}), s, _CHTMP) < 0
        return EOF_CHAR
    end
    return _CHTMP[1]
end

eof(io::IO) = Base.eof(io)
eof(c) = is(c, EOF_CHAR)

readchar(io::IO) = eof(io) ? EOF_CHAR : read(io, Char)
takechar(io::IO) = (readchar(io); io)
