module Lexer

import ..UTF8proc

export Token, TokenStream, next_token, set_token!, last_token,
       put_back!, peek_token, take_token, require_token

const SYM_TRUE  = symbol("true")
const SYM_FALSE = symbol("false")
const SYM_CTRANSPOSE = symbol("'")

const EOF = typemax(Char)

const ops_by_precedent = Any[
       [:(=),   :(:=),  :(+=), :(-=),  :(*=),  :(/=),   :(//=),  :(.//=),
        :(.*=), :(./=), :(\=), :(.\=), :(^=),  :(.^=),  :(%=),   :(.%=),
        :(|=),  :(&=),  :($=), :(=>),  :(<<=), :(>>=),  :(>>>=), :(~),
        :(.+=), :(.-=)],
       [:(?)],
       [:(||)],
       [:(&&)],
       # NOTE: -- is deprecated in 0.4
       [symbol("--"), :(-->), :(←), :(→), :(↔), :(↚), :(↛), :(↠), :(↣), :(↦),
        :(↮), :(⇎), :(⇏), :(⇒), :(⇔), :(⇴), :(⇶), :(⇷), :(⇸), :(⇹),
        :(⇺), :(⇻), :(⇼), :(⇽), :(⇾), :(⇿), :(⟵), :(⟶), :(⟷), :(⟷),
        :(⟹), :(⟺), :(⟻), :(⟼), :(⟽), :(⟾), :(⟿), :(⤀), :(⤁), :(⤂),
        :(⤃), :(⤄), :(⤅), :(⤆), :(⤇), :(⤌), :(⤍), :(⤎), :(⤏), :(⤐),
        :(⤑), :(⤔), :(⤕), :(⤖), :(⤗), :(⤘), :(⤝), :(⤞), :(⤟), :(⤠),
        :(⥄), :(⥅), :(⥆), :(⥇), :(⥈), :(⥊), :(⥋), :(⥎), :(⥐), :(⥒),
        :(⥓), :(⥖), :(⥗), :(⥚), :(⥛), :(⥞), :(⥟), :(⥢), :(⥤), :(⥦),
        :(⥧), :(⥨), :(⥩), :(⥪), :(⥫), :(⥬), :(⥭), :(⥰), :(⧴), :(⬱),
        :(⬰), :(⬲), :(⬳), :(⬴), :(⬵), :(⬶), :(⬷), :(⬸), :(⬹), :(⬺),
        :(⬻), :(⬼), :(⬽), :(⬾), :(⬿), :(⭀), :(⭁), :(⭂), :(⭃), :(⭄),
        :(⭇), :(⭈), :(⭉), :(⭊), :(⭋), :(⭌), :(￩), :(￫)],
       [:(>),  :(<), :(>=), :(≥), :(<=), :(≤), :(==), :(===), :(≡),
        :(!=), :(≠), :(!==), :(≢), :(.>), :(.<), :(.>=), :(.≥), :(.<=),
        :(.≤), :(.==), :(.!=), :(.≠), :(.=), :(.!), :(<:), :(>:), :(∈),
        :(∉), :(∋), :(∌), :(⊆), :(⊈), :(⊂), :(⊄), :(⊊), :(∝), :(∊), :(∍),
        :(∥), :(∦), :(∷), :(∺), :(∻), :(∽), :(∾), :(≁), :(≃), :(≄), :(≅),
        :(≆), :(≇), :(≈), :(≉), :(≊), :(≋), :(≌), :(≍), :(≎), :(≐), :(≑),
        :(≒), :(≓), :(≔), :(≕), :(≖), :(≗), :(≘), :(≙), :(≚), :(≛), :(≜),
        :(≝), :(≞), :(≟), :(≣), :(≦), :(≧), :(≨), :(≩), :(≪), :(≫), :(≬),
        :(≭), :(≮), :(≯), :(≰), :(≱), :(≲), :(≳), :(≴), :(≵), :(≶), :(≷),
        :(≸), :(≹), :(≺), :(≻), :(≼), :(≽), :(≾), :(≿), :(⊀), :(⊁), :(⊃),
        :(⊅), :(⊇), :(⊉), :(⊋), :(⊏), :(⊐), :(⊑), :(⊒), :(⊜), :(⊩), :(⊬),
        :(⊮), :(⊰), :(⊱), :(⊲), :(⊳), :(⊴), :(⊵), :(⊶), :(⊷), :(⋍), :(⋐),
        :(⋑), :(⋕), :(⋖), :(⋗), :(⋘), :(⋙), :(⋚), :(⋛), :(⋜), :(⋝), :(⋞),
        :(⋟), :(⋠), :(⋡), :(⋢), :(⋣), :(⋤), :(⋥), :(⋦), :(⋧), :(⋨), :(⋩),
        :(⋪), :(⋫), :(⋬), :(⋭), :(⋲), :(⋳), :(⋴), :(⋵), :(⋶), :(⋷), :(⋸),
        :(⋹), :(⋺), :(⋻), :(⋼), :(⋽), :(⋾), :(⋿), :(⟈), :(⟉), :(⟒), :(⦷),
        :(⧀), :(⧁), :(⧡), :(⧣), :(⧤), :(⧥), :(⩦), :(⩧), :(⩪), :(⩫), :(⩬),
        :(⩭), :(⩮), :(⩯), :(⩰), :(⩱), :(⩲), :(⩳), :(⩴), :(⩵), :(⩶), :(⩷),
        :(⩸), :(⩹), :(⩺), :(⩻), :(⩼), :(⩽), :(⩾), :(⩿), :(⪀), :(⪁), :(⪂),
        :(⪃), :(⪄), :(⪅), :(⪆), :(⪇), :(⪈), :(⪉), :(⪊), :(⪋), :(⪌), :(⪍),
        :(⪎), :(⪏), :(⪐), :(⪑), :(⪒), :(⪓), :(⪔), :(⪕), :(⪖), :(⪗), :(⪘),
        :(⪙), :(⪚), :(⪛), :(⪜), :(⪝), :(⪞), :(⪟), :(⪠), :(⪡), :(⪢), :(⪣),
        :(⪤), :(⪥), :(⪦), :(⪧), :(⪨), :(⪩), :(⪪), :(⪫), :(⪬), :(⪭), :(⪮),
        :(⪯), :(⪰), :(⪱), :(⪲), :(⪳), :(⪴), :(⪵), :(⪶), :(⪷), :(⪸), :(⪹),
        :(⪺), :(⪻), :(⪼), :(⪽), :(⪾), :(⪿), :(⫀), :(⫁), :(⫂), :(⫃), :(⫄),
        :(⫅), :(⫆), :(⫇), :(⫈), :(⫉), :(⫊), :(⫋), :(⫌), :(⫍), :(⫎), :(⫏),
        :(⫐), :(⫑), :(⫒), :(⫓), :(⫔), :(⫕), :(⫖), :(⫗), :(⫘), :(⫙), :(⫷),
        :(⫸), :(⫹), :(⫺), :(⊢), :(⊣)],
       [:(|>),  :(<|)],
       [:(:), :(..)],
       [:(+), :(-), :(⊕), :(⊖), :(⊞), :(⊟), :(.+), :(.-), :(|), :(∪), :(∨),
        :($), :(⊔), :(±), :(∓), :(∔), :(∸), :(≂), :(≏), :(⊎), :(⊻), :(⊽),
        :(⋎), :(⋓), :(⧺), :(⧻), :(⨈), :(⨢), :(⨣), :(⨤), :(⨥), :(⨦), :(⨧),
        :(⨨), :(⨩), :(⨪), :(⨫), :(⨬), :(⨭), :(⨮), :(⨹), :(⨺), :(⩁), :(⩂),
        :(⩅), :(⩊), :(⩌), :(⩏), :(⩐), :(⩒), :(⩔), :(⩖), :(⩗), :(⩛), :(⩝),
        :(⩡), :(⩢), :(⩣)],
       [:(<<), :(>>), :(>>>), :(.<<), :(.>>), :(.>>>)],
       [:(*), :(/), :(./), :(÷), :(%), :(⋅), :(∘), :(×), :(.%), :(.*), :(\), :(.\),
        :(&), :(∩), :(∧), :(⊗), :(⊘), :(⊙), :(⊚), :(⊛), :(⊠), :(⊡), :(⊓), :(∗),
        :(∙), :(∤), :(⅋), :(≀), :(⊼), :(⋄), :(⋆), :(⋇), :(⋉), :(⋊), :(⋋), :(⋌),
        :(⋏), :(⋒), :(⟑), :(⦸), :(⦼), :(⦾), :(⦿), :(⧶), :(⧷), :(⨇), :(⨰), :(⨱),
        :(⨲), :(⨳), :(⨴), :(⨵), :(⨶), :(⨷), :(⨸), :(⨻), :(⨼), :(⨽), :(⩀), :(⩃),
        :(⩄), :(⩋), :(⩍), :(⩎), :(⩑), :(⩓), :(⩕), :(⩘), :(⩚), :(⩜), :(⩞), :(⩟),
        :(⩠), :(⫛), :(⊍)],
       [:(//), :(.//)],
       [:(^), :(.^), :(↑), :(↓), :(⇵), :(⟰), :(⟱), :(⤈), :(⤉), :(⤊), :(⤋),
        :(⤒), :(⤓),  :(⥉), :(⥌), :(⥍), :(⥏), :(⥑), :(⥔), :(⥕), :(⥘), :(⥙),
        :(⥜), :(⥝),  :(⥠), :(⥡), :(⥣), :(⥥), :(⥮), :(⥯), :(￪), :(￬)],
       [:(::)],
       [:(.)]
]

precedent_ops(n::Integer) = Set{Symbol}(ops_by_precedent[n])

const assignment_ops = Set{Symbol}(ops_by_precedent[1])

const unary_ops = Set{Symbol}([:(+),  :(-), :(!), :(~), :(<:), :(¬),
                               :(>:), :(√), :(∛), :(∜)])

const unary_and_binary_ops = Set{Symbol}([:(+), :(-), :($), :(&), :(~)])

# Operators are special forms, not function names
const syntactic_ops = Set{Symbol}([:(=),   :(:=),  :(+=),   :(-=),  :(*=),
                                   :(/=),  :(//=), :(./=),  :(.*=), :(./=),
                                   :(\=),  :(.\=), :(^=),   :(.^=), :(%=),
                                   :(.%=), :(|=),  :(&=),   :($=),  :(=>),
                                   :(<<=), :(>>=), :(>>>=), :(->),  :(-->),
                                   :(||),  :(&&),  :(.),    :(...), :(.+=),
                                   :(.-=)])

const syntactic_unary_ops = Set{Symbol}([:($), :(&), :(::)])

const operators = union(Set([:(~), :(!), :(->), :(√), :(∛), :(∜), :(...), :(¬),
                             :(.'), SYM_CTRANSPOSE]),
			                 [Set(ops) for ops in ops_by_precedent]...)

const reserved_words = Set{Symbol}([:begin,  :while, :if, :for, :try, :return,
                                    :break, :continue, :function, :stagedfunction,
                                    :macro, :quote, :let, :local, :global, :const,
                                    :abstract, :typealias, :type, :bitstype, :immutable,
                                    :ccall, :do, :module, :baremodule, :using, :import,
                                    :export, :importall])
#= Helper functions =#

const operator_prescedence = let
    const precedence_map = Dict{Symbol, Int}()
    for (i, ops) in enumerate(ops_by_precedent)
        for op in ops
            precedence_map[op] = i
        end
    end
    operator_prescedence(op::Symbol) = precedence_map[op]
end

is_syntactic_op(op::Symbol) = in(op, syntactic_ops)
is_syntactic_op(op) = false

is_syntactic_unary_op(op::Symbol) = in(op, syntactic_unary_ops)
is_syntactic_unary_op(op) = false

const is_special_char = let chars = Set{Char}("()[]{},;\"`@")
    is_special_char(c::Char)  = in(c, chars)
end

isnewline(c::Char) = c === '\n'
isnewline(c) = false

function isuws(c::Char)
    return (c==9    || c==10   || c==11   || c==12   || c==13   || c==32 ||
            c==133  || c==160  || c==5760 || c==6158 || c==8192 ||
            c==8193 || c==8194 || c==8195 || c==8196 || c==8197 ||
            c==8198 || c==8199 || c==8200 || c==8201 || c==8202 ||
            c==8232 || c==8233 || c==8239 || c==8287 || c==12288)
end

isbom(c::Char) = c == 0xFEFF

is_zero_width_space(c::Char) = c === '\u200b' || c === '\u2060' || c === '\ufeff'

is_ignorable_char(c::Char) = is_zero_width_space(c) ||
                             ('\u200c' <= c <= '\u200f') ||
                             (c === '\u00ad' || c === '\u2061' || c === '\u115f')

function is_cat_id_start(c::Char, cat::Integer)
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
    elseif (c < 0xA1 || c > 0x10ffff)
        return false
    end
    cat = UTF8proc.category_code(c)
    is_cat_id_start(c, cat) && return true
    if cat == UTF8proc.UTF8PROC_CATEGORY_MN || cat == UTF8proc.UTF8PROC_CATEGORY_MC ||
       cat == UTF8proc.UTF8PROC_CATEGORY_ND || cat == UTF8proc.UTF8PROC_CATEGORY_PC ||
       cat == UTF8proc.UTF8PROC_CATEGORY_SK || cat == UTF8proc.UTF8PROC_CATEGORY_ME ||
       cat == UTF8proc.UTF8PROC_CATEGORY_NO ||
       (0x2032 <= c <= 0x2034) || # primes
       c == 0x0387 || c == 0x19da ||
       (0x1369 <= c <= 0x1371)
       return true
    end
    return false
end

function is_identifier_start_char(c::Char)
    if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_')
        return true
    elseif (c < 0xA1 || c > 0x10ffff)
        return false
    end
    cat = UTF8proc.category_code(c)
    return is_cat_id_start(c, cat)
end

#= Characters that can be in an operator =#
const operator_chars = union([Set(string(op)) for op in operators]...)

is_opchar(c::Char) = in(c, operator_chars)

#= Characters that can follow a . in an operator =#
const is_dot_opchar = let chars = Set{Char}(".*^/\\+-'<>!=%≥≤≠")
    is_dot_opchar(c::Char) = in(c, chars)
end

is_operator(op::Symbol) = in(op, operators)
is_operator(op) = false

#= Implement peekchar for IOBuffer and IOStream =#

# modified version from Base to give the same
# semantics as the IOStream implementation

function peekchar(io::IOBuffer)
    if !io.readable || io.ptr > io.size
        return EOF
    end
    ch = uint8(io.data[io.ptr])
    if ch < 0x80
        return char(ch)
    end
    # mimic utf8.next function
    trailing = Base.utf8_trailing[ch+1]
    c::Uint32 = 0
    for j = 1:trailing
        c += ch
        c <<= 6
        ch = uint8(io.data[io.ptr+j])
    end
    c += ch
    c -= Base.utf8_offset[trailing+1]
    return char(c)
end

# this implementation is copied from Base
const _chtmp = Array(Char, 1)
peekchar(s::IOStream) = begin
    if ccall(:ios_peekutf8, Int32, (Ptr{Void}, Ptr{Char}), s, _chtmp) < 0
        return EOF
    end
    return _chtmp[1]
end

eof(io::IO) = Base.eof(io)
eof(c) = is(c, EOF)

readchar(io::IO) = eof(io) ? EOF : read(io, Char)
takechar(io::IO) = (readchar(io); io)

#= Token Stream =#

typealias Token Union(Symbol, Char, Number, Void)

type TokenStream
    io::IO
    lineno::Int
    lasttoken
    putback
    isspace::Bool
    ateof::Bool
    filename::AbstractString
end

TokenStream(io::IO) = TokenStream(io, 1, nothing, nothing, false, eof(io), "")
TokenStream(str::AbstractString) = TokenStream(IOBuffer(str))

eof(ts::TokenStream) = ts.ateof || eof(ts.io)
skip(ts::TokenStream, n)  = Base.skip(ts.io, n)
position(ts::TokenStream) = Base.position(ts.io)
peekchar(ts::TokenStream) = peekchar(ts.io)

readchar(ts::TokenStream) = begin
    eof(ts) && return EOF
    c = read(ts.io, Char)
    c === '\n' && (ts.lineno += 1)
    return c
end

takechar(ts::TokenStream) = (readchar(ts); ts)

function skipws(ts::TokenStream, newlines::Bool=false)
    nc = peekchar(ts)
    nc === EOF && return false
    skipped = false
    while !eof(ts) && (isuws(nc) || isbom(nc)) && (newlines || nc !== '\n')
        takechar(ts)
        nc, skipped = peekchar(ts), true
    end
    return skipped
end

#= Lexer =#
function skip_to_eol(io::IO)
    while !eof(io)
        nc = peekchar(io)
        nc === '\n' && break
        Base.skip(io, Base.utf8sizeof(nc))
    end
    return io
end
skip_to_eol(ts::TokenStream) = skip_to_eol(ts.io)

function read_operator(ts::TokenStream, c::Char)
    nc = peekchar(ts)
    if (c === '*') && (nc === '*')
        error("use \"^\" instead of \"**\"")
    end
    # 1 char operator
    if eof(nc) || !is_opchar(nc) || (c === ':' && nc === '-')
        return symbol(c)
    end
    str, c = [c], nc
    opsym  = symbol(utf32(str))
    while true
        if !eof(c) && is_opchar(c)
            push!(str, c)
            newop = symbol(utf32(str))
            if is_operator(newop)
                skip(ts, Base.utf8sizeof(c))
                c, opsym = peekchar(ts), newop
                continue
            end
        end
        return opsym
    end
end

#=============#
# Read Number
#=============#

function sized_uint_literal(s::AbstractString, b::Integer)
    i = s[1] === '-' ? 3 : 2
    l = (length(s) - i) * b
    l <= 8   && return parseint(Uint8,   s)
    l <= 16  && return parseint(Uint16,  s)
    l <= 32  && return parseint(Uint32,  s)
    l <= 64  && return parseint(Uint64,  s)
    l <= 128 && return parseint(Uint128, s)
    return BigInt(s)
end

function sized_uint_oct_literal(s::AbstractString)
    contains(s, "o0") && return sized_uint_literal(s, 3)
    len = length(s)
    (len < 5  || (len == 5  && s <= "0o377")) && return uint8(s)
    (len < 8  || (len == 8  && s <= "0o177777")) && return uint16(s)
    (len < 13 || (len == 13 && s <= "0o37777777777")) && return uint32(s)
    (len < 24 || (len == 24 && s <= "0o1777777777777777777777")) && return uint64(s)
    (len < 45 || (len == 45 && s <= "0o3777777777777777777777777777777777777777777")) && return uint128(s)
    return BigInt(s)
end

function compare_num_strings(s1::AbstractString, s2::AbstractString)
    s1, s2 = lstrip(s1, '0'), lstrip(s2, '0')
    l1, l2 = length(s1), length(s2)
    return l1 == l2 ? s1 <= s2 : l1 <= l2
end

function is_oct_within_uint128(s::AbstractString)
    len = length(s)
    max = "0o3777777777777777777777777777777777777777777"
    len < 45  && return true
    len > 45  && return false
    len == 45 && s[1] === '-' ? s[2:end] <= max : s <= max
end

function is_within_int128(s::AbstractString)
    len = length(s)
    if s[1] === '-'
        len < 40  && return true
        len > 40  && return false
        len == 40 && return s <= "-170141183460469231731687303715884105728"
    else
        len < 39  && return true
        len > 39  && return false
        len == 39 && s <= "170141183460469231731687303715884105727"
    end
end

# Notes:
# expressions starting with 0x are always hexadecimal literals
# expressions starting with a numeric literal followed by e or E
# are always floating point literals

function string_to_number(str::AbstractString)
    len = length(str)
    len > 0 || error("empty string")
    neg = str[1] === '-'
    # NaN and Infinity
    (str == "NaN" || str == "+NaN" || str == "-NaN") && return NaN
    (str == "Inf" || str == "+Inf" || str == "-Inf") && return Inf
    # floating point literals
    didx, fidx = 0, 0
    isfloat32, isfloat64 = false, false
    for i=1:len
        c = str[i]
        if c === '.'
            if isfloat64 == false
                didx, isfloat64 = i, true
            else
                error("invalid float string \"$str\"")
            end
        elseif c === 'f'
            if i > didx && i != len
                fidx, isfloat32 = i, true
            else
                error("invalid float32 string \"$str\"")
            end
        elseif c === 'e' || c === 'E' || c === 'p' || c === 'P'
            isfloat64 = true
        end
    end
    if isfloat32
        base = float64(str[1:fidx-1])
        expn = int(str[fidx+1:end])
        return float32(base * 10.0 ^ expn)
    elseif isfloat64
        return float64(str)
    else
        try
            return int64(str)
        catch ex
            # its better to ask for forgiveness...
            !isa(ex, OverflowError) && rethrow(ex)
            if is_within_int128(str)
                return int128(str)
            else
                return BigInt(str)
            end
        end
    end
end

# accum digit predicates
is_char_hex(c::Char) = isdigit(c) || ('a' <= c <= 'f')  || ('A' <= c <= 'F')
is_char_oct(c::Char) = '0' <= c <= '7'
is_char_bin(c::Char) = c === '0' || c === '1'

function accum_digits(ts::TokenStream, pred::Function, c::Char, leading_zero::Bool)
    if !leading_zero && c == '_'
        return (Char[], false)
    end
    charr = Char[]
    while true
        if c == '_'
            skip(ts, 1)
            c = peekchar(ts)
            if !eof(c) && pred(c)
                continue
            else
                skip(ts, -1)
                break
            end
        elseif !eof(c) && pred(c)
            skip(ts, 1)
            push!(charr, c)
            c = peekchar(ts)
            continue
        end
        break
    end
    return (charr, true)
end

#TODO: can we get rid of this?
fix_uint_neg(neg::Bool, n::Number) = neg? Expr(:call, :- , n) : n

function disallow_dot!(ts::TokenStream, charr::Vector{Char})
    if peekchar(ts) === '.'
        skip(ts, 1)
        if is_dot_opchar(peekchar(ts))
            skip(ts, -1)
        else
            error("invalid numeric constant \"$(utf32(charr)).\"")
        end
    end
end

function read_digits!(ts::TokenStream, pred::Function, charr::Vector{Char}, leading_zero::Bool)
    digits, ok = accum_digits(ts, pred, peekchar(ts), leading_zero)
    ok || error("invalid numeric constant \"$digits\"")
    isempty(digits) && return false
    append!(charr, digits)
    return true
end

#TODO: try to remove neg as it is not needed for the lexer
function read_number(ts::TokenStream, leading_dot::Bool, neg::Bool)
    charr = Char[]
    pred::Function = isdigit

    leading_zero = false
    is_float32_literal  = false
    is_hexfloat_literal = false

    neg && push!(charr, '-')
    if leading_dot
        push!(charr, '.')
    else
        if peekchar(ts) == '0'
            push!(charr, readchar(ts))
            leading_zero = true
            nc = peekchar(ts)
            if nc === 'x'
                skip(ts, 1); push!(charr, nc)
                leading_zero = false
                pred = is_char_hex
            elseif nc === 'o'
                skip(ts, 1); push!(charr, nc)
                leading_zero = false
                pred = is_char_oct
            elseif nc === 'b'
                skip(ts, 1); push!(charr, nc)
                leading_zero = false
                pred = is_char_bin
            end
        else
            nc = peekchar(ts)
            nc === '.' && (skip(ts, 1); push!(charr, nc))
        end
    end
    read_digits!(ts, pred, charr, leading_zero)
    if peekchar(ts) == '.'
        skip(ts, 1)
        if is_dot_opchar(peekchar(ts))
            skip(ts, -1)
        else
            push!(charr, '.')
            read_digits!(ts, pred, charr, false)
            disallow_dot!(ts, charr)
        end
    end
    c = peekchar(ts)
    ispP = c === 'p' || c === 'P'
    if (is_hexfloat_literal && (ispP || error("hex float literal must contain 'p' or 'P'"))) ||
       (pred === is_char_hex && ispP) || (c === 'e' || c === 'E' || c === 'f')
        skip(ts, 1)
        nc = peekchar(ts)
        if !eof(nc) && (isdigit(nc) || nc === '+' || nc === '-')
            skip(ts, 1)
            is_float32_literal = c === 'f'
            is_hexfloat_literal = ispP
            push!(charr, c)
            push!(charr, nc)
            read_digits!(ts, pred, charr, false)
            disallow_dot!(ts, charr)
        else
            skip(ts, -1)
        end
    # disallow digits after binary or octal literals, e.g. 0b12
    elseif (pred == is_char_bin || pred == is_char_oct) && !eof(c) && isdigit(c)
        push!(charr, c)
        error("invalid numeric constant \"$(utf32(charr))\"")
    end
    str = utf32(charr)
    base = pred == is_char_hex ? 16 :
           pred == is_char_oct ? 8  :
           pred == is_char_bin ? 2  : 10
    # for an unsigned literal starting with -,
    # remove the - and parse instead as a call to unary -
    (neg && base != 10 && !is_hexfloat_literal) && (str = str[2:end])
    if is_hexfloat_literal
        return float64(str)
    elseif pred == is_char_hex
        return fix_uint_neg(neg, sized_uint_literal(str, 4))
    elseif pred == is_char_oct
        return fix_uint_neg(neg, sized_uint_oct_literal(str))
    elseif pred == is_char_bin
        return fix_uint_neg(neg, sized_uint_literal(str, 1))
    elseif is_float32_literal
        n = string_to_number(str)
        return float32(n)
    else
        return string_to_number(str)
    end
end

#============================#
# Skip whitespace / comments
#============================#

# skip multiline comments
# maintain a count of the number of enclosed #= =# pairs
# to allow nesting of multi-line comments.
# The loop is exited when this count goes below zero.

# Count is the number of read (#=) tokens.
# (#= test =#)  (#= test =#)  (#= #= test =# =#)
#  ^              ^               ^        ^
# cnt 0           cnt 1         cnt 2    cnt 1
function skip_multiline_comment(ts::TokenStream, count::Int)
    start, unterminated = -1, true
    while !eof(ts)
        c = readchar(ts)
        # if "=#" token, decrement the count.
        # If count is zero, break out of the loop
        if c === '='
            start > 0 || (start = position(ts))
            if peekchar(ts) === '#' && position(ts) != start
                takechar(ts)
                count <= 1 || (count -= 1; continue)
                unterminated = false
                break
            end
            continue
        # if "#=" token increase count
        elseif c === '#' && peekchar(ts) === '='
            count += 1
        end
    end
    if unterminated
        error("incomplete: unterminated multi-line comment #= ... =#")
    end
    return ts
end

# if this is a mulitiline comment skip to the end
# otherwise skip to end of line
function skipcomment(ts::TokenStream)
    @assert readchar(ts) === '#'
    if peekchar(ts) === '='
        skip_multiline_comment(ts, 1)
    else
        skip_to_eol(ts)
    end
    return ts
end

# skip all whitespace before a comment,
# upon reaching the comment, if it is a
# single line comment skip to the end of the line
# otherwise skip to the end of the multiline comment block
function skipws_and_comments(ts::TokenStream)
    while !eof(ts)
        skipws(ts, true)
        peekchar(ts) !== '#' && break
        skipcomment(ts)
    end
    return ts
end

function accum_julia_symbol(ts::TokenStream, c::Char)
    nc, charr = c, Char[]
    while is_identifier_char(nc)
        c, nc = readchar(ts), peekchar(ts)
        # make sure that != is always an operator
        if c === '!' && nc === '='
            skip(ts, -1)
            break
        end
        push!(charr, c)
        eof(nc) && break
    end
    str = normalize_string(utf32(charr), :NFC)
    sym = symbol(str)
    return sym === SYM_TRUE ? true : sym === SYM_FALSE ? false : sym
end

#= Token stream methods =#

function next_token(ts::TokenStream, whitespace_newline::Bool)
    ts.ateof && return EOF
    tmp = skipws(ts, whitespace_newline)
    tmp == EOF && return EOF
    ts.isspace = tmp
    while !eof(ts.io)
        c = peekchar(ts)
        if eof(c)
            ts.ateof = true
            return EOF
        elseif c === ' ' || c === '\t'
            skip(ts, 1)
            continue
        elseif c === '#'
            skipcomment(ts)
            if whitespace_newline && peekchar(ts) === '\n'
                takechar(ts)
            end
            continue
        elseif isnewline(c)
            return readchar(ts)
        elseif is_special_char(c)
            return readchar(ts)
        elseif isdigit(c)
            return read_number(ts, false, false)
        elseif c === '.'
            skip(ts, 1)
            nc = peekchar(ts)
            if isdigit(nc)
                return read_number(ts, true, false)
            elseif is_opchar(nc)
                op = read_operator(ts, c)
                if op === :(..) && is_opchar(peekchar(ts))
                    error(string("invalid operator \"", op, peekchar(ts), "\""))
                end
                return op
            end
            return :(.)
        elseif is_opchar(c)
            return read_operator(ts, readchar(ts))
        elseif is_identifier_start_char(c)
            return accum_julia_symbol(ts, c)
        else
            @assert readchar(ts) === c
            if is_ignorable_char(c)
                error("invisible character \\u$(hex(c))")
            else
                error("invalid character \"$c\"")
            end
        end
    end
    ts.ateof = true
    return EOF
end

next_token(ts::TokenStream) = next_token(ts, false)
last_token(ts::TokenStream) = ts.lasttoken
set_token!(ts::TokenStream, t) = (ts.lasttoken = t; ts)

function put_back!(ts::TokenStream, t)
    if ts.putback !== nothing
        error("too many pushed back tokens (internal error)")
    end
    ts.putback = t
    return ts
end

function peek_token(ts::TokenStream, whitespace_newline::Bool)
    ts.ateof && return EOF
    if ts.putback !== nothing
        return ts.putback
    end
    lt = last_token(ts)
    if lt !== nothing
        return lt
    end
    set_token!(ts, next_token(ts, whitespace_newline))
    return last_token(ts)
end
peek_token(ts::TokenStream) = peek_token(ts, false)

function take_token(ts::TokenStream)
    ts.ateof && return EOF
    if ts.putback !== nothing
        t = ts.putback
        ts.putback = nothing
    else
        t = last_token(ts)
        set_token!(ts, nothing)
    end
    return t
end

function require_token(ts::TokenStream, whitespace_newline::Bool)
    if ts.putback !== nothing
        t = ts.putback
    elseif ts.lasttoken !== nothing
        t = ts.lasttoken
    else
        t = next_token(ts, whitespace_newline)
    end
    eof(t) && error("incomplete: premature end of input")
    while isnewline(t)
        take_token(ts)
        t = next_token(ts, whitespace_newline)
    end
    ts.putback === nothing && set_token!(ts, t)
    return t
end
require_token(ts::TokenStream) = require_token(ts, false)

end
