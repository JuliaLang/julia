# This file is a part of Julia. License is MIT: http://julialang.org/license

# Unicode properties, such as General Category
# Unix/C is* convenience functions (for now)

# whether codepoints are valid Unicode scalar values, i.e. 0-0xd7ff, 0xe000-0x10ffff
isvalid(::Type{Char}, ch::Unsigned) = !((ch - 0xd800 < 0x800) | (ch > 0x10ffff))
isvalid(::Type{Char}, ch::Integer)  = isvalid(Char, Unsigned(ch))
isvalid(::Type{Char}, ch::Char)     = isvalid(Char, UInt32(ch))

isvalid(ch::Char) = isvalid(Char, ch)

"""Unicode character properties"""
abstract Property

"""
Return various Unicode properties for character
"""
function charprop end

# Unicode General Category constants

module Category

"""Unicode character category type"""
abstract General     <: Unicode.Property

"""Unicode 'Letter' character category"""
abstract Letter      <: General
"""Unicode 'Mark' character category"""
abstract Mark        <: General
"""Unicode 'Number' character category"""
abstract Number      <: General
"""Unicode 'Punctuation' character category"""
abstract Punctuation <: General
"""Unicode 'Symbol' character category"""
abstract Symbol      <: General
"""Unicode 'Separator' character category"""
abstract Separator   <: General
"""Unicode 'Other' character category"""
abstract Other       <: General

"""Unicode uppercase & titlecase letters"""
abstract Upper       <: Letter

"""Unicode alphabetic and numeric"""
typealias AlphaNumeric Union{Letter, Number}

"""Unicode character category code (0-29)"""
bitstype 8 Code

"""Unicode character category mask"""
typealias Mask UInt32

Base.convert(::Type{Code}, x::Integer) = reinterpret(Code, x%UInt8)
Base.convert{T<:Integer}(::Type{T}, x::Code) = convert(T, reinterpret(UInt8, x))
Base.promote_rule{T<:Integer}(::Type{T}, ::Type{Code}) = T
Base.isless(x::Code, y::Code) = isless(UInt8(x), UInt8(y))
Base.isless(x::Code, y::Integer)  = isless(UInt8(x), y)
Base.isless(x::Integer, y::Code)  = isless(x, UInt8(y))

for (nam, val, cat, typ, des) in
    ((:Cn, 0,  :NotAssignedChar,         :Other,       "Other, Not assigned"),
     (:Lu, 1,  :UpperCase,               :Upper,       "Letter, uppercase"),
     (:Ll, 2,  :LowerCase,               :Letter,      "Letter, lowercase"),
     (:Lt, 3,  :TitleCase,               :Upper,       "Letter, titlecase"),
     (:Lm, 4,  :ModifierLetter,          :Letter,      "Letter, modifier"),
     (:Lo, 5,  :OtherLetter,             :Letter,      "Letter, other"),
     (:Mn, 6,  :NonSpacingMark,          :Mark,        "Mark, nonspacing"),
     (:Mc, 7,  :CombiningMark,           :Mark,        "Mark, spacing combining"),
     (:Me, 8,  :EnclosingMark,           :Mark,        "Mark, enclosing"),
     (:Nd, 9,  :DecimalDigit,            :Number,      "Number, decimal digit"),
     (:Nl, 10, :NumericLetter,           :Number,      "Number, letter"),
     (:No, 11, :OtherNumber,             :Number,      "Number, other"),
     (:Pc, 12, :ConnectorPunctuation,    :Punctuation, "Punctuation, connector"),
     (:Pd, 13, :DashPunctuation,         :Punctuation, "Punctuation, dash"),
     (:Ps, 14, :OpenPunctuation,         :Punctuation, "Punctuation, open"),
     (:Pe, 15, :ClosePunctuation,        :Punctuation, "Punctuation, close"),
     (:Pi, 16, :InitialQuotePunctuation, :Punctuation, "Punctuation, initial quote"),
     (:Pf, 17, :FinalQuotePunctuation,   :Punctuation, "Punctuation, final quote"),
     (:Po, 18, :OtherPunctuation,        :Punctuation, "Punctuation, other"),
     (:Sm, 19, :MathSymbol,              :Symbol,      "Symbol, math"),
     (:Sc, 20, :CurrencySymbol,          :Symbol,      "Symbol, currency"),
     (:Sk, 21, :ModifierSymbol,          :Symbol,      "Symbol, modifier"),
     (:So, 22, :OtherSymbol,             :Symbol,      "Symbol, other"),
     (:Zs, 23, :SpaceSeparator,          :Separator,   "Separator, space"),
     (:Zl, 24, :LineSeparator,           :Separator,   "Separator, line"),
     (:Zp, 25, :ParagraphSeparator,      :Separator,   "Separator, paragraph"),
     (:Cc, 26, :ControlChar,             :Other,       "Other, control"),
     (:Cf, 27, :FormatChar,              :Other,       "Other, format"),
     (:Cs, 28, :SurrogateChar,           :Other,       "Other, surrogate"),
     (:Co, 29, :PrivateUseChar,          :Other,       "Other, private use"))
    @eval const global $nam = $(Code(val))
    @eval abstract $cat <: $typ
    @eval Base.convert(::Type{Code}, ct::$cat) = $(Code(val))
    @eval @doc $(string("Unicode Category Code: ",des)) $nam
    @eval @doc $(string("Unicode Category Type: ",des)) $cat
end

const c2t = [NotAssignedChar, UpperCase, LowerCase, TitleCase, ModifierLetter, OtherLetter,
             NonSpacingMark, CombiningMark, EnclosingMark,
             DecimalDigit, NumericLetter, OtherNumber,
             ConnectorPunctuation, DashPunctuation, OpenPunctuation, ClosePunctuation,
             InitialQuotePunctuation, FinalQuotePunctuation, OtherPunctuation,
             MathSymbol, CurrencySymbol, ModifierSymbol, OtherSymbol,
             SpaceSeparator, LineSeparator, ParagraphSeparator,
	     ControlChar, FormatChar, SurrogateChar, PrivateUseChar]

Base.convert(::Type{General}, cat::Code) = c2t[Int(cat)+1]

Unicode.charprop(Mask, c) = Mask(1<<Int(charprop(Code, c)))

const global UpperMask  = Mask(1<<Int(Lu) | 1<<Int(Lt))
const global AlphaMask  = Mask(1<<Int(Lu) | 1<<Int(Ll) | 1<<Int(Lt) | 1<<Int(Lm) | 1<<Int(Lo))
const global NumberMask = Mask((1<<Int(Nd) | 1<<Int(Nl) | 1<<Int(No)))
const global AlphaNumericMask = AlphaMask | NumberMask

let mask = 0 ; for i = Int(Pc):Int(Po) ; mask |= (1<<i) ; end
    @eval const global PunctuationMask = $(Mask(mask))
    mask = 0 ; for i = Int(Lu):Int(So) ; mask |= (1<<i) ; end
    @eval const global GraphMask = $(Mask(mask))
    @eval const global PrintMask = $(Mask(mask | (1<<Int(Zs))))
end

end # module Cat
importall .Category

############################################################################

is_assigned_char(c) = charprop(Category.Code, c) != Category.Cn

islower(c::Char)  = charprop(Category.Code, c) == Category.Ll

# true for Unicode upper and mixed case
isupper(c::Char)  = (charprop(Category.Mask, c) & Category.UpperMask) != 0
isalpha(c::Char)  = (charprop(Category.Mask, c) & Category.AlphaMask) != 0
isnumber(c::Char) = (charprop(Category.Mask, c) & Category.NumberMask) != 0
isalnum(c::Char)  = (charprop(Category.Mask, c) & Category.AlphaNumericMask) != 0
ispunct(c::Char)  = (charprop(Category.Mask, c) & Category.PunctuationMask) != 0
isprint(c::Char)  = (charprop(Category.Mask, c) & Category.PrintMask) != 0
# true in principle if a printer would use ink
isgraph(c::Char)  = (charprop(Category.Mask, c) & Category.GraphMask) != 0

isdigit(c::Char)  = ('0' <= c <= '9')

# following C++ only control characters from the Latin-1 subset return true
iscntrl(c::Char) = (c <= Char(0x1f) || Char(0x7f) <= c <= Char(0x9f))


# \u85 is the Unicode Next Line (NEL) character
# the check for \ufffd allows for branch removal on ASCIIStrings
@inline isspace(c::Char) =
    (c == ' ' || '\t' <= c <='\r' || c == '\u85' ||
     ('\ua0' <= c && c != '\ufffd' && charprop(Category.Code, c) == Category.Zs))


for name = ("alnum", "alpha", "cntrl", "digit", "number", "graph",
            "lower", "print", "punct", "space", "upper")
    f = symbol("is",name)
    @eval begin
        function $f(s::AbstractString)
            for c in s
                $f(c) || return false
            end
            return true
        end
    end
end
