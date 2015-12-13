# This file is a part of Julia. License is MIT: http://julialang.org/license

# Unicode properties, such as General Category
# Unix/C is* convenience functions (for now)

# whether codepoints are valid Unicode scalar values, i.e. 0-0xd7ff, 0xe000-0x10ffff
isvalid(::Type{Char}, ch::Unsigned) = !((ch - 0xd800 < 0x800) | (ch > 0x10ffff))
isvalid(::Type{Char}, ch::Integer)  = isvalid(Char, Unsigned(ch))
isvalid(::Type{Char}, ch::Char)     = isvalid(Char, UInt32(ch))

isvalid(ch::Char) = isvalid(Char, ch)

# Unicode General Category constants

module Cat
export Property, CharType, CharCode

"""Unicode character properties"""
abstract Property

"""Unicode character category type"""
abstract CharType    <: Property

"""Unicode 'Letter' character category"""
abstract Letter      <: CharType
"""Unicode 'Mark' character category"""
abstract Mark        <: CharType
"""Unicode 'Number' character category"""
abstract Number      <: CharType
"""Unicode 'Punctuation' character category"""
abstract Punctuation <: CharType
"""Unicode 'Symbol' character category"""
abstract Symbol      <: CharType
"""Unicode 'Separator' character category"""
abstract Separator   <: CharType
"""Unicode 'Other' character category"""
abstract Other       <: CharType

"""Unicode uppercase & titlecase letters"""
abstract Upper       <: Letter

"""Unicode character category code (0-29)"""
bitstype 8 CharCode

end # module Cat
import .Cat: Property, CharType, CharCode

convert(::Type{CharCode}, x::Integer) = reinterpret(CharCode, x%UInt8)
convert{T<:Integer}(::Type{T}, x::CharCode) = convert(T, reinterpret(UInt8, x))
promote_rule{T<:Integer}(::Type{T}, ::Type{CharCode}) = T
isless(x::CharCode, y::CharCode) = isless(UInt8(x), UInt8(y))
isless(x::CharCode, y::Integer)  = isless(UInt8(x), y)
isless(x::Integer, y::CharCode)  = isless(x, UInt8(y))

for (nam, val, cat, typ, des) in
    ((:Cn, 0,  :NotAssignedChar,    Cat.Other,       "Other, Not assigned"),
     (:Lu, 1,  :UpperCase,          Cat.Upper,       "Letter, uppercase"),
     (:Ll, 2,  :LowerCase,          Cat.Letter,      "Letter, lowercase"),
     (:Lt, 3,  :TitleCase,          Cat.Upper,       "Letter, titlecase"),
     (:Lm, 4,  :ModifierLetter,     Cat.Letter,      "Letter, modifier"),
     (:Lo, 5,  :OtherLetter,        Cat.Letter,      "Letter, other"),
     (:Mn, 6,  :NonSpacingMark,     Cat.Mark,        "Mark, nonspacing"),
     (:Mc, 7,  :CombiningMark,      Cat.Mark,        "Mark, spacing combining"),
     (:Me, 8,  :EnclosingMark,      Cat.Mark,        "Mark, enclosing"),
     (:Nd, 9,  :DecimalDigit,       Cat.Number,      "Number, decimal digit"),
     (:Nl, 10, :NumericLetter,      Cat.Number,      "Number, letter"),
     (:No, 11, :OtherNumber,        Cat.Number,      "Number, other"),
     (:Pc, 12, :ConnectorPunct,     Cat.Punctuation, "Punctuation, connector"),
     (:Pd, 13, :DashPunct,          Cat.Punctuation, "Punctuation, dash"),
     (:Ps, 14, :OpenPunct,          Cat.Punctuation, "Punctuation, open"),
     (:Pe, 15, :ClosePunct,         Cat.Punctuation, "Punctuation, close"),
     (:Pi, 16, :BegQuotePunct,      Cat.Punctuation, "Punctuation, initial quote"),
     (:Pf, 17, :EndQuotePunct,      Cat.Punctuation, "Punctuation, final quote"),
     (:Po, 18, :OtherPunct,         Cat.Punctuation, "Punctuation, other"),
     (:Sm, 19, :MathSymbol,         Cat.Symbol,      "Symbol, math"),
     (:Sc, 20, :CurrencySymbol,     Cat.Symbol,      "Symbol, currency"),
     (:Sk, 21, :ModifierSymbol,     Cat.Symbol,      "Symbol, modifier"),
     (:So, 22, :OtherSymbol,        Cat.Symbol,      "Symbol, other"),
     (:Zs, 23, :SpaceSeparator,     Cat.Separator,   "Separator, space"),
     (:Zl, 24, :LineSeparator,      Cat.Separator,   "Separator, line"),
     (:Zp, 25, :ParagraphSeparator, Cat.Separator,   "Separator, paragraph"),
     (:Cc, 26, :ControlChar,        Cat.Other,       "Other, control"),
     (:Cf, 27, :FormatChar,         Cat.Other,       "Other, format"),
     (:Cs, 28, :SurrogateChar,      Cat.Other,       "Other, surrogate"),
     (:Co, 29, :PrivateUseChar,     Cat.Other,       "Other, private use"))
    @eval const global $nam = CharCode($val)
    @eval export $cat
    @eval abstract $cat <: $typ
    @eval @doc $(string("Unicode Category Code: ",des)) $nam
    @eval @doc $(string("Unicode Category Type: ",des)) $cat
end

const c2t = [NotAssignedChar, UpperCase, LowerCase, TitleCase, ModifierLetter, OtherLetter,
             NonSpacingMark, CombiningMark, EnclosingMark,
             DecimalDigit, NumericLetter, OtherNumber,
             ConnectorPunct, DashPunct, OpenPunct, ClosePunct,
             BegQuotePunct, EndQuotePunct, OtherPunct,
             MathSymbol, CurrencySymbol, ModifierSymbol, OtherSymbol,
             SpaceSeparator, LineSeparator, ParagraphSeparator,
	     ControlChar, FormatChar, SurrogateChar, PrivateUseChar]

############################################################################

"""
Return various Unicode properties for character
"""
function charprop end

charprop(::Type{CharType}, c) = c2t[Int(charprop(CharCode, c))+1]

is_assigned_char(c) = charprop(CharCode, c) != Cn

## libc character class predicates ##

islower(c::Char) = charprop(CharCode, c) == Ll

# true for Unicode upper and mixed case
isupper(c::Char) = (ccode = charprop(CharCode, c)) == Lu || ccode == Lt

isdigit(c::Char)  = ('0' <= c <= '9')
isalpha(c::Char)  = (Lu <= charprop(CharCode, c) <= Lo)
isnumber(c::Char) = (Nd <= charprop(CharCode, c) <= No)
isalnum(c::Char)  = (Lu <= (ccode = charprop(CharCode, c)) <= Lo) || (Nd <= ccode <= No)

# These are about 3 times slower, because the isa method
# is much slower than checking if an integer is within range (or two ranges)
# If that is sped up, then these, which are more readable, could replace the other forms.
#=
isalpha(c::Char)  = charprop(CharType, c) <: CatLetter
isnumber(c::Char) = charprop(CharType, c) <: CatNumber
isupper(c::Char)  = charprop(CharType, c) <: CatUpper
isalnum(c::Char)  = charprop(CharType, c) <: Union{CatLetter, CatNumber}
ispunct(c::Char)  = charprop(CharType, c) <: CatPunctuation
=#

# following C++ only control characters from the Latin-1 subset return true
iscntrl(c::Char) = (c <= Char(0x1f) || Char(0x7f) <= c <= Char(0x9f))

ispunct(c::Char) = (Pc <= charprop(CharCode, c) <= Po)

# \u85 is the Unicode Next Line (NEL) character
# the check for \ufffd allows for branch removal on ASCIIStrings
@inline isspace(c::Char) =
    (c == ' ' || '\t' <= c <='\r' || c == '\u85' ||
     ('\ua0' <= c && c != '\ufffd' && charprop(CharCode, c) == Zs))

isprint(c::Char) = (Lu <= charprop(CharCode, c) <= Zs)

# true in principle if a printer would use ink
isgraph(c::Char) = (Lu <= charprop(CharCode, c) <= So)

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
