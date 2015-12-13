# This file is a part of Julia. License is MIT: http://julialang.org/license

# Unicode properties, such as General Category
# Unix/C is* convenience functions (for now)

# whether codepoints are valid Unicode scalar values, i.e. 0-0xd7ff, 0xe000-0x10ffff
isvalid(::Type{Char}, ch::Unsigned) = !((ch - 0xd800 < 0x800) | (ch > 0x10ffff))
isvalid(::Type{Char}, ch::Integer)  = isvalid(Char, Unsigned(ch))
isvalid(::Type{Char}, ch::Char)     = isvalid(Char, UInt32(ch))

isvalid(ch::Char) = isvalid(Char, ch)

# Unicode General Category constants

"""Unicode character properties"""
abstract UnicodeProperty
"""Unicode character categories"""
abstract CharCategory   <: UnicodeProperty

"""Unicode letter character category"""
abstract CatLetter      <: CharCategory
"""Unicode Mark character category"""
abstract CatMark        <: CharCategory
"""Unicode Numeric character category"""
abstract CatNumber      <: CharCategory
"""Unicode Punctuation character category"""
abstract CatPunctuation <: CharCategory
"""Unicode Symbol character category"""
abstract CatSymbol      <: CharCategory
"""Unicode Separator character category"""
abstract CatSeparator   <: CharCategory
"""Unicode Other character category"""
abstract CatOther       <: CharCategory

"""Unicode uppercase & titlecase letters"""
abstract CatUpper       <: CatLetter

"""Unicode Character Category Code (0-29)"""
bitstype 8 CharCategoryCode

convert(::Type{CharCategoryCode}, x::Integer) = reinterpret(CharCategoryCode, x%UInt8)
convert{T<:Integer}(::Type{T}, x::CharCategoryCode) = convert(T, reinterpret(UInt8, x))
promote_rule{T<:Integer}(::Type{T}, ::Type{CharCategoryCode}) = T
isless(x::CharCategoryCode, y::CharCategoryCode) = isless(UInt32(x), UInt32(y))
isless(x::CharCategoryCode, y::Integer) = isless(UInt32(x), y)
isless(x::Integer, y::CharCategoryCode) = isless(x, UInt32(y))

for (nam, val, cat, typ, des) in
    ((:Cn, 0,  :NotAssignedChar,    CatOther,       "Other, Not assigned"),
     (:Lu, 1,  :UpperCase,          CatUpper,       "Letter, uppercase"),
     (:Ll, 2,  :LowerCase,          CatLetter,      "Letter, lowercase"),
     (:Lt, 3,  :TitleCase,          CatUpper,       "Letter, titlecase"),
     (:Lm, 4,  :ModifierLetter,     CatLetter,      "Letter, modifier"),
     (:Lo, 5,  :OtherLetter,        CatLetter,      "Letter, other"),
     (:Mn, 6,  :NonSpacingMark,     CatMark,        "Mark, nonspacing"),
     (:Mc, 7,  :CombiningMark,      CatMark,        "Mark, spacing combining"),
     (:Me, 8,  :EnclosingMark,      CatMark,        "Mark, enclosing"),
     (:Nd, 9,  :DecimalDigit,       CatNumber,      "Number, decimal digit"),
     (:Nl, 10, :NumericLetter,      CatNumber,      "Number, letter"),
     (:No, 11, :OtherNumber,        CatNumber,      "Number, other"),
     (:Pc, 12, :ConnectorPunct,     CatPunctuation, "Punctuation, connector"),
     (:Pd, 13, :DashPunct,          CatPunctuation, "Punctuation, dash"),
     (:Ps, 14, :OpenPunct,          CatPunctuation, "Punctuation, open"),
     (:Pe, 15, :ClosePunct,         CatPunctuation, "Punctuation, close"),
     (:Pi, 16, :BegQuotePunct,      CatPunctuation, "Punctuation, initial quote"),
     (:Pf, 17, :EndQuotePunct,      CatPunctuation, "Punctuation, final quote"),
     (:Po, 18, :OtherPunct,         CatPunctuation, "Punctuation, other"),
     (:Sm, 19, :MathSymbol,         CatSymbol,      "Symbol, math"),
     (:Sc, 20, :CurrencySymbol,     CatSymbol,      "Symbol, currency"),
     (:Sk, 21, :ModifierSymbol,     CatSymbol,      "Symbol, modifier"),
     (:So, 22, :OtherSymbol,        CatSymbol,      "Symbol, other"),
     (:Zs, 23, :SpaceSeparator,     CatSeparator,   "Separator, space"),
     (:Zl, 24, :LineSeparator,      CatSeparator,   "Separator, line"),
     (:Zp, 25, :ParagraphSeparator, CatSeparator,   "Separator, paragraph"),
     (:Cc, 26, :ControlChar,        CatOther,       "Other, control"),
     (:Cf, 27, :FormatChar,         CatOther,       "Other, format"),
     (:Cs, 28, :SurrogateChar,      CatOther,       "Other, surrogate"),
     (:Co, 29, :PrivateUseChar,     CatOther,       "Other, private use"))
    @eval const global $nam = CharCategoryCode($val)
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

charprop(::Type{CharCategory}, c) = c2t[Int(charprop(CharCategoryCode, c))+1]

is_assigned_char(c) = charprop(CharCategoryCode, c) != Cn

## libc character class predicates ##

islower(c::Char) = charprop(CharCategoryCode, c) == Ll

# true for Unicode upper and mixed case
isupper(c::Char) = (ccode = charprop(CharCategoryCode, c)) == Lu || ccode == Lt

isdigit(c::Char)  = ('0' <= c <= '9')
isalpha(c::Char)  = (Lu <= charprop(CharCategoryCode, c) <= Lo)
isnumber(c::Char) = (Nd <= charprop(CharCategoryCode, c) <= No)
isalnum(c::Char)  = (Lu <= (ccode = charprop(CharCategoryCode, c)) <= Lo) || (Nd <= ccode <= No)

# These are about 3 times slower, because the isa method
# is much slower than checking if an integer is within range (or two ranges)
# If that is sped up, then these, which are more readable, could replace the other forms.
#=
isalpha(c::Char)  = charprop(CharCategory, c) <: CatLetter
isnumber(c::Char) = charprop(CharCategory, c) <: CatNumber
isupper(c::Char)  = charprop(CharCategory, c) <: CatUpper
isalnum(c::Char)  = charprop(CharCategory, c) <: Union{CatLetter, CatNumber}
ispunct(c::Char)  = charprop(CharCategory, c) <: CatPunctuation
=#

# following C++ only control characters from the Latin-1 subset return true
iscntrl(c::Char) = (c <= Char(0x1f) || Char(0x7f) <= c <= Char(0x9f))

ispunct(c::Char) = (Pc <= charprop(CharCategoryCode, c) <= Po)

# \u85 is the Unicode Next Line (NEL) character
# the check for \ufffd allows for branch removal on ASCIIStrings
@inline isspace(c::Char) =
    (c == ' ' || '\t' <= c <='\r' || c == '\u85' ||
     ('\ua0' <= c && c != '\ufffd' && charprop(CharCategoryCode, c) == Zs))

isprint(c::Char) = (Lu <= charprop(CharCategoryCode, c) <= Zs)

# true in principle if a printer would use ink
isgraph(c::Char) = (Lu <= charprop(CharCategoryCode, c) <= So)

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
