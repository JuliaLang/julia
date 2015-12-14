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
export CategoryType, CategoryCode

"""Unicode character category type"""
abstract CategoryType <: Unicode.Property

"""Unicode 'Letter' character category"""
abstract Letter      <: CategoryType
"""Unicode 'Mark' character category"""
abstract Mark        <: CategoryType
"""Unicode 'Number' character category"""
abstract Number      <: CategoryType
"""Unicode 'Punctuation' character category"""
abstract Punctuation <: CategoryType
"""Unicode 'Symbol' character category"""
abstract Symbol      <: CategoryType
"""Unicode 'Separator' character category"""
abstract Separator   <: CategoryType
"""Unicode 'Other' character category"""
abstract Other       <: CategoryType

"""Unicode uppercase & titlecase letters"""
abstract Upper       <: Letter

"""Unicode alphabetic and numeric"""
typealias AlphaNumeric Union{Letter, Number}

"""Unicode character category code (0-29)"""
bitstype 8 CategoryCode

Base.convert(::Type{CategoryCode}, x::Integer) = reinterpret(CategoryCode, x%UInt8)
Base.convert{T<:Integer}(::Type{T}, x::CategoryCode) = convert(T, reinterpret(UInt8, x))
Base.promote_rule{T<:Integer}(::Type{T}, ::Type{CategoryCode}) = T
Base.isless(x::CategoryCode, y::CategoryCode) = isless(UInt8(x), UInt8(y))
Base.isless(x::CategoryCode, y::Integer)  = isless(UInt8(x), y)
Base.isless(x::Integer, y::CategoryCode)  = isless(x, UInt8(y))

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
    @eval const global $nam = CategoryCode($val)
    @eval abstract $cat <: $typ
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

charprop(::Type{CategoryType}, c) = c2t[Int(charprop(CategoryCode, c))+1]

end # module Cat
importall .Category

############################################################################

is_assigned_char(c) = charprop(CategoryCode, c) != Category.Cn

## libc character class predicates ##

islower(c::Char) = charprop(CategoryCode, c) == Category.Ll

# true for Unicode upper and mixed case
isupper(c::Char) = (ccode = charprop(CategoryCode, c)) == Category.Lu || ccode == Category.Lt

isdigit(c::Char)  = ('0' <= c <= '9')
isalpha(c::Char)  = (Category.Lu <= charprop(CategoryCode, c) <= Category.Lo)
isnumber(c::Char) = (Category.Nd <= charprop(CategoryCode, c) <= Category.No)
isalnum(c::Char)  = ((Category.Lu <= (ccode = charprop(CategoryCode, c)) <= Category.Lo) ||
                     (Category.Nd <= ccode <= Category.No))

# These are about 3 times slower, because the isa method
# is much slower than checking if an integer is within range (or two ranges)
# If that is sped up, then these, which are more readable, could replace the other forms.
#=
isalpha(c::Char)  = charprop(CategoryType, c) <: Category.Letter
isnumber(c::Char) = charprop(CategoryType, c) <: Category.Number
isupper(c::Char)  = charprop(CategoryType, c) <: Category.Upper
isalnum(c::Char)  = charprop(CategoryType, c) <: Category.AlphaNumeric
ispunct(c::Char)  = charprop(CategoryType, c) <: Category.Punctuation
=#

# following C++ only control characters from the Latin-1 subset return true
iscntrl(c::Char) = (c <= Char(0x1f) || Char(0x7f) <= c <= Char(0x9f))

ispunct(c::Char) = (Category.Pc <= charprop(CategoryCode, c) <= Category.Po)

# \u85 is the Unicode Next Line (NEL) character
# the check for \ufffd allows for branch removal on ASCIIStrings
@inline isspace(c::Char) =
    (c == ' ' || '\t' <= c <='\r' || c == '\u85' ||
     ('\ua0' <= c && c != '\ufffd' && charprop(CategoryCode, c) == Category.Zs))

isprint(c::Char) = (Category.Lu <= charprop(CategoryCode, c) <= Category.Zs)

# true in principle if a printer would use ink
isgraph(c::Char) = (Category.Lu <= charprop(CategoryCode, c) <= Category.So)

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
