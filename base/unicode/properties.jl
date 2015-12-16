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

"""Unicode character category code (0-29)"""
bitstype 8 Code

Base.convert(::Type{Code}, x::Integer) = reinterpret(Code, x%UInt8)
Base.convert{T<:Integer}(::Type{T}, x::Code) = convert(T, reinterpret(UInt8, x))
Base.promote_rule{T<:Integer}(::Type{T}, ::Type{Code}) = T
Base.isless(x::Code, y::Code) = isless(UInt8(x), UInt8(y))
Base.isless(x::Code, y::Integer)  = isless(UInt8(x), y)
Base.isless(x::Integer, y::Code)  = isless(x, UInt8(y))

"""Unicode character category mask"""
bitstype 32 Mask

Base.convert(::Type{Mask}, x::Integer) = reinterpret(Mask, x%UInt32)
Base.convert{T<:Integer}(::Type{T}, x::Mask) = convert(T, reinterpret(UInt32, x))
Base.promote_rule{T<:Integer}(::Type{T}, ::Type{Mask}) = T

Base.convert(::Type{Mask}, c::Code) = Mask(1<<Int(c))

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
    @eval const global $cat = $(Code(val))
    @eval @doc $(string("Unicode Category Code: ",des)) $nam
    @eval @doc $(string("Unicode Category Code: ",des)) $cat
end

Base.in(c::Code, m::Mask) = ((1<<Int(c)) & m) != 0
Base.in(x::Code, y::Code) = x == y

Base.|(x::Code, y::Code) = Mask((1<<Int(x)) | (1<<Int(y)))
Base.|(c::Code, m::Mask) = Mask((1<<Int(c)) | m)
Base.|(m::Mask, c::Code) = (c | m)
Base.|(x::Mask, y::Mask) = Mask(UInt32(x) | UInt32(y))

Base.|(x::Integer, y::Mask) = x | Int(y)
Base.|(x::Mask, y::Integer) = Int(x) | y
Base.&(x::Integer, y::Mask) = x & Int(y)
Base.&(x::Mask, y::Integer) = Int(x) & y

@eval const global Letter      = $(Mask(Lu | Ll | Lt | Lm | Lo))
@doc """Unicode Major Category: Letter (Lu, Ll, Lt, Lm, Lo)""" Letter

@eval const global Mark        = $(Mask(Mn | Mc | Me))
@doc """Unicode Major Category: Mark (Mn, Mc, Me)""" Mark

@eval const global Number      = $(Mask(Nd | Nl | No))
@doc """Unicode Major Category: Number (Nd, Nl, No)""" Number

@eval const global Symbol      = $(Mask(Sm | Sc | Sk | So))
@doc """Unicode Major Category: Symbol (Sm, Sc, Sk, So)""" Symbol

@eval const global Other       = $(Mask(Cn | Cc | Cf | Cs | Co))
@doc """Unicode Major Category: Other (Cn, Cc, Cf, Cs, Co)""" Other

@eval const global Punctuation = $(Mask(Pc | Pd | Ps | Pe | Pi | Pf | Po))
@doc """Unicode Major Category: Punctuation (Pc, Pd, Ps, Pe, Pi, Pf, Po)""" Punctuation

@eval const global Separator   = $(Mask(Zs | Zl | Zp))
@doc """Unicode Major Category: Separator: (Zs, Zl, Zp)""" Separator

@eval const global Lower  = $(Mask(Ll))
@doc """Unicode Category: Lower = LowerCase""" Lower

@eval const global Upper  = $(Mask(Lu | Lt))
@doc """Unicode Combined Categories: Upper = UpperCase | TitleCase""" Upper

@eval const global AlphaNumeric = Letter | Number
@doc """Unicode Combined Categories: AlphaNumberic = Letter | Number""" AlphaNumeric

let mask = 0 ; for i = Int(Lu):Int(So) ; mask |= (1<<i) ; end
    @eval const global Graph = $(Mask(mask))
    @doc """Unicode Combined Categories: Graph (true if printer would use ink)""" Graph
    @eval const global Print = $(Mask(mask) | Zs)
    @doc """Unicode Combined Categories: Print""" Print
end

end # module Cat
importall .Category

############################################################################

is_assigned_char(c) = charprop(Category.Code, c) != Category.Cn

islower(c::Char)    = charprop(Category.Code, c) == Category.Ll

# true for Unicode upper and mixed case
isupper(c::Char)  = charprop(Category.Code, c) in Category.Upper
isalpha(c::Char)  = charprop(Category.Code, c) in Category.Letter
isnumber(c::Char) = charprop(Category.Code, c) in Category.Number
isalnum(c::Char)  = charprop(Category.Code, c) in Category.AlphaNumeric
ispunct(c::Char)  = charprop(Category.Code, c) in Category.Punctuation
isprint(c::Char)  = charprop(Category.Code, c) in Category.Print
# true in principle if a printer would use ink
isgraph(c::Char)  = charprop(Category.Code, c) in Category.Graph

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
