# This file is a part of Julia. License is MIT: http://julialang.org/license

"""
A type of token in a date time string

each subtype must define

    tryparsenext(t::TokenType, str, i, [locale])

and

    format(io, t::TokenType, [locale])
"""
abstract AbstractDateToken

"""
Locale type for dates. By default `DateLocale{:english}()`
is used. This object is passed as the last argument to
`tryparsenext` and `format` defined for each `AbstractDateToken` type.
"""
immutable DateLocale{lang} end

"""
Information for parsing and formatting date time values.
"""
immutable DateFormat{D, T<:Tuple, L<:DateLocale, N}
    result_type::Type{D}
    tokens::T
    locale::L
    field_defaults::NTuple{N, Int}
    field_order::NTuple{N,Int}
end

include("io-util.jl")

### Token types ###

immutable DatePart{letter} <: AbstractDateToken
    width::Int
    fixed::Bool
end

### Numeric tokens

for c in "yYmdHMS"
    @eval begin
        @inline function tryparsenext(d::DatePart{$c}, str, i, len)
            tryparsenext_base10(str,i,len,d.width)
        end
    end
end

@inline function tryparsenext(d::DatePart{'s'}, str, i, len)
    tryparsenext_base10_frac(str,i,len,d.width)
end

@inline function tryparsenext(d::DatePart{'s'}, str, i, len)
    tryparsenext_base10_frac(str,i,len,d.width)
end

for (c, fn) in zip("YmdHMS", [year, month, day, hour, minute, second])
    @eval function format(io, d::DatePart{$c}, dt, locale)
        write(io, minwidth($fn(dt), d.width))
    end
end

# special cases

function format(io, d::DatePart{'y'}, dt, locale)
    write(io, rfixwidth(year(dt), d.width))
end

function format(io, d::DatePart{'s'}, dt, locale)
    write(io, string(millisecond(dt)/1000)[3:end])
end

function _show_content{c}(io::IO, d::DatePart{c})
    for i=1:d.width
        write(io, c)
    end
end

function Base.show{c}(io::IO, d::DatePart{c})
    write(io, "DatePart(")
    _show_content(io, d)
    write(io, ")")
end

### Text tokens

const english = Dict{String,Int}("january"=>1,"february"=>2,"march"=>3,"april"=>4,
                 "may"=>5,"june"=>6,"july"=>7,"august"=>8,"september"=>9,
                 "october"=>10,"november"=>11,"december"=>12)
const abbrenglish = Dict{String,Int}("jan"=>1,"feb"=>2,"mar"=>3,"apr"=>4,
                     "may"=>5,"jun"=>6,"jul"=>7,"aug"=>8,"sep"=>9,
                     "oct"=>10,"nov"=>11,"dec"=>12)
const MONTHTOVALUE = Dict{String,Dict{String,Int}}("english"=>english)
const MONTHTOVALUEABBR = Dict{String,Dict{String,Int}}("english"=>abbrenglish)


# fallback to tryparsenext methods that don't care about locale
@inline function tryparsenext(d::AbstractDateToken, str, i, len, locale)
    tryparsenext(d, str, i, len)
end

function month_from_abbr_name{l}(word, locale::DateLocale{l})
    get(MONTHTOVALUEABBR[string(l)], word, 0)
end

function month_from_name{l}(word, locale::DateLocale{l})
    get(MONTHTOVALUE[string(l)], word, 0)
end

function month_from_abbr_name(word, locale::DateLocale{:english})
    get(abbrenglish, word, 0)
end

function month_from_name(word, locale::DateLocale{:english})
    get(english, word, 0)
end

for (tok, fn) in zip("uU", [month_from_abbr_name, month_from_name])
    @eval @inline function tryparsenext(d::DatePart{$tok}, str, i, len, locale)
        R = Nullable{Int}
        c, ii = tryparsenext_word(str, i, len, locale, d.width)
        word = str[i:ii-1]
        x = $fn(lowercase(word), locale)
        ((x == 0 ? R() : R(x)), ii)
    end
end

# ignore day of week while parsing
@inline function tryparsenext(d::Union{DatePart{'e'}, DatePart{'E'}}, str, i, len, locale)
    tryparsenext_word(str, i, len, locale)
end

for (tok, fn) in zip("uU", [monthabbr, monthname])
    @eval function format{l}(io, d::DatePart{$tok}, dt, locale::DateLocale{l})
        write(io, $fn(dt; locale=string(l)))
    end
end

for (tok, dict) in zip("eE", [:VALUETODAYOFWEEKABBR, :VALUETODAYOFWEEK])
    @eval function format{l}(io, d::DatePart{$tok}, dt, locale::DateLocale{l})
        write(io, $dict[string(l)][dayofweek(dt)])
    end
end


### Delimiters

immutable Delim{T,length} <: AbstractDateToken
    d::T
end

Delim(d::Char) = Delim{Char,1}(d)
Delim(d::String) = Delim{String,length(d)}(d)

@inline function tryparsenext{N}(d::Delim{Char,N}, str, i::Int, len)
    R = Nullable{Int}
    for j=1:N
        i > len && return (R(), i)
        c, i = next(str, i)
        c != d.d && return (R(), i)
    end
    return R(0), i
end

@inline function tryparsenext{N}(d::Delim{String,N}, str, i::Int, len)
    R = Nullable{Int}
    i1=i
    i2=start(d.d)
    for j=1:N
        if i1 > len
            return R(), i1
        end
        c1, i1 = next(str, i1)
        c2, i2 = next(d.d, i2)
        if c1 != c2
            return R(), i1
        end
    end
    return R(0), i1
end

function format(io, d::Delim, str, i)
    write(io, d.d)
end

function _show_content{N}(io::IO, d::Delim{Char,N})
    if d.d in keys(SLOT_RULE)
        for i=1:N
            write(io, '\\')
            write(io, d.d)
        end
    else
        for i=1:N
            write(io, d.d)
        end
    end
end

function _show_content(io::IO, d::Delim)
    for c in d.d
        if c in keys(SLOT_RULE)
            write(io, '\\')
        end
        write(io, c)
    end
end

function Base.show(io::IO, d::Delim)
    write(io, "Delim(")
    _show_content(io, d)
    write(io, ")")
end

### DateFormat construction


abstract DayOfWeekToken # special addition to Period types

# mapping format specifiers to period types
const SLOT_RULE = Dict{Char, Type}(
    'y' => Year,
    'Y' => Year,
    'm' => Month,
    'u' => Month,
    'U' => Month,
    'e' => DayOfWeekToken,
    'E' => DayOfWeekToken,
    'd' => Day,
    'H' => Hour,
    'M' => Minute,
    'S' => Second,
    's' => Millisecond,
)

"""
    DateFormat(format::AbstractString, locale=:english, result_type::Type=DateTime) -> DateFormat

Construct a date formatting object that can be used for parsing date strings or
formatting a date object as a string. The following character codes can be used to construct the `format`
string:

| Code       | Matches   | Comment                                                      |
|:-----------|:----------|:-------------------------------------------------------------|
| `y`        | 1996, 96  | Returns year of 1996, 0096                                   |
| `Y`        | 1996, 96  | Returns year of 1996, 0096. Equivalent to `y`                |
| `m`        | 1, 01     | Matches 1 or 2-digit months                                  |
| `u`        | Jan       | Matches abbreviated months according to the `locale` keyword |
| `U`        | January   | Matches full month names according to the `locale` keyword   |
| `d`        | 1, 01     | Matches 1 or 2-digit days                                    |
| `H`        | 00        | Matches hours                                                |
| `M`        | 00        | Matches minutes                                              |
| `S`        | 00        | Matches seconds                                              |
| `s`        | .500      | Matches milliseconds                                         |
| `e`        | Mon, Tues | Matches abbreviated days of the week                         |
| `E`        | Monday    | Matches full name days of the week                           |
| `yyyymmdd` | 19960101  | Matches fixed-width year, month, and day                     |

Characters not listed above are normally treated as delimiters between date and time slots.
For example a `dt` string of "1996-01-15T00:00:00.0" would have a `format` string like
"y-m-dTH:M:S.s". If you need to use a code character as a delimiter you can escape it using
backslash. The date "1995y01m" would have the format "y\\ym\\m".

Creating a DateFormat object is expensive. Whenever possible, create it once and use it many times
or try the `dateformat""` string macro. Using this macro creates the DateFormat object once at
macro expansion time and reuses it later. see [`@dateformat_str`](@ref).

See [`DateTime`](@ref) and [`format`](@ref) for how to use a DateFormat object to parse and write Date strings
respectively.
"""
function DateFormat(f::AbstractString, locale=:english, T::Type=DateTime; default_fields=(1,1,1,0,0,0,0))
    tokens = AbstractDateToken[]
    localeobj = DateLocale{Symbol(locale)}()
    prev = ()
    prev_offset = 1

    # we store the indices of the token -> order in arguments to DateTime during
    # construction of the DateFormat. This saves a lot of time while parsing
    dateorder = (Year, Month, Day, Hour, Minute, Second, Millisecond)
    order = zeros(Int, length(default_fields))

    letters = String(collect(keys(Base.Dates.SLOT_RULE)))
    for m in eachmatch(Regex("(?<!\\\\)([\\Q$letters\\E])\\1*"), f)
        tran = replace(f[prev_offset:m.offset-1], r"\\(.)", s"\1")

        if !isempty(prev)
            letter, width = prev
            typ = SLOT_RULE[letter]

            if isempty(tran)
                push!(tokens, DatePart{letter}(width, true))
            else
                push!(tokens, DatePart{letter}(width, false))
            end

            idx = findfirst(dateorder, typ)
            if idx != 0
                order[idx] = length(tokens)
            end
        end

        if !isempty(tran)
            push!(tokens, Delim(length(tran) == 1 ? first(tran) : tran))
        end

        letter = f[m.offset]
        width = length(m.match)

        prev = (letter, width)
        prev_offset = m.offset + width
    end

    tran = replace(f[prev_offset:endof(f)], r"\\(.)", s"\1")

    if !isempty(prev)
        letter, width = prev
        typ = SLOT_RULE[letter]

        push!(tokens, DatePart{letter}(width, false))

        idx = findfirst(dateorder, typ)
        if idx != 0
            order[idx] = length(tokens)
        end
    end

    if !isempty(tran)
        push!(tokens, Delim(length(tran) == 1 ? first(tran) : tran))
    end

    return DateFormat(T, (tokens...), localeobj, default_fields, (order...))
end

function Base.show(io::IO, df::DateFormat)
    write(io, "dateformat\"")
    for t in df.tokens
        _show_content(io, t)
    end
    write(io, '"')
end

"""
    dateformat"Y-m-d H:M:S"

Create a [`DateFormat`](@ref) object. Similar to `DateFormat("Y-m-d H:M:S")`
but creates the DateFormat object once during macro expansion.

See [`DateFormat`](@ref) for details about format specifiers.
"""
macro dateformat_str(str)
    DateFormat(str)
end

# Standard formats
const ISODateTimeFormat = DateFormat("yyyy-mm-dd\\THH:MM:SS.s")
const ISODateFormat = DateFormat("yyyy-mm-dd", :english, Date)
const RFC1123Format = DateFormat("e, dd u yyyy HH:MM:SS")

### API
"""
    DateTime(dt::AbstractString, format::AbstractString; locale="english") -> DateTime

Construct a `DateTime` by parsing the `dt` date string following the pattern given in
the `format` string.

This method creates a `DateFormat` object each time it is called. If you are parsing many
date strings of the same format, consider creating a [`DateFormat`](@ref) object once and using
that as the second argument instead.
"""
DateTime(dt::AbstractString, format::AbstractString;locale::AbstractString="english") = DateTime(dt,DateFormat(format,locale))

"""
    DateTime(dt::AbstractString, df::DateFormat) -> DateTime

Construct a `DateTime` by parsing the `dt` date string following the pattern given in
the [`DateFormat`](@ref) object. Similar to
`DateTime(::AbstractString, ::AbstractString)` but more efficient when repeatedly parsing
similarly formatted date strings with a pre-created `DateFormat` object.
"""
DateTime(dt::AbstractString,df::DateFormat=ISODateTimeFormat) = tryfailparse(dt,df)

"""
    Date(dt::AbstractString, format::AbstractString; locale="english") -> Date

Construct a `Date` object by parsing a `dt` date string following the pattern given in the
`format` string. Follows the same conventions as
`DateTime(::AbstractString, ::AbstractString)`.
"""
Date(dt::AbstractString,format::AbstractString;locale=:english) = Date(dt,DateFormat(format,locale, Date))

"""
    Date(dt::AbstractString, df::DateFormat) -> Date

Parse a date from a date string `dt` using a `DateFormat` object `df`.
"""
Date(dt::AbstractString,df::DateFormat=ISODateFormat) = Date(tryfailparse(dt,df))

format(io, t, dt, locale) = format(io, t, dt)

function format(io, dt::TimeType, fmt::DateFormat)
    for t in fmt.tokens
        format(io, t, dt, fmt.locale)
    end
end

function format(dt::TimeType, fmt::DateFormat)
    sprint(io->format(io, dt, fmt))
end


"""
    format(dt::TimeType, format::AbstractString; locale="english") -> AbstractString

Construct a string by using a `TimeType` object and applying the provided `format`. The
following character codes can be used to construct the `format` string:

| Code       | Examples  | Comment                                                      |
|:-----------|:----------|:-------------------------------------------------------------|
| `y`        | 6         | Numeric year with a fixed width                              |
| `Y`        | 1996      | Numeric year with a minimum width                            |
| `m`        | 1, 12     | Numeric month with a minimum width                           |
| `u`        | Jan       | Month name shortened to 3-chars according to the `locale`    |
| `U`        | January   | Full month name according to the `locale` keyword            |
| `d`        | 1, 31     | Day of the month with a minimum width                        |
| `H`        | 0, 23     | Hour (24-hour clock) with a minimum width                    |
| `M`        | 0, 59     | Minute with a minimum width                                  |
| `S`        | 0, 59     | Second with a minimum width                                  |
| `s`        | 000, 500  | Millisecond with a minimum width of 3                        |
| `e`        | Mon, Tue  | Abbreviated days of the week                                 |
| `E`        | Monday    | Full day of week name                                        |

The number of sequential code characters indicate the width of the code. A format of
`yyyy-mm` specifies that the code `y` should have a width of four while `m` a width of two.
Codes that yield numeric digits have an associated mode: fixed-width or minimum-width.
The fixed-width mode left-pads the value with zeros when it is shorter than the specified
width and truncates the value when longer. Minimum-width mode works the same as fixed-width
except that it does not truncate values longer than the width.

When creating a `format` you can use any non-code characters as a separator. For example to
generate the string "1996-01-15T00:00:00" you could use `format`: "yyyy-mm-ddTHH:MM:SS".
Note that if you need to use a code character as a literal you can use the escape character
backslash. The string "1996y01m" can be produced with the format "yyyy\\ymm\\m".
"""
format(dt::TimeType,f::AbstractString;locale::AbstractString="english") = format(dt,DateFormat(f,locale))

# show

function Base.show(io::IO, dt::DateTime)
    if millisecond(dt) == 0
        format(io, dt, dateformat"YYYY-mm-dd\THH:MM:SS")
    else
        format(io, dt, dateformat"YYYY-mm-dd\THH:MM:SS.s")
    end
end

const simple_date = DateFormat("YYYY-mm-dd", :english, Date)
function Base.show(io::IO, dt::Date)
    format(io, dt, simple_date)
end

function Base.string(dt::TimeType)
    sprint(io->show(io, dt))
end

# vectorized
DateTime{T<:AbstractString}(Y::AbstractArray{T},format::AbstractString;locale::AbstractString="english") = DateTime(Y,DateFormat(format,locale))
function DateTime{T<:AbstractString}(Y::AbstractArray{T},df::DateFormat=ISODateTimeFormat)
    return reshape(DateTime[tryfailparse(y,df) for y in Y], size(Y))
end
Date{T<:AbstractString}(Y::AbstractArray{T},format::AbstractString;locale::AbstractString="english") = Date(Y,DateFormat(format,locale))
function Date{T<:AbstractString}(Y::AbstractArray{T},df::DateFormat=ISODateFormat)
    return reshape(Date[Date(tryfailparse(y,df)) for y in Y], size(Y))
end

format{T<:TimeType}(Y::AbstractArray{T},fmt::AbstractString;locale::AbstractString="english") = format(Y,DateFormat(fmt,locale))
function format(Y::AbstractArray{Date},df::DateFormat=ISODateFormat)
    return reshape([format(y,df) for y in Y], size(Y))
end
function format(Y::AbstractArray{DateTime},df::DateFormat=ISODateTimeFormat)
    return reshape([format(y,df) for y in Y], size(Y))
end
