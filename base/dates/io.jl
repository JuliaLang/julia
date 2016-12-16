# This file is a part of Julia. License is MIT: http://julialang.org/license

"""
A type of token in a date time string

each subtype must define

    tryparsenext(t::DatePart, str, i, len, [locale])

and

    format(io, t::TokenType, dt, [locale])
"""
abstract AbstractDateToken

# fallback to tryparsenext/format methods that don't care about locale
@inline function tryparsenext(d::AbstractDateToken, str, i, len, locale)
    tryparsenext(d, str, i, len)
end

@inline function format(io, d::AbstractDateToken, dt, locale)
    format(io, d, dt)
end

"""
Information for parsing and formatting date time values.
"""
immutable DateFormat{T<:Tuple}
    tokens::T
    locale::DateLocale
end

### Token types ###

immutable DatePart{letter} <: AbstractDateToken
    width::Int
    fixed::Bool
end

@inline min_width(d::DatePart) = d.fixed ? d.width : 1
@inline max_width(d::DatePart) = d.fixed ? d.width : 0

function _show_content{c}(io::IO, d::DatePart{c})
    for i = 1:d.width
        write(io, c)
    end
end

function Base.show{c}(io::IO, d::DatePart{c})
    write(io, "DatePart(")
    _show_content(io, d)
    write(io, ")")
end

### Parse tokens

for c in "yYmdHMS"
    @eval begin
        @inline function tryparsenext(d::DatePart{$c}, str, i, len)
            tryparsenext_base10(str, i, len, min_width(d), max_width(d))
        end
    end
end

for (tok, fn) in zip("uUeE", [monthabbr_to_value, monthname_to_value, dayabbr_to_value, dayname_to_value])
    @eval @inline function tryparsenext(d::DatePart{$tok}, str, i, len, locale)
        word, i = tryparsenext_word(str, i, len, locale, max_width(d))
        val = isnull(word) ? 0 : $fn(get(word), locale)
        return Nullable{Int64}(val == 0 ? nothing : val), i
    end
end

@inline function tryparsenext(d::DatePart{'s'}, str, i, len)
    ms, ii = tryparsenext_base10(str, i, len, min_width(d), max_width(d))
    if !isnull(ms)
        ms = Nullable{Int64}(get(ms) * 10 ^ (3 - (ii - i)))
    end
    return ms, ii
end


### Format tokens

for (c, fn) in zip("YmdHMS", [year, month, day, hour, minute, second])
    @eval function format(io, d::DatePart{$c}, dt)
        write(io, minwidth($fn(dt), d.width))
    end
end

for (tok, fn) in zip("uU", [monthabbr, monthname])
    @eval function format(io, d::DatePart{$tok}, dt, locale)
        write(io, $fn(month(dt), locale))
    end
end

for (tok, fn) in zip("eE", [dayabbr, dayname])
    @eval function format(io, ::DatePart{$tok}, dt,  locale)
        write(io, $fn(dayofweek(dt), locale))
    end
end

function format(io, d::DatePart{'y'}, dt)
    write(io, rfixwidth(year(dt), d.width))
end

function format(io, d::DatePart{'s'}, dt)
    write(io, string(millisecond(dt)/1000)[3:end])
end


### Delimiters

immutable Delim{T, length} <: AbstractDateToken
    d::T
end

Delim(c::Char, n) = Delim{Char, n}(c)
Delim(c::Char) = Delim(c,1)
Delim{S<:AbstractString}(str::S) = Delim{S,length(str)}(str)

@inline function tryparsenext{N}(d::Delim{Char,N}, str, i::Int, len)
    R = Nullable{Int64}
    for j=1:N
        i > len && return (R(), i)
        c, i = next(str, i)
        c != d.d && return (R(), i)
    end
    return R(0), i
end

@inline function tryparsenext{N}(d::Delim{String,N}, str, i::Int, len)
    R = Nullable{Int64}
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

function format(io, d::Delim, dt, locale)
    write(io, d.d)
end

function _show_content{N}(io::IO, d::Delim{Char, N})
    if d.d in keys(SLOT_RULE)
        for i=1:N
            write(io, '\\', d.d)
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

slot_order(::Type{Date}) = (Year, Month, Day)
slot_order(::Type{DateTime}) = (Year, Month, Day, Hour, Minute, Second, Millisecond)

slot_defaults(::Type{Date}) = map(Int64, (1, 1, 1))
slot_defaults(::Type{DateTime}) = map(Int64, (1, 1, 1, 0, 0, 0, 0))

slot_types{T<:TimeType}(::Type{T}) = typeof(slot_defaults(T))

"""
    DateFormat(format::AbstractString, locale="english", default_fields=(1,1,1,0,0,0,0)) -> DateFormat

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
function DateFormat(f::AbstractString, locale::DateLocale=ENGLISH)
    tokens = AbstractDateToken[]
    prev = ()
    prev_offset = 1

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
    end

    if !isempty(tran)
        push!(tokens, Delim(length(tran) == 1 ? first(tran) : tran))
    end

    return DateFormat((tokens...), locale)
end

function DateFormat(f::AbstractString, locale::AbstractString)
    DateFormat(f, LOCALES[locale])
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
const ISODateTimeFormat = DateFormat("yyyy-mm-dd\\THH:MM:SS.sss")
const ISODateFormat = DateFormat("yyyy-mm-dd")
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
DateTime(dt::AbstractString, format::AbstractString; locale=ENGLISH) = parse(DateTime,dt,DateFormat(format,locale))

"""
    DateTime(dt::AbstractString, df::DateFormat) -> DateTime

Construct a `DateTime` by parsing the `dt` date string following the pattern given in
the [`DateFormat`](@ref) object. Similar to
`DateTime(::AbstractString, ::AbstractString)` but more efficient when repeatedly parsing
similarly formatted date strings with a pre-created `DateFormat` object.
"""
DateTime(dt::AbstractString,df::DateFormat=ISODateTimeFormat) = parse(DateTime,dt,df)

"""
    Date(dt::AbstractString, format::AbstractString; locale="english") -> Date

Construct a `Date` object by parsing a `dt` date string following the pattern given in the
`format` string. Follows the same conventions as
`DateTime(::AbstractString, ::AbstractString)`.
"""
Date(dt::AbstractString,format::AbstractString;locale=ENGLISH) = parse(Date,dt,DateFormat(format,locale))

"""
    Date(dt::AbstractString, df::DateFormat) -> Date

Parse a date from a date string `dt` using a `DateFormat` object `df`.
"""
Date(dt::AbstractString,df::DateFormat=ISODateFormat) = parse(Date,dt,df)

function format(io::IO, dt::TimeType, fmt::DateFormat)
    for t in fmt.tokens
        format(io, t, dt, fmt.locale)
    end
end

function format(dt::TimeType, fmt::DateFormat)
    sprint(io -> format(io, dt, fmt))
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
format(dt::TimeType,f::AbstractString;locale=ENGLISH) = format(dt,DateFormat(f,locale))

# show

function Base.show(io::IO, dt::DateTime)
    df = if millisecond(dt) == 0
        dateformat"YYYY-mm-dd\THH:MM:SS"
    else
        dateformat"YYYY-mm-dd\THH:MM:SS.s"
    end
    format(io, dt, df)
end

function Base.show(io::IO, dt::Date)
    format(io, dt, dateformat"YYYY-mm-dd")
end

function Base.string(dt::TimeType)
    sprint(io -> show(io, dt))
end

# vectorized
DateTime{T<:AbstractString}(Y::AbstractArray{T},format::AbstractString;locale=ENGLISH) = DateTime(Y,DateFormat(format,locale))
function DateTime{T<:AbstractString}(Y::AbstractArray{T},df::DateFormat=ISODateTimeFormat)
    return reshape(DateTime[parse(DateTime,y,df) for y in Y], size(Y))
end
Date{T<:AbstractString}(Y::AbstractArray{T},format::AbstractString;locale=ENGLISH) = Date(Y,DateFormat(format,locale))
function Date{T<:AbstractString}(Y::AbstractArray{T},df::DateFormat=ISODateFormat)
    return reshape(Date[Date(parse(Date,y,df)) for y in Y], size(Y))
end

format{T<:TimeType}(Y::AbstractArray{T},fmt::AbstractString;locale=ENGLISH) = format(Y,DateFormat(fmt,locale))
function format(Y::AbstractArray{Date},df::DateFormat=ISODateFormat)
    return reshape([format(y,df) for y in Y], size(Y))
end
function format(Y::AbstractArray{DateTime},df::DateFormat=ISODateTimeFormat)
    return reshape([format(y,df) for y in Y], size(Y))
end
