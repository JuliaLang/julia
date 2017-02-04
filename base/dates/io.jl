# This file is a part of Julia. License is MIT: http://julialang.org/license

"""
    AbstractDateToken

A token used in parsing or formatting a date time string. Each subtype must
define the tryparsenext and format methods.

"""
abstract AbstractDateToken

"""
    tryparsenext(tok::AbstractDateToken, str::String, i::Int, len::Int, locale::DateLocale)

`tryparsenext` parses for the `tok` token in `str` starting at index `i`.
`len` is the length of the string.  parsing can be optionally based on the
`locale`. If a `tryparsenext` method does not need a locale, it can leave
the argument out in the method definition.

Returns a tuple of 2 elements `(res, idx)`, where:

* `res` is a `Nullable{T}` - the result of the parsing, null if parsing failed.
* `idx` is an `Int` - if parsing failed, the index at which it failed; if
   parsing succeeded, `idx` is the index _after_ the index at which parsing ended.
"""
function tryparsenext end

"""
    format(io::IO, tok::AbstractDateToken, dt::TimeType, locale)

Format the `tok` token from `dt` and write it to `io`. The formatting can
be based on `locale`.

All subtypes of `AbstractDateToken` must define this method in order
to be able to print a Date / DateTime object according to a `DateFormat`
containing that token.
"""
function format end

# fallback to tryparsenext/format methods that don't care about locale
@inline function tryparsenext(d::AbstractDateToken, str, i, len, locale)
    tryparsenext(d, str, i, len)
end

function Base.string(t::Time)
    h, mi, s = hour(t), minute(t), second(t)
    hh = lpad(h, 2, "0")
    mii = lpad(mi, 2, "0")
    ss = lpad(s, 2, "0")
    nss = tons(Millisecond(t)) + tons(Microsecond(t)) + tons(Nanosecond(t))
    ns = nss == 0 ? "" : rstrip(@sprintf("%.9f", nss / 1e+9)[2:end], '0')
    return "$hh:$mii:$ss$ns"
end

Base.show(io::IO, x::Time) = print(io, string(x))

@inline function format(io, d::AbstractDateToken, dt, locale)
    format(io, d, dt)
end

# Information for parsing and formatting date time values.
immutable DateFormat{S, T<:Tuple}
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
        if val == 0
            return Nullable{Int64}(), i
        else
            return Nullable{Int64}(val), i
        end
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
        write(io, dec($fn(dt), d.width))
    end
end

for (tok, fn) in zip("uU", [monthabbr, monthname])
    @eval function format(io, d::DatePart{$tok}, dt, locale)
        write(io, $fn(month(dt), locale))
    end
end

for (tok, fn) in zip("eE", [dayabbr, dayname])
    @eval function format(io, ::DatePart{$tok}, dt, locale)
        write(io, $fn(dayofweek(dt), locale))
    end
end

@inline function format(io, d::DatePart{'y'}, dt)
    y = year(dt)
    n = d.width

    # the last n digits of y
    # will be 0 padded if y has less than n digits
    str = dec(y, n)
    l = endof(str)
    if l == n
        # fast path
        write(io, str)
    else
        write(io, SubString(str, l - (n - 1), l))
    end
end

function format(io, d::DatePart{'s'}, dt)
    ms = millisecond(dt)
    if ms == 0
        write(io, '0')
    elseif ms % 100 == 0
        write(io, dec(div(ms, 100), 1))
    elseif ms % 10 == 0
        write(io, dec(div(ms, 10), 2))
    else
        write(io, dec(ms, 3))
    end
end

### Delimiters

immutable Delim{T, length} <: AbstractDateToken
    d::T
end

Delim(d::Char) = Delim{Char, 1}(d)
Delim(d::String) = Delim{String, length(d)}(d)

@inline function tryparsenext{N}(d::Delim{Char, N}, str, i::Int, len)
    R = Nullable{Int64}
    for j=1:N
        i > len && return (R(), i)
        c, i = next(str, i)
        c != d.d && return (R(), i)
    end
    return R(0), i
end

@inline function tryparsenext{N}(d::Delim{String, N}, str, i::Int, len)
    R = Nullable{Int64}
    i1 = i
    i2 = start(d.d)
    for j = 1:N
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

@inline function format(io, d::Delim, dt, locale)
    write(io, d.d)
end

function _show_content{N}(io::IO, d::Delim{Char, N})
    if d.d in keys(SLOT_RULE)
        for i = 1:N
            write(io, '\\', d.d)
        end
    else
        for i = 1:N
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
    DateFormat(format::AbstractString, locale="english") -> DateFormat

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
        tran = replace(f[prev_offset:m.offset - 1], r"\\(.)", s"\1")

        if !isempty(prev)
            letter, width = prev
            typ = SLOT_RULE[letter]

            push!(tokens, DatePart{letter}(width, isempty(tran)))
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

    tokens_tuple = (tokens...)
    return DateFormat{Symbol(f),typeof(tokens_tuple)}(tokens_tuple, locale)
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
const ISODateTimeFormat = DateFormat("yyyy-mm-dd\\THH:MM:SS.s")
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
DateTime(dt::AbstractString, format::AbstractString;
    locale::Union{DateLocale, String}=ENGLISH) = parse(DateTime, dt, DateFormat(format, locale))

"""
    DateTime(dt::AbstractString, df::DateFormat) -> DateTime

Construct a `DateTime` by parsing the `dt` date string following the pattern given in
the [`DateFormat`](@ref) object. Similar to
`DateTime(::AbstractString, ::AbstractString)` but more efficient when repeatedly parsing
similarly formatted date strings with a pre-created `DateFormat` object.
"""
DateTime(dt::AbstractString, df::DateFormat=ISODateTimeFormat) = parse(DateTime, dt, df)

"""
    Date(dt::AbstractString, format::AbstractString; locale="english") -> Date

Construct a `Date` object by parsing a `dt` date string following the pattern given in the
`format` string. Follows the same conventions as
`DateTime(::AbstractString, ::AbstractString)`.
"""
Date(dt::AbstractString, format::AbstractString;
    locale::Union{DateLocale, String}=ENGLISH) = parse(Date, dt, DateFormat(format, locale))

"""
    Date(dt::AbstractString, df::DateFormat) -> Date

Parse a date from a date string `dt` using a `DateFormat` object `df`.
"""
Date(dt::AbstractString,df::DateFormat=ISODateFormat) = parse(Date, dt, df)

@generated function format{S, T}(io::IO, dt::TimeType, fmt::DateFormat{S, T})
    N = nfields(T)
    quote
        ts = fmt.tokens
        loc = fmt.locale
        Base.@nexprs $N i -> format(io, ts[i], dt, loc)
    end
end

function format(dt::TimeType, fmt::DateFormat, bufsize=12)
    # preallocate to reduce resizing
    io = IOBuffer(Vector{UInt8}(bufsize), true, true)
    format(io, dt, fmt)
    String(io.data[1:io.ptr - 1])
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
format(dt::TimeType, f::AbstractString;
    locale::Union{DateLocale, String}=ENGLISH) = format(dt, DateFormat(f, locale))

# show

function Base.show(io::IO, dt::DateTime)
    if millisecond(dt) == 0
        format(io, dt, dateformat"YYYY-mm-dd\THH:MM:SS")
    else
        format(io, dt, dateformat"YYYY-mm-dd\THH:MM:SS.s")
    end
end

function Base.show(io::IO, dt::Date)
    format(io, dt, dateformat"YYYY-mm-dd")
end

function Base.string(dt::DateTime)
    if millisecond(dt) == 0
        format(dt, dateformat"YYYY-mm-dd\THH:MM:SS", 24)
    else
        format(dt, dateformat"YYYY-mm-dd\THH:MM:SS.s", 26)
    end
end

function Base.string(dt::Date)
    # don't use format - bypassing IOBuffer creation
    # saves a bit of time here.
    y,m,d = yearmonthday(value(dt))
    yy = y < 0 ? @sprintf("%05i", y) : lpad(y, 4, "0")
    mm = lpad(m, 2, "0")
    dd = lpad(d, 2, "0")
    return "$yy-$mm-$dd"
end

# vectorized
DateTime{T<:AbstractString}(Y::AbstractArray{T}, format::AbstractString;
    locale::Union{DateLocale, String}=ENGLISH) = DateTime(Y, DateFormat(format, locale))
function DateTime{T<:AbstractString}(Y::AbstractArray{T}, df::DateFormat=ISODateTimeFormat)
    return reshape(DateTime[parse(DateTime, y, df) for y in Y], size(Y))
end
Date{T<:AbstractString}(Y::AbstractArray{T}, format::AbstractString;
    locale::Union{DateLocale, String}=ENGLISH) = Date(Y, DateFormat(format, locale))
function Date{T<:AbstractString}(Y::AbstractArray{T}, df::DateFormat=ISODateFormat)
    return reshape(Date[Date(parse(Date, y, df)) for y in Y], size(Y))
end

format{T<:TimeType}(Y::AbstractArray{T}, fmt::AbstractString;
    locale::Union{DateLocale, String}=ENGLISH) = format(Y, DateFormat(fmt, locale))
function format(Y::AbstractArray{Date}, df::DateFormat=ISODateFormat)
    return reshape([format(y, df) for y in Y], size(Y))
end
function format(Y::AbstractArray{DateTime}, df::DateFormat=ISODateTimeFormat)
    return reshape([format(y, df) for y in Y], size(Y))
end
