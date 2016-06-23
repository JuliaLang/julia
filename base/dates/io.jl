# This file is a part of Julia. License is MIT: http://julialang.org/license

# TODO: optimize this
function Base.string(dt::DateTime)
    y,m,d = yearmonthday(days(dt))
    h,mi,s = hour(dt),minute(dt),second(dt)
    yy = y < 0 ? @sprintf("%05i",y) : lpad(y,4,"0")
    mm = lpad(m,2,"0")
    dd = lpad(d,2,"0")
    hh = lpad(h,2,"0")
    mii = lpad(mi,2,"0")
    ss = lpad(s,2,"0")
    ms = millisecond(dt) == 0 ? "" : string(millisecond(dt)/1000.0)[2:end]
    return "$yy-$mm-$(dd)T$hh:$mii:$ss$(ms)"
end
Base.show(io::IO,x::DateTime) = print(io,string(x))
function Base.string(dt::Date)
    y,m,d = yearmonthday(value(dt))
    yy = y < 0 ? @sprintf("%05i",y) : lpad(y,4,"0")
    mm = lpad(m,2,"0")
    dd = lpad(d,2,"0")
    return "$yy-$mm-$dd"
end
Base.show(io::IO,x::Date) = print(io,string(x))

### Parsing
const english = Dict{String,Int}("january"=>1,"february"=>2,"march"=>3,"april"=>4,
                 "may"=>5,"june"=>6,"july"=>7,"august"=>8,"september"=>9,
                 "october"=>10,"november"=>11,"december"=>12)
const abbrenglish = Dict{String,Int}("jan"=>1,"feb"=>2,"mar"=>3,"apr"=>4,
                     "may"=>5,"jun"=>6,"jul"=>7,"aug"=>8,"sep"=>9,
                     "oct"=>10,"nov"=>11,"dec"=>12)
const MONTHTOVALUE = Dict{String,Dict{String,Int}}("english"=>english)
const MONTHTOVALUEABBR = Dict{String,Dict{String,Int}}("english"=>abbrenglish)

# Date/DateTime Parsing
abstract Slot{T<:Any}

immutable DelimitedSlot{T<:Any} <: Slot{T}
    parser::Type{T}
    letter::Char
    width::Int
    prefix::AbstractString
    suffix::Union{Regex,AbstractString}
end

immutable FixedWidthSlot{T<:Any} <: Slot{T}
    parser::Type{T}
    letter::Char
    width::Int
    prefix::AbstractString
end

immutable DateFormat
    slots::Array{Slot,1}
    locale::AbstractString
end

abstract DayOfWeekSlot

# Slot rules translate letters into types. Note that
# list of rules can be extended.
immutable SlotRule
    rules::Array{Type}
end
const SLOT_RULE = SlotRule(Array{Type}(256))

getindex(collection::SlotRule, key::Char) = collection.rules[Int(key)]
setindex!(collection::SlotRule, value::Type, key::Char) = collection.rules[Int(key)] = value
keys(c::SlotRule) = map(Char, filter(el -> isdefined(c.rules, el) && c.rules[el] != Void, eachindex(c.rules)))

SLOT_RULE['y'] = Year
SLOT_RULE['m'] = Month
SLOT_RULE['u'] = Month
SLOT_RULE['U'] = Month
SLOT_RULE['e'] = DayOfWeekSlot
SLOT_RULE['E'] = DayOfWeekSlot
SLOT_RULE['d'] = Day
SLOT_RULE['H'] = Hour
SLOT_RULE['M'] = Minute
SLOT_RULE['S'] = Second
SLOT_RULE['s'] = Millisecond

duplicates(slots) = any(map(x->count(y->x.parser==y.parser,slots),slots) .> 1)

"""
    DateFormat(format::AbstractString, locale::AbstractString="english") -> DateFormat

Construct a date formatting object that can be used for parsing date strings or
formatting a date object as a string. For details on the syntax for `format` see
[`DateTime(::AbstractString, ::AbstractString)`](:ref:`parsing <man-date-parsing>`) and
[`format`](:ref:`formatting <man-date-formatting>`).
"""
function DateFormat(f::AbstractString, locale::AbstractString="english")
    slots = Slot[]
    prefix = ""
    params = ()
    last_offset = 1

    letters = join(keys(SLOT_RULE), "")
    for m in eachmatch(Regex("(?<!\\\\)([\\Q$letters\\E])\\1*"), f)
        letter = f[m.offset]
        typ = SLOT_RULE[letter]
        width = length(m.match)

        suffix = replace(f[last_offset:m.offset-1], r"\\(.)", s"\1")
        if !isempty(params)
            slot = if suffix == ""
                FixedWidthSlot(params..., prefix)
            else
                DelimitedSlot(params..., prefix, suffix)
            end
            push!(slots,slot)
        end

        params = (typ,letter,width)
        last_offset = m.offset + width
        prefix = suffix
    end

    suffix = last_offset > endof(f) ? r"(?=\s|$)" : replace(f[last_offset:end], r"\\(.)", s"\1")
    if !isempty(params)
        slot = if suffix == ""
            FixedWidthSlot(params..., prefix)
        else
            DelimitedSlot(params..., prefix, suffix)
        end
        push!(slots,slot)
    end

    duplicates(slots) && throw(ArgumentError("Two separate periods of the same type detected"))
    return DateFormat(slots,locale)
end

const SLOTERROR = ArgumentError("Non-digit character encountered")
slotparse(slot,str,locale) = isdigit(str) ? slot.parser(str) : throw(SLOTERROR)
function slotparse(slot::Slot{Month},str,locale)
    s = if slot.letter == 'm'
        isdigit(str) ? str : throw(SLOTERROR)
    elseif slot.letter == 'u'
        MONTHTOVALUEABBR[locale][lowercase(str)]
    else
        MONTHTOVALUE[locale][lowercase(str)]
    end
    return Month(s)
end
function slotparse(slot::Slot{Millisecond},str,locale)
    isdigit(str) || throw(SLOTERROR)
    return slot.parser(endswith(slot.prefix, '.') ? rpad(str, 3, '0') : str)
end
slotparse(slot::Slot{DayOfWeekSlot},str,locale) = nothing

function getslot(str,slot::DelimitedSlot,locale,cursor)
    index = search(str, slot.suffix, nextind(str, cursor))
    if first(index) == 0  # we didn't find the next delimiter
        s = str[cursor:end]
        cursor = endof(str) + 1
    else
        s = str[cursor:(first(index) - 1)]
        cursor = nextind(str, last(index))
    end
    return cursor, slotparse(slot, strip(s),locale)
end
function getslot(str,slot,locale,cursor)
    cursor + slot.width, slotparse(slot, strip(str[cursor:(cursor + slot.width - 1)]), locale)
end

function parse(str::AbstractString,df::DateFormat)
    str = strip(str)
    startswith(str, df.slots[1].prefix) && (str = replace(str, df.slots[1].prefix, "", 1))
    isempty(str) && throw(ArgumentError("Cannot parse empty format string"))
    if isa(df.slots[1], DelimitedSlot) && first(search(str, df.slots[1].suffix)) == 0
        throw(ArgumentError("Delimiter mismatch. Couldn't find first delimiter, \"$(df.slots[1].suffix)\", in date string"))
    end
    periods = Period[]
    extra = Any[]  # Supports custom slot types such as TimeZone
    cursor = 1
    for slot in df.slots
        cursor, pe = getslot(str, slot, df.locale, cursor)
        pe !== nothing && (isa(pe, Period) ? push!(periods, pe) : push!(extra, pe))
        cursor > endof(str) && break
    end
    sort!(periods,rev=true,lt=periodisless)
    return isempty(extra) ? periods : vcat(periods, extra)
end

function slotformat(slot,dt)
    s = string(value(slot.parser(dt)))
    return slot.width > 1 ? lpad(s, slot.width, '0')[(end - slot.width + 1):end] : s
end
slotformat(slot,dt,locale) = slotformat(slot,dt)
function slotformat(slot::Slot{Month},dt,locale)
    s = if slot.letter == 'm'
        slotformat(slot,dt)
    elseif slot.letter == 'u'
        VALUETOMONTHABBR[locale][month(dt)]
    else
        VALUETOMONTH[locale][month(dt)]
    end
    return rpad(s, slot.width, ' ')
end
function slotformat(slot::Slot{DayOfWeekSlot},dt,locale)
    s = if slot.letter == 'e'
        VALUETODAYOFWEEKABBR[locale][dayofweek(dt)]
    else # == 'E'
        VALUETODAYOFWEEK[locale][dayofweek(dt)]
    end
    return rpad(s, slot.width, ' ')
end
function slotformat(slot::Slot{Millisecond},dt,locale)
    if endswith(slot.prefix, '.')
        s = string(millisecond(dt) / 1000)[3:end]
        return slot.width > 1 ? rpad(s, slot.width, '0')[1:slot.width] : s
    else
        s = string(millisecond(dt))
        return slot.width > 1 ? lpad(s, slot.width, '0')[(end - slot.width + 1):end] : s
    end
end

function format(dt::TimeType,df::DateFormat)
    f = first(df.slots).prefix
    for slot in df.slots
        f *= slotformat(slot,dt,df.locale)
        if isa(slot, DelimitedSlot)
            f *= isa(slot.suffix, AbstractString) ? slot.suffix : ""
        end
    end
    return f
end

# UI
const ISODateTimeFormat = DateFormat("yyyy-mm-dd\\THH:MM:SS.s")
const ISODateFormat = DateFormat("yyyy-mm-dd")
const RFC1123Format = DateFormat("e, dd u yyyy HH:MM:SS")

"""
    DateTime(dt::AbstractString, format::AbstractString; locale="english") -> DateTime

Construct a `DateTime` by parsing the `dt` date string following the pattern given in
the `format` string. The following character codes can be used to construct the `format`
string:

| Code       | Matches   | Comment                                                      |
|:-----------|:----------|:-------------------------------------------------------------|
| `y`        | 1996, 96  | Returns year of 1996, 0096                                   |
| `m`        | 1, 01     | Matches numeric month                                        |
| `u`        | Jan       | Matches abbreviated months according to the `locale` keyword |
| `U`        | January   | Matches full month names according to the `locale` keyword   |
| `d`        | 1, 01     | Matches the day of the month                                 |
| `H`        | 00        | Matches hours                                                |
| `M`        | 00        | Matches minutes                                              |
| `S`        | 00        | Matches seconds                                              |
| `s`        | 500       | Matches milliseconds                                         |
| `e`        | Mon, Tues | Matches abbreviated days of the week                         |
| `E`        | Monday    | Matches full name days of the week                           |

Any non-code characters are treated as delimiters between date and time slots. For example:
```jldoctest
julia> DateTime("1996-01-15T00:00:00.0", "y-m-dTH:M:S.s")
1996-01-15T00:00:00
```

When delimiters do not exist then the width of the code character controls how the string is
parsed.
```jldoctest
julia> DateTime("19960115", "yyyymmdd")
1996-01-15T00:00:00
```

If you need to use a code character as a delimiter you can escape it using backslash.
```jldoctest
julia> DateTime("1995y01m", "y\\ym\\m")
1995-01-01T00:00:00
```
"""
DateTime(dt::AbstractString,format::AbstractString;locale::AbstractString="english") = DateTime(dt,DateFormat(format,locale))

"""
    DateTime(dt::AbstractString, df::DateFormat) -> DateTime

Construct a `DateTime` by parsing the `dt` date string following the pattern given in
the [`DateFormat`](:func:`Dates.DateFormat`) object. Similar to
`DateTime(::AbstractString, ::AbstractString)` but more efficient when repeatedly parsing
similarly formatted date strings with a pre-created `DateFormat` object.
"""
DateTime(dt::AbstractString,df::DateFormat=ISODateTimeFormat) = DateTime(parse(dt,df)...)

"""
    Date(dt::AbstractString, format::AbstractString; locale="english") -> Date

Construct a `Date` object by parsing a `dt` date string following the pattern given in the
`format` string. Follows the same conventions as
`DateTime(::AbstractString, ::AbstractString)`.
"""
Date(dt::AbstractString,format::AbstractString;locale::AbstractString="english") = Date(dt,DateFormat(format,locale))

"""
    Date(dt::AbstractString, df::DateFormat) -> Date

Parse a date from a date string `dt` using a `DateFormat` object `df`.
"""
Date(dt::AbstractString,df::DateFormat=ISODateFormat) = Date(parse(dt,df)...)


"""
    format(dt::TimeType, format::AbstractString; locale="english") -> AbstractString

Construct a string by using a `TimeType` object and applying the provided `format`. The
following character codes can be used to construct the `format` string:

| Code       | Examples  | Comment                                                      |
|:-----------|:----------|:-------------------------------------------------------------|
| `y`        | 1996      | Numeric year                                                 |
| `m`        | 1, 12     | Numeric month                                                |
| `u`        | Jan       | Month name abbreviation according to the `locale` keyword    |
| `U`        | January   | Full month name according to the `locale` keyword            |
| `d`        | 1, 31     | Day of the month                                             |
| `H`        | 0, 23     | Hour (24-hour clock)                                         |
| `M`        | 0, 59     | Minute                                                       |
| `S`        | 0, 59     | Second                                                       |
| `s`        | 0, 999    | Millisecond                                                  |
| `e`        | Mon, Tue  | Abbreviated day of the week                                  |
| `E`        | Monday    | Named day of the week                                        |

The number of sequential code characters indicate the width of the code. A format of
`yyyy-mm` specifies that the code `y` should have a width of four while `m` a width of two.
Codes that yield numeric digits and have a width greater than one will be truncated or
left-padded with zeros to match the specified width. Codes with a width of one are not
truncated.

Any non-code characters are treated as separator between date and time slots. For example:
```jldoctest
julia> Dates.format(DateTime(1996,1,15), "y-mm-ddTHH:MM:SS")
"1996-01-15T00:00:00"
```

The millisecond code `s` has a special behaviour when it is directly preceeded by a period:
```jldoctest
julia> dt = DateTime(1,1,1,0,0,0,25)
0001-01-01T00:00:00.025

julia> Dates.format(dt, "s")
"25"

julia> Dates.format(dt, ".s")
".025"

julia> dt = DateTime(1,1,1,0,0,0,250)
0001-01-01T00:00:00.25

julia> Dates.format(dt, "s")
"250"

julia> Dates.format(dt, ".s")
".25"
```

If you need to use a code character as a separator you can escape it using backslash.
```jldoctest
julia> Dates.format(DateTime(1995), "yyyy\\ymm\\m")
"1995y01m"
```
"""
format(dt::TimeType,f::AbstractString;locale::AbstractString="english") = format(dt,DateFormat(f,locale))

# vectorized
DateTime{T<:AbstractString}(Y::AbstractArray{T},format::AbstractString;locale::AbstractString="english") = DateTime(Y,DateFormat(format,locale))
function DateTime{T<:AbstractString}(Y::AbstractArray{T},df::DateFormat=ISODateTimeFormat)
    return reshape(DateTime[DateTime(parse(y,df)...) for y in Y], size(Y))
end
Date{T<:AbstractString}(Y::AbstractArray{T},format::AbstractString;locale::AbstractString="english") = Date(Y,DateFormat(format,locale))
function Date{T<:AbstractString}(Y::AbstractArray{T},df::DateFormat=ISODateFormat)
    return reshape(Date[Date(parse(y,df)...) for y in Y], size(Y))
end

format{T<:TimeType}(Y::AbstractArray{T},format::AbstractString;locale::AbstractString="english") = Dates.format(Y,DateFormat(format,locale))
function format(Y::AbstractArray{Date},df::DateFormat=ISODateFormat)
    return reshape([Dates.format(y,df) for y in Y], size(Y))
end
function format(Y::AbstractArray{DateTime},df::DateFormat=ISODateTimeFormat)
    return reshape([Dates.format(y,df) for y in Y], size(Y))
end
