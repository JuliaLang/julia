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
    transition::Union{Regex,AbstractString}
end

immutable FixedWidthSlot{T<:Any} <: Slot{T}
    parser::Type{T}
    letter::Char
    width::Int
end

immutable DateFormat
    slots::Array{Slot,1}
    prefix::AbstractString # optional transition from the start of a string to the 1st slot
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
SLOT_RULE['Y'] = Year
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
        tran = replace(f[last_offset:m.offset-1], r"\\(.)", s"\1")

        if isempty(params)
            prefix = tran
        else
            slot = tran == "" ? FixedWidthSlot(params...) : DelimitedSlot(params..., tran)
            push!(slots,slot)
        end

        params = (typ,letter,width)
        last_offset = m.offset + width
    end

    tran = last_offset > endof(f) ? r"(?=\s|$)" : replace(f[last_offset:end], r"\\(.)", s"\1")
    if !isempty(params)
        slot = tran == "" ? FixedWidthSlot(params...) : DelimitedSlot(params..., tran)
        push!(slots,slot)
    end

    duplicates(slots) && throw(ArgumentError("Two separate periods of the same type detected"))
    return DateFormat(slots,prefix,locale)
end

const SLOTERROR = ArgumentError("Non-digit character encountered")
slotparse(slot,x,locale) = !ismatch(r"[^0-9\s]",x) ? slot.parser(x) : throw(SLOTERROR)
function slotparse(slot::Slot{Month},x,locale)
    if slot.letter == 'm'
        ismatch(r"[^0-9\s]",x) ? throw(SLOTERROR) : return Month(x)
    elseif slot.letter == 'u'
        return Month(MONTHTOVALUEABBR[locale][lowercase(x)])
    else
        return Month(MONTHTOVALUE[locale][lowercase(x)])
    end
end
slotparse(slot::Slot{Millisecond},x,locale) = !ismatch(r"[^0-9\s]",x) ? slot.parser(Base.parse(Float64,"."*x)*1000.0) : throw(SLOTERROR)
slotparse(slot::Slot{DayOfWeekSlot},x,locale) = nothing

function getslot(x,slot::DelimitedSlot,locale,cursor)
    endind = first(search(x,slot.transition,nextind(x,cursor)))
    if endind == 0 # we didn't find the next delimiter
        s = x[cursor:end]
        index = endof(x)+1
    else
        s = x[cursor:(endind-1)]
        index = nextind(x,endind)
    end
    return index, slotparse(slot,s,locale)
end
getslot(x,slot,locale,cursor) = (cursor+slot.width, slotparse(slot,x[cursor:(cursor+slot.width-1)], locale))

function parse(x::AbstractString,df::DateFormat)
    x = strip(x)
    startswith(x, df.prefix) && (x = replace(x, df.prefix, "", 1))
    isempty(x) && throw(ArgumentError("Cannot parse empty format string"))
    if isa(df.slots[1], DelimitedSlot) && first(search(x,df.slots[1].transition)) == 0
        throw(ArgumentError("Delimiter mismatch. Couldn't find first delimiter, \"$(df.slots[1].transition)\", in date string"))
    end
    periods = Period[]
    extra = Any[]  # Supports custom slot types such as TimeZone
    cursor = 1
    for slot in df.slots
        cursor, pe = getslot(x,slot,df.locale,cursor)
        pe != nothing && (isa(pe,Period) ? push!(periods,pe) : push!(extra,pe))
        cursor > endof(x) && break
    end
    sort!(periods,rev=true,lt=periodisless)
    if isempty(extra)
        return periods
    else
        return vcat(periods, extra)
    end
end

slotformat(slot,dt,locale) = lpad(string(value(slot.parser(dt))),slot.width,"0")
function slotformat(slot::Slot{Year},dt,locale)
    s = lpad(string(value(slot.parser(dt))),slot.width,"0")
    if slot.letter == 'y'
        return s[(end-slot.width+1):end]  # Truncate the year
    else # == 'Y'
        return s
    end
end
function slotformat(slot::Slot{Month},dt,locale)
    if slot.letter == 'm'
        return lpad(month(dt),slot.width,"0")
    elseif slot.letter == 'u'
        return VALUETOMONTHABBR[locale][month(dt)]
    else
        return VALUETOMONTH[locale][month(dt)]
    end
end
function slotformat(slot::Slot{DayOfWeekSlot},dt,locale)
    if slot.letter == 'e'
        return VALUETODAYOFWEEKABBR[locale][dayofweek(dt)]
    else # == 'E'
        return VALUETODAYOFWEEK[locale][dayofweek(dt)]
    end
end
slotformat(slot::Slot{Millisecond},dt,locale) = rpad(string(millisecond(dt)/1000.0)[3:end], slot.width, "0")

function format(dt::TimeType,df::DateFormat)
    f = df.prefix
    for slot in df.slots
        f *= slotformat(slot,dt,df.locale)
        if isa(slot, DelimitedSlot)
            f *= isa(slot.transition, AbstractString) ? slot.transition : ""
        end
    end
    return f
end

# UI
const ISODateTimeFormat = DateFormat("yyyy-mm-dd\\THH:MM:SS.s")
const ISODateFormat = DateFormat("yyyy-mm-dd")
const RFC1123Format = DateFormat("e, dd u yyyy HH:MM:SS")

DateTime(dt::AbstractString,format::AbstractString;locale::AbstractString="english") = DateTime(dt,DateFormat(format,locale))
DateTime(dt::AbstractString,df::DateFormat=ISODateTimeFormat) = DateTime(parse(dt,df)...)

Date(dt::AbstractString,format::AbstractString;locale::AbstractString="english") = Date(dt,DateFormat(format,locale))
Date(dt::AbstractString,df::DateFormat=ISODateFormat) = Date(parse(dt,df)...)

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
