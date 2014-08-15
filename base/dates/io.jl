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
const english = (UTF8String=>Int)["january"=>1,"february"=>2,"march"=>3,"april"=>4,
                 "may"=>5,"june"=>6,"july"=>7,"august"=>8,"september"=>9,
                 "october"=>10,"november"=>11,"december"=>12]
const abbrenglish = (UTF8String=>Int)["jan"=>1,"feb"=>2,"mar"=>3,"apr"=>4,
                     "may"=>5,"jun"=>6,"jul"=>7,"aug"=>8,"sep"=>9,
                     "oct"=>10,"nov"=>11,"dec"=>12]
const MONTHTOVALUE = (UTF8String=>Dict{UTF8String,Int})["english"=>english]
const MONTHTOVALUEABBR = (UTF8String=>Dict{UTF8String,Int})["english"=>abbrenglish]

# Date/DateTime Parsing
abstract Slot{P<:AbstractTime} <: AbstractTime

immutable DelimitedSlot{P<:AbstractTime} <: Slot{P}
    i::Int
    period::Type{P}
    width::Int
    option::Int
    locale::String
end

immutable FixedWidthSlot{P<:AbstractTime} <: Slot{P}
    i::Int
    period::Type{P}
    width::Int
    option::Int
    locale::String
end

immutable DateFormat
    slots::Array{Slot,1}
    begtran # optional transition from the start of a string to the 1st slot
    trans #trans[i] == how to transition FROM slots[i] TO slots[i+1]
end

duplicates(slots) = any(map(x->count(y->x.period==y.period,slots),slots) .> 1)

function DateFormat(f::String,locale::String="english")
    slots = Slot[]
    trans = {}
    begtran = match(r"^.*?(?=[ymuUdHMSs])",f).match
    ss = split(f,r"^.*?(?=[ymuUdHMSs])")
    s = split(begtran == "" ? ss[1] : ss[2],r"[^ymuUdHMSs]|(?<=([ymuUdHMSs])(?!\1))")
    for (i,k) in enumerate(s)
        k == "" && break
        tran = i >= endof(s) ? r"$" : match(Regex("(?<=$(s[i])).*(?=$(s[i+1]))"),f).match
        slot = tran == "" || tran == r"$" ? FixedWidthSlot : DelimitedSlot
        width = length(k)
        typ = 'y' in k ? Year : 'm' in k ? Month : 
              'u' in k ? Month : 'U' in k ? Month :
              'd' in k ? Day : 'H' in k ? Hour : 
              'M' in k ? Minute : 'S' in k ? Second : Millisecond 
        option = 'U' in k ? 2 : 'u' in k ? 1 : 0
        push!(slots,slot(i,typ,width,option,locale))
        push!(trans,tran)
    end
    duplicates(slots) && throw(ArgumentError("Two separate periods of the same type detected"))
    return DateFormat(slots,begtran,trans)
end

const SLOTERROR = ArgumentError("Non-digit character encountered")
slotparse(slot,x) = !ismatch(r"\D",x) ? slot.period(x) : throw(SLOTERROR)
function slotparse(slot::Slot{Month},x)
    if slot.option == 0
        ismatch(r"\D",x) && throw(SLOTERROR)
        return Month(x)
    elseif slot.option == 1
        return Month(MONTHTOVALUEABBR[slot.locale][lowercase(x)])
    else
        return Month(MONTHTOVALUE[slot.locale][lowercase(x)])
    end
end
slotparse(slot::Slot{Millisecond},x) = !ismatch(r"\D",x) ? slot.period(parsefloat("."*x)*1000.0) : throw(SLOTERROR)

function getslot(x,slot::DelimitedSlot,df,cursor)
    endind = first(search(x,df.trans[slot.i],cursor+1))
    if endind == 0 # we didn't find the next delimiter
        s = x[cursor:end]
        return (endof(x)+1, isdigit(s) ? slotparse(slot,s) : default(slot.period))
    end
    return endind+1, slotparse(slot,x[cursor:(endind-1)])
end
getslot(x,slot,df,cursor) = (cursor+slot.width, slotparse(slot,x[cursor:(cursor+slot.width-1)]))

function parse(x::String,df::DateFormat)
    x = strip(replace(x, r"#.*$", ""))
    x = replace(x,df.begtran,"")
    isempty(x) && throw(ArgumentError("Cannot parse empty string"))
    (typeof(df.slots[1]) <: DelimitedSlot && first(search(x,df.trans[1])) == 0) && throw(ArgumentError("Delimiter mismatch. Couldn't find first delimiter, \"$(df.trans[1])\", in date string"))
    periods = Period[]
    cursor = 1
    for slot in df.slots
        cursor, pe = getslot(x,slot,df,cursor)
        push!(periods,pe)
        cursor > endof(x) && break
    end
    return sort!(periods,rev=true,lt=periodisless)
end

slotformat(slot::Slot{Year},dt) = lpad(string(value(slot.period(dt))),slot.width,"0")[(end-slot.width+1):end]
slotformat(slot,dt) = lpad(string(value(slot.period(dt))),slot.width,"0")
function slotformat(slot::Slot{Month},dt)
    if slot.option == 0
        return lpad(month(dt),slot.width,"0")
    elseif slot.option == 1
        return VALUETOMONTHABBR[slot.locale][month(dt)]
    else
        return VALUETOMONTH[slot.locale][month(dt)]
    end
end
slotformat(slot::Slot{Millisecond},dt) = rpad(string(millisecond(dt)/1000.0)[3:end], slot.width, "0")

function format(dt::TimeType,df::DateFormat)
    f = ""
    for slot in df.slots
        f *= slotformat(slot,dt)
        f *= typeof(df.trans[slot.i]) <: Regex ? "" : df.trans[slot.i]
    end
    return f
end

# UI
const ISODateTimeFormat = DateFormat("yyyy-mm-ddTHH:MM:SS.s")
const ISODateFormat = DateFormat("yyyy-mm-dd")

DateTime(dt::String,format::String;locale::String="english") = DateTime(dt,DateFormat(format,locale))
DateTime(dt::String,df::DateFormat=ISODateTimeFormat) = DateTime(parse(dt,df)...)

Date(dt::String,format::String;locale::String="english") = Date(dt,DateFormat(format,locale))
Date(dt::String,df::DateFormat=ISODateFormat) = Date(parse(dt,df)...)

format(dt::TimeType,f::String;locale::String="english") = format(dt,DateFormat(f,locale))

# vectorized
DateTime{T<:String}(y::AbstractArray{T},format::String;locale::String="english") = DateTime(y,DateFormat(format,locale))
function DateTime{T<:String}(y::AbstractArray{T},df::DateFormat=ISODateFormat)
    return reshape(DateTime[DateTime(parse(y[i],df)...) for i in 1:length(y)], size(y))
end
Date{T<:String}(y::AbstractArray{T},format::String;locale::String="english") = Date(y,DateFormat(format,locale))
function Date{T<:String}(y::AbstractArray{T},df::DateFormat=ISODateFormat)
    return reshape(Date[Date(parse(y[i],df)...) for i in 1:length(y)], size(y))
end

format{T<:TimeType}(y::AbstractArray{T},format::String;locale::String="english") = Dates.format(y,DateFormat(format,locale))
function format(y::AbstractArray{Date},df::DateFormat=ISODateFormat)
    return reshape([Dates.format(y[i],df) for i in 1:length(y)], size(y))
end
function format(y::AbstractArray{DateTime},df::DateFormat=ISODateTimeFormat)
    return reshape([Dates.format(y[i],df) for i in 1:length(y)], size(y))
end

export ISODateTimeFormat, ISODateFormat, DateFormat
