### truncation
Base.trunc(dt::Date,p::Type{Year}) = Date(UTD(totaldays(year(dt),1,1)))
Base.trunc(dt::Date,p::Type{Month}) = firstdayofmonth(dt)
Base.trunc(dt::Date,p::Type{Day}) = dt

Base.trunc(dt::DateTime,p::Type{Year}) = DateTime(trunc(Date(dt),Year))
Base.trunc(dt::DateTime,p::Type{Month}) = DateTime(trunc(Date(dt),Month))
Base.trunc(dt::DateTime,p::Type{Day}) = DateTime(Date(dt))
Base.trunc(dt::DateTime,p::Type{Hour}) = dt - Minute(dt) - Second(dt) - Millisecond(dt)
Base.trunc(dt::DateTime,p::Type{Minute}) = dt - Second(dt) - Millisecond(dt)
Base.trunc(dt::DateTime,p::Type{Second}) = dt - Millisecond(dt)
Base.trunc(dt::DateTime,p::Type{Millisecond}) = dt

# Adjusters
firstdayofweek(dt::Date) = Date(UTD(value(dt) - dayofweek(dt) + 1))
firstdayofweek(dt::DateTime) = DateTime(firstdayofweek(Date(dt)))
lastdayofweek(dt::Date) = Date(UTD(value(dt) + (7-dayofweek(dt))))
lastdayofweek(dt::DateTime) = DateTime(lastdayofweek(Date(dt)))

@vectorize_1arg TimeType firstdayofweek
@vectorize_1arg TimeType lastdayofweek

firstdayofmonth(dt::Date) = Date(UTD(value(dt)-day(dt)+1))
firstdayofmonth(dt::DateTime) = DateTime(firstdayofmonth(Date(dt)))
function lastdayofmonth(dt::Date)
    y,m,d = yearmonthday(dt)
    return Date(UTD(value(dt)+daysinmonth(y,m)-d))
end
lastdayofmonth(dt::DateTime) = DateTime(lastdayofmonth(Date(dt)))

@vectorize_1arg TimeType firstdayofmonth
@vectorize_1arg TimeType lastdayofmonth

firstdayofyear(dt::Date) = Date(UTD(value(dt)-dayofyear(dt)+1))
firstdayofyear(dt::DateTime) = DateTime(firstdayofyear(Date(dt)))
function lastdayofyear(dt::Date)
    y,m,d = yearmonthday(dt)
    return Date(UTD(value(dt)+daysinyear(y)-dayofyear(y,m,d)))
end

@vectorize_1arg TimeType firstdayofyear
@vectorize_1arg TimeType lastdayofyear

function firstdayofquarter(dt::Date)
    y,m = yearmonth(dt)
    mm = m < 4 ? 1 : m < 7 ? 4 : m < 10 ? 7 : 10
    return Date(y,mm,1)
end
function lastdayofquarter(dt::Date)
    y,m = yearmonth(dt)
    mm,d = m < 4 ? (3,31) : m < 7 ? (6,30) : m < 10 ? (9,30) : (12,31)
    return Date(y,mm,d)
end
firstdayofquarter(dt::DateTime) = DateTime(firstdayofquarter(Date(dt)))
lastdayofquarter(dt::DateTime) = DateTime(lastdayofquarter(Date(dt)))
@vectorize_1arg TimeType firstdayofquarter
@vectorize_1arg TimeType lastdayofquarter

# Temporal Adjusters
immutable DateFunction
    f::Function
    # validate boolean, single-arg inner constructor
    function DateFunction(f::Function,negate::Bool,dt::TimeType)
        try
            f(dt) in (true,false) || throw(ArgumentError("Provided function must take a single TimeType argument and return true or false"))
        catch e
            throw(ArgumentError("Provided function must take a single TimeType argument"))
        end
        n = negate ? (!) : identity
        return new(@eval x->$n($f(x)))
    end
end
Base.show(io::IO,df::DateFunction) = println(df.f.code)

# Core adjuster
function adjust(df::DateFunction,start,step,limit)
    for i = 1:limit
        df.f(start) && return start
        start += step
    end
    throw(ArgumentError("Adjustment limit reached: $limit iterations"))
end

function adjust(func::Function,start;step::Period=Day(1),negate::Bool=false,limit::Int=10000)
    return adjust(DateFunction(func,negate,start),start,step,limit)
end

# Constructors using DateFunctions
function Date(func::Function,y,m=1,d=1;step::Period=Day(1),negate::Bool=false,limit::Int=10000)
    return adjust(DateFunction(func,negate,Date(y,m,d)),Date(y,m,d),step,limit)
end

function DateTime(func::Function,y,m=1;step::Period=Day(1),negate::Bool=false,limit::Int=10000)
    return adjust(DateFunction(func,negate,DateTime(y,m)),DateTime(y,m),step,limit)
end
function DateTime(func::Function,y,m,d;step::Period=Hour(1),negate::Bool=false,limit::Int=10000)
    return adjust(DateFunction(func,negate,DateTime(y)),DateTime(y,m,d),step,limit)
end
function DateTime(func::Function,y,m,d,h;step::Period=Minute(1),negate::Bool=false,limit::Int=10000)
    return adjust(DateFunction(func,negate,DateTime(y)),DateTime(y,m,d,h),step,limit)
end
function DateTime(func::Function,y,m,d,h,mi;step::Period=Second(1),negate::Bool=false,limit::Int=10000)
    return adjust(DateFunction(func,negate,DateTime(y)),DateTime(y,m,d,h,mi),step,limit)
end
function DateTime(func::Function,y,m,d,h,mi,s;step::Period=Millisecond(1),negate::Bool=false,limit::Int=10000)
    return adjust(DateFunction(func,negate,DateTime(y)),DateTime(y,m,d,h,mi,s),step,limit)
end

# Return the next TimeType that falls on dow
ISDAYOFWEEK = Dict(Mon=>DateFunction(ismonday,false,Date(0)),
                   Tue=>DateFunction(istuesday,false,Date(0)),
                   Wed=>DateFunction(iswednesday,false,Date(0)),
                   Thu=>DateFunction(isthursday,false,Date(0)),
                   Fri=>DateFunction(isfriday,false,Date(0)),
                   Sat=>DateFunction(issaturday,false,Date(0)),
                   Sun=>DateFunction(issunday,false,Date(0)))

# "same" indicates whether the current date can be considered or not
tonext(dt::TimeType,dow::Int;same::Bool=false) = adjust(ISDAYOFWEEK[dow],same ? dt : dt+Day(1),Day(1),7)
# Return the next TimeType where func evals true using step in incrementing
function tonext(func::Function,dt::TimeType;step::Period=Day(1),negate::Bool=false,limit::Int=10000,same::Bool=false)
    return adjust(DateFunction(func,negate,dt),same ? dt : dt+step,step,limit)
end

toprev(dt::TimeType,dow::Int;same::Bool=false) = adjust(ISDAYOFWEEK[dow],same ? dt : dt+Day(-1),Day(-1),7)
function toprev(func::Function,dt::TimeType;step::Period=Day(-1),negate::Bool=false,limit::Int=10000,same::Bool=false)
    return adjust(DateFunction(func,negate,dt),same ? dt : dt+step,step,limit)
end

# Return the first TimeType that falls on dow in the Month or Year
function tofirst(dt::TimeType,dow::Int;of::Union(Type{Year},Type{Month})=Month)
    dt = of <: Month ? firstdayofmonth(dt) : firstdayofyear(dt)
    return adjust(ISDAYOFWEEK[dow],dt,Day(1),366)
end

# Return the last TimeType that falls on dow in the Month or Year
function tolast(dt::TimeType,dow::Int;of::Union(Type{Year},Type{Month})=Month)
    dt = of <: Month ? lastdayofmonth(dt) : lastdayofyear(dt)
    return adjust(ISDAYOFWEEK[dow],dt,Day(-1),366)
end

function recur{T<:TimeType}(fun::Function,start::T,stop::T;step::Period=Day(1),negate::Bool=false,limit::Int=10000)
    ((start != stop) & ((step > zero(step)) != (stop > start))) && return T[]
    a = T[]
    check = start <= stop ? 1 : -1
    df = Dates.DateFunction(fun,negate,start)
    while true
        next = Dates.adjust(df,start,step,limit)
        cmp(next,stop) == check && break
        push!(a,next)
        start = next + step
    end
    return a
end
function recur{T<:TimeType}(fun::Function,dr::StepRange{T};negate::Bool=false,limit::Int=10000)
    return recur(fun,first(dr),last(dr);step=step(dr),negate=negate,limit=limit)
end
