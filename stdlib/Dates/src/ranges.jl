# This file is a part of Julia. License is MIT: https://julialang.org/license

# Date/DateTime Ranges

(::Type{StepRange{<:Dates.DatePeriod,<:Real}})(start, step, stop) =
    throw(ArgumentError("must specify step as a Period when constructing Dates ranges"))

# Given a start and end date, how many steps/periods are in between
guess(a::DateTime, b::DateTime, c) = floor(Int64, (Int128(value(b)) - Int128(value(a))) / toms(c))
guess(a::Date, b::Date, c) = Int64(div(value(b - a), days(c)))
len(a::Time, b::Time, c) = Int64(div(value(b - a), tons(c)))
function len(a, b, c)
    lo, hi, st = min(a, b), max(a, b), abs(c)
    i = guess(a, b, c) - 1
    while lo + st * i <= hi
        i += 1
    end
    return i - 1
end
Base.length(r::StepRange{<:TimeType}) = isempty(r) ? Int64(0) : len(r.start, r.stop, r.step) + 1
# Period ranges hook into Int64 overflow detection
Base.length(r::StepRange{<:Period}) = length(StepRange(value(r.start), value(r.step), value(r.stop)))

# Used to calculate the last valid date in the range given the start, stop, and step
# last = stop - steprem(start, stop, step)
Base.steprem(a::T, b::T, c) where {T<:TimeType} = b - (a + c * len(a, b, c))

import Base.in
function in(x::T, r::StepRange{T}) where T<:TimeType
    n = len(first(r), x, step(r)) + 1
    n >= 1 && n <= length(r) && r[n] == x
end

Base.start(r::StepRange{<:TimeType}) = 0
Base.next(r::StepRange{<:TimeType}, i::Int) = (r.start + r.step*i, i + 1)
Base.done(r::StepRange{<:TimeType,<:Period}, i::Integer) = length(r) <= i

+(x::Period, r::AbstractRange{<:TimeType}) = (x + first(r)):step(r):(x + last(r))
+(r::AbstractRange{<:TimeType}, x::Period) = x + r
-(r::AbstractRange{<:TimeType}, x::Period) = (first(r)-x):step(r):(last(r)-x)

# Combinations of types and periods for which the range step is regular
Base.RangeStepStyle(::Type{<:OrdinalRange{<:TimeType, <:FixedPeriod}}) =
    Base.RangeStepRegular()
