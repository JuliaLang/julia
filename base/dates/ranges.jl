# Date/DateTime Ranges

# Override default step; otherwise it would be Millisecond(1)
Base.colon{T<:DateTime}(start::T, stop::T) = StepRange(start, Day(1), stop)

# Given a start and end date, how many steps/periods are in between
guess(a::DateTime,b::DateTime,c) = floor(Int64,(int128(b) - int128(a))/toms(c))
guess(a::Date,b::Date,c) = int64(div(int64(b - a),days(c)))
function len(a,b,c)
    lo, hi, st = min(a,b), max(a,b), abs(c)
    i = guess(a,b,c)-1
    while lo+st*i <= hi
        i += 1
    end
    return i-1
end
Base.length{T<:TimeType}(r::StepRange{T}) = isempty(r) ? 0 : len(r.start,r.stop,r.step) + 1
# Period ranges hook into Int64 overflow detection
Base.length{P<:Period}(r::StepRange{P}) = length(StepRange(value(r.start),value(r.step),value(r.stop)))

# Used to calculate the last valid date in the range given the start, stop, and step
# last = stop - steprem(start,stop,step)
Base.steprem{T<:TimeType}(a::T,b::T,c) = b - (a + c*len(a,b,c))

import Base.in
function in{T<:TimeType}(x::T, r::StepRange{T})
    n = len(first(r),x,step(r)) + 1
    n >= 1 && n <= length(r) && r[n] == x
end

Base.start{T<:TimeType}(r::StepRange{T}) = 0
Base.next{T<:TimeType}(r::StepRange{T}, i::Int) = (r.start+r.step*i,i+1)
Base.done{T<:TimeType,S<:Period}(r::StepRange{T,S}, i::Integer) = length(r) <= i

.+{T<:TimeType}(x::Period, r::Range{T}) = (x+first(r)):step(r):(x+last(r))
.+{T<:TimeType}(r::Range{T},x::Period) = x .+ r
+{T<:TimeType}(r::Range{T},x::Period) = x .+ r
+{T<:TimeType}(x::Period,r::Range{T}) = x .+ r
.-{T<:TimeType}(r::Range{T},x::Period) = (first(r)-x):step(r):(last(r)-x)
-{T<:TimeType}(r::Range{T},x::Period) = r .- x
