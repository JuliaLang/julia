#Period types
value{P<:Period}(x::P) = x.value

# The default constructors for Periods work well in almost all cases
# P(x) = new((convert(Int64,x))
# The following definitions are for Period-specific safety
for p in (:Year,:Month,:Week,:Day,:Hour,:Minute,:Second,:Millisecond)
    # This ensures that we can't convert between Periods
    @eval $p(x::Period) = throw(ArgumentError("Can't convert $(typeof(x)) to $($p)"))
    # Unless the Periods are the same type
    @eval $p(x::$p) = x
    # Convenience method for show()
    @eval _units(x::$p) = " " * lowercase(string($p)) * (abs(value(x)) == 1 ? "" : "s")
    # periodisless
    @eval periodisless(x::$p,y::$p) = value(x) < value(y)
    # String parsing (mainly for IO code)
    @eval $p(x::String) = $p(parseint(x))
    # Period accessors
    @eval $p(x::TimeType) = $p($(symbol(lowercase(string(p))))(x))
end
# Now we're safe to define Period-Number conversions
# Anything an Int64 can convert to, a Period can convert to
Base.convert{T<:Number}(::Type{T},x::Period) = convert(T,value(x))
Base.convert{T<:Period}(::Type{T},x::Real) = T(int64(x))

#Print/show/traits
Base.string{P<:Period}(x::P) = string(value(x),_units(x))
Base.show(io::IO,x::Period) = print(io,string(x))
Base.zero{P<:Period}(::Union(Type{P},P)) = P(0)
Base.one{P<:Period}(::Union(Type{P},P)) = P(1)
Base.typemin{P<:Period}(::Type{P}) = P(typemin(Int64))
Base.typemax{P<:Period}(::Type{P}) = P(typemax(Int64))

# Default values (as used by TimeTypes)
default{T<:DatePeriod}(p::Union(T,Type{T})) = one(p)
default{T<:TimePeriod}(p::Union(T,Type{T})) = zero(p)

(-){P<:Period}(x::P) = P(-value(x))
Base.isless{P<:Period}(x::P,y::P) = isless(value(x),value(y))
Base.isless{R<:Real}(x::Period,y::R) = isless(int64(y),value(x))
Base.isless{R<:Real}(y::R,x::Period) = isless(int64(y),value(x))
=={P<:Period}(x::P,y::P) = ===(value(x),value(y))
=={R<:Real}(x::Period,y::R) = ==(int64(y),value(x))
=={R<:Real}(y::R,x::Period) = ==(int64(y),value(x))

Base.isless(x::Period,y::Period) = throw(ArgumentError("Can't compare $(typeof(x)) and $(typeof(y))"))
==(x::Period,y::Period) = throw(ArgumentError("Can't compare $(typeof(x)) and $(typeof(y))"))

#Period Arithmetic:
import Base.div, Base.mod, Base.gcd, Base.lcm
let vec_ops = [:.+,:.-,:.*,:.%,:div]
    for op in [:+,:-,:*,:%,:mod,:gcd,:lcm,vec_ops]
        @eval begin
        #Period-Period
        ($op){P<:Period}(x::P,y::P) = P(($op)(value(x),value(y)))
        #Period-Real
        ($op){P<:Period}(x::P,y::Real) = P(($op)(value(x),convert(Int64,y)))
        ($op){P<:Period}(x::Real,y::P) = P(($op)(convert(Int64,x),value(y)))
        end
        #Vectorized
        if op in vec_ops
            @eval begin
            ($op){P<:Period}(x::AbstractArray{P}, y::P) = reshape([$op(i,y) for i in x], size(x))
            ($op){P<:Period}(y::P, x::AbstractArray{P}) = reshape([$op(i,y) for i in x], size(x))
            end
        end
    end
end

# intfuncs
Base.gcdx{T<:Period}(a::T,b::T) = ((g,x,y)=gcdx(Dates.value(a),Dates.value(b)); return T(g),x,y)
Base.abs{T<:Period}(a::T) = T(abs(value(a)))

periodisless(::Period,::Year)        = true
periodisless(::Period,::Month)       = true
periodisless(::Year,::Month)         = false
periodisless(::Period,::Week)        = true
periodisless(::Year,::Week)          = false
periodisless(::Month,::Week)         = false
periodisless(::Period,::Day)         = true
periodisless(::Year,::Day)           = false
periodisless(::Month,::Day)          = false
periodisless(::Week,::Day)           = false
periodisless(::Period,::Hour)        = false
periodisless(::Minute,::Hour)        = true
periodisless(::Second,::Hour)        = true
periodisless(::Millisecond,::Hour)   = true
periodisless(::Period,::Minute)      = false
periodisless(::Second,::Minute)      = true
periodisless(::Millisecond,::Minute) = true
periodisless(::Period,::Second)      = false
periodisless(::Millisecond,::Second) = true
periodisless(::Period,::Millisecond) = false

# Stores multiple periods in greatest to least order by type, not values
type CompoundPeriod
    periods::Array{Period,1}
end
function Base.string(x::CompoundPeriod)
    s = ""
    for p in x.periods
        s *= ", " * string(p)
    end
    return s[3:end]
end
Base.show(io::IO,x::CompoundPeriod) = print(io,string(x))
# E.g. Year(1) + Day(1)
(+)(x::Period,y::Period) = CompoundPeriod(sort!(Period[x,y],rev=true,lt=periodisless))
(+)(x::CompoundPeriod,y::Period) = (sort!(push!(x.periods,y) ,rev=true,lt=periodisless); return x)
# E.g. Year(1) - Month(1)
(-)(x::Period,y::Period) = CompoundPeriod(sort!(Period[x,-y],rev=true,lt=periodisless))
(-)(x::CompoundPeriod,y::Period) = (sort!(push!(x.periods,-y),rev=true,lt=periodisless); return x)

# Capture TimeType+-Period methods
(+)(a::TimeType,b::Period,c::Period) = (+)(a,b+c)
(-)(a::TimeType,b::Period,c::Period) = (-)(a,b-c)
(+)(a::TimeType,b::Period,c::Period,d::Period...) = (+)((+)(a,b+c),d...)
(-)(a::TimeType,b::Period,c::Period,d::Period...) = (-)((-)(a,b-c),d...)

function (+)(x::TimeType,y::CompoundPeriod)
    for p in y.periods
        x += p
    end
    return x
end
(+)(x::CompoundPeriod,y::TimeType) = y + x

# Periods from TimeTypes
Year(x::TimeType) = Year(year(x))
Month(x::TimeType) = Month(month(x))
Week(x::TimeType) = Week(week(x))
Day(x::TimeType) = Day(day(x))
Hour(x::TimeType) = Hour(hour(x))
Minute(x::TimeType) = Minute(minute(x))
Second(x::TimeType) = Second(second(x))
Millisecond(x::TimeType) = Millisecond(millisecond(x))
