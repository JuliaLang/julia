# This file is a part of Julia. License is MIT: http://julialang.org/license

#Period types
value(x::Period) = x.value

# The default constructors for Periods work well in almost all cases
# P(x) = new((convert(Int64,x))
# The following definitions are for Period-specific safety
for period in (:Year, :Month, :Week, :Day, :Hour, :Minute, :Second, :Millisecond)
    period_str = string(period)
    accessor_str = lowercase(period_str)
    # Convenience method for show()
    @eval _units(x::$period) = " " * $accessor_str * (abs(value(x)) == 1 ? "" : "s")
    # periodisless
    @eval periodisless(x::$period,y::$period) = value(x) < value(y)
    # AbstractString parsing (mainly for IO code)
    @eval $period(x::AbstractString) = $period(Base.parse(Int64,x))
    # Period accessors
    typ_str = period in (:Hour, :Minute, :Second, :Millisecond) ? "DateTime" : "TimeType"
    description = typ_str == "TimeType" ? "`Date` or `DateTime`" : "`$typ_str`"
    reference = period == :Week ? " For details see [`$accessor_str(::$typ_str)`](:func:`$accessor_str`)." : ""
    @eval begin
        @doc """
            $($period_str)(dt::$($typ_str)) -> $($period_str)

        The $($accessor_str) part of a $($description) as a `$($period_str)`.$($reference)
        """ ->
        $period(dt::$(Symbol(typ_str))) = $period($(Symbol(accessor_str))(dt))

        @doc """
            $($period_str)(v)

        Construct a `$($period_str)` object with the given `v` value. Input must be
        losslessly convertible to an `Int64`.
        """ $period(v)
    end
end
# Now we're safe to define Period-Number conversions
# Anything an Int64 can convert to, a Period can convert to
Base.convert{T<:Number}(::Type{T},x::Period) = convert(T,value(x))
Base.convert{T<:Period}(::Type{T},x::Real) = T(x)

#Print/show/traits
Base.string{P<:Period}(x::P) = string(value(x),_units(x))
Base.show(io::IO,x::Period) = print(io,string(x))
Base.zero{P<:Period}(::Union{Type{P},P}) = P(0)
Base.one{P<:Period}(::Union{Type{P},P}) = P(1)
Base.typemin{P<:Period}(::Type{P}) = P(typemin(Int64))
Base.typemax{P<:Period}(::Type{P}) = P(typemax(Int64))

# Default values (as used by TimeTypes)
"""
    default(p::Period) -> Period

Returns a sensible "default" value for the input Period by returning `one(p)` for Year,
Month, and Day, and `zero(p)` for Hour, Minute, Second, and Millisecond.
"""
function default end

default{T<:DatePeriod}(p::Union{T,Type{T}}) = one(p)
default{T<:TimePeriod}(p::Union{T,Type{T}}) = zero(p)

(-){P<:Period}(x::P) = P(-value(x))
Base.isless{P<:Period}(x::P,y::P) = isless(value(x),value(y))
=={P<:Period}(x::P,y::P) = value(x) == value(y)

# Period Arithmetic, grouped by dimensionality:
import Base: div, fld, mod, rem, gcd, lcm, +, -, *, /, %, .+, .-, .*, .%
for op in (:+,:-,:lcm,:gcd)
    @eval ($op){P<:Period}(x::P,y::P) = P(($op)(value(x),value(y)))
end

for op in (:/,:div,:fld)
    @eval begin
        ($op){P<:Period}(x::P,y::P) = ($op)(value(x),value(y))
        ($op){P<:Period}(x::P,y::Real) = P(($op)(value(x),Int64(y)))
    end
end

for op in (:rem,:mod)
    @eval begin
        ($op){P<:Period}(x::P,y::P) = P(($op)(value(x),value(y)))
        ($op){P<:Period}(x::P,y::Real) = P(($op)(value(x),Int64(y)))
    end
end

/{P<:Period}(X::StridedArray{P}, y::P) = X ./ y
%{P<:Period}(X::StridedArray{P}, y::P) = X .% y
*{P<:Period}(x::P,y::Real) = P(value(x) * Int64(y))
*(y::Real,x::Period) = x * y
.*{P<:Period}(y::Real, X::StridedArray{P}) = X .* y
for (op,Ty,Tz) in ((:.*,Real,:P),
                   (:./,:P,Float64), (:./,Real,:P),
                   (:div,:P,Int64), (:div,Integer,:P),
                   (:.%,:P,:P),
                   (:mod,:P,:P))
    sop = string(op)
    op_ = sop[1] == '.' ? Symbol(sop[2:end]) : op
    @eval begin
        function ($op){P<:Period}(X::StridedArray{P},y::$Ty)
            Z = similar(X, $Tz)
            for (Idst, Isrc) in zip(eachindex(Z), eachindex(X))
                @inbounds Z[Idst] = ($op_)(X[Isrc],y)
            end
            return Z
        end
    end
end

# intfuncs
Base.gcdx{T<:Period}(a::T,b::T) = ((g,x,y)=gcdx(value(a),value(b)); return T(g),x,y)
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

# return (next coarser period, conversion factor):
coarserperiod{P<:Period}(::Type{P}) = (P,1)
coarserperiod(::Type{Millisecond}) = (Second,1000)
coarserperiod(::Type{Second}) = (Minute,60)
coarserperiod(::Type{Minute}) = (Hour,60)
coarserperiod(::Type{Hour}) = (Day,24)
coarserperiod(::Type{Day}) = (Week,7)
coarserperiod(::Type{Month}) = (Year,12)

# Stores multiple periods in greatest to least order by type, not values,
# canonicalized to eliminate zero periods, merge equal period types,
# and convert more-precise periods to less-precise periods when possible
"""
A `CompoundPeriod` is useful for expressing time periods that are not a fixed multiple of
smaller periods. For example, \"a year and a  day\" is not a fixed number of days, but can
be expressed using a `CompoundPeriod`. In fact, a `CompoundPeriod` is automatically
generated by addition of different period types, e.g. `Year(1) + Day(1)` produces a
`CompoundPeriod` result.
"""
type CompoundPeriod <: AbstractTime
    periods::Array{Period,1}
    function CompoundPeriod(p::Vector{Period})
        n = length(p)
        if n > 1
            sort!(p, rev=true, lt=periodisless)
            # canonicalize p by merging equal period types and removing zeros
            i = j = 1
            while j <= n
                k = j+1
                while k <= n
                    if typeof(p[j]) == typeof(p[k])
                        p[j] += p[k]
                        k += 1
                    else
                        break
                    end
                end
                if p[j] != zero(p[j])
                    p[i] = p[j]
                    i += 1
                end
                j = k
            end
            n = i - 1 # new length
        elseif n == 1 && value(p[1]) == 0
            p = Period[]
            n = 0
        end
        # canonicalize Periods by pushing "overflow" into a coarser period.
        if n > 0
            pc = sizehint!(Period[], n)
            P = typeof(p[n])
            v = value(p[n])
            i = n - 1
            while true
                Pc, f = coarserperiod(P)
                if i > 0 && typeof(p[i]) == P
                    v += value(p[i])
                    i -= 1
                end
                v0 = f == 1 ? v : rem(v, f)
                v0 != 0 && push!(pc, P(v0))
                if v != v0
                    P = Pc
                    v = div(v - v0, f)
                elseif i > 0
                    P = typeof(p[i])
                    v = value(p[i])
                    i -= 1
                else
                    break
                end
            end
            p = reverse!(pc)
            n = length(p)
        else
            return new(resize!(p, n))
        end
        # reduce the amount of mixed positive/negative Periods.
        if n > 0
            pc = sizehint!(Period[], n)
            i = n
            while i > 0
                j = i

                # Determine sign of the largest period in this group which
                # can be converted into via coarserperiod.
                last = Union{}
                current = typeof(p[i])
                while i > 0 && current != last
                    if typeof(p[i]) == current
                        i -= 1
                    end
                    last, current = current, coarserperiod(current)[1]
                end
                s = sign(value(p[i + 1]))

                # Adjust all the periods in the group based upon the
                # largest period sign.
                P = typeof(p[j])
                v = 0
                while j > i
                    Pc, f = coarserperiod(P)
                    if j > 0 && typeof(p[j]) == P
                        v += value(p[j])
                        j -= 1
                    end
                    v0 = f == 1 ? v : mod(v, f * s)
                    v0 != 0 && push!(pc, P(v0))
                    if v != v0
                        P = Pc
                        v = div(v - v0, f)
                    elseif j > 0
                        P = typeof(p[j])
                        v = 0
                    else
                        break
                    end
                end
            end
            p = reverse!(pc)
        end
        return new(p)
    end
end

"""
    CompoundPeriod(periods) -> CompoundPeriod

Construct a `CompoundPeriod` from a `Vector` of `Period`s. The constructor will
automatically simplify the periods into a canonical form according to the following rules:

* All `Period`s of the same type will be added together
* Any `Period` large enough be partially representable by a coarser `Period` will be broken
  into multiple `Period`s (eg. `Hour(30)` becomes `Day(1) + Hour(6)`)
* `Period`s with opposite signs will be combined when possible
  (eg. `Hour(1) - Day(1)` becomes `-Hour(23)`)

Due to the canonicalization, `CompoundPeriod` is also useful for converting time periods
into more human-comprehensible forms.

# Examples
```julia
julia> Dates.CompoundPeriod([Dates.Hour(12), Dates.Hour(13)])
1 day, 1 hour

julia> Dates.CompoundPeriod([Dates.Hour(-1), Dates.Minute(1)])
-59 minutes

julia> Dates.CompoundPeriod([Dates.Month(1), Dates.Week(-2)])
1 month, -2 weeks

julia> Dates.CompoundPeriod(Dates.Minute(50000)))
4 weeks, 6 days, 17 hours, 20 minutes
```
"""
CompoundPeriod{P<:Period}(p::Vector{P}) = CompoundPeriod(Array{Period}(p))


Base.convert(::Type{CompoundPeriod}, x::Period) = CompoundPeriod(Period[x])
function Base.string(x::CompoundPeriod)
    if isempty(x.periods)
        return "empty period"
    else
        s = ""
        for p in x.periods
            s *= ", " * string(p)
        end
        return s[3:end]
    end
end
Base.show(io::IO,x::CompoundPeriod) = print(io,string(x))

# E.g. Year(1) + Day(1)
(+)(x::Period,y::Period) = CompoundPeriod(Period[x,y])
(+)(x::CompoundPeriod,y::Period) = CompoundPeriod(vcat(x.periods,y))
(+)(y::Period,x::CompoundPeriod) = x + y
(+)(x::CompoundPeriod,y::CompoundPeriod) = CompoundPeriod(vcat(x.periods,y.periods))
# E.g. Year(1) - Month(1)
(-)(x::Period,y::Period) = CompoundPeriod(Period[x,-y])
(-)(x::CompoundPeriod,y::Period) = CompoundPeriod(vcat(x.periods,-y))
(-)(x::CompoundPeriod) = CompoundPeriod(-x.periods)
(-)(y::Union{Period,CompoundPeriod},x::CompoundPeriod) = (-x) + y

GeneralPeriod = Union{Period,CompoundPeriod}
(+)(x::GeneralPeriod) = x
(+){P<:GeneralPeriod}(x::StridedArray{P}) = x

for op in (:.+, :.-)
    op_ = Symbol(string(op)[2:end])
    @eval begin
        function ($op){P<:GeneralPeriod}(X::StridedArray{P},y::GeneralPeriod)
            Z = similar(X, CompoundPeriod)
            for (Idst, Isrc) in zip(eachindex(Z), eachindex(X))
                @inbounds Z[Idst] = ($op_)(X[Isrc],y)
            end
            return Z
        end
        ($op){P<:GeneralPeriod}(x::GeneralPeriod,Y::StridedArray{P}) = ($op)(Y,x) |> ($op_)
        ($op_){P<:GeneralPeriod}(x::GeneralPeriod,Y::StridedArray{P}) = ($op)(Y,x) |> ($op_)
        ($op_){P<:GeneralPeriod}(Y::StridedArray{P},x::GeneralPeriod) = ($op)(Y,x)
        ($op_){P<:GeneralPeriod, Q<:GeneralPeriod}(X::StridedArray{P}, Y::StridedArray{Q}) =
            reshape(CompoundPeriod[($op_)(x,y) for (x,y) in zip(X, Y)], promote_shape(size(X),size(Y)))
    end
end

(==)(x::CompoundPeriod, y::Period) = x == CompoundPeriod(y)
(==)(y::Period, x::CompoundPeriod) = x == y
(==)(x::CompoundPeriod, y::CompoundPeriod) = x.periods == y.periods

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

function (-)(x::TimeType,y::CompoundPeriod)
    for p in y.periods
        x -= p
    end
    return x
end
(-)(x::CompoundPeriod,y::TimeType) = y - x

# Fixed-value Periods (periods corresponding to a well-defined time interval,
# as opposed to variable calendar intervals like Year).
typealias FixedPeriod Union{Week,Day,Hour,Minute,Second,Millisecond}

# like div but throw an error if remainder is nonzero
function divexact(x,y)
    q,r = divrem(x, y)
    r == 0 || throw(InexactError())
    return q
end

# FixedPeriod conversions and promotion rules
const fixedperiod_conversions = [(Week,7),(Day,24),(Hour,60),(Minute,60),(Second,1000),(Millisecond,1)]
for i = 1:length(fixedperiod_conversions)
    (T,n) = fixedperiod_conversions[i]
    N = 1
    for j = i-1:-1:1 # less-precise periods
        (Tc,nc) = fixedperiod_conversions[j]
        N *= nc
        vmax = typemax(Int64) ÷ N
        vmin = typemin(Int64) ÷ N
        @eval function Base.convert(::Type{$T}, x::$Tc)
            $vmin ≤ value(x) ≤ $vmax || throw(InexactError())
            return $T(value(x)*$N)
        end
    end
    N = n
    for j = i+1:length(fixedperiod_conversions) # more-precise periods
        (Tc,nc) = fixedperiod_conversions[j]
        @eval Base.convert(::Type{$T}, x::$Tc) = $T(divexact(value(x), $N))
        @eval Base.promote_rule(::Type{$T},::Type{$Tc}) = $Tc
        N *= nc
    end
end
# have to declare thusly so that diagonal dispatch above takes precedence:
(==){T<:FixedPeriod,S<:FixedPeriod}(x::T,y::S) = (==)(promote(x,y)...)
Base.isless{T<:FixedPeriod,S<:FixedPeriod}(x::T,y::S) = isless(promote(x,y)...)

# other periods with fixed conversions but which aren't fixed time periods
typealias OtherPeriod Union{Month,Year}
let vmax = typemax(Int64) ÷ 12, vmin = typemin(Int64) ÷ 12
    @eval function Base.convert(::Type{Month}, x::Year)
        $vmin ≤ value(x) ≤ $vmax || throw(InexactError())
        Month(value(x)*12)
    end
end
Base.convert(::Type{Year}, x::Month) = Year(divexact(value(x),12))
Base.promote_rule(::Type{Year}, ::Type{Month}) = Month
(==){T<:OtherPeriod,S<:OtherPeriod}(x::T,y::S) = (==)(promote(x,y)...)
Base.isless{T<:OtherPeriod,S<:OtherPeriod}(x::T,y::S) = isless(promote(x,y)...)

# truncating conversions to milliseconds and days:
toms(c::Millisecond) = value(c)
toms(c::Second)      = 1000*value(c)
toms(c::Minute)      = 60000*value(c)
toms(c::Hour)        = 3600000*value(c)
toms(c::Day)         = 86400000*value(c)
toms(c::Week)        = 604800000*value(c)
toms(c::Month)       = 86400000.0*30.436875*value(c)
toms(c::Year)        = 86400000.0*365.2425*value(c)
toms(c::CompoundPeriod) = isempty(c.periods)?0.0 : Float64(sum(toms,c.periods))
days(c::Millisecond) = div(value(c),86400000)
days(c::Second)      = div(value(c),86400)
days(c::Minute)      = div(value(c),1440)
days(c::Hour)        = div(value(c),24)
days(c::Day)         = value(c)
days(c::Week)        = 7*value(c)
days(c::Year)        = 365.2425*value(c)
days(c::Month)       = 30.436875*value(c)
days(c::CompoundPeriod) = isempty(c.periods)?0.0 : Float64(sum(days,c.periods))
