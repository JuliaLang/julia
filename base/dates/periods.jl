#Period types
value(x::Period) = x.value

# The default constructors for Periods work well in almost all cases
# P(x) = new((convert(Int64,x))
# The following definitions are for Period-specific safety
for p in (:Year,:Month,:Week,:Day,:Hour,:Minute,:Second,:Millisecond)
    # Convenience method for show()
    @eval _units(x::$p) = $(" " * lowercase(string(p))) * (abs(value(x)) == 1 ? "" : "s")
    # periodisless
    @eval periodisless(x::$p,y::$p) = value(x) < value(y)
    # AbstractString parsing (mainly for IO code)
    @eval $p(x::AbstractString) = $p(parseint(Int64,x))
    # Period accessors
    @eval $p(x::TimeType) = $p($(symbol(lowercase(string(p))))(x))
end
# Now we're safe to define Period-Number conversions
# Anything an Int64 can convert to, a Period can convert to
Base.convert{T<:Number}(::Type{T},x::Period) = convert(T,value(x))
Base.convert{T<:Period}(::Type{T},x::Real) = T(x)

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
=={P<:Period}(x::P,y::P) = value(x) == value(y)

# Period Arithmetic, grouped by dimensionality:
import Base: div, mod, rem, gcd, lcm, +, -, *, /, %, .+, .-, .*, .%
for op in (:+,:-,:lcm,:gcd)
    @eval ($op){P<:Period}(x::P,y::P) = P(($op)(value(x),value(y)))
end
for op in (:.+, :.-)
    op_ = symbol(string(op)[2:end])
    @eval begin
        ($op){P<:Period}(x::P,Y::StridedArray{P}) = ($op)(Y,x)
        ($op_){P<:Period}(x::P,Y::StridedArray{P}) = ($op)(Y,x)
        ($op_){P<:Period}(Y::StridedArray{P},x::P) = ($op)(Y,x)
    end
end
for op in (:/,:%,:div,:mod)
    @eval begin
        ($op){P<:Period}(x::P,y::P) = ($op)(value(x),value(y))
        ($op){P<:Period}(x::P,y::Real) = P(($op)(value(x),Int64(y)))
    end
end
/{P<:Period}(X::StridedArray{P}, y::P) = X ./ y
%{P<:Period}(X::StridedArray{P}, y::P) = X .% y
*{P<:Period}(x::P,y::Real) = P(value(x) * Int64(y))
*(y::Real,x::Period) = x * y
.*{P<:Period}(y::Real, X::StridedArray{P}) = X .* y
for (op,Ty,Tz) in ((:.+,:P,:P),(:.-,:P,:P), (:.*,Real,:P),
                   (:./,:P,Float64), (:./,Real,:P),
                   (:.%,:P,Int64), (:.%,Integer,:P),
                   (:div,:P,Int64), (:div,Integer,:P),
                   (:mod,:P,Int64), (:mod,Integer,:P))
    sop = string(op)
    op_ = sop[1] == '.' ? symbol(sop[2:end]) : op
    @eval begin
        function ($op){P<:Period}(X::StridedArray{P},y::$Ty)
            Z = similar(X, $Tz)
            for i = 1:length(X)
                @inbounds Z[i] = ($op_)(X[i],y)
            end
            return Z
        end
    end
end

# intfuncs
Base.gcdx{T<:Period}(a::T,b::T) = ((g,x,y)=gcdx(value(a),value(b)); return T(g),x,y)
Base.abs{T<:Period}(a::T) = T(abs(value(a)))

# Like Base.steprem in range.jl, but returns the correct type for Periods
Base.steprem(start::Period,stop::Period,step::Period) = (stop-start) % value(step)

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
        # canonicalize Period values so that 0 < ms < 1000 etcetera.
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
                v0 = f == 1 ? v : mod(v, f)
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
            return new(reverse!(pc))
        else
            return new(resize!(p, n))
        end
    end
end
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
(-)(y::Union(Period,CompoundPeriod),x::CompoundPeriod) = (-x) + y
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

# Fixed-value Periods (periods corresponding to a well-defined time interval,
# as opposed to variable calendar intervals like Year).
typealias FixedPeriod Union(Week,Day,Hour,Minute,Second,Millisecond)

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
typealias OtherPeriod Union(Month,Year)
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

