# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base: @deprecate, depwarn, show_unquoted, remove_linenums!

# 1.0 deprecations
function (+)(x::AbstractArray{<:TimeType}, y::GeneralPeriod)
    # depwarn("non-broadcasted arithmetic is deprecated for Dates.TimeType; use broadcasting instead", nothing)
    x .+ y
end
function (+)(x::StridedArray{<:GeneralPeriod}, y::TimeType)
    # depwarn("non-broadcasted arithmetic is deprecated for Dates.TimeType; use broadcasting instead", nothing)
    x .+ y
end
function (+)(y::GeneralPeriod, x::AbstractArray{<:TimeType})
    # depwarn("non-broadcasted arithmetic is deprecated for Dates.TimeType; use broadcasting instead", nothing)
    x .+ y
end
function (+)(y::TimeType, x::StridedArray{<:GeneralPeriod})
    # depwarn("non-broadcasted arithmetic is deprecated for Dates.TimeType; use broadcasting instead", nothing)
    x .+ y
end
function (-)(x::AbstractArray{<:TimeType}, y::GeneralPeriod)
    # depwarn("non-broadcasted arithmetic is deprecated for Dates.TimeType; use broadcasting instead", nothing)
    x .- y
end
function (-)(x::StridedArray{<:GeneralPeriod}, y::TimeType)
    # depwarn("non-broadcasted arithmetic is deprecated for Dates.TimeType; use broadcasting instead", nothing)
    x .- y
end

# TimeType, AbstractArray{TimeType}
function (-)(x::AbstractArray{T}, y::T) where {T<:TimeType}
    # depwarn("non-broadcasted arithmetic is deprecated for Dates.TimeType; use broadcasting instead", nothing)
    x .- y
end
function (-)(y::T, x::AbstractArray{T}) where {T<:TimeType}
    # depwarn("non-broadcasted arithmetic is deprecated for Dates.TimeType; use broadcasting instead", nothing)
    y .- x
end

for (op, Ty, Tz) in ((:*, Real, :P),
                   (:/, :P, Float64), (:/, Real, :P))
    @eval begin
        function ($op)(X::StridedArray{P}, y::$Ty) where P<:Period
            # depwarn("non-broadcasted arithmetic is deprecated for Dates.TimeType; use broadcasting instead", nothing)
            Z = similar(X, $Tz)
            for (Idst, Isrc) in zip(eachindex(Z), eachindex(X))
                @inbounds Z[Idst] = ($op)(X[Isrc], y)
            end
            return Z
        end
    end
end

function (+)(x::StridedArray{<:GeneralPeriod})
    # depwarn("non-broadcasted operations are deprecated for Dates.TimeType; use broadcasting instead", nothing)
    x
end

for op in (:+, :-)
    @eval begin
        function ($op)(X::StridedArray{<:GeneralPeriod}, Y::StridedArray{<:GeneralPeriod})
            # depwarn("non-broadcasted arithmetic is deprecated for Dates.TimeType; use broadcasting instead", nothing)
            reshape(CompoundPeriod[($op)(x, y) for (x, y) in zip(X, Y)], promote_shape(size(X), size(Y)))
        end
    end
end

@deprecate argerror(msg::String) ArgumentError(msg) false
@deprecate argerror() nothing false

# This is needed as the Base.@deprecate cannot work on overloaded funtion ie Base.whatever
macro simple_deprecate(old, new)
    meta = Expr(:meta, :noinline)
    remove_linenums!(new)
    oldcall = sprint(show_unquoted, old)
    newcall = sprint(show_unquoted, new)
    Expr(:toplevel,
    :($(esc(old)) = begin
        #$meta
        #depwarn($"`$oldcall` is deprecated, use `$newcall` instead.", Core.Typeof($(esc(oldsym))).name.mt.name)
        $(esc(new))
    end))
end

@simple_deprecate(Base.floor(dt::Date, p::Year) , Base.floor(p, dt)) #@depreciate
@simple_deprecate(Base.floor(dt::Date, p::Month), Base.floor(p, dt))
@simple_deprecate(Base.floor(dt::Date, p::Quarter), Base.floor(p, dt))
@simple_deprecate(Base.floor(dt::Date, p::Week), Base.floor(p, dt))
@simple_deprecate(Base.floor(dt::Date, p::Day), Base.floor(p, dt))

@simple_deprecate(Base.floor(dt::DateTime, p::DatePeriod), Base.floor(p, dt))
@simple_deprecate(Base.floor(dt::DateTime, p::TimePeriod), Base.floor(p, dt))

@simple_deprecate(Base.floor(x::ConvertiblePeriod, precision::T)  where T <: ConvertiblePeriod, Base.floor(precision, x))

@simple_deprecate(Base.ceil(dt::TimeType, p::Period), Base.ceil(p, dt))
@simple_deprecate(Base.ceil(x::ConvertiblePeriod, precision::ConvertiblePeriod), Base.ceil(precision, x))

@simple_deprecate(Base.round(dt::TimeType,  p::Period, r::RoundingMode{:NearestTiesUp}), Base.round(p, dt, r))
# The definitions were excluded due to ambiguities, and it didn't effect test results
# @simple_deprecate(Base.round(x::ConvertiblePeriod, precision::ConvertiblePeriod, r::RoundingMode{:NearestTiesUp}), Base.round(precision, x, r))
# @simple_deprecate(Base.round(x::TimeTypeOrPeriod, p::Period, r::RoundingMode{:Down}), Base.round(p,x,r))
# @simple_deprecate(Base.round(x::TimeTypeOrPeriod, p::Period, r::RoundingMode{:Up}), Base.round(p,x,r))

# This had to be replace with line below to limit ambiguities
# @simple_deprecate(# Base.round(x::TimeTypeOrPeriod, p::Period), Base.round(p, x, RoundNearestTiesUp))
@simple_deprecate(Base.round(x::TimeType, p::Period), Base.round(p, x))

@simple_deprecate(Base.floor(x::TimeTypeOrPeriod, p::Type{P}) where P <: Period, Base.floor(p, x))
@simple_deprecate(Base.ceil(x::TimeTypeOrPeriod, p::Type{P}) where P <: Period, Base.ceil(p, x))

@simple_deprecate(Base.floor(::Type{Date}, x::TimeTypeOrPeriod, ::Type{P}) where P <: Period, Base.floor(oneunit(P), Date(x)))
@simple_deprecate(Base.ceil(::Type{Date}, x::TimeTypeOrPeriod, ::Type{P}) where P <: Period, Base.ceil(oneunit(P), Date(x)))

@simple_deprecate(Base.round(x::TimeTypeOrPeriod, ::Type{P}, r::RoundingMode=RoundNearestTiesUp) where P <: Period, Base.round(oneunit(P), x, r))
@simple_deprecate(Base.round(::Type{Date}, x::TimeTypeOrPeriod, ::Type{P}, r::RoundingMode=RoundNearestTiesUp) where P <: Period, Base.round(oneunit(P), Date(x), r))

