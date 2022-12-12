# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base: @deprecate
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

import Base: ceil, round, floor

Base.@deprecate(round(dt::TimeType,  p::Period, r::RoundingMode{:NearestTiesUp}), round(p, dt, r))

Base.@deprecate(round(x::TimeType, p::Period), round(p, x))

Base.@deprecate(floor(x::TimeTypeOrPeriod, p::Type{P}) where P <: Period, floor(p, x))

Base.@deprecate(ceil(x::TimeTypeOrPeriod, p::Type{P}) where P <: Period, ceil(p, x))

Base.@deprecate(floor(::Type{Date}, x::TimeTypeOrPeriod, ::Type{P}) where P <: Period, floor(oneunit(P), Date(x)))

Base.@deprecate(ceil(::Type{Date}, x::TimeTypeOrPeriod, ::Type{P}) where P <: Period, ceil(oneunit(P), Date(x)))

Base.@deprecate(round(x::TimeTypeOrPeriod, ::Type{P}, r::RoundingMode=RoundNearestTiesUp) where P <: Period, round(oneunit(P), x, r))

Base.@deprecate(round(::Type{Date}, x::TimeTypeOrPeriod, ::Type{P}, r::RoundingMode=RoundNearestTiesUp) where P <: Period, round(oneunit(P), Date(x), r))