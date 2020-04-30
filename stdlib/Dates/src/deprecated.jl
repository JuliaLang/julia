# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base: @deprecate, depwarn

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
