# This file is a part of Julia. License is MIT: https://julialang.org/license

# 1.0 deprecations
@deprecate (+)(x::AbstractArray{<:TimeType}, y::GeneralPeriod) x .+ y
@deprecate (+)(x::StridedArray{<:GeneralPeriod}, y::TimeType)  x .+ y
@deprecate (+)(x::GeneralPeriod, y::AbstractArray{<:TimeType}) x .+ y
@deprecate (+)(x::TimeType, y::StridedArray{<:GeneralPeriod})  y .+ y
@deprecate (-)(x::AbstractArray{<:TimeType}, y::GeneralPeriod) x .- y
@deprecate (-)(x::StridedArray{<:GeneralPeriod}, y::TimeType)  x .- y
@deprecate (-)(x::AbstractArray{<:TimeType}, y::TimeType)      x .- y
@deprecate (-)(x::TimeType, y::AbstractArray{<:TimeType})      x .- y
