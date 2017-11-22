# 0.7 deprecations

import Base.colon
import Base.range

# deprecate remaining vectorized methods from Dates
@deprecate(
    DateTime(Y::AbstractArray{<:AbstractString}, f::AbstractString; locale::Locale=ENGLISH),
    DateTime.(Y, f; locale=locale) )
@deprecate(
    DateTime(Y::AbstractArray{<:AbstractString}, df::DateFormat=ISODateTimeFormat),
    DateTime.(Y, df) )
@deprecate(
    Date(Y::AbstractArray{<:AbstractString}, f::AbstractString; locale::Locale=ENGLISH),
    Date.(Y, f; locale=locale) )
@deprecate(
    Date(Y::AbstractArray{<:AbstractString}, df::DateFormat=ISODateFormat),
    Date.(Y, df) )
@deprecate(
    format(Y::AbstractArray{<:TimeType}, f::AbstractString; locale::Locale=ENGLISH),
    format.(Y, f; locale=locale) )
@deprecate(
    format(Y::AbstractArray{T}, df::DateFormat=default_format(T)) where {T<:TimeType},
    format.(Y, df) )

@deprecate +(a::GeneralPeriod, b::StridedArray{<:GeneralPeriod}) broadcast(+, a, b)
@deprecate +(a::StridedArray{<:GeneralPeriod}, b::GeneralPeriod) broadcast(+, a, b)
@deprecate -(a::GeneralPeriod, b::StridedArray{<:GeneralPeriod}) broadcast(-, a, b)
@deprecate -(a::StridedArray{<:GeneralPeriod}, b::GeneralPeriod) broadcast(-, a, b)

# #24258
# Physical units define an equivalence class: there is no such thing as a step of "1" (is
# it one day or one second or one nanosecond?). So require the user to specify the step
# (in physical units).
@deprecate colon(start::T, stop::T) where {T<:DateTime}   start:Day(1):stop
@deprecate colon(start::T, stop::T) where {T<:Date}       start:Day(1):stop
@deprecate colon(start::T, stop::T) where {T<:Time}       start:Second(1):stop

@deprecate range(start::DateTime, len::Integer)  range(start, Day(1), len)
@deprecate range(start::Date, len::Integer)      range(start, Day(1), len)

