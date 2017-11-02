# This file is a part of Julia. License is MIT: https://julialang.org/license

struct Nullable{T}
    hasvalue::Bool
    value::T

    Nullable{T}() where {T} = new(false)
    Nullable{T}(value, hasvalue::Bool=true) where {T} = new(hasvalue, value)
    Nullable{T}(::Void) where {T} = Nullable{T}()
    (::Type{Nullable{Void}})(::Void) = new{Void}(true, nothing)
end
