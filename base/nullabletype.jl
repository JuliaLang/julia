# This file is a part of Julia. License is MIT: https://julialang.org/license

struct Nullable{T}
    hasvalue::Bool
    value::T

    Nullable{T}() where {T} = new(false)
    Nullable{T}(value::T, hasvalue::Bool=true) where {T} = new(hasvalue, value)
end
