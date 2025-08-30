# This file is a part of Julia. License is MIT: https://julialang.org/license

# Interface functions defined in `Base`: define any that are not defined yet.
for c ∈ Symbol[
    :propertynames, :getproperty, :setproperty!,
    :show, :print,
    :nextind, :prevind, :thisind,
    :length, :iterate, :eltype, :size, :axes, :isdone, :isempty,
    :firstindex, :lastindex, :getindex, :setindex!,
    :copy, :copyto!,
    :isone, :iszero,
    :strides, :stride, :elsize,
    :ndims, :one, :zero, :oneunit, :widen,
    :promote_rule, :convert,
    :similar,
    :+, :-, :*, :/, ://, :<<, :>>, :>>>, :div, :fld, :cld,
]
    @eval function $c end
end

# Disable world splitting for callables to which users should add new methods.
for c ∈ Any[
    propertynames, getproperty, setproperty!,
    show, print,
    nextind, prevind, thisind,
    length, iterate, size, axes, isdone, isempty,
    firstindex, lastindex, getindex, setindex!,
    copy, :copyto!,
    isone, iszero,
    strides, stride,
    +, -, *, /, //, <<, >>, >>>, div, fld, cld,
]
    Base._stable_typeof(c).name.max_methods = 0x1
end

# Callables which take type arguments and need a method for the bottom type need a
# `max_methods` value of two for good inference, because the bottom type subtypes
# each type.
#
# TODO: add `eltype`
for c ∈ Any[
    elsize,
    ndims, one, zero, oneunit, widen,
    promote_rule, convert,
    similar,
]
    Base._stable_typeof(c).name.max_methods = 0x2
end
