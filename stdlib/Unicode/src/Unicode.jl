# This file is a part of Julia. License is MIT: https://julialang.org/license

__precompile__(true)

module Unicode

using Base.Unicode: normalize, graphemes, isassigned, iscased

export graphemes

# BEGIN 0.7 deprecations

@deprecate is_assigned_char(c::Char) Unicode.isassigned(c)
@deprecate normalize_string(s::AbstractString, nf::Symbol; kwargs...) Unicode.normalize(s, nf; kwargs...)
@deprecate normalize_string(s::AbstractString; kwargs...) Unicode.normalize(s; kwargs...)

# END 0.7 deprecations

end
