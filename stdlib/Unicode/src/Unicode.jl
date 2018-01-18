# This file is a part of Julia. License is MIT: https://julialang.org/license

__precompile__(true)

module Unicode

using Base.Unicode: normalize, graphemes, isassigned, iscased, escape, unescape

export graphemes

# BEGIN 0.7 deprecations

@deprecate is_assigned_char(c::Char) Unicode.isassigned(c)
@deprecate normalize_string(s::AbstractString, nf::Symbol; kwargs...) Unicode.normalize(s, nf; kwargs...)
@deprecate normalize_string(s::AbstractString; kwargs...) Unicode.normalize(s; kwargs...)

@deprecate escape_string(s::AbstractString) Unicode.escape(s)
@deprecate escape_string(s::AbstractString, esc::AbstractString) Unicode.escape(s, esc)
@deprecate escape_string(io, s::AbstractString) print(io, Unicode.escape(s))
@deprecate escape_string(io, s::AbstractString, esc::AbstractString) print(io, Unicode.escape(s, esc))
@deprecate unescape_string(s::AbstractString) Unicode.unescape(s)
@deprecate unescape_string(io, s::AbstractString) print(io, Unicode.unescape(s))

# END 0.7 deprecations

end
