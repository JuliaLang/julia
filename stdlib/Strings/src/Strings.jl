# This file is a part of Julia. License is MIT: https://julialang.org/license

__precompile__(true)

module Strings

using Base.Unicode: normalize, graphemes, isassigned, textwidth, isvalid,
                    islower, isupper, isalpha, isdigit, isxdigit, isnumeric, isalnum,
                    iscntrl, ispunct, isspace, isprint, isgraph,
                    lowercase, uppercase, titlecase, lcfirst, ucfirst

export graphemes, textwidth, isvalid,
       islower, isupper, isalpha, isdigit, isxdigit, isnumeric, isalnum,
       iscntrl, ispunct, isspace, isprint, isgraph,
       lowercase, uppercase, titlecase, lcfirst, ucfirst

# BEGIN 0.7 deprecations

@deprecate isnumber(c::Char) Strings.isnumeric(c)
@deprecate is_assigned_char(c::Char) Strings.isassigned(c)
@deprecate normalize_string(s::AbstractString, nf::Symbol; kwargs...) Strings.normalize(s, nf; kwargs...)
@deprecate normalize_string(s::AbstractString; kwargs...) Strings.normalize(s; kwargs...)

# END 0.7 deprecations

end
