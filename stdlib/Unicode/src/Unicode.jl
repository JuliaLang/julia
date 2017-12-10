# This file is a part of Julia. License is MIT: https://julialang.org/license

__precompile__(true)

module Unicode

using Base.Unicode: normalize_string, graphemes, is_assigned_char, textwidth, isvalid,
                    islower, isupper, isalpha, isdigit, isxdigit, isnumber, isalnum,
                    iscntrl, ispunct, isspace, isprint, isgraph,
                    lowercase, uppercase, titlecase, lcfirst, ucfirst

export normalize_string, graphemes, is_assigned_char, textwidth, isvalid,
       islower, isupper, isalpha, isdigit, isxdigit, isnumber, isalnum,
       iscntrl, ispunct, isspace, isprint, isgraph,
       lowercase, uppercase, titlecase, lcfirst, ucfirst

end
