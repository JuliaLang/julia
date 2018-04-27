# This file is a part of Julia. License is MIT: https://julialang.org/license

include("strings/substring.jl")
include("strings/search.jl")
include("strings/unicode.jl")

import .Unicode: textwidth, islowercase, isuppercase, isalpha, isdigit, isnumeric, iscntrl, ispunct,
    isspace, isprint, isxdigit, lowercase, uppercase, titlecase, lowercasefirst, uppercasefirst

include("strings/util.jl")
include("strings/io.jl")
