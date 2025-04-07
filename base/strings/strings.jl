# This file is a part of Julia. License is MIT: https://julialang.org/license

include("strings/annotated.jl")
include("strings/search.jl")
include("strings/unicode.jl")

import .Unicode: textwidth, islowercase, isuppercase, isletter, isdigit, isnumeric, iscntrl, ispunct,
    isspace, isprint, isxdigit, lowercase, uppercase, titlecase, lowercasefirst, uppercasefirst

import .Iterators: PartitionIterator

include("strings/util.jl")
include("strings/io.jl")
include("strings/annotated_io.jl")
