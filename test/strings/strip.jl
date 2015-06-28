# This file is a part of Julia. License is MIT: http://julialang.org/license

# string manipulation
@test strip("\t  hi   \n") == "hi"
