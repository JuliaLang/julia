# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Experimental multithreading support.
"""
module Threads

include("threadingconstructs.jl")
include("atomics.jl")
include("locks.jl")

end
