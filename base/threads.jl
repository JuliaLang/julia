# This file is a part of Julia. License is MIT: https://julialang.org/license

module Threads
@doc """
Experimental multithreading support.
""" -> Threads

include("threadingconstructs.jl")
include("atomics.jl")
include("locks.jl")

end
