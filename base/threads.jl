# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Multithreading support.
"""
module Threads

global Condition # we'll define this later, make sure we don't import Base.Condition

include("threadingconstructs.jl")
include("locks-mt.jl")

end
