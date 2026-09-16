# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Threads

Module providing multithreading tools.
"""
module Threads

global Condition # we'll define this later, make sure we don't import Base.Condition

include("threadingconstructs.jl")
include("locks-mt.jl")

end
