# This file is a part of Julia. License is MIT: http://julialang.org/license

ENV["PATH"] = JULIA_HOME*";"*joinpath(JULIA_HOME,"..","Git","bin")*";"*
    joinpath(JULIA_HOME,"..","Git","usr","bin")*";"*ENV["PATH"]
