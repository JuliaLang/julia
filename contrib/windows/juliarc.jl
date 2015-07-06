# This file is a part of Julia. License is MIT: http://julialang.org/license

let user_data_dir
    ENV["PATH"] = JULIA_HOME*";"*joinpath(JULIA_HOME,"..","Git","bin")*";"*ENV["PATH"]
    #haskey(ENV,"JULIA_EDITOR") || (ENV["JULIA_EDITOR"] = "start") #start is not a program, so this doesn't work
end
