# This file should contain site-specific commands to be executed on Julia startup
# Users should store their own personal commands in homedir(), in a file named .juliarc.jl

ENV["PATH"] = JULIA_HOME*";"*joinpath(JULIA_HOME,"..","Git","bin")*";"*ENV["PATH"]
