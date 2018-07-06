# This file is a part of Julia. License is MIT: https://julialang.org/license

__precompile__()
module UnregisteredWithoutProject

if filetype(joinpath(@__DIR__, "..", "deps", "deps.jl") != :file)
    error("UnregisteredWithoutProject is not installed correctly")
end

export f
f(x) = x

end
