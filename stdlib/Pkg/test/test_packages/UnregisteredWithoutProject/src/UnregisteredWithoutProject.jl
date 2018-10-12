# This file is a part of Julia. License is MIT: https://julialang.org/license

module UnregisteredWithoutProject

if !isfile(joinpath(@__DIR__, "..", "deps", "deps.jl"))
    error("UnregisteredWithoutProject is not installed correctly")
end

export f
f(x) = x

end
