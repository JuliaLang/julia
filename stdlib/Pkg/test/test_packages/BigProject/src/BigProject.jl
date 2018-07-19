# This file is a part of Julia. License is MIT: https://julialang.org/license

module BigProject

buildfile = joinpath(@__DIR__, "..", "deps", "buildartifact")
if !isfile(buildfile)
    error("Package built incorrectly")
else
    include(buildfile)
end

using SubModule
using SubModule2
using Random

f() = 1

end # module
