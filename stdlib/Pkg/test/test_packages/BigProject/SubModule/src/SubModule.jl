# This file is a part of Julia. License is MIT: https://julialang.org/license

module SubModule

using RecursiveDep

buildfile = joinpath(@__DIR__, "..", "deps", "buildartifact")
if !isfile(buildfile)
    error("Package built incorrectly")
else
    include(buildfile)
end

f() = 1

end # module
