module SubModule

buildfile = joinpath(@__DIR__, "..", "deps", "buildartifact")
if !isfile(buildfile)
    error("Package built incorrectly")
else
    include(buildfile)
end

f() = 1

end # module
