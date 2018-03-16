module SubModule

if !isfile(joinpath(@__DIR__, "..", "deps", "buildartifact"))
    error("Package built incorrectly")
end

f() = 1

end # module
