module SubModule2

if !isfile(joinpath(@__DIR__, "..", "deps", "buildartifact"))
    error("Package built incorrectly")
end

f() = 1

end # module
