module RelocationTestPkg1

greet() = print("Hello World!")

include(joinpath(@__RELOCDIR__, "..", "deps", "bar.jl"))

end # module RelocationTestPkg1
