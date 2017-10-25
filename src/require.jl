push!(empty!(LOAD_PATH), dirname(dirname(@__DIR__)))
Base.require(:Pkg3)
