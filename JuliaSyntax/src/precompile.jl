# Just parse some file as a precompile workload
let filename = joinpath(@__DIR__, "literal_parsing.jl")
    text = read(filename, String)
    parseall(Expr, text)
end
