# Just parse some file as a precompile workload
let filename = joinpath(@__DIR__, "literal_parsing.jl")
    text = read(filename, String)
    parseall(Expr, text)
    parseall(SyntaxNode, text)
    if _has_v1_6_hooks
        enable_in_core!()
        Meta.parse("1 + 2")
        enable_in_core!(false)
    end
end
