# Just parse some file as a precompile workload
let filename = joinpath(@__DIR__, "julia/literal_parsing.jl")
    text = read(filename, String)
    parseall(Expr, text)
    parseall(SyntaxNode, text)
    if _has_v1_6_hooks
        enable_in_core!()
        Meta.parse("1 + 2")
        Meta.parse(SubString("1 + 2"))
        # Versioned modules call the hook with an explicit syntax version
        Core._parse("1 + 2", "none", 1, 0, :statement; syntax_version=SYNTAX_VERSION)
        enable_in_core!(false)
    end
end
