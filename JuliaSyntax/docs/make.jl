using Documenter, JuliaSyntax

makedocs(;
    modules=[JuliaSyntax],
    format=Documenter.HTML(),
    pages=[
        "Overview" => "index.md"
        "How To" => "howto.md"
        "Reference" => [
            "reference.md"
            "api.md"
        ]
        "Design Discussion" => "design.md"
    ],
    repo="https://github.com/JuliaLang/JuliaSyntax.jl/blob/{commit}{path}#L{line}",
    sitename="JuliaSyntax.jl",
    authors = "Claire Foster and contributors: https://github.com/JuliaLang/JuliaSyntax.jl/graphs/contributors",
    strict = Documenter.except(:missing_docs)
)

deploydocs(;
    repo="github.com/JuliaLang/JuliaSyntax.jl",
    push_preview=true
)
