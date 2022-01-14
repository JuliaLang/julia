using Documenter, JuliaSyntax

makedocs(;
    modules=[JuliaSyntax],
    format=Documenter.HTML(),
    pages=[
        "Overview" => "index.md",
        "API Reference" => "reference.md",
        "Design Discussion" => "design.md",
    ],
    repo="https://github.com/c42f/JuliaSyntax.jl/blob/{commit}{path}#L{line}",
    sitename="JuliaSyntax.jl",
    authors = "Chris Foster and contributors: https://github.com/c42f/JuliaSyntax.jl/graphs/contributors"
)

deploydocs(;
    repo="github.com/c42f/JuliaSyntax.jl",
    push_preview=true
)
