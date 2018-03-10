using Documenter, Pkg3

makedocs(
    modules = [Pkg3],
    format = :html,
    sitename = "Pkg3.jl",
    pages = Any[
        "Pkg3" => "index.md",
    ]
)

deploydocs(
    repo = "github.com/JuliaLang/Pkg3.jl",
    target = "build",
    julia = "nightly",
    deps = nothing,
    make = nothing,
)
