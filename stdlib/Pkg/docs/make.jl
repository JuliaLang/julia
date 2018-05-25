using Documenter, Pkg

makedocs(
    modules = [Pkg],
    format = :html,
    sitename = "Pkg.jl",
    pages = Any[
        "Pkg" => "index.md",
    ]
)

deploydocs(
    repo = "github.com/JuliaLang/Pkg.jl",
    target = "build",
    julia = "nightly",
    deps = nothing,
    make = nothing,
)
