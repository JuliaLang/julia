# This file is a part of Julia. License is MIT: https://julialang.org/license

Base.ACTIVE_PROJECT[] = joinpath(@__DIR__, "..")
using Documenter
using Pkg

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
