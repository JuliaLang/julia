# This file is a part of Julia. License is MIT: https://julialang.org/license

using Documenter
import TOML

makedocs(
    modules = [TOML],
    sitename = "TOML",
    checkdocs = :strict,
    doctest = true,
    pages = Any[
        "TOML" => "index.md"
        ]
    )

deploydocs(repo = "github.com/KristofferC/TOML.jl.git")
