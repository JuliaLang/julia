# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Example
using BigProject

@test BigProject.f() == 1

# #306 Pkg.jl
cd("LibFoo.jl") do
    run(`$(Base.julia_cmd()) --project test/runtests.jl`)
end
