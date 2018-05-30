
using Test
using Example
using BigProject

@test BigProject.f() == 1

# #306 Pkg.jl
cd("LibFoo.jl") do
    run(`$(Base.julia_cmd()) test/runtests.jl`)
end