import JuliaSyntax
Base.include(@__MODULE__(), joinpath(pkgdir(JuliaSyntax), "test", "test_utils.jl"))
Base.include(@__MODULE__(), joinpath(pkgdir(JuliaSyntax), "test", "parser.jl"))
JuliaSyntax.enable_in_core!()
Meta.parse("x+y+z-w .+ [a b c]")
