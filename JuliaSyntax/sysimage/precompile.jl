function precompile_JuliaSyntax(mod, juliasyntax_path)
    Base.include(mod, joinpath(juliasyntax_path, "test", "test_utils.jl"))
    Base.include(mod, joinpath(juliasyntax_path, "test", "parser.jl"))
    JuliaSyntax.enable_in_core!()
    Meta.parse("x+y+z-w .+ [a b c]")
end
