@testset "Miscellanous" begin

test_mod = Module()

# Blocks
@test JuliaLowering.include_string(test_mod, """
begin
end
""") == nothing

# Placeholders
@test JuliaLowering.include_string(test_mod, """_ = 10""") == 10

assign_underscore = parsestmt(SyntaxTree, "_ + 1", filename="foo.jl")
exc = try
    JuliaLowering.eval(test_mod, assign_underscore)
catch exc
    exc
end
@test exc.msg == "all-underscore identifiers are write-only and their values cannot be used in expressions"
@test JuliaLowering.is_ancestor(exc.ex, assign_underscore[1])

end
