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

@test_throws LoweringError JuliaLowering.include_string(test_mod, """
(a=1; b=2, c=3)
""")

@test_throws LoweringError JuliaLowering.include_string(test_mod, """
(; a=xs...)
""")

@test_throws LoweringError JuliaLowering.include_string(test_mod, """
(; a[]=1)
""")

@test_throws LoweringError JuliaLowering.include_string(test_mod, """
(; a."b")
""")

@test_throws LoweringError JuliaLowering.include_string(test_mod, """
(; a=1, f())
""")

# repeated field name
@test_throws LoweringError JuliaLowering.include_string(test_mod, """
(; a=1, bs..., c=3, a=2)
""")

end
