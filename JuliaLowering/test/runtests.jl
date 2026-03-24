using Test, JuliaLowering

@test isempty(Test.detect_closure_boxes(JuliaLowering))

include("utils.jl")

@testset "JuliaLowering.jl" begin
    include("ir_tests.jl")

    @testset "ast" include("ast.jl")

    @testset "validation" include("validation.jl")

    @testset "arrays" include("arrays.jl")
    @testset "assignments" include("assignments.jl")
    @testset "branching" include("branching.jl")
    @testset "closures" include("closures.jl")
    @testset "decls" include("decls.jl")
    @testset "destructuring" include("destructuring.jl")
    @testset "exceptions" include("exceptions.jl")
    @testset "functions" include("functions.jl")
    @testset "generators" include("generators.jl")
    @testset "import" include("import.jl")
    @testset "loops" include("loops.jl")
    @testset "macros" include("macros.jl")
    @testset "misc" include("misc.jl")
    @testset "modules" include("modules.jl")
    @testset "quoting" include("quoting.jl")
    @testset "scopes" include("scopes.jl")
    @testset "typedefs" include("typedefs.jl")

    @testset "compat" include("compat.jl")
    @testset "hooks" include("hooks.jl")
end
