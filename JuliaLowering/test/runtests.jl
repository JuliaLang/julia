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

    @testset "provenance" include("provenance.jl")
    @testset "compat" include("compat.jl")
    @testset "hooks" include("hooks.jl")

end

module BaseTestMod
using Test
# Currently many Base tests will fail for known reasons (e.g. checking error
# message is equal to a string), so `!(@isdefined TESTING_JULIALOWERING)` can be
# used to guard those.
const TESTING_JULIALOWERING = true
end

@testset "Base tests under JuliaLowering" begin
    try
        base_testdir = joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "test")
        function include_basetest(file)
            # println("JL $file")
            @testset "JL base test $file" begin
                try
                    BaseTestMod.include(joinpath(base_testdir, file))
                catch e
                    @test e === nothing
                end
            end
        end

        JuliaLowering.activate!()

        include_basetest("testenv.jl")

        include_basetest("enums.jl")
        include_basetest("triplequote.jl")
        include_basetest("apint.jl")
        include_basetest("atomics.jl")
        include_basetest("intrinsics.jl")
        include_basetest("operators.jl")
        include_basetest("version.jl")
        include_basetest("tuple.jl")
        include_basetest("functional.jl")
        include_basetest("ccall.jl")
        include_basetest("llvmcall.jl")
        include_basetest("llvmcall2.jl")
        include_basetest("specificity.jl")

    finally
        JuliaLowering.activate!(false)
    end
end
