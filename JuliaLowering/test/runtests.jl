using Test

include("utils.jl")

@testset "JuliaLowering.jl" begin
    include("syntax_graph.jl")

    include("ir_tests.jl")

    include("arrays.jl")
    include("assignments.jl")
    include("branching.jl")
    include("closures.jl")
    include("decls.jl")
    include("destructuring.jl")
    include("desugaring.jl")
    include("exceptions.jl")
    include("functions.jl")
    include("generators.jl")
    include("import.jl")
    include("loops.jl")
    @testset "macros" include("macros.jl")
    include("misc.jl")
    include("modules.jl")
    include("quoting.jl")
    include("scopes.jl")
    include("typedefs.jl")
    include("compat.jl")
end
