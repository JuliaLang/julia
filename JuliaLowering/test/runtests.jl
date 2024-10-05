using Test

include("utils.jl")

@testset "JuliaLowering.jl" begin

    include("syntax_graph.jl")

    include("ir_tests.jl")

    include("branching.jl")
    include("decls.jl")
    include("desugaring.jl")
    include("exceptions.jl")
    include("functions.jl")
    include("import.jl")
    include("loops.jl")
    include("macros.jl")
    include("misc.jl")
    include("modules.jl")
    include("quoting.jl")
    include("scopes.jl")
    include("typedefs.jl")

end
