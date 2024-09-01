using Test

include("utils.jl")

@testset "JuliaLowering.jl" begin

    include("syntax_graph.jl")

    include("misc.jl")
    include("import.jl")
    include("scopes.jl")
    include("functions.jl")
    include("decls.jl")
    include("macros.jl")
    include("modules.jl")
    include("desugaring.jl")
    include("branching.jl")
    include("loops.jl")

end
