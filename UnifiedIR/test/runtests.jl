using Test
using UnifiedIR

@testset "UnifiedIR" begin
    include("smoke.jl")
    include("attrgraph.jl")
    include("acceptance.jl")
    include("closures.jl")
    include("fuzz.jl")
    include("splice.jl")
    include("regression.jl")
end
