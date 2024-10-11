# This file is a part of Julia. License is MIT: https://julialang.org/license

module ArtifactDownloadsTests

import ArtifactDownloads
using Test

@testset "ArtifactDownloads" begin
    @testset "platformengines.jl" begin
        include("platformengines.jl")
    end
end

end
