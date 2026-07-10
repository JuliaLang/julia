# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Future

@test isempty(Test.detect_closure_boxes(Future))

@testset "Docstrings" begin
    @test isempty(Docs.undocumented_names(Future))
end
