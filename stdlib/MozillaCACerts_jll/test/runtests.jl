# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using MozillaCACerts_jll

@testset "MozillaCACerts_jll" begin
    @test isfile(MozillaCACerts_jll.cacert)
end
