using Test
using MozillaCACerts_jll

@testset "MozillaCACerts_jll" begin
    @test isfile(MozillaCACerts_jll.cacert)
end
