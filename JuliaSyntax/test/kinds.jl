# Only test this once per session, as kind modules must be unique (ugh)
if !isdefined(@__MODULE__, :FooKinds)
@eval module FooKinds

using ..JuliaSyntax

function _init_kinds()
    JuliaSyntax.register_kinds!(@__MODULE__, 42, [
        "BEGIN_FOO"
        "foo_1"
        "foo_2"
        "BEGIN_FOOBAR"
        "foobar_1"
        "foobar_2"
        "END_FOOBAR"
        "END_FOO"
    ])
end

_init_kinds()

k_before_init = K"foo_1"

function __init__()
    _init_kinds()
end

end

@eval module BarKinds
    # Intentionally empty
end

end

@testset "Kinds" begin
    @test K"foo_1" != K"foo_2"

    @test FooKinds.k_before_init == K"foo_1"

    @test K"BEGIN_FOO" == K"foo_1"
    @test K"foo_2" < K"BEGIN_FOOBAR"
    @test K"BEGIN_FOOBAR" == K"foobar_1"
    @test K"END_FOOBAR" == K"foobar_2"
    @test K"END_FOO" == K"foobar_2"

    @test parentmodule(K"foo_1") == FooKinds
    @test sprint(show, K"foo_1") == "K\"foo_1\""

    # Too many kind modules
    @test_throws ErrorException JuliaSyntax.register_kinds!(BarKinds, 64, ["hoo?"])
    # Too many kind names per module
    @test_throws ErrorException JuliaSyntax.register_kinds!(BarKinds, 42, string.(1:1024))
    # Re-registering or registering new kinds is not supported
    @test_throws ErrorException JuliaSyntax.register_kinds!(FooKinds, 42, ["foo_2", "foo_1"])
    @test_throws ErrorException JuliaSyntax.register_kinds!(FooKinds, 42, ["foo_3"])
    # Module ID already taken by FooKinds
    @test_throws ErrorException JuliaSyntax.register_kinds!(BarKinds, 42, ["hii?"])
end
