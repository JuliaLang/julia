# This file is a part of Julia. License is MIT: https://julialang.org/license

module Rebinding
    using Test

    @test Base.binding_kind(@__MODULE__, :Foo) == Base.BINDING_KIND_GUARD
    struct Foo
        x::Int
    end
    const defined_world_age = Base.tls_world_age()
    x = Foo(1)

    @test Base.binding_kind(@__MODULE__, :Foo) == Base.BINDING_KIND_CONST
    @test !contains(repr(x), "@world")
    Base.delete_binding(@__MODULE__, :Foo)

    @test Base.binding_kind(@__MODULE__, :Foo) == Base.BINDING_KIND_GUARD
    @test contains(repr(x), "@world")

    # Tests for @world syntax
    @test Base.@world(Foo, defined_world_age) == typeof(x)
    @test Base.@world(Rebinding.Foo, defined_world_age) == typeof(x)
    @test Base.@world((@__MODULE__).Foo, defined_world_age) == typeof(x)
end
