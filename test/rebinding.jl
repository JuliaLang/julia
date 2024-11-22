# This file is a part of Julia. License is MIT: https://julialang.org/license

module Rebinding
    using Test

    make_foo() = Foo(1)

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

    struct Foo
        x::Int
    end
    @test Foo != typeof(x)

    # This tests that the compiler uses the correct world, but does not test
    # invalidation.
    @test typeof(Base.invoke_in_world(defined_world_age, make_foo)) == typeof(x)
    @test typeof(make_foo()) == Foo

    # Tests for @world syntax
    @test Base.@world(Foo, defined_world_age) == typeof(x)
    @test Base.@world(Rebinding.Foo, defined_world_age) == typeof(x)
    @test Base.@world((@__MODULE__).Foo, defined_world_age) == typeof(x)

    # Test invalidation (const -> undefined)
    const delete_me = 1
    f_return_delete_me() = delete_me
    @test f_return_delete_me() == 1
    Base.delete_binding(@__MODULE__, :delete_me)
    @test_throws UndefVarError f_return_delete_me()
end
