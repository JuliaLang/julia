# This file is a part of Julia. License is MIT: https://julialang.org/license

module Rebinding
    using Test

    @test Base.binding_kind(@__MODULE__, :Foo) == Base.BINDING_KIND_GUARD
    struct Foo
        x::Int
    end
    x = Foo(1)

    @test Base.binding_kind(@__MODULE__, :Foo) == Base.BINDING_KIND_CONST
    @test !contains(repr(x), "@world")
    Base.delete_binding(@__MODULE__, :Foo)

    @test Base.binding_kind(@__MODULE__, :Foo) == Base.BINDING_KIND_GUARD
    @test contains(repr(x), "@world")
end
