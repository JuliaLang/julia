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
    nameof(@__MODULE__) === :Rebinding && @test Base.@world(Rebinding.Foo, defined_world_age) == typeof(x)
    @test Base.@world((@__MODULE__).Foo, defined_world_age) == typeof(x)

    # Test invalidation (const -> undefined)
    const delete_me = 1
    f_return_delete_me() = delete_me
    @test f_return_delete_me() == 1
    Base.delete_binding(@__MODULE__, :delete_me)
    @test_throws UndefVarError f_return_delete_me()

    # + foreign module
    module NotTheDefinitionModule
        const delete_me_other = 2
    end
    @eval f_return_delete_me_foreign_module() = $(GlobalRef(NotTheDefinitionModule, :delete_me_other))
    @test f_return_delete_me_foreign_module() == 2
    Base.delete_binding(NotTheDefinitionModule, :delete_me_other)
    @test_throws UndefVarError f_return_delete_me_foreign_module()

    ## + via indirect access
    const delete_me = 3
    f_return_delete_me_indirect() = getglobal(@__MODULE__, :delete_me)
    @test f_return_delete_me_indirect() == 3
    Base.delete_binding(@__MODULE__, :delete_me)
    @test_throws UndefVarError f_return_delete_me_indirect()

    # + via generated function
    const delete_me = 4
    @generated f_generated_return_delete_me() = return :(delete_me)
    @test f_generated_return_delete_me() == 4
    Base.delete_binding(@__MODULE__, :delete_me)
    @test_throws UndefVarError f_generated_return_delete_me()
end

module RebindingPrecompile
    using Test
    include("precompile_utils.jl")

    precompile_test_harness("rebinding precompile") do load_path
        # Test that the system doesn't accidentally forget to revalidate a method without backedges
        write(joinpath(load_path, "LotsOfBindingsToDelete.jl"),
              """
              module LotsOfBindingsToDelete
                const delete_me_1 = 1
                const delete_me_2 = 2
                const delete_me_3 = 3
                const delete_me_4 = 4
              end
              """)
        Base.compilecache(Base.PkgId("LotsOfBindingsToDelete"))
        write(joinpath(load_path, "UseTheBindings.jl"),
              """
              module UseTheBindings
                using LotsOfBindingsToDelete
                @eval f_use_bindings1() = \$(GlobalRef(LotsOfBindingsToDelete, :delete_me_1))
                @eval f_use_bindings2() = \$(GlobalRef(LotsOfBindingsToDelete, :delete_me_2))
                f_use_bindings3() = LotsOfBindingsToDelete.delete_me_3
                f_use_bindings4() = LotsOfBindingsToDelete.delete_me_4
                # Code Instances for each of these
                @assert (f_use_bindings1(), f_use_bindings2(), f_use_bindings3(), f_use_bindings4()) ==
                    (1, 2, 3, 4)
              end
              """)
        Base.compilecache(Base.PkgId("UseTheBindings"))
        @eval using LotsOfBindingsToDelete
        # Delete some bindings before loading the dependent package
        Base.delete_binding(LotsOfBindingsToDelete, :delete_me_1)
        Base.delete_binding(LotsOfBindingsToDelete, :delete_me_3)
        # Load the dependent package
        @eval using UseTheBindings
        invokelatest() do
            @test_throws UndefVarError UseTheBindings.f_use_bindings1()
            @test UseTheBindings.f_use_bindings2() == 2
            @test_throws UndefVarError UseTheBindings.f_use_bindings3()
            @test UseTheBindings.f_use_bindings4() == 4
            # Delete remaining bindings
            Base.delete_binding(LotsOfBindingsToDelete, :delete_me_2)
            Base.delete_binding(LotsOfBindingsToDelete, :delete_me_4)
            invokelatest() do
                @test_throws UndefVarError UseTheBindings.f_use_bindings2()
                @test_throws UndefVarError UseTheBindings.f_use_bindings4()
            end
        end
    end

    finish_precompile_test!()
end
