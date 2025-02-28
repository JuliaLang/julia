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

    # Test that it still works if Foo is redefined to a non-type
    const Foo = 1

    @test Base.binding_kind(@__MODULE__, :Foo) == Base.BINDING_KIND_CONST
    @test contains(repr(x), "@world")
    Base.delete_binding(@__MODULE__, :Foo)

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

    module DeleteMeModule
        export delete_me_implicit
        const delete_me_explicit = 5
        const delete_me_implicit = 6
    end

    # + via import
    using .DeleteMeModule: delete_me_explicit
    f_return_delete_me_explicit() = delete_me_explicit
    @test f_return_delete_me_explicit() == 5
    Base.delete_binding(DeleteMeModule, :delete_me_explicit)
    @test_throws UndefVarError f_return_delete_me_explicit()

    # + via using
    using .DeleteMeModule
    f_return_delete_me_implicit() = delete_me_implicit
    @test f_return_delete_me_implicit() == 6
    Base.delete_binding(DeleteMeModule, :delete_me_implicit)
    @test_throws UndefVarError f_return_delete_me_implicit()
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
                export delete_me_5
                const delete_me_5 = 5
                const delete_me_6 = 6
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
                f_use_bindings5() = delete_me_5
                import LotsOfBindingsToDelete: delete_me_6
                f_use_bindings6() = delete_me_6
                # Code Instances for each of these
                @assert (f_use_bindings1(), f_use_bindings2(), f_use_bindings3(),
                         f_use_bindings4(), f_use_bindings5(), f_use_bindings6()) ==
                    (1, 2, 3, 4, 5, 6)
              end
              """)
        Base.compilecache(Base.PkgId("UseTheBindings"))
        @eval using LotsOfBindingsToDelete
        invokelatest() do
            # Delete some bindings before loading the dependent package
            Base.delete_binding(LotsOfBindingsToDelete, :delete_me_1)
            Base.delete_binding(LotsOfBindingsToDelete, :delete_me_3)
        end
        # Load the dependent package
        @eval using UseTheBindings
        invokelatest() do
            @test_throws UndefVarError UseTheBindings.f_use_bindings1()
            @test UseTheBindings.f_use_bindings2() == 2
            @test_throws UndefVarError UseTheBindings.f_use_bindings3()
            @test UseTheBindings.f_use_bindings4() == 4
            @test UseTheBindings.f_use_bindings5() == 5
            @test UseTheBindings.f_use_bindings6() == 6
            # Delete remaining bindings
            Base.delete_binding(LotsOfBindingsToDelete, :delete_me_2)
            Base.delete_binding(LotsOfBindingsToDelete, :delete_me_4)
            Base.delete_binding(LotsOfBindingsToDelete, :delete_me_5)
            Base.delete_binding(LotsOfBindingsToDelete, :delete_me_6)
            invokelatest() do
                @test_throws UndefVarError UseTheBindings.f_use_bindings2()
                @test_throws UndefVarError UseTheBindings.f_use_bindings4()
                @test_throws UndefVarError UseTheBindings.f_use_bindings5()
                @test_throws UndefVarError UseTheBindings.f_use_bindings6()
            end
        end
    end

    precompile_test_harness("export change") do load_path
        write(joinpath(load_path, "Export1.jl"),
              """
              module Export1
                export import_me1
                const import_me1 = 11
                export import_me2
                const import_me2 = 12
              end
              """)
        write(joinpath(load_path, "Export2.jl"),
              """
              module Export2
              end
              """)
        write(joinpath(load_path, "ImportTest.jl"),
              """
              module ImportTest
                using Export1, Export2
                f_use_binding1() = import_me1
                f_use_binding2() = import_me2
                @assert f_use_binding1() == 11
                @assert f_use_binding2() == 12

            end
              """)
        @eval using Export1
        @eval using Export2
        # Change the import resolution for ImportTest
        invokelatest() do
            Core.eval(Export2, :(export import_me1))
            Core.eval(Export2, :(const import_me1 = 21))
        end
        @eval using ImportTest
        invokelatest() do
            @test_throws UndefVarError ImportTest.f_use_binding1()
            @test ImportTest.f_use_binding2() == 12
        end
        invokelatest() do
            Core.eval(Export2, :(export import_me2))
            Core.eval(Export2, :(const import_me2 = 22))
        end
        invokelatest() do
            @test_throws UndefVarError ImportTest.f_use_binding2()
        end
    end

    finish_precompile_test!()
end

module Regression
    using Test

    # Issue #57377
    module GeoParams57377
        module B
            using ...GeoParams57377
            export S
            struct S end
            module C
                using ..GeoParams57377
                h() = S()
                x -> nothing
            end
        end

        using .B
        export S
    end
    @test GeoParams57377.B.C.h() == GeoParams57377.B.C.S()
end

# Test that the validation bypass fast path is not defeated by loading InteractiveUtils
@test parse(UInt, readchomp(`$(Base.julia_cmd()) -e 'using InteractiveUtils; show(unsafe_load(cglobal(:jl_first_image_replacement_world, UInt)))'`)) == typemax(UInt)

# Test that imported module binding backedges are still added in a new module that has the fast path active
let test_code =
    """
    using Test
    @assert unsafe_load(cglobal(:jl_first_image_replacement_world, UInt)) == typemax(UInt)
    include("precompile_utils.jl")

    precompile_test_harness("rebinding precompile") do load_path
        write(joinpath(load_path, "LotsOfBindingsToDelete2.jl"),
              "module LotsOfBindingsToDelete2
                 const delete_me_6 = 6
               end")
        Base.compilecache(Base.PkgId("LotsOfBindingsToDelete2"))
        write(joinpath(load_path, "UseTheBindings2.jl"),
              "module UseTheBindings2
                 import LotsOfBindingsToDelete2: delete_me_6
                 f_use_bindings6() = delete_me_6
                 # Code Instances for each of these
                 @assert (f_use_bindings6(),) == (6,)
               end")
        Base.compilecache(Base.PkgId("UseTheBindings2"))
        @eval using LotsOfBindingsToDelete2
        @eval using UseTheBindings2
        invokelatest() do
            @test UseTheBindings2.f_use_bindings6() == 6
            Base.delete_binding(LotsOfBindingsToDelete2, :delete_me_6)
            invokelatest() do
                @test_throws UndefVarError UseTheBindings2.f_use_bindings6()
            end
        end
    end

    finish_precompile_test!()
    """
    @test success(pipeline(`$(Base.julia_cmd()) -e $test_code`; stderr))
end

# Image Globalref smoke test
module ImageGlobalRefFlag
    using Test
    @eval fimage() = $(GlobalRef(Base, :sin))
    fnoimage() = x
    @test Base.has_image_globalref(first(methods(fimage)))
    @test !Base.has_image_globalref(first(methods(fnoimage)))
end
