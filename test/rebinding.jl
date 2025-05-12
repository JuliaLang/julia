# This file is a part of Julia. License is MIT: https://julialang.org/license

module Rebinding
    using Test
    make_foo() = Foo(1)

    @test Base.binding_kind(@__MODULE__, :Foo) == Base.PARTITION_KIND_GUARD
    struct Foo
        x::Int
    end
    const defined_world_age = Base.tls_world_age()
    x = Foo(1)

    @test Base.binding_kind(@__MODULE__, :Foo) == Base.PARTITION_KIND_CONST
    @test !contains(repr(x), "@world")
    Base.delete_binding(@__MODULE__, :Foo)

    @test Base.binding_kind(@__MODULE__, :Foo) == Base.PARTITION_KIND_GUARD
    @test contains(repr(x), "@world")

    # Test that it still works if Foo is redefined to a non-type
    const Foo = 1

    @test Base.binding_kind(@__MODULE__, :Foo) == Base.PARTITION_KIND_CONST
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

# Test that inference can merge ranges for partitions as long as what's being imported doesn't change
module RangeMerge
    using Test
    using InteractiveUtils

    function get_llvm(@nospecialize(f), @nospecialize(t), raw=true, dump_module=false, optimize=true)
        params = Base.CodegenParams(safepoint_on_entry=false, gcstack_arg = false, debug_info_level=Cint(2))
        d = InteractiveUtils._dump_function(f, t, false, false, raw, dump_module, :att, optimize, :none, false, params)
        sprint(print, d)
    end

    global x = 1
    const after_def_world = Base.get_world_counter()
    export x
    f() = x
    @test f() == 1
    @test only(methods(f)).specializations.cache.min_world <= after_def_world

    @test !contains(get_llvm(f, Tuple{}), "jl_get_binding_value")
end

# Test that we invalidate for undefined -> defined transitions (#54733)
module UndefinedTransitions
    using Test
    function foo54733()
        for i = 1:1_000_000_000
            bar54733(i)
        end
        return 1
    end
    @test_throws UndefVarError foo54733()
    let ci = first(methods(foo54733)).specializations.cache
        @test !Base.Compiler.is_nothrow(Base.Compiler.decode_effects(ci.ipo_purity_bits))
    end
    bar54733(x) = 3x
    @test foo54733() === 1
    let ci = first(methods(foo54733)).specializations.cache
        @test Base.Compiler.is_nothrow(Base.Compiler.decode_effects(ci.ipo_purity_bits))
    end
end

# Identical implicit partitions should be merge (#57923)
for binding in (convert(Core.Binding, GlobalRef(Base, :Math)),
                convert(Core.Binding, GlobalRef(Base, :Intrinsics)))
    # Test that these both only have two partitions
    @test isdefined(binding, :partitions)
    @test isdefined(binding.partitions, :next)
    @test !isdefined(binding.partitions.next, :next)
end

# Test various scenarios for implicit partition merging
module MergeStress
    for i = 1:5
        @eval module $(Symbol("M$i"))
            export x, y
            const x = 1
            const y = 2
        end
    end
    const before = Base.get_world_counter()
    using .M1
    const afterM1 = Base.get_world_counter()
    using .M2
    const afterM2 = Base.get_world_counter()
    using .M3
    const afterM3 = Base.get_world_counter()
    using .M4
    const afterM4 = Base.get_world_counter()
    using .M5
    const afterM5 = Base.get_world_counter()
end

function count_partitions(b::Core.Binding)
    n = 0
    isdefined(b, :partitions) || return n
    bpart = b.partitions
    while true
        n += 1
        isdefined(bpart, :next) || break
        bpart = bpart.next
    end
    return n
end
using Base: invoke_in_world

const xbinding = convert(Core.Binding, GlobalRef(MergeStress, :x))
function access_and_count(point)
    invoke_in_world(getglobal(MergeStress, point), getglobal, MergeStress, :x)
    count_partitions(xbinding)
end

@test count_partitions(xbinding) == 0
@test access_and_count(:afterM1) == 1
# M2 is the first change to the `usings` table after M1. The partitions
# can and should be merged
@test access_and_count(:afterM2) == 1

# There is a gap between M2 and M5 - the partitions should not be merged
@test access_and_count(:afterM5) == 2

# M4 and M5 are adjacent, these partitions should also be merged (in the opposite direction)
@test access_and_count(:afterM4) == 2

# M3 connects all, so we should have a single partition
@test access_and_count(:afterM3) == 1

# Test that delete_binding in an outdated world age works
module BindingTestModule; end
function create_and_delete_binding()
    Core.eval(BindingTestModule, :(const x = 1))
    Base.delete_binding(BindingTestModule, :x)
end
create_and_delete_binding()
@test Base.binding_kind(BindingTestModule, :x) == Base.PARTITION_KIND_GUARD
