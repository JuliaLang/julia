# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

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

# Retracting an `export` with `set_binding_visibility!` makes a name stop
# resolving through `using`, and re-exporting restores it.
module RebindingVisibility
    using Test

    module SrcMod
        export visg
        visg() = 42
        public visp
        visp() = 7
    end
    using .SrcMod

    @test Base.isexported(SrcMod, :visg)
    f_use_visg() = visg()
    @test f_use_visg() == 42

    # Retract the export: `visg` is still defined in SrcMod but no longer reachable here.
    Base.set_binding_visibility!(SrcMod, :visg, :none)
    @test !Base.isexported(SrcMod, :visg)
    @test !Base.ispublic(SrcMod, :visg)
    @test :visg ∉ names(SrcMod)
    @test_throws UndefVarError f_use_visg()
    @test SrcMod.visg() == 42

    # Re-export and confirm implicit resolution is restored.
    Base.set_binding_visibility!(SrcMod, :visg, :export)
    @test Base.isexported(SrcMod, :visg)
    @test :visg ∈ names(SrcMod)
    @test f_use_visg() == 42

    # The public flag is independent of export and not world-versioned.
    @test Base.ispublic(SrcMod, :visp) && !Base.isexported(SrcMod, :visp)
    Base.set_binding_visibility!(SrcMod, :visp, :none)
    @test !Base.ispublic(SrcMod, :visp)
    Base.set_binding_visibility!(SrcMod, :visp, :public)
    @test Base.ispublic(SrcMod, :visp)

    @test_throws ArgumentError Base.set_binding_visibility!(SrcMod, :visg, :bogus)
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

    precompile_test_harness("export retraction") do load_path
        write(joinpath(load_path, "RetractExport.jl"),
              """
              module RetractExport
                export retract_me
                const retract_me = 11
              end
              """)
        Base.compilecache(Base.PkgId("RetractExport"))
        write(joinpath(load_path, "UseRetractExport.jl"),
              """
              module UseRetractExport
                using RetractExport
                f_use_retract() = retract_me
                @assert f_use_retract() == 11
              end
              """)
        Base.compilecache(Base.PkgId("UseRetractExport"))
        @eval using RetractExport
        # Retract the export before loading the dependent package
        invokelatest() do
            Base.set_binding_visibility!(RetractExport, :retract_me, :none)
        end
        @eval using UseRetractExport
        invokelatest() do
            @test_throws UndefVarError UseRetractExport.f_use_retract()
        end
        # Re-export and confirm resolution is restored
        invokelatest() do
            Base.set_binding_visibility!(RetractExport, :retract_me, :export)
        end
        invokelatest() do
            @test UseRetractExport.f_use_retract() == 11
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
        d = InteractiveUtils._dump_function(InteractiveUtils.ArgInfo(f, t), false, false, raw, dump_module, :att, optimize, :none, false, "", params)
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

# Identical implicit partitions should be merged (#57923)
for binding in (convert(Core.Binding, GlobalRef(Base, :Math)),)
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

# Test that we properly invalidate bindings if the value changes, not just the
# export status (#59272)
module Invalidate59272
    using Test
    module Foo
        export Bar
        struct Bar
        # x
        end
    end
    using .Foo
    @test isa(Bar(), Foo.Bar)
    Core.eval(Foo, :(struct Bar; x; end))
    @test Bar(1) == Foo.Bar(1)
end

# Test that two const-prop'd pseudo `CodeInstance`s for the same `MethodInstance`
# carrying *different* binding edges are both kept on the caller's edge list, so
# that redefining either binding properly invalidates the caller (#61745).
module Invalidate61745
    using Test
    module N
        const foo = "foo_unchanged"
        const bar = "bar_unchanged"
    end
    helper(s::Symbol) = getglobal(N, s)::String
    caller_both() = helper(:foo) * helper(:bar)
    @test caller_both() == "foo_unchangedbar_unchanged"
    Core.eval(N, :(const foo = "foo_changed!"))
    @test caller_both() == "foo_changed!bar_unchanged"
    Core.eval(N, :(const bar = "bar_changed!"))
    @test caller_both() == "foo_changed!bar_changed!"
end

# Test that codegen does not bake in a binding's value when there is no forward
# edge from the `CodeInstance` to the binding. Without const-prop tracking the
# `Module` argument, inference cannot record a `Binding` edge for `M.foo`, so
# codegen must fall back to a runtime binding load to remain correct under
# redefinition (#61745).
module Invalidate61745_indirect
    using Test
    module M
        const foo = "unchanged"
    end
    indirect_access(modref::Module) = Base.getproperty(modref, :foo)::String
    caller() = indirect_access(M)
    @test caller() == "unchanged"
    Core.eval(M, :(const foo = "changed!"))
    @test caller() == "changed!"
end

# Test @reexport
module ReexportTests
    using Test
    using Base.Experimental: @reexport

    # Test dynamic export additions through reexport
    module Source1
        export s1
        s1() = "s1"
    end
    module Reexporter1
        import ..@reexport
        @reexport using ..Source1
    end
    module User1
        using ..Reexporter1
    end
    @test (:s1,) ⊆ names(Reexporter1)
    @test User1.s1() == "s1"
    Core.eval(Source1, :(s2() = "s2"; export s2))
    @test (:s1, :s2) ⊆ names(Reexporter1)
    @test User1.s2() == "s2"

    # Test reexport syntax, multiple modules
    module Source2
        export s3
        s3() = "s3"
    end
    module Reexporter2
        import ..@reexport
        @reexport using ..Source2, ..Source1
    end
    module User2
        using ..Reexporter2
    end
    @test (:s1, :s3) ⊆ names(Reexporter2)
    @test User2.s1() == "s1"
    @test User2.s3() == "s3"

    # Test same name from different modules - one with reexport, one without
    module Source3
        export same_name
        const same_name = 42
    end
    module Source4
        export same_name
        const same_name = 42
    end
    module Reexporter3
        import ..@reexport
        using ..Source4  # without reexport
        @reexport using ..Source3
    end
    module User3
        using ..Reexporter3
    end
    @test User3.same_name == 42
end

# #62154: replacement of mutable typed globals
@testset "typed global replacement (#62154)" begin
    # first definition and re-typing via the value-carrying form (installed atomically)
    @eval module TGR1 end
    @test Core.eval(TGR1, :(global x::Int = 5)) === 5
    @test TGR1.x === 5
    @test Core.get_binding_type(TGR1, :x) === Int
    @test Core.eval(TGR1, :(global x::Float64 = 3.0)) === 3.0
    @test TGR1.x === 3.0
    @test Core.get_binding_type(TGR1, :x) === Float64
    @test Core.eval(TGR1, :(global x::Real = 7)) === 7      # widening, with a value
    @test TGR1.x === 7

    # the value-carrying form replaces an incompatible value (the bare form would error)
    @eval module TGR8 end
    Core.eval(TGR8, :(global q::Int = 5))
    @test Core.eval(TGR8, :(global q::String = "ok")) === "ok"
    @test TGR8.q == "ok"
    @test Core.get_binding_type(TGR8, :q) === String

    # bare re-type: an incompatible existing value is an error (value is preserved)
    @eval module TGR2 end
    Core.eval(TGR2, :(global y::Int = 5))
    @test_throws ErrorException Core.eval(TGR2, :(global y::String))
    @test TGR2.y === 5
    @test Core.get_binding_type(TGR2, :y) === Int
    # bare re-type: a value that still conforms (widening) is retained
    @test Core.eval(TGR2, :(global y::Integer)) === nothing
    @test TGR2.y === 5
    @test Core.get_binding_type(TGR2, :y) === Integer

    # bare re-type of an undefined binding is always allowed
    @eval module TGR3 end
    Core.eval(TGR3, :(global z::Int))
    @test Core.eval(TGR3, :(global z::Float64)) === nothing
    @test Core.get_binding_type(TGR3, :z) === Float64
    @test !isdefined(TGR3, :z)

    # setglobal! and assignment check the latest declared type
    @eval module TGR4 end
    Core.eval(TGR4, :(global w::Int = 1))
    Core.eval(TGR4, :(global w::Float64 = 2.0))
    @test_throws TypeError setglobal!(TGR4, :w, 3)
    @test setglobal!(TGR4, :w, 3.0) === 3.0
    @test TGR4.w === 3.0

    # a reader is recompiled after re-typing (binding invalidation)
    @eval module TGR5
        global v::Int = 10
        f() = v
    end
    @test TGR5.f() === 10
    @test only(Base.return_types(TGR5.f, ())) === Int
    Core.eval(TGR5, :(global v::Float64 = 2.5))
    @test TGR5.f() === 2.5
    @test only(Base.return_types(TGR5.f, ())) === Float64

    # the value-carrying form never leaves the binding transiently undefined
    @eval module TGR6 end
    Core.eval(TGR6, :(global p::Int = 100))
    Core.eval(TGR6, :(global p::String = "hi"))
    @test TGR6.p == "hi"
    @test isdefined(TGR6, :p)

    # constants and imports still cannot be re-typed this way
    @eval module TGR7 end
    Core.eval(TGR7, :(const c = 5))
    @test_throws ErrorException Core.eval(TGR7, :(global c::Int))

    # compound assignment to a typed global keeps the declared type (the joint-form lowering
    # must only apply to plain `=`, not `+=` and friends, or the type decl would be lost)
    @eval module TGR9 end
    Core.eval(TGR9, :(global cc::Int = 5))
    Core.eval(TGR9, :(global cc::Int += 3))
    @test TGR9.cc === 8
    @test Core.get_binding_type(TGR9, :cc) === Int

    # a typed global assignment whose value references a surrounding local (in a loop at top
    # level) installs the value in place rather than hoisting it out of scope
    @eval module TGR10 end
    Core.eval(TGR10, :(acc = Ref(0)))
    Core.eval(TGR10, :(for i in 1:3; global lv::Int = i; acc[] += lv; end))
    @test TGR10.lv === 3
    @test TGR10.acc[] === 6
    @test Core.get_binding_type(TGR10, :lv) === Int
end

# #62154: generated code that observes a global's value slot after its type has changed must
# verify the value against the type it was compiled for, so that a stale access errors rather
# than returning an ill-typed value (a memory-unsafe type confusion).
@testset "typed global re-type access verification (#62154)" begin
    @eval module TGRV1 end
    Core.eval(TGRV1, :(global x::Int = 5))
    Core.eval(TGRV1, :(g() = x))
    Core.eval(TGRV1, :(g()))                           # compile `g` trusting Int
    w1 = Base.get_world_counter()
    @test TGRV1.g() === 5
    Core.eval(TGRV1, :(global x::Number = 2.5))        # re-type Int -> Number (value 2.5)
    @test TGRV1.g() === 2.5                             # latest read recompiled, sound
    # old native code, replayed for the pre-re-type world, errors instead of returning garbage
    @test_throws TypeError Base.invoke_in_world(w1, TGRV1.g)

    # a re-type happening while a frame is on the stack: the later read in the same frame errors
    @eval module TGRV2 end
    Core.eval(TGRV2, :(global y::Int = 5))
    Core.eval(TGRV2, :(function f()
        a = y
        Core.eval(TGRV2, :(global y::Number = 2.5))
        return (a, y)
    end))
    @test_throws TypeError TGRV2.f()

    # a narrowing re-type stays sound without erroring: the value still conforms to the old type
    @eval module TGRV3 end
    Core.eval(TGRV3, :(global z::Number = 3))
    Core.eval(TGRV3, :(zr() = z))
    Core.eval(TGRV3, :(zr()))
    wz = Base.get_world_counter()
    Core.eval(TGRV3, :(global z::Int = 7))             # 7 isa Number too
    @test TGRV3.zr() === 7
    @test Base.invoke_in_world(wz, TGRV3.zr) === 7

    # a re-type interleaved with `Base.delete_binding` must also activate verification,
    # even though the new typed partition does not directly replace the old one
    @eval module TGRV4 end
    Core.eval(TGRV4, :(global d::Int = 5))
    Core.eval(TGRV4, :(readd() = d))
    Core.eval(TGRV4, :(readd()))
    wd = Base.get_world_counter()
    Base.delete_binding(TGRV4, :d)
    Core.eval(TGRV4, :(global d::Float64 = 2.5))
    @test TGRV4.readd() === 2.5
    @test_throws TypeError Base.invoke_in_world(wd, TGRV4.readd)
end

# #62154: writes from code compiled against a previous declared type must validate against
# both the compile-time type ("a later declaration can narrow the set of accesses that
# succeed, but never expand it") and the latest declared type (which governs the single
# value slot).
@testset "typed global re-type write verification (#62154)" begin
    # widening re-type: the compile-time type is still enforced in the stale frame
    @eval module TGRW1 end
    Core.eval(TGRW1, :(global w::Int = 1))
    Core.eval(TGRW1, :(setw(x) = setglobal!(TGRW1, :w, x)))
    @test TGRW1.setw(2) === 2                           # compile `setw` trusting Int
    w1 = Base.get_world_counter()
    Core.eval(TGRW1, :(global w::Number = 10))
    @test_throws TypeError Base.invoke_in_world(w1, TGRW1.setw, 2.5) # !isa Int (compile type)
    @test TGRW1.w === 10
    @test Base.invoke_in_world(w1, TGRW1.setw, 5) === 5              # isa Int and isa Number
    @test TGRW1.w === 5

    # narrowing re-type: the latest declared type is enforced by the runtime store the
    # stale write diverts to
    @eval module TGRW2 end
    Core.eval(TGRW2, :(global v::Number = 1))
    Core.eval(TGRW2, :(setv(x) = (global v = x)))
    @test TGRW2.setv(2) === 2                           # compile `setv` trusting Number
    w2 = Base.get_world_counter()
    Core.eval(TGRW2, :(global v::Int = 3))
    @test_throws TypeError Base.invoke_in_world(w2, TGRW2.setv, 2.5) # isa Number, !isa Int (latest)
    @test TGRW2.v === 3
    @test Base.invoke_in_world(w2, TGRW2.setv, 4) === 4
    @test TGRW2.v === 4

    # a stale write in a frame that is on the stack across the redefinition
    @eval module TGRW3 end
    Core.eval(TGRW3, :(global u::Number = 1))
    Core.eval(TGRW3, :(function f()
        global u = 2
        Core.eval(TGRW3, :(global u::Int = 3))
        global u = 0.5      # isa Number, but the slot is now governed by Int
        return u
    end))
    @test_throws TypeError TGRW3.f()
    @test TGRW3.u === 3

    # modifyglobal! validates against the latest declared type even when invoked from an
    # older world (jl_checked_modify used to consult only the caller's world)
    @eval module TGRW4 end
    Core.eval(TGRW4, :(global m::Number = 1))
    wm = Base.get_world_counter()
    Core.eval(TGRW4, :(global m::Int = 5))
    # dynamic modifyglobal! from the old world: `+` produces 5.5, which conforms to the
    # old declared type (Number) but not to the latest one (Int)
    @test_throws TypeError Base.invoke_in_world(wm, modifyglobal!, TGRW4, :m, +, 0.5)
    @test TGRW4.m === 5

    # swapglobal! from a stale frame: the store obeys both type checks, and the returned
    # old value is verified against the compile-time type like a read
    @eval module TGRW5 end
    Core.eval(TGRW5, :(global s::Number = 1))
    Core.eval(TGRW5, :(swaps(x) = swapglobal!(TGRW5, :s, x)))
    @test TGRW5.swaps(2) === 1
    w5 = Base.get_world_counter()
    Core.eval(TGRW5, :(global s::Int = 7))
    @test Base.invoke_in_world(w5, TGRW5.swaps, 3) === 7  # 7 isa Number: result verifies
    @test TGRW5.s === 3
    @test_throws TypeError Base.invoke_in_world(w5, TGRW5.swaps, 2.5) # !isa Int (latest)
    @test TGRW5.s === 3

    # ... and when the old value does not conform to the compile-time type, the swap
    # itself succeeds (it is valid in the latest world) but the result errors
    @eval module TGRW6 end
    Core.eval(TGRW6, :(global t::Int = 1))
    Core.eval(TGRW6, :(swapt(x) = swapglobal!(TGRW6, :t, x)))
    @test TGRW6.swapt(2) === 1
    w6 = Base.get_world_counter()
    Core.eval(TGRW6, :(global t::Number = 2.5))
    @test_throws TypeError Base.invoke_in_world(w6, TGRW6.swapt, 5) # old value 2.5 !isa Int
    @test TGRW6.t === 5

    # replaceglobal! from a stale frame: the result container (a NamedTuple, which is
    # invariant in the value type) is built by the runtime at the latest declared type,
    # so once the binding is re-typed the stale result conservatively errors -- after
    # the replacement itself (which is valid in the latest world) took effect
    @eval module TGRW7 end
    Core.eval(TGRW7, :(global r::Number = 1))
    Core.eval(TGRW7, :(repl(old, new) = replaceglobal!(TGRW7, :r, old, new)))
    let res = TGRW7.repl(1, 2)
        @test res.old === 1 && res.success
    end
    w7 = Base.get_world_counter()
    Core.eval(TGRW7, :(global r::Int = 7))
    @test_throws TypeError Base.invoke_in_world(w7, TGRW7.repl, 7, 8)
    @test TGRW7.r === 8
end
