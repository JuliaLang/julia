# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

include("precompile_utils.jl")

precompile_test_harness() do load_path
    write(joinpath(load_path, "SimpleModule.jl"), :(module SimpleModule
        basic_callee(x) = x
        basic_caller(x) = basic_callee(x)
    end) |> string)

    newinterp_path = abspath(joinpath(@__DIR__,"../Compiler/test/newinterp.jl"))
    write(joinpath(load_path, "TestAbsIntPrecompile2.jl"), :(module TestAbsIntPrecompile2
        import SimpleModule: basic_caller, basic_callee

        module Custom
            import Base.Compiler: Compiler
            include($newinterp_path)
            @newinterp PrecompileInterpreter
            struct CustomData
                inferred
                CustomData(@nospecialize inferred) = new(inferred)
            end
            function Compiler.transform_result_for_cache(interp::PrecompileInterpreter, result::Compiler.InferenceResult, edges::Core.SimpleVector)
                inferred_result = @invoke Compiler.transform_result_for_cache(
                    interp::Compiler.AbstractInterpreter, result::Compiler.InferenceResult, edges::Core.SimpleVector)
                return CustomData(inferred_result)
            end
            function Compiler.src_inlining_policy(interp::PrecompileInterpreter, @nospecialize(src),
                                            @nospecialize(info::Compiler.CallInfo), stmt_flag::UInt32)
                if src isa CustomData
                    src = src.inferred
                end
                return @invoke Compiler.src_inlining_policy(interp::Compiler.AbstractInterpreter, src::Any,
                                                      info::Compiler.CallInfo, stmt_flag::UInt32)
            end
            Compiler.retrieve_ir_for_inlining(cached_result::Core.CodeInstance, src::CustomData) =
                Compiler.retrieve_ir_for_inlining(cached_result, src.inferred)
            Compiler.retrieve_ir_for_inlining(mi::Core.MethodInstance, src::CustomData, preserve_local_sources::Bool) =
                Compiler.retrieve_ir_for_inlining(mi, src.inferred, preserve_local_sources)
        end

        Base.return_types((Float64,)) do x
            basic_caller(x)
        end
        Base.return_types((Float64,); interp=Custom.PrecompileInterpreter()) do x
            basic_caller(x)
        end
        Base.return_types((Vector{Float64},)) do x
            sum(x)
        end
        Base.return_types((Vector{Float64},); interp=Custom.PrecompileInterpreter()) do x
            sum(x)
        end
    end) |> string)
    Base.compilecache(Base.PkgId("TestAbsIntPrecompile2"))

    @eval let
        using TestAbsIntPrecompile2
        cache_owner = Core.Compiler.cache_owner(
            TestAbsIntPrecompile2.Custom.PrecompileInterpreter())
        let m = only(methods(TestAbsIntPrecompile2.basic_callee))
            mi = only(Base.specializations(m))
            ci = mi.cache
            @test_broken isdefined(ci, :next)
            @test ci.owner === nothing
            @test ci.max_world == typemax(UInt)
            @test Base.module_build_id(TestAbsIntPrecompile2) ==
                Base.object_build_id(ci)
            @test_skip begin
            ci = ci.next
            @test !isdefined(ci, :next)
            @test ci.owner === cache_owner
            @test ci.max_world == typemax(UInt)
            @test Base.module_build_id(TestAbsIntPrecompile2) ==
                Base.object_build_id(ci)
            end
        end
        let m = only(methods(sum, (Vector{Float64},)))
            found = false
            for mi = Base.specializations(m)
                if mi isa Core.MethodInstance && mi.specTypes == Tuple{typeof(sum),Vector{Float64}}
                    ci = mi.cache
                    @test_broken isdefined(ci, :next)
                    @test_broken ci.owner === cache_owner
                    @test_skip begin
                    @test ci.max_world == typemax(UInt)
                    @test Base.module_build_id(TestAbsIntPrecompile2) ==
                        Base.object_build_id(ci)
                    ci = ci.next
                    end
                    @test !isdefined(ci, :next)
                    @test ci.owner === nothing
                    @test ci.max_world == typemax(UInt)
                    @test Base.module_build_id(TestAbsIntPrecompile2) ==
                        Base.object_build_id(ci)
                    found = true
                    break
                end
            end
            @test found
        end
    end
end

finish_precompile_test!()
