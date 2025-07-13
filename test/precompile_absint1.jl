# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
import Base.Compiler: Compiler

include("precompile_utils.jl")

precompile_test_harness() do load_path
    write(joinpath(load_path, "SimpleModule.jl"), :(module SimpleModule
        basic_callee(x) = x
        basic_caller(x) = basic_callee(x)
    end) |> string)

    newinterp_path = abspath(joinpath(@__DIR__,"../Compiler/test/newinterp.jl"))
    write(joinpath(load_path, "TestAbsIntPrecompile1.jl"), :(module TestAbsIntPrecompile1
        import SimpleModule: basic_caller, basic_callee

        module Custom
            import Base.Compiler: Compiler
            include($newinterp_path)
            @newinterp PrecompileInterpreter
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
    Base.compilecache(Base.PkgId("TestAbsIntPrecompile1"))

    @eval let
        using TestAbsIntPrecompile1
        cache_owner = Compiler.cache_owner(
            TestAbsIntPrecompile1.Custom.PrecompileInterpreter())
        let m = only(methods(TestAbsIntPrecompile1.basic_callee))
            mi = only(Base.specializations(m))
            ci = mi.cache
            @test_broken isdefined(ci, :next)
            @test ci.owner === nothing
            @test ci.max_world == typemax(UInt)
            @test Base.module_build_id(TestAbsIntPrecompile1) ==
                Base.object_build_id(ci)
            @test_skip begin
            ci = ci.next
            @test !isdefined(ci, :next)
            @test ci.owner === cache_owner
            @test ci.max_world == typemax(UInt)
            @test Base.module_build_id(TestAbsIntPrecompile1) ==
                Base.object_build_id(ci)
            end
        end
        let m = only(methods(sum, (Vector{Float64},)))
            found = false
            for mi in Base.specializations(m)
                if mi isa Core.MethodInstance && mi.specTypes == Tuple{typeof(sum),Vector{Float64}}
                    ci = mi.cache
                    @test_broken isdefined(ci, :next)
                    @test_broken ci.owner === cache_owner
                    @test_skip begin
                    @test ci.max_world == typemax(UInt)
                    @test Base.module_build_id(TestAbsIntPrecompile1) ==
                        Base.object_build_id(ci)
                    ci = ci.next
                    end
                    @test !isdefined(ci, :next)
                    @test ci.owner === nothing
                    @test ci.max_world == typemax(UInt)
                    @test Base.module_build_id(TestAbsIntPrecompile1) ==
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
