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

            # the owner of a root inferred with constant arguments: found again in a
            # package image because separately constructed owners are `===`
            struct SeededOwner
                owner
                argtypes::Tuple
            end
            seeded_root(n::Int) = n
            function infer_seeded(n::Int)
                interp = PrecompileInterpreter()
                mi = Base.method_instance(seeded_root, (Int,))
                argtypes = Any[Core.Const(seeded_root), Core.Const(n)]
                result = Compiler.InferenceResult(mi, argtypes, BitVector((false, true)))
                owner = SeededOwner(Compiler.cache_owner(interp), Tuple(argtypes))
                return Compiler.typeinf_ext(interp, result, owner, Compiler.SOURCE_MODE_NOT_REQUIRED)
            end
        end
        const seeded_ci = Custom.infer_seeded(1)

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
            ci = check_presence(mi, nothing)
            @test ci === nothing
            ci = check_presence(mi, cache_owner)
            @test ci !== nothing
            @test ci.owner === cache_owner
            @test ci.max_world == typemax(UInt)
            @test Base.module_build_id(TestAbsIntPrecompile1) ==
                Base.object_build_id(ci)
        end
        let Custom = TestAbsIntPrecompile1.Custom, ci = TestAbsIntPrecompile1.seeded_ci
            mi = Base.method_instance(Custom.seeded_root, (Int,))
            owner = Custom.SeededOwner(cache_owner,
                (Core.Const(Custom.seeded_root), Core.Const(1)))
            @test check_presence(mi, owner) === ci
            @test ci.rettype_const === 1
            @test Base.module_build_id(TestAbsIntPrecompile1) ==
                Base.object_build_id(ci)
            @test Custom.infer_seeded(1) === ci
            @test Custom.infer_seeded(2) !== ci
        end
        let m = only(methods(sum, (Vector{Float64},)))
            for mi in Base.specializations(m)
                if mi isa Core.MethodInstance && mi.specTypes == Tuple{typeof(sum),Vector{Float64}}
                    ci = check_presence(mi, nothing)
                    @test ci === nothing
                    ci = check_presence(mi, cache_owner)
                    @test ci !== nothing
                    @test ci.owner === cache_owner
                    @test ci.max_world == typemax(UInt)
                    @test Base.module_build_id(TestAbsIntPrecompile1) ==
                        Base.object_build_id(ci)
                end
            end
        end
    end
end

finish_precompile_test!()
