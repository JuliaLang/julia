# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

include("precompile_utils.jl")

precompile_test_harness() do load_path
    write(joinpath(load_path, "ExampleCompiler.jl"), :(
    module ExampleCompiler
        const CC = Core.Compiler

        struct ExampleInterpreter <: CC.AbstractInterpreter
            world::UInt
            inf_cache::Vector{CC.InferenceResult}
        end
        ExampleInterpreter(world::UInt) =
            ExampleInterpreter(world, CC.InferenceResult[])

        CC.InferenceParams(::ExampleInterpreter) = CC.InferenceParams()
        CC.OptimizationParams(::ExampleInterpreter) = CC.OptimizationParams()
        CC.get_inference_cache(interp::ExampleInterpreter) = interp.inf_cache
        CC.cache_owner(interp::ExampleInterpreter) = :ExampleInterpreter

        CC.get_inference_world(interp::ExampleInterpreter) = interp.world
        CC.lock_mi_inference(::ExampleInterpreter, ::Core.MethodInstance) = nothing
        CC.unlock_mi_inference(::ExampleInterpreter, ::Core.MethodInstance) = nothing

        function infer(mi, world)
            interp = ExampleInterpreter(world)
            ci = CC.typeinf_ext(interp, mi, CC.SOURCE_MODE_GET_SOURCE)
            @assert ci !== nothing "Inference of $mi failed"

            return ci
        end

        function precompile(f, tt; world = Base.get_world_counter())
            mi = Base.method_instance(f, tt; world)
            infer(mi, world)
        end
    end) |> string)
    Base.compilecache(Base.PkgId("ExampleCompiler"))

    write(joinpath(load_path, "ExampleUser.jl"), :(
    module ExampleUser
        import ExampleCompiler

        function square(x)
            return x * x
        end
        ExampleCompiler.precompile(square, (Float64,))

        # Stubbed together version of PrecompileTools
        ccall(:jl_tag_newly_inferred_enable, Cvoid, ())
        try
            # Important `identity(::Any) mi does not belong to us`
            ExampleCompiler.precompile(identity, (Float64,))
        finally
            ccall(:jl_tag_newly_inferred_disable, Cvoid, ())
        end
    end) |> string)
    Base.compilecache(Base.PkgId("ExampleUser"))

    @eval let
        using ExampleUser
        cache_owner = :ExampleInterpreter

        mi_square = Base.method_instance(ExampleUser.square, (Float64,))
        @assert check_presence(mi_square, :ExampleInterpreter) !== nothing

        mi_identity = Base.method_instance(identity, (Float64,))
        @assert check_presence(mi_identity, :ExampleInterpreter) !== nothing
    end
end

finish_precompile_test!()
