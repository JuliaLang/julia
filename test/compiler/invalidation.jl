# This file is a part of Julia. License is MIT: https://julialang.org/license

# setup
# -----

include("irutils.jl")

using Test
const CC = Core.Compiler
import Core: MethodInstance, CodeInstance
import .CC: WorldRange, WorldView

struct InvalidationTesterToken end
const INVALIDATION_TESTER_CACHE = Core.Compiler.InternalCodeCache(InvalidationTesterToken())

struct InvalidationTester <: CC.AbstractInterpreter
    world::UInt
    inf_params::CC.InferenceParams
    opt_params::CC.OptimizationParams
    inf_cache::Vector{CC.InferenceResult}
    code_cache::Core.Compiler.InternalCodeCache
    function InvalidationTester(;
                                world::UInt = Base.get_world_counter(),
                                inf_params::CC.InferenceParams = CC.InferenceParams(),
                                opt_params::CC.OptimizationParams = CC.OptimizationParams(),
                                inf_cache::Vector{CC.InferenceResult} = CC.InferenceResult[],
                                code_cache::Core.Compiler.InternalCodeCache = INVALIDATION_TESTER_CACHE)
        return new(world, inf_params, opt_params, inf_cache, code_cache)
    end
end

CC.InferenceParams(interp::InvalidationTester) = interp.inf_params
CC.OptimizationParams(interp::InvalidationTester) = interp.opt_params
CC.get_world_counter(interp::InvalidationTester) = interp.world
CC.get_inference_cache(interp::InvalidationTester) = interp.inf_cache
CC.cache_owner(::InvalidationTester) = InvalidationTesterToken()
CC.code_cache(interp::InvalidationTester) = WorldView(interp.code_cache, interp.world)

has_active_cache(mi::MethodInstance, args...) =
    isdefined(mi, :cache) && has_active_cache(mi.cache, args...)
function has_active_cache(codeinst::CodeInstance, owner)
    if codeinst.owner === owner && codeinst.max_world == typemax(UInt)
        return true
    end
    return isdefined(codeinst, :next) && has_active_cache(codeinst.next, owner)
end

# basic functionality test
# ------------------------

basic_callee(x) = x
basic_caller(x) = basic_callee(x)

# run inference and check that cache exist
@test Base.return_types((Float64,); interp=InvalidationTester()) do x
    basic_caller(x)
end |> only === Float64

@test has_active_cache(only(Base.specializations(only(methods(basic_callee)))), InvalidationTesterToken())
@test has_active_cache(only(Base.specializations(only(methods(basic_caller)))), InvalidationTesterToken())

# this redefinition below should invalidate the cache
basic_callee(x) = x, x
@test isempty(Base.specializations(only(methods(basic_callee))))
@test !has_active_cache(only(Base.specializations(only(methods(basic_caller)))), InvalidationTesterToken())

# re-run inference and check the result is updated (and new cache exists)
@test Base.return_types((Float64,); interp=InvalidationTester()) do x
    basic_caller(x)
end |> only === Tuple{Float64,Float64}
@test has_active_cache(only(Base.specializations(only(methods(basic_callee)))), InvalidationTesterToken())
@test has_active_cache(only(Base.specializations(only(methods(basic_caller)))), InvalidationTesterToken())

# backedge optimization
# ---------------------

const GLOBAL_BUFFER = IOBuffer()

# test backedge optimization when the callee's type and effects information are maximized
begin take!(GLOBAL_BUFFER)

    pr48932_callee(x) = (print(GLOBAL_BUFFER, x); Base.inferencebarrier(x))
    pr48932_caller(x) = pr48932_callee(Base.inferencebarrier(x))

    # assert that type and effects information inferred from `pr48932_callee(::Any)` are the top
    let rt = only(Base.return_types(pr48932_callee, (Any,)))
        @test rt === Any
        effects = Base.infer_effects(pr48932_callee, (Any,))
        @test effects == Core.Compiler.Effects()
    end

    # run inference on both `pr48932_caller` and `pr48932_callee`
    let (src, rt) = code_typed((Int,); interp=InvalidationTester()) do x
            @inline pr48932_caller(x)
        end |> only
        @test rt === Any
        @test any(iscall((src, pr48932_callee)), src.code)
    end

    let mi = only(Base.specializations(only(methods(pr48932_callee))))
        @test has_active_cache(mi, InvalidationTesterToken())
        @test has_active_cache(mi, nothing)
    end
    let mi = only(Base.specializations(only(methods(pr48932_caller))))
        @test has_active_cache(mi, InvalidationTesterToken())
        @test !has_active_cache(mi, nothing)
    end

    @test 42 == pr48932_caller(42)
    @test "42" == String(take!(GLOBAL_BUFFER))

    # test that we didn't add the backedge from `pr48932_callee` to `pr48932_caller`:
    # this redefinition below should invalidate the cache of `pr48932_callee` but not that of `pr48932_caller`
    pr48932_callee(x) = (print(GLOBAL_BUFFER, x); nothing)

    @test isempty(Base.specializations(only(methods(pr48932_callee))))
    let mi = only(Base.specializations(only(methods(pr48932_caller))))
        @test has_active_cache(mi, InvalidationTesterToken())
        @test has_active_cache(mi, nothing)
    end

    @test isnothing(pr48932_caller(42))
    @test "42" == String(take!(GLOBAL_BUFFER))
end

# we can avoid adding backedge even if the callee's return type is not the top
# when the return value is not used within the caller
begin take!(GLOBAL_BUFFER)
    pr48932_callee_inferable(x) = (print(GLOBAL_BUFFER, x); Base.inferencebarrier(1)::Int)
    pr48932_caller_unuse(x) = (pr48932_callee_inferable(Base.inferencebarrier(x)); nothing)

    # assert that type and effects information inferred from `pr48932_callee(::Any)` are the top
    let rt = only(Base.return_types(pr48932_callee_inferable, (Any,)))
        @test rt === Int
        effects = Base.infer_effects(pr48932_callee_inferable, (Any,))
        @test effects == Core.Compiler.Effects()
    end

    # run inference on both `pr48932_caller` and `pr48932_callee`:
    # we don't need to add backedge to `pr48932_callee` from `pr48932_caller`
    # since the inference result of `pr48932_callee` is maximized and it's not inlined
    let (src, rt) = code_typed((Int,); interp=InvalidationTester()) do x
            @inline pr48932_caller_unuse(x)
        end |> only
        @test rt === Nothing
        @test any(iscall((src, pr48932_callee_inferable)), src.code)
    end

    let mi = only(Base.specializations(only(methods(pr48932_callee_inferable))))
        @test has_active_cache(mi, InvalidationTesterToken())
        @test has_active_cache(mi, nothing)
    end
    let mi = only(Base.specializations(only(methods(pr48932_caller_unuse))))
        @test has_active_cache(mi, InvalidationTesterToken())
        @test !has_active_cache(mi, nothing)
    end

    @test isnothing(pr48932_caller_unuse(42))
    @test "42" == String(take!(GLOBAL_BUFFER))

    # test that we didn't add the backedge from `pr48932_callee_inferable` to `pr48932_caller_unuse`:
    # this redefinition below should invalidate the cache of `pr48932_callee_inferable` but not that of `pr48932_caller_unuse`
    pr48932_callee_inferable(x) = (print(GLOBAL_BUFFER, "foo"); x)

    @test isempty(Base.specializations(only(methods(pr48932_callee_inferable))))
    let mi = only(Base.specializations(only(methods(pr48932_caller_unuse))))
        @test has_active_cache(mi, InvalidationTesterToken())
        @test has_active_cache(mi, nothing)
    end
    @test isnothing(pr48932_caller_unuse(42))
    @test "foo" == String(take!(GLOBAL_BUFFER))
end

# we need to add backedge when the callee is inlined
begin take!(GLOBAL_BUFFER)

    @noinline pr48932_callee_inlined(@nospecialize x) = (print(GLOBAL_BUFFER, x); Base.inferencebarrier(x))
    pr48932_caller_inlined(x) = pr48932_callee_inlined(Base.inferencebarrier(x))

    # assert that type and effects information inferred from `pr48932_callee(::Any)` are the top
    let rt = only(Base.return_types(pr48932_callee_inlined, (Any,)))
        @test rt === Any
        effects = Base.infer_effects(pr48932_callee_inlined, (Any,))
        @test effects == Core.Compiler.Effects()
    end

    # run inference on `pr48932_caller_inlined` and `pr48932_callee_inlined`
    let (src, rt) = code_typed((Int,); interp=InvalidationTester()) do x
            @inline pr48932_caller_inlined(x)
        end |> only
        @test rt === Any
        @test any(isinvoke(:pr48932_callee_inlined), src.code)
    end

    let mi = only(Base.specializations(only(methods(pr48932_callee_inlined))))
        @test has_active_cache(mi, InvalidationTesterToken())
        @test has_active_cache(mi, nothing)
    end
    let mi = only(Base.specializations(only(methods(pr48932_caller_inlined))))
        @test has_active_cache(mi, InvalidationTesterToken())
        @test !has_active_cache(mi, nothing)
    end

    @test 42 == pr48932_caller_inlined(42)
    @test "42" == String(take!(GLOBAL_BUFFER))

    # test that we added the backedge from `pr48932_callee_inlined` to `pr48932_caller_inlined`:
    # this redefinition below should invalidate the cache of `pr48932_callee_inlined` but not that of `pr48932_caller_inlined`
    @noinline pr48932_callee_inlined(@nospecialize x) = (print(GLOBAL_BUFFER, x); nothing)

    @test isempty(Base.specializations(only(methods(pr48932_callee_inlined))))
    let mi = only(Base.specializations(only(methods(pr48932_caller_inlined))))
        @test !has_active_cache(mi, InvalidationTesterToken())
        @test !has_active_cache(mi, nothing)
    end

    @test isnothing(pr48932_caller_inlined(42))
    @test "42" == String(take!(GLOBAL_BUFFER))
end
