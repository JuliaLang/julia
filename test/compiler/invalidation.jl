# This file is a part of Julia. License is MIT: https://julialang.org/license

# setup
# -----

include("irutils.jl")

using Test
const CC = Core.Compiler
import Core: MethodInstance, CodeInstance
import .CC: WorldRange, WorldView

struct InvalidationTesterCache
    dict::IdDict{MethodInstance,CodeInstance}
end
InvalidationTesterCache() = InvalidationTesterCache(IdDict{MethodInstance,CodeInstance}())

const INVALIDATION_TESTER_CACHE = InvalidationTesterCache()

struct InvalidationTester <: CC.AbstractInterpreter
    callback!
    world::UInt
    inf_params::CC.InferenceParams
    opt_params::CC.OptimizationParams
    inf_cache::Vector{CC.InferenceResult}
    code_cache::InvalidationTesterCache
    function InvalidationTester(callback! = nothing;
                                world::UInt = Base.get_world_counter(),
                                inf_params::CC.InferenceParams = CC.InferenceParams(),
                                opt_params::CC.OptimizationParams = CC.OptimizationParams(),
                                inf_cache::Vector{CC.InferenceResult} = CC.InferenceResult[],
                                code_cache::InvalidationTesterCache = INVALIDATION_TESTER_CACHE)
        if callback! === nothing
            callback! = function (replaced::MethodInstance)
                # Core.println(replaced) # debug
                delete!(code_cache.dict, replaced)
            end
        end
        return new(callback!, world, inf_params, opt_params, inf_cache, code_cache)
    end
end

struct InvalidationTesterCacheView
    interp::InvalidationTester
    dict::IdDict{MethodInstance,CodeInstance}
end

CC.InferenceParams(interp::InvalidationTester) = interp.inf_params
CC.OptimizationParams(interp::InvalidationTester) = interp.opt_params
CC.get_world_counter(interp::InvalidationTester) = interp.world
CC.get_inference_cache(interp::InvalidationTester) = interp.inf_cache
CC.code_cache(interp::InvalidationTester) = WorldView(InvalidationTesterCacheView(interp, interp.code_cache.dict), WorldRange(interp.world))
CC.get(wvc::WorldView{InvalidationTesterCacheView}, mi::MethodInstance, default) = get(wvc.cache.dict, mi, default)
CC.getindex(wvc::WorldView{InvalidationTesterCacheView}, mi::MethodInstance) = getindex(wvc.cache.dict, mi)
CC.haskey(wvc::WorldView{InvalidationTesterCacheView}, mi::MethodInstance) = haskey(wvc.cache.dict, mi)
function CC.setindex!(wvc::WorldView{InvalidationTesterCacheView}, ci::CodeInstance, mi::MethodInstance)
    add_callback!(wvc.cache.interp.callback!, mi)
    setindex!(wvc.cache.dict, ci, mi)
end

function add_callback!(@nospecialize(callback!), mi::MethodInstance)
    callback = function (replaced::MethodInstance, max_world,
                         seen::Base.IdSet{MethodInstance} = Base.IdSet{MethodInstance}())
        push!(seen, replaced)
        callback!(replaced)
        if isdefined(replaced, :backedges)
            for item in replaced.backedges
                isa(item, MethodInstance) || continue # might be `Type` object representing an `invoke` signature
                mi = item
                mi in seen && continue # otherwise fail into an infinite loop
                var"#self#"(mi, max_world, seen)
            end
        end
        return nothing
    end

    if !isdefined(mi, :callbacks)
        mi.callbacks = Any[callback]
    else
        callbacks = mi.callbacks::Vector{Any}
        if !any(@nospecialize(cb)->cb===callback, callbacks)
            push!(callbacks, callback)
        end
    end
    return nothing
end


# basic functionality test
# ------------------------

basic_callee(x) = x
basic_caller(x) = basic_callee(x)

# run inference and check that cache exist
@test Base.return_types((Float64,); interp=InvalidationTester()) do x
    basic_caller(x)
end |> only === Float64
@test any(INVALIDATION_TESTER_CACHE.dict) do (mi, ci)
    mi.def.name === :basic_callee
end
@test any(INVALIDATION_TESTER_CACHE.dict) do (mi, ci)
    mi.def.name === :basic_caller
end

# this redefinition below should invalidate the cache
basic_callee(x) = x, x
@test !any(INVALIDATION_TESTER_CACHE.dict) do (mi, ci)
    mi.def.name === :basic_callee
end
@test !any(INVALIDATION_TESTER_CACHE.dict) do (mi, ci)
    mi.def.name === :basic_caller
end

# re-run inference and check the result is updated (and new cache exists)
@test Base.return_types((Float64,); interp=InvalidationTester()) do x
    basic_caller(x)
end |> only === Tuple{Float64,Float64}
@test any(INVALIDATION_TESTER_CACHE.dict) do (mi, ci)
    mi.def.name === :basic_callee
end
@test any(INVALIDATION_TESTER_CACHE.dict) do (mi, ci)
    mi.def.name === :basic_caller
end

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
        @test Core.Compiler.Effects(effects; noinbounds=false) == Core.Compiler.Effects()
    end

    # run inference on both `pr48932_caller` and `pr48932_callee`
    let (src, rt) = code_typed((Int,); interp=InvalidationTester()) do x
            @inline pr48932_caller(x)
        end |> only
        @test rt === Any
        @test any(iscall((src, pr48932_callee)), src.code)
    end
    @test any(INVALIDATION_TESTER_CACHE.dict) do (mi, ci)
        mi.def.name === :pr48932_callee
    end
    @test any(INVALIDATION_TESTER_CACHE.dict) do (mi, ci)
        mi.def.name === :pr48932_caller
    end
    @test 42 == pr48932_caller(42)
    @test "42" == String(take!(GLOBAL_BUFFER))

    # test that we didn't add the backedge from `pr48932_callee` to `pr48932_caller`:
    # this redefinition below should invalidate the cache of `pr48932_callee` but not that of `pr48932_caller`
    pr48932_callee(x) = (print(GLOBAL_BUFFER, x); nothing)
    @test !any(INVALIDATION_TESTER_CACHE.dict) do (mi, ci)
        mi.def.name === :pr48932_callee
    end
    @test any(INVALIDATION_TESTER_CACHE.dict) do (mi, ci)
        mi.def.name === :pr48932_caller
    end
    @test isnothing(pr48932_caller(42))
    @test "42" == String(take!(GLOBAL_BUFFER))
end

# we can avoid adding backedge even if the callee's return type is not the top
# when the return value is not used within the caller
begin take!(GLOBAL_BUFFER)

    pr48932_callee_inferrable(x) = (print(GLOBAL_BUFFER, x); nothing)
    pr48932_caller_unuse(x) = (pr48932_callee_inferrable(Base.inferencebarrier(x)); nothing)

    # assert that type and effects information inferred from `pr48932_callee(::Any)` are the top
    let rt = only(Base.return_types(pr48932_callee_inferrable, (Any,)))
        @test rt === Nothing
        effects = Base.infer_effects(pr48932_callee_inferrable, (Any,))
        @test Core.Compiler.Effects(effects; noinbounds=false) == Core.Compiler.Effects()
    end

    # run inference on both `pr48932_caller` and `pr48932_callee`:
    # we don't need to add backedge to `pr48932_callee` from `pr48932_caller`
    # since the inference result of `pr48932_callee` is maximized and it's not inlined
    let (src, rt) = code_typed((Int,); interp=InvalidationTester()) do x
            @inline pr48932_caller_unuse(x)
        end |> only
        @test rt === Nothing
        @test any(iscall((src, pr48932_callee_inferrable)), src.code)
    end
    @test any(INVALIDATION_TESTER_CACHE.dict) do (mi, ci)
        mi.def.name === :pr48932_callee_inferrable
    end
    @test any(INVALIDATION_TESTER_CACHE.dict) do (mi, ci)
        mi.def.name === :pr48932_caller_unuse
    end
    @test isnothing(pr48932_caller_unuse(42))
    @test "42" == String(take!(GLOBAL_BUFFER))

    # test that we didn't add the backedge from `pr48932_callee_inferrable` to `pr48932_caller_unuse`:
    # this redefinition below should invalidate the cache of `pr48932_callee_inferrable` but not that of `pr48932_caller_unuse`
    pr48932_callee_inferrable(x) = (print(GLOBAL_BUFFER, "foo"); x)
    @test !any(INVALIDATION_TESTER_CACHE.dict) do (mi, ci)
        mi.def.name === :pr48932_callee_inferrable
    end
    @test any(INVALIDATION_TESTER_CACHE.dict) do (mi, ci)
        mi.def.name === :pr48932_caller_unuse
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
        @test Core.Compiler.Effects(effects; noinbounds=false) == Core.Compiler.Effects()
    end

    # run inference on `pr48932_caller_inlined` and `pr48932_callee_inlined`
    let (src, rt) = code_typed((Int,); interp=InvalidationTester()) do x
            @inline pr48932_caller_inlined(x)
        end |> only
        @test rt === Any
        @test any(isinvoke(:pr48932_callee_inlined), src.code)
    end
    @test any(INVALIDATION_TESTER_CACHE.dict) do (mi, ci)
        mi.def.name === :pr48932_callee_inlined
    end
    @test any(INVALIDATION_TESTER_CACHE.dict) do (mi, ci)
        mi.def.name === :pr48932_caller_inlined
    end
    @test 42 == pr48932_caller_inlined(42)
    @test "42" == String(take!(GLOBAL_BUFFER))

    # test that we added the backedge from `pr48932_callee_inlined` to `pr48932_caller_inlined`:
    # this redefinition below should invalidate the cache of `pr48932_callee_inlined` but not that of `pr48932_caller_inlined`
    @noinline pr48932_callee_inlined(@nospecialize x) = (print(GLOBAL_BUFFER, x); nothing)
    @test !any(INVALIDATION_TESTER_CACHE.dict) do (mi, ci)
        mi.def.name === :pr48932_callee_inlined
    end
    @test !any(INVALIDATION_TESTER_CACHE.dict) do (mi, ci)
        mi.def.name === :pr48932_caller_inlined
    end
    @test isnothing(pr48932_caller_inlined(42))
    @test "42" == String(take!(GLOBAL_BUFFER))
end
