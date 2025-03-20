# This file is a part of Julia. License is MIT: https://julialang.org/license

# setup
# -----

include("irutils.jl")

using Test

struct InvalidationTesterToken end

struct InvalidationTester <: Compiler.AbstractInterpreter
    world::UInt
    inf_params::Compiler.InferenceParams
    opt_params::Compiler.OptimizationParams
    inf_cache::Vector{Compiler.InferenceResult}
    function InvalidationTester(;
                                world::UInt = Base.get_world_counter(),
                                inf_params::Compiler.InferenceParams = Compiler.InferenceParams(),
                                opt_params::Compiler.OptimizationParams = Compiler.OptimizationParams(),
                                inf_cache::Vector{Compiler.InferenceResult} = Compiler.InferenceResult[])
        return new(world, inf_params, opt_params, inf_cache)
    end
end

Compiler.InferenceParams(interp::InvalidationTester) = interp.inf_params
Compiler.OptimizationParams(interp::InvalidationTester) = interp.opt_params
Compiler.get_inference_world(interp::InvalidationTester) = interp.world
Compiler.get_inference_cache(interp::InvalidationTester) = interp.inf_cache
Compiler.cache_owner(::InvalidationTester) = InvalidationTesterToken()

# basic functionality test
# ------------------------

basic_callee(x) = x
basic_caller(x) = basic_callee(x)

# run inference and check that cache exist
@test Base.return_types((Float64,); interp=InvalidationTester()) do x
    basic_caller(x)
end |> only === Float64

let mi = Base.method_instance(basic_callee, (Float64,))
    ci = mi.cache
    @test !isdefined(ci, :next)
    @test ci.owner === InvalidationTesterToken()
    @test ci.max_world == typemax(UInt)
end

let mi = Base.method_instance(basic_caller, (Float64,))
    ci = mi.cache
    @test !isdefined(ci, :next)
    @test ci.owner === InvalidationTesterToken()
    @test ci.max_world == typemax(UInt)
end

# this redefinition below should invalidate the cache
const BASIC_CALLER_WORLD = Base.get_world_counter()+1
basic_callee(x) = x, x
@test !isdefined(Base.method_instance(basic_callee, (Float64,)), :cache)
let mi = Base.method_instance(basic_caller, (Float64,))
    ci = mi.cache
    @test !isdefined(ci, :next)
    @test ci.owner === InvalidationTesterToken()
    @test ci.max_world == BASIC_CALLER_WORLD
end

# re-run inference and check the result is updated (and new cache exists)
@test Base.return_types((Float64,); interp=InvalidationTester()) do x
    basic_caller(x)
end |> only === Tuple{Float64,Float64}
let mi = Base.method_instance(basic_callee, (Float64,))
    ci = mi.cache
    @test !isdefined(ci, :next)
    @test ci.owner === InvalidationTesterToken()
    @test ci.max_world == typemax(UInt)
end

let mi = Base.method_instance(basic_caller, (Float64,))
    ci = mi.cache
    @test isdefined(ci, :next)
    @test ci.owner === InvalidationTesterToken()
    @test ci.max_world == typemax(UInt)
    ci = ci.next
    @test !isdefined(ci, :next)
    @test ci.owner === InvalidationTesterToken()
    @test ci.max_world != typemax(UInt)
end


# backedge optimization
# ---------------------

const GLOBAL_BUFFER = IOBuffer()

# test backedge optimization when the callee's type and effects information are maximized
begin
    take!(GLOBAL_BUFFER)

    pr48932_callee(x) = (print(GLOBAL_BUFFER, x); Base.inferencebarrier(x))
    pr48932_caller(x) = pr48932_callee(Base.inferencebarrier(x))

    # assert that type and effects information inferred from `pr48932_callee(::Any)` are the top
    let rt = only(Base.return_types(pr48932_callee, (Any,)))
        @test rt === Any
        effects = Base.infer_effects(pr48932_callee, (Any,))
        @test effects == Compiler.Effects()
    end

    # run inference on both `pr48932_caller` and `pr48932_callee`
    let (src, rt) = code_typed((Int,); interp=InvalidationTester()) do x
            @inline pr48932_caller(x)
        end |> only
        @test rt === Any
        @test any(iscall((src, pr48932_callee)), src.code)
    end

    let mi = only(Base.specializations(Base.only(Base.methods(pr48932_callee))))
        # Base.method_instance(pr48932_callee, (Any,))
        ci = mi.cache
        @test isdefined(ci, :next)
        @test ci.owner === InvalidationTesterToken()
        @test ci.max_world == typemax(UInt)

        # In cache due to Base.return_types(pr48932_callee, (Any,))
        ci = ci.next
        @test !isdefined(ci, :next)
        @test ci.owner === nothing
        @test ci.max_world == typemax(UInt)
    end
    let mi = Base.method_instance(pr48932_caller, (Int,))
        ci = mi.cache
        @test !isdefined(ci, :next)
        @test ci.owner === InvalidationTesterToken()
        @test ci.max_world == typemax(UInt)
    end

    @test 42 == pr48932_caller(42)
    @test "42" == String(take!(GLOBAL_BUFFER))

    # test that we didn't add the backedge from `pr48932_callee` to `pr48932_caller`:
    # this redefinition below should invalidate the cache of `pr48932_callee` but not that of `pr48932_caller`
    pr48932_callee(x) = (print(GLOBAL_BUFFER, x); nothing)

    @test length(Base.methods(pr48932_callee)) == 1
    @test Base.only(Base.methods(pr48932_callee, Tuple{Any})) === only(Base.methods(pr48932_callee))
    @test isempty(Base.specializations(Base.only(Base.methods(pr48932_callee, Tuple{Any}))))
    let mi = only(Base.specializations(Base.only(Base.methods(pr48932_caller))))
        # Base.method_instance(pr48932_callee, (Any,))
        ci = mi.cache
        @test isdefined(ci, :next)
        @test ci.owner === nothing
        @test_broken ci.max_world == typemax(UInt)
        ci = ci.next
        @test !isdefined(ci, :next)
        @test ci.owner === InvalidationTesterToken()
        @test_broken ci.max_world == typemax(UInt)
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
        @test effects == Compiler.Effects()
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

    let mi = only(Base.specializations(Base.only(Base.methods(pr48932_callee_inferable))))
        ci = mi.cache
        @test isdefined(ci, :next)
        @test ci.owner === InvalidationTesterToken()
        @test ci.max_world == typemax(UInt)
        ci = ci.next
        @test !isdefined(ci, :next)
        @test ci.owner === nothing
        @test ci.max_world == typemax(UInt)
    end
    let mi = Base.method_instance(pr48932_caller_unuse, (Int,))
        ci = mi.cache
        @test !isdefined(ci, :next)
        @test ci.owner === InvalidationTesterToken()
        @test ci.max_world == typemax(UInt)
    end

    @test isnothing(pr48932_caller_unuse(42))
    @test "42" == String(take!(GLOBAL_BUFFER))

    # test that we didn't add the backedge from `pr48932_callee_inferable` to `pr48932_caller_unuse`:
    # this redefinition below should invalidate the cache of `pr48932_callee_inferable` but not that of `pr48932_caller_unuse`
    pr48932_callee_inferable(x) = (print(GLOBAL_BUFFER, "foo"); x)

    @test isempty(Base.specializations(Base.only(Base.methods(pr48932_callee_inferable, Tuple{Any}))))
    let mi = Base.method_instance(pr48932_caller_unuse, (Int,))
        ci = mi.cache
        @test isdefined(ci, :next)
        @test ci.owner === nothing
        @test_broken ci.max_world == typemax(UInt)
        ci = ci.next
        @test !isdefined(ci, :next)
        @test ci.owner === InvalidationTesterToken()
        @test_broken ci.max_world == typemax(UInt)
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
        @test effects == Compiler.Effects()
    end

    # run inference on `pr48932_caller_inlined` and `pr48932_callee_inlined`
    let (src, rt) = code_typed((Int,); interp=InvalidationTester()) do x
            @inline pr48932_caller_inlined(x)
        end |> only
        @test rt === Any
        @test any(isinvoke(:pr48932_callee_inlined), src.code)
    end

    let mi = Base.method_instance(pr48932_callee_inlined, (Int,))
        ci = mi.cache
        @test isdefined(ci, :next)
        @test ci.owner === InvalidationTesterToken()
        @test ci.max_world == typemax(UInt)
        ci = ci.next
        @test !isdefined(ci, :next)
        @test ci.owner === nothing
        @test ci.max_world == typemax(UInt)
    end
    let mi = Base.method_instance(pr48932_caller_inlined, (Int,))
        ci = mi.cache
        @test !isdefined(ci, :next)
        @test ci.owner === InvalidationTesterToken()
        @test ci.max_world == typemax(UInt)
    end

    @test 42 == pr48932_caller_inlined(42)
    @test "42" == String(take!(GLOBAL_BUFFER))

    # test that we added the backedge from `pr48932_callee_inlined` to `pr48932_caller_inlined`:
    # this redefinition below should invalidate the cache of `pr48932_callee_inlined` but not that of `pr48932_caller_inlined`
    @noinline pr48932_callee_inlined(@nospecialize x) = (print(GLOBAL_BUFFER, x); nothing)

    @test isempty(Base.specializations(Base.only(Base.methods(pr48932_callee_inlined, Tuple{Any}))))
    let mi = Base.method_instance(pr48932_caller_inlined, (Int,))
        ci = mi.cache
        @test isdefined(ci, :next)
        @test ci.owner === nothing
        @test ci.max_world != typemax(UInt)
        ci = ci.next
        @test !isdefined(ci, :next)
        @test ci.owner === InvalidationTesterToken()
        @test ci.max_world != typemax(UInt)
    end

    @test isnothing(pr48932_caller_inlined(42))
    @test "42" == String(take!(GLOBAL_BUFFER))
end
