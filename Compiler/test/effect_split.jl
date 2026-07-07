# This file is a part of Julia. License is MIT: https://julialang.org/license

# tests for `Core.invoke_split_effects` (effect preconditions), the conditional
# `@assume_effects` syntax, and the nothrow-precondition synthesis in the inliner

using Test
using InteractiveUtils: code_llvm

include("setup_Compiler.jl")
include("irutils.jl")

const CHECK_BOUNDS_OFF = Base.JLOptions().check_bounds == 2
const COVERAGE = (Base.JLOptions().code_coverage > 0) || (Base.JLOptions().malloc_log > 0)
const OPT_LEVEL = Base.JLOptions().opt_level

# name of the method a statically-resolved `:invoke` statement targets, or nothing
function invoked_name(@nospecialize(x))
    isexpr(x, :invoke) || return nothing
    target = x.args[1]
    if target isa Core.CodeInstance
        target = target.def
    end
    target isa Core.MethodInstance || return nothing
    m = target.def
    return m isa Method ? m.name : nothing
end

esplit_getval(A::Vector{Float64}, i::Int) = A[i] + A[i+1]

@testset "invoke_split_effects runtime semantics" begin
    # dynamic (builtin) path
    @test Core.invoke_split_effects(:nothrow, +, 1, 2) === 3
    @test_throws TypeError Core.invoke_split_effects("nothrow", +, 1, 2)

    # results and thrown errors must agree with the plain call
    usesplit(A, i) = Core.invoke_split_effects(:nothrow, esplit_getval, A, i)
    A = [1.0, 2.0, 3.0]
    @test usesplit(A, 1) === 3.0
    @test usesplit(A, 2) === 5.0
    @test_throws BoundsError usesplit(A, 3)
    @test_throws BoundsError usesplit(A, 0)

    # non-constant `which` and effects other than `:nothrow` fall back gracefully
    dynwhich(w) = Core.invoke_split_effects(w, esplit_getval, [1.0, 2.0], 1)
    @test dynwhich(:nothrow) === 3.0
    @test dynwhich(:effect_free) === 3.0
end

@Base.assume_effects (isfinite(x) && :nothrow) @noinline function esplit_sin(x::Float64)
    sin(x)
end

@testset "conditional @assume_effects" begin
    preconds = only(methods(esplit_sin)).preconditions
    @test length(preconds) == 2
    cond = preconds[1]
    @test cond isa Base.EffectsOverride
    @test cond.nothrow && !cond.consistent && !cond.effect_free
    # the check function receives the same arguments as the method itself
    @test preconds[2](esplit_sin, 1.0) === true
    @test preconds[2](esplit_sin, Inf) === false

    splitsin(x) = Core.invoke_split_effects(:nothrow, esplit_sin, x)
    @test splitsin(1.0) === sin(1.0)
    @test_throws DomainError splitsin(Inf)
    @test isnan(splitsin(NaN))
end

# a callee whose per-iteration bounds check cannot be proven away in place
# (nothing relates `upto` to `length(A)`)
@inline function esplit_sum_partial(A::Vector{Int}, upto::Int)
    s = 0
    for i = 1:upto
        s += A[i]
    end
    return s
end
esplit_sum_plain(A::Vector{Int}, upto::Int) = esplit_sum_partial(A, upto)
esplit_sum_split(A::Vector{Int}, upto::Int) = Core.invoke_split_effects(:nothrow, esplit_sum_partial, A, upto)

@testset "synthesized precondition: runtime semantics" begin
    A = collect(Int64(1):Int64(100))
    @test esplit_sum_split(A, 100) === esplit_sum_plain(A, 100)
    @test esplit_sum_split(A, 17) === esplit_sum_plain(A, 17)
    @test esplit_sum_split(A, 0) === 0
    @test_throws BoundsError esplit_sum_split(A, 101)
end

# The structure and codegen tests below assume bounds checks exist (they are
# what gets split) and that inlining works normally.
if !CHECK_BOUNDS_OFF && !COVERAGE

@testset "synthesized precondition: optimized IR structure" begin
    src = code_typed1(esplit_sum_split, (Vector{Int}, Int))
    # The equivalent plain call remains exactly once: the outlined fallback of
    # the effect split. The fast path is a separate inlined copy of the callee.
    @test count(@nospecialize(x)->invoked_name(x) === :esplit_sum_partial, src.code) == 1
    # The inlined fast path performs its loads without any reachable throw
    @test any(iscall((src, Core.memoryrefget)), src.code)
    @test !any(src.code) do @nospecialize x
        name = invoked_name(x)
        return name !== nothing && occursin("throw", String(name))
    end
    # the split branches on the (inlined) synthesized precondition
    @test any(@nospecialize(x)->isa(x, Core.GotoIfNot), src.code)
end

@testset "manual precondition: assume-side call is removable" begin
    # the result of the split call is unused, so the fast path (assumed
    # nothrow, and otherwise effect-free) must be deleted by the Julia-level
    # optimizer, leaving only the guarded fallback call
    function esplit_unused_loop(n)
        for i = 1:n
            Core.invoke_split_effects(:nothrow, esplit_sin, Float64(i))
        end
    end
    src = code_typed1(esplit_unused_loop, (Int,))
    @test count(@nospecialize(x)->invoked_name(x) === :esplit_sin, src.code) == 1

    # LLVM proves `isfinite(Float64(::Int64))`, so the fallback call is
    # unreachable and the loop disappears entirely
    if OPT_LEVEL >= 2
        ir = sprint(code_llvm, esplit_unused_loop, (Int,))
        @test !occursin("esplit_sin", ir)
    end
end

@testset "synthesized precondition: bounds check hoisted out of codegen" begin
    # the plain (inlined) version keeps the bounds check and throw inside the
    # emitted function ...
    ir_plain = sprint(code_llvm, esplit_sum_plain, (Vector{Int}, Int))
    @test occursin("throw_boundserror", ir_plain)
    # ... while in the split version all checks live in the synthesized
    # precondition and the throwing path is an outlined call: no bounds-error
    # throw is emitted into the function itself
    ir_split = sprint(code_llvm, esplit_sum_split, (Vector{Int}, Int))
    @test !occursin("throw_boundserror", ir_split)
    @test occursin("esplit_sum_partial", ir_split) # the outlined fallback
end

end # !CHECK_BOUNDS_OFF && !COVERAGE
