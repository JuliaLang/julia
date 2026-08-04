# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

# ---- OSR test harness -------------------------------------------------
#
# On-stack replacement only happens in tier-parked (interpreted) frames, so
# each check evaluates a FRESH copy of its function (a fresh method is always
# parked), lowers the OSR statement budget so the first loop trips it, and
# reads the runtime's attempt/entry counters to distinguish "a continuation
# was entered" from "the plan declined and the interpreter finished the
# frame". Either way the observable result must equal plain evaluation.
#
# The counters are process-global; keep OSR tests in this file serial.

const tier_enabled = ccall(:jl_tier_enabled, Cint, ()) != 0

function osr_stats()
    a = Ref{UInt64}(0)
    e = Ref{UInt64}(0)
    ccall(:jl_tier_get_osr_stats, Cvoid, (Ptr{UInt64}, Ptr{UInt64}), a, e)
    return (attempts = a[], entered = e[])
end

# Evaluate a fresh copy of `def`, a `function _(args...) ... end` expression
# whose placeholder name `_` is replaced with a gensym, and return the new
# (never yet called, hence parked) function object.
function osr_fresh(def::Expr)
    def = deepcopy(def)
    @assert def.head === :function
    sig = def.args[1]
    while Meta.isexpr(sig, :where)
        sig = sig.args[1]
    end
    @assert Meta.isexpr(sig, :call) && sig.args[1] === :_ "osr_fresh definitions name the function `_`"
    fname = gensym(:osr_fn)
    sig.args[1] = fname
    Core.eval(@__MODULE__, def)
    # read the binding in the latest world: inside a testset the whole block
    # is one top-level expression, so the enclosing world predates the eval
    return Core.eval(@__MODULE__, fname)
end

"""
    osr_call(f, args...; budget=64, catch_exceptions=false)

Call `f(args...)` with the OSR statement budget lowered to `budget`
(restored afterwards) and return `(; result, attempts, entered)` where the
counts are the OSR activity of this call. With `catch_exceptions`, a thrown
exception becomes the `result`.
"""
function osr_call(f, args...; budget::Integer=64, catch_exceptions::Bool=false)
    old = ccall(:jl_tier_get_osr_threshold, UInt32, ())
    s0 = osr_stats()
    ccall(:jl_tier_set_osr_threshold, Cvoid, (Cint,), Cint(budget))
    local r
    try
        # run on a fresh task with a generous stack: OSR is only attempted
        # with ample C-stack headroom (the interpreter otherwise rescues the
        # frame by compiling it), and the enclosing test harness stack depth
        # must not decide which path we exercise
        t = Task(() -> Base.invokelatest(f, args...), 16 << 20)
        t.sticky = true
        schedule(t)
        r = fetch(t)
    catch ex
        catch_exceptions || rethrow()
        r = ex isa TaskFailedException ? ex.task.exception : ex
    finally
        ccall(:jl_tier_set_osr_threshold, Cvoid, (Cint,), old <= typemax(Cint) ? Cint(old) : typemax(Cint))
    end
    s1 = osr_stats()
    return (result = r,
            attempts = Int(s1.attempts - s0.attempts),
            entered = Int(s1.entered - s0.entered))
end

# Fresh-eval `def`, run it once under a small budget, and check both the
# result and whether a continuation was actually entered. Tests that exercise
# constructs the plan must refuse pass `expect_entered=false`: the budget
# still fires (attempts >= 1) but no continuation runs, proving the decline
# path (rather than the test passing vacuously through the interpreter).
function check_osr(expected, def::Expr, args...; expect_entered::Bool,
                   budget::Integer=64, catch_exceptions::Bool=false)
    f = osr_fresh(def)
    st = osr_call(f, args...; budget, catch_exceptions)
    if catch_exceptions
        @test st.result isa expected
    else
        @test st.result == expected
    end
    @test st.attempts >= 1
    if expect_entered
        @test st.entered >= 1
    else
        @test st.entered == 0
    end
    return st
end

if !tier_enabled
    @info "tiered compilation disabled; skipping OSR continuation tests"
else

@testset "OSR continuations" begin
    @testset "loop escapes via continuation" begin
        check_osr(5050, :(function _(n)
            s = 0
            for i in 1:n
                s += i
            end
            return s
        end), 100; expect_entered=true)
    end

    @testset "loop-carried state and arguments" begin
        check_osr((:tag, 4950), :(function _(x, n)
            s = 0
            for i in 1:n
                s += i - 1
            end
            return (x, s)
        end), :tag, 100; expect_entered=true)
    end

    @testset "concrete static parameters" begin
        check_osr(Int, :(function _(x::T) where T
            s = 0
            for i in 1:2000
                s += i
            end
            return T
        end), 1; expect_entered=true)
        # @isdefined on a concrete sparam must rewrite to a boolean, not
        # malformed Expr(:isdefined, QuoteNode(...)) IR
        check_osr(true, :(function _(x::T) where T
            s = 0
            for i in 1:2000
                s += i
            end
            return @isdefined(T)
        end), 1; expect_entered=true)
    end

    @testset "uncertain static parameters decline" begin
        # the sparam is only representable as the runtime's svec uncertainty
        # marker; using it must still throw, exactly like the interpreter
        check_osr(UndefVarError, :(function _(x::Union{T,Nothing}, y::Union{T,Nothing}) where T
            s = 0
            for i in 1:2000
                s += i
            end
            return T
        end), nothing, nothing; expect_entered=false, catch_exceptions=true)
    end

    @testset "unrecognized frame-relative IR declines" begin
        # Core.Argument is valid lowered IR (the interpreter reads it like a
        # slot) but the continuation renumbers arguments; it must decline
        check_osr(:expected, :(function _(x, n)
            s = 0
            for i in 1:n
                s += i
            end
            return $(Core.Argument(2))
        end), :expected, 100; expect_entered=false)
        # Core.PiNode carries a nested reference; also decline. The node sits
        # on a dead path: the plan scans every statement (so the decline is
        # still proven), while codegen of a raw PiNode in lowered source emits
        # `unreachable`, so an executed one would crash any compiled variant
        # (e.g. if the method is ever promoted or stack-rescued).
        check_osr(:expected, :(function _(x, n)
            s = 0
            for i in 1:n
                s += i
            end
            n < 0 && return $(Core.PiNode(Core.Argument(2), Any))
            return x
        end), :expected, 100; expect_entered=false)
    end

    @testset "exception regions decline" begin
        check_osr(42, :(function _(n)
            s = 0
            for i in 1:n
                s += i
            end
            try
                s = 42
            catch
            end
            return s
        end), 100; expect_entered=false)
    end

    @testset "thrown exceptions propagate from continuations" begin
        check_osr(DivideError, :(function _(n)
            s = 0
            for i in 1:n
                s += i
            end
            return s ÷ (n - n)
        end), 100; expect_entered=true, catch_exceptions=true)
    end
end

end # tier_enabled
