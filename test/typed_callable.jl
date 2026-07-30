# This file is a part of Julia. License is MIT: https://julialang.org/license

# Test Core.TypedCallable construction, dispatch, and inference.
using Test
using Base.Experimental: @opaque

tc_add1(x) = x + 1

@testset "construction and basic call" begin
    tc = Core.TypedCallable{Tuple{Int},Int}(tc_add1)
    @test tc isa Core.TypedCallable{Tuple{Int},Int}
    # specsig fast path: a concretely-typed caller reaches the target without boxing
    caller(t::Core.TypedCallable{Tuple{Int},Int}) = t(5)
    @test caller(tc) === 6
    # boxed/jlcall path: @nospecialize forces dynamic dispatch through the builtin
    dyn(@nospecialize(t), x) = t(x)
    @test dyn(tc, 5) === 6
    @test_throws ArgumentError Core.TypedCallable{Int,Int}(tc_add1)
end

@testset "latest-world re-resolution" begin
    @eval lw_g(x) = x + 1
    tc = Core.TypedCallable{Tuple{Int},Int}(lw_g)
    cg(t::Core.TypedCallable{Tuple{Int},Int}) = t(10)
    @test cg(tc) === 11
    @eval lw_g(x) = x + 100
    @test cg(tc) === 110
    # Contrast: an OpaqueClosure freezes the construction-time world.
    @eval oc_g(x) = x + 1
    oc = @opaque (x::Int) -> oc_g(x)
    @test oc(10) === 11
    @eval oc_g(x) = x + 100
    @test oc(10) === 11
end

@testset "vararg" begin
    tc_sum(xs...) = sum(xs)
    tc = Core.TypedCallable{Tuple{Vararg{Int}},Int}(tc_sum)
    cv(t::Core.TypedCallable{Tuple{Vararg{Int}},Int}) = t(1, 2, 3, 4)
    @test cv(tc) === 10
    @test tc(1, 2, 3) === 6
end

@testset "type enforcement" begin
    tc_bad(x) = "not an int"
    tc = Core.TypedCallable{Tuple{Int},Int}(tc_bad)
    cbad(t::Core.TypedCallable{Tuple{Int},Int}) = t(1)
    @test_throws TypeError cbad(tc)
    dynbad(@nospecialize(t)) = t(1)
    @test_throws TypeError dynbad(tc)
    tcok = Core.TypedCallable{Tuple{Int},Int}(tc_add1)
    dynarg(@nospecialize(t), @nospecialize(x)) = t(x)
    @test_throws TypeError dynarg(tcok, "x")
    @test_throws MethodError dynarg(tcok, 1, 2)
end

@testset "inference" begin
    caller(t::Core.TypedCallable{Tuple{Int},Int}) = t(5)
    @test Base.return_types(caller, (Core.TypedCallable{Tuple{Int},Int},)) == Any[Int]
    callerf(t::Core.TypedCallable{Tuple{Int},Float64}) = t(1)
    @test Base.return_types(callerf, (Core.TypedCallable{Tuple{Int},Float64},)) == Any[Float64]
end

@testset "trampoline sharing" begin
    # The cache key includes typeof(f), A, R, and the ABI kind.
    share_f(x) = x + 1
    a = Core.TypedCallable{Tuple{Int},Int}(share_f)
    b = Core.TypedCallable{Tuple{Int},Int}(share_f)
    @test getfield(a, 2) === getfield(b, 2)
    @test getfield(a, 2) isa Core.DispatchTrampoline
    c = Core.TypedCallable{Tuple{Int},Float64}(share_f)
    @test getfield(c, 2) !== getfield(a, 2)
    std_tr = ccall(:jl_get_dispatch_trampoline, Any, (Any, Any, Cint, Cint),
                   Tuple{typeof(share_f), Int}, Int, Cint(1), Cint(0))
    @test std_tr !== getfield(a, 2)
end

@testset "dispatcher adapters (no resolvable target)" begin
    # A missing target falls back to dynamic dispatch rather than calling stale code.
    @eval del_f(x::Int) = x * 2
    tcd = Core.TypedCallable{Tuple{Int},Int}(del_f)
    @test tcd(4) === 8
    Base.delete_method(which(del_f, Tuple{Int}))
    @test_throws MethodError tcd(4)
    @eval del_f(x::Int) = x * 3
    @test @invokelatest(tcd(4)) === 12

    disp_f(x) = x + 2
    tca = Core.TypedCallable{Tuple{Any},Any}(disp_f)
    @test tca(3) === 5
end
