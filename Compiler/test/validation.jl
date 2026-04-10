# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Core.IR

include("setup_Compiler.jl")

@testset "stdio validation" begin
    for s in (:stdout, :stderr, :print, :println, :write)
        @test getglobal(Compiler, s) === getglobal(Core, s)
        @test isconst(Compiler, s)
    end
    @test Compiler.stdin === devnull
    @test isconst(Compiler, :stdin)
end

function f22938(a, b, x...)
    nothing
    nothing
    nothing
    nothing
    nothing
    nothing
    nothing
    d = 1
    a = d
    for i in 1:b
        d += i
    end
    return i * a
end

const c0 = let
    msig = Tuple{typeof(f22938),Int,Int,Int,Int}
    world = Base.get_world_counter()
    match = only(Base._methods_by_ftype(msig, -1, world))
    mi = Compiler.specialize_method(match)
    c0 = Compiler.retrieve_code_info(mi, world)
    @test isempty(Compiler.validate_code(mi, c0))
    c0
end

@testset "INVALID_EXPR_HEAD" begin
    c = copy(c0)
    c.code[1] = Expr(:invalid, 1)
    errors = Compiler.validate_code(c)
    @test length(errors) == 1
    @test errors[1].kind === Compiler.INVALID_EXPR_HEAD
end

@testset "INVALID_LVALUE" begin
    c = copy(c0)
    c.code[1] = Expr(:(=), GotoNode(1), 1)
    c.code[2] = Expr(:(=), :x, 1)
    c.code[3] = Expr(:(=), 3, 1)
    errors = Compiler.validate_code(c)
    @test length(errors) == 3
    @test all(e.kind === Compiler.INVALID_LVALUE for e in errors)
end

@testset "INVALID_RVALUE" begin
    c = copy(c0)
    c.code[1] = Expr(:(=), SlotNumber(2), GotoNode(1))
    c.code[2] = Expr(:(=), SlotNumber(2), LineNumberNode(2))
    i = 2
    for h in (:line, :const, :meta)
        c.code[i+=1] = Expr(:(=), SlotNumber(2), Expr(h))
    end
    errors = Compiler.validate_code(c)
    @test length(errors) == 5
    @test count(e.kind === Compiler.INVALID_RVALUE for e in errors) == 5
end

@testset "INVALID_CALL_ARG" begin
    c = copy(c0)
    c.code[1] = Expr(:(=), SlotNumber(2), Expr(:call, GlobalRef(Base,:+), SlotNumber(2), GotoNode(1)))
    c.code[2] = Expr(:call, GlobalRef(Base,:-), Expr(:call, GlobalRef(Base,:sin), GotoNode(2)), 3)
    c.code[3] = Expr(:call, LineNumberNode(2))
    i = 3
    for h in (:line, :const, :meta)
        c.code[i+=1] = Expr(:call, GlobalRef(@__MODULE__,:f), Expr(h))
    end
    errors = Compiler.validate_code(c)
    @test length(errors) == 6
    @test count(e.kind === Compiler.INVALID_CALL_ARG for e in errors) == 6
end

@testset "EMPTY_SLOTNAMES" begin
    c = copy(c0)
    empty!(c.slotnames)
    errors = Compiler.validate_code(c)
    @test length(errors) == 2
    @test any(e.kind === Compiler.EMPTY_SLOTNAMES for e in errors)
    @test any(e.kind === Compiler.SLOTFLAGS_MISMATCH for e in errors)
end

@testset "SLOTFLAGS_MISMATCH" begin
    c = copy(c0)
    push!(c.slotflags, 0x00)
    errors = Compiler.validate_code(c)
    @test length(errors) == 1
    @test errors[1].kind === Compiler.SLOTFLAGS_MISMATCH
end

@testset "SSAVALUETYPES_MISMATCH" begin
    c = code_typed(f22938, (Int,Int,Int,Int))[1][1]
    empty!(c.ssavaluetypes)
    errors = Compiler.validate_code(c)
    @test length(errors) == 1
    @test errors[1].kind === Compiler.SSAVALUETYPES_MISMATCH
end

@testset "SSAVALUETYPES_MISMATCH_UNINFERRED" begin
    c = copy(c0)
    c.ssavaluetypes -= 1
    errors = Compiler.validate_code(c)
    @test length(errors) == 1
    @test errors[1].kind === Compiler.SSAVALUETYPES_MISMATCH_UNINFERRED
end

@testset "SSAFLAGS_MISMATCH" begin
    c = copy(c0)
    empty!(c.ssaflags)
    errors = Compiler.validate_code(c)
    @test length(errors) == 1
    @test errors[1].kind === Compiler.SSAFLAGS_MISMATCH
end

@testset "NON_TOP_LEVEL_METHOD" begin
    c = copy(c0)
    c.code[1] = Expr(:method, :dummy)
    errors = Compiler.validate_code(c)
    @test length(errors) == 1
    @test errors[1].kind === Compiler.NON_TOP_LEVEL_METHOD
end
