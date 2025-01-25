# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base.ScopedValues

include(joinpath(@__DIR__,"../Compiler/test/irutils.jl"))

@testset "errors" begin
    @test ScopedValue{Float64}(1)[] == 1.0
    @test_throws InexactError ScopedValue{Int}(1.5)
    let val = ScopedValue(1)
        @test_throws MethodError val[] = 2
        with() do
            @test_throws MethodError val[] = 2
        end
    end
    let val = ScopedValue{String}()
        @test_throws KeyError val[]
    end
    let val = ScopedValue{Int}()
        @test_throws KeyError val[]
    end
    @test_throws MethodError ScopedValue()
end

const sval = ScopedValue(1)
@testset "inheritance" begin
    @test sval[] == 1
    with() do
        @test sval[] == 1
        with() do
            @test sval[] == 1
        end
        with(sval => 2) do
            @test sval[] == 2
        end
        @test sval[] == 1
    end
    @test sval[] == 1
end

const sval_float = ScopedValue(1.0)

@testset "multiple scoped values" begin
    with(sval => 2, sval_float => 2.0) do
        @test sval[] == 2
        @test sval_float[] == 2.0
    end
    with(sval => 2, sval => 3) do
        @test sval[] == 3
    end
end

emptyf() = nothing

@testset "conversion" begin
    with(emptyf, sval_float=>2)
    @test_throws MethodError with(emptyf, sval_float=>"hello")
    a = ScopedValue(1)
    with(a => 2.0) do
        @test a[] == 2
        @test a[] isa Int
    end
    a = ScopedValue(1.0)
    with(a => 2) do
        @test a[] == 2.0
        @test a[] isa Float64
    end
end

import Base.Threads: @spawn
@testset "tasks" begin
    @test fetch(@spawn begin
        sval[]
    end) == 1
    with(sval => 2) do
        @test fetch(@spawn begin
            sval[]
        end) == 2
    end
end

@testset "show" begin
    @test sprint(show, ScopedValue{Int}()) == "Base.ScopedValues.ScopedValue{$Int}(undefined)"
    @test sprint(show, sval) == "Base.ScopedValues.ScopedValue{$Int}(1)"
    @test sprint(show, Core.current_scope()) == "nothing"
    with(sval => 2.0) do
        @test sprint(show, sval) == "Base.ScopedValues.ScopedValue{$Int}(2)"
        objid = sprint(show, Base.objectid(sval))
        @test sprint(show, Core.current_scope()) == "Base.ScopedValues.Scope(Base.ScopedValues.ScopedValue{$Int}@$objid => 2)"
    end
end

const depth = ScopedValue(0)
function nth_with(f, n)
    if n <= 0
        f()
    else
        with(depth => n) do
            nth_with(f, n-1)
        end
    end
end


@testset "nested with" begin
    @testset for depth in 1:16
        nth_with(depth) do
            @test sval_float[] == 1.0
        end
        with(sval_float=>2.0) do
            nth_with(depth) do
                @test sval_float[] == 2.0
            end
        end
        nth_with(depth) do
            with(sval_float=>2.0) do
                @test sval_float[] == 2.0
            end
        end
    end
    with(sval_float=>2.0) do
        nth_with(15) do
            @test sval_float[] == 2.0
            with(sval_float => 3.0) do
                @test sval_float[] == 3.0
            end
        end
    end
end

@testset "macro" begin
    @with sval=>2 sval_float=>2.0 begin
        @test sval[] == 2
        @test sval_float[] == 2.0
    end
    # Doesn't do much...
    @with begin
        @test sval[] == 1
        @test sval_float[] == 1.0
    end
    @with sval=>2 sval_float=>2.0 begin
        @with begin
            @test sval[] == 2
            @test sval_float[] == 2.0
        end
    end
end

@testset "isassigned" begin
    sv = ScopedValue(1)
    @test isassigned(sv)
    sv = ScopedValue{Int}()
    @test !isassigned(sv)
    with(sv => 2) do
        @test isassigned(sv)
    end
end

# Test that the `@with` macro doesn't introduce unnecessary PhiC nodes
# (which can be hard for the optimizer to remove).
function with_macro_slot_cross()
    a = 1
    @with sval=>1 begin
        a = sval_float[]
    end
    return a
end

let code = code_typed(with_macro_slot_cross)[1][1].code
    @test !any(x->isa(x, Core.PhiCNode), code)
end

# inline constant scoped values
const inlineable_const_sv = ScopedValue(1)
@test fully_eliminated(; retval=(inlineable_const_sv => 1)) do
    inlineable_const_sv => 1
end

# Handle nothrow scope bodies correctly (#56609)
@eval function nothrow_scope()
    $(Expr(:tryfinally, :(), nothing, 1))
    @test Core.current_scope() === nothing
end
nothrow_scope()

# https://github.com/JuliaLang/julia/issues/56062
@testset "issue #56062" begin
    ts = Int[]
    try
        @with begin
            return
        end
    catch err
    finally
        push!(ts, 2)
    end
end
