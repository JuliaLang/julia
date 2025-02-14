original_load_path = copy(Base.LOAD_PATH)
pushfirst!(Base.LOAD_PATH, joinpath(@__DIR__, "plugins"))

using Test
using Tracer

# XXX: should these be in `Tracer/test/runtests.jl`?

function fib(x)
    if x <= 1
        return x
    else
        return fib(x-1) + fib(x-2)
    end
end

let tr = trace(fib, 1)
    @test tr.f == fib
    @test tr.args == (1,)
    child = only(tr.children)
    @test child.f == Base.:<=
    @test child.args == (1,1)
end

let tr = trace(fib, 2)
    @test tr.f == fib
    @test tr.args == (2,)
    @test length(tr.children) == 6
end


using MultilineFusion

# XXX: should these be in `MultilineFusion/test/runtests.jl`?

function multiline(A, B)
    C = A .* B
    D = C .+ A
end

let A = ones(3,3)
    B = ones(3)
    @test (@inferred multiline_fusion(multiline, A, B))::Matrix{Float64} == multiline(A, B)
end

let (ir, _) = only(Base.code_ircode(multiline, (Matrix{Float64}, Vector{Float64}), optimize_until="compact 1"))
    @test length(ir.stmts) == 5
    @test ir.stmts[2][:stmt].args[1] == GlobalRef(Base, :materialize)
end

let (ir, _) = only(Base.code_ircode(multiline, (Matrix{Float64}, Vector{Float64}), optimize_until="compact 1", interp=MultilineFusion.MLFInterp()))
    @test length(ir.stmts) == 4
end

# XXX: should these be in `CustomMethodTables/test/runtests.jl`?
using CustomMethodTables

Base.Experimental.@MethodTable(CustomMT)
Base.Experimental.@overlay CustomMT Base.sin(x::Float64) = Base.cos(x)

# FIXME: Currently doesn't infer and ends in "Skipped call_within since compiler plugin not constant"
overlay(f, args...) = CustomMethodTables.overlay(CustomMT, f, args...)
@test_broken overlay(sin, 1.0) == cos(1.0) # Bug in inference, not using the method_table for initial lookup
@test overlay((x)->sin(x), 1.0) == cos(1.0)

empty!(Base.LOAD_PATH)
append!(Base.LOAD_PATH, original_load_path)