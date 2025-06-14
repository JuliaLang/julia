# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

module ThereAreNoInvalidations
    function julia_expr_cmd(expr::Expr)
        `$(Base.julia_cmd()) -e $expr`
    end
    function readline_julia_expr_cmd(expr::Expr)
        open(readline, julia_expr_cmd(expr))
    end
    function invalidations_expr(expr::Expr)
        quote
            try
                let invs = ccall(:jl_debug_method_invalidation, Any, (Cint,), 1)
                    @eval $expr
                    invs
                end
            finally
                ccall(:jl_debug_method_invalidation, Any, (Cint,), 0)
            end
        end
    end
    function there_are_no_invalidations_expr(expr::Expr)
        e = invalidations_expr(expr)
        quote
            let invalidations = $e
                show(isempty(invalidations))
            end
        end
    end
    function there_are_no_invalidations(expr::Expr)
        e = there_are_no_invalidations_expr(expr)
        s = readline_julia_expr_cmd(e)
        parse(Bool, s)
    end
end

function type_expr(f, supertype::Type)
    type_name = :T
    rest = f(type_name)
    quote
        struct $type_name <: $supertype end
        $rest
    end
end

function test_expr(f, supertype::Type)
    e = type_expr(f, supertype)
    ThereAreNoInvalidations.there_are_no_invalidations(e)
end

@testset "no invalidations test set" begin
    @testset "2-arg `show` for various new types" begin
        stypes = (Integer, AbstractString, DenseVector{UInt8}, DenseMatrix{Float32}, AbstractUnitRange{Int})
        broken = (AbstractString,)
        @testset "S: $S" for S ∈ stypes
            @test test_expr((n -> :(function Base.show(::IO, ::$n) end)), S) broken=(S ∈ broken)
        end
    end
    @testset "new subtype of `AbstractString`" begin
        @testset "index-related" begin
            fs = (thisind, prevind, nextind)
            broken = (thisind, prevind, nextind)
            @testset "f: $f" for f ∈ fs
                @test test_expr((n -> :(function Base.$f(::$n, ::Int) end)), AbstractString) broken=(f ∈ broken)
            end
        end
    end
    @testset "new subtype of `Integer`" begin
        int_types = (Bool, Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128, BigInt)
        @testset "construction of old int type from new type" begin
            broken = (UInt16, Int, UInt)
            (Int === Int64) && (broken = (broken..., Int32))
            @testset "T: $T" for T ∈ int_types
                @test test_expr((n -> :(function Base.$T(::$n) end)), Integer) broken=(T ∈ broken)
            end
        end
        @testset "construction of new int type from old type" begin
            broken = ()
            @testset "T: $T" for T ∈ int_types
                @test test_expr((n -> :(function $n(::$T) end)), Integer) broken=(T ∈ broken)
            end
        end
        @testset "arithmetic between old int types and new int type" begin
            ops = (:+, :*, :<<, :>>, :>>>)
            broken = ()
            @testset "T: $T" for T ∈ int_types
                @testset "op: $op" for op ∈ ops
                    @test (
                        test_expr((n -> :(function Base.$op(::$n, ::$T) end)), Integer) &&
                        test_expr((n -> :(function Base.$op(::$T, ::$n) end)), Integer)
                    ) broken=((T, op) ∈ broken)
                end
            end
        end
        @testset "promotion between old int types and new int type" begin
            broken = (Bool, UInt8, Int)
            @testset "T: $T" for T ∈ int_types
                @test (
                    test_expr((n -> :(function Base.promote_rule(::Type{$T}, ::Type{$n}) end)), Integer) &&
                    test_expr((n -> :(function Base.promote_rule(::Type{$n}, ::Type{$T}) end)), Integer)
                ) broken=(T ∈ broken)
            end
        end
        @testset "unary functions" begin
            fs = (zero, one)
            broken = (zero, one)
            @testset "f: $f" for f ∈ fs
                @test test_expr((n -> :(function Base.$f(::$n) end)), Integer) broken=(f ∈ broken)
            end
        end
    end
end
