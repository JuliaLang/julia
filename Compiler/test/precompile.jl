# This file is a part of Julia. License is MIT: https://julialang.org/license

# Tests for Compiler/src/precompile.jl: the logic that selects and compiles code for
# system and package images (`--output-o`, `--output-ji`), formerly precompile_utils.c.

include("setup_Compiler.jl")

using Test

@testset "compile_all_tvar_union" begin
    # When generating a system image, every method definition is compiled for the
    # signatures obtainable by expanding `where T<:Union{...}` typevars. Typevars with
    # an abstract upper bound are not worth instantiating: doing so would compile a
    # fully generic specialization for every such method, which is wasteful and can
    # even fail codegen (e.g. `LLVMPtr{T, Any}` has no valid address space).
    # A typevar used as a type parameter makes the instantiated signature a dispatch
    # tuple (`Type{TVParam{Any}}` is concrete), which is how e.g. `LLVMPtr{T, Any}`
    # methods ended up being compiled.
    struct TVParam{T} end
    f_tvar_any(::Type{TVParam{T}}) where {T} = T
    @test !Compiler.compile_all_tvar_union(Tuple{typeof(f_tvar_any), Type{TVParam{T}}} where {T})
    @test isempty(Base.specializations(only(methods(f_tvar_any))))

    f_tvar_abstract(::Type{TVParam{T}}) where {T<:Real} = T
    @test !Compiler.compile_all_tvar_union(Tuple{typeof(f_tvar_abstract), Type{TVParam{T}}} where {T<:Real})
    @test isempty(Base.specializations(only(methods(f_tvar_abstract))))

    # ... but typevars bounded by a union of concrete types are expanded and compiled
    f_tvar_union(x::T) where {T<:Union{Int,Float64}} = x
    @test Compiler.compile_all_tvar_union(Tuple{typeof(f_tvar_union), T} where {T<:Union{Int,Float64}})
    specs = collect(Base.specializations(only(methods(f_tvar_union))))
    @test Set(mi.specTypes for mi in specs) ==
          Set([Tuple{typeof(f_tvar_union), Int}, Tuple{typeof(f_tvar_union), Float64}])
end
