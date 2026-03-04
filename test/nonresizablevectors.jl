# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base: LightBoundsError, MemoryVector, MemoryRefVectorImm, MemoryRefVectorMut
using Test

const basic_types = (MemoryVector, MemoryRefVectorImm, MemoryRefVectorMut)

function sprint_splatted(iterator)
    t = tuple(iterator...)
    if t isa Tuple{Any}
        string(only(t))
    else
        string(t)[(begin + 1):(end - 1)]
    end
end

function branch_helper(funcs::Tuple{Any, NTuple{2, Any}}, x...; y...)
    (predicate, (f, g)) = funcs
    if predicate(x...; y...)
        f(x...; y...)
    else
        g(x...; y...)
    end
end

function first_several_iterator_products(i)
    (
        Iterators.product(),
        Iterators.product(i),
        Iterators.product(i, i),
        Iterators.product(i, i, i),
    )
end

function test_helper_some_lengths_some_indices(funcs::Tuple{Any, NTuple{2, Any}}, max_len::Int = 2)
    branch_helper_fgh = Base.Fix1(branch_helper, funcs)
    foreach_splat_branch_helper_fgh = Base.Fix1(foreach, splat(branch_helper_fgh))
    r = (-1):(max_len + 1)
    r_products = first_several_iterator_products(r)
    for n ∈ 0:max_len
        func_2 = Base.Fix1(tuple, n)
        func_1 = Base.Fix1(Iterators.map, func_2)
        iterators = map(func_1, r_products)
        foreach(foreach_splat_branch_helper_fgh, iterators)
    end
end

function test_helper_some_types_some_lengths_some_indices(func::F, func_predicate::P, ::Type{Elt}, types = basic_types) where {F, P, Elt}
    for typ ∈ types
        t = typ{Elt}
        predicate = func_predicate ∘ Base.Fix1(checkbounds_bool_undef, t)
        live_branch = Base.Fix1(func, t)
        dead_branch = Returns(nothing)
        test_helper_some_lengths_some_indices((predicate, (live_branch, dead_branch)))
    end
end

function checkbounds_bool_undef(::Type{T}, n::Integer, i::Tuple) where {T}
    checkbounds(Bool, T(undef, n), i...)
end

@testset "NonResizableVectors.jl" begin
    @testset "subtyping" begin
        for typ ∈ (MemoryVector, MemoryRefVectorImm, MemoryRefVectorMut)
            @test typ <: AbstractVector
            for elt ∈ (Float32, String)
                @test typ{elt} <: AbstractVector{elt}
            end
        end
    end
    @testset "construction with `undef`" begin
        for typ ∈ (MemoryVector, MemoryRefVectorImm, MemoryRefVectorMut)
            for elt ∈ (Float32, String)
                for n ∈ 0:4
                    @test (@inferred typ{elt}(undef, n)) isa typ{elt}
                end
            end
        end
    end
    @testset "`IndexStyle`" begin
        for typ ∈ (MemoryVector, MemoryRefVectorImm, MemoryRefVectorMut)
            @test (@inferred IndexStyle(typ)) === IndexLinear()
            for elt ∈ (Float32, String)
                @test (@inferred IndexStyle(typ{elt})) === IndexLinear()
            end
        end
    end
    @testset "`size`" begin
        for typ ∈ (MemoryVector, MemoryRefVectorImm, MemoryRefVectorMut)
            for elt ∈ (Float32, String)
                for n ∈ 0:4
                    @test (@inferred size(typ{elt}(undef, n))) === (n,)
                end
            end
        end
    end
    @testset "`checkbounds`" begin
        @testset "returns" begin
            function func(::Type{T}, n::Integer, i::Tuple) where {T}
                @test nothing === @inferred checkbounds(T(undef, n), i...)
                nothing
            end
            test_helper_some_types_some_lengths_some_indices(func, identity, Float32)
        end
        @testset "throws" begin
            function func(::Type{T}, n::Integer, i::Tuple) where {T}
                @test_throws LightBoundsError checkbounds(T(undef, n), i...)
                @test_throws ["LightBoundsError: ", "`collection[$(sprint_splatted(i))]`", "`typeof(collection) == $(typeof(T(undef, n)))`", "`axes(collection) == $(axes(T(undef, n)))`"] checkbounds(T(undef, n), i...)
                nothing
            end
            test_helper_some_types_some_lengths_some_indices(func, !, Float32)
        end
    end
    @testset "`getindex`" begin
        @testset "in-bounds access" begin
            function func(::Type{T}, n::Integer, i::Tuple) where {T}
                @test let v = T(undef, n)
                    (@inferred v[i...]) isa eltype(T)
                end
                nothing
            end
            test_helper_some_types_some_lengths_some_indices(func, identity, Float32)
        end
        @testset "out-of-bounds access" begin
            function func(::Type{T}, n::Integer, i::Tuple) where {T}
                @test_throws LightBoundsError T(undef, n)[i...]
                @test_throws ["LightBoundsError: ", "`collection[$(sprint_splatted(i))]`", "`typeof(collection) == $(typeof(T(undef, n)))`", "`axes(collection) == $(axes(T(undef, n)))`"] T(undef, n)[i...]
                nothing
            end
            test_helper_some_types_some_lengths_some_indices(func, !, Float32)
        end
    end
    @testset "`setindex!`" begin
        @testset "in-bounds access" begin
            function func(::Type{T}, n::Integer, i::Tuple) where {T}
                @test let v = T(undef, n)
                    (@inferred setindex!(v, 3, i...)) === v
                end
                nothing
            end
            test_helper_some_types_some_lengths_some_indices(func, identity, Float32)
        end
        @testset "out-of-bounds access" begin
            function func(::Type{T}, n::Integer, i::Tuple) where {T}
                @test_throws LightBoundsError T(undef, n)[i...] = 3
                @test_throws ["LightBoundsError: ", "`collection[$(sprint_splatted(i))]`", "`typeof(collection) == $(typeof(T(undef, n)))`", "`axes(collection) == $(axes(T(undef, n)))`"] T(undef, n)[i...] = 3
                nothing
            end
            test_helper_some_types_some_lengths_some_indices(func, !, Float32)
        end
    end
    @testset "`getindex`, `setindex!` consistency" begin
        for typ ∈ (MemoryVector, MemoryRefVectorImm, MemoryRefVectorMut)
            elt = Float32
            for n ∈ 1:4
                @test let v = typ{elt}(undef, n)
                    r = 1:n
                    for i ∈ r
                        v[i] = i * 10
                    end
                    all((i -> v[i] == i * 10), r)
                end
            end
        end
    end
    @testset "`isassigned`" begin
        @testset "is assigned" begin
            function func(::Type{T}, n::Integer, i::Tuple) where {T}
                @test let v = T(undef, n)
                    @inferred isassigned(v, i...)
                end
                nothing
            end
            test_helper_some_types_some_lengths_some_indices(func, identity, Float32)
        end
        @testset "is not assigned" begin
            function func(::Type{T}, n::Integer, i::Tuple) where {T}
                @test let v = T(undef, n)
                    !(@inferred isassigned(v, i...))
                end
                nothing
            end
            test_helper_some_types_some_lengths_some_indices(func, !, Float32)
        end
    end
    @testset "`iterate`" begin
        @testset "vector of length 0" begin
            for typ ∈ basic_types
                t = typ{Float32}
                @test let v = t(undef, 0)
                    nothing === iterate(v)
                end
            end
        end
        @testset "vector of length 1" begin
            for typ ∈ basic_types
                t = typ{Float32}
                @test let v = t(undef, 1)
                    iterate(v) isa Tuple{Float32, Any}
                end
                @test let v = t(undef, 1)
                    v[] = 7
                    eltype(t)(7) === iterate(v)[1]
                end
                @test let v = t(undef, 1)
                    is = iterate(v)[2]
                    nothing === iterate(v, is)
                end
            end
        end
    end
    @testset "`getindex`, `setindex!`, `iterate`, etc. consistency" begin
        for typ ∈ basic_types
            t = typ{BigInt}
            for n ∈ 0:7
                v = t(undef, n)
                for i ∈ eachindex(v)
                    v[i] = 100 * i + 7
                end
                i = 0
                for e ∈ v
                    i = i + 1
                    @test e === v[i]
                end
            end
        end
    end
    @testset "`parent`" begin
        for typ ∈ basic_types
            t = typ{Float32}
            for n ∈ 0:4
                @test let v = t(undef, n)
                    (@inferred parent(v)) isa Memory
                end
                @test let v = t(undef, n)
                    parent(v) === parent(v)
                end
                if !iszero(n)
                    @test let v = t(undef, n), u = t(undef, n)
                        parent(u) !== parent(v)
                    end
                end
            end
        end
    end
    @testset "`Base.dataids`" begin
        for typ ∈ basic_types
            t = typ{Float32}
            n = 3
            @test let v = t(undef, n)
                Base.dataids(parent(v)) === @inferred Base.dataids(v)
            end
        end
    end
end
