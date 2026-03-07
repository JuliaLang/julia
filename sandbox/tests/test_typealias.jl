# Tests for Base_make_typealias, Base_make_typealiases, Base_show_typealias,
# Base_show_unionaliases, and type alias display
include("../show_type.jl")

using Test

module M37012_sandbox
export AValue, B2, SimpleU
struct AnInteger{S<:Integer} end
struct AStruct{N} end
const AValue{S} = Union{AStruct{S}, AnInteger{S}}
struct BStruct{T,S} end
const B2{S,T} = BStruct{T,S}
const SimpleU = Union{AnInteger, AStruct, BStruct}
end

@testset "make_typealias" begin
    @test Base_make_typealias(M37012_sandbox.AStruct{1}) === nothing
    @test isempty(Base_make_typealiases(M37012_sandbox.AStruct{1})[1])
end

@testset "type alias display" begin
    @test sprint(Base_show, M37012_sandbox.AStruct{1}) == "$(nameof(@__MODULE__)).M37012_sandbox.AStruct{1}"
    @test sprint(Base_show, Union{Nothing, Number, Vector}) == "Union{Nothing, Number, Vector}"
    @test sprint(Base_show, Union{Nothing, Number, Vector{<:Integer}}) == "Union{Nothing, Number, Vector{<:Integer}}"
    @test sprint(Base_show, Union{Nothing, AbstractVecOrMat}) == "Union{Nothing, AbstractVecOrMat}"
    @test sprint(Base_show, Union{Nothing, AbstractVecOrMat{<:Integer}}) == "Union{Nothing, AbstractVecOrMat{<:Integer}}"
end

@testset "BStruct alias display" begin
    modprefix = "$(nameof(@__MODULE__)).M37012_sandbox"
    @test sprint(Base_show, M37012_sandbox.BStruct{T, T} where T) == "$(modprefix).B2{T, T} where T"
    @test sprint(Base_show, M37012_sandbox.BStruct{T, S} where {T<:Unsigned, S<:Signed}) == "$(modprefix).B2{S, T} where {T<:Unsigned, S<:Signed}"
    @test sprint(Base_show, M37012_sandbox.BStruct{T, S} where {T<:Signed, S<:T}) == "$(modprefix).B2{S, T} where {T<:Signed, S<:T}"
end

@testset "SimpleU alias display" begin
    modprefix = "$(nameof(@__MODULE__)).M37012_sandbox"
    @test sprint(Base_show, Union{M37012_sandbox.SimpleU, Nothing}) == "Union{Nothing, $(modprefix).SimpleU}"
    @test sprint(Base_show, Union{M37012_sandbox.SimpleU, Nothing, T} where T) == "Union{Nothing, $(modprefix).SimpleU, T} where T"
end

@testset "Union display without aliases" begin
    @test sprint(Base_show, Union{AbstractVector{T}, T} where T) == "Union{AbstractVector{T}, T} where T"
    @test sprint(Base_show, Union{AbstractVector, T} where T) == "Union{AbstractVector, T} where T"
    @test sprint(Base_show, Union{Array, Memory}) == "Union{Array, Memory}"
end

@testset "StridedArray alias display" begin
    @test startswith(sprint(Base_show, StridedArray), "StridedArray")
    @test startswith(sprint(Base_show, StridedVecOrMat), "StridedVecOrMat")
    @test startswith(sprint(Base_show, StridedVector), "Strided")
    @test startswith(sprint(Base_show, StridedMatrix), "Strided")
end
