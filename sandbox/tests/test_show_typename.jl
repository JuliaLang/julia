# Tests for Base_show(io, ::Core.TypeName) and Base_show_type_name
include("../show_type.jl")

using Test

@testset "TypeName display" begin
    @test Base_repr(Base.typename(Array)) == "typename(Array)"
end

@testset "function type display" begin
    @test sprint(Base_show, typeof(sin)) == "typeof(sin)"
    @test sprint(Base_show, Function) == "Function"
end
