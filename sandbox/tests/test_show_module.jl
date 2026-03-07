# Tests for Base_show(io, ::Module) and Base_print_fullname
include("../show_type.jl")

using Test

@testset "Module display" begin
    @test sprint(Base_show, Main) == "Main"
    @test sprint(Base_show, Base) == "Base"
    @test sprint(Base_show, Core) == "Core"
end
