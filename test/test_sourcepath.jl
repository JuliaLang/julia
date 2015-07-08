# This file is a part of Julia. License is MIT: http://julialang.org/license

# source path in tasks
path = Base.source_path() # this variable is leaked to the source script
@test length(path) == 2
@test path[1] == expect_load_from # this variable is set in the source script
@test endswith(path[2], joinpath("test","test_sourcepath.jl"))
@test yieldto(@task Base.source_path()) == path
if expect_load_from != myid()
    @test startswith(@__FILE__, ":node $expect_load_from:\0")
else
    @test isabspath(@__FILE__)
end
