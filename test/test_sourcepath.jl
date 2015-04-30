# This file is a part of Julia. License is MIT: https://julialang.org/license

# source path in tasks
path = Base.source_path()
@test endswith(path, joinpath("test","test_sourcepath.jl"))
@test yieldto(@task Base.source_path()) == path
