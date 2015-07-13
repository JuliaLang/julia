# This file is a part of Julia. License is MIT: http://julialang.org/license

# source path in tasks
path = Base.source_path()::ByteString # this variable is leaked to the source script
@test endswith(path, joinpath("test","test_sourcepath.jl"))
@test yieldto(@task Base.source_path()) == path
@test isabspath(@__FILE__)
