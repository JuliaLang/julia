# This file is a part of Julia. License is MIT: https://julialang.org/license

# source path in tasks
path = Base.source_path()::String # this variable is leaked to the source script
@test endswith(path, joinpath("test","test_sourcepath.jl"))
@test let ct = current_task()
    Base._yieldto(@task Base._yieldto(ct, Base.source_path()))
end == path
@test let ct = current_task()
    Base._yieldto(@task schedule(ct, Base.source_path()))
end == path
@test let ct = current_task(), t = @task Base.source_path()
    schedule(ct)
    Base._yieldto(t)
    fetch(t)
end == path
@test isabspath(@__FILE__)
