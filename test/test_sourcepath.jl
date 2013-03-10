# source path in tasks
path = Base.source_path()
@test ends_with(path, joinpath("test","test_sourcepath.jl"))
@test yieldto(@task Base.source_path()) == path
