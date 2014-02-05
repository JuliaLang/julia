# source path in tasks
path = Base.source_path()
@test endswith(path, joinpath("test","test_sourcepath.jl"))
#@test yieldto(@task begin
#        t = current_task().last
#        t.result = Base.source_path()
#        Base.enq_work(current_task().last)
#    end) == path
