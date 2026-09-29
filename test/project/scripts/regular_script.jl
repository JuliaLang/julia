# a regular file without inline project metadata
println("Active project: ", repr(Base.active_project(false)))
println("Hello from regular script")
