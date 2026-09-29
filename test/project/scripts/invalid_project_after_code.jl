x = 1
# /// project
# [deps]
# ///
println("project block after code is not a script env: ", repr(Base.active_project(false)))
