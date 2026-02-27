module RelocationTestPkg5

const dir  = normpath(joinpath(@__DIR__, "..", "..", "TestPkg"))
const file = joinpath(dir, "Project.toml")
const dir_tracked      = Base.RelocPath(dir;  track_content=true)
const dir_not_tracked  = Base.RelocPath(dir;  track_content=false)
const file_tracked     = Base.RelocPath(file; track_content=true)
const file_not_tracked = Base.RelocPath(file; track_content=false)

greet() = print("Hello World!")

end # module RelocationTestPkg5
