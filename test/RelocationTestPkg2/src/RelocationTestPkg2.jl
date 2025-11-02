module RelocationTestPkg2

include_dependency("foo.txt", track_content=false)
include_dependency("foodir", track_content=false)
greet() = print("Hello World!")

end # module RelocationTestPkg2
