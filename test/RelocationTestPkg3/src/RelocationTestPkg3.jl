module RelocationTestPkg3

include_dependency("bar.txt", track_content=true)
include_dependency("bardir", track_content=true)
greet() = print("Hello World!")

end # module RelocationTestPkg3
