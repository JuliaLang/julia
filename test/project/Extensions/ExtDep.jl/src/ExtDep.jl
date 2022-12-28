module ExtDep

# loading this package makes the check for loading extensions trigger
# which tests #47921
using SomePackage

struct ExtDepStruct end

end # module ExtDep
