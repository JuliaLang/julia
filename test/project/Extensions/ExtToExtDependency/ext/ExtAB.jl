module ExtAB

using ExtToExtDependency
using SomePackage
using SomeOtherPackage

const ExtA = Base.get_extension(ExtToExtDependency, :ExtA)
if !(ExtA isa Module)
    error("expected extension to load")
end

end
