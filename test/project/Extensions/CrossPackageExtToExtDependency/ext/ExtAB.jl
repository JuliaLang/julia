module ExtAB

using CrossPackageExtToExtDependency
using SomePackage
using CyclicExtensions

const ExtA = Base.get_extension(CyclicExtensions, :ExtA)
if !(ExtA isa Module)
    error("expected extension to load")
end

end
