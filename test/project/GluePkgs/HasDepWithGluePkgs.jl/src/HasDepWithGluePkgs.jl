module HasDepWithGluePkgs

using HasGluePkgs: HasGluePkgs, HasGluePkgsStruct
using GlueDep: GlueDepStruct
# Loading GlueDep makes the glue module "GluePkg" load

function do_something()
    HasGluePkgs.foo(HasGluePkgsStruct()) == 1 || error()
    HasGluePkgs.foo(GlueDepStruct()) == 2 || error()
    return true
end

end # module
