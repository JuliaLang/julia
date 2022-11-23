module GluePkg

using HasGluePkgs, GlueDep

HasGluePkgs.foo(::GlueDep.GlueDepStruct) = 2

function __init__()
    HasGluePkgs.glue_loaded = true
end

const gluevar = 1

end
