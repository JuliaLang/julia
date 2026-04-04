module ExtensionDep

using HasExtensions, ExtDep3

function __init__()
    HasExtensions.ext_dep_loaded = true
end

end
