module Extension

using HasExtensions, ExtDep

HasExtensions.foo(::ExtDep.ExtDepStruct) = 2

function __init__()
    HasExtensions.ext_loaded = true
end

const extvar = 1

end
