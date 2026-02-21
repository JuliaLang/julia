module HasDepWithExtensions

using HasExtensions: HasExtensions, HasExtensionsStruct
using ExtDep: ExtDepStruct
# Loading ExtDep makes the extension "Extension" load

const m = Base.get_extension(HasExtensions, :Extension)
m isa Module || error("extension not loaded during precompilation")

function do_something()
    HasExtensions.foo(HasExtensionsStruct()) == 1 || error()
    HasExtensions.foo(ExtDepStruct()) == 2 || error()
    return true
end

function __init__()
    m = Base.get_extension(HasExtensions, :Extension)
    m isa Module || error("extension not loaded during __init__")
end

end # module
