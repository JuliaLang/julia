module HasDepWithExtensions

using HasExtensions: HasExtensions, HasExtensionsStruct
using ExtDep: ExtDepStruct
# Loading ExtDep makes the extension "Extension" load

function do_something()
    HasExtensions.foo(HasExtensionsStruct()) == 1 || error()
    HasExtensions.foo(ExtDepStruct()) == 2 || error()
    return true
end

end # module
