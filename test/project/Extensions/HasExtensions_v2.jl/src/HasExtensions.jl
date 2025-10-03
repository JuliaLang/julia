module HasExtensions

struct HasExtensionsStruct end

foo(::HasExtensionsStruct) = 1

ext_loaded = false
ext_folder_loaded = false

end # module
