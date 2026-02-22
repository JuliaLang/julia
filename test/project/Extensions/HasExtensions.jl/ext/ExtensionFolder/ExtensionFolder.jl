module ExtensionFolder

using ExtDep, ExtDep2, HasExtensions

function __init__()
    HasExtensions.ext_folder_loaded = true
end

end
