module GluePkgFolder

using GlueDep, GlueDep2, HasGluePkgs

function __init__()
    HasGluePkgs.glue_folder_loaded = true
end

end
