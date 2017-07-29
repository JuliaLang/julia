module Pkg3

include("Types.jl")
include("Operations.jl")
include("Loading.jl")

import .Types: VersionRange, @vr_str
import .Loading: Loader, LoadInstalled
import .Operations:
    add, find_config, find_manifest, user_depot, depots, registries

export @vr_str

end # module
