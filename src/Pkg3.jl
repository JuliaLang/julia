module Pkg3

include("Types.jl")
include("Operations.jl")
include("Loading.jl")
include("REPLMode.jl")
REPLMode.repl_init(Base.active_repl)

import .Types: VersionRange, VersionSpec
import .Loading: Loader, LoadInstalled
import .Operations:
    add, rm,
    find_config, find_manifest, user_depot, depots, registries,
    UpgradeLevel

end # module
