module Pkg3

user_depot() = abspath(homedir(), ".julia")
depots() = Base.Loading.DEPOTS

include("Types.jl")
include("Operations.jl")
include("Loading.jl")
include("REPLMode.jl")
REPLMode.repl_init(Base.active_repl)

import .Types: VersionRange, VersionSpec, EnvCache
import .Loading: Loader, LoadInstalled
import .Operations: add, rm, up

end # module
