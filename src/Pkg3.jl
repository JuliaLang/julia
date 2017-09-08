include("monkey_patch.jl")

module Pkg3

@eval Base module Loading; DEPOTS = []; end

include("Types.jl")
include("Operations.jl")
include("Loading.jl")
include("REPLMode.jl")
REPLMode.repl_init(Base.active_repl)

import .Types: VersionRange, VersionSpec, EnvCache
import .Loading: Loader, LoadInstalled
import .Operations: add, rm, up

end # module
