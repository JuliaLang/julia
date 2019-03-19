# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Profiling support, main entry point is the [`@profile`](@ref) macro.
"""
module Profile

# init with default values
# Use a max size of 1M profile samples, and fire timer every 1ms, except on Windows
function __init__()
    timer_ms = 0.001
    @static if Sys.iswindows()
        timer_ms = 0.01
    end
    Time.init(1_000_000, timer_ms)
    Memory.init(20_000_000, 1_000_000, 0xffff)
end

include("stacktrace_tools.jl")
include("memory.jl")
include("time.jl")
include("compat.jl")

using .Memory, .Time

# Export our two measuring tools
import .Time: @profile
import .Memory: @memprofile
export @profile, @memprofile

end # module
