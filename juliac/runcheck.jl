include("JuliacChecker.jl")
using .JuliacChecker
include("patches.jl")
for (mod, patch) in base_patches
    Core.eval(mod, patch)
end

report_dyncall_file("exe_examples/broadcasting.jl")
