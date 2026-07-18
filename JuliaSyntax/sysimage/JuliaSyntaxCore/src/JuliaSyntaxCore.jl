module JuliaSyntaxCore

# A tiny module to hold initialization code for JuliaSyntax.jl integration with
# the runtime.

using JuliaSyntax

function __init__()
    JuliaSyntax.enable_in_core!()
end

end
