# This file is a part of Julia. License is MIT: https://julialang.org/license

module Prelinked

# The launcher calls this through `jl_call0`, so that nothing in the program
# needs the parser.
function julia_main()
    println("prelinked ", 6 * 7)
    return Int32(0)
end

end
