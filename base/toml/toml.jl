# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
`Base.TOML` is an undocumented internal part of Julia's TOML implementation.
Users should call the documented interface in the TOML.jl standard library
instead (by `import TOML` or `using TOML`).
"""
module TOML

include("parser.jl")

# We put the printing functionality in a separate module since it
# defines a function `print` and we don't want that to collide with normal
# usage of `(Base.)print` in other files
module Printer
    include("printer.jl")
end

end
