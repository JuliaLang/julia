module JuliaSyntaxCore

# A tiny module to hold initialization code for JuliaSyntax.jl integration with
# the runtime.

using JuliaSyntax

import Base: JLOptions

function __init__()
    # HACK! Fool the runtime into allowing us to set Core._parse, even during
    # incremental compilation. (Ideally we'd just arrange for Core._parse to be
    # set to the JuliaSyntax parser. But how do we signal that to the dumping
    # code outside of the initial creation of Core?)
    i = findfirst(==(:incremental), fieldnames(JLOptions))
    ptr = convert(Ptr{fieldtype(JLOptions, i)},
                  cglobal(:jl_options, JLOptions) + fieldoffset(JLOptions, i))
    incremental = unsafe_load(ptr)
    incremental == 0 || unsafe_store!(ptr, 0)

    JuliaSyntax.enable_in_core!()

    incremental == 0 || unsafe_store!(ptr, incremental)
end

end
