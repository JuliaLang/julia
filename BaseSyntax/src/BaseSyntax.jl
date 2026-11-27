module BaseSyntax

# `syntax.jl` and JuliaLowering refer to these editions through this module. `Core` defines
# them on Julia 1.14 and later.
if isdefined(Core, :FLISP_EDITION)
    using Core: FLISP_EDITION, EXPERIMENTAL_EDITION
elseif VERSION >= VersionNumber(1, 12)
    const FLISP_EDITION = (1, 14)
    const EXPERIMENTAL_EDITION = (1, 25)
end

# `Base` includes this file to define `Base.BaseSyntax`. Loaded as a package, this module
# re-exports `Base.BaseSyntax` where it exists, so that every copy of JuliaSyntax and
# JuliaLowering shares `Base`'s definitions, and defines them itself otherwise.
if parentmodule(@__MODULE__) === Base
    # During bootstrap, all includes are relative to `base/`
    include(Base.strcat(Base.DATAROOT, "julia/BaseSyntax/src/syntax.jl"))
elseif isdefined(Base, :BaseSyntax)
    for name in names(Base.BaseSyntax; all=true)
        if name === :BaseSyntax || name === :eval || name === :include || startswith(String(name), '#')
            continue
        end
        Core.eval(@__MODULE__, :(using Base.BaseSyntax: $name))
    end
elseif VERSION >= VersionNumber(1, 12)
    include("syntax.jl")
end

end
