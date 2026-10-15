module BaseSyntax

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
