module FancyDiagnostics
    include("LineNumbers.jl")
    include("display.jl")
    include("hooks.jl")
    
    const REPLDiagnostic = BaseHooks.REPLDiagnostic
end # module
