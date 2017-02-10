macro Dict(pairs...)
    esc(Expr(:call, :Dict, pairs...))
end
macro AnyDict(pairs...)
    esc(Expr(:call, :(Base.AnyDict), pairs...))
end

if VERSION >= v"0.4.0-dev+3184"
    include("ngenerate.jl")
    using .CompatCartesian
    export @ngenerate, @nsplat
end
