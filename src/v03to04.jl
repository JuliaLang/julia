module v03to04

export @Dict

if VERSION < v"0.4.0-dev+980"
    macro Dict(pairs...)
        esc(Expr(:dict, pairs...))
    end
else
    macro Dict(pairs...)
        esc(Expr(:call, :Dict, pairs...))
    end
end

end # module
