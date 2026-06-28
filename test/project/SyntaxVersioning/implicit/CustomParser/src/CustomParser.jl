module CustomParser

function rewrite_marker!(ex)
    ex === :CUSTOM_PARSER_MARKER && return true
    if ex isa Expr
        for i in eachindex(ex.args)
            ex.args[i] = rewrite_marker!(ex.args[i])
        end
    end
    return ex
end

function core_parser_hook(code, filename::String, lineno::Int, offset::Int, options::Symbol)
    ret = Base.JuliaSyntax.core_parser_hook(code, filename, lineno, offset, options;
        syntax_version=v"1.14", module_parser=GlobalRef(@__MODULE__, :core_parser_hook))
    rewrite_marker!(ret[1])
    return ret
end

end
