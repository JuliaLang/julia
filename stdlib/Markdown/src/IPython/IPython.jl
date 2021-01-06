# This file is a part of Julia. License is MIT: https://julialang.org/license

mutable struct LaTeX
    formula::String
end

@trigger '$' ->
function tex(stream::IO, md::MD)
    result = parse_inline_wrapper(stream, "\$", rep = true)
    return result === nothing ? nothing : LaTeX(result)
end

function blocktex(stream::IO, md::MD)
    withstream(stream) do
        ex = tex(stream, md)
        if ex ≡ nothing
            return false
        else
            push!(md, ex)
            return true
        end
    end
end

show(io::IO, tex::LaTeX) =
    print(io, '$', tex.formula, '$')

latex(io::IO, tex::LaTeX) =
    println(io, "\$\$", tex.formula, "\$\$")

latexinline(io::IO, tex::LaTeX) =
    print(io, '$', tex.formula, '$')
