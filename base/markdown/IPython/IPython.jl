# This file is a part of Julia. License is MIT: http://julialang.org/license

type LaTeX
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

writemime(io::IO, ::MIME"text/plain", tex::LaTeX) =
    print(io, '$', tex.formula, '$')

latex(io::IO, tex::LaTeX) =
    println(io, "\$\$", tex.formula, "\$\$")

latexinline(io::IO, tex::LaTeX) =
    print(io, '$', tex.formula, '$')

term(io::IO, tex::LaTeX, cols) = println_with_format(:magenta, io, tex.formula)
terminline(io::IO, tex::LaTeX) = print_with_format(:magenta, io, tex.formula)
