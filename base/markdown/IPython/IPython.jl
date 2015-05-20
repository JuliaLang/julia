# This file is a part of Julia. License is MIT: http://julialang.org/license

type LaTeX
    formula::UTF8String
end

# @breaking true ->
# function textex(stream::IO, block::MD)
#     withstream(stream) do
#         startswith(stream, "\$\$", padding = true) || return false
#         tex = readuntil(stream, "\$\$", newlines = true)
#         tex ≡ nothing && return false
#         push!(block, LaTeX(tex))
#         return true
#     end
# end

@trigger '$' ->
function tex(stream::IO, md::MD)
    result = parse_inline_wrapper(stream, "\$", rep = true)
    return result ≡ nothing ? nothing : LaTeX(result)
end

function blocktex(stream::IO, md::MD)
    withstream(stream) do
        ex = tex(stream, md)
        ex ≡ nothing && return false
        push!(md, ex)
        return true
    end
end

# TODO html and htmlinline

latex(io::IO, tex::LaTeX) = print(io, "\\[", tex.formula, "\\]")
latexinline(io::IO, tex::LaTeX) = print(io, '$', tex.formula, '$')

plain(io::IO, text::LaTeX) = print(io, '$', tex.formula, '$')
plaininline(io::IO, text::LaTeX) = print(io, '$', tex.formula, '$')

term(io::IO, tex::LaTeX, cols) = println_with_format(:magenta, io, tex.formula)
terminline(io::IO, tex::LaTeX) = print_with_format(:magenta, io, tex.formula)
