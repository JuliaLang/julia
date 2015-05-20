# This file is a part of Julia. License is MIT: http://julialang.org/license

type Bold
    text
end

@trigger '*' ->
function asterisk_bold(stream::IO, md::MD)
    result = parse_inline_wrapper(stream, "**")
    return result == nothing ? nothing : Bold(parseinline(result, md))
end

function htmlinline(io::IO, md::Bold)
    withtag(io, :strong) do
        htmlinline(io, md.text)
    end
end

function latexinline(io::IO, md::Bold)
    wrapinline(io, "textbf") do
        latexinline(io, md.text)
    end
end

plaininline(io::IO, md::Bold) = plaininline(io, "**", md.text, "**")

function terminline(io::IO, md::Bold)
    with_output_format(:bold, terminline, io, md.text)
end
