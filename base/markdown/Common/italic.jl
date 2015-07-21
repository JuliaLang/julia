# This file is a part of Julia. License is MIT: http://julialang.org/license

type Italic
    text
end

@trigger '*' ->
function asterisk_italic(stream::IO, md::MD)
    result = parse_inline_wrapper(stream, "*")
    return result == nothing ? nothing : Italic(parseinline(result, md))
end

function htmlinline(io::IO, md::Italic)
    withtag(io, :em) do
        htmlinline(io, md.text)
    end
end

function latexinline(io::IO, md::Italic)
    wrapinline(io, "emph") do
        latexinline(io, md.text)
    end
end

plaininline(io::IO, md::Italic) = plaininline(io, "*", md.text, "*")

function terminline(io::IO, md::Italic)
    with_output_format(:underline, terminline, io, md.text)
end
