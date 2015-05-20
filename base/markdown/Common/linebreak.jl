# This file is a part of Julia. License is MIT: http://julialang.org/license

type LineBreak end

@trigger '\\' ->
function linebreak(stream::IO, md::MD)
    if startswith(stream, "\\\n")
        return LineBreak()
    end
end

function htmlinline(io::IO, br::LineBreak)
    tag(io, :br)
end

latexinline(io::IO, br::LineBreak) = println(io, "\n\\newline")

plaininline(io::IO, br::LineBreak) = println(io)

# TODO should this be terminline?
function term(io::IO, br::LineBreak, columns)
   println(io)
end
