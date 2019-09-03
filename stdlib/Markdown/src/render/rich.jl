# This file is a part of Julia. License is MIT: https://julialang.org/license

function tohtml(io::IO, m::MIME"text/html", x)
    show(io, m, x)
end

function tohtml(io::IO, m::MIME"text/plain", x)
    htmlesc(io, sprint(show, m, x))
end

function tohtml(io::IO, m::MIME"image/png", img)
    print(io, """<img src="data:image/png;base64,""")
    print(io, stringmime(m, img))
    print(io, "\" />")
end

function tohtml(io::IO, m::MIME"image/svg+xml", img)
    show(io, m, img)
end

# AbstractDisplay infrastructure

function bestmime(val)
    for mime in ("text/html", "image/svg+xml", "image/png", "text/plain")
        showable(mime, val) && return MIME(Symbol(mime))
    end
    error("Cannot render $val to Markdown.")
end

tohtml(io::IO, x) = tohtml(io, bestmime(x), x)
