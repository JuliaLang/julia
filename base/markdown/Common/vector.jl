# This file is a part of Julia. License is MIT: http://julialang.org/license

function htmlinline(io::IO, content::Vector)
    for x in content
        htmlinline(io, x)
    end
end

function html(io::IO, content::Vector)
    for md in content
        html(io, md)
        println(io)
    end
end

function latex(io::IO, content::Vector)
    for c in content
        latex(io, c)
    end
end

function latexinline(io::IO, md::Vector)
    for c in md
        latexinline(io, c)
    end
end

function plain(io::IO, content::Vector)
    isempty(content) && return
    for md in content[1:end-1]
        plain(io, md)
        println(io)
    end
    plain(io, content[end])
end

plaininline(io::IO, md::Vector) = !isempty(md) && plaininline(io, md...)

function term(io::IO, content::Vector, cols)
    isempty(content) && return
    for md in content[1:end-1]
        term(io, md, cols)
        println(io)
    end
    term(io, content[end], cols)
end

function terminline(io::IO, content::Vector)
    for md in content
        terminline(io, md)
    end
end
