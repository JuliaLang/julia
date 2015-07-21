# This file is a part of Julia. License is MIT: http://julialang.org/license

type Link
    text
    url::UTF8String
end

@trigger '[' ->
function link(stream::IO, md::MD)
    withstream(stream) do
        startswith(stream, '[') || return
        text = readuntil(stream, ']', match = '[')
        text ≡ nothing && return
        skipwhitespace(stream)
        startswith(stream, '(') || return
        url = readuntil(stream, ')', match = '(')
        url ≡ nothing && return
        return Link(parseinline(text, md), url)
    end
end

function htmlinline(io::IO, link::Link)
    withtag(io, :a, :href=>link.url) do
        htmlinline(io, link.text)
    end
end

function latexinline(io::IO, md::Link)
    wrapinline(io, "href") do
        print(io, md.url)
    end
    print(io, "{")
    latexinline(io, md.text)
    print(io, "}")
end

function terminline(io::IO, md::Link)
    terminline(io, md.text)
end

function plaininline(io::IO, md::Link)
    print(io, "[$(md.text)]($(md.url))")
end
