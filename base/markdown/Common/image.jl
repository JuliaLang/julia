# This file is a part of Julia. License is MIT: http://julialang.org/license

type Image
    url::UTF8String
    alt::UTF8String
end

@trigger '!' ->
function image(stream::IO, md::MD)
    withstream(stream) do
        startswith(stream, "![") || return
        alt = readuntil(stream, ']', match = '[')
        alt ≡ nothing && return
        skipwhitespace(stream)
        startswith(stream, '(') || return
        url = readuntil(stream, ')', match = '(')
        url ≡ nothing && return
        return Image(url, alt)
    end
end

function htmlinline(io::IO, md::Image)
    tag(io, :img, :src=>md.url, :alt=>md.alt)
end

function latexinline(io::IO, md::Image)
    wrapblock(io, "figure") do
        println(io, "\\centering")
        wrapinline(io, "includegraphics") do
            print(io, md.url)
        end
        println(io)
        wrapinline(io, "caption") do
            latexinline(io, md.alt)
        end
        println(io)
    end
end

plaininline(io::IO, md::Image) = plaininline(io, "![", md.alt, "](", md.url, ")")

function terminline(io::IO, md::Image)
    print(io, "(Image: $(md.alt))")
end
