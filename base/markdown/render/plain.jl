# This file is a part of Julia. License is MIT: http://julialang.org/license

plain(x) = sprint(plain, x)

plain(io::IO, md::MD) = plain(io, md.content)

plain(io::IO, md) = writemime(io, "text/plain", md)

plaininline(x) = sprint(plaininline, x)

function plaininline(io::IO, md...)
    for el in md
        plaininline(io, el)
    end
end

plaininline(io::IO, s::String) = print(io, s)

plaininline(io::IO, x) = writemime(io, MIME"text/plain"(), x)

# writemime

Base.writemime(io::IO, ::MIME"text/plain", md::MD) = plain(io, md)
