# This file is a part of Julia. License is MIT: http://julialang.org/license

include("formatting.jl")

const margin = 2
cols() = Base.tty_size()[2]

term(io::IO, md::MD, columns = cols()) = term(io, md.content, columns)

term(io::IO, x, _) = writemime(io, MIME"text/plain"(), x)

# Inline Content

terminline(md) = sprint(terminline, md)

function terminline(io::IO, md::String)
    print(io, md)
end

terminline(io::IO, x) = writemime(io, MIME"text/plain"(), x)

# Show in terminal

Base.display(d::Base.REPL.REPLDisplay, md::MD) = term(Base.REPL.outstream(d.repl), md)
