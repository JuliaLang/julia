# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
This singleton struct provides the help "command" during interactive mode which
redirects the user to the "?" help REPL mode.
"""
struct Help
end

const help = Help()
Base.show(io::IO, m::MIME"text/plain", ::Help) = show(io, m, Base.Docs.parsedoc(Base.Docs.keywords[:help]))
Base.show(io::IO, ::Help) = show(io, Base.Docs.parsedoc(Base.Docs.keywords[:help]))

(::Help)() = Base.Docs.parsedoc(Base.Docs.keywords[:help])

