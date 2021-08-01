# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
This singleton struct provides the help "command" during interactive mode which
redirects the user to the "?" help REPL mode.
"""
struct Help
end

const help = Help()
Base.show(io::IO, m::MIME"text/plain", h::Help) = show(io, m, h())
Base.show(io::IO, h::Help) = show(io, h())

(::Help)() = @doc(help)
