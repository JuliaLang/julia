# This file is a part of Julia. License is MIT: https://julialang.org/license

module FakeTerminals

import REPL

mutable struct FakeTerminal <: REPL.Terminals.UnixTerminal
    in_stream::IO
    out_stream::IO
    err_stream::IO
    supports_color::Bool
    raw::Bool
    FakeTerminal(stdin, stdout, stderr, supports_color::Bool=true) =
        new(stdin, stdout, stderr, supports_color, false)
end

REPL.Terminals.supports_color(t::FakeTerminal) = t.supports_color
REPL.Terminals.raw!(t::FakeTerminal, raw::Bool) = t.raw = raw

end
