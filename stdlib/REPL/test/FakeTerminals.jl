# This file is a part of Julia. License is MIT: https://julialang.org/license

module FakeTerminals

import REPL

mutable struct FakeTerminal <: REPL.Terminals.UnixTerminal
    in_stream::Base.IO
    out_stream::Base.IO
    err_stream::Base.IO
    hascolor::Bool
    raw::Bool
    FakeTerminal(stdin,stdout,stderr,hascolor=true) =
        new(stdin,stdout,stderr,hascolor,false)
end

REPL.Terminals.hascolor(t::FakeTerminal) = t.hascolor
REPL.Terminals.raw!(t::FakeTerminal, raw::Bool) = t.raw = raw
REPL.Terminals.size(t::FakeTerminal) = (24, 80)

end
