# This file is a part of Julia. License is MIT: https://julialang.org/license

module REPLTests
    include("repl.jl")
end
module REPLCompletionsTest
    include("replcompletions.jl")
end
module LineEditTest
    include("lineedit.jl")
end
module TerminalMenusTest
    include("TerminalMenus/runtests.jl")
end
