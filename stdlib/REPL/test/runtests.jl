# This file is a part of Julia. License is MIT: https://julialang.org/license

# Make a copy of the original environment
original_env = copy(ENV)

module PrecompilationTests
    include("precompilation.jl")
end

module REPLTests
    include("repl.jl")
end
module REPLCompletionsTest
    include("replcompletions.jl")
end
module LineEditTest
    include("lineedit.jl")
end
module DocviewTest
    include("docview.jl")
end
module TerminalMenusTest
    include("TerminalMenus/runtests.jl")
end
module BadHistoryStartupTest
    include("bad_history_startup.jl")
end

# Restore the original environment
for k in keys(ENV)
    if !haskey(original_env, k)
        delete!(ENV, k)
    end
end
for (k, v) in pairs(original_env)
    ENV[k] = v
end
