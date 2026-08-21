# This file is a part of Julia. License is MIT: https://julialang.org/license

# Coverage for a macro body defined in a different source file.

module CoverageMacros

export @twice

macro twice(x)
    quote
        v = $(esc(x))
        v + v
    end
end

end
