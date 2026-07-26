# This file is a part of Julia. License is MIT: https://julialang.org/license

# A macro defined in a different file from where it is used; see
# testhelpers/coverage_macrouse.jl. The expansion has no module recorded in its
# debuginfo, so it must not be mistaken for a sysimage source.

module CoverageMacros

export @twice

macro twice(x)
    quote
        v = $(esc(x))
        v + v
    end
end

end
