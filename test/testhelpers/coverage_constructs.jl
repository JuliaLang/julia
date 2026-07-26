# This file is a part of Julia. License is MIT: https://julialang.org/license

# Constructs that have been reported as missing coverage; see test/cmdlineargs.jl
# for the expected line numbers.

module CoverageConstructs

function with_local(x)
    local y
    y = x + 1
    return y
end

function with_let(x)
    let z
        z = x
        return z
    end
end

macro adds_lines()
    quote
        a = 1
        a + 1
    end
end

function uses_macro()
    @adds_lines
end

function implicit_return(x)
    if x > 0
        x + 1
    else
        x - 1
    end
end

Base.@assume_effects :foldable function foldable(x)
    y = x + 1
    return y * 2
end

function folds(n)
    s = 0
    for _ in 1:n
        s += foldable(3)
    end
    return s
end

function only_active_branch(x)
    @static if true
        return x + 1
    else
        return x - 1
    end
end

end # module

const C = CoverageConstructs
ok = C.with_local(1) == 2 &&
     C.with_let(1) == 1 &&
     C.uses_macro() == 2 &&
     C.implicit_return(1) == 2 &&
     C.implicit_return(-1) == -2 &&
     C.folds(3) == 24 &&
     C.only_active_branch(1) == 2
exit(ok ? 0 : 1)
