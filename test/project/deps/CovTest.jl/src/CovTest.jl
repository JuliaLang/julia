# This file is a part of Julia. License is MIT: https://julialang.org/license

module CovTest

function foo()
    x = 1
    y = 2
    z = x * y
    return z
end

function bar()
    x = 1
    y = 2
    z = x * y
    return z
end

if Base.generating_output()
    # precompile foo but not bar
    foo()
end

export foo, bar

end #module
