# This file is a part of Julia. License is MIT: https://julialang.org/license

if !@isdefined(Compiler)
    if Base.REFLECTION_COMPILER[] === nothing
        using Base.Compiler: Compiler
    else
        const Compiler = Base.REFLECTION_COMPILER[]
    end
end
