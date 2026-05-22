# This file is a part of Julia. License is MIT: https://julialang.org/license

using InteractiveUtils: @activate

if Base.identify_package("Compiler") !== nothing && !isdefined(Main, :__custom_compiler_active)
    Base.eval(Main, :(__custom_compiler_active=true))
    @activate Compiler
end

if !@isdefined(Compiler)
    if Base.REFLECTION_COMPILER[] === nothing
        using Base.Compiler: Compiler
    else
        const Compiler = Base.REFLECTION_COMPILER[]
    end
end
