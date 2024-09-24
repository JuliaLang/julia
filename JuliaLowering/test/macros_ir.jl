########################################
# Simple macro
macro add_one(ex)
    quote
        $ex + 1
    end
end
#---------------------
1   (method :@add_one)
2   (call core.Typeof %₁)
3   (call core.svec %₂ JuliaLowering.MacroContext core.Any)
4   (call core.svec)
5   (call core.svec %₃ %₄ :($(QuoteNode(:(#= line 1 =#)))))
6   --- method :@add_one %₅
    1   (call core.tuple slot₃/ex)
    2   (call JuliaLowering.interpolate_ast (inert (block (call-i ($ ex) + 1))) %₁)
    3   (return %₂)
7   (return %₁)

########################################
# Macro using `__context__`
macro foo(ex)
    ctx = __context__
end
#---------------------
1   (method :@foo)
2   (call core.Typeof %₁)
3   (call core.svec %₂ JuliaLowering.MacroContext core.Any)
4   (call core.svec)
5   (call core.svec %₃ %₄ :($(QuoteNode(:(#= line 1 =#)))))
6   --- method :@foo %₅
    1   slot₂/__context__
    2   (= slot₄/ctx %₁)
    3   (return %₁)
7   (return %₁)

