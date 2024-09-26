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

########################################
# Error: Macro with kw args
macro mmm(a; b=2)
end
#---------------------
LoweringError:
macro mmm(a; b=2)
#          └───┘ ── macros cannot accept keyword arguments
end

########################################
# Error: Bad macro name
macro mmm[](ex)
end
#---------------------
LoweringError:
macro mmm[](ex)
#     └───┘ ── invalid macro name
end

########################################
# Error: Macros not allowed in local scope
let
    macro foo(ex)
    end
end
#---------------------
LoweringError:
let
#   ┌────────────
    macro foo(ex)
    end
#─────┘ ── macro is only allowed in global scope
end

########################################
# Error: Macros not allowed in local scope
function f()
    macro foo()
    end
end
#---------------------
LoweringError:
function f()
#   ┌──────────
    macro foo()
    end
#─────┘ ── macro is only allowed in global scope
end

