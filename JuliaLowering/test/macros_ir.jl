########################################
# Simple macro
macro add_one(ex)
    quote
        $ex + 1
    end
end
#---------------------
1   (method TestMod.@add_one)
2   TestMod.@add_one
3   (call core.Typeof %₂)
4   (call core.svec %₃ JuliaLowering.MacroContext core.Any)
5   (call core.svec)
6   SourceLocation::1:7
7   (call core.svec %₄ %₅ %₆)
8   --- method core.nothing %₇
    slots: [slot₁/#self#(!read) slot₂/__context__(!read) slot₃/ex]
    1   (call core.tuple slot₃/ex)
    2   (call JuliaLowering.interpolate_ast (inert (block (call-i ($ ex) + 1))) %₁)
    3   (return %₂)
9   TestMod.@add_one
10  (return %₉)

########################################
# Macro using `__context__`
macro foo(ex)
    ctx = __context__
end
#---------------------
1   (method TestMod.@foo)
2   TestMod.@foo
3   (call core.Typeof %₂)
4   (call core.svec %₃ JuliaLowering.MacroContext core.Any)
5   (call core.svec)
6   SourceLocation::1:7
7   (call core.svec %₄ %₅ %₆)
8   --- method core.nothing %₇
    slots: [slot₁/#self#(!read) slot₂/__context__ slot₃/ex(!read) slot₄/ctx(!read)]
    1   slot₂/__context__
    2   (= slot₄/ctx %₁)
    3   (return %₁)
9   TestMod.@foo
10  (return %₉)

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

