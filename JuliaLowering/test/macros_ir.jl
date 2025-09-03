module MacroMethods
    macro some_macro()
        quote
            some_global
        end
    end

    module ExtraMacroMethods
        using ..MacroMethods
        macro MacroMethods.some_macro(ex)
            quote
                some_global
            end
        end
    end
end

macro strmac_str(ex, suff=nothing)
    s = "$(ex[1].value) from strmac"
    if !isnothing(suff)
        s = "$s with suffix $(suff.value)"
    end
    s
end

macro cmdmac_cmd(ex, suff=nothing)
    s = "$(ex[1].value) from cmdmac"
    if !isnothing(suff)
        s = "$s with suffix $(suff.value)"
    end
    s
end

#*******************************************************************************
########################################
# Simple macro
macro add_one(ex)
    quote
        $ex + 1
    end
end
#---------------------
1   (method TestMod.@add_one)
2   latestworld
3   TestMod.@add_one
4   (call core.Typeof %₃)
5   (call core.svec %₄ JuliaLowering.MacroContext core.Any)
6   (call core.svec)
7   SourceLocation::1:7
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(!read) slot₂/__context__(!read) slot₃/ex]
    1   (call core.tuple slot₃/ex)
    2   (call JuliaLowering.interpolate_ast SyntaxTree (inert (block (call-i ($ ex) + 1))) %₁)
    3   (return %₂)
10  latestworld
11  TestMod.@add_one
12  (return %₁₁)

########################################
# Macro using `__context__`
macro foo(ex)
    ctx = __context__
end
#---------------------
1   (method TestMod.@foo)
2   latestworld
3   TestMod.@foo
4   (call core.Typeof %₃)
5   (call core.svec %₄ JuliaLowering.MacroContext core.Any)
6   (call core.svec)
7   SourceLocation::1:7
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(!read) slot₂/__context__ slot₃/ex(!read) slot₄/ctx(!read)]
    1   slot₂/__context__
    2   (= slot₄/ctx %₁)
    3   (return %₁)
10  latestworld
11  TestMod.@foo
12  (return %₁₁)

########################################
# Scope for symbols emitted by macros is the module where the method was
# defined, thus two different modules in this case, even though `@some_macro`
# belongs to the MacroMethods module.
(MacroMethods.@some_macro(), MacroMethods.@some_macro(unused))
#---------------------
1   TestMod.MacroMethods.some_global
2   TestMod.MacroMethods.ExtraMacroMethods.some_global
3   (call core.tuple %₁ %₂)
4   (return %₃)

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

########################################
# Error: Macros not found
_never_exist = @m_not_exist 42
#---------------------
MacroExpansionError while expanding @m_not_exist in module Main.TestMod:
_never_exist = @m_not_exist 42
#               └─────────┘ ── Macro not found
Caused by:
UndefVarError: `@m_not_exist` not defined in `Main.TestMod`
Suggestion: check for spelling errors or missing imports.

########################################
# Simple cmdstring
`echo 1`
#---------------------
1   Base.cmd_gen
2   (call core.tuple "echo")
3   (call core.tuple "1")
4   (call core.tuple %₂ %₃)
5   (call %₁ %₄)
6   (return %₅)

########################################
# Simple string macro
strmac"hello"
#---------------------
1   (return "hello from strmac")

########################################
# String macro with suffix
strmac"hello"blah
#---------------------
1   (return "hello from strmac with suffix blah")

########################################
# Simple cmd macro
cmdmac`hello`
#---------------------
1   (return "hello from cmdmac")

########################################
# Cmd macro with suffix
cmdmac`hello`12345
#---------------------
1   (return "hello from cmdmac with suffix 12345")

