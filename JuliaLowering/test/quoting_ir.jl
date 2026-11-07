########################################
# Simple interpolation
quote
    $x + 1
end
#---------------------
1   TestMod.x
2   (call core._expr :call :+ %₁ 1)
3   (call core._expr :block :(#= :2 =#) %₂)
4   (return %₃)

########################################
# Trivial interpolation
:($x)
#---------------------
1   TestMod.x
2   (return %₁)

########################################
# Double escape
quote
    quote
        $$x + 1
    end
end
#---------------------
1   TestMod.x
2   (call core._expr :$ %₁)
3   (call core._expr :call :+ %₂ 1)
4   (call core._expr :block :(#= :3 =#) %₃)
5   (call core._expr :quote %₄)
6   (call core._expr :block :(#= :2 =#) %₅)
7   (return %₆)

########################################
# Symbols on `.` right hand side need to be scoped correctly
let x = 1
    :(A.$x)
end
#---------------------
1   1
2   (= slot₁/x %₁)
3   (call core.QuoteNode slot₁/x)
4   (call core._expr :. :A %₃)
5   (return %₄)

########################################
# Error: Double escape
quote
    $$x + 1
end
#---------------------
LoweringError:
quote
    $$x + 1
#    └┘ ── `$` expression outside string or quote
end

########################################
# Quoted property access with identifier
Core.:(foo)
#---------------------
1   TestMod.Core
2   (call top.getproperty %₁ :foo)
3   (return %₂)

########################################
# Quoted property access with operator
Core.:(!==)
#---------------------
1   TestMod.Core
2   (call top.getproperty %₁ :!==)
3   (return %₂)

########################################
# Quoted operator function definition (issue #20)
function Base.:(==)() end
#---------------------
1   TestMod.Base
2   (call top.getproperty %₁ :==)
3   (call core.TypeEqOf %₂)
4   (call core.svec %₃)
5   (call core.svec)
6   SourceLocation:1
7   (call core.svec %₄ %₅ %₆)
8   (call core.define_method TestMod core.nothing %₇
    --- code_info
    slots: [slot₁/#self#(!read)]
    1   (return core.nothing)
9   latestworld
10  (return core.nothing)
