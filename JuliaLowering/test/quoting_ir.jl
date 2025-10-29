########################################
# Simple interpolation
quote
    $x + 1
end
#---------------------
1   TestMod.x
2   (call core.tuple %₁)
3   (call JuliaLowering.interpolate_ast SyntaxTree (inert (block (call-i ($ x) + 1))) %₂)
4   (return %₃)

########################################
# Trivial interpolation
:($x)
#---------------------
1   TestMod.x
2   (call core.tuple %₁)
3   (call JuliaLowering.interpolate_ast SyntaxTree (inert ($ x)) %₂)
4   (return %₃)

########################################
# Double escape
quote
    quote
        $$x + 1
    end
end
#---------------------
1   TestMod.x
2   (call core.tuple %₁)
3   (call JuliaLowering.interpolate_ast SyntaxTree (inert (block (quote (block (call-i ($ ($ x)) + 1))))) %₂)
4   (return %₃)

########################################
# Symbols on `.` right hand side need to be scoped correctly
let x = 1
    :(A.$x)
end
#---------------------
1   1
2   (= slot₁/x %₁)
3   slot₁/x
4   (call core.tuple %₃)
5   (call JuliaLowering.interpolate_ast SyntaxTree (inert (. A ($ x))) %₄)
6   (return %₅)

########################################
# Error: Double escape
quote
    $$x + 1
end
#---------------------
LoweringError:
quote
    $$x + 1
#    └┘ ── `$` expression outside string or quote block
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
3   (call core.Typeof %₂)
4   (call core.svec %₃)
5   (call core.svec)
6   SourceLocation::1:10
7   (call core.svec %₄ %₅ %₆)
8   --- method core.nothing %₇
    slots: [slot₁/#self#(!read)]
    1   (return core.nothing)
9   latestworld
10  (return core.nothing)
