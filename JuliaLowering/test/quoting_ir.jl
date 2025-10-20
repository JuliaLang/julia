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
