########################################
# Basic import
import A: b
#---------------------
1   (call core.svec "A")
2   (call core.svec 1 "b" core.nothing)
3   (call JuliaLowering.module_import TestMod false %₁ %₂)
4   (return %₃)

########################################
# Import with paths and `as`
import A.B.C: b, c.d as e
#---------------------
1   (call core.svec "A" "B" "C")
2   (call core.svec 1 "b" core.nothing 2 "c" "d" "e")
3   (call JuliaLowering.module_import TestMod false %₁ %₂)
4   (return %₃)

########################################
# Using
using A
#---------------------
1   (call core.svec 1 "A" core.nothing)
2   (call JuliaLowering.module_import TestMod true core.nothing %₁)
3   (return %₂)

########################################
# Using with paths and `as`
using A.B.C: b, c.d as e
#---------------------
1   (call core.svec "A" "B" "C")
2   (call core.svec 1 "b" core.nothing 2 "c" "d" "e")
3   (call JuliaLowering.module_import TestMod true %₁ %₂)
4   (return %₃)

########################################
# Error: Import not at top level
function f()
    import A: b
end
#---------------------
LoweringError:
function f()
    import A: b
#   └─────────┘ ── this syntax is only allowed in top level code
end

