########################################
# vect syntax
[a, b, c]
#---------------------
1   TestMod.a
2   TestMod.b
3   TestMod.c
4   (call top.vect %₁ %₂ %₃)
5   (return %₄)

########################################
# Error: vect syntax with parameters
[a, b; c]
#---------------------
LoweringError:
[a, b; c]
└───────┘ ── unexpected semicolon in array expression

########################################
# Error: vect syntax with embedded assignments
[a=b, c]
#---------------------
LoweringError:
[a=b, c]
#└─┘ ── misplaced assignment statement in `[ ... ]`

########################################
# hcat syntax
[a b c]
#---------------------
1   TestMod.a
2   TestMod.b
3   TestMod.c
4   (call top.hcat %₁ %₂ %₃)
5   (return %₄)

########################################
# typed hcat syntax
T[a b c]
#---------------------
1   TestMod.T
2   TestMod.a
3   TestMod.b
4   TestMod.c
5   (call top.typed_hcat %₁ %₂ %₃ %₄)
6   (return %₅)

########################################
# Error: hcat syntax with embedded assignments
[a b c=d]
#---------------------
LoweringError:
[a b c=d]
#   └──┘ ── misplaced assignment statement in `[ ... ]`

########################################
# vcat syntax
[a; b; c]
#---------------------
1   TestMod.a
2   TestMod.b
3   TestMod.c
4   (call top.vcat %₁ %₂ %₃)
5   (return %₄)

########################################
# vcat with splats
[a...; b; c]
#---------------------
1   TestMod.a
2   TestMod.b
3   TestMod.c
4   (call core.tuple %₂ %₃)
5   (call core._apply_iterate top.iterate top.vcat %₁ %₄)
6   (return %₅)

########################################
# hvcat syntax
[a; b c; d e f]
#---------------------
1   (call core.tuple 1 2 3)
2   TestMod.a
3   TestMod.b
4   TestMod.c
5   TestMod.d
6   TestMod.e
7   TestMod.f
8   (call top.hvcat %₁ %₂ %₃ %₄ %₅ %₆ %₇)
9   (return %₈)

########################################
# hvcat with splats nested within rows
[a; b c...]
#---------------------
1   TestMod.a
2   (call core.tuple %₁)
3   TestMod.b
4   (call core.tuple %₃)
5   TestMod.c
6   (call core._apply_iterate top.iterate core.tuple %₄ %₅)
7   (call top.hvcat_rows %₂ %₆)
8   (return %₇)

########################################
# Error: vcat syntax with assignments
[a=b; c]
#---------------------
LoweringError:
[a=b; c]
#└─┘ ── misplaced assignment statement in `[ ... ]`

########################################
# typed_vcat syntax
T[a; b; c]
#---------------------
1   TestMod.T
2   TestMod.a
3   TestMod.b
4   TestMod.c
5   (call top.typed_vcat %₁ %₂ %₃ %₄)
6   (return %₅)

########################################
# typed_hvcat syntax
T[a; b c; d e f]
#---------------------
1   TestMod.T
2   (call core.tuple 1 2 3)
3   TestMod.a
4   TestMod.b
5   TestMod.c
6   TestMod.d
7   TestMod.e
8   TestMod.f
9   (call top.typed_hvcat %₁ %₂ %₃ %₄ %₅ %₆ %₇ %₈)
10  (return %₉)

########################################
# typed_hvcat with splats nested within rows
T[a; b c...]
#---------------------
1   TestMod.T
2   TestMod.a
3   (call core.tuple %₂)
4   TestMod.b
5   (call core.tuple %₄)
6   TestMod.c
7   (call core._apply_iterate top.iterate core.tuple %₅ %₆)
8   (call top.typed_hvcat_rows %₁ %₃ %₇)
9   (return %₈)

