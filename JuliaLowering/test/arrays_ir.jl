########################################
# vect syntax
[10, 20, 30]
#---------------------
1   (call top.vect 10 20 30)
2   (return %₁)

########################################
# Error: vect syntax with parameters
[10, 20; 30]
#---------------------
LoweringError:
[10, 20; 30]
└──────────┘ ── unexpected semicolon in array expression

########################################
# Error: vect syntax with embedded assignments
[a=20, 30]
#---------------------
LoweringError:
[a=20, 30]
#└──┘ ── misplaced assignment statement in `[ ... ]`

########################################
# hcat syntax
[10 20 30]
#---------------------
1   (call top.hcat 10 20 30)
2   (return %₁)

########################################
# typed hcat syntax
T[10 20 30]
#---------------------
1   TestMod.T
2   (call top.typed_hcat %₁ 10 20 30)
3   (return %₂)

########################################
# Error: hcat syntax with embedded assignments
[10 20 a=40]
#---------------------
LoweringError:
[10 20 a=40]
#     └───┘ ── misplaced assignment statement in `[ ... ]`

########################################
# vcat syntax
[10; 20; 30]
#---------------------
1   (call top.vcat 10 20 30)
2   (return %₁)

########################################
# vcat with splats
[a...; 20; 30]
#---------------------
1   TestMod.a
2   (call core.tuple 20 30)
3   (call core._apply_iterate top.iterate top.vcat %₁ %₂)
4   (return %₃)

########################################
# hvcat syntax
[10; 20 30; 40 e f]
#---------------------
1   (call core.tuple 1 2 3)
2   TestMod.e
3   TestMod.f
4   (call top.hvcat %₁ 10 20 30 40 %₂ %₃)
5   (return %₄)

########################################
# hvcat with splats nested within rows
[10; 20 a...]
#---------------------
1   (call core.tuple 10)
2   (call core.tuple 20)
3   TestMod.a
4   (call core._apply_iterate top.iterate core.tuple %₂ %₃)
5   (call top.hvcat_rows %₁ %₄)
6   (return %₅)

########################################
# Error: vcat syntax with assignments
[a=20; 30]
#---------------------
LoweringError:
[a=20; 30]
#└──┘ ── misplaced assignment statement in `[ ... ]`

########################################
# typed_vcat syntax
T[10; 20; 30]
#---------------------
1   TestMod.T
2   (call top.typed_vcat %₁ 10 20 30)
3   (return %₂)

########################################
# typed_hvcat syntax
T[10; 20 30; 40 50 60]
#---------------------
1   TestMod.T
2   (call core.tuple 1 2 3)
3   (call top.typed_hvcat %₁ %₂ 10 20 30 40 50 60)
4   (return %₃)

########################################
# typed_hvcat with splats nested within rows
T[10; 20 a...]
#---------------------
1   TestMod.T
2   (call core.tuple 10)
3   (call core.tuple 20)
4   TestMod.a
5   (call core._apply_iterate top.iterate core.tuple %₃ %₄)
6   (call top.typed_hvcat_rows %₁ %₂ %₅)
7   (return %₆)
