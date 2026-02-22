########################################
# Basic while loop
while f(a)
    body1
    body2
end
#---------------------
1   (= slot₁/loop_exit_result core.nothing)
2   TestMod.f
3   TestMod.a
4   (call %₂ %₃)
5   (gotoifnot %₄ label₉)
6   TestMod.body1
7   TestMod.body2
8   (goto label₂)
9   (= slot₁/loop_exit_result core.nothing)
10  slot₁/loop_exit_result
11  (return %₁₀)

########################################
# While loop with short circuit condition
while a && b
    body
end
#---------------------
1   (= slot₁/loop_exit_result core.nothing)
2   TestMod.a
3   (gotoifnot %₂ label₈)
4   TestMod.b
5   (gotoifnot %₄ label₈)
6   TestMod.body
7   (goto label₂)
8   (= slot₁/loop_exit_result core.nothing)
9   slot₁/loop_exit_result
10  (return %₉)

########################################
# While loop with with break and continue
while cond
    body1
    break
    body2
    continue
    body3
end
#---------------------
1   (= slot₁/loop_exit_result core.nothing)
2   TestMod.cond
3   (gotoifnot %₂ label₁₀)
4   TestMod.body1
5   (goto label₁₁)
6   TestMod.body2
7   (goto label₉)
8   TestMod.body3
9   (goto label₂)
10  (= slot₁/loop_exit_result core.nothing)
11  slot₁/loop_exit_result
12  (return %₁₁)

########################################
# Basic for loop
for x in xs
    body
end
#---------------------
1   (= slot₃/loop_exit_result core.nothing)
2   TestMod.xs
3   (= slot₁/next (call top.iterate %₂))
4   slot₁/next
5   (call core.=== %₄ core.nothing)
6   (call top.not_int %₅)
7   (gotoifnot %₆ label₂₀)
8   slot₁/next
9   (= slot₂/x (call core.getfield %₈ 1))
10  (call core.getfield %₈ 2)
11  TestMod.body
12  (= slot₁/next (call top.iterate %₂ %₁₀))
13  slot₁/next
14  (call core.=== %₁₃ core.nothing)
15  (call top.not_int %₁₄)
16  (gotoifnot %₁₅ label₁₈)
17  (goto label₈)
18  (= slot₄/if_val core.nothing)
19  (goto label₂₁)
20  (= slot₄/if_val core.nothing)
21  slot₄/if_val
22  (= slot₃/loop_exit_result %₂₁)
23  slot₃/loop_exit_result
24  (return %₂₃)

########################################
# Syntax sugar for nested for loop
for x in xs, y in ys
    x = 10 # Copy of x; does not overwrite x iteration var
end
#---------------------
1   (= slot₆/loop_exit_result core.nothing)
2   TestMod.xs
3   (= slot₂/next (call top.iterate %₂))
4   slot₂/next
5   (call core.=== %₄ core.nothing)
6   (call top.not_int %₅)
7   (gotoifnot %₆ label₃₇)
8   slot₂/next
9   (= slot₃/x (call core.getfield %₈ 1))
10  (call core.getfield %₈ 2)
11  TestMod.ys
12  (= slot₁/next (call top.iterate %₁₁))
13  slot₁/next
14  (call core.=== %₁₃ core.nothing)
15  (call top.not_int %₁₄)
16  (gotoifnot %₁₅ label₂₉)
17  slot₃/x
18  (= slot₄/x %₁₇)
19  slot₁/next
20  (= slot₅/y (call core.getfield %₁₉ 1))
21  (call core.getfield %₁₉ 2)
22  (= slot₄/x 10)
23  (= slot₁/next (call top.iterate %₁₁ %₂₁))
24  slot₁/next
25  (call core.=== %₂₄ core.nothing)
26  (call top.not_int %₂₅)
27  (gotoifnot %₂₆ label₂₉)
28  (goto label₁₇)
29  (= slot₂/next (call top.iterate %₂ %₁₀))
30  slot₂/next
31  (call core.=== %₃₀ core.nothing)
32  (call top.not_int %₃₁)
33  (gotoifnot %₃₂ label₃₅)
34  (goto label₈)
35  (= slot₇/if_val core.nothing)
36  (goto label₃₈)
37  (= slot₇/if_val core.nothing)
38  slot₇/if_val
39  (= slot₆/loop_exit_result %₃₈)
40  slot₆/loop_exit_result
41  (return %₄₀)

########################################
# Error: break outside for/while
break
#---------------------
LoweringError:
break
└───┘ ── unlabeled `break` outside of a `while` or `for` loop

########################################
# Error: continue outside for/while
continue
#---------------------
LoweringError:
continue
└──────┘ ── `continue` outside of a `while` or `for` loop

########################################
# Error: `outer` without outer local variable
let
    for outer i = 1:2
        nothing
    end
    i
end
#---------------------
LoweringError:
let
    for outer i = 1:2
#             ╙ ── `outer` annotations must match with a local variable in an outer scope but no such variable was found
        nothing
    end
