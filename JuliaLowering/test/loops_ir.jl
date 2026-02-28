########################################
# Basic while loop
while f(a)
    body1
    body2
end
#---------------------
1   TestMod.f
2   TestMod.a
3   (call %₁ %₂)
4   (gotoifnot %₃ label₈)
5   TestMod.body1
6   TestMod.body2
7   (goto label₁)
8   (= slot₁/loop-exit_result core.nothing)
9   (isdefined slot₁/loop-exit_result)
10  (gotoifnot %₉ label₁₂)
11  (goto label₁₃)
12  (= slot₁/loop-exit_result core.nothing)
13  slot₁/loop-exit_result
14  (return %₁₃)

########################################
# While loop with short circuit condition
while a && b
    body
end
#---------------------
1   TestMod.a
2   (gotoifnot %₁ label₇)
3   TestMod.b
4   (gotoifnot %₃ label₇)
5   TestMod.body
6   (goto label₁)
7   (= slot₁/loop-exit_result core.nothing)
8   (isdefined slot₁/loop-exit_result)
9   (gotoifnot %₈ label₁₁)
10  (goto label₁₂)
11  (= slot₁/loop-exit_result core.nothing)
12  slot₁/loop-exit_result
13  (return %₁₂)

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
1   TestMod.cond
2   (gotoifnot %₁ label₉)
3   TestMod.body1
4   (goto label₁₀)
5   TestMod.body2
6   (goto label₈)
7   TestMod.body3
8   (goto label₁)
9   (= slot₁/loop-exit_result core.nothing)
10  (isdefined slot₁/loop-exit_result)
11  (gotoifnot %₁₀ label₁₃)
12  (goto label₁₄)
13  (= slot₁/loop-exit_result core.nothing)
14  slot₁/loop-exit_result
15  (return %₁₄)

########################################
# Basic for loop
for x in xs
    body
end
#---------------------
1   TestMod.xs
2   (= slot₁/next (call top.iterate %₁))
3   slot₁/next
4   (call core.=== %₃ core.nothing)
5   (call top.not_int %₄)
6   (gotoifnot %₅ label₁₉)
7   slot₁/next
8   (= slot₂/x (call core.getfield %₇ 1))
9   (call core.getfield %₇ 2)
10  TestMod.body
11  (= slot₁/next (call top.iterate %₁ %₉))
12  slot₁/next
13  (call core.=== %₁₂ core.nothing)
14  (call top.not_int %₁₃)
15  (gotoifnot %₁₄ label₁₇)
16  (goto label₇)
17  (= slot₄/if_val core.nothing)
18  (goto label₂₀)
19  (= slot₄/if_val core.nothing)
20  slot₄/if_val
21  (= slot₃/loop-exit_result %₂₀)
22  (isdefined slot₃/loop-exit_result)
23  (gotoifnot %₂₂ label₂₅)
24  (goto label₂₆)
25  (= slot₃/loop-exit_result core.nothing)
26  slot₃/loop-exit_result
27  (return %₂₆)

########################################
# Syntax sugar for nested for loop
for x in xs, y in ys
    x = 10 # Copy of x; does not overwrite x iteration var
end
#---------------------
1   TestMod.xs
2   (= slot₂/next (call top.iterate %₁))
3   slot₂/next
4   (call core.=== %₃ core.nothing)
5   (call top.not_int %₄)
6   (gotoifnot %₅ label₃₆)
7   slot₂/next
8   (= slot₃/x (call core.getfield %₇ 1))
9   (call core.getfield %₇ 2)
10  TestMod.ys
11  (= slot₁/next (call top.iterate %₁₀))
12  slot₁/next
13  (call core.=== %₁₂ core.nothing)
14  (call top.not_int %₁₃)
15  (gotoifnot %₁₄ label₂₈)
16  slot₃/x
17  (= slot₄/x %₁₆)
18  slot₁/next
19  (= slot₅/y (call core.getfield %₁₈ 1))
20  (call core.getfield %₁₈ 2)
21  (= slot₄/x 10)
22  (= slot₁/next (call top.iterate %₁₀ %₂₀))
23  slot₁/next
24  (call core.=== %₂₃ core.nothing)
25  (call top.not_int %₂₄)
26  (gotoifnot %₂₅ label₂₈)
27  (goto label₁₆)
28  (= slot₂/next (call top.iterate %₁ %₉))
29  slot₂/next
30  (call core.=== %₂₉ core.nothing)
31  (call top.not_int %₃₀)
32  (gotoifnot %₃₁ label₃₄)
33  (goto label₇)
34  (= slot₇/if_val core.nothing)
35  (goto label₃₇)
36  (= slot₇/if_val core.nothing)
37  slot₇/if_val
38  (= slot₆/loop-exit_result %₃₇)
39  (isdefined slot₆/loop-exit_result)
40  (gotoifnot %₃₉ label₄₂)
41  (goto label₄₃)
42  (= slot₆/loop-exit_result core.nothing)
43  slot₆/loop-exit_result
44  (return %₄₃)

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
