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
# While loop with break and continue
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
2   (call core.declare_global TestMod : true)
3   latestworld
4   (call top.iterate %₁)
5   (call core.get_binding_type TestMod :)
6   (= slot₂/tmp %₄)
7   (call core.isa slot₂/tmp %₅)
8   (gotoifnot %₇ label₁₀)
9   (goto label₁₁)
10  (= slot₂/tmp (call top.convert %₅ slot₂/tmp))
11  slot₂/tmp
12  (call core.setglobal! TestMod : %₁₁)
13  (call core.declare_global TestMod : true)
14  latestworld
15  TestMod.
16  (call core.=== %₁₅ core.nothing)
17  (call top.not_int %₁₆)
18  (gotoifnot %₁₇ label₃₉)
19  TestMod.
20  (= slot₁/x (call core.getfield %₁₉ 1))
21  (call core.getfield %₁₉ 2)
22  TestMod.body
23  (call top.iterate %₁ %₂₁)
24  (call core.get_binding_type TestMod :)
25  (= slot₃/tmp %₂₃)
26  (call core.isa slot₃/tmp %₂₄)
27  (gotoifnot %₂₆ label₂₉)
28  (goto label₃₀)
29  (= slot₃/tmp (call top.convert %₂₄ slot₃/tmp))
30  slot₃/tmp
31  (call core.setglobal! TestMod : %₃₀)
32  TestMod.
33  (call core.=== %₃₂ core.nothing)
34  (call top.not_int %₃₃)
35  (gotoifnot %₃₄ label₃₇)
36  (goto label₁₉)
37  (= slot₅/if_val core.nothing)
38  (goto label₄₀)
39  (= slot₅/if_val core.nothing)
40  slot₅/if_val
41  (= slot₄/loop-exit_result %₄₀)
42  (isdefined slot₄/loop-exit_result)
43  (gotoifnot %₄₂ label₄₅)
44  (goto label₄₆)
45  (= slot₄/loop-exit_result core.nothing)
46  slot₄/loop-exit_result
47  (return %₄₆)

########################################
# Syntax sugar for nested for loop
for x in xs, y in ys
    x = 10 # Copy of x; does not overwrite x iteration var
end
#---------------------
1   TestMod.xs
2   (call core.declare_global TestMod : true)
3   latestworld
4   (call top.iterate %₁)
5   (call core.get_binding_type TestMod :)
6   (= slot₅/tmp %₄)
7   (call core.isa slot₅/tmp %₅)
8   (gotoifnot %₇ label₁₀)
9   (goto label₁₁)
10  (= slot₅/tmp (call top.convert %₅ slot₅/tmp))
11  slot₅/tmp
12  (call core.setglobal! TestMod : %₁₁)
13  (call core.declare_global TestMod : true)
14  latestworld
15  TestMod.
16  (call core.=== %₁₅ core.nothing)
17  (call top.not_int %₁₆)
18  (gotoifnot %₁₇ label₅₆)
19  TestMod.
20  (= slot₁/x (call core.getfield %₁₉ 1))
21  (call core.getfield %₁₉ 2)
22  TestMod.ys
23  (= slot₂/ (call top.iterate %₂₂))
24  slot₂/
25  (call core.=== %₂₄ core.nothing)
26  (call top.not_int %₂₅)
27  (gotoifnot %₂₆ label₄₀)
28  slot₁/x
29  (= slot₃/x %₂₈)
30  slot₂/
31  (= slot₄/y (call core.getfield %₃₀ 1))
32  (call core.getfield %₃₀ 2)
33  (= slot₃/x 10)
34  (= slot₂/ (call top.iterate %₂₂ %₃₂))
35  slot₂/
36  (call core.=== %₃₅ core.nothing)
37  (call top.not_int %₃₆)
38  (gotoifnot %₃₇ label₄₀)
39  (goto label₂₈)
40  (call top.iterate %₁ %₂₁)
41  (call core.get_binding_type TestMod :)
42  (= slot₆/tmp %₄₀)
43  (call core.isa slot₆/tmp %₄₁)
44  (gotoifnot %₄₃ label₄₆)
45  (goto label₄₇)
46  (= slot₆/tmp (call top.convert %₄₁ slot₆/tmp))
47  slot₆/tmp
48  (call core.setglobal! TestMod : %₄₇)
49  TestMod.
50  (call core.=== %₄₉ core.nothing)
51  (call top.not_int %₅₀)
52  (gotoifnot %₅₁ label₅₄)
53  (goto label₁₉)
54  (= slot₈/if_val core.nothing)
55  (goto label₅₇)
56  (= slot₈/if_val core.nothing)
57  slot₈/if_val
58  (= slot₇/loop-exit_result %₅₇)
59  (isdefined slot₇/loop-exit_result)
60  (gotoifnot %₅₉ label₆₂)
61  (goto label₆₃)
62  (= slot₇/loop-exit_result core.nothing)
63  slot₇/loop-exit_result
64  (return %₆₃)

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
