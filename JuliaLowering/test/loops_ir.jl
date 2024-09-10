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
8   (return core.nothing)

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
7   (return core.nothing)

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
4   (goto label₉)
5   TestMod.body2
6   (goto label₈)
7   TestMod.body3
8   (goto label₁)
9   (return core.nothing)

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
6   (gotoifnot %₅ label₁₇)
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
17  (return core.nothing)

