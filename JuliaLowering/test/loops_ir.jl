########################################
# Basic while loop
while f(a)
    body1
    body2
end
#----------
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
#----------
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
#---------
1   TestMod.cond
2   (gotoifnot %₁ label₉)
3   TestMod.body1
4   (goto label₉)
5   TestMod.body2
6   (goto label₈)
7   TestMod.body3
8   (goto label₁)
9   (return core.nothing)
