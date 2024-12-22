########################################
# Simple destructuring
let
    (x,y) = as
end
#---------------------
1   TestMod.as
2   (call top.indexed_iterate %₁ 1)
3   (= slot₂/x (call core.getfield %₂ 1))
4   (= slot₁/iterstate (call core.getfield %₂ 2))
5   TestMod.as
6   slot₁/iterstate
7   (call top.indexed_iterate %₅ 2 %₆)
8   (= slot₃/y (call core.getfield %₇ 1))
9   TestMod.as
10  (return %₉)

########################################
# Trivial slurping
let
    (xs...,) = as
end
#---------------------
1   TestMod.as
2   (= slot₁/xs (call top.rest %₁))
3   TestMod.as
4   (return %₃)

########################################
# Slurping last arg
let
    (x, ys...) = as
end
#---------------------
1   TestMod.as
2   (call top.indexed_iterate %₁ 1)
3   (= slot₂/x (call core.getfield %₂ 1))
4   (= slot₁/iterstate (call core.getfield %₂ 2))
5   TestMod.as
6   slot₁/iterstate
7   (= slot₃/ys (call top.rest %₅ %₆))
8   TestMod.as
9   (return %₈)

########################################
# Slurping, first arg
let
    (xs..., y, z) = as
end
#---------------------
1   TestMod.as
2   (call top.split_rest %₁ 2)
3   (= slot₂/xs (call core.getfield %₂ 1))
4   (call core.getfield %₂ 2)
5   (call top.indexed_iterate %₄ 1)
6   (= slot₃/y (call core.getfield %₅ 1))
7   (= slot₁/iterstate (call core.getfield %₅ 2))
8   slot₁/iterstate
9   (call top.indexed_iterate %₄ 2 %₈)
10  (= slot₄/z (call core.getfield %₉ 1))
11  TestMod.as
12  (return %₁₁)

########################################
# Slurping, middle arg
let
    (x, ys..., z) = as
end
#---------------------
1   TestMod.as
2   (call top.indexed_iterate %₁ 1)
3   (= slot₂/x (call core.getfield %₂ 1))
4   (= slot₁/iterstate (call core.getfield %₂ 2))
5   TestMod.as
6   slot₁/iterstate
7   (call top.split_rest %₅ 1 %₆)
8   (= slot₃/ys (call core.getfield %₇ 1))
9   (call core.getfield %₇ 2)
10  (call top.indexed_iterate %₉ 1)
11  (= slot₄/z (call core.getfield %₁₀ 1))
12  TestMod.as
13  (return %₁₂)

########################################
# Error: Slurping multiple args
(xs..., ys...) = x
#---------------------
LoweringError:
(xs..., ys...) = x
#      └────┘ ── multiple `...` in destructuring assignment are ambiguous

########################################
# Recursive destructuring
let
    ((x,y), (z,w)) = as
end
#---------------------
1   TestMod.as
2   (call top.indexed_iterate %₁ 1)
3   (call core.getfield %₂ 1)
4   (= slot₁/iterstate (call core.getfield %₂ 2))
5   TestMod.as
6   slot₁/iterstate
7   (call top.indexed_iterate %₅ 2 %₆)
8   (call core.getfield %₇ 1)
9   (call top.indexed_iterate %₃ 1)
10  (= slot₅/x (call core.getfield %₉ 1))
11  (= slot₂/iterstate (call core.getfield %₉ 2))
12  slot₂/iterstate
13  (call top.indexed_iterate %₃ 2 %₁₂)
14  (= slot₆/y (call core.getfield %₁₃ 1))
15  (call top.indexed_iterate %₈ 1)
16  (= slot₇/z (call core.getfield %₁₅ 1))
17  (= slot₃/iterstate (call core.getfield %₁₅ 2))
18  slot₃/iterstate
19  (call top.indexed_iterate %₈ 2 %₁₈)
20  (= slot₄/w (call core.getfield %₁₉ 1))
21  TestMod.as
22  (return %₂₁)

########################################
# Recursive destructuring with slurping
let
    ((x,ys...), z) = as
end
#---------------------
1   TestMod.as
2   (call top.indexed_iterate %₁ 1)
3   (call core.getfield %₂ 1)
4   (= slot₁/iterstate (call core.getfield %₂ 2))
5   TestMod.as
6   slot₁/iterstate
7   (call top.indexed_iterate %₅ 2 %₆)
8   (= slot₅/z (call core.getfield %₇ 1))
9   (call top.indexed_iterate %₃ 1)
10  (= slot₃/x (call core.getfield %₉ 1))
11  (= slot₂/iterstate (call core.getfield %₉ 2))
12  slot₂/iterstate
13  (= slot₄/ys (call top.rest %₃ %₁₂))
14  TestMod.as
15  (return %₁₄)

########################################
# Destructuring with simple tuple elimination
let
    (x, y) = (a, b)
end
#---------------------
1   TestMod.a
2   (= slot₁/x %₁)
3   TestMod.b
4   (= slot₂/y %₃)
5   TestMod.a
6   TestMod.b
7   (call core.tuple %₅ %₆)
8   (return %₇)

########################################
# Destructuring with simple tuple elimination and non effect-free rhs
let
    (x, y) = (f(), b)
end
#---------------------
1   TestMod.f
2   (call %₁)
3   TestMod.b
4   (= slot₂/y %₃)
5   (= slot₁/x %₂)
6   TestMod.b
7   (call core.tuple %₂ %₆)
8   (return %₇)

########################################
# Destructuring with tuple elimination where variables are repeated
let
    (x, y, z) = (y, a, x)
end
#---------------------
1   slot₂/y
2   TestMod.a
3   (= slot₂/y %₂)
4   slot₁/x
5   (= slot₃/z %₄)
6   (= slot₁/x %₁)
7   TestMod.a
8   slot₁/x
9   (call core.tuple %₁ %₇ %₈)
10  (return %₉)

########################################
# Destructuring with tuple elimination and trailing rhs ...
let
    (x, y) = (a, rhs...)
end
#---------------------
1   TestMod.a
2   (= slot₁/x %₁)
3   TestMod.rhs
4   (call top.indexed_iterate %₃ 1)
5   (= slot₂/y (call core.getfield %₄ 1))
6   TestMod.a
7   (call core.tuple %₆)
8   (call core._apply_iterate top.iterate core.tuple %₇ %₃)
9   (return %₈)

########################################
# Property destructuring
let
    (; x, y) = rhs
end
#---------------------
1   TestMod.rhs
2   (= slot₁/x (call top.getproperty %₁ :x))
3   TestMod.rhs
4   (= slot₂/y (call top.getproperty %₃ :y))
5   TestMod.rhs
6   (return %₅)

########################################
# Property destructuring with colliding symbolic lhs/rhs
let
    local x
    (; x, y) = x
end
#---------------------
1   slot₁/x
2   (= slot₁/x (call top.getproperty %₁ :x))
3   (= slot₂/y (call top.getproperty %₁ :y))
4   (return %₁)

########################################
# Property destructuring with nontrivial rhs
let
    (; x) = f()
end
#---------------------
1   TestMod.f
2   (call %₁)
3   (= slot₁/x (call top.getproperty %₂ :x))
4   (return %₂)

########################################
# Property destructuring with type decl
let
    (; x::T) = rhs
end
#---------------------
1   (newvar slot₁)
2   TestMod.rhs
3   (call top.getproperty %₂ :x)
4   (= slot₂/tmp %₃)
5   slot₂/tmp
6   TestMod.T
7   (call core.isa %₅ %₆)
8   (gotoifnot %₇ label₁₀)
9   (goto label₁₅)
10  TestMod.T
11  slot₂/tmp
12  (call top.convert %₁₀ %₁₁)
13  TestMod.T
14  (= slot₂/tmp (call core.typeassert %₁₂ %₁₃))
15  slot₂/tmp
16  (= slot₁/x %₁₅)
17  TestMod.rhs
18  (return %₁₇)

########################################
# Error: Property destructuring with frankentuple
(x ; a, b) = rhs
#---------------------
LoweringError:
(x ; a, b) = rhs
└────────┘ ── Property destructuring must use a single `;` before the property names, eg `(; a, b) = rhs`

########################################
# Error: Property destructuring with values for properties
(; a=1, b) = rhs
#---------------------
LoweringError:
(; a=1, b) = rhs
#  └─┘ ── invalid assignment location

