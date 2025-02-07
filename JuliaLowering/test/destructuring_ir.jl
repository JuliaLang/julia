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
2   TestMod.b
3   (= slot₁/x %₁)
4   (= slot₂/y %₂)
5   (call core.tuple %₁ %₂)
6   (return %₅)

########################################
# Destructuring with tuple elimination where variables are repeated
let
    (x, y, z) = (y, a, x)
end
#---------------------
1   slot₂/y
2   TestMod.a
3   slot₁/x
4   (= slot₁/x %₁)
5   (= slot₂/y %₂)
6   (= slot₃/z %₃)
7   (call core.tuple %₁ %₂ %₃)
8   (return %₇)

########################################
# Destructuring with simple tuple elimination and rhs with side effects
let
    (x, y) = (f(), b)
end
#---------------------
1   TestMod.f
2   (call %₁)
3   TestMod.b
4   (= slot₁/x %₂)
5   (= slot₂/y %₃)
6   (call core.tuple %₂ %₃)
7   (return %₆)

########################################
# Destructuring with simple tuple elimination and lhs with side effects
let
    (x[10], y[20]) = (1,2)
end
#---------------------
1   1
2   TestMod.x
3   (call top.setindex! %₂ %₁ 10)
4   2
5   TestMod.y
6   (call top.setindex! %₅ %₄ 20)
7   (call core.tuple 1 2)
8   (return %₇)

########################################
# Destructuring with tuple elimination and trailing rhs ...
let
    (x, y) = (a, rhs...)
end
#---------------------
1   TestMod.a
2   TestMod.rhs
3   (= slot₁/x %₁)
4   (call top.indexed_iterate %₂ 1)
5   (= slot₂/y (call core.getfield %₄ 1))
6   (call core.tuple %₁)
7   (call core._apply_iterate top.iterate core.tuple %₆ %₂)
8   (return %₇)

########################################
# Destructuring with with non-trailing rhs `...` does not use tuple elimination
# (though we could do it for the `x = a` part here)
let
    (x, y, z) = (a, rhs..., b)
end
#---------------------
1   TestMod.a
2   (call core.tuple %₁)
3   TestMod.rhs
4   TestMod.b
5   (call core.tuple %₄)
6   (call core._apply_iterate top.iterate core.tuple %₂ %₃ %₅)
7   (call top.indexed_iterate %₆ 1)
8   (= slot₂/x (call core.getfield %₇ 1))
9   (= slot₁/iterstate (call core.getfield %₇ 2))
10  slot₁/iterstate
11  (call top.indexed_iterate %₆ 2 %₁₀)
12  (= slot₃/y (call core.getfield %₁₁ 1))
13  (= slot₁/iterstate (call core.getfield %₁₁ 2))
14  slot₁/iterstate
15  (call top.indexed_iterate %₆ 3 %₁₄)
16  (= slot₄/z (call core.getfield %₁₅ 1))
17  (return %₆)

########################################
# Destructuring with tuple elimination and final ... on lhs
let
    (x, ys...) = (a,b,c)
end
#---------------------
1   TestMod.a
2   TestMod.b
3   TestMod.c
4   (= slot₁/x %₁)
5   (call core.tuple %₂ %₃)
6   (= slot₂/ys %₅)
7   (call core.tuple %₁ %₂ %₃)
8   (return %₇)

########################################
# Destructuring with tuple elimination, slurping, and completely effect free right hand sides
let
    (x, ys...) = (1,2,3)
end
#---------------------
1   (= slot₁/x 1)
2   (call core.tuple 2 3)
3   (= slot₂/ys %₂)
4   (call core.tuple 1 2 3)
5   (return %₄)

########################################
# Destructuring with tuple elimination and non-final ... on lhs
let
    (x, ys..., z) = (a,b,c)
end
#---------------------
1   TestMod.a
2   TestMod.b
3   TestMod.c
4   (= slot₁/x %₁)
5   (call core.tuple %₂)
6   (= slot₂/ys %₅)
7   (= slot₃/z %₃)
8   (call core.tuple %₁ %₂ %₃)
9   (return %₈)

########################################
# Destructuring with tuple elimination but not in value position never creates
# the tuple
let
    (x, ys...) = (a,b,c)
    nothing
end
#---------------------
1   TestMod.a
2   TestMod.b
3   TestMod.c
4   (= slot₁/x %₁)
5   (call core.tuple %₂ %₃)
6   (= slot₂/ys %₅)
7   TestMod.nothing
8   (return %₇)

########################################
# Property destructuring
let
    (; x, y) = rhs
end
#---------------------
1   TestMod.rhs
2   (= slot₁/x (call top.getproperty %₁ :x))
3   (= slot₂/y (call top.getproperty %₁ :y))
4   (return %₁)

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
    (; x, y) = f()
end
#---------------------
1   TestMod.f
2   (call %₁)
3   (= slot₁/x (call top.getproperty %₂ :x))
4   (= slot₂/y (call top.getproperty %₂ :y))
5   (return %₂)

########################################
# Property destructuring with type decl
let
    (; x::T) = rhs
end
#---------------------
1   (newvar slot₁/x)
2   TestMod.rhs
3   (call top.getproperty %₂ :x)
4   TestMod.T
5   (= slot₂/tmp %₃)
6   slot₂/tmp
7   (call core.isa %₆ %₄)
8   (gotoifnot %₇ label₁₀)
9   (goto label₁₃)
10  slot₂/tmp
11  (call top.convert %₄ %₁₀)
12  (= slot₂/tmp (call core.typeassert %₁₁ %₄))
13  slot₂/tmp
14  (= slot₁/x %₁₃)
15  (return %₂)

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

