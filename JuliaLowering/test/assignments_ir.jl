########################################
# chain of assignments
let
    a = b = c = 1
end
#---------------------
1   1
2   (= slot₁/a %₁)
3   (= slot₂/b %₁)
4   (= slot₃/c %₁)
5   (return %₁)

########################################
# chain of assignments with nontrivial rhs
let
    a = b = c = f()
end
#---------------------
1   TestMod.f
2   (call %₁)
3   (= slot₁/a %₂)
4   (= slot₂/b %₂)
5   (= slot₃/c %₂)
6   (return %₂)

########################################
# short form function def, not chain of assignments
let
    a = b() = c = d
end
#---------------------
1   (method :b)
2   TestMod.b
3   (call core.Typeof %₂)
4   (call core.svec %₃)
5   (call core.svec)
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 2 =#)))))
7   --- method :b %₆
    1   TestMod.d
    2   (= slot₂/c %₁)
    3   (return %₁)
8   (= slot₁/a %₁)
9   (return %₁)

########################################
# a.b = ... => setproperty! assignment
let
    a.b = c
end
#---------------------
1   TestMod.a
2   TestMod.c
3   (call top.setproperty! %₁ :b %₂)
4   TestMod.c
5   (return %₄)

########################################
# a.b.c = f() => setproperty! assignment, complex case
let
    a.b.c = f()
end
#---------------------
1   TestMod.a
2   (call top.getproperty %₁ :b)
3   TestMod.f
4   (call %₃)
5   (call top.setproperty! %₂ :c %₄)
6   (return %₄)

########################################
# declarations of typed locals
let
    x::T = f()
    x
end
#---------------------
1   TestMod.f
2   (call %₁)
3   (= slot₂/tmp %₂)
4   slot₂/tmp
5   TestMod.T
6   (call core.isa %₄ %₅)
7   (gotoifnot %₆ label₉)
8   (goto label₁₄)
9   TestMod.T
10  slot₂/tmp
11  (call top.convert %₉ %₁₀)
12  TestMod.T
13  (= slot₂/tmp (call core.typeassert %₁₁ %₁₂))
14  slot₂/tmp
15  (= slot₁/x %₁₄)
16  slot₁/x
17  (return %₁₆)

########################################
# "complex lhs" of `::T` => type-assert, not decl
let
    a.b::T = f()
    x
end
#---------------------
1   TestMod.a
2   (call top.getproperty %₁ :b)
3   TestMod.T
4   (call core.typeassert %₂ %₃)
5   TestMod.f
6   (call %₅)
7   TestMod.a
8   (call top.setproperty! %₇ :b %₆)
9   TestMod.x
10  (return %₉)

