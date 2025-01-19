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
begin
    local a
    a = b() = c = d
end
#---------------------
1   (method TestMod.b)
2   TestMod.b
3   (call core.Typeof %₂)
4   (call core.svec %₃)
5   (call core.svec)
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 3 =#)))))
7   --- method core.nothing %₆
    slots: [slot₁/#self#(!read) slot₂/c(!read)]
    1   TestMod.d
    2   (= slot₂/c %₁)
    3   (return %₁)
8   TestMod.b
9   (= slot₁/a %₈)
10  (return %₈)

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
1   (newvar slot₁/x)
2   TestMod.f
3   (call %₂)
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
17  slot₁/x
18  (return %₁₇)

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

########################################
# UnionAll expansion at global scope results in const decl
X{T} = Y{T,T}
#---------------------
1   (const TestMod.X)
2   (call core.TypeVar :T)
3   (= slot₁/T %₂)
4   slot₁/T
5   TestMod.Y
6   slot₁/T
7   slot₁/T
8   (call core.apply_type %₅ %₆ %₇)
9   (call core.UnionAll %₄ %₈)
10  (= TestMod.X %₉)
11  (return %₉)

########################################
# UnionAll expansion in local scope
let
    X{T} = Y{T,T}
end
#---------------------
1   (call core.TypeVar :T)
2   (= slot₂/T %₁)
3   slot₂/T
4   TestMod.Y
5   slot₂/T
6   slot₂/T
7   (call core.apply_type %₄ %₅ %₆)
8   (call core.UnionAll %₃ %₇)
9   (= slot₁/X %₈)
10  (return %₈)

########################################
# simple setindex!
a[i] = x
#---------------------
1   TestMod.x
2   TestMod.a
3   TestMod.i
4   (call top.setindex! %₂ %₁ %₃)
5   (return %₁)

########################################
# simple setindex! with begin
a[begin] = x
#---------------------
1   TestMod.x
2   TestMod.a
3   TestMod.a
4   (call top.firstindex %₃)
5   (call top.setindex! %₂ %₁ %₄)
6   (return %₁)

########################################
# simple setindex! with end
a[end] = x
#---------------------
1   TestMod.x
2   TestMod.a
3   TestMod.a
4   (call top.lastindex %₃)
5   (call top.setindex! %₂ %₁ %₄)
6   (return %₁)

########################################
# multidimensional setindex! with begin
a[i, begin] = x
#---------------------
1   TestMod.x
2   TestMod.a
3   TestMod.i
4   TestMod.a
5   (call top.firstindex %₄ 2)
6   (call top.setindex! %₂ %₁ %₃ %₅)
7   (return %₁)

########################################
# multidimensional setindex! with end
a[i, end] = x
#---------------------
1   TestMod.x
2   TestMod.a
3   TestMod.i
4   TestMod.a
5   (call top.lastindex %₄ 2)
6   (call top.setindex! %₂ %₁ %₃ %₅)
7   (return %₁)

########################################
# multidimensional setindex! with begin/end and splats
a[is..., end, js..., begin] = x
#---------------------
1   TestMod.is
2   TestMod.a
3   (call top.length %₁)
4   (call top.+ 1 %₃)
5   (call top.lastindex %₂ %₄)
6   TestMod.js
7   TestMod.a
8   (call top.length %₁)
9   (call top.length %₆)
10  (call top.+ 2 %₈ %₉)
11  (call top.firstindex %₇ %₁₀)
12  TestMod.x
13  TestMod.a
14  (call core.tuple %₁₃ %₁₂)
15  (call core.tuple %₅)
16  (call core.tuple %₁₁)
17  (call core._apply_iterate top.iterate top.setindex! %₁₄ %₁ %₁₅ %₆ %₁₆)
18  (return %₁₂)

########################################
# setindex! with nontrivial array expression and begin/end
f()[end] = x
#---------------------
1   TestMod.f
2   (call %₁)
3   TestMod.x
4   (call top.lastindex %₂)
5   (call top.setindex! %₂ %₃ %₄)
6   (return %₃)

########################################
# nested refs (fixme!)
b[a[begin]] = x
#---------------------
1   TestMod.x
2   TestMod.b
3   TestMod.a
4   TestMod.begin
5   (call top.getindex %₃ %₄)
6   (call top.setindex! %₂ %₁ %₅)
7   (return %₁)

########################################
# empty ref and setindex!
let
    a[] = rhs
end
#---------------------
1   TestMod.rhs
2   TestMod.a
3   (call top.setindex! %₂ %₁)
4   (return %₁)

########################################
# Error: Invalid lhs in `=`
a.(b) = rhs
#---------------------
LoweringError:
a.(b) = rhs
└───┘ ── invalid dot call syntax on left hand side of assignment

########################################
# Error: Invalid lhs in `=`
T[x y] = rhs
#---------------------
LoweringError:
T[x y] = rhs
└────┘ ── invalid spacing in left side of indexed assignment

########################################
# Error: Invalid lhs in `=`
T[x; y] = rhs
#---------------------
LoweringError:
T[x; y] = rhs
└─────┘ ── unexpected `;` in left side of indexed assignment

########################################
# Error: Invalid lhs in `=`
T[x ;;; y] = rhs
#---------------------
LoweringError:
T[x ;;; y] = rhs
└────────┘ ── unexpected `;` in left side of indexed assignment

########################################
# Error: Invalid lhs in `=`
[x, y] = rhs
#---------------------
LoweringError:
[x, y] = rhs
└────┘ ── use `(a, b) = ...` to assign multiple values

########################################
# Error: Invalid lhs in `=`
[x y] = rhs
#---------------------
LoweringError:
[x y] = rhs
└───┘ ── use `(a, b) = ...` to assign multiple values

########################################
# Error: Invalid lhs in `=`
[x; y] = rhs
#---------------------
LoweringError:
[x; y] = rhs
└────┘ ── use `(a, b) = ...` to assign multiple values

########################################
# Error: Invalid lhs in `=`
[x ;;; y] = rhs
#---------------------
LoweringError:
[x ;;; y] = rhs
└───────┘ ── use `(a, b) = ...` to assign multiple values

########################################
# Error: Invalid lhs in `=`
1 = rhs
#---------------------
LoweringError:
1 = rhs
╙ ── invalid assignment location

