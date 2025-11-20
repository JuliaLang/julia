########################################
# vect syntax
[10, 20, 30]
#---------------------
1   (call top.vect 10 20 30)
2   (return %₁)

########################################
# vect with splat
[x, xs...]
#---------------------
1   TestMod.x
2   (call core.tuple %₁)
3   TestMod.xs
4   (call core._apply_iterate top.iterate top.vect %₂ %₃)
5   (return %₄)

########################################
# vect with splats
[x, xs..., y, ys...]
#---------------------
1   TestMod.x
2   (call core.tuple %₁)
3   TestMod.xs
4   TestMod.y
5   (call core.tuple %₄)
6   TestMod.ys
7   (call core._apply_iterate top.iterate top.vect %₂ %₃ %₅ %₆)
8   (return %₇)

########################################
# Error: vect syntax with parameters
[10, 20; 30]
#---------------------
LoweringError:
[10, 20; 30]
#      └──┘ ── unexpected semicolon in array expression

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
# hcat with splat
[x xs...]
#---------------------
1   TestMod.x
2   (call core.tuple %₁)
3   TestMod.xs
4   (call core._apply_iterate top.iterate top.hcat %₂ %₃)
5   (return %₄)

########################################
# typed hcat syntax
T[10 20 30]
#---------------------
1   TestMod.T
2   (call top.typed_hcat %₁ 10 20 30)
3   (return %₂)

########################################
# typed hcat syntax with splat
T[x xs...]
#---------------------
1   TestMod.T
2   TestMod.x
3   (call core.tuple %₁ %₂)
4   TestMod.xs
5   (call core._apply_iterate top.iterate top.typed_hcat %₃ %₄)
6   (return %₅)

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

########################################
# ncat with a single dimension
[10 ;;; 20 ;;; 30]
#---------------------
1   (call top.hvncat 3 10 20 30)
2   (return %₁)

########################################
# typed_ncat with a single dimension
T[10 ;;; 20 ;;; 30]
#---------------------
1   TestMod.T
2   (call top.typed_hvncat %₁ 3 10 20 30)
3   (return %₂)

########################################
# ncat with balanced column major element layout
[10 ; 20 ; 30 ;;; 40 ; 50 ; 60]
#---------------------
1   (call core.tuple 3 1 2)
2   (call top.hvncat %₁ false 10 20 30 40 50 60)
3   (return %₂)

########################################
# typed multidimensional ncat
T[10 ; 20 ; 30 ;;; 40 ; 50 ; 60]
#---------------------
1   TestMod.T
2   (call core.tuple 3 1 2)
3   (call top.typed_hvncat %₁ %₂ false 10 20 30 40 50 60)
4   (return %₃)

########################################
# ncat with balanced row major element layout
[10 20 30 ; 40 50 60 ;;;]
#---------------------
1   (call core.tuple 2 3 1)
2   (call top.hvncat %₁ true 10 20 30 40 50 60)
3   (return %₂)

########################################
# ncat of 3D array with balanced layout
[10 ; 20 ;; 30 ; 40 ;;; 50 ; 60 ;; 70 ; 80]
#---------------------
1   (call core.tuple 2 2 2)
2   (call top.hvncat %₁ false 10 20 30 40 50 60 70 80)
3   (return %₂)

########################################
# ncat with unbalanced column major layout
[10 ; 20 ;; 30 ;;; 40 ;;;;]
#---------------------
1   (call core.tuple 2 1 1)
2   (call core.tuple 3 1)
3   (call core.tuple 4)
4   (call core.tuple 4)
5   (call core.tuple %₁ %₂ %₃ %₄)
6   (call top.hvncat %₅ false 10 20 30 40)
7   (return %₆)

########################################
# ncat with unbalanced row major layout
[10 20 ; 30 40 ; 50 60 ;;; 70 ;;; 80 ;;;;]
#---------------------
1   (call core.tuple 2 2 2 1 1)
2   (call core.tuple 6 1 1)
3   (call core.tuple 8)
4   (call core.tuple 8)
5   (call core.tuple %₁ %₂ %₃ %₄)
6   (call top.hvncat %₅ true 10 20 30 40 50 60 70 80)
7   (return %₆)

########################################
# Splatting with 1D ncat
[xs ;;; ys... ;;; zs]
#---------------------
1   TestMod.xs
2   (call core.tuple 3 %₁)
3   TestMod.ys
4   TestMod.zs
5   (call core.tuple %₄)
6   (call core._apply_iterate top.iterate top.hvncat %₂ %₃ %₅)
7   (return %₆)

########################################
# Error: splatting with multi-dimensional ncat
[xs ; ys ;;; zs...]
#---------------------
LoweringError:
[xs ; ys ;;; zs...]
#            └───┘ ── Splatting ... in an `ncat` with multiple dimensions is not supported

########################################
# Error: bad nrow nesting
@ast_ [K"ncat"(syntax_flags=set_numeric_flags(3))
    [K"nrow"(syntax_flags=set_numeric_flags(1))
        [K"nrow"(syntax_flags=set_numeric_flags(1))
            1::K"Integer"
        ]
    ]
]
#---------------------
LoweringError:
#= line 1 =# - Badly nested rows in `ncat`

########################################
# Error: bad nrow nesting
@ast_ [K"ncat"(syntax_flags=set_numeric_flags(3))
    [K"nrow"(syntax_flags=set_numeric_flags(2))
        [K"row"
            1::K"Integer"
        ]
    ]
]
#---------------------
LoweringError:
#= line 1 =# - 2D `nrow` cannot be mixed with `row` in `ncat`

########################################
# Error: bad nrow nesting
@ast_ [K"ncat"(syntax_flags=set_numeric_flags(3))
    [K"row"
        [K"row"
            1::K"Integer"
        ]
    ]
]
#---------------------
LoweringError:
#= line 1 =# - Badly nested rows in `ncat`

########################################
# Simple getindex
a[i]
#---------------------
1   TestMod.a
2   TestMod.i
3   (call top.getindex %₁ %₂)
4   (return %₃)

########################################
# simple 1D getindex with begin
a[begin]
#---------------------
1   TestMod.a
2   (call top.firstindex %₁)
3   (call top.getindex %₁ %₂)
4   (return %₃)

########################################
# simple 1D getindex with end
a[end]
#---------------------
1   TestMod.a
2   (call top.lastindex %₁)
3   (call top.getindex %₁ %₂)
4   (return %₃)

########################################
# multidimensional getindex with begin
a[i, begin]
#---------------------
1   TestMod.a
2   TestMod.i
3   (call top.firstindex %₁ 2)
4   (call top.getindex %₁ %₂ %₃)
5   (return %₄)

########################################
# multidimensional getindex with end
a[i, end]
#---------------------
1   TestMod.a
2   TestMod.i
3   (call top.lastindex %₁ 2)
4   (call top.getindex %₁ %₂ %₃)
5   (return %₄)

########################################
# multidimensional getindex with begin/end and splats
a[is..., end, js..., begin]
#---------------------
1   TestMod.a
2   TestMod.is
3   (call top.length %₂)
4   (call top.+ 1 %₃)
5   (call top.lastindex %₁ %₄)
6   TestMod.js
7   (call top.length %₂)
8   (call top.length %₆)
9   (call top.+ 2 %₇ %₈)
10  (call top.firstindex %₁ %₉)
11  (call core.tuple %₁)
12  (call core.tuple %₅)
13  (call core.tuple %₁₀)
14  (call core._apply_iterate top.iterate top.getindex %₁₁ %₂ %₁₂ %₆ %₁₃)
15  (return %₁₄)

########################################
# getindex with nontrivial array expression and begin/end
f()[end]
#---------------------
1   TestMod.f
2   (call %₁)
3   (call top.lastindex %₂)
4   (call top.getindex %₂ %₃)
5   (return %₄)

########################################
# nested refs with getindex and begin/end
b[a[begin, end], begin, end]
#---------------------
1   TestMod.b
2   TestMod.a
3   (call top.firstindex %₂ 1)
4   (call top.lastindex %₂ 2)
5   (call top.getindex %₂ %₃ %₄)
6   (call top.firstindex %₁ 2)
7   (call top.lastindex %₁ 3)
8   (call top.getindex %₁ %₅ %₆ %₇)
9   (return %₈)

########################################
# Error: parameters in array ref
a[i, j; w=1]
#---------------------
LoweringError:
a[i, j; w=1]
#     └───┘ ── unexpected semicolon in array expression

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
1   TestMod.a
2   TestMod.x
3   (call top.firstindex %₁)
4   (call top.setindex! %₁ %₂ %₃)
5   (return %₂)

########################################
# simple setindex! with end
a[end] = x
#---------------------
1   TestMod.a
2   TestMod.x
3   (call top.lastindex %₁)
4   (call top.setindex! %₁ %₂ %₃)
5   (return %₂)

########################################
# multidimensional setindex! with begin
a[i, begin] = x
#---------------------
1   TestMod.a
2   TestMod.x
3   TestMod.i
4   (call top.firstindex %₁ 2)
5   (call top.setindex! %₁ %₂ %₃ %₄)
6   (return %₂)

########################################
# multidimensional setindex! with end
a[i, end] = x
#---------------------
1   TestMod.a
2   TestMod.x
3   TestMod.i
4   (call top.lastindex %₁ 2)
5   (call top.setindex! %₁ %₂ %₃ %₄)
6   (return %₂)

########################################
# multidimensional setindex! with begin/end and splats
a[is..., end, js..., begin] = x
#---------------------
1   TestMod.a
2   TestMod.is
3   (call top.length %₂)
4   (call top.+ 1 %₃)
5   (call top.lastindex %₁ %₄)
6   TestMod.js
7   (call top.length %₂)
8   (call top.length %₆)
9   (call top.+ 2 %₇ %₈)
10  (call top.firstindex %₁ %₉)
11  TestMod.x
12  (call core.tuple %₁ %₁₁)
13  (call core.tuple %₅)
14  (call core.tuple %₁₀)
15  (call core._apply_iterate top.iterate top.setindex! %₁₂ %₂ %₁₃ %₆ %₁₄)
16  (return %₁₁)

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
# nested refs
b[a[begin]] = x
#---------------------
1   TestMod.b
2   TestMod.x
3   TestMod.a
4   (call top.firstindex %₃)
5   (call top.getindex %₃ %₄)
6   (call top.setindex! %₁ %₂ %₅)
7   (return %₂)

########################################
# empty ref and setindex!
a[] = rhs
#---------------------
1   TestMod.rhs
2   TestMod.a
3   (call top.setindex! %₂ %₁)
4   (return %₁)
