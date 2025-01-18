########################################
# Getproperty syntax
x.a
#---------------------
1   TestMod.x
2   (call top.getproperty %₁ :a)
3   (return %₂)

########################################
# Getproperty syntax with a string on right hand side
x."b"
#---------------------
1   TestMod.x
2   (call top.getproperty %₁ "b")
3   (return %₂)

########################################
# Standalone dot syntax
.*
#---------------------
1   TestMod.*
2   (call top.BroadcastFunction %₁)
3   (return %₂)

########################################
# Error: Wrong number of children in `.`
@ast_ [K"." "x"::K"Identifier" "a"::K"Identifier" 3::K"Integer"]
#---------------------
LoweringError:
#= line 1 =# - `.` form requires either one or two children

########################################
# Error: Placeholder value used
_ + 1
#---------------------
LoweringError:
_ + 1
╙ ── all-underscore identifiers are write-only and their values cannot be used in expressions

########################################
# Named tuple
(a=1, b=2)
#---------------------
1   (call core.tuple :a :b)
2   (call core.apply_type core.NamedTuple %₁)
3   (call core.tuple 1 2)
4   (call %₂ %₃)
5   (return %₄)

########################################
# Named tuple with parameters
(; a=1, b=2)
#---------------------
1   (call core.tuple :a :b)
2   (call core.apply_type core.NamedTuple %₁)
3   (call core.tuple 1 2)
4   (call %₂ %₃)
5   (return %₄)

########################################
# Empty named tuple
(;)
#---------------------
1   (call core.NamedTuple)
2   (return %₁)

########################################
# Named tuple with implicit field names
(;x, a.b.c, y._)
#---------------------
1   (call core.tuple :x :c :_)
2   (call core.apply_type core.NamedTuple %₁)
3   TestMod.x
4   TestMod.a
5   (call top.getproperty %₄ :b)
6   (call top.getproperty %₅ :c)
7   TestMod.y
8   (call top.getproperty %₇ :_)
9   (call core.tuple %₃ %₆ %₈)
10  (call %₂ %₉)
11  (return %₁₀)

########################################
# Named tuple with splats
(; a=1, b=2, bs..., c=3, ds...)
#---------------------
1   (call core.tuple :a :b)
2   (call core.apply_type core.NamedTuple %₁)
3   (call core.tuple 1 2)
4   (call %₂ %₃)
5   TestMod.bs
6   (call top.merge %₄ %₅)
7   (call core.tuple :c)
8   (call core.apply_type core.NamedTuple %₇)
9   (call core.tuple 3)
10  (call %₈ %₉)
11  (call top.merge %₆ %₁₀)
12  TestMod.ds
13  (call top.merge %₁₁ %₁₂)
14  (return %₁₃)

########################################
# Named tuple with only splats
(; as..., bs...)
#---------------------
1   (call core.NamedTuple)
2   TestMod.as
3   (call top.merge %₁ %₂)
4   TestMod.bs
5   (call top.merge %₃ %₄)
6   (return %₅)

########################################
# Named tuple with dynamic names
(; a=1, b=2, c=>d)
#---------------------
1   (call core.tuple :a :b)
2   (call core.apply_type core.NamedTuple %₁)
3   (call core.tuple 1 2)
4   (call %₂ %₃)
5   TestMod.c
6   (call core.tuple %₅)
7   (call core.apply_type core.NamedTuple %₆)
8   TestMod.d
9   (call core.tuple %₈)
10  (call %₇ %₉)
11  (call top.merge %₄ %₁₀)
12  (return %₁₁)

########################################
# Error: Named tuple with repeated fields
(; a=1, bs..., c=3, a=2)
#---------------------
LoweringError:
(; a=1, bs..., c=3, a=2)
#                   ╙ ── Repeated named tuple field name

########################################
# Error: Named tuple frankentuple
(a=1; b=2, c=3)
#---------------------
LoweringError:
(a=1; b=2, c=3)
#   └────────┘ ── unexpected semicolon in tuple - use `,` to separate tuple elements

########################################
# Error: Named tuple field dots in rhs
(; a=xs...)
#---------------------
LoweringError:
(; a=xs...)
#    └───┘ ── `...` cannot be used in a value for a named tuple field

########################################
# Error: Named tuple field invalid lhs
(; a[]=1)
#---------------------
LoweringError:
(; a[]=1)
#  └─┘ ── invalid named tuple field name

########################################
# Error: Named tuple element with weird dot syntax
(; a."b")
#---------------------
LoweringError:
(; a."b")
#  └───┘ ── invalid named tuple element

########################################
# Error: Named tuple element without valid name
(; a=1, f())
#---------------------
LoweringError:
(; a=1, f())
#       └─┘ ── Invalid named tuple element

########################################
# Error: Modules not allowed in local scope
let
    module C
    end
end
#---------------------
LoweringError:
let
#   ┌───────
    module C
    end
#─────┘ ── module is only allowed in global scope
end

########################################
# Error: Modules not allowed in local scope
function f()
    module C
    end
end
#---------------------
LoweringError:
function f()
#   ┌───────
    module C
    end
#─────┘ ── module is only allowed in global scope
end

########################################
# Basic type assert
x::T
#---------------------
1   TestMod.x
2   TestMod.T
3   (call core.typeassert %₁ %₂)
4   (return %₃)

########################################
# Error: Invalid :: syntax outside function arg list
::T
#---------------------
LoweringError:
::T
└─┘ ── `::` must be written `value::type` outside function argument lists

########################################
# Error: braces vector syntax
{x, y}
#---------------------
LoweringError:
{x, y}
└────┘ ── { } syntax is reserved for future use

########################################
# Error: braces matrix syntax
{x y; y z}
#---------------------
LoweringError:
{x y; y z}
└────────┘ ── { } syntax is reserved for future use

########################################
# Error: Test AST which has no source form and thus must have been constructed
# programmatically (eg, a malformed if)
@ast_ [K"if"]
#---------------------
LoweringError:
#= line 1 =# - expected `numchildren(ex) >= 2`

