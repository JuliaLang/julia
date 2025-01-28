########################################
# Simple closure
# (FIXME: #self# should have `read` flag set)
let
    x = 1
    function f(y)
        x + y
    end
end
#---------------------
1   (= slot₂/x (call core.Box))
2   --- thunk
    1   (global TestMod.#f##0)
    2   (call core.svec)
    3   (call core.svec :x)
    4   (call core.svec)
    5   (call core._structtype TestMod :#f##0 %₂ %₃ %₄ false 1)
    6   (call core._setsuper! %₅ core.Function)
    7   (const TestMod.#f##0)
    8   (= TestMod.#f##0 %₅)
    9   (call core.svec core.Box)
    10  (call core._typebody! %₅ %₉)
    11  (return core.nothing)
3   TestMod.#f##0
4   (call core.svec %₃ core.Any)
5   (call core.svec)
6   SourceLocation::3:14
7   (call core.svec %₄ %₅ %₆)
8   --- method core.nothing %₇
    slots: [slot₁/#self#(!read) slot₂/y slot₃/x(!read)]
    1   TestMod.+
    2   (call core.getfield slot₁/#self# :x)
    3   (call core.isdefined %₂ :contents)
    4   (gotoifnot %₃ label₆)
    5   (goto label₈)
    6   (newvar slot₃/x)
    7   slot₃/x
    8   (call core.getfield %₂ :contents)
    9   (call %₁ %₈ slot₂/y)
    10  (return %₉)
9   1
10  slot₂/x
11  (call core.setfield! %₁₀ :contents %₉)
12  TestMod.#f##0
13  slot₂/x
14  (= slot₁/f (new %₁₂ %₁₃))
15  slot₁/f
16  (return %₁₅)

########################################
# Closure declaration with no methods
begin
    local no_method_f
    function no_method_f
    end
end
#---------------------
1   --- thunk
    1   (global TestMod.#no_method_f##0)
    2   (call core.svec)
    3   (call core.svec)
    4   (call core.svec)
    5   (call core._structtype TestMod :#no_method_f##0 %₂ %₃ %₄ false 0)
    6   (call core._setsuper! %₅ core.Function)
    7   (const TestMod.#no_method_f##0)
    8   (= TestMod.#no_method_f##0 %₅)
    9   (call core.svec)
    10  (call core._typebody! %₅ %₉)
    11  (return core.nothing)
2   TestMod.#no_method_f##0
3   (= slot₁/no_method_f (new %₂))
4   slot₁/no_method_f
5   (return %₄)

########################################
# Closure which sets the value of a captured variable
let
    x = 1
    function f(y)
        x = 2
    end
end
#---------------------
1   (= slot₂/x (call core.Box))
2   --- thunk
    1   (global TestMod.#f##1)
    2   (call core.svec)
    3   (call core.svec :x)
    4   (call core.svec)
    5   (call core._structtype TestMod :#f##1 %₂ %₃ %₄ false 1)
    6   (call core._setsuper! %₅ core.Function)
    7   (const TestMod.#f##1)
    8   (= TestMod.#f##1 %₅)
    9   (call core.svec core.Box)
    10  (call core._typebody! %₅ %₉)
    11  (return core.nothing)
3   TestMod.#f##1
4   (call core.svec %₃ core.Any)
5   (call core.svec)
6   SourceLocation::3:14
7   (call core.svec %₄ %₅ %₆)
8   --- method core.nothing %₇
    slots: [slot₁/#self#(!read) slot₂/y(!read)]
    1   2
    2   (call core.getfield slot₁/#self# :x)
    3   (call core.setfield! %₂ :contents %₁)
    4   (return %₁)
9   1
10  slot₂/x
11  (call core.setfield! %₁₀ :contents %₉)
12  TestMod.#f##1
13  slot₂/x
14  (= slot₁/f (new %₁₂ %₁₃))
15  slot₁/f
16  (return %₁₅)

########################################
# Function where arguments are captured into a closure and assigned
function f(x)
    function g()
        x = 10
    end
    g()
    x
end
#---------------------
1   (method TestMod.f)
2   --- thunk
    1   (global TestMod.#f#g##0)
    2   (call core.svec)
    3   (call core.svec :x)
    4   (call core.svec)
    5   (call core._structtype TestMod :#f#g##0 %₂ %₃ %₄ false 1)
    6   (call core._setsuper! %₅ core.Function)
    7   (const TestMod.#f#g##0)
    8   (= TestMod.#f#g##0 %₅)
    9   (call core.svec core.Box)
    10  (call core._typebody! %₅ %₉)
    11  (return core.nothing)
3   TestMod.#f#g##0
4   (call core.svec %₃)
5   (call core.svec)
6   SourceLocation::2:14
7   (call core.svec %₄ %₅ %₆)
8   --- method core.nothing %₇
    slots: [slot₁/#self#(!read)]
    1   10
    2   (call core.getfield slot₁/#self# :x)
    3   (call core.setfield! %₂ :contents %₁)
    4   (return %₁)
9   TestMod.f
10  (call core.Typeof %₉)
11  (call core.svec %₁₀ core.Any)
12  (call core.svec)
13  SourceLocation::1:10
14  (call core.svec %₁₁ %₁₂ %₁₃)
15  --- method core.nothing %₁₄
    slots: [slot₁/#self#(!read) slot₂/x slot₃/g(called) slot₄/x(!read)]
    1   (= slot₂/x (call core.Box slot₂/x))
    2   TestMod.#f#g##0
    3   (= slot₃/g (new %₂ slot₂/x))
    4   slot₃/g
    5   slot₃/g
    6   (call %₅)
    7   slot₂/x
    8   (call core.isdefined %₇ :contents)
    9   (gotoifnot %₈ label₁₁)
    10  (goto label₁₃)
    11  (newvar slot₄/x)
    12  slot₄/x
    13  (call core.getfield %₇ :contents)
    14  (return %₁₃)
16  TestMod.f
17  (return %₁₆)

########################################
# Closure where a local `x` is captured but not boxed
function f(x)
    function g()
        y = x
    end
    z = x
end
#---------------------
1   (method TestMod.f)
2   --- thunk
    1   (global TestMod.#f#g##1)
    2   (call core.TypeVar :x_type)
    3   (call core.svec %₂)
    4   (call core.svec :x)
    5   (call core.svec)
    6   (call core._structtype TestMod :#f#g##1 %₃ %₄ %₅ false 1)
    7   (call core._setsuper! %₆ core.Function)
    8   (const TestMod.#f#g##1)
    9   (= TestMod.#f#g##1 %₆)
    10  (call core.svec %₂)
    11  (call core._typebody! %₆ %₁₀)
    12  (return core.nothing)
3   TestMod.#f#g##1
4   (call core.svec %₃)
5   (call core.svec)
6   SourceLocation::2:14
7   (call core.svec %₄ %₅ %₆)
8   --- method core.nothing %₇
    slots: [slot₁/#self#(!read) slot₂/y(!read)]
    1   (call core.getfield slot₁/#self# :x)
    2   (= slot₂/y %₁)
    3   (return %₁)
9   TestMod.f
10  (call core.Typeof %₉)
11  (call core.svec %₁₀ core.Any)
12  (call core.svec)
13  SourceLocation::1:10
14  (call core.svec %₁₁ %₁₂ %₁₃)
15  --- method core.nothing %₁₄
    slots: [slot₁/#self#(!read) slot₂/x slot₃/g slot₄/z(!read)]
    1   TestMod.#f#g##1
    2   (call core.typeof slot₂/x)
    3   (call core.apply_type %₁ %₂)
    4   (= slot₃/g (new %₃ slot₂/x))
    5   slot₃/g
    6   slot₂/x
    7   (= slot₄/z %₆)
    8   (return %₆)
16  TestMod.f
17  (return %₁₆)

########################################
# Closure where a static parameter of an outer function is captured
function f(::T) where T
    function g()
        use(T)
    end
end
#---------------------
1   (method TestMod.f)
2   --- thunk
    1   (global TestMod.#f#g##2)
    2   (call core.TypeVar :T_type)
    3   (call core.svec %₂)
    4   (call core.svec :T)
    5   (call core.svec)
    6   (call core._structtype TestMod :#f#g##2 %₃ %₄ %₅ false 1)
    7   (call core._setsuper! %₆ core.Function)
    8   (const TestMod.#f#g##2)
    9   (= TestMod.#f#g##2 %₆)
    10  (call core.svec %₂)
    11  (call core._typebody! %₆ %₁₀)
    12  (return core.nothing)
3   TestMod.#f#g##2
4   (call core.svec %₃)
5   (call core.svec)
6   SourceLocation::2:14
7   (call core.svec %₄ %₅ %₆)
8   --- method core.nothing %₇
    slots: [slot₁/#self#(!read)]
    1   TestMod.use
    2   (call core.getfield slot₁/#self# :T)
    3   (call %₁ %₂)
    4   (return %₃)
9   (= slot₁/T (call core.TypeVar :T))
10  TestMod.f
11  (call core.Typeof %₁₀)
12  slot₁/T
13  (call core.svec %₁₁ %₁₂)
14  slot₁/T
15  (call core.svec %₁₄)
16  SourceLocation::1:10
17  (call core.svec %₁₃ %₁₅ %₁₆)
18  --- method core.nothing %₁₇
    slots: [slot₁/#self#(!read) slot₂/_(!read) slot₃/g]
    1   TestMod.#f#g##2
    2   static_parameter₁
    3   (call core.typeof %₂)
    4   (call core.apply_type %₁ %₃)
    5   static_parameter₁
    6   (= slot₃/g (new %₄ %₅))
    7   slot₃/g
    8   (return %₇)
19  TestMod.f
20  (return %₁₉)

########################################
# Closure captures with `isdefined`
function f(x)
    function g()
        z = 3
        (@isdefined(x), # unboxed, always defined capture
         @isdefined(y), # boxed capture
         @isdefined(z)) # normal local var
    end
    y = 2
    (@isdefined(y), # boxed local
     @isdefined(x)) # always defined local (function arg)
end
#---------------------
1   (method TestMod.f)
2   --- thunk
    1   (global TestMod.#f#g##3)
    2   (call core.TypeVar :x_type)
    3   (call core.svec %₂)
    4   (call core.svec :x :y)
    5   (call core.svec)
    6   (call core._structtype TestMod :#f#g##3 %₃ %₄ %₅ false 2)
    7   (call core._setsuper! %₆ core.Function)
    8   (const TestMod.#f#g##3)
    9   (= TestMod.#f#g##3 %₆)
    10  (call core.svec %₂ core.Box)
    11  (call core._typebody! %₆ %₁₀)
    12  (return core.nothing)
3   TestMod.#f#g##3
4   (call core.svec %₃)
5   (call core.svec)
6   SourceLocation::2:14
7   (call core.svec %₄ %₅ %₆)
8   --- method core.nothing %₇
    slots: [slot₁/#self#(!read) slot₂/z]
    1   (= slot₂/z 3)
    2   (call core.getfield slot₁/#self# :y)
    3   (call core.isdefined %₂ :contents)
    4   (isdefined slot₂/z)
    5   (call core.tuple true %₃ %₄)
    6   (return %₅)
9   TestMod.f
10  (call core.Typeof %₉)
11  (call core.svec %₁₀ core.Any)
12  (call core.svec)
13  SourceLocation::1:10
14  (call core.svec %₁₁ %₁₂ %₁₃)
15  --- method core.nothing %₁₄
    slots: [slot₁/#self#(!read) slot₂/x slot₃/g slot₄/y]
    1   (= slot₄/y (call core.Box))
    2   TestMod.#f#g##3
    3   (call core.typeof slot₂/x)
    4   (call core.apply_type %₂ %₃)
    5   slot₄/y
    6   (= slot₃/g (new %₄ slot₂/x %₅))
    7   slot₃/g
    8   2
    9   slot₄/y
    10  (call core.setfield! %₉ :contents %₈)
    11  slot₄/y
    12  (call core.isdefined %₁₁ :contents)
    13  (call core.tuple %₁₂ true)
    14  (return %₁₃)
16  TestMod.f
17  (return %₁₆)

########################################
# FIXME: Nested captures of arguments
function f(x)
    function g(y)
        function h(z)
            (x,y,z)
        end
    end
end
#---------------------
LoweringError:
function f(x)
#          ╙ ── Found unexpected binding of kind argument
    function g(y)
        function h(z)

Detailed provenance:
#₈/x
└─ x
   └─ x
      └─ @ :1


########################################
# Global method capturing local variables
begin
    local x = 1
    function f()
        x = x + 1
    end
end
#---------------------
1   (= slot₁/x (call core.Box))
2   1
3   slot₁/x
4   (call core.setfield! %₃ :contents %₂)
5   (method TestMod.f)
6   TestMod.f
7   (call core.Typeof %₆)
8   (call core.svec %₇)
9   (call core.svec)
10  SourceLocation::3:14
11  (call core.svec %₈ %₉ %₁₀)
12  --- code_info
    slots: [slot₁/#self#(!read) slot₂/x(!read)]
    1   TestMod.+
    2   (captured_local 1)
    3   (call core.isdefined %₂ :contents)
    4   (gotoifnot %₃ label₆)
    5   (goto label₈)
    6   (newvar slot₂/x)
    7   slot₂/x
    8   (call core.getfield %₂ :contents)
    9   (call %₁ %₈ 1)
    10  (captured_local 1)
    11  (call core.setfield! %₁₀ :contents %₉)
    12  (return %₉)
13  slot₁/x
14  (call core.svec %₁₃)
15  (call JuliaLowering.replace_captured_locals! %₁₂ %₁₄)
16  --- method core.nothing %₁₁ %₁₅
17  TestMod.f
18  (return %₁₇)

########################################
# Anonymous function syntax with ->
x -> x*x
#---------------------
1   --- thunk
    1   (global TestMod.#->##0)
    2   (call core.svec)
    3   (call core.svec)
    4   (call core.svec)
    5   (call core._structtype TestMod :#->##0 %₂ %₃ %₄ false 0)
    6   (call core._setsuper! %₅ core.Function)
    7   (const TestMod.#->##0)
    8   (= TestMod.#->##0 %₅)
    9   (call core.svec)
    10  (call core._typebody! %₅ %₉)
    11  (return core.nothing)
2   TestMod.#->##0
3   (new %₂)
4   TestMod.#->##0
5   (call core.svec %₄ core.Any)
6   (call core.svec)
7   SourceLocation::1:1
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(!read) slot₂/x]
    1   TestMod.*
    2   (call %₁ slot₂/x slot₂/x)
    3   (return %₂)
10  (return %₃)

########################################
# Anonymous function syntax with `function`
function (x)
    x*x
end
#---------------------
1   --- thunk
    1   (global TestMod.##anon###0)
    2   (call core.svec)
    3   (call core.svec)
    4   (call core.svec)
    5   (call core._structtype TestMod :##anon###0 %₂ %₃ %₄ false 0)
    6   (call core._setsuper! %₅ core.Function)
    7   (const TestMod.##anon###0)
    8   (= TestMod.##anon###0 %₅)
    9   (call core.svec)
    10  (call core._typebody! %₅ %₉)
    11  (return core.nothing)
2   TestMod.##anon###0
3   (new %₂)
4   TestMod.##anon###0
5   (call core.svec %₄ core.Any)
6   (call core.svec)
7   SourceLocation::1:10
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(!read) slot₂/x]
    1   TestMod.*
    2   (call %₁ slot₂/x slot₂/x)
    3   (return %₂)
10  (return %₃)

########################################
# `do` blocks
f(x; a=1) do y
    y + 2
end
#---------------------
1   TestMod.f
2   (call core.tuple :a)
3   (call core.apply_type core.NamedTuple %₂)
4   (call core.tuple 1)
5   (call %₃ %₄)
6   --- thunk
    1   (global TestMod.#do##0)
    2   (call core.svec)
    3   (call core.svec)
    4   (call core.svec)
    5   (call core._structtype TestMod :#do##0 %₂ %₃ %₄ false 0)
    6   (call core._setsuper! %₅ core.Function)
    7   (const TestMod.#do##0)
    8   (= TestMod.#do##0 %₅)
    9   (call core.svec)
    10  (call core._typebody! %₅ %₉)
    11  (return core.nothing)
7   TestMod.#do##0
8   (call core.svec %₇ core.Any)
9   (call core.svec)
10  SourceLocation::1:13
11  (call core.svec %₈ %₉ %₁₀)
12  --- method core.nothing %₁₁
    slots: [slot₁/#self#(!read) slot₂/y]
    1   TestMod.+
    2   (call %₁ slot₂/y 2)
    3   (return %₂)
13  TestMod.#do##0
14  (new %₁₃)
15  TestMod.x
16  (call core.kwcall %₅ %₁ %₁₄ %₁₅)
17  (return %₁₆)

########################################
# Error: Attempt to add methods to a function argument
function f(g)
    function g()
    end
end
#---------------------
LoweringError:
function f(g)
    function g()
#            ╙ ── Cannot add method to a function argument
    end
end

########################################
# Error: Static parameter clashing with closure name
function f() where {g}
    function g()
    end
end
#---------------------
LoweringError:
function f() where {g}
    function g()
#            ╙ ── local variable name `g` conflicts with a static parameter
    end
end

########################################
# Opaque closure
let y = 1
    Base.Experimental.@opaque (x, z::T)->2x + y - z
end
#---------------------
1   1
2   (= slot₁/y (call core.Box))
3   slot₁/y
4   (call core.setfield! %₃ :contents %₁)
5   TestMod.T
6   (call core.apply_type core.Tuple core.Any %₅)
7   (call core.apply_type core.Union)
8   --- opaque_closure_method  core.nothing 2 false SourceLocation::2:31
    slots: [slot₁/#self#(!read) slot₂/x slot₃/z slot₄/y(!read)]
    1   TestMod.-
    2   TestMod.+
    3   TestMod.*
    4   (call %₃ 2 slot₂/x)
    5   (call core.getfield slot₁/#self# 1)
    6   (call core.isdefined %₅ :contents)
    7   (gotoifnot %₆ label₉)
    8   (goto label₁₁)
    9   (newvar slot₄/y)
    10  slot₄/y
    11  (call core.getfield %₅ :contents)
    12  (call %₂ %₄ %₁₁)
    13  (call %₁ %₁₂ slot₃/z)
    14  (return %₁₃)
9   slot₁/y
10  (new_opaque_closure %₆ %₇ core.Any true %₈ %₉)
11  (return %₁₀)

########################################
# Opaque closure with `...`
let
    Base.Experimental.@opaque (x, ys...)->ys
end
#---------------------
1   (call core.apply_type core.Vararg core.Any)
2   (call core.apply_type core.Tuple core.Any %₁)
3   (call core.apply_type core.Union)
4   --- opaque_closure_method  core.nothing 2 true SourceLocation::2:31
    slots: [slot₁/#self#(!read) slot₂/x(!read) slot₃/ys]
    1   slot₃/ys
    2   (return %₁)
5   (new_opaque_closure %₂ %₃ core.Any true %₄)
6   (return %₅)

########################################
# Error: Opaque closure with default args
Base.Experimental.@opaque (x=1)->2x
#---------------------
LoweringError:
Base.Experimental.@opaque (x=1)->2x
#                            ╙ ── Default positional arguments cannot be used in an opaque closure

