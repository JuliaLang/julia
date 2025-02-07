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
2   1
3   slot₂/x
4   (call core.setfield! %₃ :contents %₂)
5   (call core.svec :x)
6   (call core.svec true)
7   (call JuliaLowering.eval_closure_type TestMod :#f##0 %₅ %₆)
8   TestMod.#f##0
9   slot₂/x
10  (new %₈ %₉)
11  (= slot₁/f %₁₀)
12  TestMod.#f##0
13  (call core.svec %₁₂ core.Any)
14  (call core.svec)
15  SourceLocation::3:14
16  (call core.svec %₁₃ %₁₄ %₁₅)
17  --- method core.nothing %₁₆
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
18  slot₁/f
19  (return %₁₈)

########################################
# Closure declaration with no methods
begin
    local no_method_f
    function no_method_f
    end
end
#---------------------
1   (call core.svec)
2   (call core.svec)
3   (call JuliaLowering.eval_closure_type TestMod :#no_method_f##0 %₁ %₂)
4   TestMod.#no_method_f##0
5   (new %₄)
6   (= slot₁/no_method_f %₅)
7   slot₁/no_method_f
8   (return %₇)

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
2   1
3   slot₂/x
4   (call core.setfield! %₃ :contents %₂)
5   (call core.svec :x)
6   (call core.svec true)
7   (call JuliaLowering.eval_closure_type TestMod :#f##1 %₅ %₆)
8   TestMod.#f##1
9   slot₂/x
10  (new %₈ %₉)
11  (= slot₁/f %₁₀)
12  TestMod.#f##1
13  (call core.svec %₁₂ core.Any)
14  (call core.svec)
15  SourceLocation::3:14
16  (call core.svec %₁₃ %₁₄ %₁₅)
17  --- method core.nothing %₁₆
    slots: [slot₁/#self#(!read) slot₂/y(!read)]
    1   2
    2   (call core.getfield slot₁/#self# :x)
    3   (call core.setfield! %₂ :contents %₁)
    4   (return %₁)
18  slot₁/f
19  (return %₁₈)

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
2   (call core.svec :x)
3   (call core.svec true)
4   (call JuliaLowering.eval_closure_type TestMod :#f#g##0 %₂ %₃)
5   TestMod.#f#g##0
6   (call core.svec %₅)
7   (call core.svec)
8   SourceLocation::2:14
9   (call core.svec %₆ %₇ %₈)
10  --- method core.nothing %₉
    slots: [slot₁/#self#(!read)]
    1   10
    2   (call core.getfield slot₁/#self# :x)
    3   (call core.setfield! %₂ :contents %₁)
    4   (return %₁)
11  TestMod.f
12  (call core.Typeof %₁₁)
13  (call core.svec %₁₂ core.Any)
14  (call core.svec)
15  SourceLocation::1:10
16  (call core.svec %₁₃ %₁₄ %₁₅)
17  --- method core.nothing %₁₆
    slots: [slot₁/#self#(!read) slot₂/x slot₃/g(called) slot₄/x(!read)]
    1   (= slot₂/x (call core.Box slot₂/x))
    2   TestMod.#f#g##0
    3   (new %₂ slot₂/x)
    4   (= slot₃/g %₃)
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
18  TestMod.f
19  (return %₁₈)

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
2   (call core.svec :x)
3   (call core.svec false)
4   (call JuliaLowering.eval_closure_type TestMod :#f#g##1 %₂ %₃)
5   TestMod.#f#g##1
6   (call core.svec %₅)
7   (call core.svec)
8   SourceLocation::2:14
9   (call core.svec %₆ %₇ %₈)
10  --- method core.nothing %₉
    slots: [slot₁/#self#(!read) slot₂/y(!read)]
    1   (call core.getfield slot₁/#self# :x)
    2   (= slot₂/y %₁)
    3   (return %₁)
11  TestMod.f
12  (call core.Typeof %₁₁)
13  (call core.svec %₁₂ core.Any)
14  (call core.svec)
15  SourceLocation::1:10
16  (call core.svec %₁₃ %₁₄ %₁₅)
17  --- method core.nothing %₁₆
    slots: [slot₁/#self#(!read) slot₂/x slot₃/g slot₄/z(!read)]
    1   TestMod.#f#g##1
    2   (call core.typeof slot₂/x)
    3   (call core.apply_type %₁ %₂)
    4   (new %₃ slot₂/x)
    5   (= slot₃/g %₄)
    6   slot₂/x
    7   (= slot₄/z %₆)
    8   (return %₆)
18  TestMod.f
19  (return %₁₈)

########################################
# Closure where a static parameter of an outer function is captured
function f(::T) where T
    function g()
        use(T)
    end
end
#---------------------
1   (method TestMod.f)
2   (call core.svec :T)
3   (call core.svec false)
4   (call JuliaLowering.eval_closure_type TestMod :#f#g##2 %₂ %₃)
5   TestMod.#f#g##2
6   (call core.svec %₅)
7   (call core.svec)
8   SourceLocation::2:14
9   (call core.svec %₆ %₇ %₈)
10  --- method core.nothing %₉
    slots: [slot₁/#self#(!read)]
    1   TestMod.use
    2   (call core.getfield slot₁/#self# :T)
    3   (call %₁ %₂)
    4   (return %₃)
11  (= slot₁/T (call core.TypeVar :T))
12  TestMod.f
13  (call core.Typeof %₁₂)
14  slot₁/T
15  (call core.svec %₁₃ %₁₄)
16  slot₁/T
17  (call core.svec %₁₆)
18  SourceLocation::1:10
19  (call core.svec %₁₅ %₁₇ %₁₈)
20  --- method core.nothing %₁₉
    slots: [slot₁/#self#(!read) slot₂/_(!read) slot₃/g]
    1   TestMod.#f#g##2
    2   static_parameter₁
    3   (call core.typeof %₂)
    4   (call core.apply_type %₁ %₃)
    5   static_parameter₁
    6   (new %₄ %₅)
    7   (= slot₃/g %₆)
    8   slot₃/g
    9   (return %₈)
21  TestMod.f
22  (return %₂₁)

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
2   (call core.svec :x :y)
3   (call core.svec false true)
4   (call JuliaLowering.eval_closure_type TestMod :#f#g##3 %₂ %₃)
5   TestMod.#f#g##3
6   (call core.svec %₅)
7   (call core.svec)
8   SourceLocation::2:14
9   (call core.svec %₆ %₇ %₈)
10  --- method core.nothing %₉
    slots: [slot₁/#self#(!read) slot₂/z]
    1   (= slot₂/z 3)
    2   (call core.getfield slot₁/#self# :y)
    3   (call core.isdefined %₂ :contents)
    4   (isdefined slot₂/z)
    5   (call core.tuple true %₃ %₄)
    6   (return %₅)
11  TestMod.f
12  (call core.Typeof %₁₁)
13  (call core.svec %₁₂ core.Any)
14  (call core.svec)
15  SourceLocation::1:10
16  (call core.svec %₁₃ %₁₄ %₁₅)
17  --- method core.nothing %₁₆
    slots: [slot₁/#self#(!read) slot₂/x slot₃/g slot₄/y]
    1   (= slot₄/y (call core.Box))
    2   TestMod.#f#g##3
    3   (call core.typeof slot₂/x)
    4   (call core.apply_type %₂ %₃)
    5   slot₄/y
    6   (new %₄ slot₂/x %₅)
    7   (= slot₃/g %₆)
    8   2
    9   slot₄/y
    10  (call core.setfield! %₉ :contents %₈)
    11  slot₄/y
    12  (call core.isdefined %₁₁ :contents)
    13  (call core.tuple %₁₂ true)
    14  (return %₁₃)
18  TestMod.f
19  (return %₁₈)

########################################
# Nested captures - here `g` captures `x` because it is needed to initialize
# the closure `h` which captures both `x` and `y`.
# [method_filter: #f_nest#g_nest##0]
function f_nest(x)
    function g_nest(y)
        function h_nest(z)
            (x,y,z)
        end
    end
end
#---------------------
slots: [slot₁/#self#(!read) slot₂/y(!read) slot₃/h_nest]
1   TestMod.#f_nest#g_nest#h_nest##0
2   (call core.getfield slot₁/#self# :x)
3   (call core.typeof %₂)
4   (call core.typeof slot₂/y)
5   (call core.apply_type %₁ %₃ %₄)
6   (call core.getfield slot₁/#self# :x)
7   (new %₅ %₆ slot₂/y)
8   (= slot₃/h_nest %₇)
9   slot₃/h_nest
10  (return %₉)

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
1   (call core.svec)
2   (call core.svec)
3   (call JuliaLowering.eval_closure_type TestMod :#->##0 %₁ %₂)
4   TestMod.#->##0
5   (new %₄)
6   TestMod.#->##0
7   (call core.svec %₆ core.Any)
8   (call core.svec)
9   SourceLocation::1:1
10  (call core.svec %₇ %₈ %₉)
11  --- method core.nothing %₁₀
    slots: [slot₁/#self#(!read) slot₂/x]
    1   TestMod.*
    2   (call %₁ slot₂/x slot₂/x)
    3   (return %₂)
12  (return %₅)

########################################
# Anonymous function syntax with `function`
function (x)
    x*x
end
#---------------------
1   (call core.svec)
2   (call core.svec)
3   (call JuliaLowering.eval_closure_type TestMod :##anon###0 %₁ %₂)
4   TestMod.##anon###0
5   (new %₄)
6   TestMod.##anon###0
7   (call core.svec %₆ core.Any)
8   (call core.svec)
9   SourceLocation::1:10
10  (call core.svec %₇ %₈ %₉)
11  --- method core.nothing %₁₀
    slots: [slot₁/#self#(!read) slot₂/x]
    1   TestMod.*
    2   (call %₁ slot₂/x slot₂/x)
    3   (return %₂)
12  (return %₅)

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
6   (call core.svec)
7   (call core.svec)
8   (call JuliaLowering.eval_closure_type TestMod :#do##0 %₆ %₇)
9   TestMod.#do##0
10  (call core.svec %₉ core.Any)
11  (call core.svec)
12  SourceLocation::1:13
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read) slot₂/y]
    1   TestMod.+
    2   (call %₁ slot₂/y 2)
    3   (return %₂)
15  TestMod.#do##0
16  (new %₁₅)
17  TestMod.x
18  (call core.kwcall %₅ %₁ %₁₆ %₁₇)
19  (return %₁₈)

########################################
# Error: Static parameter clashing with closure name
function f(::g) where {g}
    function g()
    end
end
#---------------------
LoweringError:
function f(::g) where {g}
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

########################################
# Mutually recursive closures
let
    function recursive_a()
        recursive_b()
    end
    function recursive_b()
        recursive_a()
    end
end
#---------------------
1   (= slot₁/recursive_a (call core.Box))
2   (= slot₂/recursive_b (call core.Box))
3   (call core.svec :recursive_b)
4   (call core.svec true)
5   (call JuliaLowering.eval_closure_type TestMod :#recursive_a##0 %₃ %₄)
6   TestMod.#recursive_a##0
7   slot₂/recursive_b
8   (new %₆ %₇)
9   slot₁/recursive_a
10  (call core.setfield! %₉ :contents %₈)
11  TestMod.#recursive_a##0
12  (call core.svec %₁₁)
13  (call core.svec)
14  SourceLocation::2:14
15  (call core.svec %₁₂ %₁₃ %₁₄)
16  --- method core.nothing %₁₅
    slots: [slot₁/#self#(!read) slot₂/recursive_b(!read)]
    1   (call core.getfield slot₁/#self# :recursive_b)
    2   (call core.isdefined %₁ :contents)
    3   (gotoifnot %₂ label₅)
    4   (goto label₇)
    5   (newvar slot₂/recursive_b)
    6   slot₂/recursive_b
    7   (call core.getfield %₁ :contents)
    8   (call %₇)
    9   (return %₈)
17  (call core.svec :recursive_a)
18  (call core.svec true)
19  (call JuliaLowering.eval_closure_type TestMod :#recursive_b##0 %₁₇ %₁₈)
20  TestMod.#recursive_b##0
21  slot₁/recursive_a
22  (new %₂₀ %₂₁)
23  slot₂/recursive_b
24  (call core.setfield! %₂₃ :contents %₂₂)
25  TestMod.#recursive_b##0
26  (call core.svec %₂₅)
27  (call core.svec)
28  SourceLocation::5:14
29  (call core.svec %₂₆ %₂₇ %₂₈)
30  --- method core.nothing %₂₉
    slots: [slot₁/#self#(!read) slot₂/recursive_a(!read)]
    1   (call core.getfield slot₁/#self# :recursive_a)
    2   (call core.isdefined %₁ :contents)
    3   (gotoifnot %₂ label₅)
    4   (goto label₇)
    5   (newvar slot₂/recursive_a)
    6   slot₂/recursive_a
    7   (call core.getfield %₁ :contents)
    8   (call %₇)
    9   (return %₈)
31  slot₂/recursive_b
32  (call core.isdefined %₃₁ :contents)
33  (gotoifnot %₃₂ label₃₅)
34  (goto label₃₇)
35  (newvar slot₄/recursive_b)
36  slot₄/recursive_b
37  (call core.getfield %₃₁ :contents)
38  (return %₃₇)

########################################
# Closure with keywords
let y = y_init
    function f_kw_closure(; x::X=x_default)
        x + y
    end
end
#---------------------
1   TestMod.y_init
2   (= slot₁/y (call core.Box))
3   (= slot₂/#f_kw_closure#0 (call core.Box))
4   slot₁/y
5   (call core.setfield! %₄ :contents %₁)
6   (call core.svec :#f_kw_closure#0)
7   (call core.svec true)
8   (call JuliaLowering.eval_closure_type TestMod :#f_kw_closure##0 %₆ %₇)
9   TestMod.#f_kw_closure##0
10  slot₂/#f_kw_closure#0
11  (new %₉ %₁₀)
12  (= slot₃/f_kw_closure %₁₁)
13  (call core.svec :y)
14  (call core.svec true)
15  (call JuliaLowering.eval_closure_type TestMod :##f_kw_closure#0##0 %₁₃ %₁₄)
16  TestMod.##f_kw_closure#0##0
17  slot₁/y
18  (new %₁₆ %₁₇)
19  slot₂/#f_kw_closure#0
20  (call core.setfield! %₁₉ :contents %₁₈)
21  TestMod.##f_kw_closure#0##0
22  TestMod.X
23  TestMod.#f_kw_closure##0
24  (call core.svec %₂₁ %₂₂ %₂₃)
25  (call core.svec)
26  SourceLocation::2:14
27  (call core.svec %₂₄ %₂₅ %₂₆)
28  --- method core.nothing %₂₇
    slots: [slot₁/#self#(!read) slot₂/x slot₃/#self#(!read) slot₄/y(!read)]
    1   (meta :nkw 1)
    2   TestMod.+
    3   (call core.getfield slot₁/#self# :y)
    4   (call core.isdefined %₃ :contents)
    5   (gotoifnot %₄ label₇)
    6   (goto label₉)
    7   (newvar slot₄/y)
    8   slot₄/y
    9   (call core.getfield %₃ :contents)
    10  (call %₂ slot₂/x %₉)
    11  (return %₁₀)
29  (call core.typeof core.kwcall)
30  TestMod.#f_kw_closure##0
31  (call core.svec %₂₉ core.NamedTuple %₃₀)
32  (call core.svec)
33  SourceLocation::2:14
34  (call core.svec %₃₁ %₃₂ %₃₃)
35  --- code_info
    slots: [slot₁/#self#(!read) slot₂/kws slot₃/#self# slot₄/kwtmp slot₅/x(!read) slot₆/#f_kw_closure#0(!read)]
    1   (newvar slot₅/x)
    2   (call core.isdefined slot₂/kws :x)
    3   (gotoifnot %₂ label₁₄)
    4   (call core.getfield slot₂/kws :x)
    5   TestMod.X
    6   (call core.isa %₄ %₅)
    7   (gotoifnot %₆ label₉)
    8   (goto label₁₂)
    9   TestMod.X
    10  (new core.TypeError :keyword argument :x %₉ %₄)
    11  (call core.throw %₁₀)
    12  (= slot₄/kwtmp %₄)
    13  (goto label₁₆)
    14  TestMod.x_default
    15  (= slot₄/kwtmp %₁₄)
    16  slot₄/kwtmp
    17  (call top.keys slot₂/kws)
    18  (call core.tuple :x)
    19  (call top.diff_names %₁₇ %₁₈)
    20  (call top.isempty %₁₉)
    21  (gotoifnot %₂₀ label₂₃)
    22  (goto label₂₄)
    23  (call top.kwerr slot₂/kws slot₃/#self#)
    24  (captured_local 1)
    25  (call core.isdefined %₂₄ :contents)
    26  (gotoifnot %₂₅ label₂₈)
    27  (goto label₃₀)
    28  (newvar slot₆/#f_kw_closure#0)
    29  slot₆/#f_kw_closure#0
    30  (call core.getfield %₂₄ :contents)
    31  (call %₃₀ %₁₆ slot₃/#self#)
    32  (return %₃₁)
36  slot₂/#f_kw_closure#0
37  (call core.svec %₃₆)
38  (call JuliaLowering.replace_captured_locals! %₃₅ %₃₇)
39  --- method core.nothing %₃₄ %₃₈
40  TestMod.#f_kw_closure##0
41  (call core.svec %₄₀)
42  (call core.svec)
43  SourceLocation::2:14
44  (call core.svec %₄₁ %₄₂ %₄₃)
45  --- method core.nothing %₄₄
    slots: [slot₁/#self# slot₂/#f_kw_closure#0(!read)]
    1   (call core.getfield slot₁/#self# :#f_kw_closure#0)
    2   (call core.isdefined %₁ :contents)
    3   (gotoifnot %₂ label₅)
    4   (goto label₇)
    5   (newvar slot₂/#f_kw_closure#0)
    6   slot₂/#f_kw_closure#0
    7   (call core.getfield %₁ :contents)
    8   TestMod.x_default
    9   (call %₇ %₈ slot₁/#self#)
    10  (return %₉)
46  slot₃/f_kw_closure
47  (return %₄₆)

########################################
# Error: Closure outside any top level context
# (Should only happen in a user-visible way when lowering code emitted
#  from a `@generated` function code generator.)
@ast_ [K"lambda"(is_toplevel_thunk=false)
    [K"block"]
    [K"block"]
    [K"->" [K"tuple"] [K"block"]]
]
#---------------------
LoweringError:
#= line 1 =# - Top level code was found outside any top level context. `@generated` functions may not contain closures, including `do` syntax and generators/comprehension

