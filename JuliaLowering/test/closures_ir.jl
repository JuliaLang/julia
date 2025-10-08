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
8   latestworld
9   TestMod.#f##0
10  slot₂/x
11  (new %₉ %₁₀)
12  (= slot₁/f %₁₁)
13  TestMod.#f##0
14  (call core.svec %₁₃ core.Any)
15  (call core.svec)
16  SourceLocation::3:14
17  (call core.svec %₁₄ %₁₅ %₁₆)
18  --- method core.nothing %₁₇
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
19  latestworld
20  slot₁/f
21  (return %₂₀)

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
4   latestworld
5   TestMod.#no_method_f##0
6   (new %₅)
7   (= slot₁/no_method_f %₆)
8   slot₁/no_method_f
9   (return %₈)

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
8   latestworld
9   TestMod.#f##1
10  slot₂/x
11  (new %₉ %₁₀)
12  (= slot₁/f %₁₁)
13  TestMod.#f##1
14  (call core.svec %₁₃ core.Any)
15  (call core.svec)
16  SourceLocation::3:14
17  (call core.svec %₁₄ %₁₅ %₁₆)
18  --- method core.nothing %₁₇
    slots: [slot₁/#self#(!read) slot₂/y(!read)]
    1   2
    2   (call core.getfield slot₁/#self# :x)
    3   (call core.setfield! %₂ :contents %₁)
    4   (return %₁)
19  latestworld
20  slot₁/f
21  (return %₂₀)

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
2   latestworld
3   (call core.svec :x)
4   (call core.svec true)
5   (call JuliaLowering.eval_closure_type TestMod :#f#g##0 %₃ %₄)
6   latestworld
7   TestMod.#f#g##0
8   (call core.svec %₇)
9   (call core.svec)
10  SourceLocation::2:14
11  (call core.svec %₈ %₉ %₁₀)
12  --- method core.nothing %₁₁
    slots: [slot₁/#self#(!read)]
    1   10
    2   (call core.getfield slot₁/#self# :x)
    3   (call core.setfield! %₂ :contents %₁)
    4   (return %₁)
13  latestworld
14  TestMod.f
15  (call core.Typeof %₁₄)
16  (call core.svec %₁₅ core.Any)
17  (call core.svec)
18  SourceLocation::1:10
19  (call core.svec %₁₆ %₁₇ %₁₈)
20  --- method core.nothing %₁₉
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
21  latestworld
22  TestMod.f
23  (return %₂₂)

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
2   latestworld
3   (call core.svec :x)
4   (call core.svec false)
5   (call JuliaLowering.eval_closure_type TestMod :#f#g##1 %₃ %₄)
6   latestworld
7   TestMod.#f#g##1
8   (call core.svec %₇)
9   (call core.svec)
10  SourceLocation::2:14
11  (call core.svec %₈ %₉ %₁₀)
12  --- method core.nothing %₁₁
    slots: [slot₁/#self#(!read) slot₂/y(!read)]
    1   (call core.getfield slot₁/#self# :x)
    2   (= slot₂/y %₁)
    3   (return %₁)
13  latestworld
14  TestMod.f
15  (call core.Typeof %₁₄)
16  (call core.svec %₁₅ core.Any)
17  (call core.svec)
18  SourceLocation::1:10
19  (call core.svec %₁₆ %₁₇ %₁₈)
20  --- method core.nothing %₁₉
    slots: [slot₁/#self#(!read) slot₂/x slot₃/g slot₄/z(!read)]
    1   TestMod.#f#g##1
    2   (call core.typeof slot₂/x)
    3   (call core.apply_type %₁ %₂)
    4   (new %₃ slot₂/x)
    5   (= slot₃/g %₄)
    6   slot₂/x
    7   (= slot₄/z %₆)
    8   (return %₆)
21  latestworld
22  TestMod.f
23  (return %₂₂)

########################################
# Closure where a static parameter of an outer function is captured
function f(::T) where T
    function g()
        use(T)
    end
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   (call core.svec :T)
4   (call core.svec true)
5   (call JuliaLowering.eval_closure_type TestMod :#f#g##2 %₃ %₄)
6   latestworld
7   TestMod.#f#g##2
8   (call core.svec %₇)
9   (call core.svec)
10  SourceLocation::2:14
11  (call core.svec %₈ %₉ %₁₀)
12  --- method core.nothing %₁₁
    slots: [slot₁/#self#(!read) slot₂/T(!read)]
    1   TestMod.use
    2   (call core.getfield slot₁/#self# :T)
    3   (call core.isdefined %₂ :contents)
    4   (gotoifnot %₃ label₆)
    5   (goto label₈)
    6   (newvar slot₂/T)
    7   slot₂/T
    8   (call core.getfield %₂ :contents)
    9   (call %₁ %₈)
    10  (return %₉)
13  latestworld
14  (= slot₁/T (call core.TypeVar :T))
15  TestMod.f
16  (call core.Typeof %₁₅)
17  slot₁/T
18  (call core.svec %₁₆ %₁₇)
19  slot₁/T
20  (call core.svec %₁₉)
21  SourceLocation::1:10
22  (call core.svec %₁₈ %₂₀ %₂₁)
23  --- method core.nothing %₂₂
    slots: [slot₁/#self#(!read) slot₂/_(!read) slot₃/g]
    1   TestMod.#f#g##2
    2   static_parameter₁
    3   (new %₁ %₂)
    4   (= slot₃/g %₃)
    5   slot₃/g
    6   (return %₅)
24  latestworld
25  TestMod.f
26  (return %₂₅)

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
2   latestworld
3   (call core.svec :x :y)
4   (call core.svec false true)
5   (call JuliaLowering.eval_closure_type TestMod :#f#g##3 %₃ %₄)
6   latestworld
7   TestMod.#f#g##3
8   (call core.svec %₇)
9   (call core.svec)
10  SourceLocation::2:14
11  (call core.svec %₈ %₉ %₁₀)
12  --- method core.nothing %₁₁
    slots: [slot₁/#self#(!read) slot₂/z]
    1   (= slot₂/z 3)
    2   (call core.getfield slot₁/#self# :y)
    3   (call core.isdefined %₂ :contents)
    4   (isdefined slot₂/z)
    5   (call core.tuple true %₃ %₄)
    6   (return %₅)
13  latestworld
14  TestMod.f
15  (call core.Typeof %₁₄)
16  (call core.svec %₁₅ core.Any)
17  (call core.svec)
18  SourceLocation::1:10
19  (call core.svec %₁₆ %₁₇ %₁₈)
20  --- method core.nothing %₁₉
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
21  latestworld
22  TestMod.f
23  (return %₂₂)

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
6   latestworld
7   TestMod.f
8   (call core.Typeof %₇)
9   (call core.svec %₈)
10  (call core.svec)
11  SourceLocation::3:14
12  (call core.svec %₉ %₁₀ %₁₁)
13  --- code_info
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
14  slot₁/x
15  (call core.svec %₁₄)
16  (call JuliaLowering.replace_captured_locals! %₁₃ %₁₅)
17  --- method core.nothing %₁₂ %₁₆
18  latestworld
19  TestMod.f
20  (return %₁₉)

########################################
# Anonymous function syntax with ->
x -> x*x
#---------------------
1   (call core.svec)
2   (call core.svec)
3   (call JuliaLowering.eval_closure_type TestMod :#->##0 %₁ %₂)
4   latestworld
5   TestMod.#->##0
6   (new %₅)
7   TestMod.#->##0
8   (call core.svec %₇ core.Any)
9   (call core.svec)
10  SourceLocation::1:1
11  (call core.svec %₈ %₉ %₁₀)
12  --- method core.nothing %₁₁
    slots: [slot₁/#self#(!read) slot₂/x]
    1   TestMod.*
    2   (call %₁ slot₂/x slot₂/x)
    3   (return %₂)
13  latestworld
14  (return %₆)

########################################
# Anonymous function syntax with `function`
function (x)
    x*x
end
#---------------------
1   (call core.svec)
2   (call core.svec)
3   (call JuliaLowering.eval_closure_type TestMod :##anon###0 %₁ %₂)
4   latestworld
5   TestMod.##anon###0
6   (new %₅)
7   TestMod.##anon###0
8   (call core.svec %₇ core.Any)
9   (call core.svec)
10  SourceLocation::1:10
11  (call core.svec %₈ %₉ %₁₀)
12  --- method core.nothing %₁₁
    slots: [slot₁/#self#(!read) slot₂/x]
    1   TestMod.*
    2   (call %₁ slot₂/x slot₂/x)
    3   (return %₂)
13  latestworld
14  (return %₆)

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
9   latestworld
10  TestMod.#do##0
11  (call core.svec %₁₀ core.Any)
12  (call core.svec)
13  SourceLocation::1:13
14  (call core.svec %₁₁ %₁₂ %₁₃)
15  --- method core.nothing %₁₄
    slots: [slot₁/#self#(!read) slot₂/y]
    1   TestMod.+
    2   (call %₁ slot₂/y 2)
    3   (return %₂)
16  latestworld
17  TestMod.#do##0
18  (new %₁₇)
19  TestMod.x
20  (call core.kwcall %₅ %₁ %₁₈ %₁₉)
21  (return %₂₀)

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
6   latestworld
7   TestMod.#recursive_a##0
8   slot₂/recursive_b
9   (new %₇ %₈)
10  slot₁/recursive_a
11  (call core.setfield! %₁₀ :contents %₉)
12  TestMod.#recursive_a##0
13  (call core.svec %₁₂)
14  (call core.svec)
15  SourceLocation::2:14
16  (call core.svec %₁₃ %₁₄ %₁₅)
17  --- method core.nothing %₁₆
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
18  latestworld
19  (call core.svec :recursive_a)
20  (call core.svec true)
21  (call JuliaLowering.eval_closure_type TestMod :#recursive_b##0 %₁₉ %₂₀)
22  latestworld
23  TestMod.#recursive_b##0
24  slot₁/recursive_a
25  (new %₂₃ %₂₄)
26  slot₂/recursive_b
27  (call core.setfield! %₂₆ :contents %₂₅)
28  TestMod.#recursive_b##0
29  (call core.svec %₂₈)
30  (call core.svec)
31  SourceLocation::5:14
32  (call core.svec %₂₉ %₃₀ %₃₁)
33  --- method core.nothing %₃₂
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
34  latestworld
35  slot₂/recursive_b
36  (call core.isdefined %₃₅ :contents)
37  (gotoifnot %₃₆ label₃₉)
38  (goto label₄₁)
39  (newvar slot₄/recursive_b)
40  slot₄/recursive_b
41  (call core.getfield %₃₅ :contents)
42  (return %₄₁)

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
9   latestworld
10  TestMod.#f_kw_closure##0
11  slot₂/#f_kw_closure#0
12  (new %₁₀ %₁₁)
13  (= slot₃/f_kw_closure %₁₂)
14  (call core.svec :y)
15  (call core.svec true)
16  (call JuliaLowering.eval_closure_type TestMod :##f_kw_closure#0##0 %₁₄ %₁₅)
17  latestworld
18  TestMod.##f_kw_closure#0##0
19  slot₁/y
20  (new %₁₈ %₁₉)
21  slot₂/#f_kw_closure#0
22  (call core.setfield! %₂₁ :contents %₂₀)
23  TestMod.##f_kw_closure#0##0
24  TestMod.X
25  TestMod.#f_kw_closure##0
26  (call core.svec %₂₃ %₂₄ %₂₅)
27  (call core.svec)
28  SourceLocation::2:14
29  (call core.svec %₂₆ %₂₇ %₂₈)
30  --- method core.nothing %₂₉
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
31  latestworld
32  (call core.typeof core.kwcall)
33  TestMod.#f_kw_closure##0
34  (call core.svec %₃₂ core.NamedTuple %₃₃)
35  (call core.svec)
36  SourceLocation::2:14
37  (call core.svec %₃₄ %₃₅ %₃₆)
38  --- code_info
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
39  slot₂/#f_kw_closure#0
40  (call core.svec %₃₉)
41  (call JuliaLowering.replace_captured_locals! %₃₈ %₄₀)
42  --- method core.nothing %₃₇ %₄₁
43  latestworld
44  TestMod.#f_kw_closure##0
45  (call core.svec %₄₄)
46  (call core.svec)
47  SourceLocation::2:14
48  (call core.svec %₄₅ %₄₆ %₄₇)
49  --- method core.nothing %₄₈
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
50  latestworld
51  slot₃/f_kw_closure
52  (return %₅₁)

########################################
# Closure capturing a typed local must also capture the type expression
# [method_filter: #f_captured_typed_local##0]
let T=Blah
    x::T = 1.0
    function f_captured_typed_local()
        x = 2.0
    end
    f_captured_typed_local()
    x
end
#---------------------
slots: [slot₁/#self#(!read) slot₂/T(!read) slot₃/tmp(!read)]
1   2.0
2   (call core.getfield slot₁/#self# :x)
3   (call core.getfield slot₁/#self# :T)
4   (call core.isdefined %₃ :contents)
5   (gotoifnot %₄ label₇)
6   (goto label₉)
7   (newvar slot₂/T)
8   slot₂/T
9   (call core.getfield %₃ :contents)
10  (= slot₃/tmp %₁)
11  slot₃/tmp
12  (call core.isa %₁₁ %₉)
13  (gotoifnot %₁₂ label₁₅)
14  (goto label₁₈)
15  slot₃/tmp
16  (call top.convert %₉ %₁₅)
17  (= slot₃/tmp (call core.typeassert %₁₆ %₉))
18  slot₃/tmp
19  (call core.setfield! %₂ :contents %₁₈)
20  (return %₁)

########################################
# Error: Closure outside any top level context
# (Should only happen in a user-visible way when lowering code emitted
#  from a `@generated` function code generator.)
@ast_ [K"lambda"(is_toplevel_thunk=false, toplevel_pure=false)
    [K"block"]
    [K"block"]
    [K"->" [K"tuple"] [K"block"]]
]
#---------------------
LoweringError:
#= line 1 =# - Top level code was found outside any top level context. `@generated` functions may not contain closures, including `do` syntax and generators/comprehension

