########################################
# Simple closure - single-assigned capture before control flow doesn't need Box
let
    x = 1
    function f(y)
        x + y
    end
end
#---------------------
1   (= slot₂/x 1)
2   (call core.svec :x)
3   (call core.svec false)
4   (call JuliaLowering.eval_closure_type TestMod :#f##0 %₂ %₃)
5   latestworld
6   TestMod.#f##0
7   (call core._typeof_captured_variable slot₂/x)
8   (call core.apply_type %₆ %₇)
9   (new %₈ slot₂/x)
10  (= slot₁/f %₉)
11  TestMod.#f##0
12  (call core.svec %₁₁ core.Any)
13  (call core.svec)
14  SourceLocation::3:14
15  (call core.svec %₁₂ %₁₃ %₁₄)
16  --- method core.nothing %₁₅
    slots: [slot₁/#self#(!read) slot₂/y]
    1   TestMod.+
    2   (call core.getfield slot₁/#self# :x)
    3   (call %₁ %₂ slot₂/y)
    4   (return %₃)
17  latestworld
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
20  --- method TestMod.f %₁₉
    slots: [slot₁/#self#(!read) slot₂/x(single_assign) slot₃/g(single_assign,called) slot₄/x(!read,maybe_undef) slot₅/x(!read)]
    1   (= slot₅/x slot₂/x)
    2   slot₅/x
    3   (= slot₅/x (call core.Box %₂))
    4   TestMod.#f#g##0
    5   slot₅/x
    6   (new %₄ %₅)
    7   (= slot₃/g %₆)
    8   slot₃/g
    9   (call %₈)
    10  slot₅/x
    11  (call core.isdefined %₁₀ :contents)
    12  (gotoifnot %₁₁ label₁₄)
    13  (goto label₁₆)
    14  (newvar slot₄/x)
    15  slot₄/x
    16  (call core.getfield %₁₀ :contents)
    17  (return %₁₆)
21  latestworld
22  TestMod.f
23  (return %₂₂)

########################################
# Argument reassigned in outer scope then captured - no Box needed
# (from PR #60567 review)
function foo(x)
    if rand(Bool)
        x = 5
        return ()->x
    end
    return x
end
#---------------------
1   (method TestMod.foo)
2   latestworld
3   (call core.svec :x)
4   (call core.svec false)
5   (call JuliaLowering.eval_closure_type TestMod :#foo##->###0 %₃ %₄)
6   latestworld
7   TestMod.#foo##->###0
8   (call core.svec %₇)
9   (call core.svec)
10  SourceLocation::4:16
11  (call core.svec %₈ %₉ %₁₀)
12  --- method core.nothing %₁₁
    slots: [slot₁/#self#(!read)]
    1   (call core.getfield slot₁/#self# :x)
    2   (return %₁)
13  latestworld
14  TestMod.foo
15  (call core.Typeof %₁₄)
16  (call core.svec %₁₅ core.Any)
17  (call core.svec)
18  SourceLocation::1:10
19  (call core.svec %₁₆ %₁₇ %₁₈)
20  --- method TestMod.foo %₁₉
    slots: [slot₁/#self#(!read) slot₂/x(single_assign) slot₃/#->#(single_assign) slot₄/x(!read)]
    1   (= slot₄/x slot₂/x)
    2   (newvar slot₃/#->#)
    3   TestMod.rand
    4   TestMod.Bool
    5   (call %₃ %₄)
    6   (gotoifnot %₅ label₁₇)
    7   (= slot₄/x 5)
    8   TestMod.#foo##->###0
    9   slot₄/x
    10  (call core._typeof_captured_variable %₉)
    11  (call core.apply_type %₈ %₁₀)
    12  slot₄/x
    13  (new %₁₁ %₁₂)
    14  (= slot₃/#-># %₁₃)
    15  slot₃/#->#
    16  (return %₁₅)
    17  slot₄/x
    18  (return %₁₇)
21  latestworld
22  TestMod.foo
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
    slots: [slot₁/#self#(!read) slot₂/y(!read,single_assign)]
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
20  --- method TestMod.f %₁₉
    slots: [slot₁/#self#(!read) slot₂/x slot₃/g(single_assign) slot₄/z(!read,single_assign)]
    1   TestMod.#f#g##1
    2   (call core._typeof_captured_variable slot₂/x)
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
4   (call core.svec false)
5   (call JuliaLowering.eval_closure_type TestMod :#f#g##2 %₃ %₄)
6   latestworld
7   TestMod.#f#g##2
8   (call core.svec %₇)
9   (call core.svec)
10  SourceLocation::2:14
11  (call core.svec %₈ %₉ %₁₀)
12  --- method core.nothing %₁₁
    slots: [slot₁/#self#(!read)]
    1   TestMod.use
    2   (call core.getfield slot₁/#self# :T)
    3   (call %₁ %₂)
    4   (return %₃)
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
23  --- method TestMod.f %₂₂
    slots: [slot₁/#self#(!read) slot₂/#unused#(!read) slot₃/g(single_assign)]
    1   TestMod.#f#g##2
    2   static_parameter₁
    3   (call core._typeof_captured_variable %₂)
    4   (call core.apply_type %₁ %₃)
    5   static_parameter₁
    6   (new %₄ %₅)
    7   (= slot₃/g %₆)
    8   slot₃/g
    9   (return %₈)
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
    slots: [slot₁/#self#(!read) slot₂/z(single_assign)]
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
20  --- method TestMod.f %₁₉
    slots: [slot₁/#self#(!read) slot₂/x slot₃/g(single_assign) slot₄/y(single_assign)]
    1   (= slot₄/y (call core.Box))
    2   TestMod.#f#g##3
    3   (call core._typeof_captured_variable slot₂/x)
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
slots: [slot₁/#self#(!read) slot₂/y slot₃/h_nest(single_assign)]
1   TestMod.#f_nest#g_nest#h_nest##0
2   (call core.getfield slot₁/#self# :x)
3   (call core._typeof_captured_variable %₂)
4   (call core._typeof_captured_variable slot₂/y)
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
    slots: [slot₁/#self#(!read) slot₂/x(!read,maybe_undef)]
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
17  --- method TestMod.f %₁₂ %₁₆
18  latestworld
19  TestMod.f
20  (return %₁₉)

########################################
# Anonymous function syntax with ->
x -> x*x
#---------------------
1   (call core.svec)
2   (call core.svec)
3   (call JuliaLowering.eval_closure_type TestMod :##->###0 %₁ %₂)
4   latestworld
5   TestMod.##->###0
6   (new %₅)
7   (= slot₁/#-># %₆)
8   TestMod.##->###0
9   (call core.svec %₈ core.Any)
10  (call core.svec)
11  SourceLocation::1:1
12  (call core.svec %₉ %₁₀ %₁₁)
13  --- method core.nothing %₁₂
    slots: [slot₁/#self#(!read) slot₂/x]
    1   TestMod.*
    2   (call %₁ slot₂/x slot₂/x)
    3   (return %₂)
14  latestworld
15  slot₁/#->#
16  (return %₁₅)

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
7   (= slot₁/#anon# %₆)
8   TestMod.##anon###0
9   (call core.svec %₈ core.Any)
10  (call core.svec)
11  SourceLocation::1:10
12  (call core.svec %₉ %₁₀ %₁₁)
13  --- method core.nothing %₁₂
    slots: [slot₁/#self#(!read) slot₂/x]
    1   TestMod.*
    2   (call %₁ slot₂/x slot₂/x)
    3   (return %₂)
14  latestworld
15  slot₁/#anon#
16  (return %₁₅)

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
8   (call JuliaLowering.eval_closure_type TestMod :##->###1 %₆ %₇)
9   latestworld
10  TestMod.##->###1
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
17  TestMod.##->###1
18  (new %₁₇)
19  (= slot₁/#-># %₁₈)
20  slot₁/#->#
21  TestMod.x
22  (call core.kwcall %₅ %₁ %₂₀ %₂₁)
23  (return %₂₂)

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
#            ╙ ── cannot overwrite a static parameter
    end
end

########################################
# Opaque closure (y is single-assigned before capture, no Box needed)
let y = 1
    Base.Experimental.@opaque (x, z::T)->2x + y - z
end
#---------------------
1   1
2   (= slot₁/y %₁)
3   TestMod.T
4   (call core.apply_type core.Tuple core.Any %₃)
5   (call core.apply_type core.Union)
6   --- opaque_closure_method  core.nothing 2 false SourceLocation::2:31
    slots: [slot₁/#self#(!read) slot₂/x slot₃/z]
    1   TestMod.-
    2   TestMod.+
    3   TestMod.*
    4   (call %₃ 2 slot₂/x)
    5   (call core.getfield slot₁/#self# 1)
    6   (call %₂ %₄ %₅)
    7   (call %₁ %₆ slot₃/z)
    8   (return %₇)
7   (new_opaque_closure %₄ %₅ core.Any true %₆ slot₁/y)
8   (return %₇)

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
#                          └─┘ ── opaque closure cannot have optional or keyword arguments

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
1   (= slot₂/recursive_b (call core.Box))
2   (call core.svec :recursive_b)
3   (call core.svec true)
4   (call JuliaLowering.eval_closure_type TestMod :#recursive_a##0 %₂ %₃)
5   latestworld
6   TestMod.#recursive_a##0
7   slot₂/recursive_b
8   (new %₆ %₇)
9   (= slot₁/recursive_a %₈)
10  TestMod.#recursive_a##0
11  (call core.svec %₁₀)
12  (call core.svec)
13  SourceLocation::2:14
14  (call core.svec %₁₁ %₁₂ %₁₃)
15  --- method core.nothing %₁₄
    slots: [slot₁/#self#(!read) slot₂/recursive_b(!read,maybe_undef)]
    1   (call core.getfield slot₁/#self# :recursive_b)
    2   (call core.isdefined %₁ :contents)
    3   (gotoifnot %₂ label₅)
    4   (goto label₇)
    5   (newvar slot₂/recursive_b)
    6   slot₂/recursive_b
    7   (call core.getfield %₁ :contents)
    8   (call %₇)
    9   (return %₈)
16  latestworld
17  (call core.svec :recursive_a)
18  (call core.svec false)
19  (call JuliaLowering.eval_closure_type TestMod :#recursive_b##0 %₁₇ %₁₈)
20  latestworld
21  TestMod.#recursive_b##0
22  (call core._typeof_captured_variable slot₁/recursive_a)
23  (call core.apply_type %₂₁ %₂₂)
24  (new %₂₃ slot₁/recursive_a)
25  slot₂/recursive_b
26  (call core.setfield! %₂₅ :contents %₂₄)
27  TestMod.#recursive_b##0
28  (call core.svec %₂₇)
29  (call core.svec)
30  SourceLocation::5:14
31  (call core.svec %₂₈ %₂₉ %₃₀)
32  --- method core.nothing %₃₁
    slots: [slot₁/#self#(!read)]
    1   (call core.getfield slot₁/#self# :recursive_a)
    2   (call %₁)
    3   (return %₂)
33  latestworld
34  slot₂/recursive_b
35  (call core.isdefined %₃₄ :contents)
36  (gotoifnot %₃₅ label₃₈)
37  (goto label₄₀)
38  (newvar slot₃/recursive_b)
39  slot₃/recursive_b
40  (call core.getfield %₃₄ :contents)
41  (return %₄₀)

########################################
# Closure with keywords
let y = y_init
    function f_kw_closure(; x::X=x_default)
        x + y
    end
end
#---------------------
1   TestMod.y_init
2   (= slot₁/y %₁)
3   (call core.svec :y)
4   (call core.svec false)
5   (call JuliaLowering.eval_closure_type TestMod :##kw_body#f_kw_closure#0##0 %₃ %₄)
6   latestworld
7   TestMod.##kw_body#f_kw_closure#0##0
8   (call core._typeof_captured_variable slot₁/y)
9   (call core.apply_type %₇ %₈)
10  (new %₉ slot₁/y)
11  (= slot₂/#kw_body#f_kw_closure#0 %₁₀)
12  (call core.svec :#kw_body#f_kw_closure#0)
13  (call core.svec false)
14  (call JuliaLowering.eval_closure_type TestMod :#f_kw_closure##0 %₁₂ %₁₃)
15  latestworld
16  TestMod.#f_kw_closure##0
17  (call core._typeof_captured_variable slot₂/#kw_body#f_kw_closure#0)
18  (call core.apply_type %₁₆ %₁₇)
19  (new %₁₈ slot₂/#kw_body#f_kw_closure#0)
20  (= slot₃/f_kw_closure %₁₉)
21  TestMod.##kw_body#f_kw_closure#0##0
22  TestMod.X
23  TestMod.#f_kw_closure##0
24  (call core.svec %₂₁ %₂₂ %₂₃)
25  (call core.svec)
26  SourceLocation::2:14
27  (call core.svec %₂₄ %₂₅ %₂₆)
28  --- method core.nothing %₂₇
    slots: [slot₁/#kw_body#f_kw_closure#0(!read) slot₂/x slot₃/#self#(!read)]
    1   (meta :nkw 1)
    2   TestMod.+
    3   (call core.getfield slot₁/#kw_body#f_kw_closure#0 :y)
    4   (call %₂ slot₂/x %₃)
    5   (return %₄)
29  latestworld
30  TestMod.#f_kw_closure##0
31  (call core.svec %₃₀)
32  (call core.svec)
33  SourceLocation::2:14
34  (call core.svec %₃₁ %₃₂ %₃₃)
35  --- method core.nothing %₃₄
    slots: [slot₁/#self#]
    1   (call core.getfield slot₁/#self# :#kw_body#f_kw_closure#0)
    2   TestMod.x_default
    3   (call %₁ %₂ slot₁/#self#)
    4   (return %₃)
36  latestworld
37  (call core.typeof core.kwcall)
38  TestMod.#f_kw_closure##0
39  (call core.svec %₃₇ core.NamedTuple %₃₈)
40  (call core.svec)
41  SourceLocation::2:14
42  (call core.svec %₃₉ %₄₀ %₄₁)
43  --- method core.nothing %₄₂
    slots: [slot₁/#unused#(!read) slot₂/kws slot₃/#self# slot₄/x(!read) slot₅/kwtmp]
    1   (newvar slot₄/x)
    2   (newvar slot₅/kwtmp)
    3   (call core.isdefined slot₂/kws :x)
    4   (gotoifnot %₃ label₁₅)
    5   (call core.getfield slot₂/kws :x)
    6   TestMod.X
    7   (call core.isa %₅ %₆)
    8   (gotoifnot %₇ label₁₀)
    9   (goto label₁₃)
    10  TestMod.X
    11  (new core.TypeError :keyword argument :x %₁₀ %₅)
    12  (call core.throw %₁₁)
    13  (= slot₅/kwtmp %₅)
    14  (goto label₁₇)
    15  TestMod.x_default
    16  (= slot₅/kwtmp %₁₅)
    17  slot₅/kwtmp
    18  (call top.keys slot₂/kws)
    19  (call core.tuple :x)
    20  (call top.diff_names %₁₈ %₁₉)
    21  (call top.isempty %₂₀)
    22  (gotoifnot %₂₁ label₂₄)
    23  (goto label₂₅)
    24  (call top.kwerr slot₂/kws slot₃/#self#)
    25  (call core.getfield slot₃/#self# :#kw_body#f_kw_closure#0)
    26  (call %₂₅ %₁₇ slot₃/#self#)
    27  (return %₂₆)
44  latestworld
45  slot₃/f_kw_closure
46  (return %₄₅)

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
slots: [slot₁/#self#(!read) slot₂/tmp(!read)]
1   2.0
2   (call core.getfield slot₁/#self# :x)
3   (call core.getfield slot₁/#self# :T)
4   (= slot₂/tmp %₁)
5   (call core.isa slot₂/tmp %₃)
6   (gotoifnot %₅ label₈)
7   (goto label₁₀)
8   (call top.convert %₃ slot₂/tmp)
9   (= slot₂/tmp (call core.typeassert %₈ %₃))
10  slot₂/tmp
11  (call core.setfield! %₂ :contents %₁₀)
12  (return %₁)

########################################
# Assignment after if statement doesn't need Box (flisp-compatible save/restore)
function f_after_if(cond)
    if cond
        println("hello")
    end
    y = 1
    () -> y
end
#---------------------
1   (method TestMod.f_after_if)
2   latestworld
3   (call core.svec :y)
4   (call core.svec false)
5   (call JuliaLowering.eval_closure_type TestMod :#f_after_if##->###0 %₃ %₄)
6   latestworld
7   TestMod.#f_after_if##->###0
8   (call core.svec %₇)
9   (call core.svec)
10  SourceLocation::6:5
11  (call core.svec %₈ %₉ %₁₀)
12  --- method core.nothing %₁₁
    slots: [slot₁/#self#(!read)]
    1   (call core.getfield slot₁/#self# :y)
    2   (return %₁)
13  latestworld
14  TestMod.f_after_if
15  (call core.Typeof %₁₄)
16  (call core.svec %₁₅ core.Any)
17  (call core.svec)
18  SourceLocation::1:10
19  (call core.svec %₁₆ %₁₇ %₁₈)
20  --- method TestMod.f_after_if %₁₉
    slots: [slot₁/#self#(!read) slot₂/cond slot₃/#->#(single_assign) slot₄/y(single_assign)]
    1   (newvar slot₃/#->#)
    2   (gotoifnot slot₂/cond label₅)
    3   TestMod.println
    4   (call %₃ "hello")
    5   (= slot₄/y 1)
    6   TestMod.#f_after_if##->###0
    7   (call core._typeof_captured_variable slot₄/y)
    8   (call core.apply_type %₆ %₇)
    9   (new %₈ slot₄/y)
    10  (= slot₃/#-># %₉)
    11  slot₃/#->#
    12  (return %₁₁)
21  latestworld
22  TestMod.f_after_if
23  (return %₂₂)

########################################
# Ternary operator (if expression in value position) doesn't need Box
function f_ternary(x)
    y = x > 0 ? x : 0
    () -> y
end
#---------------------
1   (method TestMod.f_ternary)
2   latestworld
3   (call core.svec :y)
4   (call core.svec false)
5   (call JuliaLowering.eval_closure_type TestMod :#f_ternary##->###0 %₃ %₄)
6   latestworld
7   TestMod.#f_ternary##->###0
8   (call core.svec %₇)
9   (call core.svec)
10  SourceLocation::3:5
11  (call core.svec %₈ %₉ %₁₀)
12  --- method core.nothing %₁₁
    slots: [slot₁/#self#(!read)]
    1   (call core.getfield slot₁/#self# :y)
    2   (return %₁)
13  latestworld
14  TestMod.f_ternary
15  (call core.Typeof %₁₄)
16  (call core.svec %₁₅ core.Any)
17  (call core.svec)
18  SourceLocation::1:10
19  (call core.svec %₁₆ %₁₇ %₁₈)
20  --- method TestMod.f_ternary %₁₉
    slots: [slot₁/#self#(!read) slot₂/x slot₃/#->#(single_assign) slot₄/y(single_assign) slot₅/if_val(!read)]
    1   (newvar slot₃/#->#)
    2   TestMod.>
    3   (call %₂ slot₂/x 0)
    4   (gotoifnot %₃ label₈)
    5   slot₂/x
    6   (= slot₅/if_val %₅)
    7   (goto label₉)
    8   (= slot₅/if_val 0)
    9   slot₅/if_val
    10  (= slot₄/y %₉)
    11  TestMod.#f_ternary##->###0
    12  (call core._typeof_captured_variable slot₄/y)
    13  (call core.apply_type %₁₁ %₁₂)
    14  (new %₁₃ slot₄/y)
    15  (= slot₃/#-># %₁₄)
    16  slot₃/#->#
    17  (return %₁₆)
21  latestworld
22  TestMod.f_ternary
23  (return %₂₂)

########################################
# || guard pattern (value position with early exit) doesn't need Box
function f_or_guard(x)
    (x === nothing || x === missing) && return nothing
    y = x
    () -> y
end
#---------------------
1   (method TestMod.f_or_guard)
2   latestworld
3   (call core.svec :y)
4   (call core.svec false)
5   (call JuliaLowering.eval_closure_type TestMod :#f_or_guard##->###0 %₃ %₄)
6   latestworld
7   TestMod.#f_or_guard##->###0
8   (call core.svec %₇)
9   (call core.svec)
10  SourceLocation::4:5
11  (call core.svec %₈ %₉ %₁₀)
12  --- method core.nothing %₁₁
    slots: [slot₁/#self#(!read)]
    1   (call core.getfield slot₁/#self# :y)
    2   (return %₁)
13  latestworld
14  TestMod.f_or_guard
15  (call core.Typeof %₁₄)
16  (call core.svec %₁₅ core.Any)
17  (call core.svec)
18  SourceLocation::1:10
19  (call core.svec %₁₆ %₁₇ %₁₈)
20  --- method TestMod.f_or_guard %₁₉
    slots: [slot₁/#self#(!read) slot₂/x slot₃/#->#(single_assign) slot₄/y(single_assign) slot₅/if_val(!read)]
    1   (newvar slot₃/#->#)
    2   TestMod.===
    3   TestMod.nothing
    4   (call %₂ slot₂/x %₃)
    5   (gotoifnot %₄ label₈)
    6   (= slot₅/if_val true)
    7   (goto label₁₁)
    8   TestMod.===
    9   TestMod.missing
    10  (= slot₅/if_val (call %₈ slot₂/x %₉))
    11  slot₅/if_val
    12  (gotoifnot %₁₁ label₁₆)
    13  TestMod.nothing
    14  (return %₁₃)
    15  (goto label₁₆)
    16  slot₂/x
    17  (= slot₄/y %₁₆)
    18  TestMod.#f_or_guard##->###0
    19  (call core._typeof_captured_variable slot₄/y)
    20  (call core.apply_type %₁₈ %₁₉)
    21  (new %₂₀ slot₄/y)
    22  (= slot₃/#-># %₂₁)
    23  slot₃/#->#
    24  (return %₂₃)
21  latestworld
22  TestMod.f_or_guard
23  (return %₂₂)

########################################
# Argument reassigned in outer scope - no Box needed
function f_arg_reassign(x)
    x = 1
    return ()->x
end
#---------------------
1   (method TestMod.f_arg_reassign)
2   latestworld
3   (call core.svec :x)
4   (call core.svec false)
5   (call JuliaLowering.eval_closure_type TestMod :#f_arg_reassign##->###0 %₃ %₄)
6   latestworld
7   TestMod.#f_arg_reassign##->###0
8   (call core.svec %₇)
9   (call core.svec)
10  SourceLocation::3:12
11  (call core.svec %₈ %₉ %₁₀)
12  --- method core.nothing %₁₁
    slots: [slot₁/#self#(!read)]
    1   (call core.getfield slot₁/#self# :x)
    2   (return %₁)
13  latestworld
14  TestMod.f_arg_reassign
15  (call core.Typeof %₁₄)
16  (call core.svec %₁₅ core.Any)
17  (call core.svec)
18  SourceLocation::1:10
19  (call core.svec %₁₆ %₁₇ %₁₈)
20  --- method TestMod.f_arg_reassign %₁₉
    slots: [slot₁/#self#(!read) slot₂/x(single_assign) slot₃/#->#(single_assign) slot₄/x(!read)]
    1   (= slot₄/x slot₂/x)
    2   (= slot₄/x 1)
    3   TestMod.#f_arg_reassign##->###0
    4   slot₄/x
    5   (call core._typeof_captured_variable %₄)
    6   (call core.apply_type %₃ %₅)
    7   slot₄/x
    8   (new %₆ %₇)
    9   (= slot₃/#-># %₈)
    10  slot₃/#->#
    11  (return %₁₀)
21  latestworld
22  TestMod.f_arg_reassign
23  (return %₂₂)

########################################
# Label can be jumped to, bypassing assignment - needs Box
let
    @goto L
    y = 1
    @label L
    ()->y
end
#---------------------
1   (newvar slot₁/#->#)
2   (= slot₂/y (call core.Box))
3   (goto label₇)
4   1
5   slot₂/y
6   (call core.setfield! %₅ :contents %₄)
7   (call core.svec :y)
8   (call core.svec true)
9   (call JuliaLowering.eval_closure_type TestMod :##->###2 %₇ %₈)
10  latestworld
11  TestMod.##->###2
12  slot₂/y
13  (new %₁₁ %₁₂)
14  (= slot₁/#-># %₁₃)
15  TestMod.##->###2
16  (call core.svec %₁₅)
17  (call core.svec)
18  SourceLocation::5:5
19  (call core.svec %₁₆ %₁₇ %₁₈)
20  --- method core.nothing %₁₉
    slots: [slot₁/#self#(!read) slot₂/y(!read,maybe_undef)]
    1   (call core.getfield slot₁/#self# :y)
    2   (call core.isdefined %₁ :contents)
    3   (gotoifnot %₂ label₅)
    4   (goto label₇)
    5   (newvar slot₂/y)
    6   slot₂/y
    7   (call core.getfield %₁ :contents)
    8   (return %₇)
21  latestworld
22  slot₁/#->#
23  (return %₂₂)

########################################
# Local single-assigned after declaration - no Box needed
function f_local_no_box()
    local x
    x = 1
    ()->x
end
#---------------------
1   (method TestMod.f_local_no_box)
2   latestworld
3   (call core.svec :x)
4   (call core.svec false)
5   (call JuliaLowering.eval_closure_type TestMod :#f_local_no_box##->###0 %₃ %₄)
6   latestworld
7   TestMod.#f_local_no_box##->###0
8   (call core.svec %₇)
9   (call core.svec)
10  SourceLocation::4:5
11  (call core.svec %₈ %₉ %₁₀)
12  --- method core.nothing %₁₁
    slots: [slot₁/#self#(!read)]
    1   (call core.getfield slot₁/#self# :x)
    2   (return %₁)
13  latestworld
14  TestMod.f_local_no_box
15  (call core.Typeof %₁₄)
16  (call core.svec %₁₅)
17  (call core.svec)
18  SourceLocation::1:10
19  (call core.svec %₁₆ %₁₇ %₁₈)
20  --- method TestMod.f_local_no_box %₁₉
    slots: [slot₁/#self#(!read) slot₂/x(single_assign) slot₃/#->#(single_assign)]
    1   (= slot₂/x 1)
    2   TestMod.#f_local_no_box##->###0
    3   (call core._typeof_captured_variable slot₂/x)
    4   (call core.apply_type %₂ %₃)
    5   (new %₄ slot₂/x)
    6   (= slot₃/#-># %₅)
    7   slot₃/#->#
    8   (return %₇)
21  latestworld
22  TestMod.f_local_no_box
23  (return %₂₂)

########################################
# Typed local single-assigned after declaration - no Box needed
function f_typed_local_no_box()
    local x::Int
    x = 1
    ()->x
end
#---------------------
1   (method TestMod.f_typed_local_no_box)
2   latestworld
3   (call core.svec :x)
4   (call core.svec false)
5   (call JuliaLowering.eval_closure_type TestMod :#f_typed_local_no_box##->###0 %₃ %₄)
6   latestworld
7   TestMod.#f_typed_local_no_box##->###0
8   (call core.svec %₇)
9   (call core.svec)
10  SourceLocation::4:5
11  (call core.svec %₈ %₉ %₁₀)
12  --- method core.nothing %₁₁
    slots: [slot₁/#self#(!read)]
    1   (call core.getfield slot₁/#self# :x)
    2   (return %₁)
13  latestworld
14  TestMod.f_typed_local_no_box
15  (call core.Typeof %₁₄)
16  (call core.svec %₁₅)
17  (call core.svec)
18  SourceLocation::1:10
19  (call core.svec %₁₆ %₁₇ %₁₈)
20  --- method TestMod.f_typed_local_no_box %₁₉
    slots: [slot₁/#self#(!read) slot₂/x(single_assign) slot₃/#->#(single_assign) slot₄/tmp(!read)]
    1   (newvar slot₃/#->#)
    2   1
    3   TestMod.Int
    4   (= slot₄/tmp %₂)
    5   (call core.isa slot₄/tmp %₃)
    6   (gotoifnot %₅ label₈)
    7   (goto label₁₀)
    8   (call top.convert %₃ slot₄/tmp)
    9   (= slot₄/tmp (call core.typeassert %₈ %₃))
    10  slot₄/tmp
    11  (= slot₂/x %₁₀)
    12  TestMod.#f_typed_local_no_box##->###0
    13  (call core._typeof_captured_variable slot₂/x)
    14  (call core.apply_type %₁₂ %₁₃)
    15  (new %₁₄ slot₂/x)
    16  (= slot₃/#-># %₁₅)
    17  slot₃/#->#
    18  (return %₁₇)
21  latestworld
22  TestMod.f_typed_local_no_box
23  (return %₂₂)

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
Expression:
  (call JuliaLowering.eval_closure_type Main.TestMod :##->###3 (call core.svec) (call core.svec))
Containing expressions:
  (call JuliaLowering.eval_closure_type Main.TestMod :##->###3 (call core.svec) (call core.svec))
