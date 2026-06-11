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
2   (call core.TypeVar :x_type)
3   (call core.tuple :x)
4   (call core.tuple %₂)
5   (call core.tuple %₂)
6   (call core.eval_closure_type TestMod :#f##0 %₃ %₄ %₅)
7   latestworld
8   TestMod.#f##0
9   (call core._typeof_captured_variable slot₂/x)
10  (call core.apply_type %₈ %₉)
11  (new %₁₀ slot₂/x)
12  (= slot₁/f %₁₁)
13  TestMod.#f##0
14  (call core.svec %₁₃ core.Any)
15  (call core.svec)
16  SourceLocation::3:14
17  (call core.svec %₁₄ %₁₅ %₁₆)
18  --- method core.nothing %₁₇
    slots: [slot₁/#self#(!read) slot₂/y]
    1   TestMod.+
    2   (call core.getfield slot₁/#self# :x)
    3   (call %₁ %₂ slot₂/y)
    4   (return %₃)
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
1   (call core.tuple)
2   (call core.tuple)
3   (call core.tuple)
4   (call core.eval_closure_type TestMod :#no_method_f##0 %₁ %₂ %₃)
5   latestworld
6   TestMod.#no_method_f##0
7   (new %₆)
8   (= slot₁/no_method_f %₇)
9   slot₁/no_method_f
10  (return %₉)

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
5   (call core.tuple :x)
6   (call core.tuple core.Box)
7   (call core.tuple)
8   (call core.eval_closure_type TestMod :#f##1 %₅ %₆ %₇)
9   latestworld
10  TestMod.#f##1
11  slot₂/x
12  (new %₁₀ %₁₁)
13  (= slot₁/f %₁₂)
14  TestMod.#f##1
15  (call core.svec %₁₄ core.Any)
16  (call core.svec)
17  SourceLocation::3:14
18  (call core.svec %₁₅ %₁₆ %₁₇)
19  --- method core.nothing %₁₈
    slots: [slot₁/#self#(!read) slot₂/y(!read)]
    1   2
    2   (call core.getfield slot₁/#self# :x)
    3   (call core.setfield! %₂ :contents %₁)
    4   (return %₁)
20  latestworld
21  slot₁/f
22  (return %₂₁)

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
3   (call core.tuple :x)
4   (call core.tuple core.Box)
5   (call core.tuple)
6   (call core.eval_closure_type TestMod :#f#g##0 %₃ %₄ %₅)
7   latestworld
8   TestMod.#f#g##0
9   (call core.svec %₈)
10  (call core.svec)
11  SourceLocation::2:14
12  (call core.svec %₉ %₁₀ %₁₁)
13  --- method core.nothing %₁₂
    slots: [slot₁/#self#(!read)]
    1   10
    2   (call core.getfield slot₁/#self# :x)
    3   (call core.setfield! %₂ :contents %₁)
    4   (return %₁)
14  latestworld
15  TestMod.f
16  (call core.Typeof %₁₅)
17  (call core.svec %₁₆ core.Any)
18  (call core.svec)
19  SourceLocation::1:10
20  (call core.svec %₁₇ %₁₈ %₁₉)
21  --- method TestMod.f %₂₀
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
22  latestworld
23  TestMod.f
24  (return %₂₃)

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
3   (call core.TypeVar :x_type)
4   (call core.tuple :x)
5   (call core.tuple %₃)
6   (call core.tuple %₃)
7   (call core.eval_closure_type TestMod :#foo##->###0 %₄ %₅ %₆)
8   latestworld
9   TestMod.#foo##->###0
10  (call core.svec %₉)
11  (call core.svec)
12  SourceLocation::4:16
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read)]
    1   (call core.getfield slot₁/#self# :x)
    2   (return %₁)
15  latestworld
16  TestMod.foo
17  (call core.Typeof %₁₆)
18  (call core.svec %₁₇ core.Any)
19  (call core.svec)
20  SourceLocation::1:10
21  (call core.svec %₁₈ %₁₉ %₂₀)
22  --- method TestMod.foo %₂₁
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
23  latestworld
24  TestMod.foo
25  (return %₂₄)

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
3   (call core.TypeVar :x_type)
4   (call core.tuple :x)
5   (call core.tuple %₃)
6   (call core.tuple %₃)
7   (call core.eval_closure_type TestMod :#f#g##1 %₄ %₅ %₆)
8   latestworld
9   TestMod.#f#g##1
10  (call core.svec %₉)
11  (call core.svec)
12  SourceLocation::2:14
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read) slot₂/y(!read,single_assign)]
    1   (call core.getfield slot₁/#self# :x)
    2   (= slot₂/y %₁)
    3   (return %₁)
15  latestworld
16  TestMod.f
17  (call core.Typeof %₁₆)
18  (call core.svec %₁₇ core.Any)
19  (call core.svec)
20  SourceLocation::1:10
21  (call core.svec %₁₈ %₁₉ %₂₀)
22  --- method TestMod.f %₂₁
    slots: [slot₁/#self#(!read) slot₂/x slot₃/g(single_assign) slot₄/z(!read,single_assign)]
    1   TestMod.#f#g##1
    2   (call core._typeof_captured_variable slot₂/x)
    3   (call core.apply_type %₁ %₂)
    4   (new %₃ slot₂/x)
    5   (= slot₃/g %₄)
    6   slot₂/x
    7   (= slot₄/z %₆)
    8   (return %₆)
23  latestworld
24  TestMod.f
25  (return %₂₄)

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
3   (call core.TypeVar :T_type)
4   (call core.tuple :T)
5   (call core.tuple %₃)
6   (call core.tuple %₃)
7   (call core.eval_closure_type TestMod :#f#g##2 %₄ %₅ %₆)
8   latestworld
9   TestMod.#f#g##2
10  (call core.svec %₉)
11  (call core.svec)
12  SourceLocation::2:14
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read)]
    1   TestMod.use
    2   (call core.getfield slot₁/#self# :T)
    3   (call %₁ %₂)
    4   (return %₃)
15  latestworld
16  (= slot₁/T (call core.TypeVar :T))
17  TestMod.f
18  (call core.Typeof %₁₇)
19  slot₁/T
20  (call core.svec %₁₈ %₁₉)
21  slot₁/T
22  (call core.svec %₂₁)
23  SourceLocation::1:10
24  (call core.svec %₂₀ %₂₂ %₂₃)
25  --- method TestMod.f %₂₄
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
26  latestworld
27  TestMod.f
28  (return %₂₇)

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
3   (call core.TypeVar :x_type)
4   (call core.tuple :x :y)
5   (call core.tuple %₃ core.Box)
6   (call core.tuple %₃)
7   (call core.eval_closure_type TestMod :#f#g##3 %₄ %₅ %₆)
8   latestworld
9   TestMod.#f#g##3
10  (call core.svec %₉)
11  (call core.svec)
12  SourceLocation::2:14
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read) slot₂/z(single_assign)]
    1   (= slot₂/z 3)
    2   (call core.getfield slot₁/#self# :y)
    3   (call core.isdefined %₂ :contents)
    4   (isdefined slot₂/z)
    5   (call core.tuple true %₃ %₄)
    6   (return %₅)
15  latestworld
16  TestMod.f
17  (call core.Typeof %₁₆)
18  (call core.svec %₁₇ core.Any)
19  (call core.svec)
20  SourceLocation::1:10
21  (call core.svec %₁₈ %₁₉ %₂₀)
22  --- method TestMod.f %₂₁
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
23  latestworld
24  TestMod.f
25  (return %₂₄)

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
15  (call core.tuple %₁₄)
16  (call core.replace_captured_locals! %₁₃ %₁₅)
17  --- method TestMod.f %₁₂ %₁₆
18  latestworld
19  TestMod.f
20  (return %₁₉)

########################################
# Anonymous function syntax with ->
x -> x*x
#---------------------
1   (call core.tuple)
2   (call core.tuple)
3   (call core.tuple)
4   (call core.eval_closure_type TestMod :##->###0 %₁ %₂ %₃)
5   latestworld
6   TestMod.##->###0
7   (new %₆)
8   (= slot₁/#-># %₇)
9   TestMod.##->###0
10  (call core.svec %₉ core.Any)
11  (call core.svec)
12  SourceLocation::1:1
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read) slot₂/x]
    1   TestMod.*
    2   (call %₁ slot₂/x slot₂/x)
    3   (return %₂)
15  latestworld
16  slot₁/#->#
17  (return %₁₆)

########################################
# Anonymous function syntax with `function`
function (x)
    x*x
end
#---------------------
1   (call core.tuple)
2   (call core.tuple)
3   (call core.tuple)
4   (call core.eval_closure_type TestMod :##anon###0 %₁ %₂ %₃)
5   latestworld
6   TestMod.##anon###0
7   (new %₆)
8   (= slot₁/#anon# %₇)
9   TestMod.##anon###0
10  (call core.svec %₉ core.Any)
11  (call core.svec)
12  SourceLocation::1:10
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read) slot₂/x]
    1   TestMod.*
    2   (call %₁ slot₂/x slot₂/x)
    3   (return %₂)
15  latestworld
16  slot₁/#anon#
17  (return %₁₆)

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
6   (call core.tuple)
7   (call core.tuple)
8   (call core.tuple)
9   (call core.eval_closure_type TestMod :##->###1 %₆ %₇ %₈)
10  latestworld
11  TestMod.##->###1
12  (call core.svec %₁₁ core.Any)
13  (call core.svec)
14  SourceLocation::1:13
15  (call core.svec %₁₂ %₁₃ %₁₄)
16  --- method core.nothing %₁₅
    slots: [slot₁/#self#(!read) slot₂/y]
    1   TestMod.+
    2   (call %₁ slot₂/y 2)
    3   (return %₂)
17  latestworld
18  TestMod.##->###1
19  (new %₁₈)
20  (= slot₁/#-># %₁₉)
21  slot₁/#->#
22  TestMod.x
23  (call core.kwcall %₅ %₁ %₂₁ %₂₂)
24  (return %₂₃)

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
2   (call core.tuple :recursive_b)
3   (call core.tuple core.Box)
4   (call core.tuple)
5   (call core.eval_closure_type TestMod :#recursive_a##0 %₂ %₃ %₄)
6   latestworld
7   TestMod.#recursive_a##0
8   slot₂/recursive_b
9   (new %₇ %₈)
10  (= slot₁/recursive_a %₉)
11  TestMod.#recursive_a##0
12  (call core.svec %₁₁)
13  (call core.svec)
14  SourceLocation::2:14
15  (call core.svec %₁₂ %₁₃ %₁₄)
16  --- method core.nothing %₁₅
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
17  latestworld
18  (call core.TypeVar :recursive_a_type)
19  (call core.tuple :recursive_a)
20  (call core.tuple %₁₈)
21  (call core.tuple %₁₈)
22  (call core.eval_closure_type TestMod :#recursive_b##0 %₁₉ %₂₀ %₂₁)
23  latestworld
24  TestMod.#recursive_b##0
25  (call core._typeof_captured_variable slot₁/recursive_a)
26  (call core.apply_type %₂₄ %₂₅)
27  (new %₂₆ slot₁/recursive_a)
28  slot₂/recursive_b
29  (call core.setfield! %₂₈ :contents %₂₇)
30  TestMod.#recursive_b##0
31  (call core.svec %₃₀)
32  (call core.svec)
33  SourceLocation::5:14
34  (call core.svec %₃₁ %₃₂ %₃₃)
35  --- method core.nothing %₃₄
    slots: [slot₁/#self#(!read)]
    1   (call core.getfield slot₁/#self# :recursive_a)
    2   (call %₁)
    3   (return %₂)
36  latestworld
37  slot₂/recursive_b
38  (call core.isdefined %₃₇ :contents)
39  (gotoifnot %₃₈ label₄₁)
40  (goto label₄₃)
41  (newvar slot₃/recursive_b)
42  slot₃/recursive_b
43  (call core.getfield %₃₇ :contents)
44  (return %₄₃)

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
3   (call core.TypeVar :y_type)
4   (call core.tuple :y)
5   (call core.tuple %₃)
6   (call core.tuple %₃)
7   (call core.eval_closure_type TestMod :##kw_body#f_kw_closure#0##0 %₄ %₅ %₆)
8   latestworld
9   TestMod.##kw_body#f_kw_closure#0##0
10  (call core._typeof_captured_variable slot₁/y)
11  (call core.apply_type %₉ %₁₀)
12  (new %₁₁ slot₁/y)
13  (= slot₂/#kw_body#f_kw_closure#0 %₁₂)
14  (call core.TypeVar :#kw_body#f_kw_closure#0_type)
15  (call core.tuple :#kw_body#f_kw_closure#0)
16  (call core.tuple %₁₄)
17  (call core.tuple %₁₄)
18  (call core.eval_closure_type TestMod :#f_kw_closure##0 %₁₅ %₁₆ %₁₇)
19  latestworld
20  TestMod.#f_kw_closure##0
21  (call core._typeof_captured_variable slot₂/#kw_body#f_kw_closure#0)
22  (call core.apply_type %₂₀ %₂₁)
23  (new %₂₂ slot₂/#kw_body#f_kw_closure#0)
24  (= slot₃/f_kw_closure %₂₃)
25  TestMod.##kw_body#f_kw_closure#0##0
26  TestMod.X
27  TestMod.#f_kw_closure##0
28  (call core.svec %₂₅ %₂₆ %₂₇)
29  (call core.svec)
30  SourceLocation::2:14
31  (call core.svec %₂₈ %₂₉ %₃₀)
32  --- method core.nothing %₃₁
    slots: [slot₁/#kw_body#f_kw_closure#0(!read) slot₂/x slot₃/#self#(!read)]
    1   (meta :nkw 1)
    2   TestMod.+
    3   (call core.getfield slot₁/#kw_body#f_kw_closure#0 :y)
    4   (call %₂ slot₂/x %₃)
    5   (return %₄)
33  latestworld
34  TestMod.#f_kw_closure##0
35  (call core.svec %₃₄)
36  (call core.svec)
37  SourceLocation::2:14
38  (call core.svec %₃₅ %₃₆ %₃₇)
39  --- method core.nothing %₃₈
    slots: [slot₁/#self#]
    1   (call core.getfield slot₁/#self# :#kw_body#f_kw_closure#0)
    2   TestMod.x_default
    3   (call %₁ %₂ slot₁/#self#)
    4   (return %₃)
40  latestworld
41  (call core.typeof core.kwcall)
42  TestMod.#f_kw_closure##0
43  (call core.svec %₄₁ core.NamedTuple %₄₂)
44  (call core.svec)
45  SourceLocation::2:14
46  (call core.svec %₄₃ %₄₄ %₄₅)
47  --- method core.nothing %₄₆
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
48  latestworld
49  slot₃/f_kw_closure
50  (return %₄₉)

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
3   (call core.TypeVar :y_type)
4   (call core.tuple :y)
5   (call core.tuple %₃)
6   (call core.tuple %₃)
7   (call core.eval_closure_type TestMod :#f_after_if##->###0 %₄ %₅ %₆)
8   latestworld
9   TestMod.#f_after_if##->###0
10  (call core.svec %₉)
11  (call core.svec)
12  SourceLocation::6:5
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read)]
    1   (call core.getfield slot₁/#self# :y)
    2   (return %₁)
15  latestworld
16  TestMod.f_after_if
17  (call core.Typeof %₁₆)
18  (call core.svec %₁₇ core.Any)
19  (call core.svec)
20  SourceLocation::1:10
21  (call core.svec %₁₈ %₁₉ %₂₀)
22  --- method TestMod.f_after_if %₂₁
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
23  latestworld
24  TestMod.f_after_if
25  (return %₂₄)

########################################
# Ternary operator (if expression in value position) doesn't need Box
function f_ternary(x)
    y = x > 0 ? x : 0
    () -> y
end
#---------------------
1   (method TestMod.f_ternary)
2   latestworld
3   (call core.TypeVar :y_type)
4   (call core.tuple :y)
5   (call core.tuple %₃)
6   (call core.tuple %₃)
7   (call core.eval_closure_type TestMod :#f_ternary##->###0 %₄ %₅ %₆)
8   latestworld
9   TestMod.#f_ternary##->###0
10  (call core.svec %₉)
11  (call core.svec)
12  SourceLocation::3:5
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read)]
    1   (call core.getfield slot₁/#self# :y)
    2   (return %₁)
15  latestworld
16  TestMod.f_ternary
17  (call core.Typeof %₁₆)
18  (call core.svec %₁₇ core.Any)
19  (call core.svec)
20  SourceLocation::1:10
21  (call core.svec %₁₈ %₁₉ %₂₀)
22  --- method TestMod.f_ternary %₂₁
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
23  latestworld
24  TestMod.f_ternary
25  (return %₂₄)

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
3   (call core.TypeVar :y_type)
4   (call core.tuple :y)
5   (call core.tuple %₃)
6   (call core.tuple %₃)
7   (call core.eval_closure_type TestMod :#f_or_guard##->###0 %₄ %₅ %₆)
8   latestworld
9   TestMod.#f_or_guard##->###0
10  (call core.svec %₉)
11  (call core.svec)
12  SourceLocation::4:5
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read)]
    1   (call core.getfield slot₁/#self# :y)
    2   (return %₁)
15  latestworld
16  TestMod.f_or_guard
17  (call core.Typeof %₁₆)
18  (call core.svec %₁₇ core.Any)
19  (call core.svec)
20  SourceLocation::1:10
21  (call core.svec %₁₈ %₁₉ %₂₀)
22  --- method TestMod.f_or_guard %₂₁
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
23  latestworld
24  TestMod.f_or_guard
25  (return %₂₄)

########################################
# Argument reassigned in outer scope - no Box needed
function f_arg_reassign(x)
    x = 1
    return ()->x
end
#---------------------
1   (method TestMod.f_arg_reassign)
2   latestworld
3   (call core.TypeVar :x_type)
4   (call core.tuple :x)
5   (call core.tuple %₃)
6   (call core.tuple %₃)
7   (call core.eval_closure_type TestMod :#f_arg_reassign##->###0 %₄ %₅ %₆)
8   latestworld
9   TestMod.#f_arg_reassign##->###0
10  (call core.svec %₉)
11  (call core.svec)
12  SourceLocation::3:12
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read)]
    1   (call core.getfield slot₁/#self# :x)
    2   (return %₁)
15  latestworld
16  TestMod.f_arg_reassign
17  (call core.Typeof %₁₆)
18  (call core.svec %₁₇ core.Any)
19  (call core.svec)
20  SourceLocation::1:10
21  (call core.svec %₁₈ %₁₉ %₂₀)
22  --- method TestMod.f_arg_reassign %₂₁
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
23  latestworld
24  TestMod.f_arg_reassign
25  (return %₂₄)

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
7   (call core.tuple :y)
8   (call core.tuple core.Box)
9   (call core.tuple)
10  (call core.eval_closure_type TestMod :##->###2 %₇ %₈ %₉)
11  latestworld
12  TestMod.##->###2
13  slot₂/y
14  (new %₁₂ %₁₃)
15  (= slot₁/#-># %₁₄)
16  TestMod.##->###2
17  (call core.svec %₁₆)
18  (call core.svec)
19  SourceLocation::5:5
20  (call core.svec %₁₇ %₁₈ %₁₉)
21  --- method core.nothing %₂₀
    slots: [slot₁/#self#(!read) slot₂/y(!read,maybe_undef)]
    1   (call core.getfield slot₁/#self# :y)
    2   (call core.isdefined %₁ :contents)
    3   (gotoifnot %₂ label₅)
    4   (goto label₇)
    5   (newvar slot₂/y)
    6   slot₂/y
    7   (call core.getfield %₁ :contents)
    8   (return %₇)
22  latestworld
23  slot₁/#->#
24  (return %₂₃)

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
3   (call core.TypeVar :x_type)
4   (call core.tuple :x)
5   (call core.tuple %₃)
6   (call core.tuple %₃)
7   (call core.eval_closure_type TestMod :#f_local_no_box##->###0 %₄ %₅ %₆)
8   latestworld
9   TestMod.#f_local_no_box##->###0
10  (call core.svec %₉)
11  (call core.svec)
12  SourceLocation::4:5
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read)]
    1   (call core.getfield slot₁/#self# :x)
    2   (return %₁)
15  latestworld
16  TestMod.f_local_no_box
17  (call core.Typeof %₁₆)
18  (call core.svec %₁₇)
19  (call core.svec)
20  SourceLocation::1:10
21  (call core.svec %₁₈ %₁₉ %₂₀)
22  --- method TestMod.f_local_no_box %₂₁
    slots: [slot₁/#self#(!read) slot₂/x(single_assign) slot₃/#->#(single_assign)]
    1   (= slot₂/x 1)
    2   TestMod.#f_local_no_box##->###0
    3   (call core._typeof_captured_variable slot₂/x)
    4   (call core.apply_type %₂ %₃)
    5   (new %₄ slot₂/x)
    6   (= slot₃/#-># %₅)
    7   slot₃/#->#
    8   (return %₇)
23  latestworld
24  TestMod.f_local_no_box
25  (return %₂₄)

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
3   (call core.TypeVar :x_type)
4   (call core.tuple :x)
5   (call core.tuple %₃)
6   (call core.tuple %₃)
7   (call core.eval_closure_type TestMod :#f_typed_local_no_box##->###0 %₄ %₅ %₆)
8   latestworld
9   TestMod.#f_typed_local_no_box##->###0
10  (call core.svec %₉)
11  (call core.svec)
12  SourceLocation::4:5
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read)]
    1   (call core.getfield slot₁/#self# :x)
    2   (return %₁)
15  latestworld
16  TestMod.f_typed_local_no_box
17  (call core.Typeof %₁₆)
18  (call core.svec %₁₇)
19  (call core.svec)
20  SourceLocation::1:10
21  (call core.svec %₁₈ %₁₉ %₂₀)
22  --- method TestMod.f_typed_local_no_box %₂₁
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
23  latestworld
24  TestMod.f_typed_local_no_box
25  (return %₂₄)

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
  (block (call core.eval_closure_type Main.TestMod :##->###3 (call core.tuple) (call core.tuple) (call core.tuple)))
Containing expressions:
  (block (call core.eval_closure_type Main.TestMod :##->###3 (call core.tuple) (call core.tuple) (call core.tuple)))
