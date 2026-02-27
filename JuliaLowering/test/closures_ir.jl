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
20  --- method core.nothing %₁₉
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
5   (call JuliaLowering.eval_closure_type TestMod :#foo#->##0 %₃ %₄)
6   latestworld
7   TestMod.#foo#->##0
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
20  --- method core.nothing %₁₉
    slots: [slot₁/#self#(!read) slot₂/x(single_assign) slot₃/x(!read)]
    1   (= slot₃/x slot₂/x)
    2   TestMod.rand
    3   TestMod.Bool
    4   (call %₂ %₃)
    5   (gotoifnot %₄ label₁₄)
    6   (= slot₃/x 5)
    7   TestMod.#foo#->##0
    8   slot₃/x
    9   (call core._typeof_captured_variable %₈)
    10  (call core.apply_type %₇ %₉)
    11  slot₃/x
    12  (new %₁₀ %₁₁)
    13  (return %₁₂)
    14  slot₃/x
    15  (return %₁₄)
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
20  --- method core.nothing %₁₉
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
23  --- method core.nothing %₂₂
    slots: [slot₁/#self#(!read) slot₂/#arg1#(!read) slot₃/g(single_assign)]
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
20  --- method core.nothing %₁₉
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
    slots: [slot₁/#self#(!read) slot₂/recursive_a(!read,maybe_undef)]
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
2   (= slot₂/#f_kw_closure#0 (call core.Box))
3   (= slot₁/y %₁)
4   (call core.svec :#f_kw_closure#0)
5   (call core.svec true)
6   (call JuliaLowering.eval_closure_type TestMod :#f_kw_closure##0 %₄ %₅)
7   latestworld
8   TestMod.#f_kw_closure##0
9   slot₂/#f_kw_closure#0
10  (new %₈ %₉)
11  (= slot₃/f_kw_closure %₁₀)
12  (call core.svec :y)
13  (call core.svec false)
14  (call JuliaLowering.eval_closure_type TestMod :##f_kw_closure#0##0 %₁₂ %₁₃)
15  latestworld
16  TestMod.##f_kw_closure#0##0
17  (call core._typeof_captured_variable slot₁/y)
18  (call core.apply_type %₁₆ %₁₇)
19  (new %₁₈ slot₁/y)
20  slot₂/#f_kw_closure#0
21  (call core.setfield! %₂₀ :contents %₁₉)
22  TestMod.##f_kw_closure#0##0
23  TestMod.X
24  TestMod.#f_kw_closure##0
25  (call core.svec %₂₂ %₂₃ %₂₄)
26  (call core.svec)
27  SourceLocation::2:14
28  (call core.svec %₂₅ %₂₆ %₂₇)
29  --- method core.nothing %₂₈
    slots: [slot₁/#f_kw_closure#0(!read) slot₂/x slot₃/#self#(!read)]
    1   (meta :nkw 1)
    2   TestMod.+
    3   (call core.getfield slot₁/#f_kw_closure#0 :y)
    4   (call %₂ slot₂/x %₃)
    5   (return %₄)
30  latestworld
31  (call core.typeof core.kwcall)
32  TestMod.#f_kw_closure##0
33  (call core.svec %₃₁ core.NamedTuple %₃₂)
34  (call core.svec)
35  SourceLocation::2:14
36  (call core.svec %₃₃ %₃₄ %₃₅)
37  --- method core.nothing %₃₆
    slots: [slot₁/#kwcall_self#(!read) slot₂/kws slot₃/#self# slot₄/kwtmp slot₅/x(!read) slot₆/#f_kw_closure#0(!read,maybe_undef)]
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
    24  (call core.getfield slot₃/#self# :#f_kw_closure#0)
    25  (call core.isdefined %₂₄ :contents)
    26  (gotoifnot %₂₅ label₂₈)
    27  (goto label₃₀)
    28  (newvar slot₆/#f_kw_closure#0)
    29  slot₆/#f_kw_closure#0
    30  (call core.getfield %₂₄ :contents)
    31  (call %₃₀ %₁₆ slot₃/#self#)
    32  (return %₃₁)
38  latestworld
39  TestMod.#f_kw_closure##0
40  (call core.svec %₃₉)
41  (call core.svec)
42  SourceLocation::2:14
43  (call core.svec %₄₀ %₄₁ %₄₂)
44  --- method core.nothing %₄₃
    slots: [slot₁/#self# slot₂/#f_kw_closure#0(!read,maybe_undef)]
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
45  latestworld
46  slot₃/f_kw_closure
47  (return %₄₆)

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
5   (call JuliaLowering.eval_closure_type TestMod :#f_after_if#->##0 %₃ %₄)
6   latestworld
7   TestMod.#f_after_if#->##0
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
20  --- method core.nothing %₁₉
    slots: [slot₁/#self#(!read) slot₂/cond slot₃/y(single_assign)]
    1   (gotoifnot slot₂/cond label₄)
    2   TestMod.println
    3   (call %₂ "hello")
    4   (= slot₃/y 1)
    5   TestMod.#f_after_if#->##0
    6   (call core._typeof_captured_variable slot₃/y)
    7   (call core.apply_type %₅ %₆)
    8   (new %₇ slot₃/y)
    9   (return %₈)
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
5   (call JuliaLowering.eval_closure_type TestMod :#f_ternary#->##0 %₃ %₄)
6   latestworld
7   TestMod.#f_ternary#->##0
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
20  --- method core.nothing %₁₉
    slots: [slot₁/#self#(!read) slot₂/x slot₃/y(single_assign) slot₄/if_val(!read)]
    1   TestMod.>
    2   (call %₁ slot₂/x 0)
    3   (gotoifnot %₂ label₇)
    4   slot₂/x
    5   (= slot₄/if_val %₄)
    6   (goto label₈)
    7   (= slot₄/if_val 0)
    8   slot₄/if_val
    9   (= slot₃/y %₈)
    10  TestMod.#f_ternary#->##0
    11  (call core._typeof_captured_variable slot₃/y)
    12  (call core.apply_type %₁₀ %₁₁)
    13  (new %₁₂ slot₃/y)
    14  (return %₁₃)
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
5   (call JuliaLowering.eval_closure_type TestMod :#f_or_guard#->##0 %₃ %₄)
6   latestworld
7   TestMod.#f_or_guard#->##0
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
20  --- method core.nothing %₁₉
    slots: [slot₁/#self#(!read) slot₂/x slot₃/y(single_assign) slot₄/if_val(!read)]
    1   TestMod.===
    2   TestMod.nothing
    3   (call %₁ slot₂/x %₂)
    4   (gotoifnot %₃ label₇)
    5   (= slot₄/if_val true)
    6   (goto label₁₀)
    7   TestMod.===
    8   TestMod.missing
    9   (= slot₄/if_val (call %₇ slot₂/x %₈))
    10  slot₄/if_val
    11  (gotoifnot %₁₀ label₁₅)
    12  TestMod.nothing
    13  (return %₁₂)
    14  (goto label₁₅)
    15  slot₂/x
    16  (= slot₃/y %₁₅)
    17  TestMod.#f_or_guard#->##0
    18  (call core._typeof_captured_variable slot₃/y)
    19  (call core.apply_type %₁₇ %₁₈)
    20  (new %₁₉ slot₃/y)
    21  (return %₂₀)
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
5   (call JuliaLowering.eval_closure_type TestMod :#f_arg_reassign#->##0 %₃ %₄)
6   latestworld
7   TestMod.#f_arg_reassign#->##0
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
20  --- method core.nothing %₁₉
    slots: [slot₁/#self#(!read) slot₂/x(single_assign) slot₃/x(!read)]
    1   (= slot₃/x slot₂/x)
    2   (= slot₃/x 1)
    3   TestMod.#f_arg_reassign#->##0
    4   slot₃/x
    5   (call core._typeof_captured_variable %₄)
    6   (call core.apply_type %₃ %₅)
    7   slot₃/x
    8   (new %₆ %₇)
    9   (return %₈)
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
1   (= slot₁/y (call core.Box))
2   (goto label₆)
3   1
4   slot₁/y
5   (call core.setfield! %₄ :contents %₃)
6   (call core.svec :y)
7   (call core.svec true)
8   (call JuliaLowering.eval_closure_type TestMod :#->##1 %₆ %₇)
9   latestworld
10  TestMod.#->##1
11  slot₁/y
12  (new %₁₀ %₁₁)
13  TestMod.#->##1
14  (call core.svec %₁₃)
15  (call core.svec)
16  SourceLocation::5:5
17  (call core.svec %₁₄ %₁₅ %₁₆)
18  --- method core.nothing %₁₇
    slots: [slot₁/#self#(!read) slot₂/y(!read,maybe_undef)]
    1   (call core.getfield slot₁/#self# :y)
    2   (call core.isdefined %₁ :contents)
    3   (gotoifnot %₂ label₅)
    4   (goto label₇)
    5   (newvar slot₂/y)
    6   slot₂/y
    7   (call core.getfield %₁ :contents)
    8   (return %₇)
19  latestworld
20  (return %₁₂)

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
5   (call JuliaLowering.eval_closure_type TestMod :#f_local_no_box#->##0 %₃ %₄)
6   latestworld
7   TestMod.#f_local_no_box#->##0
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
20  --- method core.nothing %₁₉
    slots: [slot₁/#self#(!read) slot₂/x(single_assign)]
    1   (= slot₂/x 1)
    2   TestMod.#f_local_no_box#->##0
    3   (call core._typeof_captured_variable slot₂/x)
    4   (call core.apply_type %₂ %₃)
    5   (new %₄ slot₂/x)
    6   (return %₅)
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
5   (call JuliaLowering.eval_closure_type TestMod :#f_typed_local_no_box#->##0 %₃ %₄)
6   latestworld
7   TestMod.#f_typed_local_no_box#->##0
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
20  --- method core.nothing %₁₉
    slots: [slot₁/#self#(!read) slot₂/x(single_assign) slot₃/tmp(!read)]
    1   1
    2   TestMod.Int
    3   (= slot₃/tmp %₁)
    4   (call core.isa slot₃/tmp %₂)
    5   (gotoifnot %₄ label₇)
    6   (goto label₉)
    7   (call top.convert %₂ slot₃/tmp)
    8   (= slot₃/tmp (call core.typeassert %₇ %₂))
    9   slot₃/tmp
    10  (= slot₂/x %₉)
    11  TestMod.#f_typed_local_no_box#->##0
    12  (call core._typeof_captured_variable slot₂/x)
    13  (call core.apply_type %₁₁ %₁₂)
    14  (new %₁₃ slot₂/x)
    15  (return %₁₄)
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
