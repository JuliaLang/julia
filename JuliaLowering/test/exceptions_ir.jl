########################################
# Return from inside try/catch
try
    f
    return x
catch
    g
    return y
end
#---------------------
1   (enter label₆)
2   TestMod.f
3   TestMod.x
4   (leave %₁)
5   (return %₃)
6   TestMod.g
7   TestMod.y
8   (pop_exception %₁)
9   (return %₇)

########################################
# Return from inside try/catch with simple return vals
try
    f
    return 10
catch
    g
    return 20
end
#---------------------
1   (enter label₅)
2   TestMod.f
3   (leave %₁)
4   (return 10)
5   TestMod.g
6   (pop_exception %₁)
7   (return 20)

########################################
# Return from multiple try + try/catch
try
    try
        return 10
    catch
        return 20
    end
catch
end
#---------------------
1   (enter label₁₄)
2   (enter label₇)
3   (leave %₁ %₂)
4   (return 10)
5   (leave %₂)
6   (goto label₁₁)
7   (leave %₁)
8   (pop_exception %₂)
9   (return 20)
10  (pop_exception %₂)
11  slot₁/try_result
12  (leave %₁)
13  (return %₁₁)
14  (pop_exception %₁)
15  (return core.nothing)

########################################
# Return from multiple catch + try/catch
try
catch
    try
        return 10
    catch
        return 20
    end
end
#---------------------
1   (enter label₄)
2   (leave %₁)
3   (return core.nothing)
4   (enter label₈)
5   (leave %₄)
6   (pop_exception %₁)
7   (return 10)
8   (pop_exception %₁)
9   (return 20)

########################################
# try/catch/else, tail position
try
    a
catch
    b
else
    c
end
#---------------------
1   (enter label₆)
2   TestMod.a
3   (leave %₁)
4   TestMod.c
5   (return %₄)
6   TestMod.b
7   (pop_exception %₁)
8   (return %₆)

########################################
# try/catch/else, value position
let
    z = try
        a
    catch
        b
    else
        c
    end
end
#---------------------
1   (newvar slot₁/z)
2   (enter label₈)
3   TestMod.a
4   (leave %₂)
5   TestMod.c
6   (= slot₂/try_result %₅)
7   (goto label₁₁)
8   TestMod.b
9   (= slot₂/try_result %₈)
10  (pop_exception %₂)
11  slot₂/try_result
12  (= slot₁/z %₁₁)
13  (return %₁₁)

########################################
# try/catch/else, not value/tail
begin
    try
        a
    catch
        b
    else
        c
    end
    z
end
#---------------------
1   (enter label₆)
2   TestMod.a
3   (leave %₁)
4   TestMod.c
5   (goto label₈)
6   TestMod.b
7   (pop_exception %₁)
8   TestMod.z
9   (return %₈)

########################################
# basic try/finally, tail position
try
    a
finally
    b
end
#---------------------
1   (enter label₇)
2   (= slot₁/finally_tag -1)
3   (= slot₂/returnval_via_finally TestMod.a)
4   (= slot₁/finally_tag 1)
5   (leave %₁)
6   (goto label₁₀)
7   TestMod.b
8   (call top.rethrow)
9   (return core.nothing)
10  TestMod.b
11  slot₂/returnval_via_finally
12  (return %₁₁)

########################################
# basic try/finally, value position
let
    z = try
        a
    finally
        b
    end
end
#---------------------
1   (newvar slot₁/z)
2   (enter label₈)
3   (= slot₃/finally_tag -1)
4   TestMod.a
5   (= slot₂/try_result %₄)
6   (leave %₂)
7   (goto label₁₁)
8   TestMod.b
9   (call top.rethrow)
10  (return core.nothing)
11  TestMod.b
12  slot₂/try_result
13  (= slot₁/z %₁₂)
14  (return %₁₂)

########################################
# basic try/finally, not value/tail
begin
    try
        a
    finally
        b
    end
    z
end
#---------------------
1   (enter label₆)
2   (= slot₁/finally_tag -1)
3   TestMod.a
4   (leave %₁)
5   (goto label₉)
6   TestMod.b
7   (call top.rethrow)
8   (return core.nothing)
9   TestMod.b
10  TestMod.z
11  (return %₁₀)

########################################
# try/finally + break
while true
    try
        a
        break
    finally
        b
    end
end
#---------------------
1   (gotoifnot true label₁₈)
2   (enter label₁₀)
3   (= slot₂/finally_tag -1)
4   TestMod.a
5   (= slot₂/finally_tag 1)
6   (leave %₂)
7   (goto label₁₃)
8   (leave %₂)
9   (goto label₁₃)
10  TestMod.b
11  (call top.rethrow)
12  (return core.nothing)
13  TestMod.b
14  (call core.=== slot₂/finally_tag 1)
15  (gotoifnot %₁₄ label₁₇)
16  (goto label₁₉)
17  (goto label₁)
18  (= slot₁/loop-exit_result core.nothing)
19  (isdefined slot₁/loop-exit_result)
20  (gotoifnot %₁₉ label₂₂)
21  (goto label₂₃)
22  (= slot₁/loop-exit_result core.nothing)
23  slot₁/loop-exit_result
24  (return %₂₃)

########################################
# try/catch/finally
try
    a
catch
    b
finally
    c
end
#---------------------
1   (enter label₁₅)
2   (= slot₁/finally_tag -1)
3   (enter label₈)
4   TestMod.a
5   (= slot₂/try_result %₄)
6   (leave %₃)
7   (goto label₁₁)
8   TestMod.b
9   (= slot₂/try_result %₈)
10  (pop_exception %₃)
11  (= slot₃/returnval_via_finally slot₂/try_result)
12  (= slot₁/finally_tag 1)
13  (leave %₁)
14  (goto label₁₈)
15  TestMod.c
16  (call top.rethrow)
17  (return core.nothing)
18  TestMod.c
19  slot₃/returnval_via_finally
20  (return %₁₉)

########################################
# Nested finally blocks
try
    try
        if x
            return a
        end
        b
    finally
        c
    end
finally
    d
end
#---------------------
1   (enter label₂₉)
2   (= slot₁/finally_tag -1)
3   (enter label₁₅)
4   (= slot₃/finally_tag -1)
5   TestMod.x
6   (gotoifnot %₅ label₁₁)
7   (= slot₄/returnval_via_finally TestMod.a)
8   (= slot₃/finally_tag 1)
9   (leave %₃)
10  (goto label₁₈)
11  TestMod.b
12  (= slot₂/try_result %₁₁)
13  (leave %₃)
14  (goto label₁₈)
15  TestMod.c
16  (call top.rethrow)
17  (return core.nothing)
18  TestMod.c
19  (call core.=== slot₃/finally_tag 2)
20  (gotoifnot %₁₉ label₂₅)
21  (= slot₅/returnval_via_finally slot₂/try_result)
22  (= slot₁/finally_tag 1)
23  (leave %₁)
24  (goto label₃₂)
25  (= slot₆/returnval_via_finally slot₄/returnval_via_finally)
26  (= slot₁/finally_tag 2)
27  (leave %₁)
28  (goto label₃₂)
29  TestMod.d
30  (call top.rethrow)
31  (return core.nothing)
32  TestMod.d
33  (call core.=== slot₁/finally_tag 2)
34  (gotoifnot %₃₃ label₃₇)
35  slot₆/returnval_via_finally
36  (return %₃₅)
37  slot₅/returnval_via_finally
38  (return %₃₇)

########################################
# Access to the exception object
try
    a
catch exc
    b
end
#---------------------
1   (enter label₅)
2   TestMod.a
3   (leave %₁)
4   (return %₂)
5   (= slot₁/exc (call JuliaLowering.current_exception))
6   TestMod.b
7   (pop_exception %₁)
8   (return %₆)

########################################
# Error: unmatched goto from try
begin
    try
        @goto lab
    finally
    end
    @label lab
end
#---------------------
LoweringError:
begin
    try
        @goto lab
#             └─┘ ── `goto` out of a `try` block is not permitted with `finally`
    finally
    end

########################################
# Error: unmatched goto from catch
begin
    try
    catch
        @goto lab
    finally
    end
    @label lab
end
#---------------------
LoweringError:
    try
    catch
        @goto lab
#             └─┘ ── `goto` out of a `catch` block is not permitted with `finally`
    finally
    end

########################################
# Error: unmatched goto from else
begin
    try
    catch
    else
        @goto lab
    finally
    end
    @label lab
end
#---------------------
LoweringError:
    catch
    else
        @goto lab
#             └─┘ ── `goto` out of an `else` block is not permitted with `finally`
    finally
    end
