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
1   (enter label₇)
2   TestMod.a
3   (leave %₁)
4   TestMod.c
5   (= slot₂/try_result %₄)
6   (goto label₁₀)
7   TestMod.b
8   (= slot₂/try_result %₇)
9   (pop_exception %₁)
10  slot₂/try_result
11  (= slot₁/z %₁₀)
12  (return %₁₀)

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
6   (goto label₈)
7   (= slot₁/finally_tag 2)
8   TestMod.b
9   (call core.=== slot₁/finally_tag 2)
10  (gotoifnot %₉ label₁₂)
11  (call top.rethrow)
12  slot₂/returnval_via_finally
13  (return %₁₂)

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
1   (enter label₇)
2   (= slot₃/finally_tag -1)
3   TestMod.a
4   (= slot₂/try_result %₃)
5   (leave %₁)
6   (goto label₈)
7   (= slot₃/finally_tag 1)
8   TestMod.b
9   (call core.=== slot₃/finally_tag 1)
10  (gotoifnot %₉ label₁₂)
11  (call top.rethrow)
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
5   (goto label₇)
6   (= slot₁/finally_tag 1)
7   TestMod.b
8   (call core.=== slot₁/finally_tag 1)
9   (gotoifnot %₈ label₁₁)
10  (call top.rethrow)
11  TestMod.z
12  (return %₁₁)

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
1   (gotoifnot true label₁₅)
2   (enter label₉)
3   (= slot₁/finally_tag -1)
4   TestMod.a
5   (leave %₂)
6   (goto label₁₅)
7   (leave %₂)
8   (goto label₁₀)
9   (= slot₁/finally_tag 1)
10  TestMod.b
11  (call core.=== slot₁/finally_tag 1)
12  (gotoifnot %₁₁ label₁₄)
13  (call top.rethrow)
14  (goto label₁)
15  (return core.nothing)

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
14  (goto label₁₆)
15  (= slot₁/finally_tag 2)
16  TestMod.c
17  (call core.=== slot₁/finally_tag 2)
18  (gotoifnot %₁₇ label₂₀)
19  (call top.rethrow)
20  slot₃/returnval_via_finally
21  (return %₂₀)

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
1   (enter label₃₀)
2   (= slot₁/finally_tag -1)
3   (enter label₁₅)
4   (= slot₃/finally_tag -1)
5   TestMod.x
6   (gotoifnot %₅ label₁₁)
7   (= slot₄/returnval_via_finally TestMod.a)
8   (= slot₃/finally_tag 1)
9   (leave %₃)
10  (goto label₁₆)
11  TestMod.b
12  (= slot₂/try_result %₁₁)
13  (leave %₃)
14  (goto label₁₆)
15  (= slot₃/finally_tag 2)
16  TestMod.c
17  (call core.=== slot₃/finally_tag 2)
18  (gotoifnot %₁₇ label₂₀)
19  (call top.rethrow)
20  (call core.=== slot₃/finally_tag 1)
21  (gotoifnot %₂₀ label₂₆)
22  (= slot₅/returnval_via_finally slot₄/returnval_via_finally)
23  (= slot₁/finally_tag 1)
24  (leave %₁)
25  (goto label₃₁)
26  (= slot₆/returnval_via_finally slot₂/try_result)
27  (= slot₁/finally_tag 2)
28  (leave %₁)
29  (goto label₃₁)
30  (= slot₁/finally_tag 3)
31  TestMod.d
32  (call core.=== slot₁/finally_tag 3)
33  (gotoifnot %₃₂ label₃₅)
34  (call top.rethrow)
35  (call core.=== slot₁/finally_tag 2)
36  (gotoifnot %₃₅ label₃₉)
37  slot₆/returnval_via_finally
38  (return %₃₇)
39  slot₅/returnval_via_finally
40  (return %₃₉)

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

