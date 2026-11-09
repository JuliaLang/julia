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
3   (leave %₂ %₁)
4   (return 10)
5   (leave %₂)
6   (goto label₁₁)
7   (leave %₁)
8   (pop_exception %₂)
9   (return 20)
10  (pop_exception %₂)
11  slot₁/
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
5   (= slot₂/ %₄)
6   (goto label₁₀)
7   TestMod.b
8   (= slot₂/ %₇)
9   (pop_exception %₁)
10  slot₂/
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
2   (= slot₁/ -1)
3   (= slot₂/ TestMod.a)
4   (= slot₁/ 1)
5   (leave %₁)
6   (goto label₁₁)
7   TestMod.b
8   (call top.rethrow)
9   (pop_exception %₁)
10  (return core.nothing)
11  TestMod.b
12  slot₂/
13  (return %₁₂)

########################################
# empty finally
try
    a
finally
end
#---------------------
1   (enter label₅)
2   TestMod.a
3   (leave %₁)
4   (return %₂)
5   (call top.rethrow)
6   (pop_exception %₁)
7   (return %₅)

########################################
# trivial finally
try
    a
finally
    1
end
#---------------------
1   (enter label₅)
2   TestMod.a
3   (leave %₁)
4   (return %₂)
5   (call top.rethrow)
6   (pop_exception %₁)
7   (return %₅)

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
2   (= slot₃/ -1)
3   TestMod.a
4   (= slot₂/ %₃)
5   (leave %₁)
6   (goto label₁₁)
7   TestMod.b
8   (call top.rethrow)
9   (pop_exception %₁)
10  (return core.nothing)
11  TestMod.b
12  slot₂/
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
2   (= slot₁/ -1)
3   TestMod.a
4   (leave %₁)
5   (goto label₁₀)
6   TestMod.b
7   (call top.rethrow)
8   (pop_exception %₁)
9   (return core.nothing)
10  TestMod.b
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
1   (gotoifnot true label₁₉)
2   (enter label₁₀)
3   (= slot₂/ -1)
4   TestMod.a
5   (= slot₂/ 1)
6   (leave %₂)
7   (goto label₁₄)
8   (leave %₂)
9   (goto label₁₄)
10  TestMod.b
11  (call top.rethrow)
12  (pop_exception %₂)
13  (return core.nothing)
14  TestMod.b
15  (call core.=== slot₂/ 1)
16  (gotoifnot %₁₅ label₁₈)
17  (goto label₂₀)
18  (goto label₁)
19  (= slot₁/ core.nothing)
20  (isdefined slot₁/)
21  (gotoifnot %₂₀ label₂₃)
22  (goto label₂₄)
23  (= slot₁/ core.nothing)
24  slot₁/
25  (return %₂₄)

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
2   (= slot₁/ -1)
3   (enter label₈)
4   TestMod.a
5   (= slot₂/ %₄)
6   (leave %₃)
7   (goto label₁₁)
8   TestMod.b
9   (= slot₂/ %₈)
10  (pop_exception %₃)
11  (= slot₃/ slot₂/)
12  (= slot₁/ 1)
13  (leave %₁)
14  (goto label₁₉)
15  TestMod.c
16  (call top.rethrow)
17  (pop_exception %₁)
18  (return core.nothing)
19  TestMod.c
20  slot₃/
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
1   (enter label₃₂)
2   (= slot₁/ -1)
3   (enter label₁₅)
4   (= slot₃/ -1)
5   TestMod.x
6   (gotoifnot %₅ label₁₁)
7   (= slot₄/ TestMod.a)
8   (= slot₃/ 1)
9   (leave %₃)
10  (goto label₂₁)
11  TestMod.b
12  (= slot₂/ %₁₁)
13  (leave %₃)
14  (goto label₂₁)
15  TestMod.c
16  (call top.rethrow)
17  (= slot₁/ 1)
18  (pop_exception %₃)
19  (leave %₁)
20  (goto label₃₆)
21  TestMod.c
22  (call core.=== slot₃/ 1)
23  (gotoifnot %₂₂ label₂₈)
24  (= slot₅/ slot₄/)
25  (= slot₁/ 2)
26  (leave %₁)
27  (goto label₃₆)
28  (= slot₆/ slot₂/)
29  (= slot₁/ 3)
30  (leave %₁)
31  (goto label₃₆)
32  TestMod.d
33  (call top.rethrow)
34  (pop_exception %₁)
35  (return core.nothing)
36  TestMod.d
37  (call core.=== slot₁/ 3)
38  (gotoifnot %₃₇ label₄₁)
39  slot₆/
40  (return %₃₉)
41  (call core.=== slot₁/ 2)
42  (gotoifnot %₄₁ label₄₅)
43  slot₅/
44  (return %₄₃)
45  (return core.nothing)

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
5   (the_exception)
6   (= slot₁/exc %₅)
7   TestMod.b
8   (pop_exception %₁)
9   (return %₇)

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
