######################################
# Basic branching tail && value
begin
    local a, b
    if a
        b
    end
end
#-------------------------
1   slot₁/a
2   (gotoifnot %₁ label₅)
3   slot₂/b
4   (return %₃)
5   (return core.nothing)

######################################
# Branching, !tail && !value
begin
    local a, b, c
    if a
        b
    end
    c
end
#-------------------------
1   slot₁/a
2   (gotoifnot %₁ label₄)
3   slot₂/b
4   slot₃/c
5   (return %₄)

######################################
# Branching with else
begin
    local a, b, c
    if a
        b
    else
        c
    end
end
#---------------------
1   slot₁/a
2   (gotoifnot %₁ label₅)
3   slot₂/b
4   (return %₃)
5   slot₃/c
6   (return %₅)

######################################
# Branching with else, !tail && !value
begin
    local a, b, c, d
    if a
        b
    else
        c
    end
    d
end
#---------------------
1   slot₁/a
2   (gotoifnot %₁ label₅)
3   slot₂/b
4   (goto label₆)
5   slot₃/c
6   slot₄/d
7   (return %₆)

######################################
# Blocks compile directly to branches
begin
   local a, b, c, d
   if (a; b && c)
       d
   end
end
#---------------------
1   slot₁/a
2   slot₂/b
3   (gotoifnot %₂ label₈)
4   slot₃/c
5   (gotoifnot %₄ label₈)
6   slot₄/d
7   (return %₆)
8   (return core.nothing)

########################################
# symbolic goto forward jump
begin
    a
    @goto foo
    b
    @label foo
end
#----------
1   TestMod.a
2   (goto label₄)
3   TestMod.b
4   (return core.nothing)

########################################
# symbolic goto backward jump
begin
    a
    @label foo
    b
    @goto foo
end
#----------
1   TestMod.a
2   TestMod.b
3   (goto label₂)

######################################
# Jumping out of try and catch blocks using @goto
begin
    try
        a
        @goto lab
        b
    catch
        c
        @goto lab
        d
    end
    @label lab
end
#----------
1   (enter label₈)
2   TestMod.a
3   (leave %₁)
4   (goto label₁₃)
5   TestMod.b
6   (leave %₁)
7   (goto label₁₃)
8   TestMod.c
9   (pop_exception %₁)
10  (goto label₁₃)
11  TestMod.d
12  (pop_exception %₁)
13  (return core.nothing)

########################################
# Jumping out of nested try/catch and catch/try
begin
    try
        try
            a
        catch
            b
            @goto lab
            c
        end
    catch
        try
            d
            @goto lab
            e
        catch
        end
    end
    @label lab
end
#----------
1   (enter label₁₄)
2   (enter label₆)
3   TestMod.a
4   (leave %₂)
5   (goto label₁₂)
6   TestMod.b
7   (pop_exception %₂)
8   (leave %₁)
9   (goto label₂₄)
10  TestMod.c
11  (pop_exception %₂)
12  (leave %₁)
13  (goto label₂₄)
14  (enter label₂₂)
15  TestMod.d
16  (pop_exception %₁)
17  (leave %₁₄)
18  (goto label₂₄)
19  TestMod.e
20  (leave %₁₄)
21  (goto label₂₃)
22  (pop_exception %₁₄)
23  (pop_exception %₁)
24  (return core.nothing)
