########################################
# Basic branching tail && value
begin
    local a, b
    if a
        b
    end
end
#---------------------
1   (newvar slot₁)
2   (newvar slot₂)
3   slot₁/a
4   (gotoifnot %₃ label₇)
5   slot₂/b
6   (return %₅)
7   (return core.nothing)

########################################
# Branching, !tail && !value
begin
    local a, b, c
    if a
        b
    end
    c
end
#---------------------
1   (newvar slot₁)
2   (newvar slot₂)
3   (newvar slot₃)
4   slot₁/a
5   (gotoifnot %₄ label₇)
6   slot₂/b
7   slot₃/c
8   (return %₇)

########################################
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
1   (newvar slot₁)
2   (newvar slot₂)
3   (newvar slot₃)
4   slot₁/a
5   (gotoifnot %₄ label₈)
6   slot₂/b
7   (return %₆)
8   slot₃/c
9   (return %₈)

########################################
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
1   (newvar slot₁)
2   (newvar slot₂)
3   (newvar slot₃)
4   (newvar slot₄)
5   slot₁/a
6   (gotoifnot %₅ label₉)
7   slot₂/b
8   (goto label₁₀)
9   slot₃/c
10  slot₄/d
11  (return %₁₀)

########################################
# Blocks compile directly to branches
begin
   local a, b, c, d
   if (a; b && c)
       d
   end
end
#---------------------
1   (newvar slot₁)
2   (newvar slot₂)
3   (newvar slot₃)
4   (newvar slot₄)
5   slot₁/a
6   slot₂/b
7   (gotoifnot %₆ label₁₂)
8   slot₃/c
9   (gotoifnot %₈ label₁₂)
10  slot₄/d
11  (return %₁₀)
12  (return core.nothing)

########################################
# symbolic goto forward jump
begin
    a
    @goto foo
    b
    @label foo
end
#---------------------
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
#---------------------
1   TestMod.a
2   TestMod.b
3   (goto label₂)

########################################
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
#---------------------
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
#---------------------
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

########################################
# Error: no symbolic label
begin
    @goto foo
end
#---------------------
LoweringError:
begin
    @goto foo
#         └─┘ ── label `foo` referenced but not defined
end

########################################
# Error: duplicate symbolic label
begin
    @label foo
    @label foo
end
#---------------------
LoweringError:
begin
    @label foo
    @label foo
#          └─┘ ── Label `foo` defined multiple times
end

########################################
# Error: using value of symbolic label
x = @label foo
#---------------------
LoweringError:
x = @label foo
#          └─┘ ── misplaced label in value position

