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
