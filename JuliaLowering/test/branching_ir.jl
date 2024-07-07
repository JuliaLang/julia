######################################
# Basic branching tail && value
begin
    local a, b
    if a
        b
    end
end
#-------------------------
1   slot.₁/a
2   (gotoifnot %.₁ label.₅)
3   slot.₂/b
4   (return %.₃)
5   core.nothing
6   (return %.₅)

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
1   slot.₁/a
2   (gotoifnot %.₁ label.₄)
3   slot.₂/b
4   slot.₃/c
5   (return %.₄)

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
1   slot.₁/a
2   (gotoifnot %.₁ label.₅)
3   slot.₂/b
4   (return %.₃)
5   slot.₃/c
6   (return %.₅)

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
1   slot.₁/a
2   (gotoifnot %.₁ label.₅)
3   slot.₂/b
4   (goto label.₆)
5   slot.₃/c
6   slot.₄/d
7   (return %.₆)

######################################
# Blocks compile directly to branches
begin
   local a, b, c, d
   if (a; b && c)
       d
   end
end
#---------------------
1   slot.₁/a
2   slot.₂/b
3   (gotoifnot %.₂ label.₈)
4   slot.₃/c
5   (gotoifnot %.₄ label.₈)
6   slot.₄/d
7   (return %.₆)
8   core.nothing
9   (return %.₈)
