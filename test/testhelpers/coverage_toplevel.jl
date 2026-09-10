# This file is a part of Julia. License is MIT: https://julialang.org/license

# Coverage for top-level statements, whose lowered thunks carry no line information.

x = 1
y = x + 1
if y > 1
    z = 10
else
    z = 20
end
exit(z == 10 ? 0 : 1)
