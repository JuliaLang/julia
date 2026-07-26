# This file is a part of Julia. License is MIT: https://julialang.org/license

# Top-level statements run in the interpreter rather than through codegen (#37059).
# Note the `if` below is a single top-level statement: only its first line is
# tracked, since the thunk it lowers to carries no line info.

x = 1
y = x + 1
if y > 1
    z = 10
else
    z = 20
end
exit(z == 10 ? 0 : 1)
