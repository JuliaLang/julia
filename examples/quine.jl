# This file is a part of Julia. License is MIT: https://julialang.org/license

x="println(\"x=\$(repr(x))\\n\$x\")"
println("x=$(repr(x))\n$x")
