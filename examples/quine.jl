# This file is a part of Julia. License is MIT: https://julialang.org/license

x="println(\"# This file is a part of Julia. License is MIT: https://julialang.org/license\\n\\nx=\$(repr(x))\\n\$x\")"
println("# This file is a part of Julia. License is MIT: https://julialang.org/license\n\nx=$(repr(x))\n$x")
