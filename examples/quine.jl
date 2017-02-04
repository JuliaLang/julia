# This file is a part of Julia. License is MIT: http://julialang.org/license

x="println(\"# This file is a part of Julia. License is MIT: http://julialang.org/license\\n\\nx=\$(repr(x))\\n\$x\")"
println("# This file is a part of Julia. License is MIT: http://julialang.org/license\n\nx=$(repr(x))\n$x")
