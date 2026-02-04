# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

# Fill at least one cgmemmgr block
src = raw"""
for i in 1:100
    s = Symbol("f$i")
    f = eval(:($s(x) = @inline(cos(x + $i))))
    invokelatest(f, 1.0)
end
"""

map_types = Sys.islinux() ? ["dual", "selfmem", "fallback"] : ["dual", "fallback"]
block_sizes = [16, 22]

for block_size in block_sizes
    for map_type in map_types
        cmd = addenv(`$(Base.julia_cmd()) -e $src`,
                     "JULIA_CGMEMMGR_BLOCK_SIZE" => string(block_size),
                     "JULIA_CGMEMMGR_MAP_TYPE" => map_type)
        @test success(cmd)
    end
end
