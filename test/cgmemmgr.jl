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

# /proc/self/mem must be accessible so we can test SelfMemMapper
map_types = Sys.islinux() ? ["dual", "selfmem", "fallback"] : ["dual", "fallback"]
block_sizes = [s for s in [14, 16, 22] if s > log2(Sys.PAGESIZE)]

for block_size in block_sizes
    for map_type in map_types
        cmd = addenv(`$(Base.julia_cmd()) -e $src`,
                     "JULIA_CGMEMMGR_BLOCK_SIZE" => string(block_size),
                     "JULIA_CGMEMMGR_MAP_TYPE" => map_type)
        @test success(cmd)
    end
end
