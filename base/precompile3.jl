# This file is a part of Julia. License is MIT: https://julialang.org/license

# Steps to regenerate this file:
# 1. Remove all `precompile` calls
# 2. Rebuild system image
# 3. Enable TRACE_COMPILE in options.h and rebuild
# 4. Run `./julia 2> precompiles.txt` and do various things.
# 5. Run `grep -v '#[0-9]' precompiles.txt >> base/precompile.jl` (filters out closures, which
#    might have different generated names in different environments)

