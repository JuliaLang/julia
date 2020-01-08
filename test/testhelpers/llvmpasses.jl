# This file is a part of Julia. License is MIT: https://julialang.org/license

using InteractiveUtils
using Printf

# get a temporary directory
dir = ARGS[1]
rm(dir, force=true, recursive=true)
mkdir(dir)

# toggle between unoptimized (CHECK/LOWER) and optimized IR (FINAL).
optimize=false
if length(ARGS) >= 2
    optimize = ARGS[2]=="-O"
end

# Emit LLVM IR to dir
counter = 0
function emit(f, tt...)
    global counter
    name = nameof(f)
    open(joinpath(dir, @sprintf("%05d-%s.ll", counter, name)), "w") do io
        code_llvm(io, f, tt, raw=true, optimize=optimize, dump_module=true, debuginfo=:none)
    end
    counter+=1
end

