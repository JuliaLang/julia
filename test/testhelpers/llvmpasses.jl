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
        params = Base.CodegenParams(
            # defaults for code_llvm + unique_names
            debug_info_kind=Cint(0), debug_info_level=Cint(2), safepoint_on_entry=true,
            gcstack_arg=true, unique_names=true,
        )
        code_llvm(io, f, tt, raw=true, optimize=optimize, dump_module=true, debuginfo=:none, params=params)
    end
    counter+=1
end
