# This file is a part of Julia. License is MIT: http://julialang.org/license

# Run the parallel test outside of the main driver since it needs its own
# set of dedicated workers.

inline_flag = Base.JLOptions().can_inline == 1 ? `` : `--inline=no`
cov_flag = ``
if Base.JLOptions().code_coverage == 1
    cov_flag = `--code-coverage=user`
elseif Base.JLOptions().code_coverage == 2
    cov_flag = `--code-coverage=all`
end

cmd = `$(Base.julia_cmd()) $inline_flag $cov_flag --check-bounds=yes --startup-file=no --depwarn=error parallel_exec.jl`

if !success(pipeline(cmd; stdout=STDOUT, stderr=STDERR)) && ccall(:jl_running_on_valgrind,Cint,()) == 0
    error("Parallel test failed, cmd : $cmd")
end
