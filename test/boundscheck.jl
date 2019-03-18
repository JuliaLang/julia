# This file is a part of Julia. License is MIT: https://julialang.org/license

# run boundscheck tests on separate workers launched with --check-bounds={default,yes,no}

cmd = `$(Base.julia_cmd()) --depwarn=error --startup-file=no boundscheck_exec.jl`
if !success(pipeline(cmd; stdout=stdout, stderr=stderr))
    error("boundscheck test failed, cmd : $cmd")
end

cmd = `$(Base.julia_cmd()) --check-bounds=yes --startup-file=no --depwarn=error boundscheck_exec.jl`
if !success(pipeline(cmd; stdout=stdout, stderr=stderr))
    error("boundscheck test failed, cmd : $cmd")
end

cmd = `$(Base.julia_cmd()) --check-bounds=no --startup-file=no --depwarn=error boundscheck_exec.jl`
if !success(pipeline(cmd; stdout=stdout, stderr=stderr))
    error("boundscheck test failed, cmd : $cmd")
end
