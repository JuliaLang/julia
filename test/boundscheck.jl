# This file is a part of Julia. License is MIT: http://julialang.org/license

# run boundscheck tests on separate workers launched with --check-bounds={default,yes,no}

cmd = `$(Base.julia_cmd()) --depwarn=error boundscheck_exec.jl`
if !success(pipeline(cmd; stdout=STDOUT, stderr=STDERR))
    error("boundscheck test failed, cmd : $cmd")
end

cmd = `$(Base.julia_cmd()) --check-bounds=yes --depwarn=error boundscheck_exec.jl`
if !success(pipeline(cmd; stdout=STDOUT, stderr=STDERR))
    error("boundscheck test failed, cmd : $cmd")
end

cmd = `$(Base.julia_cmd()) --check-bounds=no --depwarn=error boundscheck_exec.jl`
if !success(pipeline(cmd; stdout=STDOUT, stderr=STDERR))
    error("boundscheck test failed, cmd : $cmd")
end
