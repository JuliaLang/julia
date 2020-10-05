# This file is a part of Julia. License is MIT: https://julialang.org/license

cmd = `$(Base.julia_cmd()) --depwarn=no --startup-file=no download_exec.jl`
if !success(pipeline(cmd; stdout=stdout, stderr=stderr))
    error("download test failed, cmd : $cmd")
end
