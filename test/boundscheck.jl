# This file is a part of Julia. License is MIT: https://julialang.org/license

# run boundscheck tests on separate workers launched with --check-bounds={default,yes,no}

function run_boundscheck(cmd)
    mktemp() do _, output
        ok = success(pipeline(cmd; stdout=output, stderr=output))
        if !ok
            seekstart(output)
            write(stderr, read(output))
            error("boundscheck test failed, cmd : $cmd")
        end
    end
    return nothing
end

let cmd = `$(Base.julia_cmd()) --check-bounds=auto --depwarn=error --startup-file=no boundscheck_exec.jl`
    run_boundscheck(cmd)
end

let cmd = `$(Base.julia_cmd()) --check-bounds=yes --startup-file=no --depwarn=error boundscheck_exec.jl`
    run_boundscheck(cmd)
end

let cmd = `$(Base.julia_cmd()) --check-bounds=no --startup-file=no --depwarn=error boundscheck_exec.jl`
    run_boundscheck(cmd)
end
