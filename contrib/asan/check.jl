#!/bin/bash
# -*- mode: julia -*-
# This file is a part of Julia. License is MIT: https://julialang.org/license
#
# Usage:
#     contrib/asan/check.jl <julia>
#
# Check that <julia> is built with ASAN.
#
#=
JULIA="${JULIA:-julia}"
exec "$JULIA" --startup-file=no --compile=min "${BASH_SOURCE[0]}" "$@"
=#

function main(args = ARGS)::Int
    if length(args) != 1
        @error "Expect a single argument" args
        return 2
    end
    julia, = args

    # It looks like double-free is easy to robustly trigger.
    code = """
    @info "Testing a pattern that would trigger ASAN"
    write(ARGS[1], "started")

    ptr = ccall(:malloc, Ptr{UInt}, (Csize_t,), 256)
    ccall(:free, Cvoid, (Ptr{UInt},), ptr)
    ccall(:free, Cvoid, (Ptr{UInt},), ptr)

    @error "Failed to trigger ASAN"
    """

    local proc
    timeout = Threads.Atomic{Bool}(false)
    isstarted = false
    mktemp() do tmppath, tmpio
        cmd = `$julia -e $code $tmppath`
        # Note: Ideally, we set ASAN_SYMBOLIZER_PATH here. But there is no easy
        # way to find out the path from just a Julia binary.

        @debug "Starting a process" cmd
        proc = run(pipeline(cmd; stdout, stderr); wait = false)
        timer = Timer(10)
        @sync try
            @async begin
                try
                    wait(timer)
                    true
                catch err
                    err isa EOFError || rethrow()
                    false
                end && begin
                    timeout[] = true
                    kill(proc)
                end
            end
            wait(proc)
        finally
            close(timer)
        end

        # At the very beginning of the process, the `julia` subprocess put a
        # marker that it is successfully started. This is to avoid mixing
        # non-functional `julia` binary (or even non-`julia` command) and
        # correctly working `julia` with ASAN:
        isstarted = read(tmpio, String) == "started"
    end

    if timeout[]
        @error "Timeout waiting for the subprocess"
        return 1
    elseif success(proc)
        @error "ASAN was not triggered"
        return 1
    elseif !isstarted
        @error "Failed to start the process"
        return 1
    else
        @info "ASAN is functional in the Julia binary `$julia`"
        return 0
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    exit(main())
end
