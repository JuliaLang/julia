#!/usr/bin/env julia
# This file is a part of Julia. License is MIT: https://julialang.org/license

include("JuliaConfig.jl")
using .JuliaConfig

const options = [
    "--cflags",
    "--ldflags",
    "--ldlibs",
    "--allflags"
];

function check_args(args)
    checked = intersect(args, options)
    if length(checked) == 0 || length(checked) != length(args)
        println(STDERR, "Usage: julia-config [", join(options, " | "), "]")
        exit(1)
    end
end

function main()
    check_args(ARGS)
    for args in ARGS
        if args == "--ldflags"
            println(ldflags())
        elseif args == "--cflags"
            println(cflags())
        elseif args == "--ldlibs"
            println(ldlibs())
        elseif args == "--allflags"
            println(allflags())
        end
    end
end

main()
