#!/usr/bin/env julia
# This file is a part of Julia. License is MIT: https://julialang.org/license

# Builds a cache for all stdlib packages and installs that alongside Julia.

@assert length(ARGS) == 2
pkgdir = ARGS[1]
cachedir = ARGS[2]

let cache_path = Base.LOAD_CACHE_PATH
    empty!(cache_path)
    push!(cache_path, cachedir)
end

let load_path = Base.LOAD_PATH
    empty!(load_path)
    push!(load_path, pkgdir)
end

for pkg in readdir(pkgdir)
    @info "Building cache for $pkg"
    mfile = joinpath(pkgdir, pkg, "src", string(pkg, ".jl"))
    if isfile(mfile)
        # We can't use Base.compilecache since we don't know the order of dependencies
        Base.require(Symbol(pkg))
    else
        @info "Directory $pkg doesn't have a $mfile. Skipping."
        continue
    end
end
