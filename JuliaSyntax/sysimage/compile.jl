#!/bin/bash
#=
[[ $1 == +* ]] && juliaup_arg=$1 && shift # release channel for juliaup
exec julia ${juliaup_arg} --startup-file=no -e 'include(popfirst!(ARGS))' "$0" "$@"
=#

imgs_base_path = joinpath(first(DEPOT_PATH), "sysimages", "v$VERSION")
mkpath(imgs_base_path)

using Libdl

cd(@__DIR__)

using Pkg
Pkg.activate(".")
Pkg.develop("JuliaSyntax")
Pkg.develop(path="./JuliaSyntaxCore")

image_path = joinpath(imgs_base_path, "juliasyntax_sysimage."*Libdl.dlext)

using PackageCompiler
PackageCompiler.create_sysimage(
    ["JuliaSyntaxCore"],
    project=".",
    sysimage_path=image_path,
    precompile_execution_file="precompile_exec.jl",
    incremental=true,
)

@info """## System image compiled!

      Use it with `julia -J "$image_path"`
      """

