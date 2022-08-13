#!/bin/bash
#=
[[ $1 == +* ]] && juliaup_arg=$1 && shift # release channel for juliaup
exec julia ${juliaup_arg} --startup-file=no -e 'include(popfirst!(ARGS))' "$0" "$@"
=#

imgs_base_path = joinpath(first(DEPOT_PATH), "sysimages", "v$VERSION")
mkpath(imgs_base_path)

using Libdl

cd(@__DIR__)

# Create a copy of JuliaSyntax so we can change the project UUID.
# This allows us to use an older version of JuliaSyntax for developing
# JuliaSyntax itself.
rm("JuliaSyntax", force=true, recursive=true)
mkdir("JuliaSyntax")
cp("../src", "JuliaSyntax/src")
cp("../test", "JuliaSyntax/test")
projstr = replace(read("../Project.toml", String),
    "70703baa-626e-46a2-a12c-08ffd08c73b4"=>"54354a4c-6cac-4c00-8566-e7c1beb8bfd8")
write("JuliaSyntax/Project.toml", projstr)

using Pkg
rm("Project.toml", force=true)
rm("Manifest.toml", force=true)
Pkg.activate(".")
Pkg.develop(path="./JuliaSyntax")
Pkg.develop(path="./JuliaSyntaxCore")
Pkg.add("PackageCompiler")

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

