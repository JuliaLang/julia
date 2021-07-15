#!/usr/bin/env julia

## This rootfs includes everything that must be installed to build Julia
## within an alpine-based environment with GCC 9.

include("rootfs_utils.jl")

const tag_name, force_overwrite = get_arguments(ARGS, @__FILE__)

# Build alpine-based image with the following extra packages:
packages = [
    AlpinePackage("bash"),
    AlpinePackage("cmake"),
    AlpinePackage("curl"),
    AlpinePackage("git"),
    AlpinePackage("less"),
    AlpinePackage("m4"),
    AlpinePackage("perl"),
    AlpinePackage("python3"),
    AlpinePackage("wget"),

    # Install gcc/g++/gfortran v9, which comes from the Alpine v3.11 line
    AlpinePackage("g++~9", "v3.11"),
    AlpinePackage("gcc~9", "v3.11"),
    AlpinePackage("gfortran~9", "v3.11"),
]
tarball_path = alpine_bootstrap("package_musl64"; packages)

# Upload it
upload_rootfs_image(tarball_path; tag_name, force_overwrite)
