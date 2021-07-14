#!/usr/bin/env julia

## This rootfs includes enough of a host toolchain to build the LLVM passes.
## Eventually, this image will probably be replaced with the actual builder image,
## as that will have the necessary toolchains as well, but that image is not built yet.

if length(ARGS) != 1
    throw(ArgumentError("Usage: llvm-passes.jl [tag_name]"))
end
const tag_name = convert(String, strip(ARGS[1]))::String

include("rootfs_utils.jl")

# Build debian-based image with the following extra packages:
packages = [
    "bash",
    "build-essential",
    "cmake",
    "curl",
    "gfortran",
    "git",
    "less",
    "libatomic1",
    "m4",
    "perl",
    "pkg-config",
    "python",
    "python3",
    "wget",
]
tarball_path = debootstrap("llvm-passes"; packages)

# Upload it
upload_rootfs_image(tarball_path; tag_name)
