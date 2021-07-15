#!/usr/bin/env julia

## This rootfs includes enough of a host toolchain to build the LLVM passes.
## Eventually, this image will probably be replaced with the actual builder image,
## as that will have the necessary toolchains as well, but that image is not built yet.

if length(ARGS) != 1
    throw(ArgumentError("Usage: package_linux64.jl [tag_name]"))
end
const tag_name = convert(String, strip(ARGS[1]))::String

include("rootfs_utils.jl")

# Build debian-based image with the following extra packages:
packages = [
    "automake",
    "bash",
    "bison",
    "cmake",
    "curl",
    "flex",
    "gdb",
    "git",
    "less",
    "libatomic1",
    "libtool",
    "m4",
    "make",
    "perl",
    "pkg-config",
    "python3",
    "wget",
    "vim",
]
tarball_path = debootstrap("package_linux64"; packages) do rootfs
    # Install GCC 9, specifically
    @info("Installing gcc-9")
    gcc_install_cmd = """
    echo 'deb http://deb.debian.org/debian testing main' >> /etc/apt/sources.list && \\
    apt-get update && \\
    DEBIAN_FRONTEND=noninteractive apt-get install -y \\
        gcc-9 g++-9 gfortran-9

    # Create symlinks for `gcc` -> `gcc-9`, etc...
    for tool_path in /usr/bin/*-9; do
        tool="\$(basename "\${tool_path}" | sed -e 's/-9//')"
        ln -sf "\${tool}-9" "/usr/bin/\${tool}"
    done
    """
    chroot(rootfs, "bash", "-c", gcc_install_cmd; uid=0, gid=0)
end

# Upload it
upload_rootfs_image(tarball_path; tag_name)
