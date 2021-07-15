#!/usr/bin/env julia

## This rootfs includes everything that must be installed to build Julia
## within a debian-based environment with GCC 9.

include("rootfs_utils.jl")

const tag_name, force_overwrite = get_arguments(ARGS, @__FILE__)

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
upload_rootfs_image(tarball_path; tag_name, force_overwrite)
