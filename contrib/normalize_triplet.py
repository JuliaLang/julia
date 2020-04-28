#!/usr/bin/env python

import re, sys

# This script designed to mimick `src/PlatformNames.jl` in `BinaryProvider.jl`, which has
# a method `platform_key_abi()` to parse uname-like output into something standarized.

if len(sys.argv) < 2:
    print("Usage: {} <host triplet> [<gcc version>] [<cxxabi11>]".format(sys.argv[0]))
    sys.exit(1)

arch_mapping = {
    'x86_64': '(x86_|amd)64',
    'i686': "i\\d86",
    'aarch64': "aarch64",
    'armv7l': "arm(v7l)?",
    'powerpc64le': "p(ower)?pc64le",
}
platform_mapping = {
    'darwin': "-apple-darwin[\\d\\.]*",
    'freebsd': "-(.*-)?freebsd[\\d\\.]*",
    'windows': "-w64-mingw32",
    'linux': "-(.*-)?linux",
}
libc_mapping = {
    'blank_libc': "",
    'gnu': "-gnu",
    'musl': "-musl",
}
call_abi_mapping = {
    'blank_call_abi': "",
    'eabihf': "eabihf",
}
libgfortran_version_mapping = {
    'blank_libgfortran': "",
    'libgfortran3': "-libgfortran3",
    'libgfortran4': "-libgfortran4",
    'libgfortran5': "-libgfortran5",
}
cxx_abi_mapping = {
    'blank_cxx_abi': "",
    'cxx03': "-cxx03",
    'cxx11': "-cxx11",
}

# Helper function to collapse dictionary of mappings down into a regex of
# named capture groups joined by "|" operators
c = lambda mapping: "("+"|".join(["(?P<%s>%s)"%(k,v) for (k, v) in mapping.items()]) + ")"
mondo_regex = re.compile(
    "^"+
    c(arch_mapping)+
    c(platform_mapping)+
    c(libc_mapping)+
    c(call_abi_mapping)+
    c(libgfortran_version_mapping)+
    c(cxx_abi_mapping)+
    "$"
)

# Apply our mondo regex to our input:
m = mondo_regex.match(sys.argv[1])
if m is None:
    print("ERROR: Unmatchable platform string '%s'!"%(sys.argv[1]))
    sys.exit(1)

# Helper function to find the single named field within the giant regex
# that is not `nothing` for each mapping we give it.
def get_field(m, mapping):
    g = m.groupdict()
    for k in mapping:
        if g[k] is not None:
            return k

arch = get_field(m, arch_mapping)
platform = get_field(m, platform_mapping)
libc = get_field(m, libc_mapping)
call_abi = get_field(m, call_abi_mapping)
libgfortran_version = get_field(m, libgfortran_version_mapping)
cxx_abi = get_field(m, cxx_abi_mapping)

# The default libc on Linux is glibc
if platform == "linux" and libc == "blank_libc":
    libc = "gnu"

def r(x):
    x = x.replace("blank_call_abi", "")
    x = x.replace("blank_libgfortran", "")
    x = x.replace("blank_cxx_abi", "")
    x = x.replace("blank_libc", "")
    return x

def p(x):
    # These contain characters that can't be easily represented as
    # capture group names, unfortunately:
    os_remapping = {
        'darwin': 'apple-darwin14',
        'windows': 'w64-mingw32',
        'freebsd': 'unknown-freebsd11.1',
    }
    x = r(x)
    if x:
        for k in os_remapping:
            x = x.replace(k, os_remapping[k])
        return '-' + x
    return x

# If the user passes in a GCC version (like 8.2.0) use that to force a
# "-libgfortran5" tag at the end of the triplet, but only if it has otherwise
# not been specified
if libgfortran_version == "blank_libgfortran":
    if len(sys.argv) >= 3:
        libgfortran_version = {
            "4": "libgfortran3",
            "5": "libgfortran3",
            "6": "libgfortran3",
            "7": "libgfortran4",
            "8": "libgfortran5",
            "9": "libgfortran5",
        }[list(filter(lambda x: re.match("\d+\.\d+(\.\d+)?", x), sys.argv[2].split()))[-1][0]]

if cxx_abi == "blank_cxx_abi":
    if len(sys.argv) == 4:
        cxx_abi = {
            "0": "cxx03",
            "1": "cxx11",
            "": "",
        }[sys.argv[3]]

print(arch+p(platform)+p(libc)+r(call_abi)+p(libgfortran_version)+p(cxx_abi))

# Testing suite:
# triplets="i686-w64-mingw32 x86_64-pc-linux-musl arm-linux-musleabihf x86_64-linux-gnu arm-linux-gnueabihf x86_64-apple-darwin14 x86_64-unknown-freebsd11.1"
# for t in $triplets; do
#     if [[ $(./normalize_triplet.py "$t") != "$t" ]]; then
#         echo "ERROR: Failed test on $t"
#     fi
# done
