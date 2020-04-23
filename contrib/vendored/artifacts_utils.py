def compiler_abi_str(entry):
    cabi_str = ""
    if "libgfortran_version" in entry:
        cabi_str = cabi_str + "-libgfortran%s"%(entry["libgfortran_version"][0])
    if "cxxstring_abi" in entry:
        cabi_str = cabi_str + "-%s"%(entry["cxxstring_abi"])
    return cabi_str

def platform_key(entry):
    if entry["os"] == "windows":
        if entry["arch"] == "x86_64":
            return "x86_64-w64-mingw32"
        elif entry["arch"] == "i686":
            return "i686-w64-mingw32"
    elif entry["os"] == "macos":
        return "x86_64-apple-darwin14"
    elif entry["os"] == "freebsd":
        return "x86_64-unknown-freebsd11.1"
    elif entry["os"] == "linux":
        call_abi = ""
        if entry["arch"] == "armv7l":
            call_abi = "eabihf"
        libc = ""
        if entry["libc"] == "glibc":
            libc = "gnu"
        elif entry["libc"] == "musl":
            libc = "musl"
        return "%s-linux-%s%s"%(entry["arch"], libc, call_abi)
    else:
        raise ValueError("What kind of an OS is '%s'?"%(entry["os"]))