import sys, os
sys.path.append(os.path.join(os.path.dirname(os.path.abspath(__file__)), "vendored"))
import toml

artifacts_toml = sys.argv[1]
platform_triplet = sys.argv[2]

# To debug when TOML files get parsed, set this to True
verbose = False

if verbose:
    sys.stderr.write("PARSING %s "%(artifacts_toml))

# Fail out gracefully if a file does not exist, printing nothing
if not os.path.isfile(artifacts_toml):
    if verbose:
        sys.stderr.write("SKIP\n")
    sys.exit(0)

# Get the artifact_name as `MbedTLS_jll-<hash>` -> `MbedTLS`
artifact_name = os.path.basename(os.path.dirname(artifacts_toml)).split("_jll")[0]
data = toml.load(artifacts_toml)
if verbose:
    sys.stderr.write("OK\n")

# Extract the arch, OS, libc, libgfortran and cxxabi from our platform triplet:
arch, os, libc, libgfortran, cxxabi = platform_triplet.split("-")

# Do some normalization to make up for our super simplistic parsing
if os == "apple":
    os = "macos"
elif os == "w64":
    os = "windows"

if libc == "gnu" or libc == "gnueabihf":
    libc = "glibc"
elif libc == "musleabihf":
    libc = "musl"
elif libc.startswith("freebsd"):
    # This is where the simplistic parsing really starts to break down
    os = "freebsd"
    libc = ""

def compiler_abi_str(entry):
    cabi_str = ""
    if "libgfortran_version" in entry:
        cabi_str = cabi_str + "-libgfortran%s"%(entry["libgfortran_version"][0])
    if "cxxstring_abi" in entry:
        cabi_str = cabi_str + "-%s"%(entry["cxxstring_abi"])
    return cabi_str

def platform_key(entry):
    if entry["os"] == "windows":
        if arch == "x86_64":
            return "x86_64-w64-mingw32"
        elif arch == "i686":
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

def platform_key_abi(entry):
    return platform_key(entry) + compiler_abi_str(entry)

# Find the key that best matches our platform triplet.
for entry in data[artifact_name]:
    # Skip entries that don't match ours
    if entry["arch"] != arch:
        continue
    if entry["os"] != os:
        continue
    if "libc" in entry and entry["libc"] != libc:
        continue
    if "libgfortran_version" in entry and entry["libgfortran_version"][0] != libgfortran[-1]:
        continue
    if "cxxstring_abi" in entry and entry["cxxstring_abi"] != cxxabi:
        continue

    # Otherwise, print out git-tree-sha1 and download URL
    sys.stdout.write("%s %s %s"%(entry["git-tree-sha1"],entry["download"][0]["url"],platform_key_abi(entry)))
    sys.exit(0)

raise Exception("Unable to find a matching artifact in %s for %s!"%(artifacts_toml, platform_triplet))