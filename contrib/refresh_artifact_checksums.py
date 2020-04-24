import sys, os, subprocess
sys.path.append(os.path.join(os.path.dirname(os.path.abspath(__file__)), "vendored"))
import toml
from artifacts_utils import *

# This file is a part of Julia. License is MIT: https://julialang.org/license
# Invoke this with no arguments to refresh all tarballs, or with jll names to refresh only that project.
#
# Example:
#   ./refresh_artifact_checksums.sh GMP OpenBLAS

# Extract --verbose from command line arguments
verbose = "--verbose" in sys.argv or "-v" in sys.argv
sys.argv = [a for a in sys.argv if not a in ["--verbose", "-v"]]

juliahome = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
deps_dir = os.path.join(juliahome, "deps")
all_jlls = [f[:-12] for f in os.listdir(deps_dir) if f.endswith("_jll.version")]

# Explicitly exclude some things we don't want to track
all_jlls = filter(lambda f: f not in ["Clang", "LLVM", "Objconv"], all_jlls)

# Strip jll filters of their _jll and capitalization
def normalize(name):
    if name.endswith("_jll"):
        name = name[:-4]
    return name.lower()

jll_filters = [normalize(n) for n in sys.argv[1:]]
if jll_filters:
    all_jlls = filter(lambda n: normalize(n) in jll_filters, all_jlls)

def vrun(cmd):
    if verbose:
        os.system(' '.join(cmd))
    else:
        subprocess.check_output(cmd)

# Extract some information out of the buildsystem
srccache = subprocess.check_output(["make", "-sC", "deps", "print-SRCCACHE"]).decode('utf-8').split('=')[1].strip()
jldownload = os.path.join(juliahome, "deps", "tools", "jldownload")
jlchecksum = os.path.join(juliahome, "deps", "tools", "jlchecksum")
devnull = open(os.devnull, 'wb')

# Next, ensure the JLLs themselves are installed, then parse their Artifacts.toml files:
os.chdir(juliahome)
for jll_name in all_jlls:
    # Install the JLL (but not the actual artifact itself.  yet.)
    print("Installing %s_jll..."%(jll_name))
    vrun(["make", "-sC", "deps", "extract-%s_jll"%(jll_name)])
    
    print("Parsing %s_jll Artifacts.toml file..."%(jll_name))
    output = subprocess.check_output(["make", "-sC", "deps", "print-%s_jll_ARTIFACTS_TOML"%(jll_name)]).decode('utf-8')
    artifacts_toml = os.path.join(juliahome, "deps", output.split('=')[1].strip())
    artifact_data = toml.load(artifacts_toml)
    for entry in artifact_data[jll_name]:
        # Check to make sure this is a downloadable artifact entry
        if "download" not in entry:
            print("ERROR: non-downloadable artifact in STDLB?!?!? %s"%(entry))
            sys.exit(1)

        # Calculate where the tarball would be stored
        treehash = entry["git-tree-sha1"]
        platform = platform_key(entry)
        tarball_path = os.path.join(srccache, "%s_jll-%s-%s.tar.gz"%(jll_name, platform, treehash))

        # Do the checksums already check out?  If so, skip!
        try:
            if os.path.isfile(tarball_path) and subprocess.check_call([jlchecksum, tarball_path], stdout=devnull, stderr=devnull) == 0:
                continue
            else:
                print("jlchecksum %s failed"%(tarball_path))
        except:
            print "Unexpected error:", sys.exc_info()[0]
            pass

        for dl_info in entry["download"]:
            print(" -> Downloading for %s to %s"%(platform, os.path.basename(tarball_path)))
            try:
                os.remove(tarball_path)
            except:
                pass

            dl_output = subprocess.check_output([jldownload, tarball_path, dl_info['url']], stderr=subprocess.STDOUT)
            if os.path.isfile(tarball_path):
                if subprocess.check_call([jlchecksum, tarball_path], stdout=devnull, stderr=devnull) != 0:
                    print("Checksum failed even after downloading!")
                    sys.exit(1)
                break
            else:
                print('Download failed:\n', dl_output)
            

    # Load in the Artifacts.toml file