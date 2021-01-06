import sys, os
if len(sys.argv) != 3:
    sys.stderr.write("\nrelative_path.py - incomplete arguments: %s\n"%(sys.argv))
    sys.exit(1)

# We always use `/` as the path separator, no matter what OS we're running on, since our
# shells and whatnot during the build are all POSIX shells/cygwin.  We rely on the build
# system itself to canonicalize to `\` when it needs to, and deal with the shell escaping
# and whatnot at the latest possible moment.
sys.stdout.write(os.path.relpath(sys.argv[2], sys.argv[1]).replace(os.path.sep, '/'))