# This file is a part of Julia. License is MIT: https://julialang.org/license

# Write the contents of a `.debug_gdb_scripts` section for the installed system image.
# The debug info of images refers to Julia's own sources under the `share/julia` that the
# system image was built with (see `Base.init_debuginfo_source_paths`). The embedded gdb
# script maps that directory to the one of the installation it finds the system image in.
#
# gdb only runs it if the system image is in its `auto-load safe-path`; see the devdocs
# on debugging with gdb.
#
# Usage: julia debug_gdb_scripts.jl <output file>

recorded = dirname(joinpath(normpath(Sys.BUILD_STDLIB_PATH, "..", ".."), ""))
relpath_share = relpath(abspath(Sys.BINDIR, Base.DATAROOTDIR, "julia"),
                        abspath(Sys.BINDIR, Base.PRIVATE_LIBDIR))
commit = Base.GIT_VERSION_INFO.commit_short_raw
name = isempty(commit) ? "julia-$VERSION-source-paths" : "julia-$VERSION-$commit-source-paths"

# A Python string literal that means the same to Python 2 and 3
function pystr(s::String)
    io = IOBuffer()
    print(io, "u\"")
    for c in s
        if c == '\\' || c == '"'
            print(io, '\\', c)
        elseif isascii(c) && isprint(c)
            print(io, c)
        else
            print(io, codepoint(c) <= 0xffff ? "\\u" * string(codepoint(c), base=16, pad=4) :
                                               "\\U" * string(codepoint(c), base=16, pad=8))
        end
    end
    print(io, '"')
    return String(take!(io))
end

script = """
import gdb, os

def _julia_map_source_paths(objfile, recorded, relpath):
    # gdb splits the arguments of `set substitute-path` like a shell
    def quote(path):
        return '"' + path.replace('\\\\', '\\\\\\\\').replace('"', '\\\\"') + '"'
    # separate debug info files are not installed next to the system image
    if objfile.owner is not None:
        objfile = objfile.owner
    root = os.path.normpath(os.path.join(os.path.dirname(os.path.realpath(objfile.filename)), relpath))
    if root == recorded or not os.path.isdir(root):
        return
    # keep a mapping that was set up explicitly, e.g. to sources stored with an rr trace
    if len(gdb.execute('show substitute-path ' + quote(recorded), to_string=True).splitlines()) > 1:
        return
    gdb.execute('set substitute-path ' + quote(recorded) + ' ' + quote(root))

_julia_map_source_paths(gdb.current_objfile(), $(pystr(recorded)), $(pystr(relpath_share)))
"""

# An inline Python script: its id, its name on the first line, and a terminating NUL
write(ARGS[1], UInt8[0x04], name, "\n", script, UInt8[0x00])
