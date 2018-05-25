#!/usr/bin/env julia
# This file is a part of Julia. License is MIT: https://julialang.org/license

# Adds the julia license line `new_license` to configured file extensions in `rootdirs`.
#
# Option `old_license` to remove an existing license first in case one wants to change the
# license text in the future.
#
# Checks also if somewhere else in the file the license text is found (`copy/past error`)
# and if possible deletes such lines - if other text is on the same line it raises an error.

### CONFIG HERE

const print_result = true  # prints files which where not processed.

const rootdirs = [
    "../base",
    "../contrib",
    "../src",
    "../stdlib",
    "../test",
]

# to exculde whole sub directories
const excludedirs = [
    # see: https://github.com/JuliaLang/julia/pull/11073#issuecomment-98090053
    "../base/grisu",
    "../src/flisp",
]

const skipfiles = [
    "../contrib/add_license_to_files.jl",
    # files to check - already copyright
    # see: https://github.com/JuliaLang/julia/pull/11073#issuecomment-98099389
    "../base/special/trig.jl",
    "../base/special/exp.jl",
    "../base/special/rem_pio2.jl",
    #
    "../src/abi_llvm.cpp",
    "../src/abi_ppc64le.cpp",
    "../src/abi_win32.cpp",
    "../src/abi_win64.cpp",
    "../src/abi_x86.cpp",
    "../src/abi_x86_64.cpp",
    "../src/disasm.cpp",
    "../src/getopt.c",
    "../src/getopt.h",
    "../src/support/END.h",
    "../src/support/ENTRY.amd64.h",
    "../src/support/ENTRY.i387.h",
    "../src/support/MurmurHash3.c",
    "../src/support/MurmurHash3.h",
    "../src/support/asprintf.c",
    "../src/support/dirname.c",
    "../src/support/strptime.c",
    "../src/support/strtod.c",
    "../src/support/tzfile.h",
    "../src/support/utf8.c",
    "../src/crc32c.c",
]

const ext_prefix = Dict([
    (".jl", "# "),
    (".sh", "# "),
    (".h", "// "),
    (".c", "// "),
    (".cpp", "// "),
])

const new_license = "This file is a part of Julia. License is MIT: https://julialang.org/license"

# Old License text if such should be first removed - or empty string
const old_license = ""

### END CONFIG HERE


function check_lines!(
    path::AbstractString, lines::Vector, checktxt::AbstractString,
    prefix::AbstractString, oldcheck::Bool)
    remove = []
    for i in 1:length(lines)
        line = lines[i]
        if occursin(checktxt, line)
            if strip(line) == strip(prefix * checktxt) || strip(line) == strip(checktxt)
                push!(remove, i)
            else
                error(string("`path` contains an additional line with ",
                         oldcheck ? "old" : "new",
                         " license.\nlinenum: $(i): $(line) \n`path:` $(path)\n",
                         "Fix this first or add the file to `skipfiles`.\n\n"))
            end
        end
    end
    deleteat!(lines, remove)
end

license_linenum(line) = startswith(strip(line), "#!") ? 2 : 1

# Collects all absolute file paths in rootdir inclusive subdirs
function getfilespaths!(filepaths::Vector, rootdir::AbstractString)
    isdir(rootdir) || error(string("`rootdir` must be an directory. "))
    abs_rootdir = abspath(rootdir)
    for name in readdir(abs_rootdir)
        path = joinpath(abs_rootdir, name)
        if isdir(path)
            getfilespaths!(filepaths, path)
        else
            push!(filepaths, joinpath(abs_rootdir, name))
        end
    end
end

function add_license_line!(unprocessed::Vector, src::AbstractString, new_license::AbstractString,
                          old_license::AbstractString, ext_prefix::Dict, abs_excludedirs::Vector,
                                                                               skipfiles::Vector)

    for name in readdir(src)
        path = normpath(joinpath(src, name))
        if isdir(path)
            if path in abs_excludedirs
                getfilespaths!(unprocessed, path)
                continue
            else
                add_license_line!(unprocessed, path, new_license, old_license,
                                  ext_prefix, abs_excludedirs, skipfiles)
            end
        elseif path in skipfiles
            push!(unprocessed, path)
            continue
        else
            ext = splitext(path)[2]
            if ext in keys(ext_prefix)
                prefix = ext_prefix[ext]
                f = open(path, "r")
                lines = readlines(f, keep=true)
                close(f)
                isempty(lines) && (push!(unprocessed, path); continue)
                isempty(old_license) || check_lines!(path, lines, old_license, prefix, true)
                check_lines!(path, lines, new_license, prefix, false)
                # check shebang file
                linenum = license_linenum(lines[1])
                if !isempty(strip(lines[linenum]))
                    insert!(lines, linenum, string(prefix, new_license, "\n\n"))
                else
                    insert!(lines, linenum, string(prefix, new_license, "\n"))
                end
                open(path, "w") do f
                    for line in lines
                        write(f, line)
                    end
                end
            else
                push!(unprocessed, path)
            end
        end
    end
    return unprocessed
end

# Returns a new Vector with all absolute path: raises an error if path does not exist
function abspaths(A::Vector)
    abs_A = []
    for p in A
        abs_p = isabspath(p) ? normpath(p) : normpath(joinpath(dirname(@__FILE__), p))
        ispath(abs_p) || error(string("`abs_p` seems not to be an existing path. ",
                                      "Adjust your configuration: <", p, "> : ", abs_p, "\n"))
        push!(abs_A, abs_p)
    end
    return abs_A
end

function add_license(rootdirs::Vector, new_license::AbstractString, old_license::AbstractString,
                     ext_prefix::Dict, excludedirs::Vector, skipfiles::Vector, print_result::Bool)
    isempty(strip(new_license)) && error("`new_license` may not only contain white space.")
    abs_skipfiles = abspaths(skipfiles)
    abs_rootdirs = abspaths(rootdirs)
    abs_excludedirs = abspaths(excludedirs)
    for p in abs_rootdirs
        p in abs_excludedirs && error(string("Seems one of the `rootdirs` is also included in ",
                                             "the `excludedirs`. `rootdirs`: ", p))
        unprocessed = []
        add_license_line!(unprocessed, p, new_license, old_license, ext_prefix,
                                                abs_excludedirs, abs_skipfiles)
        if print_result
            println("\nUnprocessed files in rootdir: <$(p)>\n")
            for file in unprocessed
                println("  <", basename(file), "> : ", file)
            end
        end
    end
end


## ---------------

add_license(rootdirs, new_license, old_license, ext_prefix, excludedirs, skipfiles, print_result)
